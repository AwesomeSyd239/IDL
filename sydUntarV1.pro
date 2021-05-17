pro sydGZIP_COMPRESS, fileIn, fileOut, $
  BUFFER=buffer, $
  CLOSE=doClose, $
  COUNT=count, $
  DEBUG=debug, $
  DELETE=delete, $
  NBYTES=fileNbytes, $
  OFFSET=fileOffset, $
  UNCOMPRESS=uncompress, $
  VERBOSE=verbose

  compile_opt idl2, hidden
  common file_gzip_common, hashFileIn, hashFileOut
  if (~KEYWORD_SET(debug)) then ON_ERROR, 2
  lunIn = !null
  lunOut = !null
  keepOpen = ISA(doClose) && ~KEYWORD_SET(doClose)

  if (ISA(fileIn, 'STRING') && ISA(hashFileIn, 'HASH')) then $
    if (hashFileIn.HasKey(fileIn)) then lunIn = hashFileIn[fileIn]
  if (ISA(fileOut, 'STRING') && ISA(hashFileOut, 'HASH')) then $
    if (hashFileOut.HasKey(fileOut)) then lunOut = hashFileOut[fileOut]

  if (KEYWORD_SET(doClose)) then begin
    count = 0L
    buffer = 0b

    if (ISA(lunIn)) then hashFileIn.Remove, fileIn
    if (ISA(lunIn)) then FREE_LUN, lunIn, /FORCE
    if (ISA(lunOut)) then hashFileOut.Remove, fileOut
    if (ISA(lunOut)) then FREE_LUN, lunOut, /FORCE
    
    if (ISA(hashFileIn,'HASH') && hashFileIn.IsEmpty()) then OBJ_DESTROY,hashFileIn
    if (ISA(hashFileOut,'HASH') && hashFileOut.IsEmpty()) then OBJ_DESTROY, hashFileOut
    RETURN
  endif

  if ~(ISA(lunIn) || FILE_TEST(fileIn, /REGULAR, /READ)) then $
    MESSAGE, /NONAME, 'Unable to read file: "' + fileIn + '"'

  CATCH, iErr
  if (iErr ne 0) then begin ;If something goes wrong
    if (ISA(lunIn)) then FREE_LUN, lunIn, /FORCE ;Free the files
    if (ISA(lunOut)) then FREE_LUN, lunOut, /FORCE
    CATCH, /CANCEL ;Abort the procedure
    MESSAGE, /NONAME, 'Unable to write file: "' + fileOut + '"' ;Tell us what happened
  endif


  if (ISA(buffer)) then begin
    ; Need to determine the original number of uncompressed bytes.
    if (KEYWORD_SET(uncompress) && ~(ISA(fileOffset) || ISA(fileNbytes))) then begin
      OPENR, lunInTmp, fileIn, /GET_LUN, /SWAP_IF_BIG_ENDIAN
      infoIn = FSTAT(lunInTmp)
      POINT_LUN, lunInTmp, infoIn.size - 4
      fileNbytes = 0ul
      READU, lunInTmp, fileNbytes
      FREE_LUN, lunInTmp, /FORCE
    endif

    if (~ISA(lunIn)) then OPENR, lunIn, fileIn, /GET_LUN, COMPRESS=KEYWORD_SET(uncompress)
    if (ISA(fileOffset)) then POINT_LUN, lunIn, fileOffset

    ; Need to determine the number of bytes to compress.
    if (~KEYWORD_SET(uncompress) && ~ISA(fileNbytes)) then begin
      infoIn = FSTAT(lunIn)
      fileNbytes = infoIn.size
      if (ISA(fileOffset)) then fileNbytes = (fileNbytes - fileOffset) > 0
    endif

    buffer = []
    count = 0L
    if (fileNbytes gt 0) then begin
      buffer = BYTARR(fileNbytes, /NOZERO)
      POINT_LUN, -lunIn, currentLun
      CATCH, iErr
      if (iErr ne 0) then CATCH, /CANCEL else READU, lunIn, buffer
      POINT_LUN, -lunIn, newLun
      count = newLun - currentLun
      if (count lt N_ELEMENTS(buffer)) then buffer = (count gt 0) ? buffer[0:count-1] : 0b
    endif

    if (keepOpen) then begin
      if (~ISA(hashFileIn)) then hashFileIn = HASH()
      hashFileIn[fileIn] = lunIn
    endif else FREE_LUN, lunIn, /FORCE
    lunIn = 0

    if (~KEYWORD_SET(uncompress)) then buffer = ZLIB_COMPRESS(buffer, /GZIP_HEADER)
    infoOut = {size: N_ELEMENTS(buffer)}
    
  endif else begin
    if (~ISA(lunIn)) then  OPENR, lunIn, fileIn, /GET_LUN, COMPRESS=KEYWORD_SET(uncompress)
    if (ISA(fileOffset)) then POINT_LUN, lunIn, fileOffset
    
    if (~ISA(lunOut)) then OPENW, lunOut, fileOut, /GET_LUN, COMPRESS=~KEYWORD_SET(uncompress)
    if (ISA(fileNbytes)) then begin
      POINT_LUN, -lunOut, currentLun
      CATCH, iErr
      if (iErr ne 0) then CATCH, /CANCEL else COPY_LUN, lunIn, lunOut, fileNbytes
      POINT_LUN, -lunOut, newLun
      count = newLun - currentLune
    endif else COPY_LUN, lunIn, lunOut, /EOF, TRANSFER_COUNT=count ;Write the new file

    if (keepOpen) then begin
      if (~ISA(hashFileIn)) then hashFileIn = HASH()
      hashFileIn[fileIn] = lunIn
      if (~ISA(hashFileOut)) then hashFileOut = HASH()
      hashFileOut[fileOut] = lunOut
    endif else begin
      FREE_LUN, lunIn, /FORCE
      FREE_LUN, lunOut, /FORCE
    endelse
    lunIn = 0
    lunOut = 0
  endelse


  if (KEYWORD_SET(delete) && ~keepOpen) then $
    FILE_DELETE, fileIn

  if (KEYWORD_SET(verbose)) then begin
    infoIn = FILE_INFO(fileIn)
    infoOut = FILE_INFO(fileOut)
    if (KEYWORD_SET(uncompress)) then begin
      countStr = '  ' + STRTRIM(count,2) + ' bytes'
      MESSAGE, /INFO, /NONAME, 'Uncompress ' + fileIn + countStr
    endif else begin
      spaceSavings = 100 - 100.0*infoOut.size/(infoIn.size > 1)
      spaceSavings = 0 > ROUND(spaceSavings*10)/10. < 100
      spaceSavings = STRTRIM(STRING(spaceSavings, FORMAT='(F5.1)'),2)
      MESSAGE, /INFO, /NONAME, 'Compress ' + fileIn + '  ' + spaceSavings + '%'
    endelse
  endif
end


pro sydUntar_DecodeHeader, header, filename, nbytes
  compile_opt idl2, hidden

  ustar = STRLOWCASE(STRING(header[257:261])) eq 'ustar'
  if (~ustar) then $
    MESSAGE, /NONAME, 'TAR file is not in USTAR format.'

  filename = STRTRIM(STRING(header[0:99]),2)
  filepath = STRTRIM(STRING(header[345:*]),2)
  if (filepath ne '') then $
    filename = filepath + '/' + filename

  nbytes = 0ull
  READS, STRING(header[124:134]), nbytes, FORMAT='(o)'

  typeflag = STRING(header[156])

  case (typeflag) of
    '5': begin
      c = STRMID(filename, 0, /REVERSE_OFFSET)
      if (c ne '/' && c ne '\') then filename += '/'
    end
    '0': ; regular file
    '': ; regular file
    'x': filename = '@PaxHeader'
    'L': ; special GNU @LongLink
    else: MESSAGE, /NONAME, 'Skipping header of type: ' + typeflag
  endcase

end

function sydUntar_GetFileInfo, tar_lun, pointlun, filename, nbytes, $
  STREAM=stream

  compile_opt idl2, hidden

  if (tar_lun ne 0) then begin
    header = BYTARR(512, /NOZERO)
    READU, tar_lun, header
  endif else begin
    header = stream[pointlun:pointlun+511]
  endelse
  pointlun += 512

  ; Empty block?
  if (ARRAY_EQUAL(header, 0b) || ARRAY_EQUAL(header, 32b)) then begin
    RETURN, -1
  endif

  sydUntar_DecodeHeader, header, filename, nbytes
  if (filename eq '') then RETURN, 0

  ; Special GNU tar trick to handle long filenames.
  if (STRPOS(filename, '@LongLink') ge 0) then begin
    if (nbytes eq 0) then RETURN, 0
    ; Read the long filename (in 512-byte blocks)
    nbytes = 512*((nbytes + 511ull)/512)
    if (tar_lun ne 0) then begin
      filename = BYTARR(nbytes)
      READU, tar_lun, filename
    endif else begin
      filename = stream[pointlun:pointlun+nbytes-1]
    endelse

    pointlun += nbytes
    filename = STRING(filename)

    ; Now read the normal header block but ignore the filename.
    if (tar_lun ne 0) then begin
      READU, tar_lun, header
    endif else begin
      header = stream[pointlun:pointlun+511]
    endelse
    pointlun += 512

    sydUntar_DecodeHeader, header, !NULL, nbytes

  endif else if (STRPOS(filename, '@PaxHeader') ge 0) then begin
    ; POSIX extended header (the @PaxHeader is our own IDL internal flag)
    if (nbytes eq 0) then RETURN, 0
    ; Read the PaxHeader (in 512-byte blocks)
    nbytes = 512*((nbytes + 511ull)/512)
    if (tar_lun ne 0) then begin
      paxheader = BYTARR(nbytes)
      READU, tar_lun, paxheader
    endif else begin
      paxheader = stream[pointlun:pointlun+511]
    endelse
    pointlun += nbytes
    paxheader = STRING(paxheader)
    keywords = STRTOK(paxheader, STRING(10b), /EXTRACT)
    nbytes = !NULL
    filename = !NULL
    foreach kw, keywords do begin
      kwStart = STRPOS(kw, ' ') + 1
      kwEnd = STRPOS(kw, '=')
      if (kwStart le 0 || kwEnd le kwStart) then continue
      keyword = STRMID(kw, kwStart, kwEnd-kwStart)
      value = STRMID(kw, kwEnd+1)
      case keyword of
        'path': filename = value
        'size': nbytes = ULONG64(value)
        else: ; ignore other keywords
      endcase
    endforeach
    ; Now read the normal header block but ignore either the filename or size.
    if (tar_lun ne 0) then begin
      READU, tar_lun, header
    endif else begin
      header = stream[pointlun:pointlun+511]
    endelse
    pointlun += 512
    if (~ISA(nbytes) || ~ISA(filename)) then begin
      sydUntar_DecodeHeader, header, filenameTmp, nbytesTmp
      if (~ISA(nbytes)) then nbytes = nbytesTmp
      if (~ISA(filename)) then filename = filenameTmp
    endif
  endif

  RETURN, 1
end



PRO sydUntar, stream, outdirIn, $
  FILES=files, $
  LIST=listIn, $
  debug=debug, $
  VERBOSE=verboseIn
  compile_opt idl2, hidden
  
  if (N_PARAMS() eq 0) then MESSAGE, 'Incorrect number of arguments.'
  verbose=keyword_set(verboseIn)
  if (verbose) then tic

  isBuffer = ISA(stream, 'BYTE')
  isFile = ISA(stream, 'STRING') && N_ELEMENTS(stream) eq 1
  if (~isBuffer && ~isFile) then MESSAGE, 'Input file must be a byte array or scalar string.'
  if (isFile) then file = stream[0]

  doList = KEYWORD_SET(listIn)
  verbose = KEYWORD_SET(verboseIn)
  sep = PATH_SEP()

  if (N_PARAMS() eq 1) then begin
    if (isBuffer) then MESSAGE, 'Output directory must be specified with buffer data.'
    outdir = FILE_DIRNAME(file)
  endif else begin
    if (N_ELEMENTS(outdirIn) gt 1) then MESSAGE, 'Output directory must be a scalar string.'
    outdir = outdirIn[0]
  endelse

  c = STRMID(outdir, 0, /REVERSE)
  outdir = STRJOIN(STRTOK(outdir, '\/', /EXTRACT, /PRESERVE_NULL), sep) ;"Normalize" the name to the current OS.
  if (c ne '') then outdir += sep ;If outdir is just a blank, then don't append a slash, because that would put the untarred files at the root of the file system.

  tar_lun = 0
  CATCH, iErr
  if (iErr ne 0) then begin
    CATCH, /CANCEL
    if (tar_lun ne 0) then FREE_LUN, tar_lun
    if (ISA(streamOrig)) then stream = TEMPORARY(streamOrig)
    MESSAGE, !ERROR_STATE.msg
  endif

  if (isFile) then begin
    fi = STRLOWCASE(file)
    isGZIP = STRMID(fi,2,/REVERSE) eq '.gz' || STRMID(fi,3,/REVERSE) eq '.tgz'
    if (~isGZIP) then begin ;might be a secret gzip, the first 2 bytes will tell us
      OPENR, tar_lun, file, /GET_LUN
      x = BYTARR(2)
      READU, tar_lun, x
      FREE_LUN, tar_lun
      isGZIP = x[0] eq 31b && x[1] eq 139b
      tar_lun = 0
    endif

    tmp = ''
    if (isGZIP) then begin
      tmp = FILEPATH(FILE_BASENAME(outdir), /TMP) + '.tar'
      sydGZIP_COMPRESS, file, tmp, /UNCOMPRESS,/debug
      file = tmp
    endif

    OPENR, tar_lun, file, /GET_LUN, DELETE=(tmp ne '')
  endif else begin
    isGZIP = stream[0] eq 31b && stream[1] eq 139b
    if (isGZIP) then begin
      streamOrig = TEMPORARY(stream)
      stream = ZLIB_UNCOMPRESS(streamOrig, /GZIP_HEADER)
    endif
  endelse

  ;TODO: What if the directory already exists?!
  if (~doList && ~FILE_TEST(outdir)) then FILE_MKDIR, outdir
  
  ;Collect all of the files in the tar
  files = LIST()
  empty = 0
  totalBytes = 0ull
  pointlun = 0ull
  while (1) do begin
    if (isFile) then if (EOF(tar_lun)) then break
    if ~(isFile) then if (pointlun ge N_ELEMENTS(stream)) then break

    lun = 0
    filename = ''
    nbytes = 0ull
    if keyword_set(debug) then begin
      CATCH, iErr
      if (iErr ne 0) then begin
          CATCH, /CANCEL
          MESSAGE, /RESET
          MESSAGE, /INFO, msg
        if (lun ne 0) then FREE_LUN, lun
        if (nbytes ne 0) then begin
          pointlun += 512*((nbytes+511ull)/512) ;Advance to the next block.
          if (isFile) then POINT_LUN, tar_lun, pointlun
        endif
        continue
      endif
    endif
    
    msg = 'Error reading ' + filename + ', skipping...'
    status = sydUntar_GetFileInfo(tar_lun, pointlun, filename, nbytes, STREAM=stream)
    if (status eq -1) then begin ;Status == -1 indicates an empty block
      if empty eq 1 then break ;Two empty 512-byte blocks indicates the end of the TAR file.
      empty++
      continue
    endif
    empty = 0 ;Reset the empty flag if we found a header
    if (status eq 0 || filename eq '') then continue ;Status == 0 indicates skip the file

    if (verbose) then begin
      c = STRMID(filename, 0, /REVERSE_OFFSET)
      if (c ne '/' && c ne '\') then bytes=', '+STRTRIM(nbytes,2)+' bytes' else bytes=''
      MESSAGE, /INFO, /NONAME, filename+bytes
      totalBytes += nbytes
    endif

    fname = STRJOIN(STRTOK(filename, '\/', /EXTRACT, /PRESERVE_NULL), sep) ;"Normalize" the name to the current OS.

    if (~doList) then begin
      fname = outdir + fname
      c = STRMID(fname, 0, /REVERSE_OFFSET)
      if (c eq '/' || c eq '\') then begin
        if (~FILE_TEST(fname)) then begin
          msg = 'Error writing directory ' + fname + ', skipping...'
          FILE_MKDIR, fname
        endif
      endif else if (nbytes gt 0) then begin
        msg = 'Error writing file ' + fname + ', skipping...'
        OPENW, lun, fname, /GET_LUN
        if (isFile) $
          then COPY_LUN, tar_lun, lun, nbytes $
          else WRITEU, lun, stream[pointlun:pointlun+nbytes-1]
        FREE_LUN, lun
      endif
    endif
    files.Add, fname
    
    pointlun += 512*((nbytes + 511ull)/512) ;Advance to the next block.
    if (isFile) then POINT_LUN, tar_lun, pointlun
  endwhile

  CATCH, /CANCEL
  if (isFile) then FREE_LUN, tar_lun
  tar_lun = 0

  files = files.ToArray(/NO_COPY)
  if (ISA(streamOrig)) then stream = TEMPORARY(streamOrig)
  if (verbose) then begin
    MESSAGE, /INFO, /NONAME, 'Total '+STRTRIM(N_ELEMENTS(files),2)+' files, '+STRTRIM(totalBytes,2)+' bytes'
    toc
  endif
end


function FileTar_Header, filename, finfo, filemode, POSIX=posix
  compile_opt idl2, hidden

  header = BYTARR(512)
  header[257:261] = BYTE('ustar')
  header[263:264] = BYTE('00')
  posix = KEYWORD_SET(posix)
  needPosix = posix
  if (STRLEN(filename) gt 255) then needPosix = 1b

  if (~needPosix) then begin
    if (STRLEN(filename) le 100) $
    then header[0] = BYTE(filename) $
    else begin
      ; Do not use /MARK_DIR (so says the tar spec)
      dir = FILE_DIRNAME(filename)
      name = FILE_BASENAME(filename)
      while (STRLEN(dir) gt 155) do begin
        name = FILE_BASENAME(dir) + '/' + name
        dir = FILE_DIRNAME(dir)
      endwhile
      if (STRLEN(name) gt 100 || dir eq '' || dir eq '.') $
      then needPosix = 1b $ ;MESSAGE, /NONAME, 'Filename too long: "' + filename + '"'
      else begin
        name = STRJOIN(STRTOK(name, '\', /EXTRACT, /PRESERVE), '/')
        dir = STRJOIN(STRTOK(dir, '\', /EXTRACT, /PRESERVE), '/')
        if (finfo.directory) then name += '/'
        header[0] = BYTE(STRMID(name, 0, 100))
        header[345] = BYTE(STRMID(dir, 0, 155))
      endelse
    endelse
  endif

  if (needPosix) then begin
    fakeFilename = './PaxHeaders/' + FILE_BASENAME(filename)
    header[0] = BYTE(STRMID(fakeFilename, 0, 99))
    typeflag = 'x'
    fmode = "644   ; rw-r--r--
    time = ULONG64(SYSTIME(1,/UTC))
    record = ' path=' + filename + STRING(10b)
    len = STRLEN(record)
    for digit=1,9 do begin
      if (len lt (10ull^digit - digit)) then break
    endfor
    len += digit
    n = 512*((len + 511ull)/512)
    paxdata = BYTARR(n)
    paxdata[0] = BYTE(STRTRIM(len,2) + record)
    ; Size of the extended PAX header
    nbytes = len
    fakename = FILE_BASENAME(filename)
    if (finfo.directory) then fakename += '/'
    newheader = FileTar_Header(STRMID(fakename,0,99), finfo, filemode)
  endif else begin
    typeflag = finfo.directory ? '5' : '0'
    fmode = filemode
    nbytes = finfo.size
    time = finfo.mtime
  endelse

  header[156] = BYTE(typeflag)
  header[100:106] = BYTE(STRING(fmode, FORMAT='(o7.7)'))
  header[124:134] = BYTE(STRING(nbytes, FORMAT='(o11.11)'))
  header[136:146] = BYTE(STRING(time, FORMAT='(o11.11)'))

  ; To compute the header checksum
  header[148:155] = 32b ;fill the checksum value with space characters
  checksum = TOTAL(header, /INTEGER) ;then add up the entire header
  header[148:153] = BYTE(STRING(checksum, FORMAT='(o6.6)'));Then fill in the checksum value.
  header[154:155] = [0b, 32b]

  if (needPosix) then header = [header, paxdata, newheader]
  RETURN, header
end


pro sydTar, filesIn, outfileIn, $
  BUFFER=buffer, $
  FILES=files, $
  GZIP=gzip, $
  LIST=listIn, $
  POSIX=posix, $
  VERBOSE=verboseIn

  compile_opt idl2, hidden
  if (N_PARAMS() eq 0) then MESSAGE, 'Incorrect number of arguments.'

  sep = PATH_SEP()
  files = ''
  fileList = LIST()
  filenames = LIST()
  doList = KEYWORD_SET(listIn)
  verbose = KEYWORD_SET(verboseIn)
  hasBuffer = ARG_PRESENT(buffer)
  if (verbose) then tic
  
  if (N_PARAMS() eq 2 && hasBuffer) then MESSAGE, 'BUFFER keyword can not be used with FileOut argument.'
  if (ISA(buffer) && ~hasBuffer) then MESSAGE, 'BUFFER must be set to a named variable, not an expression.'

  for i=0, N_ELEMENTS(filesIn)-1 do begin
    if (~FILE_TEST(filesIn[i], /READ)) then MESSAGE, 'Unable to open file: ' + filesIn[i]
    if (FILE_TEST(filesIn[i], /DIRECTORY)) then begin
      dir = filesIn[i]
      c = STRMID(dir, 0, /REVERSE_OFFSET)
      if (c ne '/' && c ne '\') then dir += sep
      ; Remove the directory's parent from both the directory and all of the contained files/subdirectories.
      parentdir = FILE_DIRNAME(dir)
      parentdir = (parentdir ne '.') ? parentdir + sep : ''
      parentlen = STRLEN(parentdir)
      fileList.Add, dir
      if (~doList) then filenames.Add, FILE_BASENAME(dir) + sep
      subfiles = FILE_SEARCH(dir, '*', COUNT=nf, /MARK_DIR, /MATCH_INITIAL_DOT)
      for j=0,nf-1 do begin
        subfile = subfiles[j]
        c = STRMID(subfile, 0, /REVERSE_OFFSET)
        isDir = (c eq '/' || c eq '\')
        if (~isDir && ~FILE_TEST(subfile, /REGULAR)) then continue
        if (~STRCMP(subfile, dir, parentlen, /FOLD_CASE)) then continue
        fileList.Add, subfile
        if (~doList) then filenames.Add, STRMID(subfile, parentlen)
      endfor
    endif $
    else if (FILE_TEST(filesIn[i], /REGULAR)) then begin
      ; TODO: Filter out symlinks?
      fileList.Add, filesIn[i]
      if (~doList) then filenames.Add, FILE_BASENAME(filesIn[i])
    endif
  endfor

  files = fileList.ToArray(/NO_COPY)
  fileList = 0
  filenames = filenames.ToArray(/NO_COPY)
  nfiles = N_ELEMENTS(files)

  if (doList) then begin
    if (verbose) then foreach f, files do MESSAGE, /INFO, /NONAME, f
    RETURN
  endif

  if (N_PARAMS() eq 1 || hasBuffer) then begin
    if (nfiles eq 0) then MESSAGE, 'Incorrect number of arguments.'
    fname = FILE_BASENAME(filesIn[0])
    dotPos = STRPOS(fname, '.', /REVERSE_SEARCH)
    if (dotPos gt 0) then fname = STRMID(fname, 0, dotPos)
    outfile = FILE_DIRNAME(filesIn[0], /MARK) + fname + '.tar'
  endif else begin
    if (N_ELEMENTS(outfileIn) gt 1) then MESSAGE, 'Output file must be a scalar string.'
    outfile = outfileIn[0]
    outfilegz = outfile
    hasDot = STRPOS(outfile, '.', /REVERSE_SEARCH)
    sufflow = STRLOWCASE((hasDot ge 0) ? STRMID(outfile, hasDot) : '')
    if (sufflow eq '.gz' || sufflow eq '.tgz') then begin
      gzip = 1b
      outfile = STRMID(outfile, 0, hasDot)
    endif else if (KEYWORD_SET(gzip)) then outfilegz += '.gz'
  endelse

  if (~hasBuffer) then begin
    if (~FILE_TEST(FILE_DIRNAME(outfile), /WRITE)) then MESSAGE, 'Unable to write to the file: ' + outfile
    OPENW, tar_lun, outfile, /GET_LUN
  endif else outlist = LIST()

  total_size = 0LL
  for i=0, nfiles-1 do begin
    if (sep eq '\') then filenames[i] = STRJOIN(STRTOK(filenames[i], '\', /EXTRACT, /PRESERVE), '/')
    finfo = FILE_INFO(files[i])
    void = FILE_TEST(files[i], GET_MODE=filemode)
    file_arr = []

    if (finfo.regular && finfo.size gt 0) then begin
      lun = 0
      CATCH, iErr
      if (iErr ne 0) then begin
        if (lun ne 0) then FREE_LUN, lun
        CATCH, /CANCEL
        goto, handleError
      endif
      OPENR, lun, files[i], /GET_LUN
      file_arr = BYTARR(finfo.size, /NOZERO)
      READU, lun, file_arr
      FREE_LUN, lun
      lun = 0
    endif else finfo.size = 0
    total_size += finfo.size

    header = FileTar_Header(filenames[i], finfo, filemode, POSIX=posix)
    if (hasBuffer) then outlist.Add, header else WRITEU, tar_lun, header

    if (finfo.size gt 0) then begin
      nextra = 512 - finfo.size mod 512
      if (hasBuffer) then outlist.Add, file_arr else WRITEU, tar_lun, file_arr
      if (nextra gt 0 && nextra lt 512) then begin  
        if (hasBuffer) then outlist.Add, BYTARR(nextra) else WRITEU, tar_lun, BYTARR(nextra)
      endif 
    endif

    if (verbose) then begin
      msg = files[i]
      if (finfo.size gt 0) then msg += ', ' + STRTRIM(finfo.size,2) + ' bytes'
      MESSAGE, /INFO, /NONAME, msg
    endif
  endfor

  if (hasBuffer) then begin
    outlist.Add, BYTARR(1024)
    buffer = outlist.ToArray(DIMENSION=1, /NO_COPY)
    outlist = 0
  endif else begin
    WRITEU, tar_lun, BYTARR(1024)
    FREE_LUN, tar_lun, /FORCE
  endelse
  tar_lun = 0

  if (verbose) then begin
    bytes = STRTRIM(total_size,2) + ' bytes'
    MESSAGE, /INFO, /NONAME, 'Total ' + STRTRIM(nfiles,2) + ' files, ' + bytes
  endif
  if (~ISA(files)) then files = ''
  if (KEYWORD_SET(gzip)) then begin
    if (hasBuffer) $
    then buffer = ZLIB_COMPRESS(buffer, /GZIP_HEADER) $
    else if (ISA(outfilegz)) $
      then FILE_GZIP, outfile, outfilegz, /DELETE, VERBOSE=verbose $
      else FILE_GZIP, outfile, /DELETE, VERBOSE=verbose
  endif
  if (verbose) then toc
  RETURN

  handleError:
  if (tar_lun ne 0) then FREE_LUN, tar_lun
  FILE_DELETE, outfile, /QUIET
  files = ''
  MESSAGE, /REISSUE_LAST
end