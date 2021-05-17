@'/home/ullrich/syd/makepathV1.pro'
@'/home/ullrich/syd/getmetaV2.pro'
@'/home/ullrich/syd/sydUntarV1.pro'
;+
;Gets fit files,
;dirPref  [str] directories to look in, favoring first match
;radars   [str] list of 3 letter radar codes
;year
;month
;days
;noAsk
;verb     int   >0:basic   >1:prints files to unzip  
;noZip    bool  if not set, finds an unzips compressed files, otherwise ignores compressed 

;HOLDERS:
;tarCount bool  if set, looks for tars that might contain the desired files
;   then holds the number of sub-files from the tar
;-
Function GetFiles,dirPref,radars,year,months,days,$
  noask=noask,verb=verb,tarCount=tarCount,noZip=noZip
  @'/home/ullrich/syd/sydCommonV2.pro'
  if ~isa(dirpref,/str) then begin
    ds='data'+PATH_SEP()
    dirPref=[this_dir+ds+['fitidl','fitcon']]
    if year lt 2019 then dirPref=[dirPref,s+ds+'fitcon']
    if year ge 2019 then dirPref=[dirPref,s+ds+'fitacf_30']
    dirpref+=path_sep()
  endif
  if ~isa(verb) then verb=0
  if ~ISA(radars,/STRING) then rn=radars.name else rn=radars
  if N_ELEMENTS(tarCount) eq 0 then tarCount=1
  files=REPLICATE('GetFile_Found_Nothing',N_ELEMENTS(radars),N_ELEMENTS(days),N_ELEMENTS(months))
  ;if verb then PRINT,Format="Looking for %-i files",N_ELEMENTS(files)
  for month=0,N_ELEMENTS(months)-1 do begin
    m=months[month]
    folder=STRING(format='%sdata/fitidl/%04i%s%02i',this_dir,year,s,m)
    for rad=0,N_ELEMENTS(radars)-1 do begin
      if KEYWORD_SET(tarCount) then begin
        ;Look for tar files, which I made to save space
        testTar=STRING(format='%s.%s.tar.gz',folder,rn[rad])
        if ~FILE_TEST(testTar) then testTar=STRING(format='%s.tar.gz',folder) ;Check both names syd uses
        if FILE_TEST(testTar) then begin
          if verb then PRINT, STRING(FORMAT='UnTarring %s',testTar)
          sydUntar,testTar,folder,FILES=tarNames
          tarCount=N_ELEMENTS(tarNames)
        endif
      endif

      for day=0,N_ELEMENTS(days)-1 do begin ;Loop over the days
        for try=0,N_ELEMENTS(dirPref)-1 do begin
          ;Check if the file exists, looking in preferred directories first
          for doubleTry=0,(year le 2006) do begin
          toFind=MakePath(dirPref[try],year,m,days[day],rn[rad],modern=(dirpref[try].contains('idl') or doubleTry))
          fname=FILE_SEARCH(toFind+'*',count=cnt)
          if cnt gt 1 then begin
            PRINT,"more than 1 file"
            fname=fname[1]
          endif
          if fname ne '' then break
          endfor
          if fname ne '' then break
        endfor ;trys
        if fname eq '' and ~KEYWORD_SET(noask) then $
          fname=DIALOG_PICKFILE(get_path=g_path,path=g_path,FILTER=STRJOIN(['*',rn[rad],'.*']),title=$
          STRING(Format="Looking for month: %-i, day: %-i,radar: %s",m,days[day],rn[rad]))
        if fname ne '' then          files[rad,day,month]=fname
      endfor
    endfor
  endfor

  GetMeta,files,types,comp=comps;comp notes Files that need to be decompressed
  ;Clean up unwanted files from a tar
  if ISA(tarNames) then for i=0,N_ELEMENTS(tarNames)-1 do begin
    a=WHERE(STRMATCH(files,tarNames[i]),cnt)
    if cnt ne 1 then FILE_DELETE,tarNames[i],/allow ;Stop leaving idl files laying around
    comps[a]=1
  endfor

  a=WHERE(files eq '',cnt)
  if cnt gt 0 then files[a]='GetFile_Found_Nothing'

  if ~keyword_set(noZip) and (verb and 1) then $
    PRINT,STRING(format='Files found: total: %-i ',TOTAL(files ne 'GetFile_Found_Nothing'))
  if (verb and 2) then PRINT,STRING(format=$
    'idl: %-i, tar: %-i acf: %-i, gz: %-i, bz2: %i, red: %i, n/a: %-i',$
    TOTAL((types eq 'idl')*(comps eq 0)),TOTAL((types eq 'idl')*(comps eq 1)),$
    TOTAL((types eq 'acf')*(comps eq 0)),total(comps eq 1),total(comps eq 2),$
    TOTAL(types eq 'red'),TOTAL(types eq 'ing'))

  if total(comps) ne 0 and ~keyword_set(noZip) then begin
    a=WHERE(files ne 'GetFile_Found_Nothing' and ~STRCMP(this_dir,files,STRLEN(this_dir)),cnt)
    if cnt gt 0 then begin
      PRINT,'Unzipping starts:', SYSTIME(0)
      source=files[a]
      if (verb and 4) gt 1 then print,'files',source
      dbl=where(this_dir.endswith(s)*files.startswith(s),cdb)
      if cdb ne 0 then files[dbl]=files[dbl].remove(0,0)
      files[a]=this_dir+files[a].remove(-2-comps[a])
      FILE_MKDIR,FILE_DIRNAME(files[a[0]])
      for i=0,cnt-1 do sydGZIP_COMPRESS, source[i],files[a[i]],/UNCOMPRESS;,/verbose
      PRINT,'Done copy/dezip at',SYSTIME(0)
    endif
    ;Check that they went
    files=GetFiles(dirPref,radars,year,months,days,/noask,/noZip)
  endif
  RETURN,files
END