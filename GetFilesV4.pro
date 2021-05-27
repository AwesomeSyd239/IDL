@'/home/ullrich/syd/sydTime.pro'
@'/home/ullrich/syd/getmetaV2.pro'
@'/home/ullrich/syd/sydUntarV1.pro'
Function ymFold,notDir=notDir
  common SingleDate,year,month,day
  sp=PATH_SEP()
  if KEYWORD_SET(notDir) then RETURN,STRING(FORM="%04i%s%02i%s",year,sp,month)
  RETURN,STRING(FORM="%04i%s%02i%s",year,sp,month,sp)
END

;+
;Gives it a fitacf-like filename, and it will parse some data
;-
Pro FileNameBreak,fileName,year,month,day,radN,type
  ;/home/ullrich/syd/data/fitcon/2016/03/20160301.C0.rkn.fitacf
  parts=STRSPLIT(fileName,'/',/extract)
  last=parts[-1]
  year=STRMID(last,0,4)
  month=STRMID(last,4,2)
  day=STRMID(last,6,2)
  radN=STRMID(last,12,3)
  type=STRMID(last,3,3,/reverse)
END


Function DefaultDir
  common SingleDate,year,month,day
  @'/home/ullrich/syd/sydCommonV2.pro'
  ds='data'+PATH_SEP();i use path sep to make it safe for windows
  dirPref=[this_dir+ds+['fitidl','fitcon']]
  if year lt 2019 then dirPref=[dirPref,s+ds+'fitcon'];but i bet that starting a file name with path sep isn't valid
  if year ge 2019 then dirPref=[dirPref,s+ds+'fitacf_30']
  RETURN,dirPref+s
END

;+
;Puts strings together to form a proper name for a radar data file
;Setting the MAKE keyword will create a directory if one is not found
;modern keyword controls the use of C0 in file names
;-
Function MakePathV4,dir,rad,make=make,modern=modern
  common SingleDate,year,month,day
  s=PATH_SEP() ;/ for unix, \ for windows
  data_dir=dir+ymFold()
  filename=dir+ymFold()+ymdStr()
  if KEYWORD_SET(make) and ~FILE_TEST(data_dir) then begin
    SPAWN,'mkdir --parents '+data_dir
    PRINT,'made directory',data_dir
  endif
  ;if ~isa(modern) then begin
  modern=0
  if (year+month/12. GE 2006+7./12.) then modern=1
  if year ge 2019 then modern=2
  ;endif
  if dir.contains('idl') or KEYWORD_SET(make) then modern=1
  case modern of
    2:filename=filename+'*'+rad
    1:filename=filename+'.C0.'+rad ;do modern file names when making
    0:begin
      rs=strsplit('gbr,sch,kap,hal,sas,pgr,kod,sto,pyk,han,san,sys,sye,tig,ker,ksr,unw,wal',/extract,',')
      rc=strsplit('g,s,k,h,t,b,a,w,e,f,d,j,n,r,p,c,u,i',/extract,',')
      radCode=rc[WHERE(rs eq rad,cnt)]
      if cnt ne 1 then MESSAGE,'code table is sad'
      filename=data_dir+date+'00'+radcode+'C'
    end
  ENDCASE
  if KEYWORD_SET(make) then filename=filename+'.fit'
  RETURN,filename
END


PRO FindSydTar,tarCount,tarNames,radn
  common SingleDate,year,month,day
  ;Look for tar files, which I made to save space
  localFolder="/home/ullrich/syd/data/fitidl/"+ymFold(/notDir)
  testTar=STRING(format='%s.%s.tar.gz',localFolder,radn)
  if ~FILE_TEST(testTar) then testTar=STRING(format='%s.tar.gz',localFolder) ;Check both names syd uses
  if FILE_TEST(testTar) then begin
    if KEYWORD_SET(verb) then PRINT, STRING(FORMAT='UnTarring %s',testTar)
    sydUntar,testTar,localFolder,FILES=tarNames
    tarCount=N_ELEMENTS(tarNames)
  endif
END
;+
;Gets data file for 1 day (use setDate,year,month,day) by searching through directories
;CALL:   GetDayFile,dirPref,radn,verb=verb,pick=pick
;INPUTS:
;dirPref [str]  list of directories to search in {DefaultDir()}
;radn     str   string name of radar code, for example "rkn"
;verb     bool  prints results
;pick     bool  if set, will bring up the dialog to find files by hand
;RETURNSL
;fname   [str]  file names
;-
Function GetDayFile,dirPref,radn,verb=verb,pick=pick
  common SingleDate,year,month,day
  if ~ISA(dirpref,/str) then dirPref=DefaultDir()
  fname='';start with empty string
  foreach dir,dirPref,di do $;Check if the file exists, looking in preferred directories first
    if TOTAL(STRCMP(fname,'')) then begin ;only do while we have no file
    srch:
    toFind=MakePathV4(dir,radn,modern=modern)
    fname=FILE_SEARCH(toFind+'*',count=cnt)
    if modern lt 2 and cnt gt 1 then begin
      PRINT,"GetDayFile found unexpected matches for "+toFind+': '+$
        STRJOIN(fname.substring(STRLEN(tofind)),' ',/single)
      fname=fname[0]
    endif
  endif ;trys

  if cnt eq 0 and KEYWORD_SET(pick)  then begin;and ~picked
    fname=DIALOG_PICKFILE(get_path=dir,FILTER=STRJOIN(['*',radn,'.*']),/multiple,title=$
      STRING(Format="Looking for month: %-i, day: %-i,radar: %s",month,day,radn))
    ;hitting cancel returns empty strings, otherwise set current path to chosen
    if TOTAL(STRCMP(fname,'')) then begin
      dirPref=[dirPref,dir];add path to dirpref
      if modern eq 2 then goto,srch;loop back to grab all mini-files
    endif else pick=0 ;prevent spam of searching for non-existent files
  endif
  if KEYWORD_SET(verb) then PRINT,STRING(FORM='GetDayFile got %i files',N_ELEMENTS(fname)-TOTAL(STRCMP(fname,'')))
  RETURN,fname
END


Pro GetBZ2ACF,sourceFiles,destFiles,outdir=outdir,verb=verb
  common SingleDate,year,month,day
  stt=SYSTIME(/seconds)
  if ~ISA(outDir) then outDir="/home/ullrich/syd/data/fitcon/"
  if ~ISA(destFiles) then destFiles=outDir+ymFold()+FILE_BASENAME(sourceFiles,'.bz2')
  needsGet=(1-FILE_TEST(destFiles));<(KEYWORD_SET(reGet))
  foreach file,sourceFiles,fi do if needsGet[fi] then begin;skip files already here
    bzhere=destFiles[fi]+'.bz2'
    if FILE_TEST(sourceFiles[fi]) then begin ;this should always be true if your source files are valid
      if KEYWORD_SET(verb) then PRINT,'copying bz to'+bzhere
      SPAWN,STRJOIN(["cp",file,bzHere]," ")
    endif else PRINT,file+' not found'
    SPAWN,"bunzip2 -d "+bzHere;bunzip command
    if FILE_TEST(destFiles[fi]) then FILE_DELETE,bzHere,/allow ;we got the file, kill the bz
  endif else PRINT,'already have '+destFiles[fi]+' skipping'
  if KEYWORD_SET(verb) then PRINT,STRING(FORM=$
    'BZ2ACF did %i files in %f seconds',N_ELEMENTS(destFiles),SYSTIME(/seconds)-stt)
END


;Yes
Function Get2hrFilesFromMaxwell,radn
  common SingleDate,year,month,day
  RETURN,FILE_SEARCH('/data/fitacf_30/'+ymFold()+ymdStr()+'*'+radn)
END


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
Function GetFilesV4,dirPref,radars,yearin,months,days,$
  noask=noask,verb=verb,tarCount=tarCount,noZip=noZip
  @'/home/ullrich/syd/sydCommonV2.pro'
  common SingleDate,year,month,day
  if ISA(yearin) then setDate,yr=yearin,mo=months[0],dy=1 else begin;might need to initialize SingleDate
    yearin=year;get from the singleDate block
    months=month
  endelse
  if ~KEYWORD_SET(noAsk) then noask=0
  if ~ISA(dirpref,/str) then dirPref=DefaultDir()
  if ~ISA(verb) then verb=0
  if ~ISA(radars,/STRING) then radns=radars.name else radns=radars
  if N_ELEMENTS(tarCount) eq 0 then tarCount=1
  files=REPLICATE('GetFile_Nothing',N_ELEMENTS(radars),N_ELEMENTS(days),N_ELEMENTS(months))
  ;if verb then PRINT,Format="Looking for %-i files",N_ELEMENTS(files)
  foreach month,months,mi do  foreach nam,radns,ri do begin
    pick=1-noask
    if KEYWORD_SET(tarCount) then FindSydTar,tarCount,tarNames,nam
    foreach day,days,di do begin
      setDate,yr=yearin,mo=month,dy=day
      files[ri,di,mi]=GetDayFile(dirPref,nam,pick=pick)
    endforeach
  endforeach

  GetMeta,files,types,comp=comps;comp notes Files that need to be decompressed
  ;Clean up unwanted files from a tar
  if ISA(tarNames) then foreach tn,tarNames do begin
    a=WHERE(STRMATCH(files,tn),cnt); i guess this will kill tars if things are weird?
    if cnt ne 1 then MESSAGE,'something strange';FILE_DELETE,tn,/allow ;Stop leaving idl files laying around
    ; comps[a]=1 idk why this was here
  endforeach

  missing=(files eq '')
  missingInd=WHERE(missing,cnt)
  if cnt gt 0 then files[missingInd]='GetFile_Nothing'

  if ~KEYWORD_SET(noZip) and (verb and 1) then $
    PRINT,STRING(format='Files found: total: %-i ',TOTAL(1-missing))
  if (verb and 2) then PRINT,STRING(format=$
    'idl: %-i, tar: %-i acf: %-i, gz: %-i, bz2: %i, red: %i, n/a: %-i',$
    TOTAL((types eq 'idl')*(comps eq 0)),TOTAL((types eq 'idl')*(comps eq 1)),$
    TOTAL((types eq 'acf')*(comps eq 0)),TOTAL(comps eq 1),TOTAL(comps eq 2),$
    TOTAL(types eq 'red'),TOTAL(types eq 'ing'))
  if total(comps) ne 0 then uncompressFiles,files,radars,comps
  RETURN,files
END

pro uncompressFiles,files,radars,comp
  if ~ISA(comps) then getMeta,files,types,comp=comp
  compInd=WHERE(comp ne 0,ccnt)
  if ccnt ne 0 and ~KEYWORD_SET(noZip) then begin
    source=files
    PRINT,'Unzipping starts:', SYSTIME(0)
    s=path_sep()
    this_dir='/home/ullrich/syd/'
    dbl=WHERE(this_dir.endswith(s)*files.startswith(s),cdb);prevent double file sep
    if cdb ne 0 then files[dbl]=files[dbl].remove(0,0)
    files[compind]=this_dir+files[compind].remove(-2-comp[compind])
    outDir=FILE_DIRNAME(files[compind[0]])
    FILE_MKDIR,outdir
    gzp=WHERE(comp eq 1,gzc)
    bzp=WHERE(comp eq 2,bzc)
    if bzc ne 0 then getbz2acf,source[bzp],files[bzp]
    if gzc ne 0 then foreach gzi,gzp do sydGZIP_COMPRESS, source[gzi],files[gzi],/UNCOMPRESS;,/verbose
    PRINT,'Done copy/dezip at',SYSTIME(0)
    ;Check that they went
;should never be needed    files=GetFilesV4(dirPref,radars,/noask,/noZip)
  endif else PRINT,"All files are uncompressed"
END

PRO Squish,years,radn
  data_dir='/home/ullrich/syd/data/'
  if ~ISA(radn) then radn='rkn'
  foreach year,years do begin
    fullStr=data_dir+STRING(FORM="vel/%4i/",year)
    filesIn=FILE_SEARCH(fullStr+'r'+radn+'*')
    fileOut=fullStr+STRING(FORM="%4i.%s.tar.gz",year,radn)
    if N_ELEMENTS(filesin) ne 12 then MESSAGE,"could not find files"
    if ~FILE_TEST(fileOut) then begin
      sydtar,filesIn,fileOut,/gzip
      if FILE_TEST(fileOut) then begin
        if ~FILE_TEST(fileout.remove(-3)) then FILE_DELETE, filesIn else MESSAGE,'failed to cleanup'
      endif else MESSAGE, 'failed to tar files'
    endif else MESSAGE, file+' alreacdy exists'
  endforeach
END