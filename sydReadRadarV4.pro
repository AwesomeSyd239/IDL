;+
;Functions in this file
;TagMatch
;;GetTime
;Squeeze
;GetMeta
;SmartOpen
;Setup
;SmartClose
;ReadFit
;MakeFit2IDL
;GetBZ (for bzipped files)
;-
@'/home/ullrich/syd/sydRadStructs.pro'
@'/home/ullrich/syd/sydReadVecs.pro'
@'/home/ullrich/syd/GetFilesV4.pro'

;+
;Matches tags to tags in dats
;-
Function TagMatch,tags,dats
  i=INTARR(N_ELEMENTS(tags))
  datNames=STRLOWCASE(TAG_NAMES(dats))
  for j=0,N_ELEMENTS(tags)-1 do i[j]=WHERE(datNames eq tags[j])
  RETURN,i
END

;+
;Returns a time array (in floating point hours) from the provided scalars
;-
Function GetTime,type,scls
  time=FLTARR(N_ELEMENTS(scls))
  if type eq 'acf' then time=scls.time.hr+scls.time.mt/60.+scls.time.sc/3600.
  if type eq 'fit' then time=scls.time.hr+scls.time.mt/60.+scls.time.sc/3600.
  if type eq 'red' then time=scls.hour+scls.minut/60.+scls.sec/3600.
  if type eq 'vec' then begin
    hr=scls[WHERE(scls.name eq 'time.hr')].ptr
    mt=scls[WHERE(scls.name eq 'time.mt')].ptr
    sc=scls[WHERE(scls.name eq 'time.sc')].ptr
    for i=0,N_ELEMENTS(scls)-1 do time[i]=*hr[i]+*mt[i]/60.+*sc[i]/3600.
  endif
  RETURN,time
END


;+
;Turns fit into my custom idl structure
;type   str       'acf', 'red', or 'fit' will go normal, 'vec' is my old sideways reader
;count  int       the output from sydReadFit, should be the same as n_elements(dats)
;dats   [SDdata
;scls   [SDscalar]
;-
Function Squeeze,type,count,dats,scls
  ;Set up final structure
  if count ne N_ELEMENTS(dats) then MESSAGE,"bad data count"
  fin_=REPLICATE(sydFitDef(type='acf2'),count)
  fin_.t=GetTime(type,scls);Time is first

  tags=STRLOWCASE(TAG_NAMES(fin_))
  if TOTAL(type eq ['acf','red','fit']) then begin
    gates=scls.rsep/45.
    ;    m=TOTAL(gates ne 1)
    ;    if m gt 0 then begin
    ;      h=HISTOGRAM(scls.rsep,binsize=15,locations=l)
    ;      PRINT,STRING(FORMAT="odd ranges on month:%i  day:%i",scls[0].time.mo,scls[0].time.dy)
    ;      PRINT,STRING(format='range:%i, num:%i    ',TRANSPOSE([[l],[h]]))
    ;    endif
    STRUCT_ASSIGN,scls,fin_,/NOZERO ;If someone went to the trouble of sorting the tags, we may as well use them

    fin_.slist=PTRARR(count,/ALLOCATE_HEAP) ;@WIP
    tagId=TagMatch(tags,dats[0]) ;Assumes that dats are always in the same order, dangerous but fast
    tInds=WHERE(tagId ne -1,nti)
    tagId=tagId[tInds]
    for i=0,count-1 do begin
      *fin_[i].slist=WHERE(dats[i].qflg ne 0)*gates[i]
      for j=0,nti-1 do fin_[i].(tInds[j])=PTR_NEW(dats[i].(tagId[j])[*fin_[i].slist],/no_copy)
    endfor
  endif else if type eq 'vec' then begin
    for j=0,N_ELEMENTS(tags)-1 do begin
      if tags[j] eq 't' then continue ;already matched time
      sclid=WHERE(scls.name eq tags[j],cnt) ;see if we have the tag in our scls
      if cnt ne 0 $
        then for i=0,count-1 do fit_[i].(j)=(*(scls[sclid[i]].ptr)) $ ;assign from scls
      else begin
        arrid=WHERE(dats.name eq tags[j],cnt)
        ad=arrid/ awid
        arrid=arrid[0] mod awid
        if cnt ne 0 $
          then fit_[ad].(j)=REFORM(dats[arrid,ad].ptr) $
        else PRINT,'unmatched tag: ',tags[j]
      endelse
    endfor
  endif else PRINT, "Unknown type"+type
  RETURN,fin_
END


;+
;Automatically detects file type and compression, then opens the file properly
;Gets a LUN for the opened file, and tells you the type
;file    = file name
;fitfp   = where the LUN will go
;type    = where the type will go
;-
Pro SmartOpen,file,fitfp,type,comp=comp,cworks=cworks
  case type of
    'idl':a=0 ;pass
    'red':OPENR,fitfp,file,/get_lun,compress=(STRMID(file,1,2,/REVERSE_OFFSET) eq 'gz'),/swap_if_little_endian
    'fit':fitfp=oldFitOpen(file);
    'acf':fitfp=fitOpen(file,/read) ;yes this is silly, but copy/pasting that function doesn't work
    else:PRINT,'SmartOpen encountered unkown file type',file
  endcase
  compiled=(ISA(cworks)?cworks:1) ;Compiled C code means the RST's version is fastest
  if ~compiled then begin
    if type eq 'fit' then fitfp=fitfp.fitunit
    type='vec' ;vec is my idl way
  endif
END


;+
;Reads a small part of a file in order to find the number of range gates
;Also will create a space in which you should store the data
;fitfp   = a pointer to the file
;type    = the type of the data ('acf', etc.)
;ranges  = retruns the number of range gates
;dats    = returns where array data should go
;scls    = returns where scalar data should go
;chunks  = returns the data chunks (vec type only)
;n_rec   = returns the number of data points (vec type only)
;fit1    = a sample data, to be used for struct assign
;if type eq 'vec' then begin     ;my way
;-
Pro SetUpVec,fitfp,ranges,dats,scls,chunks,n_rec
  common bytes, typeSize ,ind
  typeSize=[0,1,2,4,4,0,0,0,8]
  ind=16
  n_rec=Chunky(fitfp,chunks,swid,awid)
  scl=BinScalars(*chunks[0].dat,chunks[0].snum)
  ranges=*scl[WHERE(scl.name eq 'nrang')].ptr
  scls=REPLICATE({scalar, name:'',type:0B,ptr:PTR_NEW(0)},swid,n_rec)
  dats=REPLICATE({array, name:'',type:0B,ptr:PTR_NEW(/ALLOCATE_HEAP)},awid,n_rec)
end

Pro SetUp,fitfp,type,ranges,dats,scls,n_rec,fit1
  if ~KEYWORD_SET(n_rec) then n_rec=30000;usually files have ~23k
  if type eq 'red' then begin ;cat way
    fit1=sydFitDef(type='red',ranges=300,simple=0) ;Temporary placeholder
    READU, fitfp, fit1 ;because the reading is imprecise
    ranges=fit1.p.nrang
  endif else if type eq 'acf' then begin ;rst way
    dumb=fitRead(fitfp, prm, fit1)
    if dumb lt 0 then begin
      PRINT,'problem!'
      n_rec=-1
      RETURN
    endif
    ranges=prm[0].nrang
    scls=REPLICATE(prm, n_rec)
  endif else if type eq 'fit' then begin ;old way
    dumb=oldFitRead(fitfp,prm,fit1)
    ranges=prm[0].nrang
    scls=REPLICATE(prm, n_rec)
    POINT_LUN,fitfp.fitunit,0
    rlen=0
    READU,fitfp.fitunit,rlen
    POINT_LUN,fitfp.fitunit,rlen
  endif
  fit1=sydFitDef(type=type,ranges=ranges, simple=1)
  dats=REPLICATE(fit1,n_rec);Should be enough for all the data
  if type ne 'fit' then POINT_LUN,fitfp,0 ;Saves the trouble of close/reopen by seeking to file start
END


;+
;Closes the file properly, @might not work on 'fit' type
;-
Pro SmartClose,fitfp,type
  if type eq 'fit' then begin
    FREE_LUN,fitfp.fitunit
    if (fitfp.inxunit ne -1) then FREE_LUN, fitfp.inxunit
    CLOSE,fitfp.fitunit
  endif else begin
    FREE_LUN, fitfp
    CLOSE, fitfp
  endelse
END

PRO ForceFitToDay,dats,scls,count
  common SingleDate,year,month,day
  common FitDataOverDay,exdat,exscl
  nd=N_ELEMENTS(dats) & nx=0
  if ISA(exDat) then begin
    nx=N_ELEMENTS(exdat)
    ;  PRINT,STRING(FORM="exdats has %g @ %g",nx, N_ELEMENTS(exdat[0].qflg)))
    if exscl[0].nrang eq scls[0].nrang then begin
      dats=[dats,exDat]
      scls=[scls,exScl]
    endif
    exDat=!Null
    exScl=!Null
  endif
  isExtra=(scls.time.dy ne day or scls.time.mo ne month)
  count=nd+nx
  exInd=WHERE(isExtra,c1)
  if c1 ne 0 then begin
    exDat=dats[exInd]
    exScl=scls[exInd]
    if exscl[0].nrang ne N_ELEMENTS(exdat[0].qflg) then $;TODO Make someone explain how this can ever happen
      for i=0,c1-1 do exscl[i].nrang = N_ELEMENTS(exdat[i].qflg)
    goodInd=WHERE(~isExtra,count)
    dats=dats[goodInd]
    scls=scls[goodInd]
  endif
  PRINT,STRING(form="%-i_%-i has nrang= %i and %-i base, %-i total, %-i good, and %-i bad points",$
    month,day,scls[0].nrang,nd,nd+nx,count,c1)
END

;+
;Reads a fit file and gets the scalars and arrays
;file = name string (can be gotten with GetFile) or opened file (pointer)
;type = type (as found by getmeta)
;dats = where the arrays go (the normal fit structure)
;scls = where the scalars go (some people call it PRM)
;verb = set keyword to report time
;-
FUNCTION sydReadFit,file=file,type=type,dats=dats,scls=scls,verb=verb,cworks=cworks
  st=SYSTIME(/seconds)
  if ~KEYWORD_SET(file) then file=DIALOG_PICKFILE(get_path=g_path,path='/data/', /read) ;If no filename supplied then open the search window
  count=LONG(0) ;Record counter
  if ~FILE_TEST(file) then MESSAGE,'no file';return, -1
  if ~KEYWORD_SET(type) then GetMeta,file,type,comp=comp
  if type eq 'idl' then begin
    RESTORE,file
    RETURN,fit;n_elements(dats)
  endif
  SmartOpen,file,fitfp,type,comp=comp,cworks=cworks
  a=FSTAT(fitfp)
  if a.size eq 0 then goto,cls
  case type of
    'vec':SetUpVec,fitfp,type,ranges,dats,scls,chunks,n_rec
    else:    Setup,fitfp,type,ranges,dats,scls,n_rec,fit1
  endcase
  lim=-1L
  CATCH,err
  if err ne 0 then begin
    dats=[dats,REPLICATE(fit1,n_rec)]
    if type eq 'acf' or type eq 'fit' then scls=[scls,REPLICATE(prm,n_rec)]
    PRINT,!error_state.msg
    if lim gt 4*n_rec then begin
      CATCH,/cancel;break for real problems
      print,"Giving up"
      goto, cls
    endif

  endif
  lim+=n_rec ;Used to expand arrays if needed

  ;Reading and filling the arrays until there is no more records
  if n_rec gt 0 then $
    case type of
    'acf': while fitRead(fitfp,prm,fit) gt -1 do begin
      STRUCT_ASSIGN, fit, fit1
      dats[count]=fit1
      scls[count]=prm
      count+=1
      if count ge lim then MESSAGE,'too big';begin ;Expand arrays if needed
      ;dats=[dats,REPLICATE(fit1,n_rec)]
      ;scls=[scls,REPLICATE(prm,n_rec)]
      ;lim+=n_rec
      ;if count ge 8*lim then  message,'file massively exceeds normal size, please check'
      ;  break
      ;endif
      ;endif
    endwhile
    'fit': while oldfitread(fitfp,prm,fit) gt -1 do begin
      STRUCT_ASSIGN, fit, fit1
      ;point_lun,-fitfp.fitunit,pos
      ;print,"point number ",count,"at byte",pos
      dats[count]=fit1
      scls[count]=prm
      count+=1
      if count ge lim then begin ;Expand arrays if needed
        dats=[dats,REPLICATE(fit1,n_rec)]
        scls=[scls,REPLICATE(prm,n_rec)]
        lim+=n_rec
      endif
    endwhile
    'vec': begin
      sclinds=[0,TOTAL(chunks.snum,/CUMULATIVE)]
      arrinds=[0,TOTAL(chunks.anum,/CUMULATIVE)]
      for i=0,n_rec-1 do begin
        scls[0,i]=BinScalars(*chunks[i].dat,chunks[i].snum)
        dats[0,i]=BinArrays(*chunks[i].dat,chunks[i].anum)
      endfor
    end
    'red': while not EOF(fitfp) do begin
      READU, file, dats[count]
      count+=1
      if count ge lim then begin ;Expand arrays if needed
        dats=[dats,REPLICATE(dats[0],n_rec)]
        lim+=n_rec
      endif
    endwhile
  endcase else PRINT,file+' had problem'

  cls:
  if type ne 'vec' then dats=dats[0:count-1] ;Trims off empty data
  if type eq 'red' then scls=fit_.p
  if type eq 'fit' then scls=scls[0:count-1]
  if type eq 'acf' then scls=scls[0:count-1]

  SmartClose,fitfp,type
  if KEYWORD_SET(verb) then PRINT,'read ',file,' and got ',STRING(format='%i',count),' data points in ',STRING(format='%f',SYSTIME(/seconds)-st),' seconds'
  CLOSE, /all
  RETURN,count
END

@'/home/ullrich/syd/radParseV3.pro'
;+
;You tell it what data you want, then it gets that data
;IPUTS:
;years    [int]
;months   [int]
;days     [int]
;radars   [str]
;noTar    bits  0:don't look for tars  1:don't make tars of output idl files
;verb     bits
;  0:years
;  1:months
;  2:days
;  3:tar timing
;  4:profiler
;handPick bool if set, will ask for help finding files
;decrash  bool goes into my way of reading, avoids c crashes on bad files
;
;Requires:
;  sydCommonV2.pro
;
;WARNING: DELETES LOCAL SOURCE FILES
;REQUIRES (up to) 3 GB BUFFER SPACE
;The data will be changed down this list:
;gz -> acf -> idl ->tar.gz
;defaults:
;  years=2015,months=[1:12],days=[1:31],radars=['inv','cly',rkn']
;-
PRO MakeFit2IDL,years=years,months=months,days=days,radars=radars,$
  noTar=noTar,verb=verb,handPick=handPick,decrash=decrash,fileverb=fileverb
  Common SingleDate,year,month,day
  common FitDataOverDay,exdat,exscl
  exDat=!Null
  exScl=!Null
  radars=RadParseV3(radars)
  PROFILER,/RESET
  PROFILER,/SYSTEM & PROFILER
  cworks=1-KEYWORD_SET(decrash) ;does the c code work?
  st=SYSTIME(/seconds)
  if ~ISA(verb) then verb=1
  if ~ISA(noTar) then noTar=0
  PRINT, 'Making starts at:', SYSTIME(0)
  if ~KEYWORD_SET(radars) then radars=['inv','cly','rkn','dce','mcm.a']
  @'/home/ullrich/syd/sydCommonV2.pro'
  dirPref=[this_dir+'data'+s+'fitidl'+s,$
    this_dir+'data'+s+'fitcon'+s,''+s+'data'+s+'fitcon'+s,$
    this_dir+'data'+s+'fitred'+s];,''+s+'data'+s+'fitred'+s]
  for year=years[0],years[-1] do begin
    yrTime=SYSTIME(/seconds)
    md=[31,28+(~(year mod 4)),31,30,31,30,31,31,30,31,30,31]<N_ELEMENTS(days)
    foreach month,months do begin
      days=[1:md[month-1]]
      ;Loop over radars, grab a month worth, then get meta info
      foreach radn,radars.name do begin
        moTime=SYSTIME(/seconds)
        tarCount=~(noTar and 1)
        ;Get the files for this month (only do 1 at a time, otherwise files get big)
        PRINT,STRING(FORMAT='Starting on radar %s, month %-i, year %-i',radn,month,year)
        files=REFORM(GetFilesV4(dirPref,radn,year,month,days,$
          noask=~KEYWORD_SET(handPick),verb=fileverb,tarCount=tarCount))
        GetMeta,files,types
        typestr=STRJOIN(types,' ')
        PRINT,STRING(format='Types for month %-i, radar %s: %s',month,radn,typestr)
        todo=types ne 'ing' and types ne 'idl'

        ;Loop over days, dealing with each and reporting results
        foreach day,days,di do begin
          dyTime=SYSTIME(/seconds)
          type=types[di]
          if todo[di] then begin ;and type eq 'acf'
            ;Read an ACF file to make an idl file, then delete the ACF
            dats=0 &    scls=0
            pts=sydReadFit(file=files[di],type=type,scls=scls,dats=dats,cworks=cworks)
            forcefittoday,dats,scls,pts
            if pts eq 0 then MESSAGE,"bad point count";PRINT,'bad file?' else begin
            fit=Squeeze(type,pts,dats,scls)
            fname=STRJOIN([MakePathV4(this_dir+'data/fitidl/',radn,/MAKE),'.idl'])
            save,fit,filename=fname
            FILE_DELETE,files[di]
            files[di]=fname
            word='Converted'
            ;endelse
            CATCH,err
            if err ne 0 then begin
              PRINT,!error_state.msg
            endif
          endif else if type eq 'ing' then word="Didn't Find" else word='Found'
          if (verb and 4) ne 0 then PRINT,STRING(FORMAT=$
            "%s radar %s for month %i, day %i in %6.2f",$
            word,radn,month,day,SYSTIME(/seconds)-dyTime)
        endforeach ;days

        ;Turn the idl files into tar.gz, unless told not to
        if ~(noTar and 2) then begin
          tarTime=SYSTIME(/seconds)
          a=WHERE(types ne 'ing',cnt)
          ;if there are no files, or if there's a full tar, do not bother to make a tar
          if (cnt ne 0) and (tarCount ne cnt) then begin
            PRINT,STRING(format='Tarring %-i Files',cnt)
            sydTar,files[a],STRING(format='%sdata/fitidl/%04i%s%02i.%s.tar.gz',$
              this_dir,year,s,month,radn),/GZIP
            if ((verb and 8) ne 0) then PRINT,STRING(FORMAT='tarring took %6.2f',SYSTIME(/seconds)-tarTime)
          endif
          if cnt ne 0 and ~(noTar and 4) then FILE_DELETE,files[a],/ALLOW_NONEXISTENT ;tar is a copy, so we delete the originals
        endif
        if (verb and 2) ne 0 then PRINT,STRING(FORMAT="done month %i in %6.2f seconds",month,SYSTIME(/seconds)-moTime)
      endforeach ;radars
    endforeach ;months
    if (verb and 1) ne 0 then PRINT,STRING(FORMAT="done year %i in %6.1f seconds",year,SYSTIME(/seconds)-yrTime)
  endfor ;years
  PRINT, 'Making finished at:', SYSTIME(0)
  PRINT, 'Took ',SYSTIME(/seconds)-st,' seconds'
  if (verb and 16) ne 0 then PROFILER,/REPORT,filename='/home/ullrich/syd/makefitProfile'
END


;+
;The purpose is to take in a buinch of 2 hour ACF and make a full day of data
;-
Function AssembleFitCon,fdat=fdat,fscl=fscl,files=files,toidl=toidl,radn=radn,noAsk=noAsk
  if ~ISA(files) then begin
    if ~ISA(radn) then MESSAGE,'need some input'
    ;files=Get2hrFilesFromMaxwell(radn)
    d2u=['/home/ullrich/syd/data/fitcon/','/data/fitacf_30/']
    files=GetDayFile(0,radn,verb=1,noAsk=noAsk)
  endif
  if TOTAL(STRCMP(files,'')) then RETURN,0
  bzp=WHERE(files.endswith('bz2'),bzcnt)
  if bzcnt ne 0 then begin
    getbz2acf,files[bzp],outfiles,outdir='/home/ullrich/syd/data/fitcon/',verb=1
    files[bzp]=outfiles
  endif
  if ~ISA(radn) then begin
    base=FILE_BASENAME(files[0])
    radn=base.extract('[a-z]+')
  endif
  if N_ELEMENTS(files) eq 1 then if files.endswith('idl') then begin
    PRINT,files+' is already done'
    RETURN,files
  endif
  fdat=[] & fscl=[] & pts=0 ;run this if
  foreach f, files ,fi do begin
    good=1
    pt=sydreadfit(file=f,dats=dats,scls=scls)
    if N_ELEMENTS(fdat) gt 0 then good=(scls[0].nrang eq fscl[0].nrang)
    ;otherwise error on conflicting data structures
    ;todo make this actually read the data
    if good then fdat=[fdat,dats]
    if good then fscl=[fscl,scls]
    if good then pts+=pt
    ;todO hope that these are sorted nicely without duplicate data
    ; could use forcefittoday in sydReadradarV4, maybe
  endforeach
  if KEYWORD_SET(toidl) then begin
    PRINT,'squeezing'
    fit=Squeeze('fit',pts,fdat,fscl)
    fname=STRJOIN([MakePathV4('/home/ullrich/syd/data/fitidl/',radn,/MAKE),'.idl'])
    save,fit,filename=fname
    FILE_DELETE,files;2 hour files
    RETURN,fname
  endif
  RETURN,pts
END

PRO AssembleManyFitCon2IDL,radn,yr,mo,dy
  common SingleDate,year,month,day
  stt=SYSTIME(/seconds)
  fnames=[]
  moDy=[31,28+(yr mod 4 eq 0),31,30,31,30,31,31,30,31,30,31]
  if ~KEYWORD_SET(dy) then dy=[1:moDy[mo-1]]
  foreach d,dy do begin
    SetDate,yr=yr,mo=mo,dy=d
    PRINT,'working on '+ymdstr()
    gotten=AssembleFitCon(radn=radn,/toidl,noAsk=noAsk)
    if gotten then fnames=[fnames,gotten]
  endforeach
  PRINT,SYSTIME(),SYSTIME(/seconds)-stt
  localFolder="/home/ullrich/syd/data/fitidl/"+ymFold(/notDir)
  Tar=STRING(format='%s.%s.tar.gz',localFolder,radn)
  PRINT,'tarring'
  sydTar,fnames,tar
  FILE_DELETE,fnames,/allow
end