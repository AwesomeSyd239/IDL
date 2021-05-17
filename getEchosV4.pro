@'/home/ullrich/syd/getmetaV2.pro'
@'/home/ullrich/syd/GetFilesV4.pro'
@'/home/ullrich/syd/filterarrsV3.pro'
;@'/home/ullrich/syd/radParseV3.pro'
;@'/home/ullrich/syd/sydReadRadarV4.pro'
;@'/home/ullrich/syd/sydElev.pro'


;
;+
;This can be re-purposed for reading odd data from ACF files, I already have ground
;;Read an ACF file to make an idl file, then delete the ACF
;needsmake(files[0],tag='pwr',{name:'rkn',beams:[0,15],gates:[0:50]},[8,11,14])
;RETURNS
;ret [gate,beam,freq,time] Based on whatever tag you asked for
;-
Function NeedsMake,file,tag=tag,rad,freqs
  pts=sydReadFit(file=file,type=type,scls=scls,dats=dats,/verb)
  forcefittoday,dats,scls

  ret=FLTARR(51,16,2,96);51 gates by 16 beams by 2 freq by 96 times
  if ~isa(freqs) then freqs=[8,11,14]
  if MAX(freqs) lt 1000 then freqs*=1000
  ;Filter out unwanted beams
  tags=tag_names(dats[0])
  tagInd=where(strmatch(tags,tag,/fold),cnt)
  if cnt eq 0 then begin
    print,tags
    return,tags
  endif
  t=getTime('acf',scls)
  val=dats.(tagInd) ;discard all info other than the requested
  val=val[0:50,*] ;only want first 50 gates
  if STRCMP(tag,'pwr',3,/fold) then val=10^(val/10.)
  for beam=rad.beams[0],rad.beams[-1] do begin
    for fr=0,1 do begin
      bf=WHERE($
        scls.bmnum eq beam and $
        scls.tfreq ge freqs[fr] and $
        scls.tfreq le freqs[fr+1],cnt)
      if cnt eq 0 then continue
      valFil=val[*,bf]
      ;ScreenPlot
      ;filtering to remove stuff
      ;a=fd.qflg and $
      ;    (abs(fd.v) ge 10) and (abs(fd.v) le 2000) and $
      ;    (fd.p_l ge 3) and (fd.p_l le 70) and $
      ;    (fd.w_l ge 20) and (fd.w_l le 500)
      ;fd=fd[a]

      ;Plot,t*60mean(phi0[10:30,*],dim=1,/nan)*180.0/!DPI ;phi0
      ;ele=FITACFElevation(prm,site=65,mean(phi0[10:30,*],dim=1,/nan))
      ;OPLOT,t*60,mean(fd.elv[10:30,*],dim=1),col=120 ;dataElev
      ;OPLOT,t*60,ele,color=240 ;FitElev
      ;p=MakeLegend(['phi0','elev','calc'],[0,120,240],!p.clip,scale=10,place=[.5,.5])
      h=HISTOGRAM(t[bf],BINSIZE=.25,MIN=0,MAX=24)
      t_data=[0,TOTAL(h,/CUMULATIVE)]
      for time=0,N_ELEMENTS(t_data)-2 do begin
        if h[time] eq 0 then continue
        pt=[t_data[time]:t_data[time+1]-1]
        now=valFil[*,pt]
        good=WHERE(now gt 0.01,cnt);a[rad.gates[0]:rad.gates[-1],*],cnt)
        if cnt eq 0 then continue
        if h[time] eq 1 $
          then ret[*,beam,fr,time]=now $
        else ret[*,beam,fr,time]=TOTAL(now,2)/FLOAT(TOTAL(now gt 0.01,2));mean(m[good])
      endfor
    endfor
  end
  RETURN,ret
  PRINT,"Done a file"
END

;+
;rad={name:'rkn',beams:[0,15],gate:[0,50]}
;GetMonthDat,'pwr0',rkn
;-
PRO GetMonthDat,toGet,rad
  @'/home/ullrich/syd/sydCommonV2.pro'
  year=2016
  month=3
  files=GetFiles(dirPref,rad.name,year,month)
  power=fltarr([51,16,2,96,n_elements(files)])
  if strcmp('power',toget,/fold) then tag='pwr0'
  page=pageDef(plot_dir+'MonthPower',perdiv=[1,3])
  ;screenplot  
  foreach f,files,fi do begin
    a=needsmake(f,tag=tag,rad)
    power[*,*,*,*,fi]=a
    p=makeplot(xr=[0,24],yr=[0,50],title=STRING(FORM='12 MHz beam 7, day %02i',fi))
    pixelplot,transpose(alog10(reform(a[*,7,1,*]))),x=[0:24:.25],clip=[0,6],/leg
  endforeach
  saveName=STRING(FORMAT='%sdata/%s/%s%04i%02i',this_dir,toGet,rad.name,year,month)
  save,power,file=saveName
END


;+
;forceRun bool  will cause it to read through data files rather than loading the save from a finished output
;tempACF  bool  will make it delete the local copies of ACF files (default 1)
;fileverb [bits]
;   bit 0 limited file info
;   bit 1 much file info
;   bit 2 for tar contents
;verb     [bits]
;   eq 1 year times
;   ge 2 month times
;   ge 3 day times
;   bit 4 profiler
;   +4 announces start as well as end
;
;GetEchos,years=[2008:2017],/forceRun,/verb,/noask,radars=['inv','cly','rkn'],toGet='vel'
;-
PRO GetEchos,years=years,months=months,forceRun=forceRun,$
  verb=verb,noask=noask,radars=radars,toGet=toGet,fileverb=fileverb,gates=gates
  compile_opt strictarr ;specifically do NOT use idl2(default)
  ;because i want integers to be 16 bit for counting echos
  ;;otherwise the echo save files take up too much space
  PROFILER,/RESET
  PROFILER,/SYSTEM & PROFILER
  st=SYSTIME(/seconds)
  @'/home/ullrich/syd/sydCommonV2.pro'

  if ~KEYWORD_SET(toGet) then toGet='echos'
  if STRCMP(toGet,'echos',/fold) then toGet='echos'
  if STRCMP(toGet,'vel',/fold) then toGet='vel'

  radars=RadParseV3(radars) ;50 gates, 16 beams
  if N_ELEMENTS(verb) eq 0 then verb=0
  if N_ELEMENTS(noask) eq 0 then noask=1
  freqs=[8e3,11e3,18e3] ;normally upper limit is 14

  ;We will need these sooner or laters
  if keyword_set(gates) then radars.gates=gates
  ngates=MAX(radars.gates[1]-radars.gates[0])+1
  nbeams=MAX(radars.beams[1]-radars.beams[0])+1
  ntimes=N_ELEMENTS(times)
  nfreqs=N_ELEMENTS(freqs)-1
  nrads=N_ELEMENTS(radars)

  dirPref=[this_dir+'data'+s+'fitidl'+s,this_dir+'data'+s+'fitcon'+s];,s+'data'+s+'fitcon'+s]
  for yr=0,N_ELEMENTS(years)-1 do begin
    year=years[yr]
    saveDir=STRING(FORMAT='%sdata/%s/%i/',this_dir,toGet,year)
    if ~FILE_TEST(saveDir) then FILE_MKDIR, saveDir
    saveNames=REPLICATE(saveDir,N_ELEMENTS(months),nrads)
    IF ((year mod 4 EQ 0) AND (year mod 100 NE 0)) OR (year mod 400 EQ 0) $
      THEN NumDays=[31,29,31,30,31,30,31,31,30,31,30,31] $
    ELSE NumDays=[31,28,31,30,31,30,31,31,30,31,30,31]
    for rad=0,nrads-1 do saveNames[*,rad]+=STRING(FORMAT='r%s',radars[rad].name)
    yearTime=SYSTIME(/seconds)
    foreach month,months,m do begin
      monthTime=SYSTIME(/seconds)
      saveNames[m,*]+=STRING(FORMAT='_m%02i',month)
      days=[1:numDays[month-1]]
      for rad=0,N_ELEMENTS(radars)-1 do $
        if ~FILE_TEST(saveNames[m,rad]) or KEYWORD_SET(forceRun) then begin
        files=REFORM(GetFilesV4(dirPref,radars[rad].name,year,month,days,noask=noask,verb=fileverb)) ;Get the files
        ndays=N_ELEMENTS(days)
        GetMeta,files,types

        if STRCMP(toGet,'echos',/FOLD) then begin
          echoCount=BYTARR(ngates,nbeams,nfreqs,ntimes,ndays) ;will count echos
          groundCount=BYTARR(ngates,nbeams,nfreqs,ntimes,ndays) ;will count ground-flagged echos
          scanCount=BYTARR(nbeams,nfreqs,ntimes,ndays) ;will count scanned days per month
        endif
        if STRCMP(toGet,'vel',/FOLD) then vels=FLTARR(ngates,nbeams,nfreqs,ntimes,ndays)
        if STRCMP(toGet,'phase',/FOLD) then phi=FLTARR(ngates,nbeams,nfreqs,ntimes,ndays)

        for day=0,NumDays[month-1]-1 do begin ;Loop over days
          if files[day] eq 'GetFile_Nothing' then continue
          if types[day] eq 'acf' or types[day] eq 'fit' then begin
            message,"run makeFit2IDL first"
            if STRCMP(toGet,'phase',/FOLD) then begin
              a=needsMake(files[day],type=types[day],radars[rad],freqs)
              phi[*,*,*,*,day]=a
            endif
            continue
          endif else if types[day] ne 'idl' then begin
            PRINT, 'Bad type: ',types[day]
            continue
          endif
          ;Getting this far means that we have idl type files for the day in question
          RESTORE,files[day],/NO_COMPILE
          if (verb and 4) ne 0 then PRINT,FORMAT='Read file for month: %-i, day: %-i, radar: %s',month,days[day],radars[rad].name
          for fr=0,nfreqs-1 do begin
            ;Now we have a fit structure
            a=FilterArrs(fit,scans,e,g,v,cnt=cnt,powBounds=[3,70],$
              gatBounds=[radars[rad].gates[0],radars[rad].gates[1]],$
              bmnBounds=radars[rad].beams,frqBounds=[freqs[fr],freqs[fr+1]])
            if a eq 0 then begin
              print,STRING(FORM='bad echos %s, freq %i',files[day],fr)
            endif
            if STRCMP(toGet,'echos',/FOLD) then begin
              if MAX(scans) gt 255 then begin
                scancount=uint(scancount)
                echoCount=uint(echoCount)
                groundCount=uint(groundCount)
                print,size(scancount)
                PRINT,radars[rad].name,STRING(FORMAT="day %i Has too many scans for byte",day)
              endif
              scanCount[*,fr,*,day]=scans
              echoCount[*,*,fr,*,day]=e
              groundCount[*,*,fr,*,day]=g
            endif else if STRCMP(toGet,'vel',/FOLD) then begin
              vels[*,*,fr,*,day]=v/e
            endif
            for i=0,50 do if TOTAL(e[i,*,*] gt scans) then MESSAGE,"bad"
          endfor ;frequency
        endfor ;days
        case toGet of
          'echos':save,echoCount,groundCount,scanCount,FILENAME=saveNames[m,rad],/compress
          'vels':save,vels,FILENAME=saveNames[m,rad]
          'phase':save,phi,FILENAME=saveNames[m,rad]
        endcase
        FILE_DELETE,files,/ALLOW_NONEXISTENT
        if (verb and 2) ne 0 then PRINT,FORMAT='Read file for month: %-i, radar: %s',month,radars[rad].name
        if (verb and 4) ne 0 then PRINT,"deleting files"
      endif ;rads
      if (verb and 2) ne 0 then PRINT,'month in',SYSTIME(/seconds)-monthTime
    endforeach ;months
    if (verb and 1) ne 0 then PRINT,STRING(FORM='year %-i in -%-i',year,SYSTIME(/seconds)-yearTime)
  endfor ;years
  PRINT,'DONE in ',STRING(format='%-i',SYSTIME(/seconds)-st),' seconds'
  if (verb and 16) ne 0 then PROFILER,/REPORT,filename='/home/ullrich/syd/getEchosProfile'
  goto, done
  needsMake:
  PRINT,'use makefit2idl (in readRadar)first';File existing
  PRINT,files
  done:
END