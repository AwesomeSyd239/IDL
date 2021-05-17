;@'/home/ullrich/syd/sydTools.pro'
;+
;Gathers EchoPercents into BEP
;CALL:   GetDays, options:see below
;
;INPUTS:
;years   [int]
;months  [int]
;radars  [rad] the radars you want (see RadParse)
;doShift bool  shift the outputs into local time {false}
;coll    flag  Collapse along these dimensions [gate,beam,freq,time,days]
;quiet   bool  be quiet (False)
;shortTe bool  condense the list of missing files (False)
;
;HOLDERS:
;ground  [int] ground echos
;scans   [int] a count of scans, dimensions are freq,time,day,rad
;
;RETURNS:
;BEP     [flt] with dimensions freq,time,day/month,rad
;new in V2:
;updated to sydCommonV2
;new in V3:
;returning counts rather than percents
;made frequency collapsing possible
;
;new in V4:
;cleaned up a bunch of stuff, but aloso broke the 'keepgate' option
;
;new in V5:
;removed frac keyword
;removed type keyowrd (I have no idea if it even worked in V4)
;removed res keyowrd from help info(Iit wasn't even in the function call in V4)
;removed keepgate from help (definitely broken in V4). It works as a 'coll' paramater now.
;removed mutli-radar ability (It might have screwed up BEP in old versions if radars had different params, not sure)
;-
function GetDaysV5,years,radar,$
  months=months,$
  doShift=doShift,$
  ground=ground,$
  scan=scan,$
  coll=coll,$
  quiet=quiet,$
  shortTell=shortTell,$
  nan=nan

  @'/home/ullrich/syd/sydCommonV2.pro'
  form=['g','b','f','t','d']
  coll=STRMID(coll,0,1)
  if TOTAL(STRCMP(coll,'d',1,/fold)) then begin
    res='month'
    NR=12
  endif else begin
    res='day'
    NR=1366
  endelse
  IF TOTAL(STRCMP(coll,'f',1,/fold)) then nfr=1 else nfr=2 ;number of frequencies
  if TOTAL(STRCMP(coll,'t',1,/fold)) then nt=1 else nt=96
  gate=[radar.gates[0]:radar.gates[-1]]
  beam=[radar.beams[0]:radar.beams[-1]]
  IF TOTAL(STRCMP(coll,'g',1,/fold)) then ng=1 else ng=(gate[-1]-gate[0]+1)
  IF TOTAL(STRCMP(coll,'b',1,/fold)) then nb=1 else nb=beam[-1]-beam[0]+1

  bep=REPLICATE(!VALUES.F_NAN,[ng,nb,nfr,nt,NR*N_ELEMENTS(years)])
  scan=REPLICATE(!VALUES.F_NAN,[nb,nfr,nt,NR*N_ELEMENTS(years)])
  ground=bep

  ind=0 & told=1
  missedFiles=[]
  for year=years[0],years[-1] do begin
    IF ((year mod 4 EQ 0) AND (year mod 100 NE 0)) OR (year mod 400 EQ 0) $
      THEN numDays=[31,29,31,30,31,30,31,31,30,31,30,31] $
    ELSE numDays=[31,28,31,30,31,30,31,31,30,31,30,31]
    for month=months[0],months[-1] do begin
      if nr eq 366 then i2=ind+numDays[month-1] else i2=ind+1
      file=STRING(format='%sdata/echos/%i/r%s_m%02i',this_dir,year,radar.name,month)
      if FILE_TEST(file) then RESTORE,file else begin
        missedFiles=[missedFiles,STRING(format='year:%04i, month:%02i',year,month)] ;append or create
        ind=i2 ;increment day/month count
        continue ;to next loop, before resetting tell of printing
      endelse
      told=0


      if KEYWORD_SET(doShift) then begin
        echoCount=SHIFT(echocount,[0,0,0,4*radar.time,0])
        scanCount=SHIFT(scancount,[0,0,4*radar.time,0])
        groundCount=SHIFT(groundcount,[0,0,0,4*radar.time,0])
      endif
      ;Collapse the desired chunks of arrays
      if (0 eq total(echocount ne 0)) then print,STRING(FORM='bad echo file? %s,%s %-i',radar.name,monthNames[month-1],year)
      E=Collapse(echoCount[gate,beam,*,*,*],['g','b','f','t','d'],coll)
      SC=Collapse(scancount[beam,*,*,*],['b','f','t','d'],coll)
      Gr=collapse(groundCount[gate,beam,*,*,*],['g','b','f','t','d'],coll)

      ground[*,*,*,*,ind:i2-1]=REFORM(Gr,[ng,nb,nfr,nt,i2-ind])
      bep[*,*,*,*,ind:i2-1]=REFORM(E,[ng,nb,nfr,nt,i2-ind])
      scan[*,*,*,ind:i2-1]=REFORM(SC,[nb,nfr,nt,i2-ind])
      ;print,month,ind,i2
      ind=i2
    endfor ;months
    if ~told and (n_elements(missedFiles) ne 0) then begin ;now tell the user about a missed sequence of files
      if N_ELEMENTS(missedFiles) eq 1 $
        then PRINT,radar.name+' missing file:'+missedFiles[0] $
      else begin
        if keyword_set(shortTell) then PRINT,radar.name+' missing files: ',missedFiles[0],'...',missedFiles[-1] $
        else PRINT,radar.name+' missing files: ',missedFiles
      endelse
      missedFiles=[]
      told=1
    endif
  endfor;years

  scan=REFORM(scan[*,*,*,0:i2-1,*]);,[nfr,96,i2,N_ELEMENTS(radars)])
  ground=REFORM(ground[*,*,*,*,0:i2-1,*])
  if total(coll eq 'g') then begin
    bep/=ng;div by num gates
    ground/=ng
  endif else begin
    fsiz=size(ground,/dim)
    s3=reform(scan,[1,fsiz[1:*]])
    scan=rebin(s3,fsiz);cant rebin in the order i want
  endelse
  if ~KEYWORD_SET(quiet) then PRINT,"GetDays got",i2," total ",res,"s of data"
  ;if ng gt 1 $
  ;  then RETURN,REFORM(BEP[*,*,*,0:i2-1,*],[51,nfr,96,i2,N_ELEMENTS(radars)]) else $
  RETURN,REFORM(BEP[*,*,*,*,0:i2-1,*])
  ;REFORM(BEP[*,*,0:i2-1,*],[nfr,96,i2,N_ELEMENTS(radars)])
END
