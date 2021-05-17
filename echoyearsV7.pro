;@'/home/ullrich/syd/sydPlotToolsV4.pro'
;@'/home/ullrich/syd/sydTools.pro'
;@'/home/ullrich/syd/getdaysV5.pro'
@'/home/ullrich/syd/zenithV2.pro'
@'/home/ullrich/syd/radParseV3.pro'
;+
;Plots echo eccurence vs time and date, then draws solar zenith angles
;CALL:   echoYears,years,radars=radars,res=res,noShift=noShift
;
;INPUTS:
;years   [int]
;radars  [str]
;gates
;beams
;res     str   resolution ('day' or 'month') {'month')}
;noShift int   if 1, uses UT, else uses local time + noShift
;freq    int   0 uses any, 1 uses 10MHz, 2 uses 12MHz
;
;in V4: Added maxima, use radParse, better year support
;in V5: Added radMods keyword, and sun rise/set times
;rise set is the same as zenith though
;also added radar info page, and ability to collFreq
;
;V6:
;echoyearsV6,[2013:2017],radars=['inv','dce','mcm','sps','rkn','cly'],radmods=['expbeams','expgates']
;
;todo make getdays v5 that doesn't mess with the dims of bep until the very end
;-
PRO echoYearsV7,years,radar=radar,beams=beams,gates=gates,res=res,noShift=noShift,freq=freq,gnd=gnd
  st=SYSTIME(/seconds)
  @'/home/ullrich/syd/sydCommonV2.pro'
  if ~keyword_set(freq) then freq=0
  fnames=['','10 MHz','12 MHz']
  ;@'RadDefV2.pro'
  ;radar2=rads[WHERE(STRCMP(rads.name,radar))]
  radars=radParseV3(radar)
  if ~isa(gates) then gates=[10,30]
  if ~isa(beams) then beams=[6,8]
  ;radars.time=radars.lon/15. ;gotten by radparsev2
  radars.gates=gates
  radars.beams=beams
  if ~KEYWORD_SET(res) then res='month'
  if ~ISA(noShift) then noShift=0
  mult=(res eq 'day')?30.4375:1
  cx=96./95.*times

  n_years=N_ELEMENTS(years)
  coll=['g','b','d']
  if freq eq 0 then coll=[coll,'f']
  BEP=GetDaysV5(years,radars,coll=coll,scan=scan,ground=ground);,res='month')
  BEP=isa(gnd)?ground/scan:bep/scan
  if (freq ne 0) then BEP=BEP[freq-1,*,*,*]
  BEP=REFORM(BEP)
  sz=size(BEP)
  n_elm=sz[2]

  lin2cont=FLOAT(n_elm/(n_elm-1.))
  cy=years[0]+([0:n_elm-1.]*lin2cont)/(12.*mult)
  doy=([0:n_elm]+.5)*30.4375/mult;midpoints of months
  doy=doy[0:-2]

  if isa(gnd) then tag='gnd' else tag='ion'
  file_ps=plot_dir+'EchoMonths/'+STRJOIN([stryears(years,/u),radars.name,fnames[freq],tag],'_');@'/home/ullrich/syd/getdaysV5.pro'.
  page=PageDef(file_ps.replace("__","_")+'V7',$
    divs=[1,10/n_years],fsize=16)
  USERSYM,[-1,0,1,0,-1],[0,1,0,-1,0],THICK=5

  foreach rad,radars,ri do begin
    isSouth=(rad.lat lt 0)
    if noShift then shiftTime=0 else shiftTime=rad.time+noShift
    locTime=SHIFT(times,-4*rad.time)
    PRINT,"UT ",times[48]," = LT",locTime[48]

    p=MakePlot(xr=[0,24],yd=cy,xtitle='',ytitle=' ',$
      title=STRING(FORMAT='%s, beams:%-i-%-i, gates:%-i-%-i',rad.name,rad.beams,rad.gates),/noAxis)
    toPlot=REFORM(SMOOTH(BEP[*,*,ri],[1,mult],/NAN,/EDGE_WRAP))
    LevCol,toPlot,levs,cols,nlevels=15,/over
    CONTOUR,SHIFT(toPlot,[shiftTime*4,0]),cx,cy,LEVELS=levs,C_COLORS=cols,/overplot,/closed,/CELL_FILL
    ;Make ticklike guidelines
    for tim=3,21,6 do OPLOT,[tim,tim],[cy[0],cy[-1]],thick=2,color=0,linestyle=2

    ;Solar Zenith
    doy_arr=REBIN(times,96,n_elm)/24
    for kk=0,95 do doy_arr[kk,*]+=FIX(doy)
    szen=zenith(doy_arr,rad.lat,rad.lon)/!dtor ;Getting zenith angle
    contour,SHIFT(szen,[shiftTime*4,0]),cx,cy,levels=[90,105],overplot=1, color=255, C_labels=[1,1],thick=6

    ;Get those max squares
    smoothSample=SMOOTH(toplot,[4,3*mult],/NAN)
    sz=FLOAT(N_ELEMENTS(toPlot[0,*]))
    maxOccs=FLTARR(sz)
    timeInds=INTARR(sz)
    for date=0,sz-1 do begin
      a=MAX(smoothSample[*,date],ind)
      maxOccs[date]=a
      timeInds[date]=ind mod 96
    endfor
    maxTimes=(times[timeInds]+24+shiftTime) mod 24 ;the +24 is for sign safety, mod 24 fixes it

    bestTimes=INTARR(n_years+1)
    yrs=FLTARR(n_years+1)
    step=sz/n_years

    yoff=KEYWORD_SET(isSouth)/2.
    for yr=yoff,n_years-1+yoff do begin
      yearOccs=maxOccs[((yr-.25)*step)>0:((yr+.25)*step-1)<(sz-1)]
      a=MAX(yearOccs,ind)
      plac=ind+yr*step
      plac2=WHERE(maxOccs eq a,cnt)
      if cnt gt 1 then plac2=plac2[0]
      bestTimes[yr]=plac2
    endfor
    ytp=bestTimes/12.+years[0]
    OPLOT,maxTimes[BestTimes],ytp,psym=8,color=0,symsize=1.4

    ;Sun rise and set
    ; for yr=years[0],years[-1] do begin
    ;   y2=yr+([0:365.25]*n_years/(n_years*365.25-1))
    ;   sunTimes,y2,lat,lon,rise,set
    ;   rise+=shiftTime
    ;   set+=shiftTime
    ;
    ;   a=where(~(rise ge 0),cnt)
    ;   if cnt gt 1 then OPLOT,rise[a]+24,y2[a],thick=6,linestyle=2,col=240
    ;   a=where(~(rise le 24),cnt)
    ;   if cnt gt 1 then OPLOT,rise[a]-24,y2[a],thick=6,linestyle=2,col=240
    ;   a=WHERE(~(rise lt 0 or rise gt 24),cnt)
    ;   if cnt gt 1 then OPLOT,rise[a],y2[a],thick=6,linestyle=2,col=240
    ;
    ;   a=WHERE(~(set ge 0),cnt)
    ;   if cnt gt 1 then OPLOT,set[a]+24,y2[a],thick=6,linestyle=2
    ;   a=WHERE(~(set le 24),cnt)
    ;   if cnt gt 1 then OPLOT,set[a]-24,y2[a],thick=6,linestyle=2
    ;   a=WHERE(~(set lt 0 and set gt 24),cnt)
    ;   if cnt gt 1 then OPLOT,set[a],y2[a],thick=6,linestyle=2
    ; endfor
    timeLabels=STRING(FORMAT='%-i',(([0,6,12,18,24]+noShift) mod 24))
    FixContour,p,0,252,6,1,6,4,ytitle='Year',$
      xtitle=(noShift?'Universal Time':'Local Time'),xtname=timeLabels
    ColBar,p,100*[levs],colIn=cols,num=8,place=[1.1,.99],form='%-i'
  endforeach
  SpitRadars,radars
  DEVICE,/CLOSE
  PRINT,'DONE in ',STRING(format='%f',SYSTIME(/seconds)-st),' seconds'
END