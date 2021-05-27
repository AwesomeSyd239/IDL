@'/home/ullrich/syd/sydTime.pro'
@'/home/ullrich/syd/getdaysV5.pro'
@'/home/ullrich/syd/radParseV3.pro'


;+
;keepE keeps gates 0-10 (E Region) {false}
;linear does the weighting in linear space, (this disregards th e fact that signal power is log)
;useing logs, which are sometimes negative
;-
FUNCTION weightGate,data,keepE=keepE,linear=linear,ng=ng
  sz=SIZE(data,/dim);normally 51 gates x 96 times
  if ~KEYWORD_SET(ng) then ng=sz[0]
  gates=REBIN([0:ng],sz) ;the gate numbers
  p2u=KEYWORD_SET(linear)?data:10.^(data/10.);make local copy, out of log space as needed
  if ~KEYWORD_SET(keepE) then p2u[0:10,*]=0;set occurrence to 0 in gates 0-10
  p2u[WHERE(data eq 0)]=0;prevents negative numbers in log from affecting data
  RETURN,TOTAL(gates*p2u,1,/nan)/TOTAL(p2u,1,/nan) ;calculate center of mass
END


PRO TraceEchoBand,plotdata,peak,near,far,drawNow=drawNow
  tpd=plotdata
  tpd[*,0:10]=0
  hmv=MAX(tpd,peak,dim=2)/2;half max value to use for bounds
  peak/=96;maxgate mod 96
  near=INTARR(96) & far=INTARR(96)
  for i=0,95 do begin
    pd=tpd[i,*]
    gom=peak[i]
    li=gom & hi=gom
    while (pd[li] gt hmv[i] and li gt 0) do li-=1
    while (pd[hi] gt hmv[i] and hi lt 50) do hi+=1
    far[i]=hi & near[i]=li
  endfor
  if KEYWORD_SET(drawNow) then begin
    times=[.125:23.875:.25]
    OPLOT,times,peak,col=255,thick=5
    OPLOT,times,near,col=255,thick=5,linestyle=1
    OPLOT,times,far,col=255,thick=5,linestyle=1
  endif
END

;+
;Echos must be time,gate
;-
Function KoustovBand,echos,nt,ng
  sz=SIZE(echos,/dim);normally 96 times x 51 gates
  nt=sz[0]
  ng=sz[1]
  p2u=echos
  p2u[*,0:10]=0
  p2u[0:FIX(11*nt/24),10:15]=0
  p2u[FIX(23*nt/24):*,10:15]=0
  if ng gt 61 then p2u[*,60:*]=0
  RETURN,p2u
END

Function Best20,p2u,nt,ng
  centermass=FLTARR(nt)
  for t=0,nt-1 do begin
    low2high=SORT(p2u[t,*])
    maxgates=low2high[-20:*]
    centermass[t]=TOTAL(p2u[t,maxGates]*MaxGates)/TOTAL(p2u[t,MaxGates])
  endfor
  RETURN,centermass
END

Function Max20,p2u,nt,ng
  hmv=MAX(p2u,peak,dim=2)
  peak=peak /nt;get gate indexes (would do mod ng for transposed data)
  centerMass=FLTARR(nt)
  for t=0,nt-1 do begin
    gateHere=[(peak[t]-10)>10:(peak[t]+10)<50]
    centerMass[t]=TOTAL(p2u[t,gateHere]*gateHere)/TOTAL(p2u[t,gateHere])
  endfor
  RETURN,centermass
END

Function SkipDist,yr,rad,tag=tag,scale=scale,mnths=mnths
  common SingleDate
  SetDate,yr=yr
  if ISA(rad,/STRING) then rad=RadParseV3(rad)
  ;rad.gates=[0,75]
  gates=[rad.gates[0]:rad.gates[-1]]
  beams=[rad.beams[0]:rad.beams[-1]]
  @'/home/ullrich/syd/sydCommonV2.pro'

  if ~KEYWORD_SET(scale) then scale=.7
  if ~KEYWORD_SET(mnths) then mnths=[1:12]

  file_ps=plot_dir+'EchoMonths'+s+'SkipDist'+rad.name+STRING(format='%i',year) ;plot file name
  page=PageDef(file_ps+(ISA(tag)?tag:''),divs=[1,2],space=[1,1],perdiv=[2,1])
  ;screenplot,divs=[3,4]
  foreach month,mnths do begin
    SetDate,yr=year,mo=month,dy=15
    RESTORE,STRING(FORMAT='%sdata/echos/%i/r%s_m%02i',this_dir,year,rad.name,month)

    firstgate=INTARR([N_ELEMENTS(gates),2,96,2])
    foreach b,beams do for d=0,N_ELEMENTS(echocount[0,0,0,0,*])-1 do for f=0,1 do for t=0,95 do begin
      min2look=10+5*(t lt 44 or t ge 92)
      fis=MIN(WHERE(echocount[min2look:*,b,f,t,d] ne 0,ci))+min2look
      fgs=MIN(WHERE(groundcount[min2look:*,b,f,t,d] ne 0,cg))+min2look
      ;print,STRING(FORM="gs:%i is:%i, b:%i, f:%i ,t:%i, d:%i",fgs,fis,b,f,t,d)
      firstgate[fis,f,t,0]+=(ci ne 0)
      firstgate[fgs,f,t,1]+=(cg ne 0)
    endfor
    ;todo: mark this line for future syd to find the time shift
    doShift=0
    ;ep=doshift?SHIFT(ec/sc,[0,0,4*rad.time]):(ec/sc)
    endString=' Approximate First Echo Distance in '+dateString(year=year,month=month,sep=' ')
    xtit=(doShift?"Local":"Universal")+" Time"
    for freq=0,1 do begin
      MHz=10+2*freq
      fs=STRING(FORM='%i MHz ',Mhz)
      experString=STRING(FORM="based on %-i samples, using beams:%-i-%-i",TOTAL(scancount[beams,freq,*,*]),rad.beams[0],rad.beams[-1])
      for isg=0,1 do begin;isGround
        p=MakePlot(xd=times,yd=gates,title="experiment "+(isg?"Ground":"Sky"),xtit=xtit,ytit="Gates")
        pixelplot,transpose(REFORM(firstgate[*,freq,*,isg])),levs=levs,/nonorm,xi=times,yi=gates
      endfor
      ;TraceEchoBand,plotdata,peak,near,far,/drawNow
      ColBar,p,levs,scale=.75,form='%-i',place=[1.2,1]

      XYOUTS,.5,freq*.45+.505,STRUPCASE(rad.name)+(freq?' HF':' LF')+endString,/normal,align=0.5,charsiz=1.5
      XYOUTS,.5,freq*.45+.48,experString,/normal,align=0.5,charsiz=1.3
    endfor
  endforeach
  closepage
END


PRO GetTheRayTrace
  common SingleDate
  DefineRTExtras,yr=year,mo=month,dy=15,ra='rkn',bm=5
  DefineRTParams,xg=[0:3000:30];,dn=dn,gn=gn
  for f=10,12,2 do begin
    fs=STRING(FORM='%i MHz ',f)+dateString(year=year,month=month,sep=' ')
    AllDayTrace,.5,ipowcntgat=ipowcntgat,gpowcntgat=gpowcntgat,freq=f*1.e6,pstep=0
    p=makeplot(title="model ionosphere"+fs,xr=[0,24],yr=[0,50])
    pixelplot,TRANSPOSE(REFORM(ipowcntgat[*,*,1])),clip=[0,.7]
    p=makeplot(title="model ground"+fs,xr=[0,24],yr=[0,50])
    pixelplot,TRANSPOSE(REFORM(gpowcntgat[*,*,1])),clip=[0,.7]
  endfor
END

PRO ProcessEchos,dat,line,col,scale
  sel=TRANSPOSE(REFORM(dat))
  ; line=weightGate(sel,/linear)
  line=Best20(KoustovBand(sel,nt,ng),nt,ng)
  pixelplot,sel,clip=[0,scale]
  OPLOT,INTERPOL([0:24],nt),line,thick=5,col=col
END

;rad={sydRadar,name:'hok',beams:[0,2],gates:[0,50],col:54,time:9.5,south:0b}
;rads=[rad,rad,rad]
;for i=0,2 do rads[i].beams=[5*i,5*i+5]
;
;+
;CALL:   gatePlot,2014,['inv'],[{beams:[13,15]}],freqs=['All Freqs']
;INPUTS:
;year    int   the year
;radars  [str] or [rad]
;freqs   [str]
;tag     str   tags the file
;New in V3: Only does one radar
;rkn=SetBeams(RadParseV3('rkn'),[5,7])
;gateplotV3,2018,setbeams(radparsev3('rkn'),[5,7]),tag='intMag2x2'
;-
PRO GatePlotV3,yr,rad,tag=tag,scale=scale,mnths=mnths
  common SingleDate
  SetDate,yr=yr
  if ISA(rad,/STRING) then rad=RadParseV3(rad)
  ;rad.gates=[0,75]
  gates=[rad.gates[0]:rad.gates[-1]]
  beams=[rad.beams[0]:rad.beams[-1]]
  @'/home/ullrich/syd/sydCommonV2.pro'

  if ~KEYWORD_SET(scale) then scale=.7
  if ~KEYWORD_SET(mnths) then mnths=[1:12]

  file_ps=plot_dir+'EchoGates'+s+rad.name+STRING(format='%i',year) ;plot file name
  page=PageDef(file_ps+(ISA(tag)?tag:''),divs=[1,2],space=[1,1],perdiv=[2,1])
  ;screenplot,divs=[3,4]
  foreach month,mnths do begin
    SetDate,yr=year,mo=month,dy=15
    RESTORE,STRING(FORMAT='%sdata/echos/%i/r%s_m%02i',this_dir,year,rad.name,month)

    sc=collapse(scancount[beams,*,*,*],['b','f','t','d'],['b','d'])
    ec=collapse(echocount[gates,beams,*,*,*],['g','b','f','t','d'],['b','d'])
    gc=collapse(groundcount[gates,beams,*,*,*],['g','b','f','t','d'],['b','d'])
    fsiz=SIZE(gc,/dim)
    sc=REBIN(REFORM(sc,[1,fsiz[1:*]]),fsiz);by forcing size, i can do division on next line
    ;todo: mark this line for future syd to find the time shift
    doShift=0
    gp=doshift?SHIFT(gc/sc,[0,0,4*rad.time]):(gc/sc)
    ep=doshift?SHIFT(ec/sc,[0,0,4*rad.time]):(ec/sc)

    DefineRTExtras,yr=year,mo=month,dy=15,ra=rad.name,bm=5
    DefineRTParams,xg=[0:gates[-1]*45:30];,dn=dn,gn=gn
    endString=' Normalized occurenece Rate in '+dateString(year=year,month=month,sep=' ')
    xtit=(doShift?"Local":"Universal")+" Time"
    for freq=0,1 do begin
      MHz=10+2*freq
      AllDayTrace,.5,ipowcntgat=ipowcntgat,gpowcntgat=gpowcntgat,freq=Mhz*1.e6,pstep=0,gatebin=1,maxgate=rad.gates[-1]+1
      fs=STRING(FORM='%i MHz ',Mhz)

      experString=STRING(FORM="based on %-i samples, using beams:%-i-%-i",TOTAL(sc[0,freq,*]),rad.beams[0],rad.beams[-1])

      p=MakePlot(xd=times,yd=gates,title="experiment Sky",xtit=xtit,ytit="Gates")
      ProcessEchos,ep[*,freq,*],avgGateS,244,scale;,gat=[10,50]
      ;TraceEchoBand,plotdata,peak,near,far,/drawNow
      ColBar,p,[0:scale*100:10],colin=cols,scale=.7,num=FIX(scale*10+1),form='%-i%%',place=[1.2,1]

      p=MakePlot(xd=times,yd=gates,title="experiment Ground",xtit=xtit,ytit="Gates")
      ProcessEchos,gp[*,freq,*],avgGateG,244,scale;,gat=[10,50]

      p=makeplot(xd=times,yd=gates,title="model sky")
      ProcessEchos,ipowcntgat[*,*,1],modGateS,255,scale;,gat=[10,50]
      OPLOT,times,avgGates,thick=5,col=244
      cleanup,avgGateS,interpol(modGates,96),cas,cms,/nosort
      XYOUTS,5,5,"cor: "+STRING(Form='(F+5.2)',correlate(cas,cms)),col=255,charthick=1.5

      p=makeplot(xd=times,yd=gates,title="model ground")
      ProcessEchos,gpowcntgat[*,*,1],modGateG,255,scale;,gat=[10,70]
      OPLOT,times,avgGateg,thick=5,col=244
      cleanup,avgGateG,interpol(modGateG,96),cag,cmg,/nosort
      XYOUTS,5,5,"cor: "+STRING(FORM='(F+5.2)',correlate(cag,cmg)),col=255,charthick=1.5

      XYOUTS,.5,.955,STRUPCASE(rad.name)+(freq?' HF':' LF')+endString,/normal,align=0.5,charsiz=1.5
      XYOUTS,.5,.93,experString,/normal,align=0.5,charsiz=1.3
    endfor
  endforeach
  closepage
END


;@'sydReadRadarV4.pro'
;@/home/ullrich/syd/GetFilesV4
;freqs  [num] low and high bounds for frequency
;tag
;pwr0
;p_l
;v    velocity
;bg      int  how many beams should be lumped together {3}
PRO SinglePlot,freqs,year,month,day,tag,smth=smth,pg=pg,bg=bg,gnd=gnd
  @'/home/ullrich/syd/sydCommonV2.pro'
  setdate,yr=year,mo=month,dy=day
  if ~ISA(pg) then pg=1
  if ~ISA(tag) then tag='pwr0'
  if MAX(freqs) lt 1000 then freqs*=1000
  fstr=STRING(FORM='%iMHz',(freqs[0]+freqs[1])/2000)
  dstr=datestring(year=year,month=month,day=day)
  nstr=STRING(FORM='%04i%02i%02i',year,month,day)
  gnd=KEYWORD_SET(gnd)
  typ=gnd?'gnd':'ion'
  timetyp='UT'
  ;files=GetFilesV(40,'rkn',year,month,day)
  ;pts=sydReadFit(file=file,type=type,scls=scls,dats=dats,/verb)
  MESSAGE, 'needs assmble from getfiles';pts=assemblefitcon(fdat=dats,fscl=scls,radn='rkn')

  tags=TAG_NAMES(dats[0])
  tagInd=WHERE(STRMATCH(tags,tag,/fold),cnt)
  if cnt eq 0 then begin
    PRINT,tags
  endif
  data=dats.(tagInd)
  bad=WHERE(dats.qflg eq 0 or dats.gflg ne gnd,cnt)
  if cnt ne 0 then data[bad]=!Values.f_nan

  if ~ISA(bg) then bg=3
  pgstr=plot_dir+STRJOIN([tag,typ,nstr,fstr],'_')
  if pg eq 1 then page=pagedef(pgstr,divs=[1,3])
  if pg eq 2 then screenplot,divs=[1,3]
  for bb=0,15/bg-1 do begin
    beams=bg*[bb,bb+1];-[0,1]

    bf=WHERE(scls.time.dy eq day and $
      scls.bmnum ge beams[0] and scls.bmnum le beams[1] and $
      scls.tfreq ge freqs[0] and scls.tfreq le freqs[1],cnt)

    ttstr=STRING(FORM='Beams:%02i-%02i, n=%i',beams,cnt)
    p=makeplot(xr=[0,24],yr=[0,50],title=ttstr,ytitle='Gate',xtitle=timeTyp)
    if cnt lt 100 then begin
      XYOUTS,12,25,'insufficient data',align=.5,charsize=1.5
    endif else begin
      prm=scls[bf]
      val=data[0:50,bf]

      ;commandstofindpower,val,dats[bf].gflg
      ;continue
      time=prm.time.hr+prm.time.mt/60.+prm.time.sc/3660.
      time=(time+24.-STRCMP(timeTyp,"LT")*6.5) mod 24 ;todo allow time other than rkn
      to=SORT(time)
      t1=time[to]
      TimeShift=(t1[0:-2]-t1[1:*])
      PRINT,'max shift (minutes) is',MAX(TimeShift)*60
      step=cnt/288 ;5 minute intervals with cnt/288
      ;t2=smooth(t1,2*step,/nan,/edge_truncate)
      if tag.startswith('p') then v1=10.^(val[*,to]/10.) $
      else v1=val[*,to];the 'to' part sorts the data by time
      ;v2=smooth(v1,[0,2*step],/nan,/edge_truncate)
      vtp=FLTARR([51,288])
      th=HISTOGRAM(t1,min=0,max=24,binsize=5./60.,reverse=ri)
      for m=0,287 do if th[m] gt 0 then vtp[*,m]=mean(v1[*,ri[ri[m]:ri[m+1]-1]],dim=2,/na)
      ;pixelplot,transpose(v2[*,0:*:step]),leg=[1.3,1],x=t2[0:*:step],y=[0:49],bgc=255,clip=[0,60]
      if KEYWORD_SET(smth) then vtp=SMOOTH(vtp,[2,6],/nan)
      pixelplot,TRANSPOSE(10*ALOG10(vtp)),leg=[1.3,1],bgc=0,clip=[0,60]
      wg=weightGate(vtp,/linear)
      ;wg2=smooth(wg,2*step,/nan,/edge_wrap)
      ;oplot,t2[0:*:step],wg2[0:*:step],thick=3,col=254
      OPLOT,[0:24:5./60.],wg,thick=3,col=254
    endelse
  endfor
  XYOUTS,.5,.25,'Power and Average Gates !C for RKN '+fstr+' '+dstr,/normal,align=.5,charsize=1.8
  PRINT,'done'
  if ISA(page) then DEVICE,/close
END

Pro weightPower,rad,year,month
  @'sydCommonV2.pro'
  spec=STRING(FORM='%s%04i%02i',rad,year,month)
  RESTORE,STRING(FORMAT='%sdata/power/%s',this_dir,spec)
  ;form is gate,beam,freq,time,day with size 51,16,2,96,31
  page=PageDef(plot_dir+'Power_'+spec,divs=[1,3])
  powTime=SHIFT(power,[0,0,0,-26,0])
  for f=0,1 do begin
    fstr=(f?'12':'10')+'MHz'
    for bb=0,2 do begin
      beams=[5*bb:5*(bb+1)]
      bmStr=STRING(FORM='Beams %i-%i ',beams[0],beams[-1])
      ;cnt=total(power[*,beams,f,*,*] gt .01)
      colPow=REFORM(Collapse(powTime[*,beams,f,*,*],['gate','beam','freq','time','day'],['beam','day'],/avg))
      p=makeplot(xd=times,yr=[0,50],title='Average Power (dB) in '+bmStr+fstr,xtit='Local Time',ytit='Gate')
      pixelplot,TRANSPOSE(ALOG10(REFORM(colPow))*10),leg=[1.4,1],clip=[0,50]
      OPLOT,times,weightGate(colPow),col=255,thick=3;'Average gate, weighted by power'
    endfor
  endfor
  DEVICE,/close
END

PRO PowerDistanceFalloff,val,g1
  ;run me about half way through singleplot
  screenplot,divs=[1,3]
  g1=g1[0:50,*]
  sky=val*(1-g1)
  gnd=val*g1
  si=FLTARR([2,2])
  for ri=0,1 do begin
    if ri eq 0 then tdat=sky else tdat=gnd
    tdat[WHERE(tdat lt 1)]=!values.f_nan
    spow=SMOOTH(tdat,[2,80],/nan,/edge_mirror)
    spow=SMOOTH(spow,[2,80],/nan,/edge_mirror)
    spow=spow[*,0:*:100]

    ;delp/delx should give power falloff
    for i=0,50 do mns[i]=ALOG10(mean(10.^(tdat[i,*]/10.),/nan))
    delP=mns[0:-2]-mns[1:*]
    delX=ALOG10([4.:53.]/[5.:54.])

    pk=MAX(spow,maxGate,dim=1)
    gsam=WHERE(pk gt 10,nsam);arbitrary
    ;TODO: #@
    maxGate=maxGate[gsam] mod 51
    res=FLTARR([2,nsam])
    gtarr=FLOAT(REBIN([4:54],[51,nsam]))
    spow=(spow[*,gsam]);-max(pk))/alog10(gtarr);should cause linear power falloff
    ;p=makeplot(xr=[0,50],yr=[0,nsam])
    con=FLTARR([51,nsam])*!VALUES.F_NAN
    for i=2,6,2 do begin
      carr=(spow[0:-1-i,*]-spow[i:*,*])/ALOG10(gtarr[0:-1-i,*]/gtarr[i:*,*])
      pixelplot,carr,/leg,clip=[-80,0],/make
    endfor
    PRINT,'cars';p=makeplot(xr=[0,50],yr=[0,nsam])
    for i=0,nsam-1 do begin
      gts=maxgate[i]+[0:10]
      y=spow[gts,i]
      gtu=gts[WHERE(FINITE(y))]
      xFall=ALOG10(FLOAT(gtu[1:*]+4)/(gtu[0]+4))
      pFall=y[1:*]-y[0]
      con[gtu[1:*],i]=pFall/xFall
      ;oplot,[4:54],-spow[*,i],col=fix(254*i/nsam)
      ;res[*,i]=linfit(gtu+4,spow[gtu,i]);the +4 accounts for gate 0 dist
      ;oplot,gtu+4,-(res[0,i]+gtu*res[1,i]),col=fix(254*i/nsam),thick=3
    endfor
    ;si[*,ri]=mean(res,dim=2,/nan)
    pixelplot,con,/leg,clip=[-80,0],/make
    pixelplot,spow,/leg,/make
    PRINT,ri
  endfor
  PRINT,si
END