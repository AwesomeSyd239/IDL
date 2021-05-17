PRO PixelPowerRT,etp,clips,xr=xr,yr=yr,ground=ground,title=title,ystep=ystep
  if ~ISA(xr) then xr=[0,MAX(etp[0,*])>40]
  if ~ISA(yr) then yr=[50*(1-KEYWORD_SET(ground)),MAX(etp[1,*])>300]
  if ~ISA(ystep) then ystep=10
  if ystep le 1 then ystep=fix((yr[1]-yr[0])*ystep)
  p=makeplot(xr=fix(xr),yr=yr,xtitle='Range Gate',ytitle='Altitude (km)',$
    title=optstring('Echo Power vs Location',title),ytint=5*ystep)
  retSig=FLTARR([FIX(xr[-1]-xr[0]),FIX(yr[-1]-yr[0])/ystep])
  yvals=[yr[0]:yr[-1]-ystep:ystep];all the different heights
  expH=fltarr(n_elements(retsig[*,0]));best height at each range gate
  for gat=xr[0],xr[-1]-1 do begin
    here=WHERE((etp[0,*] gt gat) and (etp[0,*] le gat+1),gc)
    if gc eq 0 then continue
    foreach ht,yvals,hi do begin
      hh=WHERE((etp[1,here]>0 ge ht) and (etp[1,here] lt ht+ystep),cnt)
      if cnt eq 0 then continue
      if ht ge 10 then $
        retSig[gat,hi]=ALOG10(TOTAL(etp[3,here[hh]])) else $
        retSig[gat,hi]=ALOG10(cnt/(gat+4.)^3.)
    endforeach
    expH[gat]=total((yvals+ystep/2.)*10.^retsig[gat,*])/total(10.^retsig[gat,*]) ;mean heights weighted by power
  endfor
  sr=MIN(retsig,max=mr)
  if ~ISA(clips) then clips=FIX([mr-5.5,mr+.99])
  nzg=WHERE(retsig[*,0] ne 0,cnt)
  if cnt ne 0 then retsig[nzg,0]+=clips[-1] ;fix ground
  retsig[WHERE(retsig eq 0,/null)]=sr-1
  ;pixelplot,retSig-clips[-1],cliplev=clips-clips[-1];,leg=[1.2,1]
  pixelplot,retSig,cliplev=clips;,leg=[1.2,1]
  Redoaxis,p,255,255,5,5*ystep,ys=5
  plots,[xr[0]:xr[-1]-1]+.5,expH,psym=7,symsize=.5,thick=2,noclip=0
  colbar,p,place=[1.1,1],[10*clips[0]:10*clips[-1]],num=10,form='%i',name='Power (dB)'
END

Function PowerInGates,etp,clips=clips,bin=bin
  if ~isa(bin) then bin=.5
  gateHist=HISTOGRAM(etp[0,*],binsize=bin,min=0,rev=rev)
  powgat=FLTARR([3,N_ELEMENTS(gateHist)])+1
  npt=0
  for i=0,N_ELEMENTS(gateHist)-1 do if gateHist[i] ne 0 then begin ;iterate through each gate
    pp=rev[rev[i]:rev[i+1]-1];(gatehist[i] eq 1)]
    npt+=N_ELEMENTS(pp)
    greg=WHERE(etp[1,pp] lt 5.,cnt)
    if cnt gt 0 then powgat[0,i]=TOTAL(10.^(clips[1]/10.-1)/(etp[0,pp[greg]]+4)^3.)
    ereg=WHERE(etp[1,pp] lt 120 and etp[1,pp] ge 5,cnt)
    if cnt gt 0 then powgat[1,i]=TOTAL(etp[3,pp[ereg]])
    freg=WHERE(etp[1,pp] ge 120,cnt)
    if cnt gt 0 then powgat[2,i]=TOTAL(etp[3,pp[freg]])
  endif
  return,powgat
END

Pro LinePowerRT,etp,clips,sep=sep,xr=xr,title=title,plt=plt,bin=bin,cols=cols
  if ~ISA(plt) then begin
    if ~ISA(xr) then xr=[0,MAX(etp[0,*])>40]

    plt=makeplot(xr=xr,yr=clips,title=optstring('Echo Power vs Gate',title),$
      xtitle='Range Gate',ytitle='Power (dB)',xtint=5,ytsub=2)
    for gat=0,70,5 do OPLOT,[gat,gat],clips,lin=2
    for pwr=clips[0],clips[-1],10 do OPLOT,[0,100],[pwr,pwr],lin=2
  endif
  if ~ISA(bin) then bin=(xr[1]-xr[0])/100.
  powgat=PowerInGates(etp,clips=clips,bin=bin)

  x=[0:N_ELEMENTS(powGat[0,*])*bin:bin]+.2
  OPLOT,x,SMOOTH(10*ALOG10(TOTAL(powgat,1)),2,/nan),thick=3
  if ISA(sep) then begin
    if ~isa(cols) then cols=[254,150,80,0]
    for i=0,2 do OPLOT,x,SMOOTH(10*ALOG10(powgat[i,*]),2,/nan),col=cols[i],thick=3
    pos=makeLegend(['E','F','G','Tot'],cols[[1,2,0,3]],plt)
  endif
END

;+
;echos are 2x4xn
;first index is 10v12 MHz
;0:gates
;1:heights
;2:elevation
;3:power
;plotintegratert,echos,li
;-
PRO PlotIntegrateRT,echos,li,clips,yr=yr,ext=ext
  common RTExtras
  if ~isa(yr) then yr=[0,300]
  if ~isa(ext) then ext=''
  me=MAX(echos[*,2,*],min=se)
  colscl=255./me
  page=PageDef('plots/RT_Integrate'+ext,divs=[1,2],fsiz=16)
  good=WHERE(echos[*,1,*] lt yr[1] and echos[*,1,*] gt yr[0])
  e10=REFORM(echos[0,*,0:li[0]])
  e12=REFORM(echos[1,*,0:li[1]])
  xr=[[0,MAX(e10[0,*])],[0,MAX(e12[0,*])]]
  echoAltplot,0,e10[0,*],e10[1,*],6,col=BYTE(e10[2,*]*colScl),$
    plt=plt,sz=.2,yr=yr,xr=xr[*,1],tit='+ 10 MHz'
  XYOUTS,5,85,STRING(FORM='n=%i',li[0])
  XYOUTS,.5,.95,strdate,/normal,charsize=2,align=.5
  colbar,plt,[se:me:3],form='%i',place=[1.1,1],name='Elevation'
  echoAltPlot,0,e12[0,*],e12[1,*],4,col=BYTE(e12[2,*]*colScl),$
    plt=plt,sz=.2,yr=yr,xr=xr[*,1],tit='+ 12 MHz'
  XYOUTS,5,85,STRING(FORM='n=%i',li[1])
  foreach f,['+ 10 MHz','+ 12 MHz'],fi do begin
    these=REFORM(echos[fi,*,0:li[fi]])
    PixelPowerRT,these,clips,/ground,xr=xr[*,1],title=f,yr=yr,ystep=1/20.
    LinePowerRT,these,(clips+1)*10,/sep,xr=xr[*,fi],title=f
    XYOUTS,.5,.95,strdate,/normal,charsize=2,align=.5
  endforeach
  DEVICE,/CLOSE
END



PRO FourIon,ym
  x=[1,1.00156897251489,1.00310211405608,1.00420749707745,1.00334943215897,0.998387145989126,0.989410522054304,0.979785686595838,0.973511044866482,0.972451797705238,0.976018103427735,0.982414156144606,0.989803361072377,0.996768236980746,1.00226112368249,1.00540791017767,1.00539894914944,1.00153787114576,0.993400921881773,0.981007900840198,0.964916829359303,0.946200691991412,0.926312849017615,0.906878744715578,0.889463253111213,0.875361578364113,0.865452259958984,0.86013503507577,0.859355320007276,0.862695948973107,0.869503407976874,0.879014999464285,0.890463132077866,0.903146482792458,0.916468890411389,0.929952894546941,0.943236410375731,0.956059976956207,0.968250006268962,0.97970148423185,.98,0.990362035580979]
  y=FFT(x,/center)
  screenplot
  PRINT,y[20:30]
  p=makeplot(yr=[-ym,ym],xr=[0,39])
  OPLOT,real_part(y[1:*]),col=60
  OPLOT,IMAGINARY(y[1:*]),col=250
end