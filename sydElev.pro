@'/home/ullrich/syd/sydTools.pro'
@'/home/ullrich/syd/sydPlotToolsV4.pro'
@'/home/ullrich/syd/smartelevation.pro'
@'/home/ullrich/syd/getfilesV3.pro'
@'/home/ullrich/syd/sydReadRadarV4.pro'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:
;       RadarLoad
;
; PURPOSE:
;       Loads the radar identification table.
;
; CALLING SEQUENCE:
;       radar = RadarLoad(unit)
;
;       This function reads the radar identification table from
;       the open file with logical unit number, unit
;
;       The returned value is the radar identification table or
;       zero if an error occurred.
;-----------------------------------------------------------------
function SydRadarLoad,unit
  ON_IOERROR, iofail
  RadarMakeRadar, rad
  radar=REPLICATE(rad,1)
  c=0
  id=0L & status=0L & stnum=0L
  txt='' & line=''
  ednum=0L
  yr=0 & mo=0 & dy=0

  while ~EOF(unit) do begin
    ; Different than main.1.21 version of radar.pro
    READF,unit,line
    s=STRPOS(line,'#')
    if (s ne -1) then continue
    if (STRLEN(line) eq 0) then continue
    READS,line,id,status,stnum,ednum,txt
    rad.id=id
    rad.status=status
    rad.st_time=TimeYMDHMSToEpoch(stnum/10000,(stnum/100) mod 100,stnum mod 100,0,0,0)
    rad.ed_time=TimeYMDHMSToEpoch(ednum/10000,(ednum/100) mod 100,ednum mod 100,0,0,0)

    ; decode the text strings
    s=STRPOS(txt,'"')
    e=STRPOS(txt,'"',s+1)
    rad.name=STRMID(txt,s+1,e-s-1)
    s=STRPOS(txt,'"',e+1)
    e=STRPOS(txt,'"',s+1)
    rad.operator=STRMID(txt,s+1,e-s-1)
    s=STRPOS(txt,'"',e+1)
    e=STRPOS(txt,'"',s+1)
    rad.hdwfname=STRMID(txt,s+1,e-s-1)
    n=0
    while (s ne -1) do begin
      s=STRPOS(txt,'"',e+1)
      if (s eq -1) then break
      e=STRPOS(txt,'"',s+1)
      rad.code[n]=STRMID(txt,s+1,e-s-1)
      n+=1
    endwhile
    if (c ne 0) then radar=[radar,rad] $
    else radar[0]=rad
    c+=1
  endwhile
  iofail:
  RETURN, radar
end


;+
;Code for reading hardware information for the given radar
;radar_site_read,prm=prm,site=site
;-
pro radar_site_read, prm=prm,stid, site=site,rad=rad
  if ~ISA(prm) then begin
    prm=REPLICATE({$
      interfer:[0,5,20],$
      phidiff:1,$
      bmnum:7,$
      maxbeam:15,$
      bmsep:3,$
      tfreq:10000,$
      tdiff:1.0e-4,$
      stid:65,$
      time:{yr:2013,mo:1,dy:1,hr:1,mt:1,sc:1}},$
      5)
    MESSAGE,'test'
  endif
  fname='/home/rst3/rst/tables/superdarn/radar.dat'
  OPENR,unit,fname,/get_lun
  radar = SydRadarLoad(unit)
  CLOSE, unit
  FREE_LUN,unit
  s=RadarLoadHardware(radar,path='/home/rst3/rst/tables/superdarn/hdw/')
  yr=prm[0].time.yr
  mo=prm[0].time.mo
  dy=prm[0].time.dy
  hr=prm[0].time.hr
  mt=prm[0].time.mt
  sc=prm[0].time.sc
  rad=RadarGetRadar(radar,stid)
  site=RadarYMDHMSGetSite(rad,yr,mo,dy,hr,mt,sc)
END


PRO PlotPhi0,phi0,prm,title=title
  ph=phi0
  ph[WHERE(phi0 eq 0 or phi0 gt 3.5 or phi0 lt -3.5)]=!VALUES.F_NAN
  sp=decROUND(MIN(ph/!PI,max=mp),1)*!PI ;-.5,.5
  mp=decRound(mp,1)*!PI
  phix=[sp:mp:(mp-sp)/49.]
  hf=prm.tfreq gt 11000
  bfh=FLTARR([16,2,51])
  for b=0,15 do for f=0,1 do begin
    these=WHERE(prm.bmnum eq b and (hf eq f),cf)
    if cf eq 0 then continue
    filt=ph[*,these]
    mode=WHERE(filt-SHIFT(filt,1) eq 0,cnt)
    bfh[b,f,*]=HISTOGRAM(filt,min=sp,max=mp,binsize=(mp-sp)/50.5)
  endfor
  name=SCOPE_VARNAME(phi0,level=-1)
  toplot=SMOOTH(bfh[*,*,0:-2],[0,0,2],/edge_wrap)
  ym=MAX(toplot)
  p=makeplot(xr=[sp,mp],yr=[0,ym],xtnum=9,$
    xtit=name,ytit='Number',title=title)
  XYOUTS,sp+[.25,.7]*(mp-sp),[.75,.75]*ym,['10MHz','12MHz'],col=[94,222]
  for f=0,1 do for b=0,15 do begin
    col=127*f+8*b
    OPLOT,phix,toPlot[b,f,*],col=col
    y=MAX(toPlot[b,f,*],x)
    if b mod 2 then XYOUTS,phix[x-1],y,STRING(FORMAT='b:%i',b),col=col
    ;PRINT,b,f,phix[x]
  endfor
END

PRO plotTdifErr,p,tdifs,err
  yr=[p.yr[1]-20,p.yr[1]]
  POLYFILL,[-10,50,50,-10],[yr[0],yr[0],yr[1],yr[1]],col=255
  XYOUTS,.5,.95,'Tdiff',/normal
  ntd=normalize(tdifs,s=p.xr[0],m=p.xr[1])
  ner=normalize(err,s=p.yr[1]-20,m=p.yr[1])
  OPLOT,ntd,ner[*,0];10mhz
  OPLOT,ntd,ner[*,1],col=222;12 mhz
  axis,0,p.yr[1],/xaxis,xrange=[tdifs[0],tdifs[-1]];,/font;,charsize=.5
  XYOUTS,p.xr[0],[yr[0]:yr[1]:4],STRING(FORMAT='%3.1f',[0:1:.2]),charsize=.5,/align,/font
  XYOUTS,p.xr[0]*.9,60,/align,'normalized !cbadness',/font,orient=90
END


PRO PlotEleHist,ele,log=log
  sz=SIZE(ele,/dim)
  eleHist=FLTARR([sz[0],46])
  for i=0,sz[0]-1 do eleHist[i,*]=HISTOGRAM(ele[i,*],min=0,max=45)
  p=makeplot(xr=[0,sz[0]],yr=[0,46])
  if ISA(log) then $
    pixelplot,ALOG(eleHist+1),x=[0:sz[0]],y=[0:46],levs=levs else $
    pixelplot,eleHist,x=[0:sz[0]],y=[0:46],levs=levs
  ColBar,[0,0,1,.9],Levs,colin=[0:255:255/13],form='%i'
END

function phiW2elev,phiW,k,phi_sign,frq,c_phi,tdif,bw=bw
  dchi_cable= -tdif*frq
  if ~ISA(bw) then bw=0
  ;Interferometer in front of antenna->lower elevation=earlier arrival/greater phase difference
  ;Otherwise, lower elevation angles=later arrival and smaller phase difference
  chi_max= phi_sign*k*c_phi+dchi_cable ;phi0 at elev=0 @!@!
  wrap=(chi_max - phiW+bw)+(phi_sign lt 0) ;how many wraps?
  psi= (phiW+FLOOR(wrap)-dchi_cable)/k;unwrapped, subtract cable once, then get to distance
  emx=ASIN(SQRT(c_phi^2-((k-1.-dchi_cable)/k)^2))*180./!PI
  theta= c_phi^2 - psi^2
  ;bad=WHERE(theta lt 0.0 or theta gt 1.0,cnt)
  ;if cnt gt 0 then theta[bad]=-elev_corr
  elev=180.0d/!DPI*(ASIN(SQRT(theta)))
  de=psi^2/SQRT(theta)/SQRT(1-theta)
  RETURN,elev
  ;plotTheseElevs,elev,wrap,k,dchi_cable,c_phi,phiW,x0=240
end

pro ElevsMinHeight,height
  height=70
  dist=([0:ng]+4)*45.
  emin=ASIN(height/dist)*180./!PI
END

PRO plotTheseElevs,elev,wrap,k,dchi_cable,c_phi,phiW,x0=x0
  if ~ISA(x0) then x0=0
  e=TRANSPOSE(elev[0:20,x0:x0+60])
  w=TRANSPOSE(wrap[0:20,x0:x0+60])
  dcc=TRANSPOSE(dchi_cable[0:20,x0:x0+60])
  tk=TRANSPOSE(k[0:20,x0:x0+60])
  cp=TRANSPOSE(c_phi[0:20,x0:x0+60])
  q=-w mod 1
  dist=45.*([0:20]+4)
  emin=ASIN(75./dist)*180./!PI
  emin=TRANSPOSE(REBIN(emin,[21,61]))
  badwrap=ABS(.5-q)*2
  phi=TRANSPOSE(phiW[0:20,x0:x0+60])
  er=e*!PI/180.
  ;de=(phi+FLOOR(w))/transpose(k[0:20,x0:x0+60])^2/sin(er)/sqrt(1-sin(er)^2)
  eproblem=(emin-e)>0.0
  bw=BOOLARR([61,21])
  bw[WHERE((ABS(eproblem/de)^.5*badwrap) gt .5)]=1
  x=[0:61]/61.
  y=[0:21]/21.

  page=pagedef('plots/Elevation',divs=[1,3])
  ;p=makeplot(title='phiW')
  ;pixelplot,transpose(phiW[0:20,0:60]),clip=[-.5,.5]
  ;for i=0,16 do OPLOT,.0655*[i,i],[0,1],col=255,thick=2

  p=makeplot(title='elevation',/noaxis)
  pixelplot,e,leg=[1.4,1.5],clip=[0,50]
  for i=0,60,4 do OPLOT,[x[i],x[i]],[0,1],col=255,thick=2
  XYOUTS,x[4:*:8],REPLICATE(-y[2],8),REPLICATE('10 12',8),align=.5

  coldhotct
  p=makeplot(title='Elev-minimum',/noax)
  pixelplot,e-emin+ABS(de),clip=[0,!VALUES.F_NAN],leg=[1.4,1.5]
  i=WHERE(e-emin+ABS(de) lt 0)
  PLOTS,x[i mod 61],y[i/61],col=255,psym=8
  for i=0,16 do OPLOT,.0655*[i,i],[0,1],col=127,thick=2
  XYOUTS,x[4:*:8],REPLICATE(-y[2],8),REPLICATE('10 12',8),align=.5

  loadct,39
  !p.background=255
  p=makeplot(title='evelations above minimum',/noax)
  psi2=(phi+FLOOR(w+bw)-dcc)/tk
  e2=180.0d/!DPI*(ASIN(SQRT(cp^2-psi2^2)))
  pixelplot,e2,leg=[1.4,.5],clip=[0,50]
  for i=0,16 do OPLOT,.0655*[i,i],[0,1],col=127,thick=2
  XYOUTS,x[4:*:8],REPLICATE(-y[2],8),REPLICATE('10 12',8),align=.5
  coldhotct
  p=makeplot(title='wrap',/noax)
  pixelplot,w,leg=[1.4,1.5]
  for i=0,16 do OPLOT,.0655*[i,i],[0,1],col=127,thick=2
  XYOUTS,x[4:*:8],REPLICATE(-y[2],8),REPLICATE('10 12',8),align=.5
  ;colbar,p,[min(w,max=m),m],place=[1.2,1],form='%3.1f'

  loadct,39
  !p.background=255
  p=makeplot(title='wrap ambiguity',/noax)
  pixelplot,badwrap^2,clip=[0,1]
  for i=0,16 do OPLOT,.0655*[i,i],[0,1],col=255,thick=2
  ;pixelplot,w+bw,leg=[1.4,1.5]
  XYOUTS,x[4:*:8],REPLICATE(-y[2],8),REPLICATE('10 12',8),align=.5

  ;other wrap
  ;chi_opp= -phi_sign*k*c_phi+dchi_cable
  ;wopp=(chi_opp - phiW)+(-phi_sign lt 0)
  ;pixelplot,transpose(wopp[0:20,0:60]),leg=[1.2,1.6]
  ;sopp=(phiW+FLOOR(wopp)+bw-dchi_cable)/k
  ;topp= c_phi^2 - sopp^2
  ;eopp=180.0d/!DPI*(ASIN(SQRT(topp)))
  ;p=makeplot()
  ;pixelplot,TRANSPOSE(eopp[0:20,0:60]),leg=[1.2,.6]
  DEVICE,/CLOSE
END

pro multiphi,tp,tdifs,phiW,k,phi_sign,frq,c_phi
  colbar,[3:21:3],form='%i'
  foreach tdif,tdifs do begin
    makebox,[21,23],[51,60],units='data'
    XYOUTS,22,52,tdif*1.e6
    elev=phiW2elev(phiW,k,phi_sign,frq,c_phi,tdif)
    pixelplot,TRANSPOSE(elev[*,tp]),/bgc,clip=[3,21]
  endforeach
end

PRO ASSIGNELEV,prm,phiw,k,phi_sign,frq,c_phi,tdif
  screenplot,divs=[4,1],order=[3,2,0,1]
  elev=phiw2elev(phiw,k,phi_sign,frq,c_phi,tdif)
  belv=phiw2elev(phiw,k,-phi_sign,frq,c_phi,tdif)
  zeros=WHERE(phiw eq 0)
  elev[zeros]=!VALUES.F_NAN
  belv[zeros]=!VALUES.F_NAN
  slc=[496:511]
  t0=prm[slc].time.mt
  b7=WHERE(prm.bmnum eq 7 and prm.tfreq gt 11000 and prm.time.hr eq 19)
  x=[0:15]+.5
  y=[0:39]+.5
  for back=0,3 do begin
    if back eq 0 then e2u=elev[*,slc]
    if back eq 1 then e2u=belv[*,slc]
    if back eq 2 then e2u=elev[*,b7]
    if back eq 3 then e2u=belv[*,b7]
    d=REBIN([4:43]*45,SIZE(e2u,/dim))
    vht=SQRT(d^2.+6370.^2+2*d*6370.*SIN(e2u*!PI/180.))-6370.
    d1=WHERE(vht gt 75 and vht le 115)
    e1=WHERE(vht gt 115 and vht le 150)
    f1=WHERE(vht gt 150 and vht le 900)
    l1=WHERE(vht gt 900)
    if back eq 2 then pagerediv,[2,2],cur=2
    if back eq 2 then x=prm[b7].time.mt
    p=makeplot(xr=[x[0]-1,x[-1]+1],yr=[0,40])
    USERSYM,[1,0,1,1]-.5,[0,.5,1,0]-.5,/fill
    PLOTS,x[d1/40],y[d1 mod 40],psym=8,col=e2u[d1]*6,symsize=1.5
    USERSYM,[0,.5,1,0]-.5,[0,1,0,0]-.5,/fill
    PLOTS,x[e1/40],y[e1 mod 40],psym=8,col=e2u[e1]*6,symsize=1.5
    USERSYM,[0,.5,1,0]-.5,[1,0,1,1]-.5,/fill
    PLOTS,x[f1/40],y[f1 mod 40],psym=8,col=e2u[f1]*6,symsize=1.5
    USERSYM,[0,1,0,0]-.5,[0,.5,1,0]-.5,/fill
    PLOTS,x[l1/40],y[l1 mod 40],psym=8,col=e2u[l1]*6,symsize=1.5
    if back ge 2 then OPLOT,t0+[-1,-1,1,1],[0,40,40,0],lin=2
  endfor
  ;pixelplot,TRANSPOSE(elev[*,slc]),leg=[1.3,1],clip=clipk,-phi_sign,frq,c_phi,tdif
  ;pixelplot,transpose(belv[*,slc]),clip=clip
  pagerediv,[2,2]
  for b=0,1 do begin
    e2u=b?belv[*,slc]:elev[*,slc]
    p=makeplot(xr=[0,40],yr=[00,900])
    d=REBIN([4:43]*45,SIZE(e2u,/dim))
    vht=SQRT(d^2.+6370.^2+2*d*6370.*SIN(e2u*!PI/180.))-6370.
    for i=0,39 do PLOTS,i,vht[i,*],psym=4

    ;slidy hist like their fig 5
    p=makeplot(yr=[0,900],xr=[0,40],xtint=5,/xgst)
    htp=FLTARR([40,30])
    for i=0,39 do $;win=[i*.7:i*1.3<40]
      htp[i,*]=normalize(HISTOGRAM(vht[[i*.8:i*1.2<40],*],bin=30,min=0,max=880))
    htp[where(htp eq 0)]=!VALUES.F_NAN
    pixelplot,htp
  endfor
  PRINT,'done'
END


;+
;Get Elevation angles for fitACF files
;GetEchos,years=[2013],/forceRun,/verb,/noask,radars=['rkn'],toGet='phase'
;
;CALL:   angle=FitACFElevation(prm,phi0)
;INPUTS:
;prm    data structure containing
;  interfer
;  phidiff
;  bmnum
;  maxbeam
;  bmsep
;  tfreq
;  tdiff
;phi0   num  uncorrected phase probably
;tdif   num  time delay (in milliseconds), rkn should have about 3.e-6 (3ns)
;deg    bool is the phi0 data in degrees?
;plt    bits  plotting options
;     0 plot to a page / to the screen
;     1 do elevhist
;     2 do time series
;     3 do phi0
;     4
;notry  bool don't find the best tdif, just use the input
;
;HOLDERS:
;err
;
;RETURNS:
;elev   flt  elevation angle, in degrees
;-
function SmartElevation,prm,phi0,site=site,tdif=tdif,g=g,$
  err=err,notry=notry,backlobe=backlobe,plt=plt,deg=deg,nokill=nokill
  ;if ~TOTAL(FINITE(phi0)) then RETURN, -!VALUES.D_INFINITY
  sz=SIZE(phi0,/dim)
  ;d=45*([0:sz[0]-1]+4)
  ;minE=asin((6440.^2-6370.^2-d^2)/2/6370/d)*180./!PI
  ;impossible=where(elev lt rebin(minE,sz),cnt)
  ;bw=bytarr(sz)
  ;bw[impossible]=1

  if ~ISA(plt) then plt=0 else begin
    pBits=ToBits(plt,pad=8)
    if pBits[0] then screenplot else page=pageDef('/home/ullrich/syd/plots/Elevation/test',divs=[1,3])
  endelse
  if plt then if pBits[1] then p=makeplot(xr=[0,sz[0]],yr=[0,70],ytitle='Elevation',xtitle='gate')
  if KEYWORD_SET(deg) then phi0*=!DPI/180

  if ISA(prm) then if ISA(prm[0].stid,/int) then stid=prm[0].stid
  if ~ISA(stid,/int) then MESSAGE,'site required'
  radar_site_read,prm=prm,stid,site=site

  ;;
  ut=prm.time.hr+prm.time.mt/60.+prm.time.sc/3600.
  dtp=WHERE(ut gt 19 and ut lt 21); and hokfil(prm),cnt)
  ;;

  phi_sign=(site.interfer[1] le 0.0)?-1.0d:1.0d;behind or in front of main antenna
  antenna_separation=SQRT(TOTAL(site.interfer[0:2]^2))
  beam=(prm.bmnum-(site.maxbeam/2.0d)+0.5d)
  c_phi= REBIN(TRANSPOSE(COS((site.bmsep*!DPI/180.0d)*beam)),[sz[0],sz[1]]);

  phi0=(((phi0/!PI+11) mod 2)-1)*!PI
  ;phi0[WHERE(ABS(phi0) gt !PI,/null)]=0 ;kill bad data
  zeros=WHERE(phi0 eq 0)
  phiW=phi0/(2.0d*!DPI) ;now in waves
  ;k=2*pi*1000/c*tfreq*sep ;waves that fit between antennas
  k=REBIN(TRANSPOSE((antenna_separation/2.99792e5) * prm.tfreq),[sz[0],sz[1]])
  frq=REBIN(TRANSPOSE(prm.tfreq),[sz[0],sz[1]])

  a=WHERE(prm.tfreq lt 11000,/null)
  b=WHERE(prm.tfreq gt 11000,/null)
  ngate=N_ELEMENTS(phi0[*,0])
  if ISA(plt) then begin
    ;PlotPhi0,phi0,prm
    ch=WHERE((ABS(beam) lt 1) * (prm.tfreq gt 11000),/null)
    oh=WHERE((ABS(beam) gt 7) * (prm.tfreq gt 11000),/null)
    cl=WHERE((ABS(beam) lt 1) * (prm.tfreq lt 11000),/null)
    ol=WHERE((ABS(beam) gt 7) * (prm.tfreq lt 11000),/null)
  endif

  if KEYWORD_SET(tdif) then begin
    if N_ELEMENTS(tdif) gt 1 then begin
      tdifs=tdif
      if MAX(tdif) gt .01 then tdifs*=1.e-6
    endif else begin
      if tdif ge 1 then tdif=0
      tdifs=(tdif+[-40:40:8])*1.e-6
    endelse
  endif else begin
    tdifs=[0:10:1]*1.e-6
    tdifs=[tdifs,15.e-6,20.e-6]
  endelse
  if KEYWORD_SET(notry) then $
    if ~ISA(tdif) then MESSAGE,'need a tdif' else tdifs=[tdif]
  ;if plt eq 3 then tdifs=[-2:20:2]*1.e-6
  err=FLTARR([N_ELEMENTS(tdifs),2])
  try=0

  if ISA(backlobe) then phi_sign*=-1 ;#@this might not supposed to affect elev_corr
  elev_corr=phi_sign*site.phidiff*ASIN(site.interfer[2]/antenna_separation)

  tdifLoop:
  tdif=tdifs[try]  ;dchi_cable= - 2.*!DPI * site.tdiff/1000. * prm.tfreq
  elev=phiW2Elev(phiW,k,phi_sign,frq,c_phi,tdifs[try])
  ASSIGNELEV,prm,phiw,k,phi_sign,frq,c_phi,tdif
  if ~KEYWORD_SET(nokill) then elev[zeros]=!VALUES.F_NAN ;kill bad values
  ;;sanity check, these elevations should turn back into phi0
  phi_real=COS(elev*!PI/180.)*c_phi*k*phi_sign
  ;  Phi0atfreq,elev[*,dtp],ut[dtp],WHERE(ut[dtp] gt 19),$
  ;    STRING(form='%i ns, 12',tdif*1e6),other=prm[dtp],/stretch,/ele
  ;;
  ;  if isa(backlobe) then begin
  ;    backlobe*=-1
  ;    if backlobe eq 1 then elev_front=elev else elev_back=elev
  ;    phi_sign*=-1
  ;  endif
  if KEYWORD_SET(notry) then RETURN,elev

  if ISA(a) then err[try,0]=mean(ABS(elev[1:*,a]-.7*elev[0:-2,a]),/nan)
  if ISA(b) then err[try,1]=mean(ABS(elev[1:*,b]-.7*elev[0:-2,b]),/nan)

  if ISA(pbits) then begin
    tit=STRING(FORMAT='tdif:%4.1f',tdif*1.e6)
    ;if (fix(plt/2) mod 2) then p=makeplot(xr=[0,50],yr=[0,46],ytitle='Elevation',xtitle='gate',tit=tit)
    if pBits[1] then PlotEleHist,elev[*,a]
    if N_ELEMENTS(cl) ge 300 then dtp=cl[WHERE(cl ge dtp[0] and cl le dtp[-1])] else dtp=cl
    if pBits[2] and ~ISA(done) then begin
      tr=prm[dtp].time
      p=makeplot(xd=(tr.hr+tr.mt/60.),yr=[0,ngate+20],$;ngate+20 for room for tdif plot
        ytitle='Gate',xtitle='UT',title='Elevation in Central Beams, 10MHz')
      if ISA(cl) then pixelplot,TRANSPOSE(elev[*,dtp]),y=[0:ngate],bgc=0,leg=(plt ge 2)*[1.2,1]
    endif
    if ~KEYWORD_SET(nokill) then elev[zeros]=!VALUES.F_NAN
    if pBits[3] then plotphi0,elev,prm,tit=tit
    WAIT,.1
  endif

  try+=1 ;do this as a goto instead of a loop so that we can re-use it for final elev calc
  if TOTAL(err[-1,*]) eq 0 then goto,tdifLoop ;if we are not done

  if try lt N_ELEMENTS(tdifs)-1 or ISA(done) then RETURN,elev ;this will happen on the second pass
  if ISA(pbits) then begin
    ;pixelplot,transpose(elev[*,b]),bgc=0
    ;if N_ELEMENTS(cl) ge 300 then pixelplot,TRANSPOSE(elev[*,ch[200:250]]),bgc=0,/leg
    PlotTDifErr,p,tdifs,err
    if ~(pBits[0]) then DEVICE,/CLOSE
  endif
  ; ut=prm.time.hr+prm.time.mt/60.+prm.time.sc/3600.
  ; dtp=WHERE(ut gt 4 and ut lt 4.5 and hokfil(prm),cnt)
  ser=MIN(TOTAL(err,2),try) ;find the best try, and recalc elev for that
  done=1
  goto, tdifLoop ;since err is full here, it won't loop
end


Function GenRad,center,radii,fall=fall,over=over
  if ~KEYWORD_SET(over) then over=0.
  radii=FLOAT(radii)
  if ~ISA(fall) then fall=.5
  if N_ELEMENTS(radii) eq 1 then radii=[radii,radii]
  s=center-radii
  m=center+radii
  x=REBIN([s[0]:m[0]],2*radii+1)/radii[0]
  y=REBIN(TRANSPOSE([s[1]:m[1]]),2*radii+1)/radii[1]
  shape=(x^2+y^2)^fall
  RETURN,(normalize(1-shape,/nan)+over)<1
END


Function MaxTrace,dat,n,shape,ox=ox,oy=oy,init=init
  tdat=dat
  if ~ISA(shape) then begin
    shape=[$
      [0.1,0.1,0.1,0.1,0.1],$
      [0.1,0.2,0.2,0.2,0.2],$
      [0.1,0.2,0.0,1.0,0.5],$
      [0.1,0.2,1.0,1.0,0.5],$
      [0.1,0.2,0.5,0.5,0.5]]
  endif
  sz=SIZE(shape,/dim)
  w=sz[0] & h=sz[1]
  if ~ISA(ox) then ox=sz[0]/2
  if ~ISA(oy) then oy=sz[1]/2
  pos=FLTARR([n,2])
  ;cx=FLTARR(n) & cy=FLTARR(n)
  if ISA(init) then begin
    ;cx[0]=init[0] & cy[0]=init[1]
    pos[0,*]=init
  endif else begin
    ;cx[0]=12 & cy[0]=10
    pos[0,*]=[12,10]
  endelse
  sz=SIZE(dat,/dim)
  shape[ox,oy]=0
  offset=[ox,oy]

  for i=0,n-2 do begin
    ;xs=cx[i]-ox & xm=cx[i]-ox+w-1
    ;ys=cy[i]-oy & ym=cy[i]-oy+h-1
    xys=pos[i,*]-offset
    xym=pos[i,*]-offset+[w,h]-1

    ;top=FIX([xm,ym]+2-[sz])>1
    ;bot=FIX([xs,ys]>0)
    ;xm<=sz[0]-1 & ym<=sz[1]-1
    top=FIX(xym+2-sz)>1
    bot=FIX(xys>0)
    xym<=sz-1

    ;a=tdat[bot[0]:xm,bot[1]:ym]
    ;b=shape[-xs>0:-top[0],-ys>0:-top[1]]
    a=tdat[bot[0]:xym[0],bot[1]:xym[1]]
    b=shape[-xys[0]>0:-top[0],-xys[1]>0:-top[1]]

    new=MAX(a*b,ind)
    ;cx[i+1]=ind mod (w+1-top[0]) - ox + cx[i]
    ;cy[i+1]=ind/(w+1-top[0]) - oy + cy[i]
    pos[i+1,*]=[ind mod (w+1-top[0]),ind/(w+1-top[0])]-offset+pos[i,*]
    ;tdat[cx[i],cy[i]]=0;no repeats
    tdat[pos[i,*]]=0
    ;    if i gt 3 then if TOTAL(cx[i] eq cx[0:i-1]) then $
    ;      if TOTAL(cy[i] eq cy[0:i-1]) then break
    ;    if cy[i] lt 0 or cy[i] gt sz[1] or cx[i] lt 0 or cx[i] gt sz[0] then break
    if TOTAL(pos[i,*] gt sz) or TOTAL(pos[i,*] lt 0) then break
  endfor
  ;RETURN,[[cx[0:i]],[cy[0:i]]]
  RETURN,pos[0:i,*]
END

;+
;CalcElev,rad='rkn',year=2015,/test
;-
PRO CalcElev,rad=rad,year=year,test=test,setTd=setTd,tds=tds
  @'/home/ullrich/syd/sydCommonV2.pro'

  if ~ISA(rad) then rad='rkn'
  if ~ISA(year) then year=2015
  if ~ISA(setTD) then setTD=0
  monDays=[31,28+((year mod 4) eq 0),31,30,31,30,31,31,30,31,30,31]
  baseFile='/home/ullrich/syd/data/sydDen/'
  tds=FLTARR(366) &iter=0
  foreach month,months do begin
    days=[1:monDays[month-1]]
    files=REFORM(GetFiles($
      dirPref,'rkn',year,month,days,/noask)) ;Get the files
    GetMeta,files,types
    allow=WHERE(~types.endswith('ing'),/null)
    foreach i,allow,j do begin
      if types[i].endswith('idl') then $
        RESTORE,files[i] else $
        pts=sydReadFit(file=files[i],type=types[i],scls=scls,dats=dats)
      phi0=dats.phi0[0:49]
      hasDat=phi0[0:49,*] ne 0
      good=WHERE(TOTAL(hasDat,1) ne 0)
      time=scls.time.hr+scls.time.mt/60.+scls.time.sc/3600.
      ;page=pagedef('/home/ullrich/syd/plots/Tdif.ps',divs=[1,2])
      td=setTd
      if KEYWORD_SET(test) then $
        elev=smartelevation(scls,phi0,td=td,plt=3) else $;(j eq 0?2:1)) else $
        elev=smartelevation(scls,phi0,td=td,/notry)
      ;device,/close
      tds[iter]=td
      iter+=1 &td=0
      elev[WHERE(phi0 eq 0)]*=0;=!VALUES.F_NAN

      elev=elev[0:49,good]
      time=time[good]
      tfreq=BOOLEAN(scls[good].tfreq gt 11000)
      beam=BYTE(scls[good].bmnum)
      elev[WHERE(dats[good].gflg[0:49])]*=-1 ;store ground as negative elevation

      name=STRING(format='rkn%4i/%02i%02i_ftd',year,month,days[i])
      if ~KEYWORD_SET(test) then save,time,tfreq,beam,elev,filename=baseFile+name
      PRINT,name
    endforeach
    if ~KEYWORD_SET(test) then FILE_DELETE,files[allow],/allow
  endforeach
  ;Plot,tds[0:iter-1]
END

PRO otherway
  elestr={time:0.,tfreq:0,beam:BYTE(1),elev:FLTARR(50),gflg:BOOLARR(50)}
  data=REPLICATE(elestr,pts)
  good=WHERE(dats[i].qflg)
  for pt=0,pts do gts[pt]=WHERE(hasDat[*,pt])
END


;sq keeps it square
Function plasFreq,elevIn,range,gate=gate,sq=sq,hv=hv,ground=ground
  if ~KEYWORD_SET(ground) then ground=0
  gInd=WHERE(elevIn le 0,gcnt,/Null)
  if ground eq -1 then elev=-elevIn ;flip to use ground
  if ground eq 0 then elev=elevIn ;use sky
  if ground eq 1 then elev=ABS(elevIn) ;use either

  elev[WHERE(elev le 0,cnt,/Null)]=!VALUES.f_nan
  if ~ISA(range) then range=[0:49]*45+180 else $
    if KEYWORD_SET(gate) then range=45*range+180
  if TOTAL(elev gt 20) then elev*=!PI/180. ;converts to radians if needed
  sz=SIZE(elev)
  range=REBIN(range,[sz[1],sz[2]])
  if gcnt ne 0 then range[gInd]/=2 ;Pasha says so
  Re=6366.
  magAng=(7.-range/400.)*!PI/180. ;magnetic angles for RKN
  hT2=(Re^2+range^2.+2*Re*range*SIN(elev)) ;(virtual height+Re)^2
  cf=(1-COS(elev)^2/COS(magAng)^2*Re^2/ht2)
  hv=SQRT(ht2)-Re
  RETURN,KEYWORD_SET(sq)?cf:SQRT(cf)
END

PRO EleHist1012,h4,ti,fom,lom
  moh10=normalize(TOTAL(h4[*,*,ti,[fom:lom],0],4))
  moh12=normalize(TOTAL(h4[*,*,ti,[fom:lom],1],4))
  coldhotct
  pixelplot,moh10-moh12,leg=[.35,1.2],clip=[-1,1]
END

PRO ThickDiamond
  X = [-1, 0, 1, 0, -1]
  Y = [0, 1, 0, -1, 0]
  USERSYM,X*.5,Y*.5,thick=2
END


;+
;plot histColor elevation for each time sector of each month
;year
;log       bool log plotting
;ground    int  1=normal, 0=disclude,-1 only ground
;splitFreq bool splits by 10 vs 12 MHz
;
PRO MTElevHist,year=year,log=log,ground=ground,splitFreq=splitFreq,scr=scr,mnths=mnths
  ;  PROFILER,/RESET
  ;  PROFILER,/SYSTEM
  ;  PROFILER
  @'/home/ullrich/syd/sydCommonV2.pro'
  dat=STRING(FORMAT='/home/ullrich/syd/data/sydDen/rkn%4i/',year)
  if ~KEYWORD_SET(ground) then ground=0 ;treat it as normal ;-1 to plot separate; 0 to disclude
  splitFreq=KEYWORD_SET(splitFreq)
  scr=KEYWORD_SET(scr)

  vhtRng=[0:600:15]
  freqRng=[1:7.9:.15]
  times=[0:24:2]

  numDays=[31,28,31,30,31,30,31,31,30,31,30,31]
  numDays[1]+=~(year mod 4)
  monDays=[0,TOTAL(numdays,/cum)]
  nplot=0

  h4=FLTARR([(7.9-1)/.15+1,41,N_ELEMENTS(times)-1,365,1+splitFreq])
  file_ps=STRING(FORM='/home/ullrich/syd/plots/Elevation/SD_Ionogram%i',year)
  if ground eq -1 then file_ps+='_ground'
  if ground eq 0 then file_ps+='_sky'
  if splitFreq then file_ps+='_freq'
  if scr then ScreenPlot else page=pagedef(file_ps+'_Trace',perdiv=[2,3],space=[-.1,-.1])
  if splitFreq then BlockLabel,['10 MHz','12MHz'],xdivs=[0],xsubs=[0,1],xlabel=0,keep=-1
  if ~ISA(mnths) then mnths=[1:12]
  foreach month,mnths do for day=0,numDays[month-1]-1 do begin
    if FILE_TEST((file=dat+STRING(FORM='%02i%02i_ftd',month,day+1))) then goto, gotFile
    if FILE_TEST((file=dat+STRING(FORM='%02i%02i',month,day+1))) then begin
      gotFile:
      RESTORE,filename=file
      sz=SIZE(elev,/dim)
      f0=REBIN(TRANSPOSE(10+2*tfreq),[sz[0],sz[1]])
      fp=plasfreq(elev,hv=hv,ground=ground)*f0;(9.+f0/10.)
      tfreq*=splitFreq
      doy=mondays[month-1]+day
      for ti=0,N_ELEMENTS(times)-2 do begin
        ttu=WHERE(time gt times[ti] and time le times[ti+1],cnt)
        if cnt eq 0 then continue
        for fsplit=0,splitFreq do begin ;only does the thing when keyword is set
          tt=ttu[WHERE(tfreq[ttu] eq fsplit,cnt2)]
          if cnt2 eq 0 then continue
          h2=hist_2d(fp[*,tt],hv[*,tt],$
            min1=freqRng[0],max1=freqRng[-1],bin1=freqRNG[1]-freqRNG[0],$
            min2=vhtRng[0],max2=vhtRng[-1],bin2=vhtRng[1]-vhtRng[0])
          h4[*,*,ti,doy,fsplit]=h2
        endfor
      endfor
    endif

    s1=GenRad([-2.0,-0.0],[9,7],over=0.0)
    s2=GenRad([-1.0,-2.0],[3,9],over=0.0)
    PRINT,month,day
    if (day eq numDays[month-1]-1) then begin
      fom=mondays[month-1]
      lom=mondays[month]-1
      for ti=0,N_ELEMENTS(times)-2 do begin
        ;EleHist1012,h4,ti,fom,lom
        for fsplit=0,splitFreq do begin
          mohst=TOTAL(h4[*,*,ti,[fom:lom],fsplit],4)
          p=makeplot(xd=freqRng,yd=vhtRng,/xtsub,/ytsub,$
            xtit='Plasma Frequency',ytit='Virtual Height',title=scr?STRING(10+2*fsplit):' ')
          nplot+=1
          if (nplot mod 6) eq 0 then XYOUTS,.5,.95,$
            STRING(FORM='%s %i',monthnames[month-1],year),align=.5,/normal,charsize=2
          n=TOTAL(mohst,/nan)
          nz=WHERE(mohst ne 0,/null)
          if KEYWORD_SET(log) then mohst[nz]=ALOG10(1+mohst[nz])
          pixelplot,mohst,x=freqRNG,y=vhtRng,bgc=0
          ;traceionogram,mohst
          XYOUTS,2,550,STRING(FORMAT='UT: %i-%i!Cn=%i',times[ti],times[ti+1],n),col=222

          Thickdiamond
          xy=maxtrace(mohst,40,s1,init=[14,14])
          PLOTS,xy[*,0]*.15+1,xy[*,1]*15,psym=-8,col=255,thick=2
          xy=maxtrace(mohst,40,s2,init=[14,14])
          PLOTS,xy[*,0]*.15+1,xy[*,1]*15,psym=-8,col=0,thick=2
          if scr then begin
            XYOUTS,.5,.95,STRING(FORM='%s %i',$
              monthnames[month-1],year),align=.5,/normal,charsize=2
            PRINT,'yes'
          endif
        endfor
      endfor
    endif
  endfor
  ; PROFILER,/REPORT
  DEVICE,/close
END


PRO PlotElevHist,year=year,log=log,ground=ground
  ;grandarr=GetElevArr()
  file_ps=plot_dir+STRING(FORMAT='Elevation%4i_18UT',year)
  page=pagedef(file_ps,divs=[1,2])

  yr=[0,43] ;elevation range
  if ground eq 1 then grandarr=ABS(grandarr)
  if ground eq -1 then yr[0]=-43
  for mon=0,11 do begin
    p=makeplot(xr=[0,50],yr=yr,$
      xtitle='range gate',ytitle='elevation',title=monthNames[mon])
    new=FLTARR([50,yr[1]-yr[0]])
    for gate=0,49 do begin
      h=HISTOGRAM(ABS(grandarr[*,*,gate,(mon+[0,1])*30,1080:1140]),$
        min=yr[0],max=yr[1]-1,loc=loc)
      new[gate,*]=h
    endfor
    new[*,WHERE(loc eq 0)]=0

    if KEYWORD_SET(log) then new=ALOG10(new+1)
    new=SMOOTH(new,[2,2])
    levcol,new,lev,col;,log=log
    pixelplot,new
    ColBar,p,lev,colin=col,place=[1,1]
    XYOUTS,30,5,STRING(FORMAT='n=%i',TOTAL(new)),col=255
  endfor
  DEVICE,/close
  ;  screenplot
  ;  pixelplot,REFORM(grandarr[0,0,0,*,*])
  ;  pixelplot,REFORM(denarr[0,0,*,*])
  PRINT,'hi'
END


PRO TraceIonogram,h,xr,yr
  sz=SIZE(h,/dim)
  xm=MAX(h,dim=1,xi,/nan)
  ym=MAX(h,dim=2,yi,/nan)
  if ~ISA(xr) then xr=normalize(FINDGEN(sz[0]),s=!X.CRANGE[0],m=!X.CRANGE[1])
  if ~ISA(yr) then yr=normalize(FINDGEN(sz[1]),s=!Y.CRANGE[0],m=!Y.CRANGE[1])
  ysc=yr[1]-yr[0]
  xsc=xr[1]-xr[0]
  OPLOT,xr+.5*xsc,ysc*(yi/sz[0]+.5),col=0,thick=2
  OPLOT,(xi+.5-sz[0]*[0:sz[0]])*xsc+xr[0],yr+.5*ysc
  PRINT,'traced'
END



PRO TestPlasmaFreq,f0,hmax=hmax,fs=fs,ground=ground
  if ~ISA(f0) then f0=10
  if ~ISA(fs) then fs=.1
  if ~ISA(ground) then ground=0.
  ;n=.1
  ;elev=REBIN(TRANSPOSE([0.:40.-n:n]),[50,40/n])
  elev=REFORM(RANDOMU(seed,50000)*40,[50,1000])
  fact=REBIN([1.0+fs/200.:1.+fs:fs/100.],[50,1000])
  ;fact=1
  elev/=fact
  g=WHERE(RANDOMU(seed,N_ELEMENTS(elev)) lt ground,cnt)
  if cnt ne 0 then elev[g]*=-1.
  cf=f0*plasFreq(elev,hv=hv,/ground)
  sf=MIN(cf,max=mf)
  sh=MIN(hv,max=mh)
  if ISA(hmax) then mh=hmax
  h3=hist_2d(cf,hv,min1=sf,max1=mf,bin1=(mf-sf)/40.,min2=sh,max2=mh,bin2=(mh-sh)/40.)
  h3[0,0]=0 ;fix any nans
  ScreenPlot
  p=makeplot(xr=[sf,mf],yr=[sh,mh],xtint=1,$
    xtit='Plasma Frequency',ytit='Virtual Height',tit='Random Elevations')
  pixelplot,ALOG10(h3+1),levs=levs;,leg=[.35,1.1]
  levcol,h3,levs,cols,/log,n=10
  ColBar,[0,0,1,1],levs,colin=cols,place=[.2,.5]
END