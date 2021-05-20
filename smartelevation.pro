@'/home/ullrich/syd/sydPlotToolsV4.pro'

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

PRO plotTdifErr,tdifs,err
  POLYFILL,[-10,50,50,-10],[50,50,70,70],col=255
  XYOUTS,.5,.95,'Tdiff',/normal
  ntd=50*normalize(tdifs)
  ner=20*normalize(err)+50
  OPLOT,ntd,ner[*,0];10mhz
  OPLOT,ntd,ner[*,1],col=222;12 mhz
  axis,0,70,/xaxis,xrange=[tdifs[0],tdifs[-1]];,/font;,charsize=.5
  XYOUTS,0,[50:70:4],STRING(FORMAT='%3.1f',[0:1:.2]),charsize=.5,/align,/font
  XYOUTS,-10,60,/align,'normalized !cbadness',/font,orient=90
END


PRO PlotEleHist,ele,log=log
  eleHist=FLTARR([50,46])
  for i=0,49 do eleHist[i,*]=HISTOGRAM(ele[i,*],min=0,max=45)
  p=makeplot(xr=[0,50],yr=[0,46])
  if ISA(log) then $
    pixelplot,ALOG(eleHist+1),x=[0:50],y=[0:46],levs=levs else $
    pixelplot,eleHist,x=[0:50],y=[0:46],levs=levs
  ColBar,[0,0,1,.9],Levs,colin=[0:255:255/13],form='%i'
END

function phi02elev,phiW,k,phi_sign,frq,c_phi,tdif,tp
  dchi_cable= -tdif*frq
  chi_max= phi_sign*k*c_phi+dchi_cable ;phi0 at elev=0 @!@!
  wrap=FLOOR(chi_max - phiW)+(phi_sign lt 0) ;how many wraps?
  psi= (phiW+wrap-dchi_cable)/k;unwrapped, subtract cable once, then get to distance
  theta= c_phi^2 - psi^2

  bad=WHERE(theta lt 0.0 or theta gt 1.0,cnt)
  elev=180.0d/!DPI*(ASIN(SQRT(theta)))
  ;elev[zeros]=!VALUES.F_NAN ;kill bad values
  ;;sanity check, these elevations should turn back into phi0
  phi_real=COS(elev*!PI/180.)*c_phi*k*phi_sign
  return,elev
end
pro multiphi,tp,tdifs,phiW,k,phi_sign,frq,c_phi
colbar,[3:21:3],form='%i'
foreach tdif,tdifs do begin
  makebox,[21,23],[51,60],units='data'
  xyouts,22,52,tdif*1.e6
  elev=phi02elev(phiW,k,phi_sign,frq,c_phi,tdif)
  pixelplot,transpose(elev[*,tp]),/bgc,clip=[3,21]
endforeach
end
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
function SmartElevation,prm,phi0,site=site,tdif=tdif,$
  err=err,notry=notry,backlobe=backlobe,plt=plt,deg=deg,nokill=nokill
  ;if ~TOTAL(FINITE(phi0)) then RETURN, -!VALUES.D_INFINITY
  if ~ISA(plt) then plt=0 else begin
    if (plt mod 2) then screenplot else page=pageDef('/home/ullrich/syd/plots/Elevation/test',divs=[1,3])
  endelse
  if (FIX(plt/2) mod 2) or (FIX(plt/4) mod 2) $
    then p=makeplot(xr=[0,50],yr=[0,70],ytitle='Elevation',xtitle='gate')
  if KEYWORD_SET(deg) then phi0*=!DPI/180

  sz=SIZE(phi0,/dim)
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
    if ~KEYWORD_SET(tdif) then MESSAGE,'need a tdif' else tdifs=[tdif]
  ;if plt eq 3 then tdifs=[-2:20:2]*1.e-6
  err=FLTARR([N_ELEMENTS(tdifs),2])
  try=0

  if ISA(backlobe) then phi_sign*=-1 ;#@this might not supposed to affect elev_corr
  elev_corr=phi_sign*site.phidiff*ASIN(site.interfer[2]/antenna_separation)

  tdifLoop:
  tdif=tdifs[try]  ;dchi_cable= - 2.*!DPI * site.tdiff/1000. * prm.tfreq
  dchi_cable= -tdif*frq ;waves in delay

  ;Interferometer in front of antenna->lower elevation=earlier arrival/greater phase difference
  ;Otherwise, lower elevation angles=later arrival and smaller phase difference
  chi_max= phi_sign*k*c_phi+dchi_cable ;phi0 at elev=0 @!@!
  wrap=FLOOR(chi_max - phiW)+(phi_sign lt 0) ;how many wraps?
  psi= (phiW+wrap-dchi_cable)/k;unwrapped, subtract cable once, then get to distance
  theta= c_phi^2 - psi^2

  bad=WHERE(theta lt 0.0 or theta gt 1.0,cnt)
  if cnt gt 0 then theta[bad]=-elev_corr
  elev=180.0d/!DPI*(ASIN(SQRT(theta))+elev_corr)
  if ~keyword_set(nokill) then elev[zeros]=!VALUES.F_NAN ;kill bad values
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

  if plt ne 0 then begin
    tit=STRING(FORMAT='tdif:%4.1f',tdif*1.e6)
    ;if (fix(plt/2) mod 2) then p=makeplot(xr=[0,50],yr=[0,46],ytitle='Elevation',xtitle='gate',tit=tit)
    if (FIX(plt/2) mod 2) then PlotEleHist,elev[*,a]
    if N_ELEMENTS(cl) ge 300 then dtp=cl[WHERE(cl ge dtp[0] and cl le dtp[-1])] else dtp=cl
    if (FIX(plt/4) mod 2) and ~ISA(done) then begin
      tr=prm[dtp].time
      p=makeplot(xd=(tr.hr+tr.mt/60.),yr=[0,ngate+20],$;ngate+20 for room for tdif plot
        ytitle='Gate',xtitle='UT',title='Elevation in Central Beams, 10MHz')
      if ISA(cl) then pixelplot,TRANSPOSE(elev[*,dtp]),y=[0:ngate],bgc=0,leg=(plt ge 2)*[1.2,1]
    endif
    if ~keyword_set(nokill) then elev[zeros]=!VALUES.F_NAN
    if (FIX(plt/8) mod 2) then plotphi0,elev,prm,tit=tit
    WAIT,.1
  endif

  try+=1 ;do this as a goto instead of a loop so that we can re-use it for final elev calc
  if TOTAL(err[-1,*]) eq 0 then goto,tdifLoop ;if we are not done

  if try lt N_ELEMENTS(tdifs)-1 or ISA(done) then RETURN,elev ;this will happen on the second pass
  if plt gt 0 and ~(plt mod 2) then DEVICE,/CLOSE
  if plt ne 0 then begin
    ;pixelplot,transpose(elev[*,b]),bgc=0
    ;if N_ELEMENTS(cl) ge 300 then pixelplot,TRANSPOSE(elev[*,ch[200:250]]),bgc=0,/leg
    PlotTDifErr,tdifs,err
  endif
  ; ut=prm.time.hr+prm.time.mt/60.+prm.time.sc/3600.
  ; dtp=WHERE(ut gt 4 and ut lt 4.5 and hokfil(prm),cnt)
  ser=MIN(TOTAL(err,2),try) ;find the best try, and recalc elev for that
  done=1
  goto, tdifLoop ;since err is full here, it won't loop
end