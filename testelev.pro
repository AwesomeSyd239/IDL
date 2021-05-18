PRO testElev
  screenplot,divs=[3,3],order=[3,2,1,0];y then x
  c=3.e8
  f0=[10.:12.:1]
  phi=[-1.:1:.1]
  ts=[-1.:1.:1.]*5.
  cb=COS(3.24/180*!PI*[-7:7:1])
  nf=N_ELEMENTS(f0)
  np=N_ELEMENTS(phi)
  nt=N_ELEMENTS(ts)
  nb=N_ELEMENTS(cb)

  phi0=REBIN(phi,[np,nf])/2. ;x
  freq=TRANSPOSE(REBIN(f0*1.e6,[nf,np]));y
  elev=FLTARR([np,nf,nt])

  b0=.95
  k=100./c*freq
  ppy=[freq[0,0]:freq[-1,-1]:(freq[-1,-1]-freq[0,0])/(nf-1.)]
  bea=[7,11,15]
  for t=0,nt-1 do begin
    tau=ts[t]*1.e-9;TRANSPOSE(phi0)/1.e8 ;+/- 5 ns
    dcc=-tau*freq
    for b=0,2 do begin
      cb=COS(.226*b);center, mid, outside
      cm=(-k*cb+dcc)
      w=FLOOR(cm-phi0)+1.
      psi=(phi0+w-dcc)/k
      elev[*,*,b]=ASIN(SQRT(cb^2-psi^2))*180./!PI
      p=makeplot(xd=phi0,yd=freq,xtitle='phi0',ytitle='freq',$
        title=STRING(FORMAT='Tdif: %4.1f ns, beam:%i',tau*1.e9,bea[b]))
      PixelPlot,elev[*,*,b],x=phi0[*,0],clip=[0,45]
      for i=-1,1 do OPLOT,[i,i]/2.00001,[freq[0,0],freq[0,-1]]
      ;PixelPlot,w,x=phi0[*,0],y=freq[0,*],clip=[-5,0]
    endfor
    ;    epp=makeplot(loc=[2,1],xr=[-.5,.5],yr=[0,45])
    ;    OPLOT,phi0[*,0]-dcc[*,0],elev[*,0,t],col=50
    ;    OPLOT,phi0[*,-1]-dcc[*,-1],elev[*,-1,t],col=200
    if t eq 0 or t eq nt-1 then begin
      PRINT,tau
      for b=0,2 do PRINT,STRING(FORM='10MHz, beam:%02i   ',7+4*b),FIX(elev[*,0,b])
      for b=0,2 do PRINT,STRING(FORM='12MHz, beam:%02i   ',7+4*b),FIX(elev[*,-1,b])
    endif
  endfor
  colbar,[0:45:5],form='%i',place=[0,.5]
END

PRO dec12,mode=mode,freq=freq
  screenplot,divs=[3,3];,order=[3,2,1,0];y then x
  c=3.e8
  if~KEYWORD_SET(freq) then freq=12.
  freq*=1e6
  phi0=[-.45:.45:.01]
  elev=[0.:90.]
  tdif=[-20:20:5]*1.e-9
  csbm=COS(3.24/180*!PI*[-7.5:7.5:1])
  d=-100.
  k=d/c*freq
  if ~KEYWORD_SET(mode) then mode=1
  if mode eq 1 then begin
    y=phi0
    ytitle='phi0'
  endif else begin
    coldhotct
    y=elev
    ytitle='elev'
  endelse

  ny=N_ELEMENTS(y)
  nb=N_ELEMENTS(csbm)
  nt=N_ELEMENTS(tdif)
  y=REBIN(TRANSPOSE(y),[nb,ny])
  csbm=REBIN(csbm,[nb,ny])

  for t=0,nt-1 do begin
    tau=tdif[t]
    dcc=tau*freq
    cm=(k*csbm+dcc)
    if mode eq 1 then begin ;plot elev
      w=FLOOR(cm-y)+1.
      psi=(y+w-dcc)/k
      toPlot=ASIN(SQRT(csbm^2-psi^2))*180./!PI
    endif else begin ;plot phi0
      yt=y*!PI/180.
      if mode eq 2.5 then $
        psi=dcc+k*SQRT(csbm^2-SIN(yt)^2)/COS(yt) else $ ;from interfer paper
        psi=dcc+k*csbm*COS(yt)
      toPlot=((psi+15.5) mod 1) -.5
    endelse

    p=makeplot(xd=[0:15],yd=y[0,*],xtitle='beam',ytitle=ytitle,$
      title=STRING(FORMAT='Tdif: %i ns',tau*1.e9))
    PixelPlot,toPlot,y=y[0,*];,clip=[0,90]
  endfor
  if mode eq 1 then colbar,[0:45:5],form='%i',place=[1,.5]
  if mode eq 2 then colbar,[-.5:.5:.1],form='%4.1f',place=[1,.5]
END

PRO ElevOfPhi0T,freq=freq,mode=mode
  screenplot,divs=[3,3];,order=[3,2,1,0];y then x
  c=3.e8
  if~KEYWORD_SET(freq) then freq=12.3
  freq*=1e6
  phi0=[-.5:.5:.02]
  tdif=[-10:10]*1.e-9
  csbm=COS(3.24/180*!PI*[-7.5:7.5:1])
  d=-100.
  k=d/c*freq
  if ~KEYWORD_SET(mode) then mode=0
  if mode eq 0 then begin
    y=phi0
    ytitle='phi0'
    clip=[0,45]
  endif else begin
    coldhotct
    y=[0:45]
    ytitle='elev'
    clip=[-.5,.5]
  endelse
  x=tdif
  yr=[min(y,max=ym),ym]
  ny=N_ELEMENTS(y)
  nx=N_ELEMENTS(x)
  toplot=FLTARR([nx,ny])
  for b=0,N_ELEMENTS(csbm)-1,2 do begin
    cb=csbm[b]
    for xi=0,N_ELEMENTS(x)-1 do begin
      tau=x[xi]
      dcc=tau*freq
      if ~mode then begin
        cm=(k*cb+dcc)
        w=FLOOR(cm-y)+1.
        psi=(y+w-dcc)/k
        elev=ASIN(SQRT(cb^2-psi^2))*180./!PI
        toPlot[xi,*]=elev
      endif else begin
        
        cm=(k*cb*COS(y*!PI/180.)+dcc)
        phi=(cm+10.5) mod 1
        toPlot[xi,*]=phi-.5
      endelse
    endfor
    p=makeplot(xd=x,yr=yr,xtitle='Tau',ytitle=ytitle,$
      title=STRING(FORMAT='beam: %i',b))
    PixelPlot,toPlot,clip=clip
    OPLOT,[0,0],yr,lin=2,col=255
    OPLOT,[-1,1],[0,0],lin=2,col=255
  endfor
  if mode then $
    colbar,[-.5:.5:.1],form='%4.1f',place=[1,.5] else $
    colbar,[0:45:5],form='%i',place=[1,.5]
END

PRO GENPHI,h=h,r=r,f=f,re=re
  if ~ISA(h) then h=100. else h=FLOAT(h)
  if ~ISA(f) then f=12. else f=FLOAT(f)
  if ~ISA(r) then begin
    gs=22.5
    g0=180
    if h lt 150 then r=FLOAT([g0:810:gs]) else r=FLOAT([g0:1900:gs])
  endif
  nr=N_ELEMENTS(r)
  b=[-7:7]
  nb=N_ELEMENTS(b)
  d=100;m
  ele=r/SQRT(r^2+h^2)
  ;ele=sqrt(r^2-h^2)/r
  if ISA(re) then ele=SQRT(1-(h/r-r/12700.+h^2/r/12700)^2)
  phi0=f/300.*d*REBIN(COS(3.24*b*!PI/180.),[nb,nr])*REBIN(TRANSPOSE(ele),[nb,nr]);*2*!PI
  Screenplot
  coldhotct
  ttl=(h lt 150?'E':'F')+STRING(FORM=' Region, %i MHz',f)
  p=makeplot(xr=[0,15],yd=r,title=ttl,ytitle='Range',xtitle='Beam',/font)
  Pixelplot,(phi0) mod 1,/leg,clip=[0,1]
  axis,15,0,yrange=[r[0],2*r[-1]-r[-2]]/45.-4,/yaxis,ytitle='Gate',/ystyle
END