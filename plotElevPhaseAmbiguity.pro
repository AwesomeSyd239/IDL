;@'/home/ullrich/syd/sydPlotToolsV4.pro'
;@'/home/ullrich/syd/sydTools.pro'
PRO PlotElevPhaseAmbiguity,elevs,screen=screen
  if keyword_set(screen) then screenplot,divs=[1,n_elements(elevs)] else $
  page=PageDef('/home/ullrich/syd/plots/elevPhase',fsize=16,divs=[1,3]);perdiv=[1,3])
  !P.Font=-1
  xmax=300.
  freq=10000000. ;10 mhz
  wavelen=3.e8/freq
  radSep=100.
  ytop=[.85,.72,.45]
  ybot=[.72,.42,.1]
  xlft=.1
  xrht=.9
  foreach elev,elevs,ei do begin
    rele=elev*!PI/180.
    ytit='Altitude (m)'
    xtit='Ground Distance (m)'
    ymax=150<(xmax*tan(rele))>40
    if ei ne fix(n_elements(elevs)/2) then ytit=''
    if ei ne (n_elements(elevs)-1) then $
      p=Makeplot(xr=[0,xmax],yr=[-10,ymax],/iso,/noXaxis,xtf='(A1)',ytsub=1,$
      xtit='',ytit=ytit,pos=[xlft,ybot[ei],xrht,ytop[ei]],/auto) else $
      p=Makeplot(xr=[0,xmax],yr=[-10,ymax],/iso,ytsub=1,$
      xtit=xtit,ytit=ytit,pos=[xlft,ybot[ei],xrht,ytop[ei]],/auto)
  
    pathSep=radSep*cos(rele)
    phaseOff=pathSep/waveLen*2.*!PI
    for i=0,1 do begin
        nwave=ceil((xmax-radSep*i)/cos(rele)/wavelen)
      phase=[0:2*!PI*nwave:.01]
      dx=phase*waveLen/2./!PI
      phase+=phaseOff*i
      dy=sin(phase)*10.

      xtp=dx*cos(rele)-dy*sin(rele)+radSep*i
      ytp=dx*sin(rele)+dy*cos(rele)
      ;print,dx[0],dy[0],xtp[0],ytp[0],phase[0]/!PI
      PLOTS,xtp,ytp,col=(floor(phase/2./!PI)*50 mod 255),thick=3,noclip=0
    endfor
    axis,0,0,/xaxis,/nodata,xtickformat='(A1)'
    OPLOT,[100,100-100*SIN(rele)],[0,100*cos(rele)]
    xyouts,radsep/2.,radsep/2.*tan(rele),STRING(FORM='%4.2f',phaseOff/!PI)+' !4p',orient=elev,font=-1,charthick=2.
    xyouts,radSep+10,10,STRING(FORM="!3Ele=%i!Eo",elev),font=-1,charthick=2.
  endforeach
  if isa(page) then device,/close
END
