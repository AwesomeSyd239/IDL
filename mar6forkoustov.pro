@'/home/ullrich/syd/getfilesV3.pro'
@'/home/ullrich/syd/sydReadRadarV4.pro'
@'/home/ullrich/syd/sydElev.pro'
pro Mar6ForKoustov,prm,dat,elev,beams=beams,td=td
if ~isa(beams) then beams=[4,7]
  pts=SydReadFit(file=GetFiles(0,'rkn',2016,3,6),scls=scls,dats=dats)
  good=WHERE(scls.bmnum ge beams[0] and scls.bmnum le beams[-1] and scls.time.hr ge 19 and scls.time.hr le 20,cnt)
  prm=scls[good]
  dat=dats[good]
  phi0=dat.phi0[0:39]
  ;if ~isa(td) then td=3.e-6
  g=dat.gflg[0:39]
  time=prm.time.hr+prm.time.mt/60.+prm.time.sc/3600.
  freq=prm.tfreq
  beam=prm.bmnum
  elev=smartelevation(prm,phi0,td=td,g=g,/notry);,/nokill)
  pwr=dat.pwr0[0:39]
  vel=dat.v[0:39]
  wid=dat.w_l[0:39]
  strt='/home/ullrich/syd/data/mar6_t'+string(format='%i',td*1.e6)
  save,time,freq,beam,pwr,elev,phi0,vel,wid,filename=strt
  PRINT,'done'
  ;b=where(prm.bmnum eq 5 and prm.tfreq le 11000)
  ;p=makeplot(xr=[0,40],yr=[0,max(elev[*,b])])
  ;for g=0,39 do plots,g,elev[g,b]*(phi0[g,b] ne 0),col=(-800>vel[g,b]<200+800)/4.,psym=4
END

PRO elevel,prm,dat,elev,beams=beams,pg=pg
common comPage
  if isa(pg) then page=pagedef('/home/ullrich/syd/plots/elevel',divs=[1,1],perdiv=[2,4]) $
  else screenplot,divs=[2,4]
  palect
  vr=[-800,200]
  gmax=25;39
  if ~ISA(beam) then beam=5
  time=prm.time.hr+prm.time.mt/60.+prm.time.sc/3600.
  times=19.+[0:30:2]/60.
  tfreq=prm.tfreq/1000.
  foreach t,times,ti do for freq=9.5,11.5,2 do begin;foreach beam,beams,b do begin
    gd=WHERE(prm.bmnum eq beam and $
      tfreq gt freq and tfreq lt freq+2 and $
      time ge t and time lt t+2./60.)
    if gd eq -1 then tt=-1 else tt=prm[gd].time.mt+prm[gd].time.sc/60.
    if gd eq -1 then ft=-1 else ft=prm[gd].tfreq
    eMax=180./!PI*ACOS(1-3./tfreq[gd])  
    th=dat[gd] & eth=elev[*,gd] & eerr=.3/tan(eth*!PI/180.)
    v=th.v[0:gmax] 
    x=v & y=eth[0:gmax] & z=[0:gmax] & zMax=gmax & xr=vr & yr=[0,eMax] & xname='velocity' & yname='elevation' & zname='gate'
    ;x=[0:gmax] & y=v & z=eth[0:gmax] & zMax=39 & xr=[0,gmax] & yr=vr &xname='gate' & yname='velocity' & zname='elevation'
    p=makeplot(xr=xr,yr=yr,xtit=xname,ytit=yname,xgstyle=2,$
      title=STRING(form='UT=19:%02i Freq=%i',tt,ft/1000.))
    if page.cur eq 2 then colbar,[0,0,1,.8],[0:zMax],num=10,form='%i',name=zname
    if gd eq -1 then continue    
    gates=where(v ne 0) 
    PLOTS,x[gates],y[gates],color=BYTE((z[gates]*254./zMax)<250),$
      psym=4,clip=[xr[0],yr[0],xr[1],yr[1]],noclip=0,thick=3
    p=where(th.phi0[0:gmax] eq 0 and v ne 0,cnt)
    if cnt eq 0 then continue
    PLOTS,x[p],y[p],psym=1,clip=[xr[0],yr[0],xr[1],yr[1]],noclip=0
  endfor
  if isa(pg) then device,/close
END