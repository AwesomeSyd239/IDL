;++++++
;Raytracing program made by syd
;Unless otherwise specified, I use the following units
;time      milliseconds
;distance  km
;angles    degrees
;dates     day of year
;
;Update log
;Feb 14, 2021
;Started this log
;Moved IRI profile into RTProfile
;Cleanup various code snippets
;Deleteing obsolete functions
;
;Apr 10, 2021
;Made RTCommon.pro, but not implemented
;Made InterpolateMag and backgroundMagField hopefully will be much faster than WalkRay
;------

;Define these common blocks before I do any other imports
;common RTParams,xgrid,ygrid,den,grad,inter,denNoise,gradNoise
;+
;xg  [flt] density grid spacing (in km) along horizontal distance axis {[0:2000:20]}
;yg  [flt] density grid spacing (in km) along height axis {[0:395:5]}
;it  bool  interpolate locations of rays within grid cells? {true}
;dn  flt   density noise scale (std dev of density noise relative to density) {0.03}
;gn  flt   gradient noise (degrees) {1}
;-
PRO DefineRTParams,xg=xg,yg=yg,it=it,dn=dn,gn=gn
  common RTParams,xgrid,ygrid,den,grad,inter,denNoise,gradNoise
  Common REC,Re,c
  Re=6370. ;km
  c=300. ;km/ms
  ;this line will update anyhting we input, or keep previous values if they exist
  xgrid=(ISA(xg)?xg:(isa(xgrid)?xgrid:[0:2000:20]))
  ygrid=(ISA(yg)?yg:(isa(ygrid)?ygrid:[0:395:5]))
  inter=ISA(it)?it:1
  denNoise=ISA(dn)?dn:.03
  gradNoise=ISA(gn)?gn:1.0
  if isa(xg) or isa(yg) then begin
    den=!Null
    grad=!Null
  endif
END

;+
;Sets up various parameters for raytracing
;mostly to be used by location-specific things like ECHAIM
;yr   int    year
;mo   int    month
;dy   int    day
;ut   int    universal time
;ra   str/rad  radar to use for raytracing
;bm   int   radar beam
;TODO:
;Set up a common block for dates
;-
PRO DefineRTExtras,yr=yr,mo=mo,dy=dy,ut=ut,ra=ra,bm=bm
  common RTExtras,year,month,day,time,strDate,rad,beam
  ;if any of these changes, we need a new profile
  needNewProfile=isa(yr)+isa(mo)+isa(dy)+isa(ut)+isa(ra)+isa(bm)
  year=ISA(yr)?yr:(isa(year)?year:2016)
  IGRF_SETDATETIME,year,err=err
  month=ISA(mo)?mo:(isa(month)?month:3)
  day = ISA(dy)?dy:(isa(day) ? day : 6)
  time= ISA(ut)?ut:(isa(time)? time:19)
  beam= isa(bm)?bm:(isa(beam)? beam: 5)
  strDate=Datestring(year=year,month=month,day=day)
  if isa(ra,/string) then begin
    @'RadDefV2.pro'
    th=WHERE(STRCMP(rads.name,ra,3,/fold),cnt)
    if cnt eq 1 then rad=rads[th] else message,'Unknown radar'
  endif else if isa(ra) then rad=ra
  if needNewProfile then begin
    common RTParams
    den=!Null ;if we change the day, we need to kill any obsolete profile
    grad=!Null
  endif
END

@'/home/ullrich/syd/MapTools.pro'
@'/home/ullrich/syd/RTdrawmag.pro'
@'/home/ullrich/syd/plotIntegrateRT.pro'
@'/home/ullrich/syd/globe.pro'
@'/home/ullrich/syd/echaim_IDL_v3.0/echaim.pro'

;+
;Gets the coords along the ground path from the radar
;INPUTS
;beam [num] desired beams (will return center path)
;RTPARAMS.xgrid   the distances along the beam
;RTEXTRAS.rad     the radar to use
;RTEXTRAS.beam    if you don't specify beams
;HOLDERS:
;lats [flt] latitudes
;lons [flt] longitutdes
;;-
Function RadFov,lats,lons,beams=beams
  common RTParams
  common RTExtras
  if ~isa(beams) then beams=beam;the common one
  f=rad2FOV(rad,rng=xgrid,beam=beams)
  lats=mean(f[1:-2,1,*],dim=1)
  lons=mean(f[1:-2,0,*]+360*(f[1:-2,0,*] lt 0),dim=1)
  RETURN,0
END


;+
;Multiplies a given density profile by an exponential function
;prof [flt] a density profile
;amt  flt
;ht   flt   height of the center;
;wid  flt   the buff falls off by a factor of e for every width away from center
;-
Function BuffHeight,prof,amt,wid,ht
  x=(FLOAT([0:N_ELEMENTS(prof[0,*])-1])*5-ht)
  buff=(amt-1)*EXP(-(x/wid)^2)+1
  RETURN,prof*REBIN(TRANSPOSE(buff),SIZE(prof,/dim))
END


;+
;calculates the field of view along a radar beam (lat/lon) and then
;asks echaim to make a density profile
;
;INPUTS
;year month day time
;ltime [num] if set, use this local time instead of any UT
;lift   num  changes the height profile, +60 puts e region density on the ground {0}
;mult   num  multiply the desnity {1}
;HOLDERS
;lats/lons [flt] lat and lon of ground track along look direction
;-
PRO EchaimProfile,ltime=ltime,lats=lats,lons=lons,lift=lift,mult=mult
  common RTParams
  common RTExtras
  if ~ISA(lats) then begin
    bad=radfov(lats,lons)
    print,'generating profile from radar'
    if bad then MESSAGE,"problem from fov"
  endif
  if ~isa(lift) then lift=0.
  if ~isa(mult) then mult=1.0
  if ISA(ltime) then time=ltime-mean(lons)/15.
  datime=julday(month,day,year,time,0,0)
  if min(abs(lats)) lt 45 then print,"EchaimProfile will mess this up!"
  wd=ECHAIM(lats,lons,REPLICATE(datime,N_ELEMENTS(xgrid)),ygrid+lift,/storm,/silence)
  ;_wrapper_NmF2_hmF2_combined_2018_new(lats,lons,REPLICATE(datime,N_ELEMENTS(xgrid)),ygrid,/storm)
  den=wd.dens/1.e11*mult
  ; RETURN,den
end


;+
;Ensures smooth transitions in chaim profiles to prevent unrealistic transitions
;Density drops off by 1/e per scale height
;Globals
;Computes density falloff vs altitude using ygrid
;Afects global Den
;INPUTS:
;sclH      flt   the scale height of the decay, in km {8.5}
;denIn     [2d]  a density profile {defaults to the "den" from RTParams}
;useGlobal bool  if true, will set RTParams.den to be the result {~isa(denIn)}
;-
PRO ExtendchaimDown,sclH=sclH,top=top,denIn=denIn,useGlobal=useGlobal
  common RTParams
  if ~isa(denIn) then begin
    denIn=den
    useGlobal=1
  endif
  if ~ISA(scl) then sclH=8.5
  for i=n_elements(ygrid)-2,0,-1 do begin
    if isa(top) then if ygrid[i] gt top then continue
    fallOff=EXP((ygrid[i]-ygrid[i+1])/sclH)
    denIn[*,i]>=denIn[*,i+1]*fallOff
    if isa(top) then denIn[*,i]<=denIn[*,i+1]*falloff
  endfor
  if keyword_set(useGlobal) then den=denIn
END

PRO PLOTSMOOTHLOG,q,amt,col=col
  q2=SMOOTH(q,amt,/edge_truncate)
  OPLOT,ALOG10(ABS(q2)>1.e-10),col=col
END

;+
;INPUTS
;now pulls most from RTParams
;clip [num] angles outside the interval will be truncated {no clipping}
;smth int   if not 0, smooths the gradients {0}
;;-
Function GradCalc,clip=clip,smth=smth
  common RTParams
  sz=SIZE(den,/dim)
  dx=FLTARR(sz)
  for i=1,sz[0]-2 do dx[i,*]=(den[i+1,*]-den[i-1,*])/(xgrid[i+1]-xgrid[i-1])
  dx[0,*]=(den[1,*]-den[0,*])/(xgrid[1]-xgrid[0])
  dx[-1,*]=(den[-2,*]-den[-1,*])/(xgrid[-2]-xgrid[-1])

  dy=FLTARR(sz)
  for i=1,sz[1]-2 do dy[*,i]=(den[*,i+1]-den[*,i-1])/(ygrid[i+1]-ygrid[i-1])
  dy[*,0]=(den[*,1]-den[*,0])/(ygrid[1]-ygrid[0])
  dy[*,-1]=(den[*,-2]-den[*,-1,*])/(ygrid[-2]-ygrid[-1])

  grad=ATAN(-dx/dy)
  grad[WHERE(~FINITE(grad),/null)]=0
  if ISA(clip) then grad=clip[0]>grad<clip[1]
  if ISA(smth) then begin
    sd=stddev(grad) &  ng=mean(ABS(grad))
    grads=SMOOTH(grad,[smth,smth],/edge_truncate)
    bad=ABS(grad-grads)/sd
    grad=SIGNUM(grad)*(ABS(grad)*ng^bad)^(1./(bad+1.))
  endif
  RETURN,grad
END


Function Snell,e,n1,n2,h1,h2,g=g
  sinPhi=h1/h2*COS(e)
  up=2*(e ge 0)-1 ;down has -1 (am going away from earth)
  above=2*(e le g)-1 ;relative to gradient
  phi=asin(sinPhi)*up+!PI*(e lt 0) ;angle between incoming ray and center of earth
  r1=where(~finite(phi),cnt)
  if cnt gt 0 then phi[r1]=!PI/2.
  if cnt gt 0 then print,'Snell weirdness'
  sinRef=(n1/n2*sin(phi+g))
  eo=phi+2*g-!PI/2.;reflection is easy to calculate
  refract=where(sinRef lt 1,cnt,/null)
  eo[refract]=g[refract]+above[refract]*(asin(sinRef[refract])-!PI/2.)
  ;et=Snell0(e,n1,n2,h1,h2,g=g)
  ;bad=where(abs(eo-et) gt .02,bc)
  return,eo
END

;Function Snell0,e,n1,n2,h1,h2,g=g,dNdH=dNdH
;  if ISA(g) then begin
;    b2=(1-2*(e gt g))*!PI/2 ;positive 90 degrees for rays coming from above
;    inc=g+(ASIN(-1>h1/h2*COS(e)<1)-!PI/2)*SIGNUM(e)-b2
;    st2=n1/n2*SIN(inc)
;    refract=WHERE(ABS(st2) lt 1,cnt);opposite conditions for above vs below refracting surface
;    eo=g+(inc+b2)*(ABS(st2) gt 1)
;    if cnt ne 0 then eo[refract]=g[refract]-(ASIN(st2[refract])+b2[refract])
;    if TOTAL(~FINITE(eo)) then MESSAGE,'something wrong'
;  endif else begin
;    snl=n1/n2*COS(e)*h1/h2;snells law, spherical
;    eo=ACOS(snl)*SIGNUM(e);assigned blindly, will fix errors next
;    ref=WHERE(snl gt 1,cnt) ;reflection
;    if cnt gt 0 then eo[ref]=-e[ref];SIGNUM(e[ref])*ACOS(2.-snl[ref]);reflect by flipping angle
;  endelse
;  ;z=WHERE(ABS(eo) le 2.e-2,cnt)
;  ;if cnt ne 0 then eo[z]-=2.2*dNdH;2.e-2 ;prevent pederson
;  RETURN,eo
;END


;+
;gdst,hout=current (propagation) x and y
;HOLDERS
;?here d=density g=gradient
;-
PRO GetRefracts,gdst,hout,dhere,ghere,dndh=dndh
  common RTParams
  xind=VALUE_LOCATE(xgrid,gdst)
  yind=VALUE_LOCATE(ygrid,hout)
  if KEYWORD_SET(inter) then begin
    ;this is just the cleanest way I found to do interpolation properly
    dx=(gdst-xgrid[xind])/(xgrid[1]-xgrid[0])
    dy=(hout-ygrid[yind])/(ygrid[1]-ygrid[0])
    dhere=INTERPOLATE(den,xind+dx,yind+dy,/double)
    dNdH=(den[xind,yind+1]-den[xind,yind])/5.
    ghere=INTERPOLATE(grad,xind+dx,yind+dy,/double)
  endif else begin
    dhere=den[xind,yind]
    ghere=grad[xind,yind]
  endelse
END

;+
;lat/lon/azm  radar location and direction
;steps   int  number of steps to take
;w
;e      [flt] initial elevations of rays
;ior_0  flt  frequency dependent conversion from den to ior
;dt     flt   time step (in milliseconds)
;hop     boool keep propagating after hitting the ground?
;hout,gdst [] propagation paths in altitude and ground distances
;lats/lons [] propagation in geographic coords
;xyz       [] propagation in xyz earth centered coords
;-
PRO TraceRaysV6,lat,lon,azm,steps,w,e,ior_0,dt,$
  hop=hop,$
  hout=hout,lats=lats,lons=lons,xyz=xyz,gdst=gdst
  common rec
  common RTParams
  xyz=FLTARR([w,steps,3])
  xyz[*,0,*]=TRANSPOSE(REBIN(Pol2Car([Re,lon,lat]),[3,1,w]))
  gdst=FLTARR([w,steps])
  n1=fltarr(w)+1. & an1=0
  if ~KEYWORD_SET(grad) then MESSAGE,"no grad"; grad=gradCalc(clip=[-!PI/4.,!PI/4.],smth=4)
  const=COS(e[*,0])*hout[*,0]
  ranDen=1+RANDOMN(seed,SIZE(e,/dim))*denNoise
  ranGrad=RANDOMN(seed,SIZE(e,/dim))*gradNoise*!PI/180.
  for i=0,steps-2 do begin
    step=c*dt*n1
    dxyz=azelstep(lons[*,i],lats[*,i],azm[*,i],e[*,i],c*dt,/radians)
    xyz[*,i+1,*]=xyz[*,i,*]+dxyz*rebin(n1,[w,3])
    ;polar coords
    gdst[*,i+1]=gdst[*,i]+step*COS(e[*,i])
    hout[*,i+1]=SQRT(hout[*,i]^2+(step)^2+step*2*hout[*,i]*SIN(e[*,i]))
    ;index of refraction
    GetRefracts,gdst[*,i+1],hout[*,i+1],d,g
    n2=SQRT(1.-IOR_0*d*randen[*,i]) ;index of refraction
    e[*,i+1]=Snell(e[*,i],n1,n2,hout[*,i],hout[*,i+1],g=g+rangrad[*,i])
    n1=n2;update n
    gnd=WHERE(hout[*,i+1] lt Re,cnt)
    if cnt gt 0 and ISA(hop) then e[gnd,i+1]*=-1

    lons[*,i+1]=ATAN(xyz[*,i+1,1],xyz[*,i+1,0])
    lats[*,i+1]=ASIN(xyz[*,i+1,2]/hout[*,i+1])
    ;has issues near pole (formula is correct, computer fails) try azm+=dLon
    azm[*,i+1]=ASIN(SIN(azm[*,i])*COS(lats[*,i])/COS(lats[*,i+1]))
    add=!PI*(azm[*,i] ge 1.571)
    flipAzm=2*(azm[*,i] lt 1.571)-1
    azm[*,i+1]=add+flipAzm*azm[*,i+1]
    badAzm=WHERE(~FINITE(azm[*,i+1]),bac)
    if bac ge 1 then begin
      azm[badAzm,i+1]=azm[badAzm,i]-(lons[badAzm,i+1]-lons[badAzm,i])
      if not isa(woe) then begin
        woe=1;warn of (potential) error
        print,"pole flipping takes place"
      endif
    endif
  endfor
  print,"done main trace"
END

;+
;INPUTS
;r    [flt] altitudes
;lats [flt]
;lons [flt]
;xyz  [flt]
;RETURNS
;aspect [flt] angles between rays and magnetic field
;-
Function WalkRay5B,r,lats,lons,xyz
  common ReC
  clat=!PI/2.-lats;because igrf wants co-latitude
  w=size(r,/dim)
  dot=fltarr([w[0],w[1]-1])
  mxyz=fltarr([w[1],w[0]])
  st = SIN(clat) & ct = COS(clat)
  sp = SIN(lons) & cp = COS(lons)

  dir=(REFORM(xyz[*,1:*,*])-REFORM(xyz[*,0:-2,*]))/2. ;difference gives direction of travel
  dir/=REBIN(SQRT(TOTAL(dir^2,3)),[w[0],w[1]-1,3]) ;normailze
  for k=0,w[0]-1 do begin
    mag=TRANSPOSE(syd_IGRF_COMPUTE(r[k,*]/Re,clat[k,*],lons[k,*]));this gets spherical magnetic field, i need xyz
    nm=mag/REBIN(SQRT(TOTAL(mag^2,2)),[w[1],3]) ;normalized
    ;nm is inward,northward,eastward on the globe. The next four lines turn it into my cartesian system
    be = nm[*,0]*st[k,*] + nm[*,1]*ct[k,*] ;amount of field pointing at 0,0,z
    mxyz[*,0] = be*cp[k,*] - nm[*,2]*sp[k,*] ;x
    mxyz[*,1] = be*sp[k,*] + nm[*,2]*cp[k,*] ;y
    mxyz[*,2] = nm[*,0]*ct[k,*] - nm[*,1]*st[k,*] ;z

    mdir=(mxyz[1:*,*]+mxyz[0:-2,*])/2. ;technically not needed, want to match dimensions of dxyz
    dot[k,*]=TOTAL(mdir*REFORM(dir[k,*,*]),2);dot product
  endfor

  aspect=ACOS(dot)
  RETURN,180.*(1.-aspect/!PI)
END


;todo: make a standard coordinate system of hieght,circle path
;this will allow multiple traces to use the same igrf calc, since it is an expensive operation
function backgroundMagField
  common ReC
  common RTParams ;coord grids live in here
  if radfov(lats,lons) lt 0 then message,"RadFov Failed?"
  ;setup stuff
  clat=!PI/180.*(90.-lats);because igrf wants co-latitude
  lons*=!PI/180.
  sz2d=[n_elements(lats),n_elements(ygrid)]
  sz=[n_elements(lats),n_elements(ygrid),3]
  magGridXYZ=fltarr(sz)
  st = rebin(transpose(SIN(clat)),sz2d) & ct = rebin(transpose(COS(clat)),sz2d)
  sp = rebin(transpose(SIN(lons)),sz2d) & cp = rebin(transpose(COS(lons)),sz2d)
  ;get the magnetic field (this is a very slow calculation)
  ;hopefully it gets the thing in one shot, otherwise
  magGridSphere=magGridXYZ
  fromEarthCenter=ygrid/Re+(ygrid[0] lt Re);altitudes for igrf_compute
  foreach ht,fromEarthCenter,ind do magGridSphere[*,ind,*]=$
    transpose(syd_IGRF_COMPUTE(ht,clat,lons))

  nm=magGridSphere/REBIN(SQRT(TOTAL(magGridSphere^2,3)),sz) ;normalized
  ;nm is inward,northward,eastward on the globe. The next four lines turn it into my cartesian system
  be = nm[*,*,0]*st + nm[*,*,1]*ct ;amount of field pointing at 0,0,z (geographic north pole)
  magGridxyz[*,*,0] = be*cp - nm[*,*,2]*sp ;x
  magGridxyz[*,*,1] = be*sp + nm[*,*,2]*cp ;y
  magGridxyz[*,*,2] = nm[*,*,0]*ct - nm[*,*,1]*st ;z
  return,magGridXYZ
END

Function AspectFromInterpolateMag,gdst,hout,xyz,magGrid

  common RTParams
  ;gdist[ray#,step#]
  nrays=n_elements(gdst[*,0])
  steps=n_elements(gdst[0,*])
  mdir=fltarr([steps,3])

  dot=fltarr([nrays,steps-1])
  dir=(REFORM(xyz[*,1:*,*])-REFORM(xyz[*,0:-2,*])) ;difference gives direction of travel
  dir/=REBIN(SQRT(TOTAL(dir^2,3)),size(dir,/dim)) ;normailze
  for k=0,nrays-1 do begin
    xind=VALUE_LOCATE(xgrid,gdst[k,*])
    yind=VALUE_LOCATE(ygrid,hout[k,*])
    ;this is just the cleanest way I found to do interpolation properly
    dx=(gdst[k,*]-xgrid[xind])/(xgrid[1]-xgrid[0])
    dy=(hout[k,*]-ygrid[yind])/(ygrid[yind+1]-ygrid[yind])
    for i=0,2 do mdir[*,i]=INTERPOLATE(magGrid,xind+dx,yind+dy,replicate(i,size(xind,/dim)),/double)

    dot[k,*]=TOTAL(mdir[1:*,*]*REFORM(dir[k,*,*]),2);dot product
  endfor
  aspect=ACOS(dot)
  RETURN,180.*(1.-aspect/!PI)
END

;+
;maps the density grid into the sky
;-
PRO RaySkyColor,levs,cols
  common Rec
  common RTparams
  sz=SIZE(den,/dim)
  thet=REBIN(!PI/2.+(MAX(xgrid)/2.-xgrid)/Re,sz)
  radii=TRANSPOSE(REBIN(ygrid,[sz[1],sz[0]]))
  x=radii*COS(thet) & y=radii*SIN(thet)
  ;levcol,den,levs,cols,over=255,nlev=15,/log
  PaleCT
  levs=[0,2,4,8,12,16,20,24,28,32,36,40,45]*0.1
  cols=[[1.:254.:22.5],255]
  Contour,den,x,y,/over,c_col=cols,lev=levs,/fill
  if KEYWORD_SET(grad) then begin
    lev=[-45,-20,-10,-5,-2,2,5,10,20,45]&col=[0:252:28]
    Contour,SMOOTH(grad,[4,4])*180./!PI,x,y,/over,lev=lev,c_col=col;,/cell_fill
  endif
  ColBar,levs*10,place=[1.45,.4],col=cols,form='%i',posOut=posOut;,num=9
  XYOUTS,posOut[1]+3*FLOAT(!D.Y_CH_SIZE)/!D.Y_VSIZE/2.,(posout[2]+posout[3])/2.,$
    'Electron Density (10!E10 !Nm!E-3!N)',/normal,orientation=90,charsize=.85,align=.5
END


;aspect 0 is the thign
PRO RayGates,h,inds
  tof=FLTARR(w) &  gr=FLTARR(w)
  tts=FLTARR(w) &  sr=FLTARR(w,2)
  for i=0,w-1 do begin
    if ISA(hop) then begin
      dec=whereblocks(e[i,*] lt 0)
      tof[i]=dec[1,0]
    endif else tof[i]=WHERE((h[i,*] gt 0) * (SHIFT(h[i,*],-1) lt 0))
    tts[i]=zeronear(e[i,*],tof[i]/2)
    gr[i]=x[i,tof[i]]
    sr[i,0]=x[i,tts[i]]
    sr[i,1]=h[i,tts[i]]
  endfor
  ;plot gates, 5 at a time
  for t=1.2/2.,tf,.3/2.*5. do begin ;the divide by 2 is for round trip
    tx=x[*,t/dt]
    ty=h[*,t/dt]
    OPLOT,tx,ty,col=255,thick=2
  endfor
  ;XYOUTS,gr,REPLICATE(0,w),STRING(FORM='%i',(2*dt*tof-1.2)/.3),col=255
  ;XYOUTS,sr[*,0],sr[*,1],STRING(FORM='%i',(2*dt*tts-1.2)/.3);(2*(dt*tts*c-180))/45.)
END

FUNCTION RISRCoords,bmAzm,bmEle,alts
  common Rec
  nStep=n_elements(alts)
  risr=Pol2CAR([Re,-94.9,74.7]) ;risr location
  if bmEle gt 89 then dsts=alts else begin
    sinConst=(alts+Re*(alts lt 6370))/sin((90+bmEle)*!PI/180.);the limit catches vertical
    skyAng=asin(Re/sinConst)*180./!PI;sin lawa
    gndAng=90-bmEle-skyAng ;angles must add to 180
    dsts=sin(gndAng*!PI/180.)*sinConst
  endelse
  ;all in cartesian coords now
  rtp=fltarr([3,nstep])
  for i=0,nstep-1 do begin
    dxyz=azelstep(-94.9,74.7,bmAzm,bmEle,dsts[i])
    ;pts=TRANSPOSE(REBIN(risr,[3,nstep]))+rebin(dsts,[nstep,3])*rebin(dxyz,[nstep,3])
    rtp[*,i]=car2pol(risr+dxyz,/rad)
  endfor
  return,rtp
END

@'/home/ullrich/syd/RTprofile.pro'
;+
;-
PRO PlotRISR,xtp,lats,levs,cols,risrDen
  common Rec
  common RTExtras
  common RTParams
  hts=ygrid
  bmele=34.9 & bmAzm=177
  if strcmp(strdate,'06/mar',5,/fold) then begin
    dats=filterEvent('readmar6',time+[-.05,.05],3)
    hts=*(dats[0].hts)
    risrden=*(dats[0].den)/10.
    bmEle=dats[0].ele
    bmAzm=dats[0].azm
  endif
  nstep=n_elements(hts)
  risrRTP=RISRCoords(bmAzm,bmEle,hts);beam 3 azim elev dist
  np=FLTARR([2,nstep])
  cls=VALUE_LOCATE(lats[0,*],risrRTP[2,*]) ;cls is where lats are closest to each element of risr lat
  for i=0,nstep-1 do np[*,i]=[xtp[0,cls[i]],risrRTP[0,i]]
  loadct,12,/silent
  OPLOT,np[1,*],np[0,*],/polar,thick=2,psym=-4,col=150
  XYOUTS,re*COS(np[0]),re*SIN(np[0])-50,'RISR',col=150
  palect
  if isa(risrDen) then begin
    xrisr=np[1,*]*cos(np[0,*])
    yrisr=np[1,*]*sin(np[0,*])
    ;print,[TRANSPOSE(hts),np[1,*]-Re,TRANSPOSE(risrDen)]
    plots,xrisr,yrisr,col=byte(interpol(cols,levs,risrden)),thick=3,psym=-4,noclip=0
  endif
END

;+
;bm     int   risr beam
;screen bool  plot to screen?
;pg     bits  set 0 to make single page
;             set 1  to make page and leave it open
;             set 2+ to plot on existing page
;for i=1,11 do RISRvsECHAIM,bm=i,pg=i,t=19.+[-.1,.1]
;-
PRO RISRvsECHAIM,bm=bm,screen=screen,pg=pg,t=t,$
  shft=shft,scr=scr
  if ~isa(t) then t=[18,20];ut
  definertextras,yr=2016,mo=3,dy=6,ut=mean(t)
  common RTExtras
  if ~isa(pg) then pg=0
  bmStr=string(form='beam_%i',bm)
  dats=filterEvent('readmar6',t,bm)
  hts=*(dats[0].hts)
  rden=*(dats[0].den)/10.
  nstep=n_elements(hts)
  rtp=risrcoords(dats[0].azm,dats[0].ele,hts)
  lats=rtp[2,*]*180./!PI
  lons=rtp[1,*]*180./!PI
  alts=rtp[0,*]-6370.

  datime=julday(month,day,year,time,0,0)
  wd=ECHAIM(lats,lons,REPLICATE(datime,nStep),alts,/storm,/silence)
  eden=wd.dens/1.e11
  ;if isa(shft) then eden=screwwithChaim(eden,shft,scr)
  maxDen=max([max(eden),max(rden)])

  if isa(screen) then screenplot else $
    if pg le 1 then page=pageDef('/home/ullrich/syd/plots/RISRvsECHAIM'+bmStr,divs=[1,2])

  p=makeplot(xd=lats,yd=alts,xstyle=9,$
    title='ECHAIM for RISR '+bmStr+' '+strdate,ytitle='Altitude',xtitle='Latitude')
  pixelplot,eden[0:-2,0:-2],x=lats[1:-1],y=alts[0:-2],$
    leg=[1.2,1],cols=cols,levs=levs,clip=[0,maxden]
  axis,0,alts[-1],xaxis=0,xtickV=lons,xtickname=lons,xtitle='Longitude'
  plots,lats,alts,col=byte(interpol([0:255:255/12],levs,rden)),thick=3,psym=-4

  p=makeplot(xd=lats,yd=[0,maxden],$
    title='ECHAIM and RISR '+bmStr+' '+strDate,xtit='Latitude',ytit='Density')
  oplot,lats,rden,col=0,thick=3
  chmLine=fltarr(nstep)
  for i=0,nstep-1 do chmLine[i]=eden[i,i]
  oplot,lats,chmline,col=100,thick=3
  ilts=unique(fix(lats))
  nin=n_elements(ilts)
  ialt=interpol(alts,lats,ilts)
  axis,0,maxden,xaxis=0,xticks=nin-1,xtickV=ilts,xtickname=ialt,xtitle='Altitude'
  pos=makelegend(['RISR','ECHAIM'],[0,100],p)
  if isa(page) and (pg eq 0) then device,/close
END


;+
;Makes a plot of echos height and elevation vs range gate
;INPUTS:
;startEnd  int  kills the device, not sure why
;gate
;hecho
;sym   int plot symbol
;plt   in case you want to put this on top of an existing plot
;sz    flt symbol size
;sz,title,xr,yr  Used for Makeplot
;-
PRO EchoAltPlot,startEnd,gate,hEcho,sym,col=col,plt=plt,sz=sz,title=title,xr=xr,yr=yr
  if startEnd le 0 then begin
    if ~ISA(xr) then xr=[0,40]
    if ~ISA(yr) then yr=[50,MAX(hecho)>300]
    plt=makeplot(xr=xr,yr=yr,xgsty=2,ygsty=2,xtlen=.5,ytlen=.5,xtint=5,$
      tit=optstring('Echo Elevation Angles',title),xtitle='Range Gate',ytitle='Altitude (km)')
    OPLOT,xr,[120,120],thick=3
  endif
  PLOTS,gate,hEcho,psym=sym,col=col,clip=[0,yr[0],xr[1],yr[1]],NOCLIP=0,symsize=sz
  if startEnd eq 2 or startend lt 0 then DEVICE,/CLOSE
END


;+
;RayTrace
;CALL:  RayTraceMainV2,e0=e0,dt=dt,tf=tf,freq=freq,hop=hop,iht=iht,idn=idn,interp=interp,$
;       mag=mag,lat=lat,lon=lon,azm=azm,rad=rad,beam=beam,year=year,page=page
;
;INPUTS:
;e0     [num] elevations
;dt     num   time step (tf/1000) or number of steps {1000 is great)
;tf     flt   time stop (millisec) {5}
;freq   num   radar requency {10 MHz}
;hop    bool  do hops {false}
;mag    bool
;rad    rad2  radar
;lat    num   latitude of start (90=north pole){rad.lat}
;lon    num   longitude of start {rad.lon}
;azm    num   azimuth of start (0=north) {rad.azm)
;beam   num   beam
;pg     bool  if set, make plots
;     if string, make ps with this name,
;     if set to 1, assume a plot surface exists
;     otherwise plot on screen
;HOLDERS
;gate   [flt] gates where echos occur
;hEcho  [flt] heights of echos
;echEle [flt] elevation angles of echoe
;echPow [flt] power of echos
;-
PRO RayTraceMainV6,e0=e0,dt=dt,tf=tf,freq=freq,hop=hop,aspCon=aspCon,$
  mag=mag,lat=lat,lon=lon,azm=azm,pg=pg,$
  gate=gate,hEcho=hEcho,echEle=echEle,echPow=echPow
  common rec
  common RTParams
  common RTExtras
  if ISA(rad) then begin
    @'RadDefV2.pro'
    if ISA(rad,/STR) then rad=rads[WHERE(STRCMP(rads.name,rad,/fold))]
    if ~ISA(lat) then lat=rad.lat
    if ~ISA(lon) then lon=rad.lon
    if ~ISA(azm) then azm=rad.bor
    if ISA(beam) then azm+=3.24*(beam-rad.beam/2.)
  endif else begin
    rad={name:'unknown',lat:lat,lon:lon,azm:azm}
  endelse
  if ISA(mag) then begin
    if ~ISA(year) then MESSAGE,'year?'$;IGRF_SETNOW,err=err
    else IGRF_SETDATETIME,year,err=err
    if n_elements(mag) eq 1 then mag=backgroundmagField()
  endif

  if ~KEYWORD_SET(e0) then e0=[0.:45.]
  if ~KEYWORD_SET(tf) then tf=5.
  tf=FLOAT(tf)
  if ~KEYWORD_SET(dt) then dt=tf/1000.
  if ~KEYWORD_SET(freq) then freq=1.e7
  if ~KEYWORD_SET(aspCon) then aspCon=0.1
  if dt gt tf/10. then dt=tf/dt
  steps=tf/dt

  w=N_ELEMENTS(e0)
  e=DBLARR(w,steps)
  e[*,0]=REBIN(e0*!PI/180.,w)
  h=DBLARR(w,steps)
  h[*,0]=Re
  if ygrid[0] eq 0 then ygrid+=Re
  IOR_0=DOUBLE(80.5/freq^2*1.e11) ;w_p^2=n_e*e^2/m_e/e0/(2*pi)^2=80.5*n_e

  lons=REPLICATE(lon*!PI/180.,[w,steps])
  lats=REPLICATE(lat*!PI/180.,[w,steps])
  azm=REPLICATE(azm*!PI/180.,[w,steps])

  TraceRaysV6,lat,lon,azm,steps,w,e,ior_0,dt,$
    hout=h,gdst=gdst,lats=lats,lons=lons,xyz=xyz,/hop

  space=where(h gt 6900,cnt)
  if cnt ne 0 then h[space]=690
  ;tic
  ;aspects=WalkRay5B(h,lats,lons,xyz)
  aspects=aspectfrominterpolatemag(gdst,h,xyz,mag)
  ; toc

  dh=h[*,1:*]-h[0:-2]
  dx=SQRT((c*dt)^2-dh^2)
  x=TOTAL(dx*Re/h[*,0:-2],2,/cum) ;Re/h keeps it in terms of ground dist
  ;i wanna see 'x' vs gdst now
  badAngle=where(~finite(aspects),cntBadAngle)
  if cntBadAngle ne 0 then begin
    print,"bad aspect angles"
    aspects[badAngle]=0
  endif
  echo=WHERE((aspects[*,0:-2] ge 90.) eq (aspects[*,1:-1] le 90.),echoCount)
  tof=WHERE(h gt re and ABS(aspects-90.) lt aspCon,cntof,/NULL)
  ;tof=UNIQUE([tof,echo])
  tof=[tof,echo]
  tof=tof[uniq(tof,sort(tof))]
  kmFromRad=(tof/w)*c*dt
  gate=kmFromRad/45.-4.
  good=WHERE(~((gate lt 8) and h[tof]-Re lt 50));don't count orthogonality in the D region

  gate=gate[good]
  tof=tof[good]
  hecho=h[tof]-re
  decho=x[tof]
  inden=INTERPOLATE(den,decho/(xgrid[1]-xgrid[0]),hecho/(ygrid[1]-ygrid[0]))
  echpow=((inden>.01)^2*1.e16)/kmFromRad[good]^3.;electron^2/m^-6
  echEle=e0[tof mod w]

  if KEYWORD_SET(pg) then begin
    if ISA(pg,'String') then page=PageDef('plots/RayTrace'+rad.name+pg,divs=[1,4]) $
    else begin
      if isa(pg,'STRUCT') then pass=1 else $;doesn'tdo anything
        if pg eq 2 then ScreenPlot,divs=[1,2]
    endelse
    title=rad.name+STRING(FORM=' Beam:%i, Frequency:%i MHz, %s UT%02i',beam,freq/1.e6,strDate,time)

    maxHeight=SQRT(MAX(ygrid)*(MAX(h[echo])<6700))
    plotDists=!PI/2.+MAX(xgrid)/2./Re
    xtp=plotDists-x/Re  ;PRINT,'RaytraceMain: Hey xtp should use h not RE'
    xr=[MAX(xtp,min=ts),ts]
    p=makeplot(xr=Re*COS(xr),yr=[MIN(re*SIN(xtp)),maxHeight],$
      /polar,/ynozero,thick=5,title=title,xtf='(A1)',ytf='(A1)')
    OPLOT,COS([0:2*!PI:.02])*Re,SIN([0:2*!PI:.02])*Re,thick=3

    rayskycolor,levs,cols
    loadct,0,/silent
    for k=0,w-1 do begin
      good=WHERE(h[k,*] ge Re,cnt)
      OPLOT,h[k,good],xtp[k,good],/polar,col=255*FLOAT(w-k)/w
    endfor

    colbar,p,e[*,0]*180./!PI,colin=[w-1:0:-1]*255/w,num=6,place=[0,1],form='%i',posOut=posOut
    XYOUTS,posOut[0]-FLOAT(!D.Y_CH_SIZE)/!D.Y_VSIZE/2.,(posout[2]+posout[3])/2.,$
      'Elevation',/normal,orientation=90,charsize=.85,align=.5
    drawmagloop,lats,lons,x,xtp
    palect

    ;Draw gates
    ;foreach rng,[180:max(xgrid):45*5] do begin
    foreach t,[.6:tf:.15*5.],ti do begin ;it takes this many seconds for light to get to gates
      tp=WHERE(h[*,t/dt] lt maxHeight,cnt)
      if cnt lt 2 then continue
      htps=h[tp,t/dt];htps=SMOOTH(h[tp,t/dt],6,/edge_trunc)
      xtps=xtp[tp,t/dt];xtps=SMOOTH(xtp[tp,t/dt],6,/edge_trunc)
      OPLOT,htps,xtps,/polar,col=255-(ti mod 2),thick=3,psym=1,symsize=.5
    endforeach

    if echoCount ge 1 then OPLOT,h[echo],xtp[echo],/polar,col=150,thick=2,symsize=.3,psym=4
    XYOUTS,re*COS(xtp[0]),re*SIN(xtp[0])-50,rad.name
    ;Draw lines of altitude
    for ht=Re,maxHeight,100 do OPLOT,REPLICATE(ht,100),[0:3:.03],/polar,col=255,thick=2

    if strcmp(rad.name,'rkn',3,/fold) then PlotRISR,xtp,lats,levs,cols
  endif
  ygrid-=Re
END


PRO DenGradPlot
  COMMON RTParams
  common RTExtras
  leg=[1.2,.7]
  xtitle='Ground Distance (km)' & ytitle='Altitude (km)'

  p=makeplot(xd=xgrid,yd=ygrid,title='Electron Density (10!E11 !Nm!E-3!N)'+strDate,/noaxis)
  pixelplot,den,cliplev=[0,4.5]
  FixContour,p,0,0,500,50,xtit=xtitle,ytit=ytitle
  Colbar,p,[0:4.5:.5],place=leg,posOut=posOut,form='%3.1f'
  OPLOT,den[0,*]*100,ygrid,thick=2
  OPLOT,den[-1,*]*100+1500,ygrid,thick=2

  p=makeplot(xd=xgrid,yd=ygrid,xtit=xtitle,ytit=ytitle,title='Ionosphere Angle',/noaxis)
  ;gm=MAX(grad*180./!PI,min=gs)
  pixelplot,grad*180./!PI,x=xgrid[0:-2],y=ygrid[0:-2],cliplev=[-9,9];,leg=leg,cliplev=[-15>gs,15<gm]
  ;  XYOUTS,leg[0],(leg[2]+leg[3])/2.,/normal,'Ionosphere Angle',orientation=90,align=.5
  FixContour,p,0,0,500,50,xtit=xtitle,ytit=ytitle
  Colbar,p,[-9:9:2],place=leg,form='%i'
END


;definertextras,ra='rkn',yr=2016,mo=3,dy=6,ut=18,bm=7
;DefineRTParams,[0:2000:20],[0:400:5]
;+
;pg    bool  plot on postscript page?
;nog   bool  don't do gradients
;ext   str   file name extenssoion
;EchoAltYR to [80,130] to focus on E region
;-
PRO GradTest,pg=pg,nog=nog,ext=ext,echoAltYR=echoAltYR
  COMMON RTParams
  common RTExtras
  if ~ISA(ext) then ext=''

  if ~keyword_set(den) then begin
    echaimprofile
    extendchaimdown
  endif
  if KEYWORD_SET(nog) then grad=0 else grad=gradCalc(clip=[-!PI/3.,!PI/3.],smth=2)
  magGridXYZ=backgroundMagField()
  if KEYWORD_SET(pg) then begin
    pg='plots/RayTrace/Grad_'+rad.name+ext
    page=PageDef(pg,divs=[1,3],border=.75+[0,0,0,0])
  endif else screenplot,divs=[1,2]

  DenGradPlot
  dt=.005d
  tf=MAX(xgrid)/300.
  tec=TOTAL(den[10,*]/[1.:81.]);>2.8
  ;e10=normalize([0.:160.],s=2.,m=(12*tec)) & e12=normalize([0.:160.],s=2.,m=(10*tec))
  e0=[3:40:.2]
  raytracemainv6,e0=e0,mag=maggridxyz,tf=tf,dt=dt,freq=1.05e7,/pg,$
    gate=gate10,hEcho=hEcho10,echEle=echEle10,echPow=echPow10

  raytracemainv6,e0=e0,mag=maggridxyz,tf=tf,dt=dt,freq=1.23e7,/pg,$
    gate=gate12,hEcho=hEcho12,echEle=echEle12,echPow=echPow12

  mg=MAX(gate10)
  if ~ISA(echoAltYR) then echoAltYR=[0,300]
  ss=.8
  TrimMulti,WHERE(hecho10 lt echoAltYR[1] and hecho10 gt echoAltYR[0]),echele10,hecho10,echpow10,gate10
  TrimMulti,WHERE(hecho12 lt echoAltYR[1] and hecho12 gt echoAltYR[0]),echele12,hecho12,echpow12,gate12
  ;PAGEREDIV,[1,2],cur=1
  colscl=255./MAX(echele10)
  echoAltplot,0,gate10,hecho10,6,col=BYTE(echele10*colScl),plt=plt,xr=[0,mg],yr=echoAltYR,tit='+ 10 MHz',sz=ss
  levcol,echele10,levs,cols
  colbar,plt,levs,form='%i'
  ;LinePowerRT,TRANSPOSE([[gate10],[hEcho10],[echEle10],[echPow10]]),[150,200],bin=.5,/sep,plt=plt,cols=[220,120,70,0]
  echoAltPlot,0,gate12,hecho12,4,col=BYTE(echele12*colScl),plt=plt,xr=[0,mg],yr=echoAltYR,tit='+ 12 MHz',sz=ss
  ;LinePowerRT,TRANSPOSE([[gate12],[hEcho12],[echEle12],[echPow12]]),[150,200],bin=.5,/sep,plt=plt,cols=[220,120,70,0]
  closepage
END


;+
;iriden19=extendchaimdown(IRI(19,[0:2000:20]))
;IRIechos19=integratert(dens/1.e11,[0:395:5],[0:3000:30],gscl=1.,iter=50,li=IRI19li)
;returns
;echos [flt] with form [2,4,44000]
;   frequency 10/12
;   gates,heights,elevations,powers
;   44000 samples max
;-
Function IntegrateRT,iter=iter,li=li,tf=tf,dt=dt,yr=yr,npt=npt
  common RTParams
  if ~ISA(iter) then iter=10
  if ~ISA(tf) then tf=5.
  if ~ISA(dt) then dt=.005d
  if ~ISA(npt) then npt=44000L
  grad=gradCalc(clip=[-!PI/3.,!PI/3.],smth=2)
  Echos=FLTARR(2,4,npt)
  li=[0L,0L]
  foreach f,[1.05e7,1.23e7],fi do begin
    e0=normalize([0.:100.],s=3.,m=35);(8.5-1.5*fi)*MAX(dens))
    ind=li[fi]
    stp=0
    for i=0,iter do begin
      raytracemainv6,e0=e0,/mag,tf=tf,dt=dt,$
        freq=f,pg=0,$;2+i for single plot
        gate=gates,hEcho=hEchos,echEle=echEles,echPow=echPow
      ;trim by yRange
      if ISA(yr) then TrimMulti,WHERE(hechos lt yr[1] and hechos gt yr[0],cnt),gates,hechos,echeles,echpow
      n=N_ELEMENTS(hechos)
      now=[li[fi]:li[fi]+n-1]
      if li[fi]+n ge npt then begin
        PRINT,'Too many samples, iteration:',i
        break
      endif else if n eq 0 then begin
        PRINT,"no echos"
        break
      endif
      echos[fi,0,now]=gates
      echos[fi,1,now]=hechos
      echos[fi,2,now]=echeles
      echos[fi,3,now]=echPow
      li[fi]+=n
    endfor
    PRINT,li[fi],fi,i
  endforeach
  li-=1
  RETURN,echos
END


PRO AllDayAssignBins,gate,powcntgat,echpow,nRay,ti,maxgate=maxgate,gatebin=gatebin,ngs=ngs
  if n_elements(gate) le 1 then begin
    print,"AllDayAssignBins has nothing to assign!"
  endif else begin
    gateHist=HISTOGRAM(gate,binsize=gateBin,min=0,max=maxGate,rev=rev)
    for i=0,ngs-1 do if gateHist[i] ne 0 then begin ;iterate through each gate
      pp=rev[rev[i]:rev[i+1]-1]
      ;ereg=WHERE(hecho[pp] lt 120 and etp[1,pp] ge 5,cnt)
      powcntgat[i,ti,0]=TOTAL(echpow[pp]);poweringates([gate,hecho,echele,echpow])
    endif
    powcntgat[*,ti,1]=float(gateHist[0:ngs-1])/nRay/gatebin ;echo count
  endelse
END

;+
;AllDayPlot,powgat=powgat7,cntgat=cntgat7,/loct
;-
PRO AllDayPlot,powcntgat=powcntgat,loct=loct,ext=ext,maxGate=maxGate,gateBin=gateBin
  common RTParams
  common RTExtras
  common Rec
  common compage
  if ~ISA(maxGate) then maxGate=50
  if ~ISA(gateBibn) then gateBin=.5
  powgat=powcntgat[*,*,0]
  cntgat=powcntgat[*,*,1]
  sz=SIZE(powGat,/dim)
  ngs=sz[0]
  step=24./sz[1]
  times=[0:24-step:step]
  if ~ISA(page) then begin
    if ~ISA(ext) then ext=''
    iOwnPage=1
    page=pagedef('plots/RayTrace/Gates'+STRING(FORM='%04i%02i%02i_b%02i',year,month,day,beam)+ext,divs=[1,3])
  ENDIF
  mosTit=STRING(Form=' from RT %s b%02i',strdate,beam)
  if KEYWORD_SET(loct) then xtit='LT' else xtit='UT'
  p=makeplot(title='Power (dB)'+mosTit ,xr=[0,24],yr=[0,maxGate],xtitle=xtit,ytitle='Gate')
  pixelplot,TRANSPOSE(10*ALOG10(powgat)),x=times,y=[0:maxGate:gateBin],leg=[1.4,1],bgc=0

  colPow=powGat
  gates=[0:maxgate-gateBin:gateBin]
  colPow[0:FIX(10/gatebin),*]=0
  weightGate=TOTAL(REBIN(gates,sz)*colPow,1)/TOTAL(colPow,1)
  ;  p=makeplot(xr=[0,24],yr=[0,50],ytitle='Average gate, weighted by power',$
  ;    xtitle='Local Time',title='Gate'+mosTit)
  OPLOT,times+step/2.,weightGate,col=22,thick=3

  p=makeplot(title='Echoes (%)'+mosTit,xr=[0,24],yr=[0,maxGate],xtitle=xtit,ytitle='Gate')
  hv=cntgat[SORT(cntgat)]
  pixelplot,TRANSPOSE(cntgat)*100,x=times,y=[0:maxGate:gateBin],leg=[1.4,1],bgc=0,cliplev=[0,hv[-5]]*100,levs=[0:100:10]
  if ISA(iOwnPage) then closepage
END

;+
;DefineRTExtras,dy=15,ra='rkn',bm=bm
;DefineRTParams,xg=[0:3000:30],dn=dn,gn=gn
;CALL: AllDayTrace,.5,bm=5,powcntgat=pcg,ipowcntgat=ipcg,gpowcntgat=gpcg,pstep=2,userisr=risr
;INPUTS:
;step     num  the time resolution of the traces
;freq     num
;pstep=pstep
;useRisr=useRisr
;HOLDERs:
;?powcntgat
;-
PRO AllDayTrace,step,freq=freq,$
  powcntgat=powcntgat,ipowcntgat=ipowcntgat,gpowcntgat=gpowcntgat,$
  pstep=pstep,useRisr=useRisr,maxgate=maxgate,gatebin=gatebin,profil=profil
  common RTParams
  common RTExtras
  common Rec
  common comPage
  if keyword_set(profil) then sydprof,st=st
  times=[0:24-step:step]
  e0=[1:36:.2]
  if ~keyword_set(freq) then freq=1.2e7
  if ~isa(pstep) then pstep=2
  ;i really thought i could put these in an outside function and use ref_extra to make it neat
   if ~isa(maxGate) then maxgate=50
  if ~isa(gateBin) then gatebin=.5
  ngs=maxGate/gateBin

  fstr=STRING(FORM='_%iMHz',freq/1.e6)
  if ~isa(page) then begin ;page from common block
    pgstr='plots/RayTrace/'+rad.name+fstr+'_'+dateString(year=year,month=month,day=day,sep='_')
    page=PageDef(pgstr,divs=[1,4],space=[.75,.5,0,0]) ;maybe have the ability to do this?
    iOwnPage=1
  endif
  powcntgat=FLTARR([ngs,24/step,2]);power in each gate, and echo occurrence
  gpowcntgat=powcntgat&ipowcntgat=powcntgat
  ;cntgat=powgat & gcntgat=powgat&icntgat=powgat;% echo occurrence
  magGridXYZ=backgroundMagField()
  foreach time,times,ti do begin
    PRINT,strdate,time;time is part of rtextra common block

    if isa(userisr) then risrandechaimprofilebydistance,userisr,lats,lons $;,horizfall=150,vertfall=15,ts=.25,pk=.5
    else EchaimProfile
    grad=gradCalc(clip=[-!PI/3.,!PI/3.],smth=2)
    if not keyword_set(pstep) then doplot=0 $
    else doplot=((time mod pstep) eq 0)?page:0 ;will give the page obect every pstep hours

    raytracemainv6,e0=e0,mag=magGridXYZ,tf=Maxgate*45./c,dt=.005d,$
      freq=freq,gate=gate,hEcho=hEcho,echEle=echEle,echPow=echPow,pg=doplot
    if n_elements(gate) lt 2 then continue
    gnd=where(hecho le 5)
    sky=where(hecho gt 5)
    AllDayAssignBins,gate,powcntgat,echpow,n_elements(e0),ti,maxgate=maxgate,gatebin=gatebin,ngs=ngs
    AllDayAssignBins,gate[sky],ipowcntgat,echpow[sky],n_elements(e0),ti,maxgate=maxgate,gatebin=gatebin,ngs=ngs
    AllDayAssignBins,gate[gnd],gpowcntgat,echpow[gnd],n_elements(e0),ti,maxgate=maxgate,gatebin=gatebin,ngs=ngs
  endforeach
  if isa(loct) then begin
    powcntgat=shift(powcntgat,[0,fix(-6/step),0])
    ipowcntgat=shift(ipowcntgat,[0,fix(-6/step),0])
    gpowcntgat=shift(gpowcntgat,[0,fix(-6/step),0])
  endif

  if isa(iOwnPage) then begin
    closepage
    ;AllDayPlot,powcntgat=powcntgat;,/loct
    AllDayPlot,powcntgat=ipowcntgat,ext='ion'+fstr,maxgate=maxgate,gatebin=gatebin;,/loct
    AllDayPlot,powcntgat=gpowcntgat,ext='gnd'+fstr,maxgate=maxgate,gatebin=gatebin;,/loct
  endif
  if keyword_set(profile) then sydprof,st=st,out='/home/ullrich/syd/profileAllDayTrace'
END



PRO MagTest,start=start,r=r
  if ~isa(r) then r=6670
  if n_elements(r) eq 1 then r=replicate(r,100)
  lat=[40:89.5:.5]*!PI/180.
  lon=REPLICATE(-90,100)*!PI/180.
  clat=!PI/2.-lat
  rtp=REFORM([r/6370,clat,lon],[100,3])
  mag=FLTARR([3,100])
  for i=0,99 do mag[*,i]=IGRF_Compute(rtp[i,*])
  ang=ATAN(mag[1,*]/mag[0,*])*180./!PI
  if ISA(start) then begin
    Screenplot
    p=makeplot(xr=[40,90],yd=mag,xtlen=1,xtitle='latitude',ytitle='field',/ygrid);yr=[-1,1]*2.5e-7
  endif
  lat*=180./!PI
  cols=[0,100,200,50]+(r[0]-6370)/10
  OPLOT,lat,mag[0,*],col=cols[0]
  OPLOT,lat,mag[1,*],col=cols[1]
  OPLOT,lat,mag[2,*],col=cols[2]
  if isa(start) then begin
    pos=makeLegend(['R','T','P','Ang'],cols,p,scale=1.5)
    axis,90,0,YAXIS=1,yrange=[-45,45],yticklen=1,ytickint=15,/save
  endif
  OPLOT,lat,ang,col=cols[3];*2.78e-9
END
