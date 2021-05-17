;@'/home/ullrich/syd/sydTools.pro'
;@'/home/ullrich/syd/sydPlotToolsV4.pro'
;how to check if the stuff is already compiled?
PRO XYZ,theta,phi,x,y,z,r0=r0,radian=radian
  if ~KEYWORD_SET(r0) then r0=6370.
  tr=theta*(keyword_set(radian)?1.:!PI/180.)
  pr=phi*(keyword_set(radian)?1.:!PI/180.)
  x=r0*SIN(tr)*COS(pr)
  y=r0*COS(tr)*COS(pr)
  z=r0*SIN(pr)
END

PRO Pol,xyz,theta,phi,r0=r0,radian=radian
  if ~KEYWORD_SET(r0) then r0=sqrt(total(xyz^2))
  phi=ASIN(xyz[2]/r0)*(keyword_set(radian)?1.:180./!PI)
  theta=ATAN(xyz[0],xyz[1])*(keyword_set(radian)?1.:180./!PI)
END


PRO flatmap
  Screenplot
  Map_set,/continents
  @'RadDefV2.pro'
  PLOTS,rads.lon,rads.lat,psym=6,symsize=.5
  XYOUTS,rads.lon,rads.lat,rads.name
END

;+
;propogates a perfectly straight line
;-
;Function GetFoV,rads,dir=dir,rng=rng,ls=ls
;  if ~ISA(rng) then rng=2000.
;  if ~ISA(ls) then ls=.01
;  if ~ISA(dir) then dir=rads.bor
;  xyz,rads.lon,rads.lat,x1,y1,z1,r=r0 ;get spacial coords
;  ;take a small step in direction dir, then get new coords
;  nlat=rads.lat+COS(dir*!PI/180.)*ls
;  nlon=rads.lon+SIN(dir*!PI/180.)/COS(nlat*!PI/180.)*ls
;  xyz,nlon,nlat,x2,y2,z2
;  ;follow the step out to full range
;  dx=x2-x1 & dy=y2-y1 & dz=z2-z1
;  del=SQRT(dx^2+dy^2+dz^2)
;  x3=x1+dx/del*rng
;  y3=y1+dy/del*rng
;  z3=z1+dz/del*rng
;  pol,[x3,y3,z3],lon,lat,r=r
;  RETURN,[[lon],[lat]]
;END

Function FastFoV,theta,phi,dir,rng,radians=radians
  Re=6370.
  nr=n_elements(rng) & nd=n_elements(dir)
  if nd eq 1 then dir=dir+[-1,1]
  if nd eq 1 then nd=2
  if ~KEYWORD_SET(radians) then begin
    theta*=!PI/180.
    phi*=!PI/180.
    dir*=!PI/180.
  endif
  ct=COS(theta) & st=SIN(theta)
  sp=SIN(phi) & cp=COS(phi)
  cb=rebin(transpose([rng]),[nd,nr])*rebin(COS(dir),[nd,nr])
  sb=rebin(transpose([rng]),[nd,nr])*rebin(SIN(dir),[nd,nr])
  x2=(Re*st*cp-cb*sp*st+sb*ct)
  y2=(Re*ct*cp-cb*sp*ct-sb*st)
  z2=Re*sp+cb*cp
  lalo=fltarr([nd,2,nr])
  lalo[*,0,*]=ATAN(x2,y2)
  lalo[*,1,*]=ATAN(z2,SQRT(x2^2+y2^2))
  RETURN,180./!PI*lalo
END


Function FoV,lonin,latin,azmin,rngin
  ;input robustness
  if ~isa(rngin) then begin ;only got 3 inputs
    if n_elements(lonin) ne 2 then message,'invalid input to Globe/FoV'
    ;if there were 2 elements in the first input, it's a coordinate pair
    rng=azmin ;re-shuffle the parameters
    azm=latin ;in order to prevent overwrite
    lat=lonin[1] & lon=lonin[0] ;can maybe do these on one line
  endif else begin ;downside of input checking is that it requires this buffering
    lon=lonin&lat=latin
    azm=azmin&rng=rngin
  endelse
  nd=n_elements(azm)
  if nd eq 1 then nd=0
  trac=REBIN(TRANSPOSE([lon,lat]),[nd+2,2,n_elements(rng)])
  if nd ne 0 then trac[1:-2,*,*]=FastFov(lon,lat,azm,rng) else trac[*,*,*]=FastFov(lon,lat,azm,rng)
  trac=trac mod 360 ;polar coords can't be more than 360
  ;o=WHERE(trac gt 360,cnt)  ;if cnt ne 0 then trac[o]-=360
  RETURN,trac
END

;+
;Gets a outline of the desired field of view
;INPUTS:
;rad      rad   Radar to use
;rng/gate [num] ranges to consider {gate=50/range=}
;beamIn   [int] beams to consider
;RETURNS:
;trac     [flt] geographic coors of the outline of the field of view
;-
Function Rad2FoV,rad,rng=rng,gate=gate,beamIn=beamIn
  if ~ISA(gate) then gate=[50]
  if ~ISA(rng) then rng=gate*45+180
  if ~ISA(beamIn) then beam=[0:rad.beam-1.] else beam=beamIn
  beam+=.5-rad.beam/2
  if N_ELEMENTS(beam) eq 1 then beam=beam+[-.5,.5]
  dirs=rad.bor+(beam)*3.24
  return,FoV(rad.lon,rad.lat,dirs,rng)
END

;+
;Give me a trac to plot, I only handle the conversion from
;-
PRO PlotFOV,trac,col=col,dot=dot
  if keyword_set(dot) then PLOTS,trac[0],trac[1],psym=6,symsize=dot
  if ~keyword_set(col) then col=127
  if size(trac,/n_dim) eq 2 then $
    PLOTS,trac[*,0],trac[*,1],col=col,thick=2 else $
    PLOTS,trac[*,0,*],trac[*,1,*],col=col,thick=2,psym=-6,symsize=.1
END

PRO PlotLats,lats
  if ~ISA(lats) then lats=[-80:80:10]
  foreach lat,lats do OPLOT,[0:360:2],REPLICATE(lat,180)
END

PRO PlotLons,lons
  if ~ISA(lons) then lons=[-180:180:30]
  foreach lon,lons do OPLOT,[lon,lon],[-80,80]
END

PRO MagMap
  ;gonna get igrf points
  step=10.
  lolat=[0.,-90.]
  foreach phi,[-90.+step:90.-step:step] do begin
    thetStep=step/cos(phi*!PI/180.)
    foreach thet,[0.:360.:thetStep] do lolat=[lolat,thet,phi]
  endforeach
  ;mnp=[-122,84]
  ;msp=[137,-64]
  np=n_elements(lolat)/2
  lolat=reform(lolat,[2,np])
  ;lats=lolat[1,*]
  clat=(90.-lolat[1,*])*!PI/180.
  lons=(lolat[0,*])*!PI/180.

  mxyz=fltarr([np,3])
  st = SIN(clat) & ct = COS(clat)
  sp = SIN(lons) & cp = COS(lons)
  mag=TRANSPOSE(syd_IGRF_COMPUTE(1.,clat,lons));this gets spherical magnetic field, i need xyz
  nm=mag/REBIN(SQRT(TOTAL(mag^2,2)),[np,3]) ;normalized
  ;nm is inward,northward,eastward on the globe. The next four lines turn it into my cartesian system
  be = nm[*,0]*st + nm[*,1]*ct ;amount of field pointing at 0,0,z
  mxyz[*,0] = be*cp - nm[*,2]*sp ;x
  mxyz[*,1] = be*sp + nm[*,2]*cp ;y
  mxyz[*,2] = nm[*,0]*ct - nm[*,1]*st ;z
  loadct,39,/silent
  contour,nm[*,0],lolat[0,*],lolat[1,*],/over,c_col=[165:255:10],lev=[62:89:3],/iso;,/fill
  ;print,lolat,mags
END

;Gets normailzed mag field at lat/lon/alt
Function GetMags,lons,lats,alt=alt
  igrf_setdatetime,2016
  mm=FLTARR([n_elements(lons),n_elements(lats),3])
  dtr=!PI/180.
  clats=(90.-lats)
  if ~isa(alt) then alt=6370
  foreach clat,clats,ai do foreach lon,lons,oi do begin
    mag=igrf_compute([alt/6370.,clat*dtr,lon*dtr])
    mm[oi,ai,*]=mag/SQRT(TOTAL(mag^2)) ;normalize
  ENDforeach
  RETURN,mm
END

;+
;Makes mouse interactive
;Left click prints coords
;Right click stops program
;-
PRO MouseIn
  WHILE 1 DO BEGIN
    CURSOR,X,Y,/DATA
    if (!MOUSE.button EQ 1) then XYOUTS,x,y,/DATA,STRING(FORMAT='%+4i,%+3i',x,y)
    if (!MOUSE.button EQ 4) then break
  ENDWHILE
END


;+
;Finds the radars that can see a given point on Earth
;INPUTS:
;coords   [lat,lon]
;getrad   bool  gets a list of radars with no further details {false}
;getbeam  bool  narrows it down to the beams within those radars {false}
;getgate  bool  narrows it down to the gates within those beams {true}
;-
Function Rads4Coords,coords,getrad=getrad,getbeam=getbeam,getgate=getgate
  if ~isa(getrad) and ~isa(getbeam) and ~isa(getgate) then getgate=1
  radOut=[]
  @'RadDefV2.pro'
  PLOTS,coords,psym=6,color=55
  foreach rad,rads,i do begin
    print,'checking'+rad.name
    trac=Rad2FoV(rad) ;get the outline of the whole fov
    if ~pntinpoly(coords,trac) then continue
    if isa(getrad) then begin
      radOut=[radOut,rad]
      continue
    endif
    for beam=0,rad.beam-1 do begin
      trac2=Rad2FoV(rad,beam=beam) ;check each beam
      if ~pntinpoly(coords,trac2) then continue
      if isa(getBeam) then begin
        radOut=[radOut,{name:rad.name,beam:beam,gate:rad.gates}]
        break
      endif
      for gate=0,50,5 do begin
        trac3=Rad2FoV(rad,beam=beam,gate=gate) ;check gates
        if ~pntinpoly(coords,trac3) then continue
        radOut=[radOut,{name:rad.name,beam:beam,gate:gate}]
        beam=rad.beam-1 ;skips the rest of looping through radar beams
        break
      endfor
    endfor
  endforeach
  RETURN,radOut
END

function RadByName,rads,names
  radout=[]
  foreach nm,names,i do begin
    ind=WHERE(STRCMP(rads.name,nm,/fold))
    radout=[radout,rads[ind]]
  endforeach
  return,radout
end

@'/home/ullrich/syd/screenPlotTools.pro'
;+
;Sets up a globe plot on the screen
;INPUTS
;hem     str  'n' for north, or 's' for south
;limit  [flt] equatorwards, east, pole, and west boundaries {40,-180,90,180}
;HOLDERS
;rads   [rad] list of radars in the specified hemisphere
;-
PRO globeSetup,hem=hem,limit=limit,rads=rads
  Screenplot,/cct
  if ~ISA(hem) then hem=1 else if ISA(hem,/str) then $
    if STRCMP(hem,'n',1,/fold) then hem=1 else if STRCMP(hem,'s',1,/fold) then hem=-1
  if ~isa(limit) then limit=[40*hem,-180,90*hem,180];bounds
  Map_set,90*hem,0,$;center of map
    /stereo,/horiz,/iso,$;no distortion
    E_HORIZON={FILL:1, COLOR:RGB2Col(.5,.5,1)},$
    LIMIT=limit
  map_continents,/fill,color=RGB2COL(.5,1,.5)
  PLOTS,[0,90*hem],psym=6
  plotlons
  plotlats
  @'RadDefV2.pro'
  if hem gt 0 then rads=rads[WHERE(rads.lat gt 0)]
  if hem lt 0 then rads=rads[WHERE(rads.lat lt 0)]
END

PRO globeRads,rads=rads
  nr=radByName(rads,['cly','inv','rkn'])

  XYOUTS,rads.lon,rads.lat,rads.name
  foreach rad,nr,i do PlotFOV,Rad2FoV(rad)
  PlotFov,Fov(-95.,74.7,-157.,[0:1000:50]),col=55
  xyouts,-95,74.7,'RISR'
  PlotFov,Fov(-95.,74.7,-167.,[0:1000:50]),col=85
  PlotFov,Fov(-95.,74.7,-177.,[0:1000:50]),col=115
  ;a=rads4coords([-95.,75.])
  ;PRINT,a
  ;ll=[0:360:6]
  ;la=[60:90]
  ;magmap
  ;nm=getmags(ll,la)
  ;v=-180/!PI*asin(nm[*,*,0])
  ;loadct,39,/silent
  ;contour,v,ll,la,/over,c_col=[165:255:10],lev=[62:89:3],/iso;,/fill
  ;print,byte(v)

  Mousein
END