Function GetPerp,lons,lats
  common rec
  abc=Pol2Car([Re,lons[0,0],lats[0,0]],/rad)
  def=Pol2Car([Re,lons[0,-400],lats[0,-400]],/rad)
  cp1=SHIFT(abc,2)*SHIFT(def,1)-SHIFT(abc,1)*SHIFT(def,2)
  RETURN,cp1/SQRT(TOTAL(cp1^2)) ;unit vector perpendicular to radar propogation plane
END

;+
;latitudes
;longitudes
;x       [num] ground distances
;xtp     [num] x to plot
;perp     vec  unit vector perpendicular to propagation plane
;hst      num  height step size
;con      bool continuous filed lines (vs latitude locked)
;labSize  num  how big to draw lat/lon labels (set to 0 to disable) {.5}
;labFreq  int  puts lat/lon labels every freq lines
;-
PRO DrawMagLoop,lats,lons,x,xtp,perp,hst=hst,con=con,$
  labSize=labSize,labFreq=labFreq
  common rec
  if ~ISA(perp) then perp=GetPerp(lons,lats)
  if ~ISA(hst) then hst=50.;step size
  if ~ISA(con) then con=1 ;continuous magnetic (vs latitude-locked)
  if ~isa(labFreq) then labFreq=3
  if ~isa(labSize) then labSize=.5
  nb=1000/hst
  hst*=signum(lats[0])
  foreach pt,[0.:MAX(x):100.],lineCount do begin ;100 km ground steps
    closest=MIN(ABS(x-pt),ind)
    ct=COS(x[ind]/Re) & st=SIN(x[ind]/Re)
    la=lats[ind] & lo=lons[ind]
    r=re & i=1
    py=REPLICATE(re,nb) & px=FLTARR(nb)
    while r lt re+500. do begin
      mag=IGRF_COMPUTE([r,!PI/2.-la,lo])
      nm=mag/SQRT(TOTAL(mag^2))
      mxyz=BSPCAR(!PI/2.-la,lo,nm)
      plen=SIN(ACOS(TOTAL(mxyz*perp)))*hst;mag field in plane
      vert=ASIN(nm[0])

      if con then begin
        pxyz=Pol2Car([r,lo,la],/radian)
        rtp=Car2Pol(pxyz-mxyz*hst,/radian)
        r=rtp[0] & lo=rtp[1] & la=rtp[2]
        px[i:*]+=COS(vert)*plen
        py[i:*]-=nm[0]*plen
      endif else begin
        r+=hst
        vecx=[0,COS(vert)*plen] & vecy=r+[0,nm[0]*plen]
        OPLOT,vecx*ct+vecy*st,-vecx*st+vecy*ct,col=80,thick=2
      endelse
      i+=1
      if i gt nb then begin
        print,"RTDRAWMAG ERROR: ran out of iterations"
        break
      endif
    endwhile
    mxp=xtp[ind]+px/re
    if con then OPLOT,py,mxp,/polar,col=80,thick=2
    if ~(lineCount mod labFreq) then XYOUTS,Re*COS(mxp[0]),Re*SIN(mxp[0])-30.,align=.5,charsize=labSize,$
      STRING(form='la:%5.1f!Clo:%5.1f',180./!PI*[lats[ind],lons[ind]])
  endforeach
END