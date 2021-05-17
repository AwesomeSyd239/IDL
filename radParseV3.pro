
;+
;takes in raw radar (lat lon name bor nbeam)
;basically copied from fastFov in globe.pro
;new in V3:
;   keep lat/lon in rad structure. also named structure to sydradar2
;-
FUNCTION RadInitV3,rads,beam=beam,rng=rng
  if ~ISA(rng) then rng=1000.
  rad={sydRadar2,name:'',beams:[0,15],gates:[0,50],col:0,time:0.,azm:0,lat:0,lon:0}
  radars=REPLICATE(rad,N_ELEMENTS(rads))
  radars.name=rads.name
  radars.azm=rads.bor
  for i=0,n_elements(rads)-1 do radars[i].beams=[0,rads[i].beam]
  ct=COS(rads.lon*!PI/180.) & st=SIN(rads.lon*!PI/180.)
  cp=COS(rads.lat*!PI/180.) & sp=SIN(rads.lat*!PI/180.)
  centerBeams=rads.beam/2.
  cb=COS((rads.bor)*!PI/180.)*rng
  sb=SIN((rads.bor)*!PI/180.)*rng
  R=6370.
  x2=(R*st*cp-cb*sp*st+sb*ct)
  y2=(R*ct*cp-cb*sp*ct-sb*st)
  z2=R*sp+cb*cp
 
  radars.lon=ATAN(x2,y2)*180./!PI
  radars.lat=ATAN(z2,SQRT(x2^2+y2^2))*180./!PI
  radars.time=radars.lon/15.
  RETURN,radars
END


;+
;Loads radars as defined in RadDefV2
;filters to just the selected
;runs them through radInitV3 to get a full radar structure
;-
Function RadParseV3,radarWant
  @'/home/ullrich/syd/RadDefV2.pro'
  inUse=INTARR(N_ELEMENTS(radarWant))
  if ~isa(radarwant,/string) then radarwant=radarwant.name
  for i=0,N_ELEMENTS(radarWant)-1 do begin
    check=WHERE(STRCMP(radarWant[i],rads.name,3,/FOLD_CASE),cnt)
    if cnt ne 0 then inUse[i]=check
  endfor
  if TOTAL(inUse eq -1) then MESSAGE,"What radars?"
  RETURN,RadInitV3(rads[inUse])
END

;+
;A function call to set radar beams
;can be chained from radparse like SetBeams(radparse('rkn'),[5,7])
;-
Function SetBeams,rad,beams
rad.beams=beams
RETURN,rad
END

;+
;Outputs radar data to the page
;noErase makes it not start a new page
;yo is how high up to start writing (0.9}
;-
PRO SpitRadars,radars,noErase=noErase,yo=yo
  common comPage,page
  if ~KEYWORD_SET(noErase) then ERASE
  if ~KEYWORD_SET(yo) then yo=0.9
  y=yo-page.fsiz*[0:N_ELEMENTS(radars)]*1.3
  XYOUTS,4*page.fsiz,y,['radar',STRING(FORMAT='%3s',radars.name)],/normal
  XYOUTS,10*page.fsiz,y,['beams',STRING(FORMAT='%02i-%02i',radars.beams)],/normal
  XYOUTS,17*page.fsiz,y,['gates',STRING(FORMAT='%02i-%02i',radars.gates)],/normal
END