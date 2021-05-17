@lib_risr_io
@RTprofile
Function StringUpParams,time,$
  horizFall=horizFall,vertFall=vertFall,ts=ts,pk=pk,powFall=powFall
  stringTime=STRING(FORM=(time eq fix(time))?'%02i':'%04.1f',time)
  return,STRING(FORM='+chaim_risr_falloff_%s_h%i_v%i,p%i',$
    stringtime,horizFall,vertFall,pk)
END


;run this in interpreter to have access to variables
;common RTParams,xgrid,ygrid,den,grad,inter,denNoise,gradNoise
;common RTExtras,year,month,day,time,strDate,rad,beam
;defineRTExtras,yr=2019,mo=6,dy=2,ut=19,ra='rkn',bm=5
;defineRTPARAMS,xg=[0:3000:30]
;risr=load_risr_madrigal_file('/home/ullrich/syd//data/ras190531.002.hdf5',radar_loc=radar_loc,tree=tree,table=table,max_den=max_den,exp_num=exp_num,beams=risrbeams)
Pro HourlyTrace,ti,tf,ts,risr=risr,_ref_extra=profPar
  common RTParams,xgrid,ygrid,den,grad,inter,denNoise,gradNoise
  common RTExtras,year,month,day,time,strDate,rad,beam
  if ~isa(ti) then ti=1.
  if ~isa(tf) then tf=23.
  if ~isa(ts) then ts=2.
  if ~isa(risr) then risr=load_risr_madrigal_file($
    '/home/ullrich/syd//data/ras190531.002.hdf5',$
    radar_loc=radar_loc,tree=tree,table=table,max_den=max_den,exp_num=exp_num,beams=risrbeams)

  ;screenplot,div=[1,2]
  ;leg=[1.2,.7]
  ;xtitle='Ground Distance (km)' & ytitle='Altitude (km)'
  for time=float(ti),float(tf),float(ts) do begin
    print,time
    ;echaimprofile,lats=lats,lons=lons
    ;fixden,risr,lats,time,.05,smth=8
    ;extendchaimdown
    risrandechaimprofilebydistance,risr,lats,lons,$; _extra=profPar,$
      horizfall=horizfall,vertfall=vertfall,pk=pk
    print,'done fix'
    ;exStr=StringUpParams(_extra=profPar)
    stringTime=STRING(FORM=(time eq fix(time))?'%02i':'%04.1f',time)
    exStr=STRING(FORM='+chaim_risr_falloff_%s_h%i_v%i_p%i',$
      stringtime,horizFall,vertFall,pk)
    GradTest,ext=exStr,/pg
    print,'done plot'
  endfor
end

Pro SeeRisrMap,risr=risr,risbeams=risbeams
  globeSetup,hem='n',limit=[63,-105,75,-75],rads=rads
  rkn=radbyname(rads,'rkn')
  rkf=Rad2Fov(rkn)
  for i=0,15,2 do plots,rkf[[0,i,i+1,0],0],rkf[[0,i,i+1,0],1]
  if ~isa(risr) then risr=load_risr_madrigal_file('/home/ullrich/syd//data/ras190531.002.hdf5',radar_loc=radar_loc,tree=tree,table=table,max_den=max_den,exp_num=exp_num,beams=risbeams)
  radar_loc=[-94.9,74.7]
  foreach bm, risbeams,bi do begin
    a=where(risr.beam eq bm.beamid)
    u=uniq(risr[a].range,sort(risr[a].range))
    pts=risr[a[u]]
    PlotFov,Fov(radar_loc,bm.azm,pts.range*cos(bm.elv*!PI/180.))
    plots,pts.lon,pts.lat,psym=4,symsize=.3,col=22
  endforeach
END