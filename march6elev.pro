@'/home/ullrich/syd/sydPlotToolsV4.pro'
@'/home/ullrich/syd/getfilesV3.pro'
@'/home/ullrich/syd/sydElev.pro'
@'/home/ullrich/syd/sydReadRadarV4.pro'
@'/home/ullrich/syd/smartelevation.pro'
;+
;ele    using elevation angles
;-
PRO PHI0atFREQ,phi0,utIn,timeInds,ttl,other=other,stretch=stretch,ele=ele
  @'/home/ullrich/syd/RadDefV2.pro'
  if ISA(other) then rad=rads[WHERE(rads.stid eq other[0].stid)]
  st=FIX(utIn[5])
  ut=(utIn-st)*60
  scans=ut[timeInds[WHERE(ABS(ut[timeInds]-SHIFT(ut[timeInds],1)) gt 1.,nsc)]]
  if nsc gt 2 then scans=[scans,2*scans[-1]-scans[-2]] else begin
    scans=ut[WHERE(other.bmnum eq other[0].bmnum,nsc)] ;beams increase or decrease?
    scans=[scans,2*scans[-1]-scans[-2]]
  endelse
  ;PRINT,utin[0],utin[-1]
  p=makeplot(xd=scans,yr=[0,N_ELEMENTS(phi0[*,0])],xtin=2,ytin=4,$
    ytitle='Gate',xtitle=STRING(FORM='Scans started at X minutes after UT %i',st),$
    title=STRING(FORM='%s %s, %sMz',rad.name,ISA(ele)?'Elev':'Phi0',ttl),/font)
  pixelplot,TRANSPOSE(phi0[*,timeInds]),$
    x=(ISA(stretch)?0:ut[timeInds]),$
    clip=(ISA(ele)?[0,45]:[-3,3])
  colbar,p,[0:45:5],place=[1.01,.5],form='%i'
  for i=0,N_ELEMENTS(scans)-1 do OPLOT,[scans[i],scans[i]],[0,50]
  for y=0,N_ELEMENTS(phi0[*,0]),4 do OPLOT,[0,100],[y,y]
end


Function HokFil,scls
  RETURN,(scls.bmnum-SHIFT(scls.bmnum,2) ne 0);weird scan mode
end


;+
;
;CALL:
;March6Elev,rad='hok',month=3,day=6,gates=[0,49],times=[4.,7.5],ex_fil='HokFil',/page
;-
PRO March6Elev,rad=rad,month=month,day=day,plt=plt,$
  gates=gates,times=times,page=page,ex_fil=ex_fil
  if ~ISA(month) then month=3
  if ~ISA(day) then day=6
  if ~ISA(gates) then gates=[0,19]
  if ~ISA(times) then times=[16.,19.5]
  if ~ISA(plt) then plt='ele'
  
  if isa(rad) then if STRCMP(rad,'rkn',/fold) then begin
    tdif=3.e-6
    notry=1
    rkn=1
  endif else begin
    tdif=1
    notry=0
    rkn=0
  endelse
  if ~ISA(rad) then rad='rkn'

  f=GetFiles(0,rad,2016,month,day)
  pts=SydReadFit(file=f,scls=scls,dats=dats)
  ut=scls.time.hr+scls.time.mt/60.+scls.time.sc/3600.
  phi0=dats.phi0[gates[0]:gates[-1]]
  ;radar_site_read,prm=scls,scls[0].stid,site=site
  ;PRINT,site,/implied

  if KEYWORD_SET(page) then $
    page=pageDef(STRING(FORM='/home/ullrich/syd/plots/Elevation/%s%02i%02i',rad,month,day), $
    border=[.5,.5,.5,.5],divs=[1,4]) else $
    screenplot
  ;coldhotct

  ele=smartelevation(scls,phi0,tdif=tdif,notry=notry,/backlobe)
  ;PRINT,BYTE(scls.bmnum-SHIFT(scls.bmnum,1))
  yd=(STRCMP(plt,'phi')?phi0:ele)
  for tim=float(times[0]),float(times[-1]),.5 do begin
    lk=ut ge tim and ut lt tim+.5
    if ISA(ex_fil) then lk*=CALL_FUNCTION(ex_fil,scls)
    lk=WHERE(lk,cnt)
    if (tim mod 1) eq .5 then BlockLabel,STRING(FORMAT='UT %i-%i',tim+[-.5,.5]),xlabel=0,size=2.,keep=0,/now
    hf=scls[lk].tfreq gt 11000
    ;PRINT,tim,BYTE(scls[lk].bmnum)
    if TOTAL(~hf) then Phi0atfreq,yd[*,lk],ut[lk],WHERE(~hf),'10',other=scls[lk],stretch=rkn,/ele
    if TOTAL(hf) then Phi0atfreq,yd[*,lk],ut[lk],WHERE(hf),'12',other=scls[lk],stretch=rkn,/ele
  endfor
  PRINT,'done'
  if keyword_set(page) then device,/close
END