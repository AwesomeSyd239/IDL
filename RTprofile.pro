;for 12mhz use e0=[2.:10.:.1]
;for 10mhz use e0=[8.:14.:.1]
PRO KoustovProfile,htest,dens,prof=prof,lowest=lowest,$
  dens_max_E=dens_max_E,dens_max_F=dens_max_F, $
  h_max_E=h_max_E,h_max_F=h_max_F,$
  width_E=width_E,width_F=width_F
  if ~ISA(lowest) then lowest=80.
  htest=lowest+5.*FINDGEN(80)
  if ~ISA(prof) then prof=0
  case prof of
    1: begin
      dens_max_F=1.45
      h_max_F=280.
      width_F=90.
      dens_max_E=0.45
      h_max_E=110.
      width_E=50.
      ee=(htest-h_max_E)/width_E
      dens_E=dens_max_E*EXP(-ee^2)
    end
    2: begin
      dens_max_F=1.45
      h_max_F=275.
      width_F=80.
      dens_max_E=0.5
      h_max_E=85.
      width_E=30.
    end
    else: a=1
  endcase
  ; Chapman profile
  ee=(htest-h_max_E)/width_E
  if ~ISA(dens_E) then dens_E=dens_max_E*EXP(0.5*(1.-ee-EXP(-ee)))
  ff=(htest-h_max_F)/width_F
  dens_F=dens_max_F*EXP(0.5*(1.-ff-EXP(-ff)))
  dens=dens_F+dens_E
END

;PRO startgradtest
;if N_ELEMENTS(d3d) ne 1 then h=hi else begin
;  if d3d eq 2 then begin
;    KoustovProfile,h,dRKN,lowest=hi,$
;      dens_max_F=2.,h_max_F=280.,width_F=90.,$
;      dens_max_E=0.45,h_max_E=90.,width_E=40.
;    ;      KoustovProfile,h,dRISR,lowest=hi,$
;    ;        dens_max_F=2.,h_max_F=290.,width_F=80.,$
;    ;        dens_max_E=0.45,h_max_E=100.,width_E=40.
;    KoustovProfile,h,dRISR,lowest=hi,prof=2
;    d3d=FLTARR([100,80])
;    for i=0,79 do d3d[*,i]=interpol([drkn[i],drisr[i]],[0,99],[0:99])
;  endif else begin
;    KoustovProfile,h,d,prof=d3d
;    d3d=TRANSPOSE(REBIN(d,[80,100]))
;    ext=50.
;    nhts=(h*(ext+1)-MAX(h))/ext
;    ed=interpol(d,h,nhts)
;    for i=0,99 do begin
;      simHt=(-30.+i/2.)<0
;      d3d[i,*]=INTERPOL(d,h+simHt,h)
;    endfor
;  endelse
;  d3d=SMOOTH(d3d,[2,2])
;endelse
;end

Function IRI
  common RTParams
  common RTExtras
  dats=FLTARR([3,68])
  den=FLTARR([5,68])
  var=''
  for i=0,4 do begin
    OPENR,lun,STRING(form='data/IRI/UT%i_%i',ut,i*400),/get_lun
    READF,lun,var
    READF,lun,dats
    CLOSE,lun,/all
    den[i,*]=dats[1,*]
  endfor

  den2=FLTARR([101,80])
  for i=13,79 do den2[*,i]=interpol(den[*,i-13],[0:4]*400,xgrid)
  RETURN,den2
END


PRO readMar6,beam,alt,den,dne,time,elv,azm
  OPENR,f,'data/risr_c_2016_03_06_19_20_UT_data.txt',/get_lun
  form=''
  READF,f,form
  vars=FLTARR([12,3553])
  READF, f, vars
  FREE_LUN,f
  beam=BYTE(vars[0,*]) &  alt=FIX(vars[1,*])
  den=vars[4,*] & dne=vars[7,*] & time=vars[9,*] & elv=vars[10,*] & azm=vars[11,*]
END

PRO readOct13,beam,alt,den,dne,time,elv,azm
  OPENR,f,'data/risr_c_2016_10_13_0_24_UT_data.txt',/get_lun
  form=''
  READF,f,form
  vars=FLTARR([8,1298460])
  READF, f, vars
  FREE_LUN,f
  beam=BYTE(vars[0,*])
  ;want=where(beam eq 3 or beam eq 5 or beam eq 11)
  alt=FIX(vars[1,*])
  den=vars[2,*] & dne=vars[3,*]
  time=vars[5,*] & elv=vars[6,*] & azm=vars[7,*]
END

;+
;Event  str   the name of the function call that gets event data, for example 'readMar6'
;
;-
Function FilterEvent,event,times,beams,keepTime=keepTime
  CALL_PROCEDURE,event,beam,alt,den,dne,time,elv,azm
  t=WHERE(time lt times[-1] and time gt times[0])
  TrimMulti,t,beam,alt,den,dne,time,elv,azm
  dats=[]
  foreach bn,beams,bc do begin
    isbeam=WHERE(beam eq bn)
    tt=time[isbeam] & aa=alt[isbeam] & dd=den[isbeam]
    ee=elv[isbeam[0]] & zz=azm[isbeam[0]]
    ua=unique(aa)
    if ISA(keepTime) then begin
      ut=unique(tt)
      denOut=FLTARR(N_ELEMENTS(ut),N_ELEMENTS(ua))
      foreach tm,ut,ti do foreach ht,ua,hi do $
        denout[ti,hi]=TOTAL(dd[WHERE(aa eq ht and tt eq tm,cnt)])/cnt
    endif else begin
      denout=FLTARR(N_ELEMENTS(ua))
      foreach ht,ua,hi do denout[hi]=TOTAL(dd[WHERE(aa eq ht,cnt)])/cnt
    endelse
    if ISA(keeptime) then $
      dats=[dats,{beam:bn,azm:zz,ele:ee,hts:PTR_NEW(REFORM(ua)),tms:PTR_NEW(REFORM(ut)),den:PTR_NEW(denout)}] else $
      dats=[dats,{beam:bn,azm:zz,ele:ee,hts:PTR_NEW(REFORM(ua)),den:PTR_NEW(denout)}]
  endforeach
  RETURN,dats
END

Function FlattenEvent,dats,times
  d2=dats
  for i=0,N_ELEMENTS(dats)-1 do begin
    goodtime=WHERE(*dats[i].tms gt times[0] and *dats[i].tms lt times[1])
    td=*dats[i].den
    *d2[i].den=mean(td[goodtime,*],dim=1,/nan)
  endfor
  RETURN,d2
end

PRO plotEvent
  t=HISTOGRAM(time,min=19,max=20,binsize=1./12.,reverse=r)
  page=pagedef('plots/risrProfile.ps',perdiv=[3,4])
  for ti=0,12 do if t[ti] ne 0 then begin
    now=r[r[ti]:r[ti+1]-1]
    b=HISTOGRAM(beam[now],min=1,max=11,reverse=rb)
    for bi=0,10 do if b[bi] ne 0 then begin
      this=rb[rb[bi]:rb[bi+1]-1]
      tden=den[now[this]]
      talt=alt[now[this]]
      dden=dne[alt[this]]
      if ti eq tir and bi eq bir-1 then begin
        ralt=talt
        rden=tden
      endif
      p=makeplot(xd=[100,400],yr=[0,50],title=bi+1)
      OPLOT,talt,tden
      OPLOTERR,talt,tden,dden
    endif
    blocklabel,time[now[0]],xlabel=0
    AutoClear,/force
  endif
  DEVICE,/close
END

;+
;build oct13 event
;dats=filterevent('readoct13',[0,24],[3,5,11],/keeptime)
;p=makeplot(xr=[14,20],yr=[100,500],xtint=1)
;pixelplot,fixbounds(*dats[i].den,*dats[i].tms,*dats[i].hts),clip=[0,99]
;dat2=flattenevent(dats,[18.5,19])
;plot,*dat2[0].den,*dat2[0].hts
;event2profile,dat2,fullarr,/plt
;gradtest,fullarr/10.,[0:395:5],[0:1980:20],/smth,/noise,/pg
;-
PRO Event2Profile,dats,fullArr,hts=hts,low=low,plt=plt
  if ~ISA(dats) then dats=filterEvent([19,19.2],[3,5,11])
  if ~ISA(low) then low=50 ;km of 0 Ne
  if ~ISA(hts) then hts=[0:395:5]
  if ISA(plt) then begin
    screenplot
    p=makeplot(xr=[0,1980],yd=hts,ytint=50)
  endif
  x0=(74.4-62.8)*111 ;risr at about 1000
  nb=N_ELEMENTS(dats) & nh=N_ELEMENTS(hts)
  db=FLTARR(nb,nh) & gd=FLTARR(nb,nh)
  fullArr=FLTARR([100,nh])
  dst=[0:1980:20]
  for i=0,nb-1 do begin
    db[i,*]=interpol(*(dats[i].den),*(dats[i].hts),hts)>0
    if ISA(plt) then OPLOT,db[i,*]*10,hts,col=i*100
    gd[i,*]=hts/TAN(!PI/180.*dats[i].ele)*COS((177.1-dats[i].azm)*!PI/180.)
  endfor
  gd=x0-1.5*gd

  xarr=fullarr
  foreach ht,hts,hi do if ht gt low then begin
    den=[db[0,hi],db[*,hi],db[nb-1,hi]]
    rng=[dst[0],gd[*,hi],dst[-1]]
    xarr[*,hi]=interpol(den,rng,dst)<90
  endif
  xarr>=0
  if ISA(plt) then pixelplot,xarr,/leg,cliplev=[0,45]

  yarr=fullarr
  li=VALUE_LOCATE(hts,low)
  xind=VALUE_LOCATE(dst,gd)
  foreach ds,dst,di do begin
    i2=WHERE(xind eq di,/null)
    dens=[xarr[di,nh-1],xarr[di,nh/1.5],xarr[di,nh/3],db[i2],0]
    if ISA(i2) then i2/=nb
    alts=[nh-1,nh/1.5,nh/3,i2,li]
    ord=SORT(alts)
    yarr[di,li:*]=interpol(dens[ord],alts[ord],[li:nh-1])
  endforeach
  yarr>=0
  if ISA(plt) then pixelplot,yarr,cliplev=[0,45],leg=[1.2,1]

  fullarr=(xarr+yarr)/2.
  hi=[0:nh-1]
  for rep=0,3 do begin
    for i=0,nb-1 do fullarr[xind[i,*],hi]=db[i,*]
    fullarr=SMOOTH(fullarr,[4,4],/nan,/edge_truncate)
    if ISA(plt) then pixelplot,fullarr,leg=[1.2,1]
    fullarr[*,0:li]=0
  end
  fullarr[0:3,*]=SMOOTH(fullarr[0:3,*],[2,4])
  fullarr[*,0:3]=SMOOTH(fullarr[*,0:3],[4,2])
  fullarr[-4:-1,*]=SMOOTH(fullarr[-4:-1,*],[2,4])
  fullarr[*,-4:-1]=SMOOTH(fullarr[*,-4:-1],[4,2])
  if ISA(plt) then begin
    pixelplot,ALOG10(fullarr+.1),/leg,cliplev=[-1,2]
    md=MAX(ALOG10(.1+fullarr),min=sd)
    levs=10^[sd:md:(md-sd)/12.]
    ColBar,place=[1,.5],levs

    for i=0,nb-1 do OPLOT,gd[i,*],hts,col=255,thick=2
    for i=0,10 do PLOTS,x0,0,psym=4,col=254,symsize=i/10.
  endif
END


;noise flt   noise scaling factor
;radn  str   radar name ('rkn')The main parameters ar
;Vertical offset (moves the density profile up or down)
;Density scale, which multiplies ECHAIM
;Density falloff, which makes density decay exponentially at low altitudes instead of instantly becoming 0 at 60km.
;Density noise, the standard deviation of random density fluctuations
;Gradient noise, the standard deviation (in degrees) of random tilts.

PRO ManyGrad,L,M,S,dnv,gnv
  if ~isa(dnv) then dnv=[0,.03,.06]
  if ~isa(gnv) then gnv=[0.,2.,4.]
  if ~isa(M) then M=1.1
  if ~isa(S) then S=1.3
  foreach d,dnv do foreach g,gnv do begin
    definertparams,dn=d,gn=g,xg=[0:1500:20],yg=[0:395:5]
    foreach lift,L do foreach mult,m do begin
      echaimprofile,lift=lift,mult=mult
      extendchaimdown,scl=s
      LR=((lift lt 0)?STRING(FORM='L%03i',lift):STRING(FORM='L%02i',lift))
      gradtest,/pg,ext=STRING(FORM='19.24_d%02i_g%02i_%s_M%3.1f_S%3.1f',d*100,g*10,LR,mult,S)
    endforeach
  endforeach
END

;get the echaim density, lats and lons, basically setup for a raytrace
;then call this guy to add in the risr desnity
;the risr input to this function comes from bion's lib_risr_io
PRO fixden,risr,lats,tm,ts,smth=smth,spitout=spitout,plt=plt
  echaimprofile,lats=lats,lons=lons;by turning lats/lons into holders, we save the calc
  extendchaimdown;only operates on globals
  if ~isa(smth) then smth=2
  common rtparams
  a=where(risr.lon gt -95 and abs(risr.time-tm) lt ts)
  ris=risr[a]
  nl=n_elements(lats)
  dl=lats[1:*]-lats[0:-2]
  na=n_elements(ygrid)
  da=ygrid[1:*]-ygrid[0:-2]
  den2=den*!Values.f_nan
  den0=den
  smhi=where(den[0,*] ge 0.1)
  for i=0,n_elements(lats)-1 do begin
    if lats[i] le 68 then continue
    if lats[i] gt 75 then break
    b=where(ris.lat gt lats[i] and ris.lat le lats[i]+dl[i<(nl-2)],cnt)
    risb=ris[b]
    minalt=min(risb.alt)
    ;if cnt eq 0 then print,'no risr at this lat',lats[i] else print,'risr at lat',lats[i]
    if cnt eq 0 then continue
    sr=sort(risb.alt)
    if keyword_set(spitout) then print,[[risb[sr[0:*:10]].alt],[risb[sr[0:*:10]].den]]
    for j=0,n_elements(ygrid)-1 do begin
      if ygrid[j] lt minalt then continue
      c=where(risb.alt gt ygrid[j] and risb.alt le ygrid[j]+da[j<(na-2)],cnt)
      if cnt eq 0 then continue
      nd=total(risb[c].den,/nan)/cnt
      ;if cnt ne 0 then print,'risr at alt',nd,ygrid[j],i,j
      den2[i,j]=nd/10. ;because bion uses 10^4/cm^3
    endfor
  endfor
  touse=where(finite(den2))
  den3=smooth(den2,[10,10],/nan,/edge_mirror)
  if keyword_set(plt) then begin
    screenplot,divs=[2,3]
    ;ColdHotct
    p=makeplot(xd=xgrid,yd=ygrid)
    den[touse]=den2[touse]
    pixelplot,den,/leg,clip=[0,3];clip=[-.3,.3]
    pixelplot,den3,clip=[0,3]
  endif
  use3=where(finite(den3))
  for i=0,smth do begin
    den[use3]=(den[use3]+den3[use3])/2.
    ;    del=den2[touse]-den[touse]
    ;    den[touse]+=del*9;den2[touse]
    den[*,smhi]=smooth(den[*,smhi],[smth,smth],/nan,/edge_truncate)
    if isa(plt) then if ((i mod plt) eq 0) then begin
      p=makeplot(xd=xgrid,yd=ygrid)
      pixelplot,den,clip=[0,3];-den0,clip=[-.2,.2]
    endif
  endfor
end

;then call this guy to add in the risr desnity
;the risr input to this function comes from bion's lib_risr_io
;pk controls steepness of falloff.
;     pk of 0 falls off as 1/x, and can massively overweight points close to grid spacing
@MapTools
PRO risrAndEchaimProfileByDistance,risr,lats,lons,plt=plt,$
horizFall=horizFall,vertFall=vertFall,ts=ts,pk=pk,powFall=powFall
  if ~isa(powFall) then powFall=2
  if ~isa(horizFall) then horizFall=100
  if ~isa(vertFall) then vertFall=15
  if ~isa(ts) then ts=0.25 ;+/- 15 minutes
  if ~isa(pk) then pk=1
  common rtparams
  common rtextras
  echaimprofile,lats=lats,lons=lons
  common Rec
  daytime=24*date2doy([year,month,day])+time
  ristime=24*date2doy(yr=risr[0].yr,mo=risr.mo,dy=risr.dy)+risr.time
  a=where(abs(ristime-daytime) lt ts and finite(risr.den),cnt)
  print,STRING(FORM='got %i risr points',cnt)
  if cnt eq 0 then return
  ris=risr[a]
  nl=n_elements(xgrid) ; equal to n_(lats
  na=n_elements(ygrid)
  weightArr=fltarr(size(den,/dim),/nozero)
  den2=weightArr
  dtr=!PI/180.
  lar=lats*dtr
  lor=lons*dtr
  rila=ris.lat*dtr
  rilo=ris.lon*dtr
  ;calc yd in bits here to get math operations out of sub-loop, saves 8s of runtime
  raf=ris.alt/vertfall
  ygf=ygrid/vertfall+pk
  for i=0,nL-1 do begin
    dla=sin((lar[i]-rila)/2.)^2;multiline makes no speed difference
    dlo=sin((lor[i]-rilo)/2.)^2
    arc=2*asin(sqrt(dla+cos(lar[i])*cos(rila)*dlo))
    ;calculating xd without ygrid[j] leads to 3% error and -8s of runtime
    xd=(((Re+(200+ris.alt)/2.)*arc)/horizFall+pk)^2.
    for j=0,nA-1 do begin
      ;xd=(Re+(ygrid[j]+ris.alt)/2)*arc
      ;yd=(ygrid[j]-ris.alt)
      weights=(xd+(ygf[j]-raf)^2.)^(-powFall/2.)
        ;(xd/horizFall+pk)^2+$
        ;((ygrid[j]-ris.alt)/vertFall+pk)^2)
      weightArr[i,j]=total(weights)
      den2[i,j]=total(weights*ris.den)
    endfor
  endfor
  avgweight=mean(weightArr,/nan)
  den2/=(10.*avgweight);bion uses 10x for density
 weightArr/=avgweight
  den3=den2+den*1
  den3/=(weightArr+1);weight echaim as the average of risr
  ExtendChaimDown,top=90,denIn=den3;,sclH=15
  if keyword_set(plt) then begin
    ;common compage
    ;if ~isa(page) then
     screenplot,divs=[1,4]
    p=makeplot(xd=xgrid,yd=ygrid,title="RISR Sampling Weight")
    pixelplot,weightArr,lev=[.167,.25,.4,.8,1.25,2.5,4,6],/nonorm,/leg
    p=makeplot(xd=xgrid,yd=ygrid,title="RISR Profile")
    pixelplot,den2/weightArr,clip=[0,3]
    xyouts,.5,.95,STRING(FORM="UT = %02i",time),ALIGN=.5,/NORMAL
    p=makeplot(xd=xgrid,yd=ygrid,title="ECHAIM")
    pixelplot,den,/leg,clip=[0,3]
    p=makeplot(xd=xgrid,yd=ygrid,title="RISR and ECHAIM")
    pixelplot,den3,clip=[0,3]
  endif
  den=den3
END
;PROFILER,/reset
;PROFILER
;PROFILER,/system
;risrandechaimprofilebydistance,risr,lats,lons
;PROFILER,/report,filename='/home/ullrich/syd/profProf'
;print,'done'