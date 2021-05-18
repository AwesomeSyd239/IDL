@'getfilesV3.pro'
@'sydReadRadarV4.pro'
@'sydElev.pro'
;e=reverse(normalize([0:10],s=sqrt(3.),m=sqrt(13.))^2)
Function NfromE,tilt,e=e,g=g,f=f,h2=h2
  if ~ISA(g) then g=[5:15]
  if ~ISA(e) then e=[13:3]
  if ~ISA(f) then f=12.e6
  if MAX(e) gt 1 then e*=!PI/180.
  if f lt 1e3 then f*=1.e6
  if tilt gt .5 then tilt*=!PI/180.
  d=(g+4)*45.
  Re=6370.
  H2=SQRT(Re^2+2*Re*d*SIN(e));+d^2)
  n2=COS(e-tilt)*Re/H2
  den=(1-n2^2)*f^2/80.5
  RETURN,den
END

Function GetTilt,elev,gates,freq
sclHt=7.
testgrad=[-20.:20.]*!PI/180.
ddy=(!CONST.EULER-1)/!CONST.EULER/sclHt*tan(testgrad)
;p=makeplot(xd=gates,yr=[-10,15])
bads=fltarr(n_elements(testgrad))
foreach grad,testgrad,gi do begin
  den=NfromE(grad,e=elev,g=gates,f=freq)
  testdNdR=(den[1:-1]+den[0:-2])/2.*ddy[gi]
  measdNdR=den[1:-1]-den[0:-2]
  bad=(testdNdR-measdNdR)/1.e10
  bads[gi]=total(sqrt(abs(bad)),/nan)
;  plots,[0:14],bad,psym=-1,col=gi*6
endforeach
best=min(bads,gi)
return,testgrad[gi]
END


Pro Nfromdat,dat,prm,elev
  screenplot,divs=[1,2]
  nd=N_ELEMENTS(dat)
  ng=N_ELEMENTS(elev[*,0])
  allDen=FLTARR([nd,ng])
  ut=prm.time.hr+prm.time.mt/60.+prm.time.sc/3600.
  p=makeplot(xd=ut,yr=[0,ng])
  pixelplot,TRANSPOSE(elev*(dat.phi0[0:39] ne 0)*180./!PI),leg=[1.2,1.5]
  tilts=fltarr(nd)
  hts=fltarr([nd,ng])
  for i=0,N_ELEMENTS(dat)-1 do begin
    bad=WHERE(dat[i].phi0 eq 0)
    e=elev[*,i]
    e[bad]=!VALUES.F_Nan
    last=where(e[10:*] gt !PI/9.)+9
    freq=prm[i].tfreq/1000.
    tilts[i]=GetTilt(e[0:last[0]],[0:last[0]],freq)
    ;PRINT,tilts[i]*180./!PI
    den=NfromE(tilts[i],e=e,g=[0:39],f=freq,h2=h2)
    allDen[i,*]=den
    ;plots,[0:39],h2-6370,psym=-1,col=i*6
    hts[i,*]=h2-6370
  endfor
  can=(allden[*,1:-1] ne 0) and (allden[*,0:-2] ne 0)
  allden[where(allDen eq 0)]=!VALUES.F_NAN
  dndg=smooth((allden[*,1:-1]-allden[*,0:-2])*can,[0,2],/nan)/45.
  sden=(allden[*,1:-1]+allden[*,0:-2])*can/2
  dndg2=sden*rebin(tan(-tilts),[nd,ng-1])/10.
  tilt2=atan(dndg/4.5/sden)
  ;pixelplot,dndg2/1.e10,clip=[-2,1]
  erase
  p=makeplot(xd=ut,yr=[0,20],loc=[0,0])
  pixelplot,alog10(hts[*,0:20]),clip=[1.5,2.5],/bgc
  colbar,p,[80:150:10],form='%i',place=[1.1,.5]
  p=makeplot(xd=ut,yr=[0,20])
  ;pixelplot,dndg/1.e10,clip=[-2,1],/leg
  ;message,'should be the same'
  pixelplot,allDen/1.e11,leg=[1.2,.5],clip=[0,3],/bgc
  axis,0,40,/xaxis,col=255,/xstyle
END

Pro WHoleRun
pts=SydReadFit(file=GetFiles(0,'rkn',2016,3,4),scls=scls,dats=dats)
ut=scls.time.hr+scls.time.mt/60.+scls.time.sc/3600.
good=where(ut ge 17 and ut le 19 and scls.bmnum ge 5 and scls.bmnum le 9)
elev=smartelevation(scls[good],dats[good].phi0[0:39],td=3.e-6,/notry)
nfromdat,dats[good],scls[good],elev*!PI/180.
end