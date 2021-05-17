@'/home/ullrich/syd/sydTools.pro'
;+
;Functions in this file
;PageDef
;MakeCT
;RGB2Col
;ParamDef
;PlotDataDef
;GetLoc
;GetPos
;Loc2to4
;AutoClear
;FixLoc
;MakePlot
;BlockLabel
;FixContour
;MakeBox
;MakeLegend
;LevCol
;FixLength
;PlotMany
;
;New in V4
;Replaced makeHeaders system with blockLabel system
;  instead of header space, page now has borders
;  Page Structure conflicts with V3
;PixelPlot works how I like now
;Added ScreenPlot, allowing for realtime graphs
;made page into common block throughout calls
;added colbar function to replace weirdness of makelegend for both
;blocklabel uses pagewait
;-




;+
;CALL:  PageDef,file,psize=psize,divs=divs,perDiv=perDiv,space=space,fsize=fsize
;file   str   File name to save as
;psize  [x,y] Size of page {7.5,10}
;divs   [x,y] Number of areas to split the page into
;perDiv [x,y] How many sub-areas per area
;space  [num] Spacing around the page and between graphs {.75,.75,0,0} (last two are sub x&y)
;       if given [x,y], sets main div spacing
;       if given [-x,-y], sets subdiv spacing
;fsize  #     font size (72pt=1inch) {12} 
;order  [int] Natural order of graphs on page, {3,2,1,0 plots ysub first, then xsub, then y, then x)
;border [num] Margins of the page left,top,right,bottom {[space[0],space[1],space[0]/1.2,space[1]]}
;cct    bool  Custom Color Table?
;-
Function PageDef,file,psize=psize,divs=divs,perDiv=perDiv,$
  space=space,fsize=fsize,order=order,border=border,cct=cct,thick=thick
  common comPage,page,pageLabel
  if ~KEYWORD_SET(psize) then psize=[7.5,10]
  if ~KEYWORD_SET(fsize) then fsize=12
  if ~KEYWORD_SET(space) then space=[fsize/16.,fsize/16.,0.,0.] ;4.5 glyph heights
  if N_ELEMENTS(space) eq 2 then begin
    if space[0] ge 0 then space=[space,0,0] else space=[fsize/14.,fsize/14.,-space]
  endif
  if ~KEYWORD_SET(divs) then divs=[1,1]
  if ~KEYWORD_SET(perDiv) then perDiv=[1,1]
  if ~KEYWORD_SET(order) then order=[2,3,0,1]
  if ~KEYWORD_SET(border) $
    then border=FLOAT([space[0],space[1],space[0]/1.2,space[1]]) $
  else if N_ELEMENTS(border) eq 1 then border=[1.,1.,1.,1.]
  if ~ISA(thick) then thick=3

  border[0]>=.5*fsize/12.
  if ~ISA(cct) then loadct,39,/silent else begin
    if ISA(cct,/int) then if cct eq 1 then MakeCT else loadct,cct
    if ISA(cct,/str) then CALL_PROCEDURE,cct
  endelse
  SET_PLOT,'ps' ;plot is postscript
  DEVICE,/portrait,$
    xsize=psize[0],xoffset=border[0],$
    ysize=psize[1],yoffset=border[1],$
    font_size=fsize,/inches,/color,$
    FILENAME=(STRMID(file,2,3,/reverse) ne '.ps')?file+'.ps':file ;ensure that file is valid postscript

  ;Get the font size in normalized units
  XYOUTS,.5,.5,'W',WIDTH=w,CLIP=[0.,0.,0.1,0.1],COLOR=244,NOCLIP=0,FONT=-1,/NORMAL
  !P.FONT=0
  page={page,$
    xsiz:psize[0],$
    ysiz:psize[1],$
    xdiv:divs[0]>1,$
    ydiv:divs[1]>1,$
    xsub:perDiv[0]>1,$
    ysub:perDiv[1]>1,$
    spc:float(space),$
    fsiz:w,$
    border:FLOAT(border),$
    cur:-1,$
    ord:order,$
    thick:FLOAT(thick)}
  pageLabel=[]
  RETURN,page
END


PRO ClosePage
  common ComPage,page,pagelabel
  page=!null
  pagelabel=!null
  print,'closing page and deleting from common block'
  if ~strcmp(!D.name,"X",/fold) then device,/close
END


;+
;Makes a screen plot
;CALL:   ScreenPlot,cct=cct,fsize=fsize
;INPUTS:
;cct     bool uses my colortable, or defaults to rainbow
;fsize   num  sets font size {14}
;-
PRO ScreenPlot,cct=cct,fsize=fsize,divs=divs,order=order
  common comPage,page,pageLabel
  if ~KEYWORD_SET(fsize) then fsize=14
  if ~KEYWORD_SET(divs) then divs=[1,1]
  if ~KEYWORD_SET(order) then order=[2,3,0,1]
  SET_PLOT,'X' ;screen
  DEVICE,DECOMPOSED=0,SET_CHARACTER_SIZE=fsize*[1,5./3],SET_FONT='Helvetica', /TT_FONT
  if ~ISA(cct) then loadct,39,/silent else if cct eq 1 then MakeCT
  !p.background = 255b & !p.color=0b;'ffffff'x & !p.color=0
  XYOUTS,.5,.5,'W',WIDTH=w,CLIP=[0.,0.,0.1,0.1],COLOR=244,NOCLIP=0,/NORMAL
  !P.FONT=0
  page={page,$
    xsiz:!d.X_SIZE,$
    ysiz:!d.Y_SIZE,$
    xdiv:divs[0]>1,$
    ydiv:divs[1]>1,$
    xsub:1,ysub:1,$
    spc:[.75*fsize/14.,.75*fsize/14.,0.,0.]*!d.x_size/11.,$
    fsiz:w,$
    border:[80.,60.,80.,80.],$
    cur:-1,$
    ord:order,$
    thick:1.}
  ERASE
  pageLabel=[]
END

PRO Clear
  TVLCT,r,g,b,/get
  v=SQRT(MAX(r^2.+g^2.+b^2.,ind))
  POLYFILL,[0,0,1,1],[0,1,1,0],color=ind,/normal
END

;+
;Works much the same as !P.MULTI
;CALL:   PageRediv,divs,subs=subs,cur=cur,spc=spc
;INPUTS:
;divs
;subs
;cur
;spc
;bor
;-
Pro PageRediv,divs,subs=subs,cur=cur,spc=spc,bor=bor
  common comPage
  if ISA(divs) then begin
    page.xdiv=divs[0]
    page.ydiv=divs[1]
  endif
  if KEYWORD_SET(subs) then begin
    page.xsub=subs[0]
    page.ysub=subs[1]
  endif
  if KEYWORD_SET(spc) then page.spc=spc
  if KEYWORD_SET(bor) then page.border=bor
  if ISA(cur) then page.cur=cur-1
  if ~KEYWORD_SET(cur) then Autoclear,/force
END

;+
;Changes used page size (but not actual page)
;CALL:   PageResiz,siz
;INPUTS:
;siz    num  new size in inches
;-
Pro PageResiz,siz,cur=cur
  common comPage
  if ISA(siz) then begin
    page.xsiz=siz[0]
    page.ysiz=siz[1]
  endif
  if KEYWORD_SET(cur) then page.cur=cur else begin
    Autoclear,/force
    page.cur-=1
  endelse
END


;+
;Makes an RGB colortable
;Get colors from it with RGB2Col
;-
PRO MakeCT
  opts=[6,7,6]
  len=PRODUCT(opts)
  cols=BYTARR([3,len])
  rs=opts[1]*opts[2] & gs=opts[2] &  bs=1
  for r=0,0+len-rs,rs do begin
    cols[0,r:r+rs-1]=FIX(255.*(r-0)/rs/(opts[0]-1))
    for g=r,r+rs-gs,gs do begin
      cols[1,g:g+gs-1]=FIX(255.*(g-r)/gs/(opts[1]-1))
      for b=g,g+gs-bs do begin
        cols[2,b:b+bs-1]=FIX(255.*(b-g)/bs/(opts[2]-1))
      endfor
    endfor
  endfor
  TVLCT,TRANSPOSE(cols)
END

PRO ColdHotCT,mag=mag,grn=grn
  mag=255*KEYWORD_SET(mag)
  grn=255*KEYWORD_SET(grn)
  r = Interpol([  0,  0,  0,  0,255,255,220,140,255], [0,1,25,75,127,180,230,254,255], FINDGEN(256))
  g = Interpol([grn,  0,  0,255,255,255,  0,  0,mag], [0,1,25,90,127,165,230,254,255], FINDGEN(256))
  b = Interpol([  0,120,255,255,255,  0,  0,  0,255], [0,1,25,75,127,180,230,254,255], FINDGEN(256))
  TVLCT, r, g, b
  !p.background=127
END

PRO PaleCT
  r = Interpol([0,  0, 40, 20,  0,100,255,255,255,180,255], [0,1,31,64,95,127,160,192,221,254,255], FINDGEN(256))
  g = Interpol([0,  0, 40,160,255,255,255,165, 80,  0,255], [0,1,31,64,95,127,160,192,221,254,255], FINDGEN(256))
  b = Interpol([0,180,255,255,255,120,  0,  0, 60,  0,255], [0,1,31,64,95,127,160,192,221,254,255], FINDGEN(256))
  TVLCT, r, g, b
END

PRO ColorBlindCT
  r=Interpol([  0,  0,  0,  0,147,183,219,255],[0:7*38:38],FINDGEN(256))
  g=Interpol([255,196,138, 78, 37,110, 83,255],[0:7*38:38],FINDGEN(256))
  b=Interpol([255,255,255,255,219,146, 73,  0],[0:7*38:38],FINDGEN(256))
  r[0]=0 & g[0]=0 & b[0]=0 & r[-1]=255 & g[-1]=255 & b[-1]=255
  TVLCT, r, g, b
END

PRO TestCT,ct
  page=PageDef('/home/ullrich/syd/plots/ColorTest.ps',psize=[7.5,10],cct=~ISA(ct),fsize=16)
  if ISA(ct) then LoadCT,ct
  mi=255
  for i=0.,mi do begin
    y=i/mi
    POLYFILL,[0,1,1,0],[y,y,y+1./mi,y+1./mi],COLOR=i,/NORMAL
    if (i mod 16) eq 0 then POLYFILL,[0,1,1,0],[y,y,y+.5/mi,y+.5/mi],COLOR=0,/NORMAL
  endfor
  for i=0.,mi,16. do XYOUTS,.1,i/mi,STRING(FORMAT='%i',i),/NORMAL
  DEVICE,/close
END


;+
;Enter some rgb, get closest thing in color table
;-
FUNCTION RGB2Col,Rin,Gin,Bin
  if N_PARAMS() eq 3 then ci=[[Rin],[Gin],[Bin]] else ci=Rin ;If not given three arguments assume the input was an array
  if MAX(ci) le 1 then c=ci*251 else c=ci<252 ;Normalize as needed
  if N_ELEMENTS(c[*,0]) ne 3 then c=TRANSPOSE(c) ;Maybe also turn the array, if needed
  col=FIX(c[0,*]/42)*42 ;Get the red
  col+=FIX(c[1,*]/36)*6 ;Get the green
  col+=FIX(c[2,*])/42 ;Get the blue
  col=REFORM(col)
  if N_ELEMENTS(col) eq 1 then RETURN,col[0] else RETURN,col
END


;+
;Enter some rgb, get closest thing in color table
;-
FUNCTION Col2RGB,col
  r=col/42
  b=col mod 6
  g=(col-r*42-b)/6.
  RETURN,REFORM([r/5.,g/6.,b/5.],[N_ELEMENTS(col),3])
END


;+
;Enter some rgb, get closest thing in color table
;was used to create colortables like:
;  colTBL=LONARR(256)
;  for i=0,255 do COLTBL[i]=Rainbow24(255-i,v=(i/64.)<1)
;  COLTBL[0:25]='ffffff'x
;-
FUNCTION Rainbow24,hue,v=v
  if ~ISA(v) then v=1
  hp=hue/42.5
  X=(1-ABS(hp mod 2 -1))
  if hp lt 1 or hp gt 5 then r=1 $
  else if hp lt 3 then g=1 else b=1
  if hp lt 2 then b=0 $
  else if hp gt 4 then g=0 else r=0
  if ~ISA(r) then r=x else if ~ISA(g) then g=x else b=x
  col=FIX(255*v*[r,g,b])
  mult=256L^[0,1,2]
  RETURN,TOTAL(mult*col,/INT);256L*r+256L^2*g+256L^3*b-1
END


;+
;Plot parameters will allow us to come back to this plot
;-
Function ParamDef
  RETURN,{PARAM,$
    p:!P,$
    x:!X,$
    y:!Y,$
    xr:[0.1,1.1],$
    yr:[0.1,1.1],$
    pos:[.1,.1,.1,.1],$
    loc:[0,0,0,0]$
  }
END


;+
;Establishes the general structure of plot data
;-
Function PlotDataDef
  RETURN,{plotdata,$
    name:'',$ ;This is the data's name
    xd:PTR_NEW(),$ ;data x values
    yd:PTR_NEW(),$ ;data y values
    col:0} ;color to use in plots
END

;+
;Draw page labels made by BlockLabel
;-
PRO DrawPageLabels
  common comPage,page,pageLabel
  for i=0,N_ELEMENTS(pageLabel)-1 do begin
    XYOUTS,pageLabel[i].xp,pageLabel[i].yp,pageLabel[i].labels,ALIGNMENT=.5,$
      /NORMAL,ORIENTATION=pageLabel[i].r,CHARSIZE=pageLabel[i].sz
    pageLabel[i].keep-=1
  endfor
  if ISA(PageLabel) then pagelabel=PageLabel[WHERE(pageLabel.keep gt 0,/NULL)]
end


;+
;Gets loc based on page.cur, makes a new page if the current one is full
;CALL:   GetLoc
;RETURNS:
;loc     [x,y,xsub,ysub] = where the graph should go
;-
Function GetLoc,noMove=noMove
  common comPage
  if ~KEYWORD_SET(noMove) then AutoClear
  loc=[0.,0.,0.,0.]
  bits=[page.xdiv,page.ydiv,page.xsub,page.ysub]
  for i=0,3 do begin
    a=WHERE(page.ord lt page.ord[i],cnt)
    if cnt gt 0 then loc[i]=page.cur/FIX(PRODUCT(bits[a])) else loc[i]=page.cur
    loc[i]=loc[i] mod bits[i]
  endfor
  RETURN,loc
END


;+
;Gets actual position on a page, based on location
;CALL:   GetPos,loc=loc,units=units
;
;INPUTS:
;loc     [num] Get one from GetLoc ([x,y,sub,ysub])
;units   str   Units of page (<'norm'>,'px' or 'pg')
;
;RETURNS:
;pos     [xleast,yleast,xmost,ymost]
;-
Function GetPos,loc,units=units
  common comPage
  ;Find Width and Height a main graph (in page units)
  loc=FLOAT(loc)
  w=(page.xsiz-(page.xdiv-1)*page.spc[0]-page.border[0]-page.border[2]-(page.xsub-1)*page.spc[2])/page.xdiv
  h=(page.ysiz-(page.ydiv-1)*page.spc[1]-page.border[1]-page.border[3]-(page.ysub-1)*page.spc[3])/page.ydiv

  pxl=page.border[0]+page.spc[0]*loc[0]+page.spc[2]*loc[2]+(loc[0]+loc[2]/page.xsub)*w
  pxm=pxl+w/page.xsub
  pym=page.ysiz-page.spc[1]*loc[1]-page.spc[3]*loc[3]-page.border[1]-(loc[1]+loc[3]/page.ysub)*h
  pyl=pym-h/page.ysub
  ;Now in page units, might need to convert

  rescale=[1,1]
  if not KEYWORD_SET(units) then units='norm'
  if units eq 'px' then rescale=[!D.X_SIZE,!D.Y_SIZE]	;this is pixels on the screen
  if units ne 'pg' then rescale/=FLOAT([page.xsiz,page.ysiz])	;this is normalization
  RETURN, [[pxl,pyl]*rescale,[pxm,pym]*rescale]
END


;+
;Takes in 2 location arguments, figures
;CALL:   Loc2to4,loc
;
;HOLDERS:
;Loc     [num] ([x,y])
;-
Pro Loc2to4,loc
  common comPage,page,pageLabel
  if N_ELEMENTS(loc) ne 2 then PRINT,"Loc2to4 wants 2 elements in loc"
  nl=INTARR(4)
  nl[0]=loc[0]/page.xsub
  nl[1]=loc[1]/page.ysub
  nl[2]=loc[0] mod page.xsub
  nl[3]=loc[1] mod page.ysub
  loc=nl
END

;Force keyword makes a new page
;any time a page is about to be made, labels are drawn on the current page
PRO AutoClear,force=force
  common comPage,page,pageLabel
  page.cur+=1
  if page.cur ge page.ydiv*page.xdiv*page.ysub*page.xsub or KEYWORD_SET(force) then begin
    if ISA(pageLabel) then DrawPageLabels
    page.cur=0
    ERASE
  endif
  page.cur-=KEYWORD_SET(force)
END


Pro FixLoc,loc
  common comPage,page,pageLabel
  if loc[0] ge page.xdiv || loc[1] ge page.ydiv then begin
    loc[0]-=page.xdiv*(loc[0] / page.xdiv)
    loc[1]-=page.ydiv*(loc[1] / page.ydiv)
  endif
END


;+
;My version of plot
;CALL:   MakePlot(page,[Many options, see below])
;
;INPUTS:
;page    page  The page object that we draw on, get one from PageDef() (Deprecated)
;?d      [num] x,y sample data, does not get plotted
;?r      [num] x,y ranges
;er      bool  if set, expands x and y ranges to 0.9-1.1 times original
;?log    bool  x,y Sets an axis to be logaritmic
;units   str   Specifies to work in normal or data coordinates ('norm' or 'data') {'norm'}
;loc     [num] Postion will be gotten from page divs ([x,y,xsub,ysub]) {GetLoc}
;pos     [num] Position of plot ([xl,yl,xm,ym]) {GetPos(loc)}
;?title  str   x,y,_ Titles for the graph {'X','Y','Untitled'}
;calm    bool  if false, tries to automatically predict best title placement
;?style  int   x,y,line Styles, as normal {1,1,0}
;        x&y bitwise 1 force exact, 2 extends axis, 4 surpresses, 8 only does left axis
;?Fake   bool  x,y Fakes an axis so that we have more control {False,False}
;?tval   [num] x,y tick values, only used by fake axis, when ?Fake=1
;?num    int   x,y Number of major tick intervals
;?tint   num   x,y Interval between major ticks
;?tsub   int   x,y Subticks per tick
;?tlen   flt   x,y Major tick lengths
;?tlay   int   x,y Tick layout 0=normal, 1=labels only,2=?
;?tf     int   x,y Tick format
;?gstyle int   x,y gridstyle
;norm    bool  Uses normal coords {!}
;iso     bool  Isotropic {0}
;label   str   If keyword is set, labels the plot. If str, uses itself as the label. {'a'}
;lcorn   [str] corner to put the label in {['r','t']}
;noAxis  bool  Turn off the tick marks and tick labels
;auto    bool  Advance the plots, even if loc was given
;-
Function MakePlot,pageIn=pageIn,$
  xd=xd,yd=yd,$;Data
  xr=xr,yr=yr,er=er,$;Ranges
  xlog=xlog,ylog=ylog,$;Log
  units=units,loc=loc,pos=pos,$;Position
  title=titleIn,xtitle=xtitleIn,ytitle=ytitleIn,$;Labels
  calm=calm,$
  linestyle=linestyle,xstyle=xstyle,ystyle=ystyle,$;Styles
  xFake=xFake,yFake=yFake,$
  xtval=xtval,ytval=ytval,$
  xtnum=xtnum,ytnum=ytnum,$
  xtint=xtint,ytint=ytint,$
  xtsub=xtsub,ytsub=ytsub,$
  xtlen=xtlen,ytlen=ytlen,$
  xtlay=xtlay,ytlay=ytlay,$
  xtf=xtf,ytf=ytf,$
  xgstyle=xgstyle,ygstyle=ygstyle,$
  norm=norm,iso=iso,$
  label=label,lcorn=lcorn,noAxis=noAxis,auto=auto,_EXTRA=e
  common comPage
  if ISA(pageIn) then page=pageIn
  if ~KEYWORD_SET(xd) then xd=[0.0,1.0]
  if ~KEYWORD_SET(yd) then yd=[0.0,1.0]
  if ~KEYWORD_SET(xr) then begin
    xm=MAX(xd,min=xs)
    xr=[xs,xm]
  endif
  if ~KEYWORD_SET(yr) then begin
    ym=MAX(yd,min=ys)
    yr=[ys,ym]
  endif
  if KEYWORD_SET(er) then begin
    er=1.2
    xr=((xr[1]+xr[0])+er*(xr[1]-xr[0])*[-1,1])/2.
    yr=((yr[1]+yr[0])+er*(yr[1]-yr[0])*[-1,1])/2.
  endif

  if ~KEYWORD_SET(units) then units='norm'
  if ~KEYWORD_SET(pos) then begin ;if pos provided, don't do any positioning
    if ~KEYWORD_SET(loc) then loc=GetLoc() else $
      if KEYWORD_SET(auto) then AutoClear
    if N_ELEMENTS(loc) eq 2 then Loc2to4,loc ;x,y fill subdivs
    FixLoc,loc
    pos=GetPos(loc,units=units)
  endif else loc=[0,0,0,0]

  title=KEYWORD_SET(TitleIn)?titleIn:''
  xtitle=KEYWORD_SET(xTitleIn)?xTitleIn:''
  ytitle=KEYWORD_SET(yTitleIn)?yTitleIn:''

  ;Hide bits that are overlapping other graphs
  if ~(KEYWORD_SET(calm) or KEYWORD_SET(noAxis) or (page.ysub eq 1)) then begin
    XYOUTS,(pos[0]+pos[2])/2.,.9*pos[3]+.1*pos[1]-.3*page.fsiz,title,ALIGNMENT=.5,/NORMAL,CHARSIZE=1.4
    title=''
  endif
  if loc[3] lt page.ysub-1 or keyword_set(noXAxis) then begin
    xtnum=1;f='(A1)'
    xtitle=''
  endif
  if loc[2] ne 0 or keyword_set(noYAxis) then begin
    ytnum=1;f='(A1)'
    ytitle=''
  endif

  if KEYWORD_SET(noAxis) then begin
    xtf='(A1)'
    ytf='(A1)'
    xtitle=''
    ytitle=''
    xtlay=1
    ytlay=1
  endif
  if ~ISA(xstyle) then xstyle=1
  if ~ISA(ystyle) then ystyle=1

  if KEYWORD_SET(xgstyle) and ~KEYWORD_SET(xtlen) then xtlen=.5
  if KEYWORD_SET(ygstyle) and ~KEYWORD_SET(ytlen) then ytlen=.5

  if KEYWORD_SET(xFake) then begin
    xtitle2=TEMPORARY(xtitle)
    xtf2=TEMPORARY(xtf)
    xtf='(A1)'
    xtlen2=TEMPORARY(xtlen) & xtlen=0.00001
    xgstyle2=TEMPORARY(xgstyle)
  endif
  if KEYWORD_SET(yFake) then begin
    ytitle2=TEMPORARY(ytitle)
    ytf2=TEMPORARY(ytf)
    ytf='(A1)'
    ytlen2=TEMPORARY(ytlen) & ytlen=0.00001
    ygstyle2=TEMPORARY(ygstyle)
  endif

  plot,xd,yd,$
    XRANGE=xr,YRANGE=yr,$
    XLOG=xlog,YLOG=ylog,$
    XSTYLE=xstyle,YSTYLE=ystyle,$
    TITLE=title,XTITLE=xtitle,YTITLE=ytitle,$
    POSITION=pos,/NOERASE,/NODATA,$
    NORMAL=norm,iso=iso,font=0,$
    XTHICK=page.thick,YTHICK=page.thick,$
    XTICKFORMAT=xtf,YTICKFORMAT=ytf,$
    XTICKLEN=xtlen,YTICKLEN=ytlen,$
    XTICKLAYOUT=xtlay,YTICKLAYOUT=ytlay,$
    XTICKS=xtnum,YTICKS=ytnum,$
    XTICKINTERVAL=xtint,YTICKINTERVAL=ytint,$
    XMINOR=xtsub,YMINOR=ytsub,$
    XGRIDSTYLE=xgstyle,YGRIDSTYLE=ygstyle,_EXTRA=e

  if KEYWORD_SET(xFake) then begin
    AXIS,0,yr[0],XAXIS=0,$
      XRANGE=[xtval[0],xtval[-1]],$
      XTITLE=xtitle2,$
      XSTYLE=xstyle,$
      XTICKLEN=xtlen2,$
      XTICKS=N_ELEMENTS(xtval)-1,$
      XTICKINTERVAL=xtint,$
      XMINOR=xtsub,$
      XTICKFORMAT=xtf2,$
      XGRIDSTYLE=xgstyle2,$
      XTHICK=page.thick
    AXIS,0,yr[1],XAXIS=1,$
      XRANGE=[xtval[0],xtval[-1]],$
      XSTYLE=xstyle,$
      XTICKLEN=(xtlen2 eq 1)?0.00001:xtlen2,$
      XTICKS=N_ELEMENTS(xtval)-1,$
      XTICKINTERVAL=xtint,$
      XMINOR=xtsub,$
      XTICKFORMAT='(A1)',$
      XGRIDSTYLE=xgstyle2,$
      XTHICK=page.thick
  endif

  if KEYWORD_SET(yFake) then begin
    if ~KEYWORD_SET(ytsub) then ytsub=FIX((ytval[1]-ytval[0])^.5)
    AXIS,xr[0],0,YAXIS=0,$
      YRANGE=[ytval[0],ytval[-1]],$
      YTITLE=Ytitle2,$
      YSTYLE=ystyle,$
      YTICKLEN=ytlen2,$
      YTICKS=N_ELEMENTS(ytval)-1,$
      YMINOR=ytsub,$
      YTICKFORMAT=ytf2,$
      YGRIDSTYLE=ygstyle2,$
      YTHICK=page.thick
    AXIS,xr[1],0,YAXIS=1,$
      YRANGE=[ytval[0],ytval[-1]],$
      YSTYLE=ystyle,$
      YTICKLEN=(ytlen2 eq 1)?0.00001:ytlen2,$
      YTICKS=N_ELEMENTS(ytval)-1,$
      YMINOR=ytsub,$
      YTICKFORMAT='(A1)',$
      YGRIDSTYLE=ygstyle2,$
      YTHICK=page.thick
  endif

  if ISA(label) then PlotLabel,label,pos,corner=lcorn ;Label Making

  ;Return acces to this graph
  par={PARAM,p:!P,x:!X,y:!Y,xr:FLOAT(xr),yr:FLOAT(yr),pos:pos,loc:loc}
  RETURN,par
END

;tl  flt  tick length
PRO MidAxis,plt,tl=tl
  AXIS,0,0,xaxis=0,xrange=plt.xr,xticklen=tl,/xstyle
END

;+
;Mostly a helper function for MakePlot, can be called independently
;corner as stringarr ['r','t']
;-
PRO PlotLabel,label,pos,corner=corner,col=col,scl=scl
  common comPage
  if ~ISA(corner) then corner=['r','t']
  if ~KEYWORD_SET(col) then col=0
  if ~KEYWORD_SET(scl) then scl=2
  xs=0 & ys=0
  if TOTAL(STRCMP(corner,'r',1)) then xs=1 else if TOTAL(STRCMP(corner,'l',1)) then xs=-1
  if TOTAL(STRCMP(corner,'t',1)) then ys=1 else if TOTAL(STRCMP(corner,'b',1)) then ys=-1
  if ISA(label,/int) then begin
    if label eq 1 then $
      label=''+STRING(BYTE(97+page.cur))+'' else $
      label=''+STRING(BYTE(97+page.cur+label))+''
  endif
  if ISA(label,/STRING) then begin
    x=pos[1+xs]-xs*2.5/page.xdiv*page.fsiz
    if TOTAL(STRCMP(corner,'x',1)) then x=(pos[0]+pos[2])/2
    y=pos[2+ys]-(ys+.3)*3.*page.fsiz*page.xsiz/page.ysiz
    XYOUTS,x,y,label,ALIGNMENT=.5+xs/4.,/NORMAL,CHARSIZE=scl,color=col,font=1
  endif
END

;+
;Makes stuff good for pixelplot?
;this is sloppy
;-
PRO Pixelate,xd,yd,xr,yr,mx
  MESSAGE,'what even uses this?'
  n=50
  xAxis=[xr[0]:xr[-1]:FLOAT(xr[-1]-xr[0])/n]
  yAxis=[yr[0]:yr[-1]:FLOAT(yr[-1]-yr[0])/n]
  CleanUp,xd,yd,nxd,nyd,zeros='remove'
  dx=xaxis[1]-xaxis[0]
  dy=yaxis[1]-yaxis[0]
  h2d=hist_2d(nxd,nyd,bin1=dx,bin2=dy,$
    min1=xr[0],max1=xr[-1],min2=yr[0],max2=yr[-1])
  x=HISTOGRAM(nxd,min=0,max=6,nbins=n)
  xi=[0,TOTAL(x,/cum)]
  mx=MAX(x)
  n_arr=FLTARR([n,n])
  color=254./MAX(h2d)
  for i=0,n-2 do begin
    if x[i+1] gt 0 then hy=HISTOGRAM(nyd[xi[i]:xi[i+1]],min=0,max=6,NBINS=n) else hy=REPLICATE(0,n)
    n_arr[i,*]=hy
    for j=0,n-1 do PLOTS,xAxis[i]+.05,yAxis[j]+.05,col=hy[j]*color<254,psym=8,symsize=2
  endfor
END


Pro PlotVecs,x,y,dx,dy,col=col
  nx=N_ELEMENTS(x)
  OPLOT,x,y,psym=4,symsize=.5
  for i=0,nx-1 do OPLOT,x[i]+[0,dx[i]],y[i]+[0,dy[i]],col=col
END



Pro PlotPixels,z,x,y,sz,colp,acc=acc
  if KEYWORD_SET(acc) then for i=MIN(colp,max=mi),mi do begin
    q=WHERE(colp eq i)
    PLOTS,x[q],y[q],col=i,psym=8
  endfor else begin
    sz=SIZE(z,/dim)
    q=WHERE(FINITE(z),cnt)
    PLOTS,x[q mod sz[0]],y[(q/sz[0]) mod sz[1]],col=colp[q],psym=8
  endelse
END

Pro PlotEdges,z,x,y,sz,colp,edgeVert=edgeVert,thick=thick
  if ~ISA(thick) then thick=2
  sz[edgeVert+1]+=1;expand because plots needs n+1 points to make n segments
  if edgeVert eq 1 then $
    for i=0,sz[1]-1 do PLOTS,REPLICATE(x[i],sz[2]),y,color=[0,REFORM(colp[i,*])],thick=thick else $
    for j=0,sz[2]-1 do PLOTS,x,REPLICATE(y[j],sz[1]),color=[0,colp[*,j]],thick=thick
END

Function FixBounds,z,x,y
  xtp=WHERE(x ge !x.crange[0] and x lt !x.crange[1])
  ytp=WHERE(y ge !y.crange[0] and y lt !y.crange[1])
  z2=z[xtp,*]
  z3=z2[*,ytp]
  RETURN,z3
END

Function PixelRange,var,sz,crange,edgevert,edm
  s=CRANGE[0] & m=CRANGE[1]-(CRANGE[1]-s)/sz
  if ISA(edgevert) then if edgevert eq edm then m=CRANGE[1]
return,FLOAT([0:sz-1])/FLOAT(sz-1)*(m-s)+s
END

;+
;makeplot, but with colorful histograms
;An alternate to contour that gives continous depth buit discrete space
;
;INPUTS:
;z       [num] data to plot
;x       [int] x
;y       [int] y
;leg     bool  make a legend here?
;sc      num   scale of pixels
;cols    [int] colors
;levs    [num] levels
;clipLev [num] truncate input data to these bounds
;bgc     byte   background color (uses current ct)
;allowColorWrap bool if values go outside specified levels, should we wrap or clip the colors
;noNorm  bool  if set, assign colors by interpolating from levs (False)
;edgeVert  0 for horizontal lines edges, 1 for vertical lines
;-
PRO PixelPlot,z,xi=xi,yi=yi,leg=leg,sc=sc,cols=cols,levs=levs,make=make,$
  clipLev=clipLev,bgc=bgc,allowColorWrap=allowColorWrap,noNorm=noNorm,edgeVert=edgeVert
  common comPage
  sz=SIZE(z)
  if keyword_set(make) then make=makeplot(xr=[0,sz[1]],yr=[0,sz[2]])
  if ~KEYWORD_SET(xi) then x=PixelRange(xi,sz[1],!x.crange,edgevert,1) else  x=xi
  if (x[0] eq !x.crange[0] and x[-1] eq !x.crange[1]) then x=PixelRange(x,sz[1],!x.crange,edgevert,1)
  if ~KEYWORD_SET(yi) then y=PixelRange(yi,sz[2],!y.crange,edgevert,0) else  y=yi
  if (y[0] eq !y.crange[0] and y[-1] eq !y.crange[1]) then y=PixelRange(y,sz[2],!y.crange,edgevert,0)
  if ISA(edgevert) then begin
    if edgeVert eq 0 then x=[x,2*x[-1]-x[-2]] else y=[y,2*y[-1]-y[-2]]
  endif

  if ~KEYWORD_SET(sc) then begin
    pagePx=(!P.CLIP[2:3]-!P.CLIP[0:1])
    sc=PagePx*2.05/!D.X_CH_SIZE/sz[1:2]
    sc*=abs([(x[-1]-x[0])/(!x.CRANGE[1]-!X.crange[0]),(y[-1]-y[0])/(!y.CRANGE[1]-!y.crange[0])])
  endif
  USERSYM, [0,1,1,0,0]*sc[0],[0,0,1,1,0]*sc[1],/fill
  ;normalize data to color range
  consider=WHERE(FINITE(z),cnt)
  if cnt eq 0 then begin
    PRINT,'No finite values'
    RETURN
  endif
  
  s=MIN(z[consider],max=m)
  m+=.01
  if KEYWORD_SET(clipLev) then begin
    if FINITE(clipLev[0]) then s=cliplev[0]
    if FINITE(clipLev[1]) then m=cliplev[1]
  endif else clipLev=[s,m]
  if m eq s then nrm=z else nrm=255*(z-s)/(m-s)
  if ~KEYWORD_SET(allowColorWrap) then nrm=(255<nrm>0) ;clips it to prevent wrap
  colp=BYTE(nrm)
  if ~KEYWORD_SET(levs) then levs=[s:m:(m-s)/12.]
  
  if KEYWORD_SET(cols) or keyword_set(noNorm) then begin
    colp*=0
    if ~keyword_set(cols) then begin
      if ~KEYWORD_SET(allowColorWrap) then z2u=min(levs,max=mx)>z<mx else z2u=z
      colp=byte(interpol([0:255:255./(n_elements(levs)-1)],levs,z2u))
    endif else for L=0,N_ELEMENTS(levs)-1 do begin
      h=WHERE(z ge levs(L),cnt)
      if cnt gt 0 then colp[h]=cols[L]
    endfor
  endif
  
  if ISA(bgc) then MakeBox,[x[0],!x.crange[-1]],[y[0],!y.crange[-1]],col=bgc,units='data'
  if ISA(edgeVert) then $
    PlotEdges,z,x,y,sz,colp,edgeVert=edgeVert else $
    PlotPixels,z,x,y,sz,colp
  if ISA(leg) then begin
    ml=MAX(ABS(levs))
    form='%-4.1f'
    if ml gt 10 then form='%3i'
    if ml gt 100 then form='%4i'
    if ml ge 1000 then form='%5i'
    if KEYWORD_SET(leg) then ColBar,Levs,place=leg-[.0,0.5],form=form,posOut=leg
  endif
END


;+
;Labels groups of plots (as produced by produced by MakePlot)
;CALL:   BlockLabel,page,labels,size=size,xdivs=xdivs,ydivs=ydivs,$
;               xsubs=xsubs,ysubs=ysubs,xLabel=xLabel,yLabel=yLabel
;
;INPUTS:
;labels  [str] the labels to be written
;sizeL    flt   character size {1}
;?divs   [int] x,y which major divisions to span
;?subs   [int] x,y which minor divisions to span
;?Label  bool  x,y set to 0 to get top/left, set to 1 to get bottom/right
;now     bool  draw the labels right now {0} if not set, you should use DrawPageLabels
;pos     [num] position of space around which to label, repllaces divs and subs
;keep    [num] how many times to print a label before killing it
;-
PRO BlockLabel,labels,sizeL=sizeL,$
  xdivs=xdivs,ydivs=ydivs,$
  xsubs=xsubs,ysubs=ysubs,$
  xLabel=xLabel,yLabel=yLabel,$
  now=now,pos=pos,keep=keep
  common comPage,page,pageLabel

  if ~KEYWORD_SET(sizeL) then sizeL=1.0
  if ~KEYWORD_SET(keep) then keep=1.0
  nlabels=N_ELEMENTS(labels)
  if ISA(xlabel) then begin
    if (~ISA(xdivs) and ~ISA(xsubs)) then begin
      xdivs=0 & xsubs=0
      if nlabels eq page.xdiv $
        then xdivs=[0:page.xdiv-1] $
      else if nlabels eq page.xsub $
        then xsubs=[0:page.xsub-1] $
      else if nlabels eq page.xsub*page.xdiv then begin
        xdivs=[0:page.xdiv-1]
        xsubs=[0:page.xsub-1]
      endif else xdivs=[0:nlabels-1]
    endif
  endif $
  else if ISA(yLabel) then begin
    if ~ISA(xdivs) then xdivs=[0:page.xdiv-1]
  endif else MESSAGE,"You need to specify either x or y label"

  if ~ISA(xsubs) then if ISA(xLabel) then xsubs=[0:page.xsub-1] else xsubs=0
  if ~ISA(ydivs) then ydivs=0
  if ~ISA(ysubs) then ysubs=0

  nxd=N_ELEMENTS(xdivs) & nyd=N_ELEMENTS(ydivs)
  nxs=N_ELEMENTS(xsubs) & nys=N_ELEMENTS(ysubs)
  nx=nxd*nxs
  ny=nyd*nys
  if nx*ny ne nlabels then begin
    if ((nx*ny mod nlabels) ne 0) then MESSAGE,"Number of blocks does not match number of labels"
    newLabels=labels
    while N_ELEMENTS(newLabels) lt nx*ny do newLabels=[newLabels,labels]
    labels=newLabels
  endif

  for i=0,nx-1 do for j=0,ny-1 do begin ;Get the position of the block to be labelling
    if KEYWORD_SET(pos) then begin
      xs=pos[0] & ys=pos[1]
      xm=pos[2] & ym=pos[3]
    endif else begin
      xm=0 & ym=0
      xs=page.xsiz & ys=page.ysiz
      if ISA(yLabel) then begin
        if ylabel ne 0 and ylabel ne 1 then MESSAGE,"yLabel must be 0 or 1"
        if ~KEYWORD_SET(ysubs) then begin
          pos0=GetPos([xdivs[i],ydivs[j],0,0])
          pos1=GetPos([xdivs[i],ydivs[j],page.xsub,page.ysub])
        endif else begin
          pos0=GetPos([xdivs[i],ydivs,0,ysubs[j]])
          pos1=GetPos([xdivs[i],ydivs,page.xsub,ysubs[j]])
        endelse

      endif else if ISA(xLabel) then begin
        if xlabel ne 0 and xlabel ne 1 then MESSAGE,"xLabel must be 0 or 1"
        if ~KEYWORD_SET(xsubs) then begin
          pos0=GetPos([xdivs[i],ydivs[j],0,0])
          pos1=GetPos([xdivs[i],ydivs[j],page.xsub-1,page.ysub])
        endif else begin
          pos0=GetPos([xdivs,ydivs,xsubs[i],0])
          pos1=GetPos([xdivs,ydivs,xsubs[i],page.ysub])
        endelse
      endif
      xs=xs<pos0[0] & ym=ym>pos0[3]
      xm=xm>pos1[2] & ys=ys<pos1[3]
    endelse
    if ISA(ylabel) then begin
      xp=yLabel?xm+page.fsiz:xs-3.*page.fsiz ;right or left
      yp=(ys+ym)/2 ;middle
    endif else if ISA(xlabel) then begin
      xp=(xs+xm)/2 ;middle
      yp=xLabel?ys-3*page.fsiz:ym+page.fsiz*1.2 ;bottom or top
    endif
    pageLabel=[pageLabel,{label,$
      xp:xp,yp:yp,labels:labels[i*ny+j],r:90*ISA(ylabel),sz:sizeL,keep:keep}]
  endfor
  if KEYWORD_SET(now) then DrawPageLabels
END



PRO BlockLabel2,label,sizeL=sizeL,$
  divs=divs,subs=subs,$
  xLabel=xLabel,yLabel=yLabel,$
  now=now,keep=keep
  common comPage,page,pageLabel

  if ~KEYWORD_SET(sizeL) then sizeL=1.0
  if ~KEYWORD_SET(keep) then keep=1.0
  if ISA(xlabel) then begin
    if KEYWORD_SET(divs) then subs=[0,page.xsub-1] else divs=0
    pos0=getPos([divs[0],0,subs[0],0])
    pos1=getPos([divs[-1],0,subs[-1],0])
  endif

  xs=pos0[0] & ym=pos0[3]
  xm=pos1[2] & ys=pos1[3]

  if ISA(ylabel) then begin
    xp=yLabel?xm+page.fsiz:xs-3.*page.fsiz ;right or left
    yp=(ys+ym)/2. ;middle
  endif else if ISA(xlabel) then begin
    xp=(xs+xm)/2. ;middle
    yp=xLabel?ys-3*page.fsiz:ym+page.fsiz*1.2 ;bottom or top
  endif
  pageLabel=[pageLabel,{label,$
    xp:xp,yp:yp,labels:label,r:90*ISA(ylabel),sz:sizeL,keep:keep}]
  if KEYWORD_SET(now) then DrawPageLabels
END


PRO RedoAxis,par,xc,yc,xr=xr,yr=yr,xi,yi,xs=xs,ys=ys,thick=thick
  common comPage
  if ~KEYWORD_SET(thick) then thick=page.thick
  if ~KEYWORD_SET(xr) then xr=par.xr
  if ~KEYWORD_SET(yr) then yr=par.yr
  ;Draw the long ticks across the graph
  AXIS,xr[0],0,YAXIS=0,YRANGE=yr,ystyle=1,yticklen=.5,ytickinterval=yi,color=yc,yminor=ys,ythick=thick,ytickformat='(A1)'
  AXIS,xr[1],0,YAXIS=1,YRANGE=yr,ystyle=1,yticklen=.5,ytickinterval=yi,color=yc,yminor=ys,ythick=thick,ytickformat='(A1)'
  AXIS,0,yr[0],XAXIS=0,XRANGE=xr,xstyle=1,xticklen=.5,xtickinterval=xi,color=xc,xminor=xs,xthick=thick,xtickformat='(A1)'
  AXIS,0,yr[1],XAXIS=1,XRANGE=xr,xstyle=1,xticklen=.5,xtickinterval=xi,color=xc,xminor=xs,xthick=thick,xtickformat='(A1)'
END


;+
;Makes Axis fit for a contour plot
;CALL:   FixContour,par,xc,yc,xi,yi,xs,ys
;par     plot
;?c      int   colors of x and y tick marks
;?r      [num] x,y range {from par}
;?i      int   x,y tick intervals
;?s      int   x,y sub-ticks intervals
;?title  str   x,y title {''}
;?tname  [str] x,y tick labels {''}
;thick   num   thickness of axis to draw {4}
;-
PRO FixContour,par,xc,yc,xr=xr,yr=yr,xi,yi,xs,ys,thick=thick,$
  xtitle=xtitle,ytitle=ytitle,xtname=xtname,ytname=ytname
  ;Draw the long ticks across the graph
  RedoAXIS,par,xc,yc,xr=xr,yr=yr,xi,yi,xs=xs,ys=ys,thick=thick
  ;Do the labels
  if N_ELEMENTS(xtname) eq 1 then $
    if ISA(xtname,/STR) then $
    xtname=REPLICATE(xtname,CEIL((!X.CRANGE[1]-!X.CRANGE[0])/FLOAT(xi)))
  if N_ELEMENTS(ytname) eq 1 then $
    if ISA(ytname,/STR) then $
    ytname=REPLICATE(ytname,CEIL((!Y.CRANGE[1]-!Y.CRANGE[0])/FLOAT(yi)))
  AXIS,0,yr[0],XAXIS=0,XRANGE=xr,xstyle=1,xticklayout=1,xtickinterval=xi,color=0,xminor=xs,xthick=thick,xtitle=xtitle,xtickname=xtname
  AXIS,xr[0],0,YAXIS=0,YRANGE=yr,ystyle=1,yticklayout=1,ytickinterval=yi,color=0,yminor=ys,ythick=thick,ytitle=ytitle,ytickname=ytname
  if xc ne 0 or yc ne 0 then $ ;makes the outline the right color
    OPLOT,[xr[0],xr[1],xr[1],xr[0],xr[0]],[yr[0],yr[0],yr[1],yr[1],yr[0]],col=0,thick=thick
END


;+
;Draws a white box with black outline
;CALL:   MakeBox,xr,yr,thick=thick,col=col,out=out,units=units
;xr,yr   flt   x and y ranges of the box to be made
;thick   flt   thickness of outline
;col     int   color
;units   str   'norm' or 'data'
;-
PRO MakeBox,xr,yr,thick=thick,col=col,out=out,units=units
  if ~KEYWORD_SET(units) then units='norm'
  x1=MIN(xr,max=x2)
  y1=MIN(yr,max=y2)
  if N_ELEMENTS(col) eq 0 then col=!p.background;white as default
  if ~ISA(thick) then thick=(x2-x1)/100
  if ISA(out) then POLYFILL,$
    [x1-thick,x2+thick,x2+thick,x1-thick],$
    [y1-thick,y1-thick,y2+thick,y2+thick],$
    COLOR=out,NORMAL=(units eq 'norm'),DATA=(units eq 'data')
  POLYFILL,[x1,x2,x2,x1],[y1,y1,y2,y2],COLOR=col,NORMAL=(units eq 'norm'),DATA=(units eq 'data')
END

;+
;Gets the width of the widest string
;-
Function GetStrWidth,names,scale=scale
  if ~KEYWORD_SET(scale) then scale=1
  w0=0
  for i=0,N_ELEMENTS(names)-1 do begin
    XYOUTS,1,1,names[i],CHARSIZE=scale,/NORMAL,WIDTH=w,NOCLIP=0,CLIP=[0,0,0,0] ;Gets width of widest name
    w0=w>w0
  endfor
  RETURN,w0
END


;+
;Gets new levels and colors for colorbar making
;-
PRO RescaleLevCol,levIn,colIn,levOut,colOut,n
  levOut=INTERPOL(levIn,n)
  colOut=INTERPOL(colIn,n-1)
END

;+
;Makes a colorbar of n levels and n-1 colors
;CALL   PRO ColBar,pos,levIn,colIn=colIn,num=num,scale=scale,place=place,form=form
;INPUTS:
;pos    [num] position of graph with which to associate this guy
;levIn  [num] levels
;colIn  [int] colors
;num    int   number of entries to show
;scale  num   scale factor {0.75}
;place  [num] if gt 1, puts it outside [right,top]
;form   str   string formatting for levels (%8.2f)
;name   str   puts a name with it
;-
PRO ColBar,posIn,levIn,colIn=colIn,$
  num=num,scale=scale,place=place,form=form,posOut=posOut,name=name
  common comPage
  pos=posIn
  if ~(ISA(pos,/int) or ISA(pos,/float)) then pos=posIn.pos ;else use given pos
  noPos=N_ELEMENTS(pos) ne 4
  if noPos then begin
    levin=pos
    pos=GetPos(GetLoc(/noMove))
  endif
  if ~KEYWORD_SET(colIn) then colIn=[1,254]
  if ~KEYWORD_SET(scale) then scale=.75
  if ~KEYWORD_SET(place) then place=[1,1] ;right,top
  if ~KEYWORD_SET(form) then form='%-8.2f'
  levs=levIn
  cols=colIn
  if ~KEYWORD_SET(num) then num=N_ELEMENTS(levIn)
  if N_ELEMENTS(levIn) ne num then levs=INTERPOL(levIn,num)
  if N_ELEMENTS(colIn) eq N_ELEMENTS(levs) or $
    (N_ELEMENTS(colin) eq N_ELEMENTS(levin) and cols[-1] eq 255)then cols=colIn[0:-2]
  if N_ELEMENTS(cols) ne num-1 then cols=INTERPOL(cols,num-1)

  ls=4*scale/3.

  strLev=STRING(FORMAT=form,levs)
  w0=getStrWidth(strLev,scale=scale)
  f=FLOAT(!D.Y_CH_SIZE)/!D.Y_VSIZE
  g=FLOAT(!D.X_CH_SIZE)/!D.X_VSIZE
  boxWidth=w0+3*g
  boxHeight=ls*f*num
  xm=(place[0]<1)*pos[2]+(1-(place[0]<1))*(pos[0]+boxWidth)
  ym=(place[1]<1)*pos[3]+(1-(place[1]<1))*(pos[1]+boxHeight)
  xr=xm+([-1,0]+(place[0] gt 1))*boxWidth
  yr=[ym-boxHeight,ym]
  MakeBox,xr,yr,thick=.0015,out=0 ;f/8 is a good thicknes
  posOut=[xr,yr]
  for i=0,num-2 do MakeBox,[xr[0]+g/2.,xr[0]+4*g/2.],yr[0]+ls*f*[i+.5,i+1.5],col=cols[i],thick=0
  for i=0,num-1 do XYOUTS,xr[0]+5*g/2,yr[0]+ls*f*(i+.2),strLev[i],CHARSIZE=scale,/NORMAL
  if noPos then pos=levIn ;return levs back as first argument if pos was not given
  if ISA(name) then begin
    if name.strlen() gt 4 then begin
      x=posout[0]
      y=mean(posout[2:3])
      ori=90
      if place[0] ge 1 then x=posout[1]+page.fsiz*ls*1.2
    endif else begin
      x=mean(posout[0:1])
      y=posout[3]+page.fsiz/3
      ori=0
    endelse
    XYOUTS,x,y,name,charsize=.85,align=.5,/norm,orientation=ori
  endif
END


;+
;CALL:   MakeLegend,nams,cols,pageIn=pageIn,pos,scale=scale,colbar=colbar,place=place,num=num,height=height,form=form
;nams    [str] names to put in legend
;cols    [int] colors of lines associated with those names
;pos     [flt] Or Postion object from MakePlot() [left,top,right,bottom]
;scale   flt   scale {.75}
;colbar  bool  is it a colorbar?
;place   [num] Where the legend goes right,top={1,1}
;num     [int] number of entries
;height  num   resricts height
;form    str   formatting string
;RETURNS:
;pos     [num] the normal position of the legend [xl,yl,xm,ys]
;-
Function MakeLegend,namesIn,colorsIn,pos,$
  scale=scale,colbar=colbar,place=place,num=num,height=height,form=form
  common comPage
  if ~(ISA(pos,/int) or ISA(pos,/float)) then pos=pos.pos ;else use given pos
  if ~KEYWORD_SET(place) then place=[1,1] ;right,top
  if ~KEYWORD_SET(scale) then scale=.75
  if ~KEYWORD_SET(form) then form='%-8.2f'
  ls=1.733/page.ysiz*page.xsiz;line spacing
  f=page.fsiz*scale

  ;Colorbars need to be sorted
  colors=KEYWORD_SET(colbar)?reverse(colorsIn[0:-2]):colorsIn
  names=KEYWORD_SET(colbar)?reverse(namesIn[0:-2]):namesIn
  n=KEYWORD_SET(num)?num:N_ELEMENTS(namesIn)-KEYWORD_SET(colbar)

  if KEYWORD_SET(num) then RescaleLevCol,namesIn,colorsIn,names,colors,num
  if KEYWORD_SET(colBar) then names=STRING(names,format=form)

  ;Making a Box on which to draw the legend
  w0=GetStrWidth(names,scale=scale)
  lw=w0+f*(4-KEYWORD_SET(colbar)) ;getting a good normalized width for the box
  bh=f*ls*n ;box height
  m=bh/(pos[3]-pos[1])*3  ;Box should not be taller than 1/3 of graph space
  if m gt 1 and KEYWORD_SET(height) then begin
    if KEYWORD_SET(colbar) then begin
      names=names[FIX([0:n:m])]
      colors=colors[FIX([0:n:m])]
    endif
    n=FIX(n/m+.5)
    bh=f*ls*n
    if KEYWORD_SET(colbar) then m=1
  endif else m=1

  ym=place[1]*pos[3]+(1-place[1])*(pos[1]+bh)
  xm=place[0]*pos[2]+(1-place[0])*(pos[0]+lw*m)
  xr = [xm-lw*m,xm]
  yr = [ym-bh,ym]
  MakeBox,xr,yr,thick=.0015,out=0 ;f/8 is a good thicknes

  THIS=0;@ Or 1, it controls the stretch of colorbar
  ;Finally making the actual content
  for i=0,m-1 do begin
    x=xm-lw*(m-1-i)
    for j=0,n-1 do begin
      t1=i*n+j
      y=(ym-ls*f*(j+.8))
      XYOUTS,(x-w0-f/2),y,names[t1],CHARSIZE=scale,/NORMAL ;put the word
      if ~KEYWORD_SET(colbar) $
        then MakeBox,[x-lw+f/2,x-w0-f],[y+f*.2,y+f*.4],col=colors[t1],thick=0 $ ;make the line
      else if j ge THIS then begin
        y1=ym-(bh/(n-THIS)*(j-THIS))
        y2=ym-(bh/(n-THIS)*(j-THIS+1))
        MakeBox,[x-lw+f/2.,x-w0-f],[y1,y2],col=colors[t1],thick=0
      endif
    endfor
  endfor
  RETURN,[xr[0],yr[0],xr[1],yr[1]]
END


;+
;Takes in data, and spits out levels and colors to use with contour, to be used with ct 39
;CALL:   LevCol,data,levs,cols,nlevels=nlevels,zero=zero,cont=cont,log=log
;
;INPUTS:
;data    [num] The data that these levels should correspond to
;nlevels int   The nubmer of levels used {8}
;zero    num   Adds this color at level {-.01}
;over    bool  Add an extra level and color for contour plots
;log     bool  Will try to adust levels to properly fit logarithmic data
;debug   bool  Spits out debug info {0}
;
;HOLDERS:
;levs    [num] The levels
;cols    [int] The colors
;-
Pro LevCol,data,levs,cols,nlevels=nlevels,zero=zero,over=over,log=log,debug=debug
  if ~KEYWORD_SET(NLEVELS) then nlevels=8
  s=MIN(data,max=m)
  levs=INTERPOL([s,m],NLEVELS) ;Get the levels
  if m eq s then goto, getcol
  if KEYWORD_SET(log) then begin
    fin=data[WHERE(FINITE(data),cnt)]
    logS=fin[SORT(fin)]
    if ISA(m,/int) then NLEVELS<=(m-s) ;if data is int-based, nlev limited by range
    nlevels+=1
    relev:
    nlevels-=1
    step=(cnt-1.)/(nlevels-1.)
    levs=logs[FINDGEN(nlevels)*step]
    ;levs=CONGRID(logS,NLEVELS,/CENTER) ;fancy
    for i=0,NLEVELS-2 do if levs[i+1] eq levs[i] then begin
      th=WHERE(logS gt levs[i],cnt)
      if cnt le (NLEVELS-i-1) then goto, relev
      a=CONGRID(logS[th],NLEVELS-1-i,/CENTER,/INTERP)
      levs[i+1:-1]=a
    endif
  endif
  getcol:
  cols=INTERPOL([1,254],NLEVELS-1);-1
  if KEYWORD_SET(over) then cols=[cols,255] ;Add a trailing white for contours
  if ISA(zero) then begin ;Add a special color at zero, if requested
    levs=[levs,-.001]
    cols=[cols,zero]
  endif
  ;Return sorted levels and colors
  ord=SORT(levs)
  cols=cols[ord]
  levs=levs[ord]
  if KEYWORD_SET(debug) then begin
    PRINT,"LevCol: levels:"+STRJOIN(STRING(format="%8.3g ",levs))
    PRINT,"LevCol: colors:"+STRJOIN(STRING(format="  %03i ",cols))
  endif
END

;+
;Sets the edges of a data set to the mean of those edges
;to help contour work without cellfill
;-
PRO EdgeMean,data,val=val
  if ~ISA(val) then begin
    val=FLTARR(4)
    val[0]=mean(data[0,*],/nan)
    val[1]=mean(data[*,0],/nan)
    val[2]=mean(data[-1,*],/nan)
    val[3]=mean(data[*,-1],/nan)
  endif
  if N_ELEMENTS(val) eq 1 then val=REPLICATE(val,4)
  data[*,-1]=val[3]
  data[-1,*]=val[2]
  data[*,0]=val[1]
  data[0,*]=val[0]
END


PRO TestSymSize
  common comPage
  page=PageDef('/home/ullrich/syd/plots/symSize',divs=[2,2],fsize=12)
  PRINT,'size',!D.X_CH_SIZE,!D.Y_CH_SIZE
  r=BYTE(RANDOMU(seed,50,50)*255)
  for xp=1,2 do for yp=1,2 do begin
    xs=6*xp^2
    ys=6*yp^2
    ttl=STRING(FORMAT='x:%i, y:%i',xs,ys)
    PRINT,ttl
    p=MakePlot(xr=[0,12],yr=[0,12],title=ttl)
    sz=[0,xs,ys]
    pagePx=(!P.CLIP[2:3]-!P.CLIP[0:1])
    sc=PagePx*2./!D.X_CH_SIZE/sz[1:2]
    PRINT,'sc',sc
    USERSYM, [0,1,1,0,0]*sc[0],[0,0,1,1,0]*sc[1],/fill
    s=!x.CRANGE[0]
    m=!x.CRANGE[1]-(!x.CRANGE[1]-s)/sz[1]
    x=FLOAT([0:sz[1]-1])/FLOAT(sz[1]-1)*(m-s)+s

    s=!y.CRANGE[0]
    m=!y.CRANGE[1]-(!y.CRANGE[1]-s)/sz[2]
    y=FLOAT([0:sz[2]-1])/FLOAT(sz[2]-1)*(m-s)+s
    for i=0,sz[1]-1 do for j=0,sz[2]-1 do PLOTS,x[i],y[j],col=r[i,j],psym=8
  endfor
  DEVICE,/CLOSE
END