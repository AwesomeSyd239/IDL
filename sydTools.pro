;+
;Functions in this file:
;ToBase
;Flatten
;StrYears
;Normalize
;Unique
;WhereBlocks
;WhereNear
;ZeroNear
;TrimMulti
;NUDFT
;RSFT
;Sinc
;WhereMulti
;Autocorrelate
;SpaceSmooth
;Sharpen
;ChunkSum
;Collapse
;CleanUp
;sydCorrelate
;weightCorrelate
;UniqueStrings
;SetIntersection
;Envelope
;LevelFind
;-
pro sydprof,st=st,out=out
if ~isa(out) then begin
  st=SYSTIME(seconds=1)
  PROFILER,/reset
  PROFILER
  PROFILER,/system
  endif else begin
  PROFILER,/report,out
  PRINT,'done in',SYSTIME(seconds=1)-st
  endelse
  end
;+
;Converts a number to another base, with optional padding
;CALL:   ToBase,numIn,inBase=inBase,base=base,pad=pad
;INPUTS:
;numIn   num  the number to be converted
;inBase  int  the base of the input {10}
;base    int  converts to this base {16}
;pad     int  0-pads the left to match the given width
;RETURNS:
;out     str  the converted number
;-
Function ToBase,numIn,inBase=inBase,base=base,pad=pad
  if ~KEYWORD_SET(pad) then pad=0
  if ~KEYWORD_SET(inBase) then inBase=10
  if ~KEYWORD_SET(base) then base=16

  numStrIn=STRTRIM(STRING(numIn),2)
  dec=STRPOS(numStrIn,'.')
  lenIn=STRLEN(numStrIn)-(dec>0)
  lenOut=CEIL(lenIn*ALOG(inBase)/ALOG(base))
  out=REPLICATE('0',lenOut>pad)
  ind=[0:lenIn-1]
  if dec ne -1 then ind=[ind,[lenIn+1:STRLEN(numStrIn)-1]]
  x=BYTE(STRMID(STRUPCASE(numstrin),ind,1)) ;gets the bits
  x-=(48*(x le 59)+55*(x ge 60))
  num=TOTAL(x*FLOAT(inBase)^(lenIn-[1:N_ELEMENTS(x)]))
  inum=FIX(num) ;integer part
  fnum=(num-inum)*2
  i=0
  while inum gt 0 do begin
    bit=inum mod base
    out[i]=STRING(FORMAT='%-i',(bit gt 9 ? BYTE(87+bit):bit))
    inum=LONG(inum/base)
    i+=1
  endwhile
  iPart=STRJOIN(reverse(out[0:i>pad-1]))
  f=0
  fOut=REPLICATE('0',5)
  while fnum ne 0 and f lt 5 do begin
    bit=FIX(fnum)
    fOut[f]=STRING(FORMAT='%-i',(bit gt 9 ? BYTE(87+bit):bit))
    fnum=(fnum-bit)*base
    f+=1
  endwhile
  fPart=STRJOIN(fOut[0:4])
  RETURN,STRJOIN([ipart,fpart],'.')
END

Function ToBits,num,pad=pad
  if ~ISA(pad) then pad=0
  if ~ISA(num,/arr) then begin
    nb=CEIL(ALOG2(num))
    bits=BOOLARR(nb>pad)
    for i=0,nb-1 do bits[i]=(ISHFT(num,-i) mod 2)
  endif else begin
    nb=n_elements(num)
    bits=[num,replicate(0,pad-nb)]
  endelse
  RETURN,bits
END

;Returns a 1d version of the array
Function Flatten,arr
  RETURN,REFORM(arr,N_ELEMENTS(arr))
END



;+
;Normalizes data in the input array
;CALL:   Normalize,data,s=s,m=m
;INPUTS:
;data    [num]
;s       num   If set, use this instead of the normal minimum
;m       num   If set, use this instead of the normal maximum
;-
Function Normalize,data,s=s,m=m,nan=nan
  if ~KEYWORD_SET(m) then m=1
  if ~KEYWORD_SET(s) then s=0
  md=MAX(data,min=sd,nan=nan)

  if KEYWORD_SET(nan) then begin
    n=WHERE(~FINITE(data),cnt)
    if cnt ne 0 then data[n]=md
  endif
  ;if ~ISA(s) then s=ISA(s2)?s2:MIN(data)
  rescale=FLOAT(data-sd)/FLOAT(md-sd)
  RETURN,rescale*(m-s)+s
END


;+
;Returns unique values from an array
;CALL:   Unique,data,dim=dim,ind=ind
;
;INPUTS:
;data    [num] The data to be unique-ified
;dim     int   For multidimensional arrays, see example {1}
;
;HOLDERS:
;ind     [int] Indices that the uniques were found at
;
;RETURNS:
;uniques [num] an array of the unique elements of the data, in order
;-
Function Unique,dataIn,dim=dim,ind=ind
  if not KEYWORD_SET(ind) then ind=0
  s=SIZE(dataIn)
  ndim=s[0] & nelm=s[-1] ;Dimensional properties
  if KEYWORD_SET(dim) then begin ;find dimension ordering
    if dim gt 0 then dimord=[dim,[0:dim-1]] else PRINT,"Negative dimensions not allowed"
    if dim lt ndim-1 then dimord=[dimord,[dim:ndim-1]]
    data=TRANSPOSE(dataIn,dimord)
  endif else data=dataIn

  wid=ISA(dim)?nelm/s[ndim-dim]:1 ;Width of data
  slice=data[0:wid-1]
  uniques=REBIN(slice,wid,nelm/wid)
  k=wid ;index for uniques array
  for i=0,nelm-wid,wid do begin
    slice=data[i:i+wid-1] ;Get a slice of data
    for j=0,k-wid,wid do if ARRAY_EQUAL(slice,uniques[j:j+wid-1]) then break ;Compare it to each chunk of unique array, until we find a match
    if j gt k-wid then begin   ;If we did not find a match
      uniques[k:k+wid-1]=slice ;then add our slice as the next unique
      k+=wid                   ;and update the index for uniques
    endif
  endfor
  RETURN,REFORM(uniques[0:k-1],wid,k/wid)
END

;+
;Gets chunks of where bool is unchanging
;returns in a 2xN array as [start,end] pairs
;-
FUNCTION WhereBlocks,bool
  if bool[0] then $
    true=[-1,WHERE(bool ne SHIFT(bool,-1))] else $
    true=[WHERE(bool ne SHIFT(bool,-1))]
  true+=1
  if (N_ELEMENTS(true) mod 2) then true=[true,N_ELEMENTS(bool)-1]
  RETURN,REFORM(true<(N_ELEMENTS(bool)-1),2,N_ELEMENTS(true)/2)
END


;+
;Like where, but with approximate matching
;-
Function WhereNear,data,val,cnt=cnt
  i=WHERE(data eq val)
  if i ge 0 then RETURN,i
  i=WHERE(((data ge val) eq (SHIFT(data,1) lt val)) and FINITE(data),cnt)
  if N_ELEMENTS(i) eq 1 then RETURN,i-.5
  if i[0] eq 0 then RETURN,i[1:*]-.5
  RETURN,i-.5
END


;+
;Gets the index (nearest to ind) at which data is 0
;-
Function ZeroNear,data,ind,x=x
  a=whereNear(data,0,cnt=cnt)
  if cnt eq 0 then RETURN,-1
  if ISA(x) then dist=x[a]-x[ind] else dist=a-ind
  b=MIN(dist,i2,/abs)
  RETURN,a[i2]
END


;+
;Trims all input arrays to [0:inds]
;-
PRO TrimMulti,inds,a,b,c,d,e,f,g,h
  if N_ELEMENTS(inds) eq 1 then i2=[0:inds] else i2=inds
  a=a[i2]
  if ISA(b) then b=b[i2]
  if ISA(c) then c=c[i2]
  if ISA(d) then d=d[i2]
  if ISA(e) then e=e[i2]
  if ISA(f) then f=f[i2]
  if ISA(g) then g=g[i2]
  if ISA(h) then h=h[i2]
END


;+
;Non-uniform discrete fourier transform
;CALL:   NUDFT,t,x,wbins
;
;INPUTS:
;t       [num] sample times
;x       [num] sampled data
;wbin    [num] output frequencies
;d       +/-1  forward or backward
;
;RETURNS:
;P       [cpx] Complex amounts of input frequencies
;-
Function NUDFT,t,x,wbins,d=d
  if ~ISA(d) then d=-1
  m=N_ELEMENTS(wbins)
  n=N_ELEMENTS(x)
  if d eq 1 then y=CONJ(x) else y=x
  nt=Normalize(t) ;@ is this correct?
  P=COMPLEXARR(m)
  ex=MATRIX_MULTIPLY(wbins,nt)
  thing=EXP(COMPLEX(0,d*2*!CONST.PI*ex))
  ;top and left of 'thing' should be 1
  for i=0,m-1 do P[i]=TOTAL(y*thing[i,*])
  if (d eq -1) then RETURN,P/n else RETURN,P
END


;+
;Test of normal (non-fast) fourier transform
;-
Function Fourier,x,bins
  num=N_ELEMENTS(bins)
  kLim=N_ELEMENTS(x)
  out=COMPLEXARR(num)
  for n=0,num-1 do begin
    for k=0,kLim-1 do out[n]+=x[k]*EXP(COMPLEX(0,-2*!CONST.PI*n*k/kLim))
  endfor
  RETURN,out/kLim
END


;+
;Rolling Short Fourier Transform
;uses x(t) as sampled data
;returns w(t)=FT(x[t:t+window])
;-
Function RSFT,t,x,twidth,d=d
  dt=t[1]-t[0]
  maxIn=CEIL(twidth/dt)+1
  out=COMPLEXARR([N_ELEMENTS(t),maxIn])
  foreach time,t,i do begin
    relevant=WHERE(ABS(t-time) le twidth/2.,cnt)
    out[i,0:cnt-1]=FFT(x[relevant])
  endforeach
  RETURN,out
END


Function Sinc,x,c=c
  sing=WHERE(x eq 0,cnt)
  ret=(KEYWORD_SET(c)?(COS(x)-1.):SIN(x))/x
  if cnt ne 0 then ret[sing]=1-KEYWORD_SET(c)
  RETURN,ret
END

;+
;Like where(), but reserves indices
;Returns a multi-dimensional index array
;-
Function WhereMulti,test,count=count
  if not KEYWORD_SET(count) $
    then a=WHERE(test) $
  else a=WHERE(test,count)
  this=BOOLARR(SIZE(test,/dim))
  this[a]=1
  RETURN,this
END


;+
;Autocorrelation
;CALL:   AutoCorrelate,data
;
;INPUTS:
;data    [num] the data to be autocorrelated
;
;RETURNS:
;coe     [flt] autocorrelation
Function AutoCorrelate,data
  n=N_ELEMENTS(data)
  coe=FLTARR(n)
  for i=0,n-1 do coe[i]=TOTAL(data*SHIFT(data,i),/nan)
  RETURN,coe
END


;+
;Uses average/boxcar smoothing on data, can be recursive
;CALL:   SpaceSmooth,x=x,data,dis,rec=rec
;
;INPUTS:
;x       [num] indices of data to use in smoothing
;data    [num] the data to be smoothed
;dis     num   width of boxcar, measured along x, smooths abs(x-x[i]) lt dis/2.
;shape   [num] Allows weighted smoothing {INTARR(dis)+1}
;rec     int   number of recursions {0}
;atLeast int   if less than this many data points are avaliable, set NAN
;                assumes that values<1 are supposed to be as a fraction of dis
;
;RETURNS:
;smth    [flt] the smoothed data
;-
Function SpaceSmooth,x=x,data,dis,shapeIn=shapeIn,rec=rec,atLeast=atLeast,sd=sd
  if ~ISA(rec) then rec=0
  nd=N_ELEMENTS(data)
  if ~ISA(shapeIn) then shape=INTARR(nd)+1 else shape=shapeIn
  ;if a custom x is not provided, use normal indices
  if ~KEYWORD_SET(x) then x=[0:nd-1] else if N_ELEMENTS(x) ne nd then PRINT,"dimension mismatch"
  if ~KEYWORD_SET(atLeast) then atLeast=0 else if atLeast lt 1 then atLeast*=dis
  smth=FLTARR(nd)
  fin=FINITE(data)
  hd=dis/2.
  if N_ELEMENTS(shape) eq nd then begin;smoothing data is weighted, not shaped
    newDat=data*shape
    for i=0,nd-1 do begin
      a=WHERE(ABS(x-x[i]) le hd and fin,cnt)
      smth[i]=(cnt gt atLeast ? TOTAL(newDat[a])/TOTAL(shape[a]) : !Values.D_NAN)
    endfor
  endif else for i=0,nd-1 do begin
    a=WHERE(ABS(x-x[i]) le hd and fin,cnt)
    if KEYWORD_SET(sd) then shape=congrid(shape,cnt,/interp,/center);might be laggy
    smth[i]=(cnt gt atLeast ? TOTAL(data[a]*shape[a-i+hd])/TOTAL(shape[a-i+hd]) : !Values.D_NAN)
  endfor
  if rec gt 1 then smth=SpaceSmooth(x=x,smth,dis,rec=rec-1) ;Recur
  RETURN,smth
END

;+
;Sharpens by derivative
;-
Function Sharpen,data,rec=rec
  shrp=(4*data-SHIFT(data,1)-SHIFT(data,-1))/2.
  if KEYWORD_SET(rec) then shrp=Sharpen(shrp,rec=rec-1)
  RETURN,shrp
END

;+
;Adds data in chunks, useful for converting daily data into monthly
;it works sort of like collapse
;
;INPUTS:
;dataIn   [num] The data
;weights  [num] Data weights
;chunkSize[num] is how many to group together
;newsize  [num] is the size of output array
;offset    num  in case you don't want to start at the first element {0}
;avg      bool  takes the average, instead of the sum
;
;HOLDERS:
;tW      if set, keeps the new weights of each chunksummed data
;
;RETURNS:
;new     [flt] The freshly chunked data
;-
Function ChunkSum,dataIn,weights=weights,chunkSize=chunkSize,newSize=newSize,$
  offset=offset,avg=avg,tW=tW
  data=dataIn
  nd=N_ELEMENTS(data)
  oldSize=SIZE(data,/dim)
  if ~ISA(chunkSize) and ~ISA(newSize) then MESSAGE,'We need at least one size'
  if ~KEYWORD_SET(newSize) then $
    newSize=oldSize/FLOAT(chunkSize) else $
    chunkSize=oldSize/FLOAT(newSize)
  new=FLTARR(newSize)
  tw=INTARR(newSize)
  over=nd mod chunkSize
  if ISA(offset,/float) then offset=over*offset else offset=0
  chunks=[offset:nd:chunkSize]

  if ISA(weights) then data=data*weights
  for i=0,N_ELEMENTS(chunks)-2 do begin
    if ~KEYWORD_SET(avg) then div=1 else begin
      if ISA(weights) then $
        toCount=weights[chunks[i]:chunks[i+1]-1] else $
        toCount=FINITE(data[chunks[i]:chunks[i+1]-1])
      tw[i]=TOTAL(toCount,/NAN)
    endelse
    new[i]=TOTAL(data[chunks[i]:chunks[i+1]-1],/nan)/FLOAT(tw[i])
  endfor
  RETURN, new
END


;+
;Collapses an array along specified named dimensions
;CALL:   Collapse,arr,arrForm,alongs,avg
;
;INPUTS:
;arr     [num] Array to be collapsed
;arrForm [str] Names of the array's dimensions
;alongs  [str] Names of the dimensions you want to collapse
;avg     bool  Return the average instead of the sum
;
;OUTPUTS:
;new     [num] Collapsed array
;-
Function Collapse,arr,arrForm,alongs,avg=avg
  new=arr
  sz=SIZE(arr)
  if sz[0] ne N_ELEMENTS(arrForm) $
    then PRINT,"Collapse array does not match expected form" $
  else for dim=N_ELEMENTS(arrForm),1,-1 do begin
    if TOTAL(STRMATCH(alongs,arrForm[dim-1])) then begin
      div=KEYWORD_SET(avg)?TOTAL(FINITE(new),dim):1
      new=TOTAL(new,dim,/NAN)/div
    endif
  endfor
  RETURN,new
END


;+
;Data should already be index aligned, this will throw out any strays and sort the data
;CALL:   CleanUp,xd,yd,nxd,nyd,zeros=zeros,noSort=noSort
;
;INPUTS:
;?d      [num] x,y Data to clean
;zeros   str   what to do with 0s in the data {'ignore'}
;
;HOLDERS:
;n?d     [num] x,y Cleaned data
;-
Pro CleanUp,xd,yd,nxd,nyd,zeros=zeros,noSort=noSort
  if ~KEYWORD_SET(zeros) then zeros='ignore'
  case zeros of
    'remove':inds=WHERE(FINITE(xd) and FINITE(yd) and xd ne 0 and yd ne 0,cnt)
    'ignore':inds=WHERE(FINITE(xd) and FINITE(yd),cnt)
    'around':PRINT,'implement this'
  endcase
  nxd=0 & nyd=0
  if cnt gt 0 then begin
    nxd=xd[inds]
    nyd=yd[inds]
  endif else begin
    nxd=!VALUES.F_NAN
    nyd=!VALUES.F_NAN
  endelse
  if ~KEYWORD_SET(noSort) then begin
    nyd=nyd[SORT(nxd)]
    nxd=nxd[SORT(nxd)]
  endif
END

;+
;Quantiles,x,nquant,noMin=noMin,noMax=noMax
;x      [num] the data
;nquant int   number of quantiles
;no?    bool  min,max discludes the highest and lowest values
;-
Function Quantiles,x,nquant,noMin=noMin,noMax=noMax
  a=x[WHERE(FINITE(x))]
  a=a[SORT(a)]
  nx=FLOAT(TOTAL(FINITE(x)))-1.
  step=nx/nquant
  ;q=[0:nx+.1:(nx-1.)/(nquant-1.)]
  q=[0:nx:step]
  RETURN,a[q[KEYWORD_SET(noMin):-1-KEYWORD_SET(noMax)]]
END


;+
;
;CALL:   SydCorrelate,ixd,iyd,xo,yo,gm=gm,ga=ga,sm=sm,sa=sa,pts=pts,debug=debug
;
;INPUTS:
;i?d     [num] x,y Inputs
;gm      str   Group mode
;  xhist       Uses histograms with binsize of ga
;  sethist     Bins ga data points together, taking the average
;
;ga      num   Grouping argument
;sm      str   Smooth mode
;sa      str   Smoothing argument, doubles as lower bound on num pts
;
;HOLDERS:
;?o      [flt] x,y Outputs
;pts     num   return # of points in each xo,yo pair
;-
Pro SydCorrelate,ixd,iyd,xo,yo,gm=gm,ga=ga,sm=sm,sa=sa,pts=pts,debug=debug
  xo=0 & yo=0
  if ~KEYWORD_SET(debug) then ON_ERROR,2
  CleanUp,ixd,iyd,xd,yd,zeros='ignore'
  if ~KEYWORD_SET(sa) then MESSAGE
  nx=N_ELEMENTS(xd)
  if nx eq 1 or nx lt sa then begin
    if KEYWORD_SET(debug) then PRINT,"insufficient data"
    inds=[0,nx-1]
    pts=nx
  endif else begin
    case gm of
      'xhist':begin
        xs=MIN(xd,max=xm)
        hx=[0,HISTOGRAM(xd,BINSIZE=ga,min=xs,max=xm)]
        indp=ROUND(UNIQUE(TOTAL(hx,/CUMULATIVE)))
        pts=hx[WHERE(hx ne 0)] ;normal bins
        ;throw out indices that will not have enoguh data
        inds=UNIQUE([0,indp[WHERE(pts gt sa)],indp[-1]])
        ;number of pts in bins for new indices
        pts=inds[1:*]-inds[0:-2]
      end
      'sethist':begin
        inds=[0:nx+ga-1:ga<nx]
        inds[-1]=nx
        pts=inds[1:*]-[inds]
      end
      'none':inds=INDGEN(nx)
    endcase
  endelse

  xo=FLTARR(N_ELEMENTS(inds)-1)
  yo=FLTARR(N_ELEMENTS(inds)-1)
  if sm eq 'median' then for i=0,N_ELEMENTS(inds)-2 do begin
    xo[i]=MEDIAN(xd[inds[i]:inds[i+1]-1],/EVEN)
    yo[i]=MEDIAN(yd[inds[i]:inds[i+1]-1],/EVEN)
  endfor

  if sm eq 'average' then for i=0,N_ELEMENTS(inds)-2 do begin
    xo[i]=TOTAL(xd[inds[i]:inds[i+1]-1])/(inds[i+1]-inds[i])
    yo[i]=TOTAL(yd[inds[i]:inds[i+1]-1])/(inds[i+1]-inds[i])
  endfor

  if sm eq 'roll' then begin
    tx=FLTARR(nx)
    ty=tx
    for i=0,nx-1 do begin
      t=[0>(i-sa/2):(i+sa/2)<nx]
      tx[i]=TOTAL(xd[t])/N_ELEMENTS(t)
      ty[i]=TOTAL(yd[t])/N_ELEMENTS(t)
    endfor
    xo=tx[inds]
    yo=ty[inds]
  endif
END


;+
;Allows weights to go into built-in correlate function
;CALL:   WeightCorrelate,w,x,y,newx,newy
;
;INPUTS:
;w       [num] Weights of each x,y pair
;?       [num] x,y The two arrays to correlate together
;
;HOLDERS:
;new?    [flt] x,y The 'weighted' arrays
;-
Function WeightCorrelate,w,x,y,newx,newy
  nw=N_ELEMENTS(w)
  nx=N_ELEMENTS(x)
  ny=N_ELEMENTS(y)
  if ~(nx eq ny) then MESSAGE,'Covariate dimensions must agree'
  if nx lt 2 then RETURN,!values.F_NAN ;don't allow
  a=WHERE(w ne 0,cnt)
  tw=[0,TOTAL(w,/CUMULATIVE)]
  if nw gt nx then begin
    if cnt ne nx then begin
      PRINT,'Bad dimensions'
      RETURN,!values.F_NAN
    endif else begin
      nw=cnt
      x=x[a]
      y=y[a]
    endelse
  endif
  newx=FLTARR(tw[-1]) & newy=FLTARR(tw[-1])
  ;construct weighted arrays by duplicating elements
  for i=0,nw-1 do if tw[i] lt tw[i+1] then begin
    newx[tw[i]:tw[i+1]-1]=x[i]
    newy[tw[i]:tw[i+1]-1]=y[i]
  endif
  RETURN,CORRELATE(newx,newy)
END

;+
;Looks for correlation between two signals
;on a short timescale. if a spike in x leads to a spike in y
;try to ignore background levels
;-
Function ShortCorrelate,x,y,len
END


;+
;Finds and returns unique strings in a list
;-
Function UniqueStrings,strlist
  strings=[strlist[0],strlist[1:*]];make a non-destructive copy
  unq=STRARR(N_ELEMENTS(strings))
  uc=0
  while N_ELEMENTS(strings) gt 0 do begin
    unq(uc)=strings[0]
    a=WHERE(strings ne unq(uc),cnt)
    if cnt gt 0 then strings=strings[a] else break
    uc+=1
  endwhile
  RETURN,unq[0:uc]
END


;Find the intersection of the ranges.
FUNCTION SetIntersection, a, b
  mina = MIN(a, Max=maxa)
  minb = MIN(b, Max=maxb)
  minab = mina > minb
  maxab = maxa < maxb

  ; If the set ranges don't intersect, then result = NULL.
  IF (maxa LT minab AND minb GT maxab) OR (maxb LT minab AND mina GT maxab) THEN RETURN,-1

  r=WHERE((HISTOGRAM(a,Min=minab,Max=maxab) NE 0) AND (HISTOGRAM(b,Min=minab,Max=maxab) NE 0),count)
  IF count EQ 0 THEN RETURN, -1 ELSE RETURN, r + minab
END

Function DecRound,num,digits
  f=10.^digits
  RETURN,ROUND(num*f)/fix(f)
END

Function SigRound,num,sig
exp=(10^fix(alog10(num)))
kept=ROUND(num/exp*10^(sig-1))
RETURN,kept*exp/10
END

;+
;Gets the envelope for data, using a sliding window
;CALL:   Envelope,data,winSize,s,m
;
;INPUTS:
;data    [num] the data to make an envelope of
;winSize int   width of the window
;
;HOLDERS:
;s       [flt] lower bound envelope
;m       [flt] upper bound envelope
;-
Pro Envelope,data,winSize,s,m
  n=N_ELEMENTS(data)-1
  ws2=FIX(winSize/2)-1
  m=data & s=data
  window=[0:winSize]
  for i=0,n-winSize do begin
    s[i:i+ws2]=MIN(data[window+i],max=q)
    m[i:i+ws2]=q
  endfor
  s=SHIFT(s,ws2)
  m=SHIFT(m,ws2)
  ;fill in the edges
  s[0:ws2]=MIN(data[0:ws2],max=q)
  m[0:ws2]=q
  s[n-ws2:n]=MIN(data[n-ws2:n],max=q)
  m[n-ws2:n]=q
END


;+
;Finds a level in the (windowed) data, 0.5=median
;CALL:   LevelFind,x,data,winSize,lev
;
;INPUTS:
;data    [num] the data to make an envelope of
;winSize int   width of the window
;
;RETRUNS:
;new     [num] the thing we want
;-
Function LevelFind,x,data,winSize,lev
  n=N_ELEMENTS(data)
  if N_ELEMENTS(x) ne n then MESSAGE,'mismatch'

  dord=SORT(data)
  newdat=data[dord]

  sx=HISTOGRAM(x[dord],min=0,max=24,binsize=winSize,REVERSE_INDICES=R)
  new=FLTARR([N_ELEMENTS(sx)-1,N_ELEMENTS(lev)])
  for i=0,N_ELEMENTS(sx)-1 do begin
    if sx[i] eq 0 then continue
    inds=R[R[i] : R[i+1]-1]
    new[i,*]=newdat[inds[sx[i]*lev]]
  endfor
  RETURN,new
END