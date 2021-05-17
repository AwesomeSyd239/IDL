;+
;fit     = The fit structure to be filtered (assumes pointer form data)
;scans   = Retruns the number of scans (only filters scalar parameters)
;e       = Returns the number of echos (scans that also matched other filters)
;g       = Returns the number of ground scatters
;p       = Returns the number of echos
;New in V3
;Now each tag is a grabbable output
;velBounds=[10,2000]
;powBounds=[3,70]
;widBounds=[20,500]
;igatBounds=[0,75]
;bmnBounds=[0,15]
;frqBounds=[8e3,15e3]
;times=[0:24:.25]
;-
Function FilterArrs,fit, scans,e,g,v,p=p,cnt=cnt,$
  velBounds=velBounds, $
  powBounds=powBounds, $
  widBounds=widBounds, $
  gatBounds=gatBounds, $
  bmnBounds=bmnBounds, $
  frqBounds=frqBounds, $
  times=times,$
  verb=verb
  if not KEYWORD_SET(velBounds) then velBounds=[10,2000]
  if not KEYWORD_SET(powBounds) then powBounds=[3,70]
  if not KEYWORD_SET(widBounds) then widBounds=[20,500]
  if not KEYWORD_SET(gatBounds) then gatBounds=[0,75]
  if not KEYWORD_SET(bmnBounds) then bmnBounds=[0,15]
  if not KEYWORD_SET(frqBounds) then frqBounds=[8e3,15e3]
  if not KEYWORD_SET(times) then times=[0:24:.25]

  nb=bmnBounds[1]-bmnBounds[0]+1
  ng=gatBounds[1]-gatBounds[0]+1
  nt=N_ELEMENTS(times)-1
  scans=INTARR(nb,nt)
  e=INTARR(ng,nb,nt)
  g=INTARR(ng,nb,nt)
  v=FLTARR(ng,nb,nt)
  p=FLTARR(ng,nb,nt)

  ;Begin filtering
  if TOTAL(TAG_NAMES(fit) eq 'qflg') ne 0 then fit=fit[WHERE(fit.qflg eq 1)]
  f=WHERE(fit.tfreq le frqBounds[1] and fit.tfreq ge frqBounds[0],cnt)
  if KEYWORD_SET(verb) then PRINT,STRING(FORMAT="Frequency filter to %i points out of %i",cnt,N_ELEMENTS(fit))
  if cnt eq 0 then RETURN,0
  for beam=0,nb-1 do begin
    b=WHERE(fit[f].bmnum eq bmnBounds[0]+beam,cnt)
    if KEYWORD_SET(verb) then PRINT,STRING(FORMAT="Beamnum filter to %i points",cnt)
    if cnt le 1 then continue
    h=HISTOGRAM(fit[f[b]].t,BINSIZE=times[1]-times[0],MIN=times[0],MAX=times[-1])
    scans[beam,*]=h[0:nt-1]
    t_data=[0,TOTAL(h,/CUMULATIVE)]
    for time=0,nt-1 do begin
      if h[time] eq 0 then continue
      ;Gets all the data into an array
      for pt=t_data[time],t_data[time+1]-1 do begin
        if ~PTR_VALID(fit[b[pt]].slist) then continue
        gats=*fit[f[b[pt]]].slist
        pows=*fit[f[b[pt]]].p_l
        wids=*fit[f[b[pt]]].w_l
        vels=*fit[f[b[pt]]].v
        gflg=*fit[f[b[pt]]].gflg
        ;Does the data filter
        avel=ABS(vels)
        a=WHERE($
          avel gt velBounds[0] and avel le velBounds[1] and $
          pows gt powBounds[0] and pows le powBounds[1] and $
          wids gt widBounds[0] and wids le widBounds[1] and $
          gats ge gatBounds[0] and gats le gatBounds[1] and $
          gflg ne 1,cnt)

        if cnt ne 0 then begin
          e[gats[a]-gatBounds[0],beam,time]+=1 ;using ++ here causes fractional
          v[gats[a]-gatBounds[0],beam,time]+=vels[a]
          p[gats[a]-gatBounds[0],beam,time]+=pows[a]
        endif
        a3=WHERE(gats ge gatBounds[0] and gats le gatBounds[1] and gflg,cnt)
        if cnt gt 0 then g[gats[a3]-gatBounds[0],beam,time]+=1
      endfor
    endfor
  endfor

  RETURN,TOTAL(scans)
END