;+
;This is a modification of the IGRFCompute function to make it run in parallel for many lat/lons at once
;r should be in earth radii
;-
Function syd_igrf_compute,r,clats,lons,pv,dp
  common IGRF_v2_Com
  order=IGRF_nmx
  np=N_ELEMENTS(clats)
  ;;;;legendre probably?
  pv=DBLARR(IGRF_maxk,np)
  dp=DBLARR(IGRF_maxk,np)
  st= SIN(clats)
  pole=WHERE(ABS(st) lt 1e-15,cnt)
  if cnt ne 0 then clats[pole] += 1e-15*(1-2*(st[pole] lt 0))
  st=SIN(clats)
  ct=COS(clats)

  pv[0,*]  = 1.;   /* 0,0 */
  dp[0,*] = 0.;   /* 0,0 */
  pv[2,*]  =  ct; /* 1,0 */
  dp[2,*] = -st; /* 1,0 */
  ;* compute values of P^{l,l} and dP^{l,l}/dtheta */
  for l=1, order do begin
    k = l * (l+1) + l;    /* l = m */
    n = (l-1) * l + l-1;  /* l-1 = m-l, i.e., previous l=m */
    a = 2*l-1;
    pv[k,*]  = a*pv[n,*]*st;
    dp[k,*] = a*(dp[n,*]*st + pv[n,*]*ct);
  endfor

  ;/* compute values of P^{l,m} and dP^{l,m}/dtheta */
  for l=2, order do begin
    for m=0, l-1 do begin
      k = l * (l+1) + m;        /* l,m */
      n = (l-1) * l + m;        /* l-1,m */
      p = (l-2) * (l-1) + m;    /* l-2,m */
      ;/* numerical recipies in C */
      a = 2*l-1;
      if (m eq l-1) then begin
        pv[k,*]  = a*ct*pv[n,*];
        dp[k,*] = a*(ct*dp[n,*] - st*pv[n,*]);
      endif else begin
        b = l+m-1;
        pv[k,*] = (a*ct*pv[n,*] - b*pv[p,*])/(l-m);
        dp[k,*] = (a*(ct*dp[n,*] - st*pv[n,*]) - b*dp[p,*])/(l-m);
      endelse
    endfor
  endfor
  ;;;;;
  ;done legendre

  ;* we want RE/r */
  afac = 1.d/r^2.    ;* r is in units of RE to be consistent with geopack, */aor*aor
  w=IGRF_order+1
  ;* array of trig functions in phi for faster computation */
  if size(lons,/n_dim) eq 1 then begin ;if lon array came in sideways
    cosm_arr = COS(REBIN(DINDGEN(w),[w,np])*REBIN(TRANSPOSE(lons),[w,np]))
    sinm_arr = SIN(REBIN(DINDGEN(w),[w,np])*REBIN(TRANSPOSE(lons),[w,np]))
  endif else begin
    cosm_arr = COS(REBIN(DINDGEN(w),[w,np])*REBIN(lons,[w,np]))
    sinm_arr = SIN(REBIN(DINDGEN(w),[w,np])*REBIN(lons,[w,np]))
  endelse

  brtp = DBLARR(3,np)

  for l=1, IGRF_nmx do begin  ; no l = 0 term in IGRF
    tbrtp = DBLARR(3,np)
    for m=0, l do begin
      k = l*(l+1) + m;  /* g */
      n = l*(l+1) - m;  /* h */

      tbrtp[0,*]+=(IGRF_coefs[k]*cosm_arr[m,*] + IGRF_coefs[n]*sinm_arr[m,*]) *  pv[k,*];
      tbrtp[1,*]+=(IGRF_coefs[k]*cosm_arr[m,*] + IGRF_coefs[n]*sinm_arr[m,*]) *  dp[k,*];
      tbrtp[2,*]+=(-IGRF_coefs[k]*sinm_arr[m,*] + IGRF_coefs[n]*cosm_arr[m,*]) * m*pv[k,*];
    endfor
    afac/=r
    brtp[0,*] += afac*(l+1)*tbrtp[0,*]
    brtp[1,*] -= afac*tbrtp[1,*]
    brtp[2,*] -= afac*tbrtp[2,*]
  endfor

  brtp[2,*] /= st;
  RETURN, brtp
end
