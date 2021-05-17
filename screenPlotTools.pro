PRO MOUSE_DRAW
  ScreenPlot
  ; Get the initial point in normalized coordinates:
  CURSOR, X, Y, /NORMAL, /DOWN
  ; Repeat until right button is pressed. Get the second point.
  ; Draw the line. Make the current second point be the new first.
  WHILE (!MOUSE.button NE 4) DO BEGIN
    CURSOR, X1, Y1, /NORM, /DOWN
    PLOTS,[X,X1], [Y,Y1], /NORMAL
    X = X1 & Y = Y1
  ENDWHILE
END

Function IsLeft,p0,p1,p2
RETURN,(p1[0]-p0[0])*(p2[1]-p0[1])-(p2[0]-p0[0])*(p1[1]-p0[1])
END

Function PntInPoly,pnt,ply
  wn = 0; the  winding number counter
  if pnt[0] lt 0 then pnt[0]+=360
  ply[*,0]=((ply[*,0]+360) mod 360)
  for i=0,N_ELEMENTS(ply[*,0])-2 do begin;loop through all edges of the polygon
    if (ply[i,1] lt pnt[1] and ply[i+1,1] gt pnt[1]) then begin;$;crosses upward ala Rule #1
      wn+=(isleft(ply[i,*],ply[i+1,*],pnt) gt 0);a valid up intersect right of P.x
    endif else if (ply[i,1] gt pnt[1] and ply[i+1,1] lt pnt[1]) then begin;$; crosses downward ala Rule  #2
      wn-=(isleft(ply[i,*],ply[i+1,*],pnt) lt 0); and (pnt[0] gt s))
    endif;a valid down intersect right of P.x
  endfor
  RETURN, wn;    // =0 <=> P is outside the polygon
END