Function DegSym
  RETURN,'!Eo!N'
END

Function StrYears,years,u=u
  RETURN,STRTRIM(STRING(years[0]),2)+(keyword_set(u)?'_':'-')+STRMID(STRING(years[-1]),1,2,/REVERSE)
END

Function DateString,year=year,month=month,day=day,ut=ut,ltm=ltm,sep=sep
  if ~isa(sep) then sep='/'
  monthnames=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
  if ISA(month,/int) then mo=monthnames[month-1] else mo=month
  if isa(ut) then tm=ut else if isa(ltm) then tm=ltm
  pieces=[]
  foreach bit,LIST(year,mo,day,tm) do if isa(bit) then pieces=[pieces,STRTRIM(STRING(bit),2)]
  ;retstr=ISA(year)?STRING(FORM='%02i%s%s%s%i',day,sep,mo,sep,year):STRING(FORM='%02i%s%s',day,sep,mo)
  RETURN,STRJOIN(pieces,sep,/SINGLE);retstr
END

;+
;Give me a default string, and optionally an append bit prefaced with '+'
;I will append the
;-
Function OptString,base,ext
  if ~ISA(ext) then RETURN,base
  if STRCMP(ext,'+ ',2) then RETURN,base+STRMID(ext,1)
  if STRCMP(ext,'+',1) then RETURN,base+' '+STRMID(ext,1)
  RETURN,ext
END

;converts day of year into date string
Function doy2date,yr,doy
  numDays=[31,28,31,30,31,30,31,31,30,31,30,31]
  numDays[1]+=~(yr mod 4)
  monDays=[0,TOTAL(numdays,/cum)]
  out=STRARR(N_ELEMENTS(doy))
  for i=0,N_ELEMENTS(doy)-1 do begin
    mo=WHERE(monDays lt doy[i])
    dy=doy[i]-monDays[mo[-1]]
    out[i]=STRING(FORMAT='%02i%02i',mo[-1]+1,dy)
  endfor
  if yr lt 0 then RETURN,out
  RETURN,STRING(FORMAT='%4i',yr)+out
end

;+
;converts date string to day of year
;frac returns as % of year
;-
Function date2doy,date,yr=yr,mo=mo,dy=dy,frac=frac
  if N_ELEMENTS(date) eq 3 then begin
    yr=date[0]
    mo=date[1]
    dy=date[2]
  endif 
  numDays=[31,28,31,30,31,30,31,31,30,31,30,31]
  numDays[1]+=~(yr mod 4) ;valid for 1901-2099, nobody should use this code outside that range
  monDays=[0,TOTAL(numdays,/cum)]
  doy=monDays[mo-1]+dy-1
  if isa(frac) then doy/=mondays[-1]
  RETURN,doy
end