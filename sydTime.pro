;+
;Set a date with yr/mo/dy
;-
PRO SetDate,yr=yr,mo=mo,dy=dy
  common SingleDate,year,month,day,currDate,prevDate
  if ~isa(currDate) then currDate={year:0,month:0,day:0};prevents next line from erroring on first call
  prevDate=currDate;stores last call, can be used to check if date changed 
  if isa(yr) then year=yr
  if isa(mo) then month=mo
  if isa(dy) then day=dy
  if ~isa(year) then year=0
  if ~isa(month) then month=0
  if ~isa(day) then day=0
  currDate={year:year,month:month,day:day}
END

PRO SetTime,ut=ut
ut=ut
END

;todo replace common block string with this function
Function ymdStr;i think this is the best way to do it
  common SingleDate,year,month,day
  return,STRING(FORM="%04i%02i%02i",year,month,day)
end

