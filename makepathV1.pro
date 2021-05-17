;+
;Puts strings together to form a proper name for a radar data file
;Setting the MAKE keyword will create a directory if one is not found
;modern keyword controls the use of C0 in file names
;-
Function MakePath,dir,year,month,day,rad,make=make,modern=modern
  s=PATH_SEP() ;/ for unix, \ for windows
  this_dir=s+'home'+s+'ullrich'+s+'syd'+s ;Where to save the output
  data_dir=STRING(format='%s%04i%s%02i%s',dir,year,s,month,s)
  filename=STRING(format='%s%04i%02i%02i',data_dir,year,month,day)
  if (KEYWORD_SET(make) && ~FILE_TEST(data_dir)) then begin
    SPAWN,'mkdir --parents '+data_dir
    PRINT,'made directory',data_dir
  endif
  if ~isa(modern) then begin
    modern=0
    if (year+month/12. GE 2006+7./12.) then modern=1
    if year ge 2019 then modern=2
  endif
  if keyword_set(make) then modern=1
  case modern of
    2:filename=filename+'*'+rad
    1:filename=filename+'.C0.'+rad ;do modern file names when making
    0:begin
      rs=strsplit('gbr,sch,kap,hal,sas,pgr,kod,sto,pyk,han,san,sys,sye,tig,ker,ksr,unw,wal',/extract,',')
      rc=strsplit('g,s,k,h,t,b,a,w,e,f,d,j,n,r,p,c,u,i',/extract,',')
      radCode=rc[WHERE(rs eq rad,cnt)]
      if cnt ne 1 then MESSAGE,'code table is sad'
      filename=data_dir+date+'00'+radcode+'C'
    end
  ENDCASE
  RETURN,filename+'.fit'
END