;+
;Gets the type and compression status of a file, based on file name
;file    = file name
;type    = returns type (such as 'acf')
;<comp>  = returns non-0 if file is compressed (gz=1, bz2=2)
;new in V2:
;added bz2, and using string.endswith()
;-
Pro GetMeta,file,type,comp=comp
  comp=file.endswith('gz')+2*file.endswith('bz2')
  a=SIZE(comp)
  finfo=file_info(file)
  if a[0] ne 0 then l=REFORM(comp,[1,a[1:a[0]]]) else l=comp
  type=STRMID(file,(2+(2+L)*(L<1)),3,/REVERSE_OFFSET)
  bad=where((finfo.exists eq 0 or finfo.size eq 0) and not (strcmp(type,'ing') or strcmp(type,"")),bc)
  if bc ne 0 then begin
    print,"bad files"+strjoin(file[bad],", ")
    file[bad]='GetFile_Nothing'
    type[bad]='ing';as in the final 3 letters of 'missing'
  endif
  if n_elements(a) ne n_elements(size(type)) then message,'something wrong here'
END