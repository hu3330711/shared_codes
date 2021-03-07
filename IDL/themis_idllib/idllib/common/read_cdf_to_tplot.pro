pro read_cdf_to_tplot, fname, debug=debug,prefix=prefix

if not keyword_set(fname) then return
if not keyword_set(prefix) then prefix='read_cdf'

id = CDF_OPEN(fname)
inq=cdf_inquire(id)
if inq.nvars ne 0 then begin
   n=inq.nvars
endif else if inq.nzvars ne 0 then begin
   zvariable=1
   n=inq.nzvars
endif

varname=strarr(n)
for i=0L, n-1 do begin
   varinq=cdf_varinq(id,i,zvariable=zvariable)
   if keyword_set(debug) then print, varinq.name, varinq.datatype, format='(2a15)'
   cdf_control, id, get_var_info=info, variable=i,zvariable=zvariable
   cdf_varget, id, i, var, zvariable=zvariable, rec_count=info.maxrecs+1
   if (i ne 0 ) then dat1=create_struct(dat,varinq.name,var) else dat1=create_struct(varinq.name,var)
   dat=dat1
   varname(i)=varinq.name
endfor
dat=create_struct(dat1,'filename',fname)
if keyword_set(debug) then begin
  help, dat,/st
  name=strarr(100)
  for i=0,100 do begin cdf_attinq,id,i,name2,scope,max,max & name(i)=name2 & endfor
  value=strarr(100)
  for i=0,100 do begin cdf_attget,id,name(i),0,value2 & value(i)=value2 & endfor
  stop
endif
cdf_close, id

str_element,dat,'year',success=s
if s ne 0 then doy_to_month_date,dat.year,dat.day,month,day
if s ne 0 then unixtime=time_double(strtrim(string(long(dat.year)),1)+'-'+strtrim(string(long(month)),1)+'-'+strtrim(string(long(day)),1))+dat.msec*0.001
if s eq 0 then unixtime=0
for i=0L, n-1 do begin
  str_element,dat,varname(i),temp
  store_data,prefix+'_'+varname(i),data={x:reform(unixtime),y:reform(temp)}
endfor

end
