pro append_tplot,names,timeonly=timeonly

if not keyword_set(names) then begin
  print,'no names'
  return
endif

if n_elements(names) eq 1 then begin
  temp=names
  names=strarr(1)
  names(0)=temp
endif


ptr=ptrarr(n_elements(names),/allocate_heap)
for loop=0,n_elements(names)-1,1 do begin
  get_data,names(loop),data=data,dlim=dlim,lim=lim
  *ptr(loop)=data
endfor

append_str=*ptr(0)
help,append_str,output=output
str_element_temp=strarr(n_elements(output)-1)
for loop2=1,n_elements(output)-1,1 do begin
  str_element_temp(loop2-1)=(strsplit(output(loop2),' ',/extract))(0)
endfor

for loop=1,n_elements(names)-1,1 do begin
  for loop2=0,n_elements(str_element_temp)-1,1 do begin
    str_element,append_str,str_element_temp(loop2),append_str_value
    str_element,(*ptr(loop)),str_element_temp(loop2),ptr_value

    append=1
    if keyword_set(timeonly) then begin
      if strlowcase(str_element_temp(loop2)) eq 'x' then timearraysize=n_elements(append_str_value)
      if strlowcase(str_element_temp(loop2)) ne 'x' and keyword_set(timearraysize) then begin
        if ndimen(ptr_value) eq 1 then begin
          if timearraysize ne n_elements(append_str_value) then append=0
        endif
        if ndimen(ptr_value) eq 2 then begin
          if timearraysize ne n_elements(append_str_value(*,0)) then append=0
        endif
        if ndimen(ptr_value) eq 3 then begin
          if timearraysize ne n_elements(append_str_value(*,0,0)) then append=0
        endif
      endif
    endif

    if append eq 1 then append_array,append_str_value,ptr_value
    str_element,append_str,str_element_temp(loop2),append_str_value,/add_replace
  endfor
endfor

store_data,names(0)+'_append',data=append_str,dlim=dlim,lim=lim

end

