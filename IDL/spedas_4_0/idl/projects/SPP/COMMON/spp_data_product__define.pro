;+
;  spp_data_product
;  This basic object is the entry point for defining and obtaining all data for all data products
; $LastChangedBy: davin-mac $
; $LastChangedDate: 2020-04-15 17:18:26 -0700 (Wed, 15 Apr 2020) $
; $LastChangedRevision: 28586 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_4_0/projects/SPP/COMMON/spp_data_product__define.pro $
;-
;COMPILE_OPT IDL2


FUNCTION spp_data_product::Init,_EXTRA=ex,data,filename=filename,name=name
  COMPILE_OPT IDL2
  ; Call our superclass Initialization method.
  void = self->generic_object::Init()
;  printdat,ex
;  self.data = dynamicarray(name=self.name)
  self.dict = dictionary()
  if keyword_set(filename) then begin
    restore,file=filename,/verbose
  endif
  if keyword_set(data) then self.data_ptr = ptr_new(data)
  if keyword_set(name) then self.name = name
  if keyword_set(dict) then self.dict = dict
  self.created = time_string(systime(1))
  if  keyword_set(ex) then dprint,ex,phelp=2,dlevel=self.dlevel
  IF (ISA(ex)) THEN self->SetProperty, _EXTRA=ex
  RETURN, 1
END


pro spp_data_product::savedat,data,add_index=add_index,no_copy=no_copy
  if ~ptr_valid(self.data_ptr) then self.data_ptr = ptr_new(data) else *self.data_ptr = data
  if keyword_set(add_index) && isa(data,'struct') then begin
    str_element,*self.data_ptr,'index',index
    if ~isa(index) then   str_element,/add,*self.data_ptr,'index',lindgen(n_elements(*self.data_ptr) )
  endif
end


pro spp_data_product::savefile,filename=filename
   if ~keyword_set(filename) then filename = self.name+'.sav'
   data = *self.data_ptr
   name = self.name
   dict = self.dict
   save,file=filename,/verbose,data,name,dict
   dprint,'Saved data in file: '+filename,dlevel=1
end


pro spp_data_product::make_tplot_var,tagnames,prefix=prefix
  if ptr_valid(self.data_ptr) then begin
    if ~keyword_set(tagnames) then begin
      print, 'Here are your options:'
      print,(tag_names(*self.data_ptr))
      return
    endif
    if ~isa(prefix,/string) then prefix = self.name+'_'
    store_data,prefix,data= *self.data_ptr,tagnames=strupcase(tagnames)
  endif
end


pro spp_data_product::add_var,var,varname=varname
;  obj = spp_data_product_hash(vname)
  ptr = self.data_ptr
  if isa(var,/string) then begin   ; var is assumed to be a tplot variable name
    if ~keyword_set(varname) then  varname=var
    dprint,dlevel=2,'Interpolating tplot variable: '+var+' into data structure: '+self.name+'  ('+varname+')'
    dat0 = (*ptr)[0]
    t = (*ptr).time
    vardat = data_cut(var,t)    ; interpolate values of tplot variable
    if ~keyword_set(vardat) then begin
      dprint,'No data available for: ',var
    endif
    ndim = size(/n_dimen,vardat)
    shftdim = shift(indgen(ndim),-1)
    vardat = transpose(vardat,shftdim)
    str_element,/add,*ptr,varname,vardat
    self.dict[varname] = var
  endif else begin
    dim = dimen(var)
    rdim = reverse(dim)
    if rdim[0] eq n_elements(*ptr) then begin
      str_element,/add,*ptr,varname,var
    endif else dprint,'size error'
  endelse
  

end


function spp_data_product::getdat,trange=trange,index=index,nsamples=nsamples,valname=valname,verbose=verbose,extrapolate=extrapolate,cursor=cursor
  if ~ptr_valid(self.data_ptr) then begin
    dprint,'No data loaded for: ',self.name
    return,!null
  endif
 ; verbose = 3
  ns = n_elements(*self.data_ptr)
  if keyword_set(cursor) then begin
    ctime,trange,npoints=1,/silent
  endif
  
  if isa(trange) then begin
    index = interp(lindgen(ns),(*self.data_ptr).time,trange)  
    index_range = minmax(round(index))
    index = [index_range[0]: index_range[1]]
  endif

  if isa(index,/integer) then begin
    irange = minmax(index)
    if irange[0] lt 0 || irange[1] ge ns then begin
      dprint,"out of range: index="+strtrim(index,2)+", ns="+strtrim(ns,2)+' for '+self.name
      if keyword_set(extrapolate) then index = 0 > index < (ns-1)    else return, !null   
    endif
    dats = (*self.data_ptr)[index]
    wbad = where(index lt 0,/null,nbad)
    if nbad gt 0 then begin
      fill = fill_nan(dats[wbad])
      dats[wbad] = fill
      ;dats[wbad] = !null ;davin wants to use !nulls in a later revision
    endif
    if keyword_set(valname) then begin
      retval =!null
      str_element,dats,valname,retval
      return, retval
    endif
    if keyword_set(dats) then dprint,dlevel=4,verbose=verbose,self.name+' '+string(index[0])+' '+time_string(dats[0].time)
    return,dats
  endif

  if keyword_set(valname) then begin
    retval = !null
    str_element,(*self.data_ptr),valname,retval
    return, retval
  endif


  return, *self.data_ptr
end



PRO spp_data_product::GetProperty,  ptr=ptr, name=name , data=data,dict=dict
  COMPILE_OPT IDL2
;  dprint,'hello',dlevel=3
  IF (ARG_PRESENT(ptr)) THEN ptr = self.data_ptr
;  IF (ARG_PRESENT(data_ptr)) THEN data_ptr = self.data_ptr
  if arg_present(dict) then dict = self.dict
  IF (ARG_PRESENT(data)) THEN begin
    if ptr_valid(self.data_ptr) then data = *self.data_ptr else begin
      data = !null
      dprint,dlevel=self.dlevel,'Warning: Invalid pointer for: '+self.name
    endelse
  ENDIF
  IF (ARG_PRESENT(name)) THEN name = self.name
END





PRO spp_data_product__define
  void = {spp_data_product, $
    inherits generic_object, $    ; superclas
    name: '',  $
    created: '', $
    dict: obj_new() , $
    data_ptr: ptr_new() $
    ;user_ptr: ptr_new() $
  }
END



