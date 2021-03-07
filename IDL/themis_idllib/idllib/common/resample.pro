;+
;FUNCTION: resample.pro
;
;PURPOSE: Resample data to a given set of input times
;
;ARGUMENTS:
;	DATA_ARRAY	-> array of data from read_cda, read_ssc, or mread_sdt
;	IN_TIMES	-> times to be interpolated over
;	BAD_PTS		<- array of possible bad points (Not used if /NAN set)
;
;RETURNS:  a matrix containing the interpolated values for each file 
;         that fill in the data gaps left by NAN values or
;	  (0) if input array does not contain compatable data type
;
;KEYWORDS: 
;       CUBIC      /  Does a cubic spline rather than a linear interpolation
;       FORCECUBIC /  Does a cubic spline Only (doesn't fall back to interpol)
;	NAN        /  returns NaN where output data may be corrupt
;       NAMES      <> Names of quantities/units.  'UT' and 'sec' are pre-pended
;                     to this if going from SDT ptr array to standard array
;
;CALLING SEQUENCE: new_array=resample(data_array,time_array,bad_pts,/nan)
;
;NOTES: /CUBIC may run into problems with certain data sets.  If this happens,
;         the default is to fall back to linear interpolation.  Using 
;         /FORCECUBIC will eliminate this fall back and RESAMPLE will
;         instead return Nan's for the problem data quantity.
;
;CREATED BY: Lisa Rassel Jun,2001
;
;LAST MODIFIED: 
;	06/13/01-L. Rassel      created 
;	07/26/01-J. Dombeck     Require times to be monotonically increasing
;                               and make non-even data rate a warning only
;	08/08/01-J. Dombeck     Change NAMES when converting from SDT pointer
;                               array to standard array
;       10/27/01-J. Dombeck     Changed assignments to be more time and 
;                               memory efficient
;       11/05/01-J. Dombeck     Handled problems with SPLINE
;       11/07/01-J. Crumley     Fized bug with time_series size changing, but
;                               t_sz not being updated.
;-
;INCLUDED MODULES:
;   rsmpl_split_interp
;   resample
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   data_type
;   difference
;   sdt_intersect
;
;-


;* RSMPL_SPLIT_INTERP *  Interpolates times and finds gaps

function rsmpl_split_interp,array,new_times,badarr,nan=nan,cubic=cubic,$
   forcecubic=forcecubic


; Remove any NaNs

  clr_array=remove_nan(array)

  if(n_elements(clr_array) le 1) then begin
    message,'cannot interpolate.  values of array are all NAN.',/cont
    return,array
  endif


; Interpolate

  if keyword_set(cubic) or keyword_set(forcecubic) then begin
    interp_values=spline(clr_array[*,0],clr_array[*,1],new_times) 
    nans=where(finite(interp_values,/nan))
    if (nans[0] ne -1) then begin
      if keyword_set(forcecubic) then begin
        message,'Problems with SPLINE (FORCECUBIC set)',/cont
      endif else begin
        message,'WARNING: Problems with SPLINE, using INTERPOL',/cont
        interp_values=interpol(clr_array[*,1],clr_array[*,0],new_times)
      endelse
    endif
  endif else $
    interp_values=interpol(clr_array[*,1],clr_array[*,0],new_times)

; Find (and mark) gaps in original data

  a=difference(clr_array[*,0])
  rate=min(a)
  t_size=n_elements(new_times)

  strpt=0l
  while(clr_array(strpt,0) lt new_times[0]) do strpt=strpt+1

  endpt=strpt
  while(clr_array(endpt,0) lt new_times[t_size-1]) do endpt=endpt+1

  flag=1
  for yy=strpt,endpt-1 do begin
    if clr_array(yy+1,0)-clr_array(yy,0) gt 1.5*rate then begin
      badpts=where(new_times gt clr_array[yy,0] and $
                   new_times lt clr_array[yy+1,0],cnt)
      if cnt ne 0 then begin
        if(keyword_set(nan)) then $
          interp_values[badpts]=!values.d_nan $
        else badarr[badpts]=1
      endif
    endif
  endfor
return,interp_values
end     ;* RSMPL_SPLIT_INTERP *



;*** MAIN *** : * RESAMPLE *

function resample,data_array,in_times,bad_pts,nan=nan,cubic=cubic,$
   forcecubic=forcecubic,names=names


; Check input


; Check IN_TIMES

  if n_elements(in_times) le 1 then begin
    message,'IN_TIMES required.',/cont
    return,0
  endif


  if data_type(in_times) ne 5 then begin
    message,'Require IN_TIMES to be doubles.',/cont
    return,0
  endif

  delta=difference(in_times)
  bogus=where(delta le 0.,cnt)
  if cnt ne 0 then begin
    message,'requested times not monotonically increasing',/cont
    return,0
  endif

  mm=min(delta)
  bogus=where(mm*1.5 lt delta,num)
  if (num gt 0) then begin
    message,'WARNING - Input times do not have an even rate.',/cont
  endif


; Find type of data_array (5-Doubles, 10-PTRs, others-bad)

  dtype=data_type(data_array)


; Array of pointers

  if dtype eq 10 then begin
  
    sz=size(data_array)          ;sz[0]=dim sz[1]=col sz[2]=data type
    n_qty=sz[1]
    t_sz=n_elements(in_times)

    if n_elements(tims) eq 1 then return,0

    if not keyword_set(names) then begin
      message,"WARNING: NAMES not provided",/cont
    endif else begin
      nnames=n_elements(names)
      if data_type(names) ne 7 or nnames ne n_qty*2 then begin
        message,"NAMES mismatch",/cont
        return,0
      endif
    endelse

    tims=sdt_intersect(data_array)

; Array of doubles

  endif else if dtype eq 5 then begin
  
    sz=size(data_array)          ;sz[0]=dim sz[1]=col sz[2]=row sz[3]=data type
    n_el=sz[1]
    n_qty=sz[2]-1
    t_sz=n_elements(in_times)

    tims=[data_array[0,0],data_array[n_el-1,0]]


; Bad data type

  endif else begin

    message,'DATA_TYPE requires DOUBLE or Pointer array.',/cont
    return,0

  endelse


; Check in_times to make sure they are within existing start/end times
; and select sub-interval of desired times


; Find first usable time point

  b=where(in_times lt tims[0],cnt)

  if (cnt ge t_sz) then begin
    message,'Input times do not fall inside data set.',/cont
    return,0
  endif else if (cnt gt 0) then begin
    in_times=[in_times[(b[cnt-1]+1):(t_sz-1)]]
    t_sz=n_elements(in_times)  ; change t_sz if in_times changes
  endif


; Find last usable time point

  c=where(in_times gt tims[1],t_cnt)

  if (t_cnt ge t_sz) then begin
    message,'Input times do not fall inside data set.',/cont
    return,0
  endif else if (t_cnt gt 0) then begin
    in_times=[in_times[0:(c[0]-1)]]
    t_sz=n_elements(in_times)  ; change t_sz if in_times changes
  endif

; Interpolate and find bad data points

  bad=make_array(t_sz,/long,value=0)
  ans=make_array(t_sz,n_qty+1,/double)
  ans[*,0]=in_times

  flag=1l
  for i=0,(n_qty-1) do begin
    tmp_bad=make_array(t_sz,/int,value=0)

    if dtype eq 5 then begin
      d_array=[[data_array[*,0]],[data_array[*,(i+1)]]]
    endif else begin
      d_array=*data_array[i]
    endelse

    ans[*,i+1]=rsmpl_split_interp(d_array,in_times,tmp_bad,nan=nan,$
      cubic=cubic,forcecubic=forcecubic)
    bad=bad+flag*tmp_bad
    flag=flag*2
  endfor

  if keyword_set(names) and dtype eq 10 then names=[[['UT'],['sec']],names]
  if not keyword_set(nan) then bad_pts=bad

return,ans

end        ;*** MAIN *** : * RESAMPLE *

