;+
;FUNCTION: dfft.pro
;
;PURPOSE: Time varying (Dynamic) FFT in standard data format
;
;ARGUMENTS:
;       DATA_ARRAY  -> array of data from read_cda, read_ssc, or mread_sdt
;       VALUES      <- Frequencies of bins as floats
;       NAMES       <- Frequencies of bins as strings
;       POWER       <- Powers of frequencies during extire time range 
;
;RETURNS:  Standard Data Array power spectra
;          0 on error
;
;KEYWORDS:
;       HAMMING   /  Use Hamming window instead of Hanning
;       RECT      /  Use a rectangular window instead of Hanning
;       BINS      -> Use a specified number of bins rather than default
;                     Should be 2^x.
;       SHIFT     -> Points to shift by for each time step.
;       INDEX     -> Index of DATA_ARRAY quantity to fft
;       MIN_POWER -> Minimum power to return (1.0E-8 if default)
;
;CALLING SEQUENCE: new_data=dfft(data_array,index=4,values,names,power)
;
;NOTES: By default uses a Hanning window
;       Default BINS is 128 or largest power of 2 <= number of data points
;       Default SHIFT = BINS.
;
;CREATED BY: John Dombeck Aug,2001
;
;LAST MODIFIED:
;       08/31/01-J. Dombeck     Created
;       10/04/01-J. Dombeck     Switched to using EVEN_RATE
;       08/06/03-J. Dombeck     Added SHIFT keyword
;-
;INCLUDED MODULES:
;   dfft
;
;LIBRARIES USED:
;   None
;
;DEPENDANCIES
;   data_type
;   difference
;   even_rate
;   find_datarate
;
;-



;*** MAIN *** : * DFFT *

function dfft,data_array,values,names,power,hamming=hamming,rect=rect,$
           bins=bins,shift=shift,index=index,min_power=min_power,fft_time=fft_time


; Check input

  dtype=data_type(data_array)
  if dtype ne 5 and dtype ne 10 then begin
    message,"DATA_ARRAY requires standard data format",/cont
    return,0
  endif 


; Find number of data quantities

  if dtype eq 10 then begin
    nqtys=n_elements(data_type)
  endif else begin
    sz=size(data_array)
    if sz(0) ne 2 then begin
      message,"DATA_ARRAY requires standard data format",/cont
      return,0
    endif 

    nqtys=sz(2)-1
  endelse


; Check Index

  if not keyword_set(index) then begin
    if nqtys ne 1 then $
      message,$
       "WARNING - INDEX not set w/ multiple quantities.  Using 1st data qty.",$
       /cont
    index=1
  endif else begin
    if index ne fix(index) or index lt 1 or index gt nqtys then begin
      message,"Bad INDEX",/cont
      return,0
    endif
  endelse


; Check Bins

  if keyword_set(bins) then begin
    factor=alog2(bins)
    if factor ne floor(factor) or bins lt 2 then begin
      message,"BINS requires factor of 2^X, X>=1",/cont
      return,0
    endif
  endif

    
; Check minimum power

  if not keyword_set(min_power) then begin
    minpwr=1.0e-8
  endif else begin
    if min_power lt 0 then begin
      message,"MIN_POWER >= 0.0 required",/cont
      return,0
    endif
    minpwr=min_power
  endelse


; Get data 

  if dtype eq 10 then data=*data_array[index-1] $
                 else data=data_array[*,[0,index]] 


; Check for even sample rate

  rate=find_datarate(data[*,0],gaps)
  if gaps eq 2 then begin
    ;message,"Can't process - Uneven data rate",/cont
    ;return,0
    message,"WARNING - Gaps in data, interpolating",/cont
    times=even_rate(data)
    data=resample(data,times)
  endif

  if gaps eq 1 then begin
    message,"WARNING - Gaps in data, interpolating",/cont
    times=even_rate(data)
    data=resample(data,times)
  endif

  
; OLD Check for even sample rate

;;dif=difference(data[*,0])
;;rate=min(dif)
;;bogus=where(dif-rate gt rate*0.1,count) ; Consider constant rate if w/i 10%
;;if count ne 0 then begin
;;  bogus=where(abs(dif/rate-floor(dif/rate)) gt 0.1,count)
;;  if count ne 0 then begin
;;    message,"Can't process - Uneven data rate",/cont
;;    return,0
;;  endif
;;
;;  message,"WARNING - Gaps in data, interpolating",/cont
;;  npts=floor((data[n_elements(data[*,0])-1,0]-data[0,0])/rate)
;;  times=data[0,0]+rate*lindgen(npts)
;;  data=resample(data,times)
;;endif else begin
;;  npts=n_elements(data[*,0])
;;endelse
  

; Determine bins

  npts=n_elements(data[*,0])

  if npts lt 2 then begin
    message,"Not enough points in DATA",/cont
    return,0
  endif

  bns=2l^floor(alog2(npts))
  ;if bns gt 128 then bns=128
  if keyword_set(bins) then $
    if bins lt npts then begin
      bns=bins
    endif else begin
      message,"WARNING - Not enough points for BINS specified.",/cont
      message,"        - Using "+strcompress(string(bns))+" bins",/cont
    endelse
  
  
; Check Shift

  if keyword_set(shift) then begin
    if shift lt 1 or shift gt bns then begin
      message,"SHIFT must be <= BINS, >0",/cont
      print,shift,bns
      return,0
    endif
  endif else shift=bns

    

; Calculate window

  if keyword_set(rect) then begin
    wind=make_array(bns,/float,value=1.0)
  endif else begin
    if keyword_set(hamming) then alpha=0.54 $ ; ALPHA for Hamming in HANNING 
                            else alpha=0.5    ; ALPHA for Hanning in HANNING
    wind=hanning(bns,alpha=alpha)
  endelse


; Calculate frequencies

  values=findgen(1+bns/2)/(rate*bns)
  names=['UT',strcompress(string(values,format='(F10.1)'),/remove_all)]


; Calculate Dynamic FFT

  n_times=floor(float(npts)/shift)-ceil(float(bns)/shift)+1
  dspec=make_array(n_times,bns/2+2,/double)
  fft_time=dindgen(n_times)*(data_array[n_elements(data_array[*,0])-1,0]-data_array[0,0])/n_times+data_array[0,0]

  for xx=0l,n_times-1 do begin

  
  ; Get data for this time slice and multply by window

    thisdat=data[xx*shift:xx*shift+bns-1,1]
    winddat=wind*thisdat

  
  ; Compute fft and combine for power spectrum

    thisfft=fft(winddat,-1)
    thispwr=float(thisfft*conj(thisfft))*bns
    dspec[xx,1]=thispwr[0]
    dspec[xx,bns/2+1]=thispwr[bns/2]
    for yy=1,bns/2-1 do dspec[xx,yy+1]=thispwr[yy]+thispwr[bns-yy]


  ; Set minimum power

    toolow=where(dspec[xx,*] lt minpwr,count)
    if count ne 0 then dspec[xx,toolow]=minpwr
    

  ; Find time (center of slice)

    dspec[xx,0]=data[xx*shift+bns/2,0]
    
  endfor
    

; Calculate total FFT

  power=make_array(bns/2+1,/float)
  for xx=0,bns/2 do power[xx]=total(dspec[*,xx+1])


return,dspec

end        ;*** MAIN *** : * DFFT *

