;+
;FUNCTION: time_struct(time)
;NAME:
;  time_struct
;PURPOSE:
; A fast, vectorized routine that returns a time structure.
;INPUT:  input can be of type:
;  double(s)      seconds since 1970
;  string(s)      format:  YYYY-MM-DD/hh:mm:ss
;  structure(s)   similar to format below.
;
;OUTPUT:
;  structure with the following format:
;** Structure TIME_STRUCT, 11 tags, length=40:
;   YEAR            INT           1970            ; year    (0-14699)
;   MONTH           INT              1            ; month   (1-12)
;   DATE            INT              1            ; date    (1-31)
;   HOUR            INT              0            ; hours   (0-23)
;   MIN             INT              0            ; minutes (0-59)
;   SEC             INT              0            ; seconds (0-59)
;   FSEC            DOUBLE           0.0000000    ; fractional seconds (0-.999999)
;   DAYNUM          LONG            719162        ; days since 0 AD  (subject to change)
;   DOY             INT              0            ; day of year (1-366)
;   DOW             INT              3            ; day of week  (subject to change)
;   SOD             DOUBLE           0.0000000    ; seconds of day
;
;See Also:  "time_double", "time_string", "time_epoch", "time_pb5"
;
;NOTE:
;  This routine works on vectors and is designed to be fast.
;  Output will have the same dimensions as the input
;
;CREATED BY:	Davin Larson  Oct 1996
;FILE:  time_struct.pro
;VERSION:  1.11
;LAST MODIFICATION:  02/07/29
;-
function time_struct,time,epoch=epoch,no_clean=no_clean

dt = data_type(time)

tst0 = {time_struct,year:1970,month:1,date:1,hour:0,min:0,sec:0, $
        fsec:!values.d_nan, daynum:0l,doy:0,dow:0,sod:!values.d_nan}
dim = dimen(time)
ndim = ndimen(time)
if ndim eq 0 then tsts =tst0 else tsts = make_array(value=tst0,dim=dim)

if dt eq 7 then begin         ; input is a string
  bt = bindgen(256)
  bt(byte(':_-/,'))= 32
  year=0 & month=0 & date=0 & hour=0 & min=0 & fsec=0.d
  for i=0L,n_elements(time)-1L do begin
    tst = tst0
    str = string(bt(byte(time(i))))+' 0 0 0 0 0 0'    ; remove separators
    reads,str,year,month,date,hour,min,fsec
    if year lt 70  then year = year+2000
    if year lt 200 then year = year+1900
    month = month > 1
    date = date > 1
    tst.year=year
    tst.month=month
    tst.date=date
    tst.hour=hour
    tst.min=min
    tst.fsec=fsec
    tsts(i)=tst
  endfor 
  if keyword_set(no_clean) then return,tsts $
  else return,time_struct(time_double(tsts))
endif
   
if keyword_set(epoch) then return,time_struct(time_double(time,epoch=epoch))

good = where(finite(time))

if dt eq 5 or dt eq 4 or dt eq 3 then begin         ; input is a double
    if  good(0) ne -1 then begin
        dn1970 = 719162l        ; day number of 1970-1-1
        dn = floor(time(good)/3600.d/24.d)
        sod = time(good) - dn*3600.d*24.d
        if (ndim le 1) and (dim(0) eq 1) then begin & dn=dn(0) & sod=sod(0) & end 
        daynum = dn + dn1970 
        hour = floor(sod/3600.d)
        fsec = sod - hour*3600.d
        min  = floor(fsec/60.d)
        fsec  = fsec - min*60
        sec  = floor(fsec)
        fsec = fsec - sec
        day_to_year_doy,daynum,year,doy
        doy_to_month_date,year,doy,month,date
        tsts(good).month= month
        tsts(good).date = date
        tsts(good).year = year
        tsts(good).doy  = doy
        tsts(good).hour = hour
        tsts(good).min  = min
        tsts(good).sec  = sec
        tsts(good).fsec = fsec
        tsts(good).sod  = sod
        tsts(good).daynum = daynum
        tsts(good).dow = daynum mod 7
    endif
    return,tsts
endif

if dt eq 8 then  $             ;input is a structure
   return,time_struct(time_double(time))

if dt eq 0 then  $             ;input is undefined
   return,time_struct(time_string(time,prec=6))

message,/info,'Improper time input'

end



