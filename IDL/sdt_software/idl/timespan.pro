;+
;PROCEDURE:  timespan, t1, dt
;PURPOSE:
;  Define a time span for the "tplot" routine.
;INPUTS:
;    t1:  starting time (seconds since 1970 or string)
;    dt:  duration of time span  (DAYS is default)
;KEYWORDS:   set one of the following:
;  SECONDS
;  MINUTES
;  HOURS
;  DAYS     (default)
;
;CREATED BY:	Davin Larson
;LAST MODIFICATION:	@(#)timespan.pro	1.11 97/02/05
;-
pro timespan,t1,dt,  $
  SECONDS = seconds,      $
  MINUTES = minutes,      $
  HOURS = hours,          $
  DAYS  = days

@tplot_com.pro
if data_type(t1) eq 0 then begin
   t1 = ''
   read,t1,prompt='Start time  (format: yy-mm-dd/hh:mm:ss)? '
   dt = 0.d
   time_units=''
   read,dt,prompt='Duration (# of days)? '
   days = 1
endif
if n_elements(t1) eq 2 then begin
  tr = time_double(t1)
endif else begin
  start_time = time_double(t1)
  if n_elements(dt) ne 0 then begin
    case 1 of
      keyword_set(days):    deltat = dt * 86400.
      keyword_set(hours):   deltat = dt * 3600.
      keyword_set(minutes): deltat = dt * 60.
      keyword_set(seconds): deltat = dt
      else:                 deltat = dt * 86400.
    endcase
  endif else deltat = 86400.
  tr = [start_time,start_time+deltat]
endelse
print,'Time range set from ',time_to_str(tr(0)),' to ',time_to_str(tr(1))
trange_full = tr
trange_cur = tr
trange = tr
return
end



