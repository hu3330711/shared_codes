

function cluster1,data,NBins=NBins,Delta=Delta,MidPoints=MidPoints,  $
        Count=Count


     if (n_elements(data) lt 2) then begin
        message,'Data vector contains less than two elements!', $
             /Continue
        return,-1L
     endif

     if (n_elements(NBins) gt 0 AND n_elements(Delta) gt 0) then begin
        message,'You cannot specify NBins and Delta at the same time!', $
             /Continue
        return,-1L
     endif

     if (n_elements(NBins) eq 0 AND n_elements(Delta) eq 0) then $
        NBins = 10 < (N_elements(Data)-1)      ; set default


     ; Compute difference vector
     diff = Data - shift(Data,1)
     diff = diff[1:*]

     ; Test if Data was sorted
     if (min(diff) lt 0.) then begin
        message,'Data must be sorted!',/Continue
        return,-1L
     endif

     result = 0L
     MidPoints = 0.
     Count = 0L

     ; ==================================================================
     ; NBins specified
     ; ==================================================================

     if (N_elements(NBins) eq 1) then begin
     endif

     ; ==================================================================
     ; Delta given
     ; ==================================================================

     thiscount = 1L
     thismid = Data[0]
     if (N_elements(Delta) eq 1) then begin
        for i=0L,n_elements(diff)-1 do begin
           ; check if two data points are more than delta apart
           if (diff[i] gt Delta) then begin
              ; Add boundary to index array
              result = [ result, i+1 ]
              ; Increment counter
              Count = Count + 1L
              ; Compute Center value of cluster
              MidPoints = [ Midpoints, thismid/float(thiscount) ]
              ; reset counter
              thiscount = 1L
              thismid = Data[i+1]
           endif else begin
              thismid = thismid + Data[i+1]
              thiscount = thiscount + 1L
           endelse
        endfor
     endif

print,thiscount
     ; Add last batch
     if (thiscount gt 0L) then begin
         result = [ result, i+1 ] 
         Count = Count + 1L 
         MidPoints = [ Midpoints, thismid/thiscount ] 
     endif

     ; remove dummy entry from MidPoints
     MidPoints = Midpoints[1:*]

     return,result
end

