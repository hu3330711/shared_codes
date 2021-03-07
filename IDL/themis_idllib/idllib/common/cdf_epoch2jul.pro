function cdf_epoch2jul, cdf_epoch

nn = n_elements(cdf_epoch)
juls = make_array(nn, /double)

for i=0L, nn-1L do begin
    CDF_EPOCH, cdf_epoch[i], Year, Month, Day, Hour, Minute, Second, Milli, /BREAKDOWN_EPOCH
    juls[i] = julday(month, day, year, hour, minute, second+milli/1000.)
endfor

return, juls

end
