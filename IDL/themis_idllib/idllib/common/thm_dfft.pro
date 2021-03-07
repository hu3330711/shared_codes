pro thm_dfft,names,dimension=dimension,skip_time=skip_time,bins=bins,hamming=hamming,shift=shift

if not keyword_set(dimension) then dimension=0
if not keyword_set(skip_time) then skip_time=1

get_data,names,data=data
data2=[[data.x],[double(reform(data.y[*,dimension]-median(data.y[*,dimension])))]]
data2=data2[0:n_elements(data2[*,0])-1:skip_time,*]
bins=shift
fft_result=dfft(data2,values,dum,power,fft_time=fft_time,bins=bins,hamming=hamming,shift=shift)
if n_elements(fft_result[0,*]) gt n_elements(values) then fft_result=fft_result[*,0:n_elements(values)-1]
store_data,names+'_'+strtrim(string(dimension),1)+'_dfft',data={x:fft_time,y:fft_result,v:values},dlim={spec:1,zlog:1,ylog:1,ysubtitle:'[Hz]',zsubtitle:'[x^2/Hz]'}

end


