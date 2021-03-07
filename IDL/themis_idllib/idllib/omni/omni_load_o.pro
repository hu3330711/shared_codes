;omni_load,median=1,init=1


;tplot,['F','BX_GSE','BY_GSE','BZ_GSE','Vx','Vy','Vz','Psw','Pth+Pmag']


pro omni_load,noplot=noplot,init=init,median=median,timeshift=timeshift

;del_data, '*'

;set time
get_timespan, t
ts=time_struct(t[0])
Syear=''
Smonth=''
Sday=''
Shour=''
Smin=''
Ssec=''
cdum=''
print,time_string(t[0])
reads,time_string(t[0]),Syear,cdum,Smonth,cdum,Sday,cdum,Shour,cdum,Smin,cdum,Ssec,format='(a4,a1,a2,a1,a2,a1,a2,a1,a2,a1,a2)'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if(Sday eq '01' or keyword_set(init)) then begin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;cdfread
directory='/big/SATELLITE/cdaweb.gsfc.nasa.gov/pub/istp/omni/'
print,directory+'hro_1min/'+Syear+'/omni_hro_1min_'+Syear+Smonth+'01_v01.cdf'
cdf2tplot,files=directory+'hro_1min/'+Syear+'/omni_hro_1min_'+Syear+Smonth+'01_v01.cdf'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,'F',data=Btot
get_data,'Vx',data=Vx
get_data,'Vy',data=Vy
get_data,'Vz',data=Vz
get_data,'proton_density',data=Nsw
get_data,'T',data=Tsw
get_data,'Pressure',data=Psw
get_data,'E',data=E
get_data,'Mach_num',data=Mach_num

get_data,'BX_GSE',data=Bx,index=ind_Bx
get_data,'BY_GSE',data=By,index=ind_By
get_data,'BZ_GSE',data=Bz,index=ind_Bz
if ind_Bx eq 0 or ind_By eq 0 or ind_Bz eq 0 then return
store_data,'Bgse2',data={x:Bx.x,y:[[Bx.y],[By.y],[Bz.y]]}
cotrans,'Bgse2','Bgsm2',/GSE2GSM,/IGNORE_DLIMITS
get_data,'Bgsm2',data=Bgsm
store_data,'BX_GSM',data={x:Bgsm.x,y:Bgsm.y(*,0)}
store_data,'BY_GSM',data={x:Bgsm.x,y:Bgsm.y(*,1)}
store_data,'BZ_GSM',data={x:Bgsm.x,y:Bgsm.y(*,2)}
get_data,'BY_GSM',data=By
get_data,'BZ_GSM',data=Bz

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if(keyword_set(timeshift)) then begin

Btot.x=Btot.x+timeshift
Bx.x=Bx.x+timeshift
By.x=By.x+timeshift
Bz.x=Bz.x+timeshift
Nsw.x=Nsw.x+timeshift
Tsw.x=Tsw.x+timeshift
Psw.x=Psw.x+timeshift
Vx.x=Vx.x+timeshift
Vy.x=Vy.x+timeshift
Vz.x=Vz.x+timeshift
E.x=E.x+timeshift
Mach_num.x=Mach_num.x+timeshift

endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;median
if(keyword_set(median)) then begin
median=median
endif else begin
median=10
endelse

if(median ge 2) then begin
if(n_elements(Btot.y[*,0]) gt median) then begin
data_med=median(Btot.y,median)
Btot.y=data_med
data_med=median(Bx.y,median)
Bx.y=data_med
data_med=median(By.y,median)
By.y=data_med
data_med=median(Bz.y,median)
Bz.y=data_med
endif

if(n_elements(Vx.y[*,0]) gt median) then begin
data_med=median(Vx.y,median)
Vx.y=data_med
data_med=median(Vy.y,median)
Vy.y=data_med
data_med=median(Vz.y,median)
Vz.y=data_med
data_med=median(Nsw.y,median)
Nsw.y=data_med
data_med=median(Tsw.y,median)
Tsw.y=data_med
data_med=median(Psw.y,median)
Psw.y=data_med
data_med=median(Mach_num.y,median)
Mach_num.y=data_med
endif
endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Pall
;Pth
k_B=1.3806503000E-23
mu0=1.2566370614E-06

Pth=Nsw.y*1e6*k_B*Tsw.y*1e9
;Pmag
Pmag=Btot.y*1e-9*Btot.y*1e-9/(2*mu0)*1e9
;Pmag+Pth
Pmag_Pth=Pmag+Pth
store_data,'Pth+Pmag',data={x:Btot.x,y:[[Pth(*)],[Pmag(*)],[Pmag_Pth(*)]]},dlim={colors:[2,4,0],labels:['Pth','Pmag','Pmag+Pth'],ysubtitle:'[nPa]',labflag:-1,constant:0}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ER=-Vx.y*Btot.y*sin(acos(Bz.y/sqrt(By.y^2+Bz.y^2))*0.50)^2*1e-3
Ey=-(Vz.y*Bx.y-Vx.y*Bz.y)*1e-3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
AKASOFU=-(Vx.y*1e3)*(Btot.y*1e-9)^2*(7*6372.*1e3)^2*sin(acos(Bz.y/sqrt(By.y^2+Bz.y^2))*0.50)^4*1e7*1e-9;GW
NEWELL=-4.55$
       +2.229*1e-3*(-Vx.y)^(4./3)*(Btot.y)^(2./3)*sin(acos(Bz.y/sqrt(By.y^2+Bz.y^2))*0.50)^(8./3)$
       +1.73*1e-5*sqrt(Nsw.y)*(Vx.y)^2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
store_data,'Bgsm',data={x:Btot.x,y:[[By.y],[Bz.y],[Btot.y]]},dlim={colors:[4,6,0],labels:['By','Bz','|B|'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'Bz',data={x:Btot.x,y:[[Bz.y]]},dlim={colors:[0],labels:['Bz'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'Vx',data={x:Vx.x,y:[[-Vx.y]]},dlim={colors:[0],labels:['-Vx'],ysubtitle:'[km/s]',labflag:-1,constant:[400,800,1000],ylog:1,panel_size:0.7}
store_data,'Nsw',data={x:Nsw.x,y:[[Nsw.y]]},dlim={colors:[0],labels:['Nsw'],ysubtitle:'[/cm3]',labflag:-1,constant:[1,10],ylog:1,panel_size:0.7}
store_data,'Psw',data={x:Nsw.x,y:[[Psw.y]]},dlim={colors:[0],labels:['Psw'],ysubtitle:'[nPa]',labflag:-1,constant:[1,10],ylog:1,panel_size:0.7}
store_data,'Esw',data={x:Nsw.x,y:[[Ey],[ER]]},dlim={colors:[4,0],labels:['Ey','Erx'],ysubtitle:'[mV/m]',labflag:-1,constant:[1,10],ylog:1,panel_size:0.5}
;store_data,'Aurora',data={x:Nsw.x,y:[[NEWELL]]},dlim={colors:[2,6],labels:['NEWELL'],ysubtitle:'[GW]',labflag:-1,constant:[1,10],ylog:0}
store_data,'Aurora',data={x:Nsw.x,y:[[AKASOFU/10],[NEWELL]]},dlim={colors:[6,2],labels:['AK/10','NEWEL'],ysubtitle:'[GW]',labflag:-1,constant:[1,10,100],ylog:1,panel_size:0.5}
store_data,'MA',data={x:Nsw.x,y:[[Mach_num.y]]},dlim={colors:[0],labels:['MA'],ysubtitle:'[]',labflag:-1,constant:[10],ylog:1}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ylim,'Bgsm',-30,30
ylim,'Bz',-30,30
ylim,'Vx',100,2000
ylim,'Nsw',0.1,100
ylim,'Psw',0.1,100
ylim,'MA',1,100
ylim,'Esw',0.1,100
ylim,'Aurora',1,1000
options,'Bgsm',yticks=4
options,'Bgsm',yminor=3
options,'Bz',yticks=4
options,'Bz',yminor=3
options,'Psw',ytickformat='logticks_exp'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
kyoto_load_symasy2
if(Syear ne 1996) then kyoto_load_ae3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;tplot
if keyword_set(noplot) then begin
options,'*',yticklen=0.02
options,'*',xticklen=0.06
time_stamp,/off
endif else begin
options,'*',yticklen=0.02
options,'*',xticklen=0.06
time_stamp,/off
tplot_options,title='KYOTO OMNI '+Syear+Smonth+Sday
tplot,['AU-AL','SYM-ASY','Bgsm','Vx','Nsw','Psw','Esw','Aurora']
;makepng,strjoin('/home/g3/yukitosi/research/OMNI/png/'+Syear+Smonth+Sday)
endelse

end