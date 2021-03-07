;omni_load,median=1,init=1


;tplot,['F','BX_GSE','BY_GSE','BZ_GSE','Vx','Vy','Vz','Psw','Pth+Pmag']


pro omni_load,noplot=noplot,init=init,median=median,timeshift=timeshift,noinit=noinit

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
if(Sday eq '01' or not keyword_set(noinit)) then begin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;cdfread
;directory='/big/SATELLITE/cdaweb.gsfc.nasa.gov/pub/istp/omni/'
;print,directory+'hro_1min/'+Syear+'/omni_hro_1min_'+Syear+Smonth+'01_v01.cdf'
;cdf2tplot,files=directory+'hro_1min/'+Syear+'/omni_hro_1min_'+Syear+Smonth+'01_v01.cdf'
source = file_retrieve(/struct)
source.min_age_limit = 900 ; allow 15 mins between updates
source.local_data_dir = getenv('BIG_DIR')+'/big/SATELLITE/cdaweb.gsfc.nasa.gov/pub/data/omni/omni_cdaweb/'
source.remote_data_dir = ''
source.use_wget=1
source.nowait=0
datfileformat = 'hro_1min/YYYY/omni_hro_1min_YYYYMM01_v01.cdf'
relfnames = file_dailynames(file_format=datfileformat,trange=trange,/unique,times=times)
datfiles = file_retrieve(relfnames,_extra=source)
spawn,'ls -alF '+datfiles, line
if (line ne '') then cdf2tplot, file=datfiles
print,datfiles

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

get_data,'BX_GSE',data=Bx
get_data,'BY_GSE',data=By
get_data,'BZ_GSE',data=Bz
store_data,'Bgse2',data={x:Bx.x,y:[[Bx.y],[By.y],[Bz.y]]}
cotrans,'Bgse2','Bgsm2',/GSE2GSM,/IGNORE_DLIMITS
get_data,'Bgsm2',data=Bgsm
store_data,'BX_GSM',data={x:Bgsm.x,y:Bgsm.y(*,0)}
store_data,'BY_GSM',data={x:Bgsm.x,y:Bgsm.y(*,1)}
store_data,'BZ_GSM',data={x:Bgsm.x,y:Bgsm.y(*,2)}
get_data,'BY_GSM',data=By
get_data,'BZ_GSM',data=Bz
options,'B?_G??',constant=0

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
median=1
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
mp=1.67262178e-27

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
;Hill-Siscoe PCP [Boudourilis et al., 2004]
IMF_clock_angle=atan(By.y/Bz.y)*180/!pi
where_temp=where(Bz.y lt 0,count)
if count ge 1 then IMF_clock_angle(where_temp)+=180
Sigma=5.
Ftheta=sin(IMF_clock_angle*!pi/180/2)^2
PCP_Hill_Siscoe=(57.6*Psw.y^(1/3)*Ey*Ftheta)/(sqrt(Psw.y)+0.0125*(4.45-1.08*alog10(Sigma))*Sigma*Ey*Ftheta)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;[Borovsky and Birn, 2014]
C_rquick=(2.44e-4+(1+1.38*alog(Mach_num.y))^(-6))^(-1/6)
beta_s=(Mach_num.y/6)^(1.92)
Rquick=0.4*sqrt(mu0*mp)*sin(IMF_clock_angle*!pi/180/2)^2*C_rquick^(-1/2)*sqrt(Nsw.y*1e6)*(Vx.y*1e3)^2*(1+beta_s)^(-3/4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
store_data,'Bgsm',data={x:Btot.x,y:[[By.y],[Bz.y],[Btot.y]]},dlim={colors:[4,6,0],labels:['By','Bz','|B|'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'IMF',data={x:Btot.x,y:[[Bx.y],[By.y],[Bz.y],[Btot.y]]},dlim={colors:[2,4,6,0],labels:['Bx','By','Bz','|B|'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'Bz',data={x:Btot.x,y:[[Bz.y]]},dlim={colors:[0],labels:['Bz'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'Vx',data={x:Vx.x,y:[[-Vx.y]]},dlim={colors:[0],labels:['-Vx'],ysubtitle:'[km/s]',labflag:-1,constant:[400,800,1000],ylog:1,panel_size:1}
store_data,'Nsw',data={x:Nsw.x,y:[[Nsw.y]]},dlim={colors:[0],labels:['Nsw'],ysubtitle:'[/cm3]',labflag:-1,constant:[1,10],ylog:1,panel_size:1}
store_data,'Psw',data={x:Nsw.x,y:[[Psw.y]]},dlim={colors:[0],labels:['Psw'],ysubtitle:'[nPa]',labflag:-1,constant:[1,10],ylog:1,panel_size:1}
store_data,'Esw',data={x:Nsw.x,y:[[Ey],[ER]]},dlim={colors:[4,0],labels:['Ey','Erx'],ysubtitle:'[mV/m]',labflag:-1,constant:[1,10],ylog:1,panel_size:1}
;store_data,'Aurora',data={x:Nsw.x,y:[[NEWELL]]},dlim={colors:[2,6],labels:['NEWELL'],ysubtitle:'[GW]',labflag:-1,constant:[1,10],ylog:0}
store_data,'Aurora',data={x:Nsw.x,y:[[AKASOFU/10],[NEWELL]]},dlim={colors:[6,2],labels:['AK/10','NEWEL'],ysubtitle:'[GW]',labflag:-1,constant:[1,10,100],ylog:1,panel_size:1}
store_data,'MA',data={x:Nsw.x,y:[[Mach_num.y]]},dlim={colors:[0],labels:['MA'],ysubtitle:'[]',labflag:-1,constant:[10],ylog:1}
store_data,'PCP_Hill_Siscoe',data={x:Nsw.x,y:PCP_Hill_Siscoe},dlim={colors:[0],labels:[''],ysubtitle:'[kV]',labflag:-1,constant:[0],ylog:0}
store_data,'Reconnection_rate_Rquick',data={x:Nsw.x,y:Rquick},dlim={colors:[0],labels:[''],ysubtitle:'[m/sec]',labflag:-1,constant:[0],ylog:1}
;store_data,'IMF_clock_angle',data={x:Nsw.x,y:IMF_clock_angle},dlim={colors:[0],labels:[''],ysubtitle:'[deg]',labflag:-1,constant:[0],ylog:0}
;store_data,'Ftheta',data={x:Nsw.x,y:Ftheta},dlim={colors:[0],labels:[''],ysubtitle:'[]',labflag:-1,constant:[0],ylog:0}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ylim,'Bgsm',-30,30
ylim,'Bz',-30,30
ylim,'Vx',100,2000
ylim,'Nsw',0.1,100
ylim,'Psw',0.1,100
ylim,'MA',1,100
ylim,'Esw',0.1,100
ylim,'Aurora',1,1000
ylim,'Reconnection_rate_Rquick',1e-4,1e-1,1
options,'Bgsm',yticks=4
options,'Bgsm',yminor=3
options,'Bz',yticks=4
options,'Bz',yminor=3
options,['Psw','Reconnection_rate_Rquick'],ytickformat='logticks_exp'

ylim,['AU-AL','SYM-ASY','Bgsm'],0,0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
endif

;;;------calculate magnetopause location based on Shue et al. [1998]
get_data,'BZ_GSM',data=dbz
get_data,'Pressure',data=dpdyn

r0_mp = (10.22 + 1.29*tanh(0.184*(dbz.y+8.14)))*dpdyn.y^(-1./6.6)
store_data,'r0_mp_omni',data={x:dbz.x,y:r0_mp},dlim={constant:'10'}
ylim,'r0_mp_omni',5,15,0
options,'r0_mp_omni',ytitle='MP loc', ysubtitle='[Re]'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;kyoto_load_symasy2
;if(Syear ne 1996) then kyoto_ae_load3
;if(Syear ne 1996) then kyoto_load_ae3
copy_data,'AE_INDEX','AE'
copy_data,'AU_INDEX','AU'
copy_data,'AL_INDEX','AL'
options,'AE',colors=[0],labels=['AE'],ysubtitle='[nT]',labflag=-1,constant=[0],ylog=0
options,'AU',colors=[6],labels=['AU'],ysubtitle='[nT]',labflag=-1,constant=[0],ylog=0
options,'AL',colors=[2],labels=['AL'],ysubtitle='[nT]',labflag=-1,constant=[0],ylog=0
store_data,'AU-AL',data='AU AL'

store_data,'SYM-ASY',data='SYM_H SYM_D'
options,'SYM_D',linestyle=2
options,'SYM-ASY',constant=[0]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;tplot
if keyword_set(noplot) then begin
;options,'*',yticklen=0.02
;options,'*',xticklen=0.06
;time_stamp,/off
print,''
endif else begin
;options,'*',yticklen=0.02
;options,'*',xticklen=0.06
;time_stamp,/off
;tplot_options,title='OMNI '+Syear+Smonth+Sday
tplot,['AU-AL','SYM-ASY','IMF','Vx','Nsw','Psw','Reconnection_rate_Rquick','PCP_Hill_Siscoe']
;makepng,strjoin('/home/g3/yukitosi/research/OMNI/png/'+Syear+Smonth+Sday)
endelse

end
