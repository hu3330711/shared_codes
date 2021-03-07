;Ali: February 2020
; $LastChangedBy: ali $
; $LastChangedDate: 2020-08-03 13:17:39 -0700 (Mon, 03 Aug 2020) $
; $LastChangedRevision: 28974 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_4_1/projects/SPP/COMMON/spp_wav_data.pro $
; $ID: $

;no keyword: loads daily 1min resolution files (recommended for longer than 1day timerange)
;/hires  loads daily  1sec resolution files (loads faster than /hourly, since needs to load only 1 file per day)
;/hourly loads hourly 1sec resolution files (24 files per day)
;/hourly,/hires loads hourly full-resolution files (only recommended for short timespans, since file sizes can be huge!)
;run by Ali:
;/genhourly generates hourly full-resolution and 1sec files
;/gendaily generates daily 1sec and 1min files from the hourly 1sec files

pro spp_wav_data,trange=trange,types=types,genhourly=genhourly,gendaily=gendaily,hires=hires,hourly=hourly

  t1=systime(1)
  dir='/disks/data/psp/data/sci/'
  path='psp/data/sci/sweap/.wav/$TYPE$/YYYY/MM/DD/psp_fld_l2_$TYPE$_YYYYMMDD'
  cdfpath='psp/data/sci/fields/staging/l2/$TYPE$/YYYY/MM/psp_fld_l2_$TYPE$_YYYYMMDDhh_v??.cdf'
  lowresstr='_1sec'
  if keyword_set(hourly) or keyword_set(gendaily) then begin
    path=path+'hh'
    if keyword_set(hires) then lowresstr=''
  endif else if ~keyword_set(hires) then path=path+'_1min'
  alltypes=['mag_SC','dfb_wf_dvdc','dfb_wf_scm']
  if ~keyword_set(types) then types=alltypes
  tpnames=orderedhash(alltypes,['mag_SC','dfb_wf_dVdc_sensor','dfb_wf_scm_hg_sensor'])
  colors=orderedhash(alltypes,['bgr','br','br'])
  cotres=orderedhash(alltypes,['b','g','r'])
  subs=orderedhash(alltypes,[0,5,4])
  rotate=orderedhash(alltypes,[1,0,0])
  prange=orderedhash(alltypes,[24,24,1.1])
  zlim=orderedhash('mag_SC',[2e-3,50],'dfb_wf_dvdc',[1e-8,1e-4],'dfb_wf_scm',[1e-5,1])
  labels=orderedhash('mag_SC',['Bx','By','Bz'],'dfb_wf_dvdc',['dV12','dV34'],'dfb_wf_scm',['By','Bz'])
  dims=orderedhash('mag_SC',[0:2],'dfb_wf_dvdc',[0:1],'dfb_wf_scm',[1:2])

  foreach type,types do begin

    tpname0='psp_fld_l2_'+tpnames[type]
    tpname=tpname0+'_1hr'+['_tres(Hz)','','_wv_pow','_wv_pol_par','_wv_pol_perp']

    if keyword_set(genhourly) then begin
      ;fileall=file_search(dir+'fields/staging/l2/mag_SC/????/??/psp_fld_l2_mag_SC_??????????_v01.cdf')
      pathformat=str_sub(cdfpath,'$TYPE$',type)
      files=spp_file_retrieve(pathformat,trange=trange,/last_version,/valid_only,/hourly)
      for i=0,n_elements(files)-1 do begin
        del_data,[tpname0,tpname]
        if (file_info(files[i])).size eq 0 then continue
        cdf2tplot,files[i]
        get_data,tpname0,time,b,lim=lim,dlim=dlim
        options,dlim,colors=colors[type],labels=labels[type],max_points=10000,labflag=-1,ystyle=3
        t=time/3600
        t0=fix(min(t),type=14) ;hours
        for hr=0,5 do begin
          w=where(t ge t0+hr and t lt t0+hr+1,/null)
          if keyword_set(w) then begin
            del_data,tpname
            store_data,tpname[1],time[w],b[w,dims[type],0],dlim=dlim
            tres_data,tpname[1],/freq
            options,/default,tpname[0],labels=type,colors=cotres[type]
            if n_elements(w) gt 2l^16 then resolution=8 else resolution=0
            wav_data,tpname[1],/kol,rotate=rotate[type],max=2l^24,resolution=resolution,prange=[0,prange[type]]
            options,tpname[2],/default,zrange=zlim[type]
            options,tpname[2:4],/default,ystyle=3,zstyle=3
            options,tpname,'yrange',/default
            if (t0+hr) mod 24 lt 10 then zero='0' else zero=''
            ;tplotname=dir+'sweap/.wav/'+files[i].substring(40,-11)+zero+strtrim((t0+hr) mod 24,2) ;monthly directories
            tplotname=dir+'sweap/.wav/'+files[i].substring(43,57+subs[type])+files[i].substring(82+2*subs[type],-11)+files[i].substring(57+subs[type],-11)+zero+strtrim((t0+hr) mod 24,2) ;daily
            tplot_save,tpname,filename=tplotname ;full hourly
            foreach tpname1,tpname do begin
              get_data,tpname1,ptr=ptr,dat=dat
              if ~keyword_set(ptr) then continue ;necessary b/c for small field samples or for parallel polarization for dvdc and scm, there's no corresponding wavelet
              *ptr.y=average_hist2(dat.y,dat.x,centertime=*ptr.x,binsize=1.,/nan)
            endforeach
            filename=str_sub(tplotname,type,type+'_1sec')
            tplot_save,tpname,filename=filename ;1sec hourly
          endif
        endfor
      endfor
      continue
    endif

    pathformat=str_sub(path+'.tplot','$TYPE$',type+lowresstr)

    if keyword_set(gendaily) then begin
      trange=timerange(trange)
      res=24l*3600l ;1day
      tres=fix(trange/res,type=14)
      for i=0,tres[1]-tres[0]-1 do begin
        del_data,tpname
        tr=res*(tres[0]+[i,i+1])
        files=spp_file_retrieve(pathformat,trange=tr,/last_version,/valid_only,/hourly)
        if ~keyword_set(files) then continue
        tplot_restore,filenames=files,/verbose,/append,/sort
        filename=files[0].substring(0,-9)
        tplot_save,tpname,filename=filename ;1sec daily
        foreach tpname1,tpname do begin
          get_data,tpname1,ptr=ptr,dat=dat
          if ~keyword_set(ptr) then continue ;necessary b/c for small field samples or for parallel polarization for dvdc and scm, there's no corresponding wavelet
          *ptr.y=average_hist2(dat.y,dat.x,centertime=*ptr.x,binsize=60.,/nan)
          if tag_exist(ptr,'v') && size(*ptr.v,/n_dim) eq 2 then *ptr.v=average_hist2(dat.v,dat.x,binsize=60.,/nan)
        endforeach
        filename=filename+'_1min'
        tplot_save,tpname,filename=filename ;1min daily
      endfor
      continue
    endif

    files=spp_file_retrieve(pathformat,trange=trange,/last_version,/valid_only,/hourly)
    if ~keyword_set(files) then begin
      dprint,'no tplot files found for the selected time range with format: '+pathformat
      continue
    endif

    if n_elements(files) gt 1 then del_data,tpname
    tplot_restore,filenames=files,/verbose,append=keyword_set(n_elements(files)-1),/sort

  endforeach
  dprint,'Finished in '+strtrim(systime(1)-t1,2)+' seconds on '+systime()
end
