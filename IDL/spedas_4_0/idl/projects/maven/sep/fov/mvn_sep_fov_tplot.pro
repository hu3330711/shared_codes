;20180414 Ali
;mvn_sep_fov tplotter
;tplot: tplots a few important panels
;store: stores tplot variables
;fraction: carries out mars shine calculations (slow, only needed once)
;resdeg: angular resolution for fov fraction calculations

pro mvn_sep_fov_tplot,tplot=tplot,store=store,fraction=fraction,resdeg=resdeg

  @mvn_sep_fov_common.pro

  if ~keyword_set(mvn_sep_fov) then begin
    dprint,'sep fov data not loaded. Please run mvn_sep_fov,/load,/calc first! returning...'
    return
  endif
  
  pos   =mvn_sep_fov.pos
  rad   =mvn_sep_fov.rad
  pdm   =mvn_sep_fov.pdm
  occ   =mvn_sep_fov.occ
  tal   =mvn_sep_fov.tal
  crh   =mvn_sep_fov.crh
  crl   =mvn_sep_fov.crl
  times =mvn_sep_fov.time

  if keyword_set(fraction) then begin
    dprint,'calculating mars shine. this might take a while to complete...'
    fraction=mvn_sep_fov_mars_shine(mvn_sep_fov0.rmars,(replicate(1.,3)#rad.mar)*pos.mar,pos.sun,resdeg=resdeg,/fov)
    mvn_sep_fov1=fraction ;store in common block
    ;   fraction2=mvn_sep_anc_fov_mars_fraction(times,check_objects=['MAVEN_SC_BUS']) ;Rob's routine (slow)
  endif

  if keyword_set(store) then begin
    crl[where(crl lt .01,/null)]=0. ;to prevent interpolation of sep2 to cause too small count rates and mess up log plotting of data
    crh[where(crh lt .01,/null)]=0.

    tag=strlowcase(tag_names(pdm))
    npos=n_tags(pos)
    ;for ipos=0,npos-1 do begin
    for ipos=0,1 do begin
      store_data,'mvn_sep_dot_'+tag[ipos],times,transpose(pos.(ipos)),dlim={yrange:[-1,1],constant:0.,colors:'bgr',labels:['SEP1','SEP1y','SEP2'],labflag:-1,ystyle:2}
    endfor

    cos45=cos(!dtor*45.) ;earth pointed comm-pass
    cos25=cos(!dtor*25.) ;sun keep-out fov
    cos15=cos(!dtor*15.) ;sx1 sensitive fov
    store_data,'mvn_radial_distance_(km)',times,[[rad.sun],[rad.ear],[rad.mar],[rad.pho],[rad.dem]],dlim={ylog:1,colors:'kbrgm',labels:mvn_sep_fov0.objects,labflag:-1,ystyle:3}
    store_data,'mvn_speed_(km/s)',times,rad.ram,dlim={ylog:1,ystyle:2}
    store_data,'mvn_mars_dot_object',times,[[pdm.cm1],[pdm.sx1],[pdm.sun],[pdm.mar]],dlim={colors:'rbgk',labels:['Crab','Sco X1','Sun','Surface'],labflag:-1,ystyle:2,constant:0}
    store_data,'mvn_sep_occultation',times,[[occ.cm1],[occ.sx1],[occ.sun]],dlim={panel_size:.5,yrange:[0,5],ystyle:2,colors:'rbg',labels:['Crab','Sco X1','Sun'],labflag:-1,constant:[1,2,3,4]}
    store_data,'mvn_mars_tanalt(km)',times,transpose([tal[0,*].cm1,tal[2,*].sx1,tal[0,*].sun,tal[2,*].mar]),dlim={colors:'rbgk',labels:['Crab','Sco X1','Sun','mvn_alt'],labflag:-1,ystyle:2,constant:0}
    store_data,'mvn_alt_(km)',times,transpose(tal.mar),dlim={ylog:1,ystyle:2,colors:'bgr',labels:['sphere','ellipsoid','areoid'],labflag:1}
    commpass=(abs(pos[0,*].ear-cos45) lt .01) and (abs(pos[2,*].ear-cos45) lt .01) and (abs(pos[1,*].ear) lt .01)
    store_data,'mvn_sep_sun_infov',times,transpose([pos[0,*].sun gt cos25,pos[2,*].sun gt cos25,-pos[0,*].sun gt cos25,-pos[2,*].sun gt cos25,.1+transpose(pdm.sun gt pdm.mar),.2+commpass]),dlim={colors:'brcmgk',labels:['1F','2F','1R','2R','Shadow','comm-pass'],yrange:[-.1,1.3],ystyle:3,labflag:-1,panel_size:.3}
    store_data,'mvn_sep_sx1_infov',times,transpose([pos[0,*].sx1 gt cos15,pos[2,*].sx1 gt cos15,-pos[0,*].sx1 gt cos15,-pos[2,*].sx1 gt cos15,.1+transpose(pdm.sx1 gt pdm.mar)]),dlim={colors:'brcmg',labels:['1F','2F','1R','2R','Shadow'],yrange:[-.1,1.2],ystyle:3,labflag:-1,panel_size:.3}
        store_data,'mvn_sep_att',times,transpose(mvn_sep_fov.att)#[[1,0],[0,1.1]],dlim={colors:'br',labels:['SEP1','SEP2'],yrange:[.5,2.5],ystyle:3,labflag:-1,panel_size:.3}

    dlim={colors:mvn_sep_fov0.detcol,labels:mvn_sep_fov0.detlab,labflag:-1,ystyle:2,ylog:1,ytickunits:'scientific'}
    store_data,'mvn_sep1_lowe_crate',times,transpose(crl[0,*,*]),dlim=dlim
    store_data,'mvn_sep2_lowe_crate',times,transpose(crl[1,*,*]),dlim=dlim
    store_data,'mvn_sep1_high_crate',times,transpose(crh[0,*,*]),dlim=dlim
    store_data,'mvn_sep2_high_crate',times,transpose(crh[1,*,*]),dlim=dlim

    if keyword_set(mvn_sep_fov1) then begin
      fraction=mvn_sep_fov1
      for isep=0,3 do store_data,'mvn_sep'+(['1f','2f','1r','2r'])[isep]+'_fov_fraction',times,transpose([fraction.mars_surfa[isep,*],fraction.mars_shine[isep,*],fraction.mshine_fov[isep,*],fraction.atmo_shine[isep,*],fraction.ashine_fov[isep,*]]),dlim={colors:'brmcg',labels:['Disc','Mars Shine','Mfov','Atmo Shine','Afov'],labflag:-1,ystyle:2,ylog:1,yrange:[.01,1]}
      for isep=0,3 do store_data,'mvn_sep'+(['1f','2f','1r','2r'])[isep]+'_fov_fraction_model_crate',times,([1e5,1e4,1e3,3e3])[isep]*transpose(fraction.mshine_fov[isep,*])^([4,4,3.5,3.5])[isep]
      store_data,'mvn_sep1_shine_crate_model-data',data='mvn_sep1_lowe_crate mvn_sep1?_fov_fraction_model_crate',dlim={yrange:[.1,1e4]}
      store_data,'mvn_sep2_shine_crate_model-data',data='mvn_sep2_lowe_crate mvn_sep2?_fov_fraction_model_crate',dlim={yrange:[.1,1e4]}
    endif
  endif

  if keyword_set(tplot) then begin
    if mvn_sep_fov0.lowres then lrs='5min_' else lrs=''
    if mvn_sep_fov0.arc then arc='arc' else arc='svy'
    case tplot of
      1: tplot,'mvn_sep??_fov_fraction mvn_alt_(km) mvn_speed_(km/s) mvn_sep_dot_ram mvn_sep_dot_sun mvn_sep_dot_mar mvn_sep_dot_sx1 mvn_radial_distance_(km) mvn_mars_tanalt(km) mvn_sep_occultation mvn_sep?_lowe_crate mvn_sep?_high_crate mvn_sep_att mvn_'+lrs+'SEPS_'+arc+'_ATT mvn_'+lrs+'SEPS_'+arc+'_DURATION'
      2: tplot,'mvn_'+lrs+'sep?_?-?_Rate_Energy',/add
    endcase
  endif

end
