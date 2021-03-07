;+
;NAME:
; mvn_sta_l2eflux
;PURPOSE:
; Helper function for eflux calculations
;INPUT:
; cmn_dat = MAVEN STATIC data common block
;OUTPUT:
; cmn_dat = same common block with eflux variable added
;HISTORY:
; hacked from mvn_sta_cmn_l2gen, 2016-02-26, jmm,
; jimm@ssl.berkeley.edu
; $LastChangedBy: jimm $
; $LastChangedDate: 2017-01-10 13:04:56 -0800 (Tue, 10 Jan 2017) $
; $LastChangedRevision: 22567 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_4_0/projects/maven/sta/l2util/mvn_sta_l2eflux.pro $
;-

Pro mvn_sta_l2eflux, cmn_dat

  apid = strupcase(cmn_dat.apid)
  npts = n_elements(cmn_dat.time)
  iswp = cmn_dat.swp_ind
  ieff = cmn_dat.eff_ind
  iatt = cmn_dat.att_ind
  mlut = cmn_dat.mlut_ind
  nenergy = cmn_dat.nenergy
  nmass = cmn_dat.nmass
  nbins = cmn_dat.nbins
  ndef = cmn_dat.ndef
  nanode = cmn_dat.nanode
  If(apid Eq 'C0' Or apid Eq 'C2' Or apid Eq 'C4' Or apid Eq 'C6') Then Begin
     gf = reform(cmn_dat.gf[iswp,*,0]*((iatt eq 0)#replicate(1.,nenergy)) +$
                 cmn_dat.gf[iswp,*,1]*((iatt eq 1)#replicate(1.,nenergy)) +$
                 cmn_dat.gf[iswp,*,2]*((iatt eq 2)#replicate(1.,nenergy)) +$
                 cmn_dat.gf[iswp,*,3]*((iatt eq 3)#replicate(1.,nenergy)), $
                 npts*nenergy)#replicate(1.,nmass)
     gf = cmn_dat.geom_factor*reform(gf,npts,nenergy,nmass)
     eff = cmn_dat.eff[ieff,*,*]
     dt = cmn_dat.integ_t#replicate(1.,nenergy*nmass)
     eflux = (cmn_dat.data-cmn_dat.bkg)*cmn_dat.dead/(gf*eff*dt)
  Endif Else If(apid Eq 'C8' Or apid Eq 'CA') Then Begin
     gf = reform(cmn_dat.gf[iswp,*,*,0]*((iatt eq 0)#replicate(1.,nenergy*nbins)) +$
                 cmn_dat.gf[iswp,*,*,1]*((iatt eq 1)#replicate(1.,nenergy*nbins)) +$
                 cmn_dat.gf[iswp,*,*,2]*((iatt eq 2)#replicate(1.,nenergy*nbins)) +$
                 cmn_dat.gf[iswp,*,*,3]*((iatt eq 3)#replicate(1.,nenergy*nbins)), $
                 npts,nenergy,nbins)
     gf = cmn_dat.geom_factor*gf
     eff = cmn_dat.eff[ieff,*,*]
     dt = cmn_dat.integ_t#replicate(1.,nenergy*nbins)
     eflux = (cmn_dat.data-cmn_dat.bkg)*cmn_dat.dead/(gf*eff*dt)
  Endif Else Begin
     gf = reform(cmn_dat.gf[iswp,*,*,0]*((iatt eq 0)#replicate(1.,nenergy*nbins)) +$
                 cmn_dat.gf[iswp,*,*,1]*((iatt eq 1)#replicate(1.,nenergy*nbins)) +$
                 cmn_dat.gf[iswp,*,*,2]*((iatt eq 2)#replicate(1.,nenergy*nbins)) +$
                 cmn_dat.gf[iswp,*,*,3]*((iatt eq 3)#replicate(1.,nenergy*nbins)), $
                 npts*nenergy*nbins)#replicate(1.,nmass)
     gf = cmn_dat.geom_factor*reform(gf,npts,nenergy,nbins,nmass)
     eff = cmn_dat.eff[ieff,*,*,*]
     dt = cmn_dat.integ_t#replicate(1.,nenergy*nbins*nmass)
     eflux = (cmn_dat.data-cmn_dat.bkg)*cmn_dat.dead/(gf*eff*dt)
  Endelse
  eflux = float(eflux) ;eflux has to be a float array, not double, jmm, 2016-03-30
  str_element, cmn_dat, 'eflux', eflux, /add_replace
  Return
End

