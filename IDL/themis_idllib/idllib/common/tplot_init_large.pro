
pro tplot_init_large

;lvl = thm_valid_input(level,'Level',vinputs='l1 l2',definput='l2',format="('l', I1)", verbose=0)
thm_init

;tplot
window,xs=480,ys=660
tplot_options,'xmargin',[14,7]
tplot_options,'charsize',1.5
tplot_options,'ymargin',[2,2]
;tplot_options,'color',0
;tplot_options,'background',-1
!P.color=0
!P.background=-1
options,'*',yticklen=0.02
options,'*',xticklen=0.06
time_stamp,/off

end