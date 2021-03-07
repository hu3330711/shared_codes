
pro tplot_init,xs=xs,ys=ys,loadct=loadct

;lvl = thm_valid_input(level,'Level',vinputs='l1 l2',definput='l2',format="('l', I1)", verbose=0)
thm_init

if not keyword_set(xs) then xs=480
if not keyword_set(ys) then ys=660

;tplot
window,xs=xs,ys=ys
print,'window,xs,ys=',xs,ys
tplot_options,'xmargin',[14,7]
tplot_options,'charsize',1.5
tplot_options,'ymargin',[2,2]
tplot_options,'num_lab_min',3.01
tplot_options,'wshow',0
tplot_options,'ygap',0.2
;tplot_options,'color',0
;tplot_options,'background',-1
!p.color=0
!p.background=-1
!p.charsize=1.5
options,'*',yticklen=0.02
options,'*',xticklen=0.06
time_stamp,/off


;loadct2,39
loadct2,43,file=getenv('BIG_DIR')+'/big/SATELLITE/themis/software/tdas/current/idl/themis/common/thm_ctables/thm_colors.tbl'
if keyword_set (loadct) then loadct,43,file=getenv('BIG_DIR')+'/big/SATELLITE/themis/software/tdas/current/idl/themis/common/thm_ctables/thm_colors.tbl'

!themis.min_age_limit=86400

end
