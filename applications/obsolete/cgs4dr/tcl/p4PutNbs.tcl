proc p4PutNbs {root} {
#+
# Stores all nbs items for port in temporary array
#-
    global P4Widgets
    global P4NoticeBoard

#  Put character nbs items
    nbs put ${root}display_data $P4Widgets(C01) 
    nbs put ${root}device_lut $P4Widgets(C02) 
    nbs put ${root}title $P4Widgets(C03) 
    nbs put ${root}device_name $P4Widgets(C04) 
    nbs put ${root}device_xopt $P4Widgets(C05) 
    nbs put ${root}device_yopt $P4Widgets(C06) 
    nbs put ${root}display_type $P4Widgets(C07) 
    nbs put ${root}display_plane $P4Widgets(C08) 
    nbs put ${root}contour_type $P4Widgets(C09) 
    nbs put ${root}overcolour $P4Widgets(C10) 
    nbs put ${root}colour_style $P4Widgets(C11) 
    nbs put ${root}fg_colour $P4Widgets(C12) 
    nbs put ${root}bg_colour $P4Widgets(C13) 
    nbs put ${root}cut_direction $P4Widgets(C14) 
    nbs put ${root}last_type $P4Widgets(C15) 

#  Put logical nbs items
    nbs put ${root}plot_axes $P4Widgets(L01) 
    nbs put ${root}plot_errors $P4Widgets(L02) 
    nbs put ${root}plot_whole $P4Widgets(L03) 
    nbs put ${root}pre_erase_plot $P4Widgets(L04) 
    nbs put ${root}autoscale $P4Widgets(L05) 
    nbs put ${root}port_ok $P4Widgets(L06) 
    nbs put ${root}plot_ok $P4Widgets(L07) 

#  Put integer nbs items
    nbs put ${root}contour_levels $P4Widgets(I01) 
    nbs put ${root}histogram_bins $P4Widgets(I02) 
    nbs put ${root}histogram_xstep $P4Widgets(I03) 
    nbs put ${root}histogram_ystep $P4Widgets(I04) 
    nbs put ${root}hist_smooth $P4Widgets(I05) 
    nbs put ${root}toosmall $P4Widgets(I06) 
    nbs put ${root}toolarge $P4Widgets(I07) 
    nbs put ${root}istart $P4Widgets(I08) 
    nbs put ${root}iend $P4Widgets(I09) 
    nbs put ${root}jstart $P4Widgets(I10) 
    nbs put ${root}jend $P4Widgets(I11) 

#  Put real nbs items
    nbs put ${root}vxstart $P4Widgets(R01)
    nbs put ${root}vxend $P4Widgets(R02) 
    nbs put ${root}vystart $P4Widgets(R03) 
    nbs put ${root}vyend $P4Widgets(R04) 
    nbs put ${root}axstart $P4Widgets(R05) 
    nbs put ${root}axend $P4Widgets(R06) 
    nbs put ${root}aystart $P4Widgets(R07) 
    nbs put ${root}ayend $P4Widgets(R08) 
    nbs put ${root}xstart $P4Widgets(R09) 
    nbs put ${root}xend $P4Widgets(R10) 
    nbs put ${root}ystart $P4Widgets(R11) 
    nbs put ${root}yend $P4Widgets(R12) 
    nbs put ${root}mode $P4Widgets(R13) 
    nbs put ${root}mean $P4Widgets(R14) 
    nbs put ${root}sigma $P4Widgets(R15) 
    nbs put ${root}high $P4Widgets(R16) 
    nbs put ${root}low $P4Widgets(R17) 
    nbs put ${root}fmin $P4Widgets(R18) 
    nbs put ${root}fmax $P4Widgets(R19) 
    nbs put ${root}slice_start $P4Widgets(R20) 
    nbs put ${root}slice_end $P4Widgets(R21) 
    nbs put ${root}char_height $P4Widgets(R22) 

# Update the display
    update idletasks
}
