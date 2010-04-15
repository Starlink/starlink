proc p4PutNbs {nbsroot} {
#+
# Stores all nbs items for port in temporary array
#-
    global P4Widgets
    global P4NoticeBoard

#  Put character nbs items
    nbs put ${nbsroot}.display_data $P4Widgets(C01)
    nbs put ${nbsroot}.device_lut $P4Widgets(C02)
    nbs put ${nbsroot}.title $P4Widgets(C03)
    nbs put ${nbsroot}.device_name $P4Widgets(C04)
    nbs put ${nbsroot}.device_xopt $P4Widgets(C05)
    nbs put ${nbsroot}.device_yopt $P4Widgets(C06)
    nbs put ${nbsroot}.display_type $P4Widgets(C07)
    nbs put ${nbsroot}.display_plane $P4Widgets(C08)
    nbs put ${nbsroot}.contour_type $P4Widgets(C09)
    nbs put ${nbsroot}.overcolour $P4Widgets(C10)
    nbs put ${nbsroot}.colour_style $P4Widgets(C11)
    nbs put ${nbsroot}.fg_colour $P4Widgets(C12)
    nbs put ${nbsroot}.bg_colour $P4Widgets(C13)
    nbs put ${nbsroot}.cut_direction $P4Widgets(C14)
    nbs put ${nbsroot}.last_type $P4Widgets(C15)

#  Put logical nbs items
    nbs put ${nbsroot}.plot_axes $P4Widgets(L01)
    nbs put ${nbsroot}.plot_errors $P4Widgets(L02)
    nbs put ${nbsroot}.plot_whole $P4Widgets(L03)
    nbs put ${nbsroot}.pre_erase_plot $P4Widgets(L04)
    nbs put ${nbsroot}.autoscale $P4Widgets(L05)
    nbs put ${nbsroot}.port_ok $P4Widgets(L06)
    nbs put ${nbsroot}.plot_ok $P4Widgets(L07)

#  Put integer nbs items
    nbs put ${nbsroot}.contour_levels $P4Widgets(I01)
    nbs put ${nbsroot}.histogram_bins $P4Widgets(I02)
    nbs put ${nbsroot}.histogram_xstep $P4Widgets(I03)
    nbs put ${nbsroot}.histogram_ystep $P4Widgets(I04)
    nbs put ${nbsroot}.hist_smooth $P4Widgets(I05)
    nbs put ${nbsroot}.toosmall $P4Widgets(I06)
    nbs put ${nbsroot}.toolarge $P4Widgets(I07)
    nbs put ${nbsroot}.istart $P4Widgets(I08)
    nbs put ${nbsroot}.iend $P4Widgets(I09)
    nbs put ${nbsroot}.jstart $P4Widgets(I10)
    nbs put ${nbsroot}.jend $P4Widgets(I11)

#  Put real nbs items
    #nbs put ${nbsroot}.vxstart $P4Widgets(R01)
    #nbs put ${nbsroot}.vxend $P4Widgets(R02)
    #nbs put ${nbsroot}.vystart $P4Widgets(R03)
    #nbs put ${nbsroot}.vyend $P4Widgets(R04)
    #nbs put ${nbsroot}.axstart $P4Widgets(R05)
    #nbs put ${nbsroot}.axend $P4Widgets(R06)
    #nbs put ${nbsroot}.aystart $P4Widgets(R07)
    #nbs put ${nbsroot}.ayend $P4Widgets(R08)
    nbs put ${nbsroot}.xstart $P4Widgets(R09)
    nbs put ${nbsroot}.xend $P4Widgets(R10)
    nbs put ${nbsroot}.ystart $P4Widgets(R11)
    nbs put ${nbsroot}.yend $P4Widgets(R12)
    nbs put ${nbsroot}.mode $P4Widgets(R13)
    nbs put ${nbsroot}.mean $P4Widgets(R14)
    nbs put ${nbsroot}.sigma $P4Widgets(R15)
    nbs put ${nbsroot}.high $P4Widgets(R16)
    nbs put ${nbsroot}.low $P4Widgets(R17)
    nbs put ${nbsroot}.fmin $P4Widgets(R18)
    nbs put ${nbsroot}.fmax $P4Widgets(R19)
    nbs put ${nbsroot}.slice_start $P4Widgets(R20)
    nbs put ${nbsroot}.slice_end $P4Widgets(R21)
    nbs put ${nbsroot}.char_height $P4Widgets(R22)

# Update the display
    update idletasks
}
