proc p4GetNbs {nbsroot} {
#+
# Stores all nbs items for port in temporary array
#-
    global P4Widgets
    global P4NoticeBoard

#  Get character nbs items
    set P4Widgets(C01) [string trim [nbs get ${nbsroot}.display_data]]
    set P4Widgets(C02) [string trim [nbs get ${nbsroot}.device_lut]]
    set P4Widgets(C03) [string trim [nbs get ${nbsroot}.title]]
    set P4Widgets(C04) [string trim [nbs get ${nbsroot}.device_name]]
    set P4Widgets(C05) [string trim [nbs get ${nbsroot}.device_xopt]]
    set P4Widgets(C06) [string trim [nbs get ${nbsroot}.device_yopt]]
    set P4Widgets(C07) [string trim [nbs get ${nbsroot}.display_type]]
    set P4Widgets(C08) [string trim [nbs get ${nbsroot}.display_plane]]
    set P4Widgets(C09) [string trim [nbs get ${nbsroot}.contour_type]]
    set P4Widgets(C10) [string trim [nbs get ${nbsroot}.overcolour]]
    set P4Widgets(C11) [string trim [nbs get ${nbsroot}.colour_style]]
    set P4Widgets(C12) [string trim [nbs get ${nbsroot}.fg_colour]]
    set P4Widgets(C13) [string trim [nbs get ${nbsroot}.bg_colour]]
    set P4Widgets(C14) [string trim [nbs get ${nbsroot}.cut_direction]]
    set P4Widgets(C15) [string trim [nbs get ${nbsroot}.last_type]]

#  Get logical nbs items
    set P4Widgets(L01) [nbs get ${nbsroot}.plot_axes]
    set P4Widgets(L02) [nbs get ${nbsroot}.plot_errors]
    set P4Widgets(L03) [nbs get ${nbsroot}.plot_whole]
    set P4Widgets(L04) [nbs get ${nbsroot}.pre_erase_plot]
    set P4Widgets(L05) [nbs get ${nbsroot}.autoscale]
    set P4Widgets(L06) [nbs get ${nbsroot}.port_ok]
    set P4Widgets(L07) [nbs get ${nbsroot}.plot_ok]

#  Get integer nbs items
    set P4Widgets(I01) [nbs get ${nbsroot}.contour_levels]
    set P4Widgets(I02) [nbs get ${nbsroot}.histogram_bins]
    set P4Widgets(I03) [nbs get ${nbsroot}.histogram_xstep]
    set P4Widgets(I04) [nbs get ${nbsroot}.histogram_ystep]
    set P4Widgets(I05) [nbs get ${nbsroot}.hist_smooth]
    set P4Widgets(I06) [nbs get ${nbsroot}.toosmall]
    set P4Widgets(I07) [nbs get ${nbsroot}.toolarge]
    set P4Widgets(I08) [nbs get ${nbsroot}.istart]
    set P4Widgets(I09) [nbs get ${nbsroot}.iend]
    set P4Widgets(I10) [nbs get ${nbsroot}.jstart]
    set P4Widgets(I11) [nbs get ${nbsroot}.jend]

#  Get real nbs items
    #set P4Widgets(R01) [nbs get ${nbsroot}.vxstart]
    #set P4Widgets(R02) [nbs get ${nbsroot}.vxend]
    #set P4Widgets(R03) [nbs get ${nbsroot}.vystart]
    #set P4Widgets(R04) [nbs get ${nbsroot}.vyend]
    #set P4Widgets(R05) [nbs get ${nbsroot}.axstart]
    #set P4Widgets(R06) [nbs get ${nbsroot}.axend]
    #set P4Widgets(R07) [nbs get ${nbsroot}.aystart]
    #set P4Widgets(R08) [nbs get ${nbsroot}.ayend]
    set P4Widgets(R09) [nbs get ${nbsroot}.xstart]
    set P4Widgets(R10) [nbs get ${nbsroot}.xend]
    set P4Widgets(R11) [nbs get ${nbsroot}.ystart]
    set P4Widgets(R12) [nbs get ${nbsroot}.yend]
    set P4Widgets(R13) [nbs get ${nbsroot}.mode]
    set P4Widgets(R14) [nbs get ${nbsroot}.mean]
    set P4Widgets(R15) [nbs get ${nbsroot}.sigma]
    set P4Widgets(R16) [nbs get ${nbsroot}.high]
    set P4Widgets(R17) [nbs get ${nbsroot}.low]
    set P4Widgets(R18) [nbs get ${nbsroot}.fmin]
    set P4Widgets(R19) [nbs get ${nbsroot}.fmax]
    set P4Widgets(R20) [nbs get ${nbsroot}.slice_start]
    set P4Widgets(R21) [nbs get ${nbsroot}.slice_end]
    set P4Widgets(R22) [nbs get ${nbsroot}.char_height]
}
