proc p4GetNbs {root} {
#+
# Stores all nbs items for port in temporary array
#-
    global P4Widgets
    global P4NoticeBoard

#  Get character nbs items
    set P4Widgets(C01) [nbs get ${root}display_data]
    set P4Widgets(C02) [nbs get ${root}device_lut]
    set P4Widgets(C03) [nbs get ${root}title]
    set P4Widgets(C04) [nbs get ${root}device_name]
    set P4Widgets(C05) [nbs get ${root}device_xopt]
    set P4Widgets(C06) [nbs get ${root}device_yopt]
    set P4Widgets(C07) [nbs get ${root}display_type]
    set P4Widgets(C08) [nbs get ${root}display_plane]
    set P4Widgets(C09) [nbs get ${root}contour_type]
    set P4Widgets(C10) [nbs get ${root}overcolour]
    set P4Widgets(C11) [nbs get ${root}colour_style]
    set P4Widgets(C12) [nbs get ${root}fg_colour]
    set P4Widgets(C13) [nbs get ${root}bg_colour]
    set P4Widgets(C14) [nbs get ${root}cut_direction]
    set P4Widgets(C15) [nbs get ${root}last_type]

#  Get logical nbs items
    set P4Widgets(L01) [nbs get ${root}plot_axes]
    set P4Widgets(L02) [nbs get ${root}plot_errors]
    set P4Widgets(L03) [nbs get ${root}plot_whole]
    set P4Widgets(L04) [nbs get ${root}pre_erase_plot]
    set P4Widgets(L05) [nbs get ${root}autoscale]
    set P4Widgets(L06) [nbs get ${root}port_ok]
    set P4Widgets(L07) [nbs get ${root}plot_ok]

#  Get integer nbs items
    set P4Widgets(I01) [nbs get ${root}contour_levels]
    set P4Widgets(I02) [nbs get ${root}histogram_bins]
    set P4Widgets(I03) [nbs get ${root}histogram_xstep]
    set P4Widgets(I04) [nbs get ${root}histogram_ystep]
    set P4Widgets(I05) [nbs get ${root}hist_smooth]
    set P4Widgets(I06) [nbs get ${root}toosmall]
    set P4Widgets(I07) [nbs get ${root}toolarge]
    set P4Widgets(I08) [nbs get ${root}istart]
    set P4Widgets(I09) [nbs get ${root}iend]
    set P4Widgets(I10) [nbs get ${root}jstart]
    set P4Widgets(I11) [nbs get ${root}jend]

#  Get real nbs items
    set P4Widgets(R01) [nbs get ${root}vxstart]
    set P4Widgets(R02) [nbs get ${root}vxend]
    set P4Widgets(R03) [nbs get ${root}vystart]
    set P4Widgets(R04) [nbs get ${root}vyend]
    set P4Widgets(R05) [nbs get ${root}axstart]
    set P4Widgets(R06) [nbs get ${root}axend]
    set P4Widgets(R07) [nbs get ${root}aystart]
    set P4Widgets(R08) [nbs get ${root}ayend]
    set P4Widgets(R09) [nbs get ${root}xstart]
    set P4Widgets(R10) [nbs get ${root}xend]
    set P4Widgets(R11) [nbs get ${root}ystart]
    set P4Widgets(R12) [nbs get ${root}yend]
    set P4Widgets(R13) [nbs get ${root}mode]
    set P4Widgets(R14) [nbs get ${root}mean]
    set P4Widgets(R15) [nbs get ${root}sigma]
    set P4Widgets(R16) [nbs get ${root}high]
    set P4Widgets(R17) [nbs get ${root}low]
    set P4Widgets(R18) [nbs get ${root}fmin]
    set P4Widgets(R19) [nbs get ${root}fmax]
    set P4Widgets(R20) [nbs get ${root}slice_start]
    set P4Widgets(R21) [nbs get ${root}slice_end]
    set P4Widgets(R22) [nbs get ${root}char_height]
}
