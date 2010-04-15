proc initP4 taskname {
#+
# Initialises the p4 adam task by sending it a series of OBEY messages
#-
    global env
    global P4NoticeBoard
    set df env(HOME)/cgs4dr_configs/default.p4

# Set the nbs name
    $taskname set noticeboard $P4NoticeBoard -setresponse "cgs4drClear $taskname"

# Open the noticeboard and restore the defaults from file
    $taskname obey open_nb "" -inform "cgs4drInform $taskname %V" -endmsg {set done 1}
    tkwait variable done
    $taskname obey restore "file=$df port=-1" -inform "cgs4drInform $taskname %V" -endmsg {set done 1}
    tkwait variable done

# Initialise NBS values
    nbs put ${P4NoticeBoard}.port_0.display_type "IMAGE"
    nbs put ${P4NoticeBoard}.port_0.device_name "xwindows;$env(PID)xwin"
    nbs put ${P4NoticeBoard}.port_0.display_data "\$P4_CT/cgs4"
    nbs put ${P4NoticeBoard}.port_1.device_name "xwindows;$env(PID)xwin"
    nbs put ${P4NoticeBoard}.port_1.display_data "\$P4_CT/cgs4"
    nbs put ${P4NoticeBoard}.port_2.device_name "xwindows;$env(PID)xwin"
    nbs put ${P4NoticeBoard}.port_2.display_data "\$P4_CT/cgs4"
    nbs put ${P4NoticeBoard}.port_3.device_name "xwindows;$env(PID)xwin"
    nbs put ${P4NoticeBoard}.port_3.display_data "\$P4_CT/cgs4"
    nbs put ${P4NoticeBoard}.port_4.device_name "xwindows;$env(PID)xwin"
    nbs put ${P4NoticeBoard}.port_4.display_data "\$P4_CT/cgs4"
    nbs put ${P4NoticeBoard}.port_5.device_name "xwindows;$env(PID)xwin"
    nbs put ${P4NoticeBoard}.port_5.display_data "\$P4_CT/cgs4"
    nbs put ${P4NoticeBoard}.port_6.device_name "xwindows;$env(PID)xwin"
    nbs put ${P4NoticeBoard}.port_6.display_data "\$P4_CT/cgs4"
    nbs put ${P4NoticeBoard}.port_7.device_name "xwindows;$env(PID)xwin"
    nbs put ${P4NoticeBoard}.port_7.display_data "\$P4_CT/cgs4"
    nbs put ${P4NoticeBoard}.port_8.device_name "xwindows;$env(PID)xwin"
    nbs put ${P4NoticeBoard}.port_8.display_data "\$P4_CT/cgs4"
    nbs put ${P4NoticeBoard}.port_0.title ""
    nbs put ${P4NoticeBoard}.port_0.plot_axes 0

# Load the colour table and plot the ramp
    update idletasks
    $taskname obey lut "port=0" -inform "cgs4drInform $taskname %V" -endmsg "cgs4drClear $taskname"
    set data [string trim [nbs get ${P4NoticeBoard}.port_0.display_data]]
    $taskname obey display "data=$data" -inform "cgs4drInform $taskname %V" -endmsg "set P4Widgets(PORT_NO) 0"
    cgs4drClear $taskname
    $taskname obey status "" -inform "cgs4drInform $taskname %V" -endmsg "nbs put ${P4NoticeBoard}.port_0.plot_axes 1"
}
