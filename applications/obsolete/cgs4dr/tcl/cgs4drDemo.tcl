proc cgs4drDemo {} {
  global env
  global Cred4Task
  global Cred4NoticeBoard
  global QmanTask
  global QmanAccess
  global P4NoticeBoard
  global Red4Task

# Check demo system directory
  if {[catch {glob $env(CGS4DR_ROOT)/demo/*941011*.sdf}] == 1} {
    cgs4drClear $Cred4Task
    set message "cgs4drDemo error : Demo files not installed on this system!"
    cgs4drInform $Cred4Task $message
    return
  }

# Check the date
  if {$env(CGS4_DATE) != "941011"} {
    if {$env(CGS4_DATE) != "19941011"} {
      cgs4drClear $Cred4Task
      set message "cgs4drDemo error : System initialised with wrong date!"
      cgs4drInform $Cred4Task $message
      return
    }
  }

# Get temp directories for demo data
  cgs4drClear $Cred4Task
  cgs4drInform $Cred4Task "Copying data files ... please wait"
  if {[file exists $env(CGS4_DATA)] == 0} {exec /usr/bin/mkdir $env(CGS4_DATA)}
  if {[file exists $env(IDIR)] == 0} {exec /usr/bin/mkdir $env(IDIR)}
  foreach field [lsort [glob $env(CGS4DR_ROOT)/demo/i941011*.sdf]] {
    if {[file exists $env(IDIR)/[file tail $field]] == 0} {exec /usr/bin/cp $field $env(IDIR)/[file tail $field]}
  }
  if {[file exists $env(ODIR)] == 0} {exec /usr/bin/mkdir $env(ODIR)}
  foreach field [lsort [glob $env(CGS4DR_ROOT)/demo/o941011*.sdf]] {
    if {[file exists $env(ODIR)/[file tail $field]] == 0} {exec /usr/bin/cp $field $env(ODIR)/[file tail $field]}
  }
  exec /usr/bin/rm -rf $env(RODIR)
  if {[file exists $env(RODIR)] == 0} {exec /usr/bin/mkdir $env(RODIR)}
  exec /usr/bin/rm -rf $env(RGDIR)
  if {[file exists $env(RGDIR)] == 0} {exec /usr/bin/mkdir $env(RGDIR)}
  foreach field [lsort [glob $env(CGS4DR_ROOT)/demo/*.qman]] {exec /usr/bin/cp $field $env(CGS4_CONFIG)/[file tail $field]}

# Clear the database (NB we're calling Qman but sending messages to Cred4!)
  cgs4drInform $Cred4Task "Loading Qman commands ... please wait"
  $QmanTask obey delete "$QmanAccess delete_mode=all" -inform "cgs4drInform $Cred4Task %V"
  $QmanTask obey restore "$QmanAccess file=$env(CGS4_CONFIG)/cgs4dr_demo1.qman" -inform "cgs4drInform $Cred4Task %V"

# Set some NBS values
  cgs4drInform $Cred4Task "Restoring NBS values ... please wait"
  nbs put ${Cred4NoticeBoard}.reduction.add_int.execute "YES"
  nbs put ${Cred4NoticeBoard}.reduction.add_obs.execute "YES"
  nbs put ${Cred4NoticeBoard}.reduction.archive_obs.execute "NO"
  nbs put ${Cred4NoticeBoard}.reduction.file_obs.execute "YES"
  nbs put ${Cred4NoticeBoard}.reduction.divide_by_ff.execute "NO"
  nbs put ${Cred4NoticeBoard}.reduction.normalise_ff.execute "YES"
  nbs put ${Cred4NoticeBoard}.reduction.normalise_ff.method "POLYFIT"
  nbs put ${Cred4NoticeBoard}.reduction.normalise_ff.order 3
  nbs put ${Cred4NoticeBoard}.reduction.divide_by_std.execute "NO"
  nbs put ${Cred4NoticeBoard}.reduction.subtract_bias.execute "YES"
  nbs put ${Cred4NoticeBoard}.miscellaneous.bias_mode "BOTH"
  nbs put ${Cred4NoticeBoard}.reduction.subtract_dark.execute "NO"
  nbs put ${Cred4NoticeBoard}.reduction.to_wavelength.execute "YES"
  nbs put ${Cred4NoticeBoard}.reduction.to_wavelength.method "ESTIMATED"
  nbs put ${Cred4NoticeBoard}.reduction.autofit.execute "NO"
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.execute "YES"
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row1s 18
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row1e 19
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row2s 29
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row2e 29
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row3s 39
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row3e 40
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.invert FALSE
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.algorithm "BRIGHT"
  nbs put ${Cred4NoticeBoard}.miscellaneous.mask "fpa61_75"
  nbs put ${Cred4NoticeBoard}.miscellaneous.lincoeffs "#"
  nbs put ${Cred4NoticeBoard}.miscellaneous.add_in_pairs TRUE
  nbs put ${Cred4NoticeBoard}.miscellaneous.errors "FROM_OBS"
  nbs put ${Cred4NoticeBoard}.miscellaneous.variance_wt FALSE
  nbs put ${Cred4NoticeBoard}.miscellaneous.sky_wt 1.0
  nbs put ${Cred4NoticeBoard}.miscellaneous.pf_polyfit "NONE"
  nbs put ${Cred4NoticeBoard}.display.spc_p0 "YES"
  nbs put ${Cred4NoticeBoard}.display.spc_p1 "NO"
  nbs put ${Cred4NoticeBoard}.display.spc_p2 "NO"
  nbs put ${Cred4NoticeBoard}.display.spc_p3 "NO"
  nbs put ${Cred4NoticeBoard}.display.spc_p4 "NO"
  nbs put ${Cred4NoticeBoard}.display.spc_p5 "NO"
  nbs put ${Cred4NoticeBoard}.display.spc_p6 "NO"
  nbs put ${Cred4NoticeBoard}.display.spc_p7 "NO"
  nbs put ${Cred4NoticeBoard}.display.spc_p8 "NO"
  nbs put ${Cred4NoticeBoard}.display.obs_p0 "NO"
  nbs put ${Cred4NoticeBoard}.display.obs_p1 "YES"
  nbs put ${Cred4NoticeBoard}.display.obs_p2 "YES"
  nbs put ${Cred4NoticeBoard}.display.obs_p3 "YES"
  nbs put ${Cred4NoticeBoard}.display.obs_p4 "YES"
  nbs put ${Cred4NoticeBoard}.display.obs_p5 "OVERGRAPH PORT=4 CUT=X COLOUR=RED SPOS=18 EPOS=19"
  nbs put ${Cred4NoticeBoard}.display.obs_p6 "OVERGRAPH PORT=4 CUT=X COLOUR=BLUE SPOS=39 EPOS=40"
  nbs put ${Cred4NoticeBoard}.display.obs_p7 "NO"
  nbs put ${Cred4NoticeBoard}.display.obs_p8 "NO"
  nbs put ${Cred4NoticeBoard}.display.grp_p0 "NO"
  nbs put ${Cred4NoticeBoard}.display.grp_p1 "NO"
  nbs put ${Cred4NoticeBoard}.display.grp_p2 "NO"
  nbs put ${Cred4NoticeBoard}.display.grp_p3 "NO"
  nbs put ${Cred4NoticeBoard}.display.grp_p4 "NO"
  nbs put ${Cred4NoticeBoard}.display.grp_p5 "YES"
  nbs put ${Cred4NoticeBoard}.display.grp_p6 "YES"
  nbs put ${Cred4NoticeBoard}.display.grp_p7 "NO"
  nbs put ${Cred4NoticeBoard}.display.grp_p8 "NO"

  nbs put ${P4NoticeBoard}.port_0.device_name "xwindows;$env(PID)xwin"
  nbs put ${P4NoticeBoard}.port_1.device_name "xwindows;$env(PID)xwin"
  nbs put ${P4NoticeBoard}.port_2.device_name "xwindows;$env(PID)xwin"
  nbs put ${P4NoticeBoard}.port_3.device_name "xwindows;$env(PID)xwin"
  nbs put ${P4NoticeBoard}.port_4.device_name "xwindows;$env(PID)xwin"
  nbs put ${P4NoticeBoard}.port_5.device_name "xwindows;$env(PID)xwin"
  nbs put ${P4NoticeBoard}.port_6.device_name "xwindows;$env(PID)xwin"
  nbs put ${P4NoticeBoard}.port_0.device_lut "\$P4_CT/default"
  nbs put ${P4NoticeBoard}.port_1.device_lut "\$P4_CT/default"
  nbs put ${P4NoticeBoard}.port_2.device_lut "\$P4_CT/default"
  nbs put ${P4NoticeBoard}.port_3.device_lut "\$P4_CT/default"
  nbs put ${P4NoticeBoard}.port_4.device_lut "\$P4_CT/default"
  nbs put ${P4NoticeBoard}.port_5.device_lut "\$P4_CT/default"
  nbs put ${P4NoticeBoard}.port_6.device_lut "\$P4_CT/default"
  nbs put ${P4NoticeBoard}.port_0.autoscale TRUE
  nbs put ${P4NoticeBoard}.port_1.autoscale TRUE
  nbs put ${P4NoticeBoard}.port_2.autoscale TRUE
  nbs put ${P4NoticeBoard}.port_3.autoscale TRUE
  nbs put ${P4NoticeBoard}.port_4.autoscale TRUE
  nbs put ${P4NoticeBoard}.port_5.autoscale TRUE
  nbs put ${P4NoticeBoard}.port_6.autoscale TRUE
  nbs put ${P4NoticeBoard}.port_0.plot_whole TRUE
  nbs put ${P4NoticeBoard}.port_1.plot_whole TRUE
  nbs put ${P4NoticeBoard}.port_2.plot_whole TRUE
  nbs put ${P4NoticeBoard}.port_3.plot_whole TRUE
  nbs put ${P4NoticeBoard}.port_4.plot_whole TRUE
  nbs put ${P4NoticeBoard}.port_5.plot_whole TRUE
  nbs put ${P4NoticeBoard}.port_6.plot_whole TRUE
  nbs put ${P4NoticeBoard}.port_0.display_plane "DATA"
  nbs put ${P4NoticeBoard}.port_1.display_plane "DATA"
  nbs put ${P4NoticeBoard}.port_2.display_plane "DATA"
  nbs put ${P4NoticeBoard}.port_3.display_plane "DATA"
  nbs put ${P4NoticeBoard}.port_4.display_plane "DATA"
  nbs put ${P4NoticeBoard}.port_5.display_plane "DATA"
  nbs put ${P4NoticeBoard}.port_6.display_plane "DATA"
  nbs put ${P4NoticeBoard}.port_0.display_type "IMAGE"
  nbs put ${P4NoticeBoard}.port_1.display_type "IMAGE"
  nbs put ${P4NoticeBoard}.port_2.display_type "HISTOGRAM"
  nbs put ${P4NoticeBoard}.port_3.display_type "SURFACE"
  nbs put ${P4NoticeBoard}.port_4.display_type "GRAPH"
  nbs put ${P4NoticeBoard}.port_5.display_type "IMAGE"
  nbs put ${P4NoticeBoard}.port_6.display_type "GRAPH"
  nbs put ${P4NoticeBoard}.port_2.histogram_bins  50
  nbs put ${P4NoticeBoard}.port_2.hist_smooth 1
  nbs put ${P4NoticeBoard}.port_2.histogram_xstep 1
  nbs put ${P4NoticeBoard}.port_2.histogram_ystep 1
  nbs put ${P4NoticeBoard}.port_3.plot_errors FALSE
  nbs put ${P4NoticeBoard}.port_4.plot_errors TRUE
  nbs put ${P4NoticeBoard}.port_4.cut_direction X
  nbs put ${P4NoticeBoard}.port_4.slice_start 29
  nbs put ${P4NoticeBoard}.port_4.slice_end 29
  nbs put ${P4NoticeBoard}.port_6.plot_errors TRUE
  nbs put ${P4NoticeBoard}.port_6.cut_direction X
  nbs put ${P4NoticeBoard}.port_6.slice_start 29
  nbs put ${P4NoticeBoard}.port_6.slice_end 29

  cgs4drClear $Cred4Task
  cgs4drInform $Cred4Task "Click on START REDUCTION when ready"
}

proc cgs4drDemo2 {} {
  global env
  global Cred4Task
  global Cred4NoticeBoard
  global QmanTask
  global QmanAccess
  global P4NoticeBoard
  global Red4Task

# Check demo system directory
  if {[catch {glob $env(CGS4DR_ROOT)/demo/*941011*.sdf}] == 1} {
    cgs4drClear $Cred4Task
    set message "cgs4drDemo error : Demo files not installed on this system!"
    cgs4drInform $Cred4Task $message
    return
  }

# Check the date
  if {$env(CGS4_DATE) != "941011"} {
    if {$env(CGS4_DATE) != "19941011"} {
      cgs4drClear $Cred4Task
      set message "cgs4drDemo error : System initialised with wrong date!"
      cgs4drInform $Cred4Task $message
      return
    }
  }

# Get temp directories for demo data
  cgs4drClear $Cred4Task
  cgs4drInform $Cred4Task "Copying data files ... please wait"
  if {[file exists $env(HOME)/cgs4dr_demo] == 0} {exec /usr/bin/mkdir $env(HOME)/cgs4dr_demo}
  if {[file exists $env(IDIR)] == 0} {exec /usr/bin/mkdir $env(IDIR)}
  foreach field [lsort [glob $env(CGS4DR_ROOT)/demo/i941011*.sdf]] {
    if {[file exists $env(IDIR)/[file tail $field]] == 0} {exec /usr/bin/cp $field $env(IDIR)/[file tail $field]}
  }
  if {[file exists $env(ODIR)] == 0} {exec /usr/bin/mkdir $env(ODIR)}
  foreach field [lsort [glob $env(CGS4DR_ROOT)/demo/o941011*.sdf]] {
    if {[file exists $env(ODIR)/[file tail $field]] == 0} {exec /usr/bin/cp $field $env(ODIR)/[file tail $field]}
  }
  foreach field [lsort [glob $env(CGS4DR_ROOT)/demo/*.qman]] {
    if {[file exists $env(CGS4_CONFIG)/[file tail $field]] == 0} {exec /usr/bin/cp $field $env(CGS4_CONFIG)/[file tail $field]}
  }

# Clear the database (NB we're calling Qman but sending messages to Cred4!)
  cgs4drInform $Cred4Task "Loading Qman commands ... please wait"
  $QmanTask obey delete "$QmanAccess delete_mode=all" -inform "cgs4drInform $Cred4Task %V"
  $QmanTask obey restore "$QmanAccess file=$env(CGS4_CONFIG)/cgs4dr_demo2.qman" -inform "cgs4drInform $Cred4Task %V"

# Set some NBS values
  cgs4drInform $Cred4Task "Restoring NBS values ... please wait"
  nbs put ${Cred4NoticeBoard}.reduction.add_int.execute "YES"
  nbs put ${Cred4NoticeBoard}.reduction.add_obs.execute "YES"
  nbs put ${Cred4NoticeBoard}.reduction.archive_obs.execute "NO"
  nbs put ${Cred4NoticeBoard}.reduction.file_obs.execute "YES"
  nbs put ${Cred4NoticeBoard}.reduction.divide_by_ff.execute "NO"
  nbs put ${Cred4NoticeBoard}.reduction.normalise_ff.execute "YES"
  nbs put ${Cred4NoticeBoard}.reduction.normalise_ff.method "POLYFIT"
  nbs put ${Cred4NoticeBoard}.reduction.normalise_ff.order 3
  nbs put ${Cred4NoticeBoard}.reduction.divide_by_std.execute "YES"
  nbs put ${Cred4NoticeBoard}.miscellaneous.standard_mode "BOTH"
  nbs put ${Cred4NoticeBoard}.reduction.subtract_bias.execute "YES"
  nbs put ${Cred4NoticeBoard}.miscellaneous.bias_mode "BOTH"
  nbs put ${Cred4NoticeBoard}.reduction.subtract_dark.execute "NO"
  nbs put ${Cred4NoticeBoard}.reduction.to_wavelength.execute "YES"
  nbs put ${Cred4NoticeBoard}.reduction.to_wavelength.method "ESTIMATED"
  nbs put ${Cred4NoticeBoard}.reduction.autofit.execute "NO"
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.execute "YES"
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row1s 18
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row1e 19
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row2s 29
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row2e 29
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row3s 39
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row3e 40
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.invert FALSE
  nbs put ${Cred4NoticeBoard}.reduction.extract_spc.algorithm "BRIGHT"
  nbs put ${Cred4NoticeBoard}.miscellaneous.mask "fpa61_75"
  nbs put ${Cred4NoticeBoard}.miscellaneous.lincoeffs "#"
  nbs put ${Cred4NoticeBoard}.miscellaneous.add_in_pairs TRUE
  nbs put ${Cred4NoticeBoard}.miscellaneous.errors "FROM_OBS"
  nbs put ${Cred4NoticeBoard}.miscellaneous.variance_wt FALSE
  nbs put ${Cred4NoticeBoard}.miscellaneous.sky_wt 1.0
  nbs put ${Cred4NoticeBoard}.miscellaneous.pf_polyfit "NONE"
  nbs put ${Cred4NoticeBoard}.display.spc_p0 "YES"
  nbs put ${Cred4NoticeBoard}.display.spc_p1 "NO"
  nbs put ${Cred4NoticeBoard}.display.spc_p2 "NO"
  nbs put ${Cred4NoticeBoard}.display.spc_p3 "NO"
  nbs put ${Cred4NoticeBoard}.display.spc_p4 "NO"
  nbs put ${Cred4NoticeBoard}.display.spc_p5 "NO"
  nbs put ${Cred4NoticeBoard}.display.spc_p6 "NO"
  nbs put ${Cred4NoticeBoard}.display.spc_p7 "NO"
  nbs put ${Cred4NoticeBoard}.display.spc_p8 "NO"
  nbs put ${Cred4NoticeBoard}.display.obs_p0 "NO"
  nbs put ${Cred4NoticeBoard}.display.obs_p1 "YES"
  nbs put ${Cred4NoticeBoard}.display.obs_p2 "NO"
  nbs put ${Cred4NoticeBoard}.display.obs_p3 "NO"
  nbs put ${Cred4NoticeBoard}.display.obs_p4 "YES"
  nbs put ${Cred4NoticeBoard}.display.obs_p5 "OVERGRAPH PORT=4 CUT=X COLOUR=RED SPOS=18 EPOS=19"
  nbs put ${Cred4NoticeBoard}.display.obs_p6 "OVERGRAPH PORT=4 CUT=X COLOUR=BLUE SPOS=39 EPOS=40"
  nbs put ${Cred4NoticeBoard}.display.obs_p7 "NO"
  nbs put ${Cred4NoticeBoard}.display.obs_p8 "NO"
  nbs put ${Cred4NoticeBoard}.display.grp_p0 "NO"
  nbs put ${Cred4NoticeBoard}.display.grp_p1 "NO"
  nbs put ${Cred4NoticeBoard}.display.grp_p2 "NO"
  nbs put ${Cred4NoticeBoard}.display.grp_p3 "NO"
  nbs put ${Cred4NoticeBoard}.display.grp_p4 "NO"
  nbs put ${Cred4NoticeBoard}.display.grp_p5 "YES"
  nbs put ${Cred4NoticeBoard}.display.grp_p6 "YES"
  nbs put ${Cred4NoticeBoard}.display.grp_p7 "NO"
  nbs put ${Cred4NoticeBoard}.display.grp_p8 "NO"

  nbs put ${P4NoticeBoard}.port_0.device_name "xwindows;$env(PID)xwin"
  nbs put ${P4NoticeBoard}.port_1.device_name "xwindows;$env(PID)xwin"
  nbs put ${P4NoticeBoard}.port_2.device_name "xwindows;$env(PID)xwin"
  nbs put ${P4NoticeBoard}.port_3.device_name "xwindows;$env(PID)xwin"
  nbs put ${P4NoticeBoard}.port_4.device_name "xwindows;$env(PID)xwin"
  nbs put ${P4NoticeBoard}.port_5.device_name "xwindows;$env(PID)xwin"
  nbs put ${P4NoticeBoard}.port_6.device_name "xwindows;$env(PID)xwin"
  nbs put ${P4NoticeBoard}.port_0.device_lut "\$P4_CT/default"
  nbs put ${P4NoticeBoard}.port_1.device_lut "\$P4_CT/default"
  nbs put ${P4NoticeBoard}.port_2.device_lut "\$P4_CT/default"
  nbs put ${P4NoticeBoard}.port_3.device_lut "\$P4_CT/default"
  nbs put ${P4NoticeBoard}.port_4.device_lut "\$P4_CT/default"
  nbs put ${P4NoticeBoard}.port_5.device_lut "\$P4_CT/default"
  nbs put ${P4NoticeBoard}.port_6.device_lut "\$P4_CT/default"
  nbs put ${P4NoticeBoard}.port_0.autoscale TRUE
  nbs put ${P4NoticeBoard}.port_1.autoscale TRUE
  nbs put ${P4NoticeBoard}.port_2.autoscale TRUE
  nbs put ${P4NoticeBoard}.port_3.autoscale TRUE
  nbs put ${P4NoticeBoard}.port_4.autoscale TRUE
  nbs put ${P4NoticeBoard}.port_5.autoscale TRUE
  nbs put ${P4NoticeBoard}.port_6.autoscale TRUE
  nbs put ${P4NoticeBoard}.port_0.plot_whole TRUE
  nbs put ${P4NoticeBoard}.port_1.plot_whole TRUE
  nbs put ${P4NoticeBoard}.port_2.plot_whole TRUE
  nbs put ${P4NoticeBoard}.port_3.plot_whole TRUE
  nbs put ${P4NoticeBoard}.port_4.plot_whole TRUE
  nbs put ${P4NoticeBoard}.port_5.plot_whole TRUE
  nbs put ${P4NoticeBoard}.port_6.plot_whole TRUE
  nbs put ${P4NoticeBoard}.port_0.display_plane "DATA"
  nbs put ${P4NoticeBoard}.port_1.display_plane "DATA"
  nbs put ${P4NoticeBoard}.port_2.display_plane "DATA"
  nbs put ${P4NoticeBoard}.port_3.display_plane "DATA"
  nbs put ${P4NoticeBoard}.port_4.display_plane "DATA"
  nbs put ${P4NoticeBoard}.port_5.display_plane "DATA"
  nbs put ${P4NoticeBoard}.port_6.display_plane "DATA"
  nbs put ${P4NoticeBoard}.port_0.display_type "GRAPH"
  nbs put ${P4NoticeBoard}.port_1.display_type "IMAGE"
  nbs put ${P4NoticeBoard}.port_2.display_type "HISTOGRAM"
  nbs put ${P4NoticeBoard}.port_3.display_type "SURFACE"
  nbs put ${P4NoticeBoard}.port_4.display_type "GRAPH"
  nbs put ${P4NoticeBoard}.port_5.display_type "IMAGE"
  nbs put ${P4NoticeBoard}.port_6.display_type "GRAPH"
  nbs put ${P4NoticeBoard}.port_2.histogram_bins  50
  nbs put ${P4NoticeBoard}.port_2.hist_smooth 1
  nbs put ${P4NoticeBoard}.port_2.histogram_xstep 1
  nbs put ${P4NoticeBoard}.port_2.histogram_ystep 1
  nbs put ${P4NoticeBoard}.port_3.plot_errors FALSE
  nbs put ${P4NoticeBoard}.port_4.plot_errors TRUE
  nbs put ${P4NoticeBoard}.port_4.cut_direction X
  nbs put ${P4NoticeBoard}.port_4.slice_start 29
  nbs put ${P4NoticeBoard}.port_4.slice_end 29
  nbs put ${P4NoticeBoard}.port_6.plot_errors TRUE
  nbs put ${P4NoticeBoard}.port_6.cut_direction X
  nbs put ${P4NoticeBoard}.port_6.slice_start 29
  nbs put ${P4NoticeBoard}.port_6.slice_end 29

  cgs4drInform $Cred4Task "Filing \$RGDIR/rg941011_110 as a STANDARD for later use"
  $Red4Task obey file_standard \
     "grpfile=\$RGDIR/rg941011_110 teff=6030 rflambda=3.44 whole=F xstart=1 xend=256 ystart=29 yend=29 oper='OR' mend=T" \
     -inform "cgs4drInform $Cred4Task %V"
}
