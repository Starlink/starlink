proc buildRed4Widgets w {
#+
# This procedure builds the red4 interface widget tree. The names of the
# "active" widgets are stored in the global array "Red4Widgets" so that
# the names of the widgets can be changed without effecting the rest of
# the application.
#
# The only widget bindings set up here are the connection between the
# message text window and its scroll bar.
#
# The return value is the name of the frame widget that contains the
# widget tree.
#-
    global env
    global Red4Widgets

# Create frame for the widget tree.
    set mainFrame [frame $w.main]

# Build panel layout frames.
    set topFrame [frame $mainFrame.ft -relief sunken -bd 2]
    set middleFrame [frame $mainFrame.fm]
    set bottomFrame [frame $mainFrame.fb]
    pack $topFrame $middleFrame $bottomFrame -fill both \
	-expand yes -side top

# Create a listbox
    set Red4Widgets(LB) [listbox $topFrame.listbox \
	-relief raised -bd 2 -width 80]
    set Red4Widgets(SB1) [scrollbar $topFrame.scrollbar -orient vertical \
	-relief sunken -bd 2]

    $Red4Widgets(LB) configure -yscrollcommand "$Red4Widgets(SB1) set"
    $Red4Widgets(SB1)  configure -command "$Red4Widgets(LB) yview"

    pack $Red4Widgets(LB) -in $topFrame -side left -fill x
    pack $Red4Widgets(SB1) -in $topFrame -side right -fill y

# Put some stuff in the Red4Widgets(LB)
    $Red4Widgets(LB) insert end add_integration_to_observation
    $Red4Widgets(LB) insert end add_observation_to_group
    $Red4Widgets(LB) insert end add_pair_to_group
    $Red4Widgets(LB) insert end add_two_images
    $Red4Widgets(LB) insert end apply_bad_pixel_mask
    $Red4Widgets(LB) insert end AND_two_images
    $Red4Widgets(LB) insert end array_tests
    $Red4Widgets(LB) insert end calculate_observation_efficiency
    $Red4Widgets(LB) insert end cgs4list
    $Red4Widgets(LB) insert end clean_observation
    $Red4Widgets(LB) insert end close_engineering_log
    $Red4Widgets(LB) insert end create_mask_by_defining_window
    $Red4Widgets(LB) insert end create_mask_by_thresholding_data_array
    $Red4Widgets(LB) insert end create_mask_by_thresholding_error_array
    $Red4Widgets(LB) insert end delete_integration_from_observation
    $Red4Widgets(LB) insert end delete_reduced_group_file
    $Red4Widgets(LB) insert end delete_reduced_observation_file
    $Red4Widgets(LB) insert end deripple_spectrum
    $Red4Widgets(LB) insert end divide_by_standard
    $Red4Widgets(LB) insert end divide_two_images
    $Red4Widgets(LB) insert end EOR_two_images
    $Red4Widgets(LB) insert end edit_bad_pixel_mask_with_bad_values
    $Red4Widgets(LB) insert end edit_bad_pixel_mask_with_good_values
    $Red4Widgets(LB) insert end examine_data_structure
    $Red4Widgets(LB) insert end extract_bad_pixel_mask
    $Red4Widgets(LB) insert end extract_spectrum
    $Red4Widgets(LB) insert end file_observation
    $Red4Widgets(LB) insert end file_observation_as_arc
    $Red4Widgets(LB) insert end file_observation_as_bad
    $Red4Widgets(LB) insert end file_observation_as_bias
    $Red4Widgets(LB) insert end file_observation_as_calibration
    $Red4Widgets(LB) insert end file_observation_as_dark
    $Red4Widgets(LB) insert end file_observation_as_flat
    $Red4Widgets(LB) insert end file_observation_as_object
    $Red4Widgets(LB) insert end file_observation_as_sky
    $Red4Widgets(LB) insert end file_standard
    $Red4Widgets(LB) insert end flux_calibrate
    $Red4Widgets(LB) insert end get_statistics_on_data
    $Red4Widgets(LB) insert end list_FITS_header_to_file
    $Red4Widgets(LB) insert end list_FITS_header_to_screen
    $Red4Widgets(LB) insert end list_index_to_file
    $Red4Widgets(LB) insert end list_index_to_printer
    $Red4Widgets(LB) insert end list_index_to_screen
    $Red4Widgets(LB) insert end list_index_to_screen2
    $Red4Widgets(LB) insert end log_comment_to_engineering_log
    $Red4Widgets(LB) insert end mend_bad_pixels_by_interpolation
    $Red4Widgets(LB) insert end model_black_body
    $Red4Widgets(LB) insert end multiply_two_images
    $Red4Widgets(LB) insert end normalise_observation
    $Red4Widgets(LB) insert end NOT_an_image
    $Red4Widgets(LB) insert end open_engineering_log
    $Red4Widgets(LB) insert end OR_two_images
    $Red4Widgets(LB) insert end polysky_reduced_observation_or_group
    $Red4Widgets(LB) insert end read_an_emlt_observation_file
    $Red4Widgets(LB) insert end report_emlt_analysis
    $Red4Widgets(LB) insert end reset_engineering_functions
    $Red4Widgets(LB) insert end reset_reduction_task
    $Red4Widgets(LB) insert end set_format_to_DST
    $Red4Widgets(LB) insert end set_format_to_NDF
    $Red4Widgets(LB) insert end status
    $Red4Widgets(LB) insert end subtract_observation_from_group
    $Red4Widgets(LB) insert end subtract_pair_from_group
    $Red4Widgets(LB) insert end subtract_two_images
    $Red4Widgets(LB) insert end two_column_line_fit
    $Red4Widgets(LB) insert end two_row_line_fit
    $Red4Widgets(LB) insert end twod_extracted_area_line_fit
    $Red4Widgets(LB) insert end wavelength_calibrate_using_argon
    $Red4Widgets(LB) insert end wavelength_calibrate_using_krypton
    $Red4Widgets(LB) insert end wavelength_calibrate_using_oh_lines
    $Red4Widgets(LB) insert end wavelength_calibrate_using_xenon

# Set a default output (to item 57 = Status)
    $Red4Widgets(LB) selection set 57

# Create scrolling region for output.
    set Red4Widgets(SB2) [scrollbar $middleFrame.scrollbar -orient vertical \
	-relief sunken -bd 2]
    set Red4Widgets(OUTPUT) [text $middleFrame.text -state disabled \
	-wrap word -relief sunken -bd 2 -width 80]
    $Red4Widgets(SB2) configure -command "$Red4Widgets(OUTPUT) yview"
    $Red4Widgets(OUTPUT) configure -yscroll "$Red4Widgets(SB2) set"

    pack $Red4Widgets(SB2) -in $middleFrame -side right -fill y
    pack $Red4Widgets(OUTPUT) -in $middleFrame \
	-side right -fill both -expand yes

# Define some global values
    set Red4Widgets(DIN) "\$IDIR/i$env(CGS4_DATE)_oooo_iiii"
    set Red4Widgets(IN) "\$IDIR/i$env(CGS4_DATE)_oooo_iiii"
    set Red4Widgets(DOB) "\$ODIR/o$env(CGS4_DATE)_oooo"
    set Red4Widgets(OB) "\$ODIR/o$env(CGS4_DATE)_oooo"
    set Red4Widgets(DRO) "\$RODIR/ro$env(CGS4_DATE)_oooo"
    set Red4Widgets(RO) "\$RODIR/ro$env(CGS4_DATE)_oooo"
    set Red4Widgets(DRG) "\$RGDIR/rg$env(CGS4_DATE)_gggg"
    set Red4Widgets(RG) "\$RGDIR/rg$env(CGS4_DATE)_gggg"
    set Red4Widgets(DCA) "\$RODIR/ca$env(CGS4_DATE)_oooo"
    set Red4Widgets(CA) "\$RODIR/ca$env(CGS4_DATE)_oooo"
    set Red4Widgets(DST) "\$RGDIR/st$env(CGS4_DATE)_gggg"
    set Red4Widgets(ST) "\$RGDIR/st$env(CGS4_DATE)_gggg"
    set Red4Widgets(DSP) "\$RGDIR/rg$env(CGS4_DATE)_gggg_spc"
    set Red4Widgets(SP) "\$RGDIR/rg$env(CGS4_DATE)_gggg_spc"
    set Red4Widgets(DIS) "\$RGDIR/rg$env(CGS4_DATE)_gggg_imspc"
    set Red4Widgets(IS) "\$RGDIR/rg$env(CGS4_DATE)_gggg_imspc"
    set Red4Widgets(DDS) "\$RGDIR/rg$env(CGS4_DATE)_gggg_dbs"
    set Red4Widgets(DS) "\$RGDIR/rg$env(CGS4_DATE)_gggg_dbs"
    set Red4Widgets(IRO1) "\$RODIR/ro$env(CGS4_DATE)_oooo"
    set Red4Widgets(IRO2) "\$RODIR/ro$env(CGS4_DATE)_oooo"
    set Red4Widgets(IRO3) "\$RODIR/ro$env(CGS4_DATE)_oooo"
    set Red4Widgets(CDIR) "$env(ODIR)"
    set Red4Widgets(TLF_DRS) 29
    set Red4Widgets(TLF_DRE) 29
    set Red4Widgets(TLF_DXS) 1
    set Red4Widgets(TLF_DXE) 256
    set Red4Widgets(TLF_RAD01) 1
    set Red4Widgets(TWD_DXST) 1
    set Red4Widgets(TWD_DXEN) 256
    set Red4Widgets(TWD_DYST) 1
    set Red4Widgets(TWD_DYEN) 256
    set Red4Widgets(GS_WHOLE) 1
    set Red4Widgets(GS_AUTOSCALE) 1
    set Red4Widgets(GS_DIST) 1
    set Red4Widgets(GS_DIEN) 256
    set Red4Widgets(GS_DIIN) 1
    set Red4Widgets(GS_DJST) 1
    set Red4Widgets(GS_DJEN) 256
    set Red4Widgets(GS_DJIN) 1
    set Red4Widgets(GS_DHI) 1000.0
    set Red4Widgets(GS_DLO) 0.0
    set Red4Widgets(PF_POLYFIT) REDUCED_GRP
    set Red4Widgets(PF_WEIGHT) 1
    set Red4Widgets(PF_DDEG) 1
    set Red4Widgets(PF_DNRJ) 0
    set Red4Widgets(PF_DYS1) 20
    set Red4Widgets(PF_DYE1) 25
    set Red4Widgets(PF_DYS2) 35
    set Red4Widgets(PF_DYE2) 40
    set Red4Widgets(PF_DYS3) -1
    set Red4Widgets(PF_DYE3) -1
    set Red4Widgets(PF_DYS4) -1
    set Red4Widgets(PF_DYE4) -1
    set Red4Widgets(ATD) 1
    set Red4Widgets(DMS) 0.0
    set Red4Widgets(DMV) 0.0
    set Red4Widgets(FC_IN) 0.0
    set Red4Widgets(FC_FIN) "J"
    set Red4Widgets(FC_FOUT) "W/m2/um"
    set Red4Widgets(BB_DT) 5000.0
    set Red4Widgets(BB_DW) 2.2
    set Red4Widgets(BB_DF) 0
    set Red4Widgets(NO_DPOL) 3
    set Red4Widgets(NO_DBOX) 5
    set Red4Widgets(ENT_TRS) 29
    set Red4Widgets(ENT_TRE) 29
    set Red4Widgets(ENT_MRS) -1
    set Red4Widgets(ENT_MRE) -1
    set Red4Widgets(ENT_BRS) -1
    set Red4Widgets(ENT_BRE) -1
    set Red4Widgets(MBPM) "$env(USER)_mask"
    set Red4Widgets(MBPM_IMIN) 1
    set Red4Widgets(MBPM_JMIN) 1
    set Red4Widgets(MBPM_IMAX) 256
    set Red4Widgets(MBPM_JMAX) 256
    set Red4Widgets(MBPM_NCOL) 256
    set Red4Widgets(MBPM_NROW) 256
    set Red4Widgets(MASK) "#"
    set Red4Widgets(CAL_RST) 1
    set Red4Widgets(CAL_REN) 256
    set Red4Widgets(CAL_ORD) 2
    set Red4Widgets(CAL_SIG) 1
    set Red4Widgets(XST) 0.5
    set Red4Widgets(XEN) 255.5
    set Red4Widgets(AP_VARWT) 0
    set Red4Widgets(DSKYWT) 1.0
    set Red4Widgets(FS_TEFF) 5000.0
    set Red4Widgets(FS_REFW) 2.2
    set Red4Widgets(FS_YST) 29.0
    set Red4Widgets(FS_YEN) 29.0
    set Red4Widgets(SPC_INVERT) 0
    set Red4Widgets(SPC_ALGORITHM) BRIGHT
    return $mainFrame
}
