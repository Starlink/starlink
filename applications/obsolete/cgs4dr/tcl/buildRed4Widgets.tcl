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
    set Red4Widgets(DDS) "\$RGDIR/rg$env(CGS4_DATE)_gggg_dbs"
    set Red4Widgets(DS) "\$RGDIR/rg$env(CGS4_DATE)_gggg_dbs"
    set Red4Widgets(AP_VARWT) 0
    set Red4Widgets(DSKYWT) 1.0
    return $mainFrame
}
