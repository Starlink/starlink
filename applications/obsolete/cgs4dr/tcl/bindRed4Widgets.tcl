proc bindRed4Widgets {taskname} {
  global cgs4drHtml
  global Red4Widgets
  bind $Red4Widgets(LB) <Double-Button-1> "red4DoSomething $taskname action \[selection get]\ "
  bind $Red4Widgets(LB) <Button-3>        "red4DoSomething $taskname help   \[selection get]\ "

  global env
  bind $Red4Widgets(SB1)    <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cgs4drScroll.html"
  bind $Red4Widgets(SB2)    <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cgs4drScroll.html"
  bind $Red4Widgets(OUTPUT) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cgs4drTextpane.html"

  global Red4Widgets
  trace variable Red4Widgets(VERBOSE) w "cgs4drVerbose $taskname"
  set Red4Widgets(VERBOSE) 0
}

proc red4DoSomething {taskname dowhat inputs} {
  global env
  global cgs4drHtml

# Lowercase the action and do it
  set inputs [string trim [string tolower $inputs]]
  set dowhat [string trim [string tolower $dowhat]]

# Clear the output text widget
  #if {$dowhat=="action"} {cgs4drClear $taskname}

  switch $inputs {
    add_integration_to_observation
      {
       if {$dowhat=="action"} {
         red4Int $taskname add
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4IntBox1.html
       }
      }
    add_observation_to_group
      {
       if {$dowhat=="action"} {
         red4AddPair $taskname obs
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4AddPairBox1.html
       }
      }
    add_pair_to_group
      {
       if {$dowhat=="action"} {
         red4AddPair $taskname pair
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4AddPairBox2.html
       }
      }
    add_two_images
      {
       if {$dowhat=="action"} {
         red4Iarith $taskname IADD4
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4IarithBox1.html
       }
      }
    apply_bad_pixel_mask
      {
       if {$dowhat=="action"} {
         red4AppMask $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4AppMaskBox1.html
       }
      }
    and_two_images
      {
       if {$dowhat=="action"} {
         red4Iarith $taskname IAND4
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4IarithBox7.html
       }
      }
    array_tests
      {
       if {$dowhat=="action"} {
         red4ArrayTests $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4ArrayTestsBox1.html
       }
      }
    calculate_observation_efficiency
      {
       if {$dowhat=="action"} {
         red4Efficiency $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4EfficiencyBox1.html
       }
      }
    cgs4list
      {
       if {$dowhat=="action"} {
         red4Cgs4List $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4Cgs4ListBox1.html
       }
      }
    clean_observation
      {
       if {$dowhat=="action"} {
         red4CleanObs $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4CleanObsBox1.html
       }
      }
    close_engineering_log
      {
       if {$dowhat=="action"} {
         red4CloseLog $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4CloseLogBox1.html
       }
      }
    create_mask_by_defining_window
      {
       if {$dowhat=="action"} {
         red4CreWinMask $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4CreWinMaskBox1.html
       }
      }
    create_mask_by_thresholding_data_array
      {
       if {$dowhat=="action"} {
         red4CreThreshMask $taskname DATA
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4CreThreshMaskBox1.html
       }
      }
    create_mask_by_thresholding_error_array
      {
       if {$dowhat=="action"} {
         red4CreThreshMask $taskname ERRORS
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4CreThreshMaskBox2.html
       }
      }
    delete_integration_from_observation
      {
       if {$dowhat=="action"} {
         red4Int $taskname delete
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4IntBox2.html
       }
      }
    delete_reduced_group_file
      {
       if {$dowhat=="action"} {
         red4Remove $taskname grp
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4RemoveBox1.html
       }
      }
    delete_reduced_observation_file
      {
       if {$dowhat=="action"} {
         red4Remove $taskname obs
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4RemoveBox2.html
       }
      }
    deripple_spectrum
      {
       if {$dowhat=="action"} {
         red4Derip $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4DeripBox1.html
       }
      }
    divide_by_standard
      {
       if {$dowhat=="action"} {
         red4Dbs $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4DbsBox1.html
       }
      }
    divide_two_images
      {
       if {$dowhat=="action"} {
         red4Iarith $taskname IDIV4
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4IarithBox2.html
       }
      }
    eor_two_images
      {
       if {$dowhat=="action"} {
         red4Iarith $taskname IEOR4
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4IarithBox3.html
       }
      }
    edit_bad_pixel_mask_with_bad_values
      {
       if {$dowhat=="action"} {
         red4EditMask $taskname 1
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4EditMaskBox1.html
       }
      }
    edit_bad_pixel_mask_with_good_values
      {
       if {$dowhat=="action"} {
         red4EditMask $taskname 0
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4EditMaskBox2.html
       }
      }
    examine_data_structure
      {
       if {$dowhat=="action"} {
         #red4Package $taskname figaro1 exam
         red4Package $taskname hdstrace
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4PackageBox1.html
       }
      }
    extract_bad_pixel_mask
      {
       if {$dowhat=="action"} {
         red4ExtMask $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4ExtMaskBox1.html
       }
      }
    extract_spectrum
      {
       if {$dowhat=="action"} {
         red4Ext $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4ExtBox1.html
       }
      }
    file_observation
      {
       if {$dowhat=="action"} {
         red4FileObs $taskname whatever_it_is
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4FileObsBox1.html
       }
      }
    file_observation_as_arc
      {
       if {$dowhat=="action"} {
         red4FileObs $taskname arc
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4FileObsBox2.html
       }
      }
    file_observation_as_bad
      {
       if {$dowhat=="action"} {
         red4FileObs $taskname bad
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4FileObsBox3.html
       }
      }
    file_observation_as_bias
      {
       if {$dowhat=="action"} {
         red4FileObs $taskname bias
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4FileObsBox4.html
       }
      }
    file_observation_as_calibration
      {
       if {$dowhat=="action"} {
         red4FileObs $taskname calib
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4FileCalibBox1.html
       }
      }
    file_observation_as_dark
      {
       if {$dowhat=="action"} {
         red4FileObs $taskname dark
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4FileObsBox5.html
       }
      }
    file_observation_as_flat
      {
       if {$dowhat=="action"} {
         red4FileObs $taskname flat
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4FileObsBox6.html
       }
      }
    file_observation_as_object
      {
       if {$dowhat=="action"} {
         red4FileObs $taskname object
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4FileObsBox7.html
       }
      }
    file_observation_as_sky
      {
       if {$dowhat=="action"} {
         red4FileObs $taskname sky
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4FileObsBox8.html
       }
      }
    file_standard
      {
       if {$dowhat=="action"} {
         red4FileStd $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4FileStdBox1.html
       }
      }
    flux_calibrate
      {
       if {$dowhat=="action"} {
         red4Flux $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4FluxBox1.html
       }
      }
    get_statistics_on_data
      {
       if {$dowhat=="action"} {
         red4Stats $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4StatsBox1.html
       }
      }
    list_fits_header_to_file
      {
       if {$dowhat=="action"} {
         red4Package $taskname ndfpack_mon fitslist file
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4PackageBox2.html
       }
      }
    list_fits_header_to_screen
      {
       if {$dowhat=="action"} {
         red4Package $taskname ndfpack_mon fitslist screen
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4PackageBox3.html
       }
      }
    list_index_to_file
      {
       if {$dowhat=="action"} {
         red4ListIndex $taskname file 1
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4ListIndexBox1.html
       }
      }
    list_index_to_printer
      {
       if {$dowhat=="action"} {
         red4ListIndex $taskname printer 1
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4ListIndexBox2.html
       }
      }
    list_index_to_screen
      {
       if {$dowhat=="action"} {
         red4ListIndex $taskname screen 1
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4ListIndexBox3.html
       }
      }
    list_index_to_screen2
      {
       if {$dowhat=="action"} {
         red4ListIndex $taskname screen 2
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4ListIndexBox4.html
       }
      }
    log_comment_to_engineering_log
      {
       if {$dowhat=="action"} {
         red4LogComment $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4LogCommentBox1.html
       }
      }
    mend_bad_pixels_by_interpolation
      {
       if {$dowhat=="action"} {
         red4Iarith $taskname MEND
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4IarithBox8.html
       }
      }
    model_black_body
      {
       if {$dowhat=="action"} {
         red4ModelBB $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4ModelBBBox1.html
       }
      }
    multiply_two_images
      {
       if {$dowhat=="action"} {
         red4Iarith $taskname IMULT4
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4IarithBox4.html
       }
      }
    normalise_observation
      {
       if {$dowhat=="action"} {
         red4NormObs $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4NormObsBox1.html
       }
      }
    not_an_image
      {
       if {$dowhat=="action"} {
         red4Iarith $taskname INOT4
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4IarithBox9.html
       }
      }
    open_engineering_log
      {
       if {$dowhat=="action"} {
         red4OpenLog $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4OpenLogBox1.html
       }
      }
    or_two_images
      {
       if {$dowhat=="action"} {
         red4Iarith $taskname IOR4
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4IarithBox5.html
       }
      }
    polysky_reduced_observation_or_group
      {
       if {$dowhat=="action"} {
         red4Polysky $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html
       }
      }
    read_an_emlt_observation_file
      {
       if {$dowhat=="action"} {
         red4ReadObs $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4ReadObsBox1.html
       }
      }
    report_emlt_analysis
      {
       if {$dowhat=="action"} {
         red4ReportEmlt $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4ReportEmltBox1.html
       }
      }
    reset_engineering_functions
      {
       if {$dowhat=="action"} {
         red4EngReset $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4EngResetBox1.html
       }
      }
    reset_reduction_task
      {
       if {$dowhat=="action"} {
         red4Reset $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4ResetBox1.html
       }
      }
    set_format_to_dst
      {
       if {$dowhat=="action"} {
         red4Format $taskname DST
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4FormatBox1.html
       }
      }
    set_format_to_ndf
      {
       if {$dowhat=="action"} {
         red4Format $taskname NDF
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4FormatBox2.html
       }
      }
    status
      {
       if {$dowhat=="action"} {
         red4Status $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4StatusBox1.html
       }
      }
    subtract_observation_from_group
      {
       if {$dowhat=="action"} {
         red4SubPair $taskname 1
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4SubPairBox1.html
       }
      }
    subtract_pair_from_group
      {
       if {$dowhat=="action"} {
         red4SubPair $taskname 2
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4SubPairBox2.html
       }
      }
    subtract_two_images
      {
       if {$dowhat=="action"} {
         red4Iarith $taskname ISUB4
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4IarithBox6.html
       }
      }
    two_column_line_fit
      {
       if {$dowhat=="action"} {
         red4TwoLineFit $taskname COLUMN
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4TwoLineFitBox1.html
       }
      }
    two_row_line_fit
      {
       if {$dowhat=="action"} {
         red4TwoLineFit $taskname ROW
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4TwoLineFitBox2.html
       }
      }
    twod_extracted_area_line_fit
      {
       if {$dowhat=="action"} {
         red4TwoDLineFit $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4TwoDLineFitBox1.html
       }
      }
    wavelength_calibrate_using_argon
      {
       if {$dowhat=="action"} {
         red4Calibrate $taskname argon
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4CalibrateBox1.html
       }
      }
    wavelength_calibrate_using_krypton
      {
       if {$dowhat=="action"} {
         red4Calibrate $taskname krypton
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4CalibrateBox2.html
       }
      }
    wavelength_calibrate_using_oh_lines
      {
       if {$dowhat=="action"} {
         red4Calibrate $taskname oh
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4CalibrateBox3.html
       }
      }
    wavelength_calibrate_using_xenon
      {
       if {$dowhat=="action"} {
         red4Calibrate $taskname xenon
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4CalibrateBox4.html
       }
      }
    default
      {
       if {$dowhat=="action"} {
         red4Null $taskname
       } elseif {$dowhat=="help"} {
         cgs4drHelpDialog .helpDialog $cgs4drHtml/red4Null.html
       }
      }
  }
}
