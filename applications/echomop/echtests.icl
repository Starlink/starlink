{ Runs all the ECHOMOP tests from the ICL shell environment }
{  }
FIGARO
LOAD ECHOMOP_EXEC:ECHOMOP
DEFINE LOGFILE ADAM_EXE:LOGFILE
{  }
{  }
INPUT Name of output graphics device NONE
PRINT "Copying test images to local directory"
$ COPY/REPLACE ECHOMOP_DEMO:*test*.sdf *.*
PRINT "This test runs automatic order location with and without"
PRINT "optional frame checking (bad row:column, saturation)."
REPORT test1a
$ echmenu ech_rdctn=ech_test tracim=test_flat inptim=test_obj arc=test_arc ~
  tune_autloc=yes tune_fcheck=no tune_automate="1,EXIT" soft=NONE
LOGFILE LOGFILENAME=test1a OUTPUT=test1a.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test1b
$ echmenu ech_rdctn=ech_test tracim=test_flat inptim=test_obj arc=test_arc ~
  tune_autloc=yes tune_fcheck=yes tune_automate="1,EXIT" soft=NONE
LOGFILE LOGFILENAME=test1b OUTPUT=test1b.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
PRINT "Comparing results with reference."
$ DIFFERENCE ECHOMOP_TEST:test1a.ref test1a
$ DIFFERENCE ECHOMOP_TEST:test1b.ref test1b
{  }
{  }
{  }
{  }
INPUT Name of output graphics device NONE
PRINT "Copying test images to local directory"
$ COPY/REPLACE ECHOMOP_DEMO:*test*.sdf *.*
PRINT "This test runs order tracing using all the available methods."
PRINT "Note that some methods do not trace the test data particularly"
PRINT "well, producing lost traces and untraceable order warnings."
REPORT test2a
ech_trace ech_rdctn=ech_test idx_num_orders=0 tracim=test_flat trcfit=poly ~
 trc_npoly=3 trace_mode=tc soft=NONE
LOGFILE LOGFILENAME= test2a OUTPUT=test2a.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test2b
ech_trace ech_rdctn=ech_test idx_num_orders=0 tracim=test_flat trcfit=poly ~
 trc_npoly=7 trace_mode=c soft=NONE
LOGFILE LOGFILENAME= test2b OUTPUT=test2b.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test2c
ech_trace ech_rdctn=ech_test idx_num_orders=0 tracim=test_flat trcfit=poly ~
 trc_npoly=7 trace_mode=b soft=NONE
LOGFILE LOGFILENAME= test2c OUTPUT=test2c.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test2d
ech_trace ech_rdctn=ech_test idx_num_orders=0 tracim=test_flat trcfit=poly ~
 trc_npoly=7 trace_mode=g soft=NONE
LOGFILE LOGFILENAME= test2d OUTPUT=test2d.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test2e
ech_trace ech_rdctn=ech_test idx_num_orders=0 tracim=test_flat trcfit=poly ~
 trc_npoly=7 trace_mode=tc soft=NONE
LOGFILE LOGFILENAME= test2e OUTPUT=test2e.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test2f
ech_trace ech_rdctn=ech_test idx_num_orders=0 tracim=test_flat trcfit=poly ~
 trc_npoly=7 trace_mode=tb soft=NONE
LOGFILE LOGFILENAME= test2f OUTPUT=test2f.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test2g
ech_trace ech_rdctn=ech_test idx_num_orders=0 tracim=test_flat trcfit=poly ~
 trc_npoly=7 trace_mode=tg soft=NONE
LOGFILE LOGFILENAME= test2g OUTPUT=test2g.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test2h
ech_trace ech_rdctn=ech_test idx_num_orders=0 tracim=test_flat trcfit=poly ~
 trc_npoly=7 trace_mode=tag soft=NONE
LOGFILE LOGFILENAME= test2h OUTPUT=test2h.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test2i
ech_trace ech_rdctn=ech_test idx_num_orders=0 tracim=test_flat trcfit=spline ~
 trc_npoly=16 trace_mode=e soft=NONE
LOGFILE LOGFILENAME= test2i OUTPUT=test2i.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test2j
ech_trace ech_rdctn=ech_test idx_num_orders=0 tracim=test_flat trcfit=spline ~
 trc_npoly=16 trace_mode=c soft=NONE
LOGFILE LOGFILENAME= test2j OUTPUT=test2j.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test2k
ech_trace ech_rdctn=ech_test idx_num_orders=0 tracim=test_flat trcfit=spline ~
 trc_npoly=16 trace_mode=b soft=NONE
LOGFILE LOGFILENAME= test2k OUTPUT=test2k.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test2l
ech_trace ech_rdctn=ech_test idx_num_orders=0 tracim=test_flat trcfit=spline ~
 trc_npoly=16 trace_mode=g soft=NONE
LOGFILE LOGFILENAME= test2l OUTPUT=test2l.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test2m
ech_trace ech_rdctn=ech_test idx_num_orders=0 tracim=test_flat trcfit=spline ~
 trc_npoly=16 trace_mode=tc soft=NONE
LOGFILE LOGFILENAME= test2m OUTPUT=test2m.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test2n
ech_trace ech_rdctn=ech_test idx_num_orders=0 tracim=test_flat trcfit=spline ~
 trc_npoly=16 trace_mode=tb soft=NONE
LOGFILE LOGFILENAME= test2n OUTPUT=test2n.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test2o
ech_trace ech_rdctn=ech_test idx_num_orders=0 tracim=test_flat trcfit=spline ~
 trc_npoly=16 trace_mode=tg soft=NONE
LOGFILE LOGFILENAME= test2o OUTPUT=test2o.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test2p
ech_trace ech_rdctn=ech_test idx_num_orders=0 tracim=test_flat trcfit=spline ~
 trc_npoly=16 trace_mode=tag soft=NONE
LOGFILE LOGFILENAME= test2p OUTPUT=test2p.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
PRINT "Comparing results with reference."
$ DIFFERENCE ECHOMOP_TEST:test2a.ref test2a
$ DIFFERENCE ECHOMOP_TEST:test2b.ref test2b
$ DIFFERENCE ECHOMOP_TEST:test2c.ref test2c
$ DIFFERENCE ECHOMOP_TEST:test2d.ref test2d
$ DIFFERENCE ECHOMOP_TEST:test2e.ref test2e
$ DIFFERENCE ECHOMOP_TEST:test2f.ref test2f
$ DIFFERENCE ECHOMOP_TEST:test2g.ref test2g
$ DIFFERENCE ECHOMOP_TEST:test2h.ref test2h
$ DIFFERENCE ECHOMOP_TEST:test2i.ref test2i
$ DIFFERENCE ECHOMOP_TEST:test2j.ref test2j
$ DIFFERENCE ECHOMOP_TEST:test2k.ref test2k
$ DIFFERENCE ECHOMOP_TEST:test2l.ref test2l
$ DIFFERENCE ECHOMOP_TEST:test2m.ref test2m
$ DIFFERENCE ECHOMOP_TEST:test2n.ref test2n
$ DIFFERENCE ECHOMOP_TEST:test2o.ref test2o
$ DIFFERENCE ECHOMOP_TEST:test2p.ref test2p
{  }
{  }
{  }
{  }
INPUT Name of output graphics device NONE
PRINT "Copying test images to local directory"
$ COPY/REPLACE ECHOMOP_DEMO:*test*.sdf *.*
PRINT "This test checks the order fit refinement using both polynomials"
PRINT "and splines for a variety of clipping limits."
REPORT test3a
$ ech_fitord ech_rdctn=ech_test idx_num_orders=0 trcfit=poly trc_interact=no  ~
  trc_npoly=7 soft=NONE
LOGFILE LOGFILENAME= test3a OUTPUT=test3a.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test3b
$ ech_fitord ech_rdctn=ech_test idx_num_orders=0 trcfit=poly trc_interact=no  ~
  trc_npoly=7 tune_clpmxdev=1.0 soft=NONE
LOGFILE LOGFILENAME= test3b OUTPUT=test3b.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test3c
$ ech_fitord ech_rdctn=ech_test idx_num_orders=0 trcfit=poly trc_interact=no  ~
  trc_npoly=7 tune_clpmxdev=0.1 soft=NONE
LOGFILE LOGFILENAME= test3c OUTPUT=test3c.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test3d
$ ech_fitord ech_rdctn=ech_test idx_num_orders=0 trcfit=poly trc_interact=no  ~
  trc_npoly=7 tune_clpmxdev=0.05 soft=NONE
LOGFILE LOGFILENAME= test3d OUTPUT=test3d.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test3e
$ ech_fitord ech_rdctn=ech_test idx_num_orders=0 trcfit=spline trc_interact=no  ~
  trc_npoly=20 soft=NONE
LOGFILE LOGFILENAME= test3e OUTPUT=test3e.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test3f
$ ech_fitord ech_rdctn=ech_test idx_num_orders=0 trcfit=spline trc_interact=no  ~
  trc_npoly=20 tune_clpmxdev=1.0 soft=NONE
LOGFILE LOGFILENAME= test3f OUTPUT=test3f.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test3g
$ ech_fitord ech_rdctn=ech_test idx_num_orders=0 trcfit=spline trc_interact=no  ~
  trc_npoly=20 tune_clpmxdev=0.1 soft=NONE
LOGFILE LOGFILENAME= test3g OUTPUT=test3g.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test3h
$ ech_fitord ech_rdctn=ech_test idx_num_orders=0 trcfit=spline trc_interact=no  ~
  trc_npoly=20 tune_clpmxdev=0.05 soft=NONE
LOGFILE LOGFILENAME= test3h OUTPUT=test3h.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
PRINT "Comparing results with reference."
$ DIFFERENCE ECHOMOP_TEST:test3a.ref test3a
$ DIFFERENCE ECHOMOP_TEST:test3b.ref test3b
$ DIFFERENCE ECHOMOP_TEST:test3c.ref test3c
$ DIFFERENCE ECHOMOP_TEST:test3d.ref test3d
$ DIFFERENCE ECHOMOP_TEST:test3e.ref test3e
$ DIFFERENCE ECHOMOP_TEST:test3f.ref test3f
$ DIFFERENCE ECHOMOP_TEST:test3g.ref test3g
$ DIFFERENCE ECHOMOP_TEST:test3h.ref test3h
{  }
{  }
{  }
{  }
{  }
INPUT Name of output graphics device NONE
PRINT "Copying test images to local directory"
$ COPY/REPLACE ECHOMOP_DEMO:*test*.sdf *.*
PRINT "This test check the behaviour of the dekker and object spatial"
PRINT "limit calculations. It then tests the ability to set the limits"
PRINT "directly using tunable parameters."
REPORT test4a
$ ech_spatial ech_rdctn=ech_test slitim=test_flat pfl_interact=no ~
  inptim=test_obj soft=NONE
LOGFILE LOGFILENAME= test4a OUTPUT=test4a.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test4b
$ ech_spatial ech_rdctn=ech_test slitim=test_flat pfl_interact=no ~
  inptim=test_obj tune_dekthr=0.6 soft=NONE
LOGFILE LOGFILENAME= test4b OUTPUT=test4b.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test4c
$ ech_spatial ech_rdctn=ech_test slitim=test_flat pfl_interact=no ~
  inptim=test_obj tune_dekthr=0.4 soft=NONE
LOGFILE LOGFILENAME= test4c OUTPUT=test4c.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test4d
$ ech_spatial ech_rdctn=ech_test slitim=test_flat pfl_interact=no ~
  inptim=test_obj tune_dekthr=0.2 soft=NONE
LOGFILE LOGFILENAME= test4d OUTPUT=test4d.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test4e
$ ech_spatial ech_rdctn=ech_test slitim=test_flat pfl_interact=no ~
  inptim=test_obj tune_skyhilim=1.01 soft=NONE
LOGFILE LOGFILENAME= test4e OUTPUT=test4e.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test4f
$ ech_spatial ech_rdctn=ech_test slitim=test_flat pfl_interact=no ~
  inptim=test_obj tune_dekthr=0.6  tune_skyhilim=1.25 soft=NONE
LOGFILE LOGFILENAME= test4f OUTPUT=test4f.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test4g
$ ech_spatial ech_rdctn=ech_test slitim=test_flat pfl_interact=no ~
  inptim=test_obj tune_dekthr=0.4  tune_skyhilim=1.5 soft=NONE
LOGFILE LOGFILENAME= test4g OUTPUT=test4g.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test4h
$ ech_spatial ech_rdctn=ech_test  tune_skyhilim=1.05 slitim=test_flat pfl_interact=no ~
  inptim=test_obj tune_dekthr=0.2 soft=NONE
LOGFILE LOGFILENAME= test4h OUTPUT=test4h.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test4i
$ ech_spatial ech_rdctn=ech_test slitim=test_flat pfl_interact=no ~
  inptim=test_obj soft=NONE tune_dekblw=-9 ~
  tune_dekabv=9 tune_objblw=-7 tune_objabv=3
LOGFILE LOGFILENAME= test4i OUTPUT=test4i.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
PRINT "Comparing results with reference."
$ DIFFERENCE ECHOMOP_TEST:test4a.ref test4a
$ DIFFERENCE ECHOMOP_TEST:test4b.ref test4b
$ DIFFERENCE ECHOMOP_TEST:test4c.ref test4c
$ DIFFERENCE ECHOMOP_TEST:test4d.ref test4d
$ DIFFERENCE ECHOMOP_TEST:test4e.ref test4e
$ DIFFERENCE ECHOMOP_TEST:test4f.ref test4f
$ DIFFERENCE ECHOMOP_TEST:test4g.ref test4g
$ DIFFERENCE ECHOMOP_TEST:test4h.ref test4h
$ DIFFERENCE ECHOMOP_TEST:test4i.ref test4i
{  }
{  }
{  }
{  }
INPUT Name of output graphics device NONE
PRINT "Copying test images to local directory"
$ COPY/REPLACE ECHOMOP_DEMO:*test*.sdf *.*
PRINT "This test checks the flat=field photometric correction calculations."
PRINT "Models with x:y components and y only components are checked."
PRINT "Functions used during fitting are mean:median:smooth:slope:spline:poly"
REPORT test5a
$ ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=none ~
   soft=NONE
LOGFILE LOGFILENAME= test5a OUTPUT=test5a.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test5b
$ ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=mean ~
   soft=NONE
LOGFILE LOGFILENAME= test5b OUTPUT=test5b.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test5c
$ ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=median ~
   soft=NONE
LOGFILE LOGFILENAME= test5c OUTPUT=test5c.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test5d
$ ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=smooth ~
   soft=NONE
LOGFILE LOGFILENAME= test5d OUTPUT=test5d.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test5e
$ ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=slope ~
   soft=NONE
LOGFILE LOGFILENAME= test5e OUTPUT=test5e.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test5f
$ ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=mean ~
   soft=NONE tune_fflsmp=31
LOGFILE LOGFILENAME= test5f OUTPUT=test5f.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test5g
$ ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=median ~
   soft=NONE tune_fflsmp=31
LOGFILE LOGFILENAME= test5g OUTPUT=test5g.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test5h
$ ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=smooth ~
   soft=NONE tune_fflsmp=31
LOGFILE LOGFILENAME= test5h OUTPUT=test5h.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test5i
$ ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=slope ~
   soft=NONE tune_fflsmp=31
LOGFILE LOGFILENAME= test5i OUTPUT=test5i.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test5j
$ ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=poly ~
   soft=NONE tune_ffnxply=0
LOGFILE LOGFILENAME= test5j OUTPUT=test5j.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test5k
$ ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=poly ~
   soft=NONE tune_ffnxply=5
LOGFILE LOGFILENAME= test5k OUTPUT=test5k.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test5l
$ ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=poly ~
   soft=NONE tune_ffnyply=3
LOGFILE LOGFILENAME= test5l OUTPUT=test5l.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test5m
$ ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=poly ~
   soft=NONE tune_ffsubsmp=yes
LOGFILE LOGFILENAME= test5m OUTPUT=test5m.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test5n
$ ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=spline ~
   soft=NONE tune_ffnxply=0
LOGFILE LOGFILENAME= test5n OUTPUT=test5n.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test5o
$ ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=spline ~
 soft=NONE tune_ffnxply=18
LOGFILE LOGFILENAME= test5o OUTPUT=test5o.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test5p
$ ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=spline ~
   soft=NONE tune_ffnyply=16
LOGFILE LOGFILENAME= test5p OUTPUT=test5p.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test5q
$ ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=spline ~
   soft=NONE tune_fflsubsmp=yes
LOGFILE LOGFILENAME= test5q OUTPUT=test5q.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
PRINT "Comparing results with reference."
$ DIFFERENCE ECHOMOP_TEST:test5a.ref test5a
$ DIFFERENCE ECHOMOP_TEST:test5b.ref test5b
$ DIFFERENCE ECHOMOP_TEST:test5c.ref test5c
$ DIFFERENCE ECHOMOP_TEST:test5d.ref test5d
$ DIFFERENCE ECHOMOP_TEST:test5e.ref test5e
$ DIFFERENCE ECHOMOP_TEST:test5f.ref test5f
$ DIFFERENCE ECHOMOP_TEST:test5g.ref test5g
$ DIFFERENCE ECHOMOP_TEST:test5h.ref test5h
$ DIFFERENCE ECHOMOP_TEST:test5i.ref test5i
$ DIFFERENCE ECHOMOP_TEST:test5j.ref test5j
$ DIFFERENCE ECHOMOP_TEST:test5k.ref test5k
$ DIFFERENCE ECHOMOP_TEST:test5l.ref test5l
$ DIFFERENCE ECHOMOP_TEST:test5m.ref test5m
$ DIFFERENCE ECHOMOP_TEST:test5n.ref test5n
$ DIFFERENCE ECHOMOP_TEST:test5o.ref test5o
$ DIFFERENCE ECHOMOP_TEST:test5p.ref test5p
$ DIFFERENCE ECHOMOP_TEST:test5q.ref test5q
{  }
{  }
{  }
{  }
INPUT Name of output graphics device NONE
PRINT "Copying test images to local directory"
$ COPY/REPLACE ECHOMOP_DEMO:*test*.sdf *.*
PRINT "This test checks the sky modelling functions. A variety of options"
PRINT "are used, including mean,polynomial and spline fitting. Functions"
PRINT "in both x and y directions are tested and the optional monte-carlo"
PRINT "simulation mode is invoked."
REPORT test6a
$ ech_sky ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat ~
  skyfit=mean inptim=test_obj  ~
  readout_noise=0 photon_to_adu=1 soft=NONE
LOGFILE LOGFILENAME= test6a OUTPUT=test6a.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test6b
$ ech_sky ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat ~
  skyfit=poly tune_skypoly=3 inptim=test_obj  ~
  readout_noise=0 photon_to_adu=1 soft=NONE
LOGFILE LOGFILENAME= test6b OUTPUT=test6b.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test6c
$ ech_sky ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat ~
  skyfit=spline tune_skypoly=20 inptim=test_obj  ~
  readout_noise=0 photon_to_adu=1 soft=NONE
LOGFILE LOGFILENAME= test6c OUTPUT=test6c.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test6d
$ ech_sky ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat ~
  skyfit=poly tune_skypoly=3 tune_skyxply=5 inptim=test_obj  ~
  readout_noise=0 photon_to_adu=1 soft=NONE
LOGFILE LOGFILENAME= test6d OUTPUT=test6d.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test6e
$ ech_sky ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat ~
  skyfit=poly tune_skypoly=3 tune_skyxply=5 tune_skyrthr=100 inptim=test_obj  ~
  readout_noise=0 photon_to_adu=1 soft=NONE
LOGFILE LOGFILENAME= test6e OUTPUT=test6e.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test6f
$ ech_sky ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat ~
  skyfit=poly tune_skyxply=5 tune_skyrthr=5 tune_skysim=yes inptim=test_obj  ~
  readout_noise=0 photon_to_adu=1 soft=NONE
LOGFILE LOGFILENAME= test6f OUTPUT=test6f.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test6g
$ ech_sky ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat ~
  skyfit=NONE inptim=test_obj  ~
  readout_noise=0 photon_to_adu=1 soft=NONE
LOGFILE LOGFILENAME= test6g OUTPUT=test6g.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
PRINT "Comparing results with reference."
$ DIFFERENCE ECHOMOP_TEST:test6a.ref test6a
$ DIFFERENCE ECHOMOP_TEST:test6b.ref test6b
$ DIFFERENCE ECHOMOP_TEST:test6c.ref test6c
$ DIFFERENCE ECHOMOP_TEST:test6d.ref test6d
$ DIFFERENCE ECHOMOP_TEST:test6e.ref test6e
$ DIFFERENCE ECHOMOP_TEST:test6f.ref test6f
$ DIFFERENCE ECHOMOP_TEST:test6g.ref test6g
{  }
{  }
{  }
{  }
INPUT Name of output graphics device NONE
PRINT "Copying test images to local directory"
$ COPY/REPLACE ECHOMOP_DEMO:*test*.sdf *.*
PRINT "This test checks the behaviour of the object profile calculation."
PRINT "Options tested include all-order-average, per-order-profile, a"
PRINT "variety of sample region sizes, and the optional polynomial"
PRINT "fitting in the wavelength direction."
REPORT test7a
$ ech_profile ech_rdctn=ech_test inptim=test_obj tune_use_nxf=0.2 ~
  pfl_interact=no tune_objpoly=0 soft=NONE
LOGFILE LOGFILENAME=test7a OUTPUT=test7a.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test7b
$ ech_profile ech_rdctn=ech_test inptim=test_obj tune_use_nxf=0.5 ~
  pfl_interact=no tune_objpoly=0 soft=NONE
LOGFILE LOGFILENAME=test7b OUTPUT=test7b.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test7c
$ ech_profile ech_rdctn=ech_test inptim=test_obj tune_use_nxf=0.8 ~
  pfl_interact=no tune_objpoly=0 soft=NONE
LOGFILE LOGFILENAME=test7c OUTPUT=test7c.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test7d
$ ech_profile ech_rdctn=ech_test inptim=test_obj tune_use_nxf=1.0 ~
  pfl_interact=no tune_objpoly=0 soft=NONE
LOGFILE LOGFILENAME=test7d OUTPUT=test7d.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test7e
$ ech_profile ech_rdctn=ech_test inptim=test_obj tune_objpoly=5 ~
  pfl_interact=no objfit=poly soft=NONE
LOGFILE LOGFILENAME=test7e OUTPUT=test7e.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
PRINT "Comparing results with reference."
$ DIFFERENCE ECHOMOP_TEST:test7a.ref test7a
$ DIFFERENCE ECHOMOP_TEST:test7b.ref test7b
$ DIFFERENCE ECHOMOP_TEST:test7c.ref test7c
$ DIFFERENCE ECHOMOP_TEST:test7d.ref test7d
$ DIFFERENCE ECHOMOP_TEST:test7e.ref test7e
{  }
{  }
{  }
{  }
INPUT Name of output graphics device NONE
PRINT "Copying test images to local directory"
$ COPY/REPLACE ECHOMOP_DEMO:*test*.sdf *.*
PRINT "This test check the process of arc line candidate location."
PRINT "A variety of threshold setting are used leading to different
PRINT "sets of line candidates."
REPORT test8a
$ ech_linloc ech_rdctn=ech_test arc=test_arc  ~
   tune_rflnthr=1.25 soft=NONE
LOGFILE LOGFILENAME= test8a OUTPUT=test8a.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test8b
$ ech_linloc ech_rdctn=ech_test arc=test_arc  ~
   tune_rflnthr=2.0 soft=NONE
LOGFILE LOGFILENAME= test8b OUTPUT=test8b.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test8c
$ ech_linloc ech_rdctn=ech_test arc=test_arc  ~
   tune_rflnthr=5.0 soft=NONE
LOGFILE LOGFILENAME= test8c OUTPUT=test8c.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test8d
$ ech_linloc ech_rdctn=ech_test arc=test_arc  ~
   tune_rflnthr=25.0 soft=NONE
LOGFILE LOGFILENAME= test8d OUTPUT=test8d.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
PRINT "Comparing results with reference."
$ DIFFERENCE ECHOMOP_TEST:test8a.ref test8a
$ DIFFERENCE ECHOMOP_TEST:test8b.ref test8b
$ DIFFERENCE ECHOMOP_TEST:test8c.ref test8c
$ DIFFERENCE ECHOMOP_TEST:test8d.ref test8d
{  }
{  }
{  }
{  }
INPUT Name of output graphics device NONE
PRINT "Copying test images to local directory"
$ COPY/REPLACE ECHOMOP_DEMO:*test*.sdf *.*
PRINT "This test checks the behaviour of the automatic wavelength calibration"
PRINT "task. It first runs arc line candidate location to ensure a good"
PRINT "selection of identifiable lines. Various search ranges for dispersion"
PRINT "and wavelength are then tested. Polynomial and spline fits are used."
$ ech_linloc ech_rdctn=ech_test arc=test_arc  ~
   tune_rflnthr=1.25 soft=NONE
REPORT test9a
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR tune_wcal_index=5000 ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes wavfit=poly w_npoly=7 ~
  min_dispersion=0.02 max_dispersion=0.1 low_wave=4200 ~
  hi_wave=4500  soft=NONE
LOGFILE LOGFILENAME= test9a OUTPUT=test9a.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
$ DELOBJ ech_test.more.echelle.id_lines
$ DELOBJ ech_test.more.echelle.id_count
REPORT test9b
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes wavfit=poly w_npoly=7 ~
  min_dispersion=0.01 max_dispersion=0.2 low_wave=4200 ~
  hi_wave=4500  soft=NONE
LOGFILE LOGFILENAME= test9b OUTPUT=test9b.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
$ DELOBJ ech_test.more.echelle.id_lines
$ DELOBJ ech_test.more.echelle.id_count
REPORT test9c
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes w_npoly=7 ~
  min_dispersion=0.02 max_dispersion=0.1 wavfit=poly  low_wave=4000 ~
  hi_wave=5000  soft=NONE
LOGFILE LOGFILENAME= test9c OUTPUT=test9c.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
$ DELOBJ ech_test.more.echelle.id_lines
$ DELOBJ ech_test.more.echelle.id_count
REPORT test9d
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes  w_npoly=7 ~
  min_dispersion=0.02 max_dispersion=0.1 wavfit=poly  low_wave=3800 ~
  hi_wave=9000  soft=NONE
LOGFILE LOGFILENAME= test9d OUTPUT=test9d.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
$ DELOBJ ech_test.more.echelle.id_lines
$ DELOBJ ech_test.more.echelle.id_count
REPORT test9e
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes  w_npoly=20 ~
  min_dispersion=0.02 max_dispersion=0.1 wavfit=spline  low_wave=4200 ~
  hi_wave=4500  soft=NONE
LOGFILE LOGFILENAME= test9e OUTPUT=test9e.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
$ DELOBJ ech_test.more.echelle.id_lines
$ DELOBJ ech_test.more.echelle.id_count
REPORT test9f
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes w_npoly=20 ~
  min_dispersion=0.01 max_dispersion=0.2 wavfit=spline  low_wave=4200 ~
  hi_wave=4500  soft=NONE
LOGFILE LOGFILENAME= test9f OUTPUT=test9f.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
$ DELOBJ ech_test.more.echelle.id_lines
$ DELOBJ ech_test.more.echelle.id_count
REPORT test9g
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes  w_npoly=20 ~
  min_dispersion=0.02 max_dispersion=0.1 wavfit=spline  low_wave=4000 ~
  hi_wave=5000  soft=NONE
LOGFILE LOGFILENAME= test9g OUTPUT=test9g.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
$ DELOBJ ech_test.more.echelle.id_lines
$ DELOBJ ech_test.more.echelle.id_count
REPORT test9h
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes w_poly=20 ~
  min_dispersion=0.02 max_dispersion=0.1 wavfit=spline  low_wave=3800 ~
  hi_wave=9000  soft=NONE
LOGFILE LOGFILENAME= test9h OUTPUT=test9h.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
$ DELOBJ ech_test.more.echelle.id_lines
$ DELOBJ ech_test.more.echelle.id_count
REPORT test9i
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes wavfit=poly w_npoly=7 ~
  min_dispersion=0.02 max_dispersion=0.1 low_wave=4200 ~
  hi_wave=4500  soft=NONE
LOGFILE LOGFILENAME= test9i OUTPUT=test9i.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
PRINT "Comparing results with reference."
$ DIFFERENCE ECHOMOP_TEST:test9a.ref test9a
$ DIFFERENCE ECHOMOP_TEST:test9b.ref test9b
$ DIFFERENCE ECHOMOP_TEST:test9c.ref test9c
$ DIFFERENCE ECHOMOP_TEST:test9d.ref test9d
$ DIFFERENCE ECHOMOP_TEST:test9e.ref test9e
$ DIFFERENCE ECHOMOP_TEST:test9f.ref test9f
$ DIFFERENCE ECHOMOP_TEST:test9g.ref test9g
$ DIFFERENCE ECHOMOP_TEST:test9h.ref test9h
$ DIFFERENCE ECHOMOP_TEST:test9i.ref test9i
{  }
{  }
{  }
{  }
INPUT Name of output graphics device NONE
PRINT "Copying test images to local directory"
$ COPY/REPLACE ECHOMOP_DEMO:*test*.sdf *.*
PRINT "This test checks the behaviour of the scrunching task in both"
PRINT "global and per-order binsizes. The options for automatically"
PRINT "calculating and user-specifying wavelength scales are tested,"
PRINT "as are options for conserving (or not) flux."
REPORT test10a
ech_scrunch ech_rdctn=ech_test set_wscale=yes bin_size=0 start_wave=0  ~
  scrunch_type=obj soft=NONE
LOGFILE LOGFILENAME= test10a OUTPUT=test10a.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test10b
ech_scrunch ech_rdctn=ech_test set_wscale=no bin_size=0 start_wave=0  ~
  scrunch_type=obj soft=NONE
LOGFILE LOGFILENAME= test10b OUTPUT=test10b.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test10c
ech_scrunch ech_rdctn=ech_test set_wscale=yes bin_size=1 start_wave=4200  ~
  scrunch_type=obj soft=NONE
LOGFILE LOGFILENAME= test10c OUTPUT=test10c.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test10d
ech_scrunch ech_rdctn=ech_test set_wscale=no bin_size=1 start_wave=4200  ~
  scrunch_type=obj soft=NONE
LOGFILE LOGFILENAME= test10d OUTPUT=test10d.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test10e
ech_scrunch ech_rdctn=ech_test set_wscale=yes bin_size=0 start_wave=0  ~
  tune_flux=yes scrunch_type=obj soft=NONE
LOGFILE LOGFILENAME= test10e OUTPUT=test10e.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
PRINT "Comparing results with reference."
$ DIFFERENCE ECHOMOP_TEST:test10a.ref test10a
$ DIFFERENCE ECHOMOP_TEST:test10b.ref test10b
$ DIFFERENCE ECHOMOP_TEST:test10c.ref test10c
$ DIFFERENCE ECHOMOP_TEST:test10d.ref test10d
$ DIFFERENCE ECHOMOP_TEST:test10e.ref test10e
{  }
{  }
{  }
{  }
{  }
INPUT Name of output graphics device NONE
PRINT "Copying test images to local directory"
$ COPY/REPLACE ECHOMOP_DEMO:*test*.sdf *.*
PRINT "This test checks the various extraction algorithms available."
PRINT "It also varies the tunable parameters controlling the ADU and"
PRINT "readout-noise and the the effect of disabling the flat-field"
PRINT "and the arc extraction. All these tests use the ECH_RESULT task to"
PRINT "create an ascii listing of the output spectrum."
REPORT test11a
$ ech_extrct ech_rdctn=ech_test arc=test_arc ffield=test_flat extract_mode=S ~
  idx_num_ordes=0 inptim=test_obj ~
 readout_noise=0 photon_to_adu=1  soft=NONE
$ ech_result ech_rdctn=ech_test inptim=test_obj result_type=EXTOBJ ~
 result_format=ASCII ech_rducd=spectrum tune_archive=no ~
 idx_num_orders=0 soft=NONE
$ APPEND test11a.log ECHOMOP_OUTPUT.TAB
LOGFILE LOGFILENAME=test11a OUTPUT=test11a.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test11b
$ ech_extrct ech_rdctn=ech_test arc=test_arc ffield=test_flat extract_mode=P ~
  idx_num_ordes=0 inptim=test_obj ~
 readout_noise=0 photon_to_adu=1  soft=NONE
$ ech_result ech_rdctn=ech_test inptim=test_obj result_type=EXTOBJ ~
 result_format=ASCII ech_rducd=spectrum tune_archive=no ~
 idx_num_orders=0 soft=NONE
$ APPEND test11b.log ECHOMOP_OUTPUT.TAB
LOGFILE LOGFILENAME=test11b OUTPUT=test11b.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test11c
$ ech_extrct ech_rdctn=ech_test arc=test_arc ffield=test_flat extract_mode=O ~
  idx_num_ordes=0 inptim=test_obj ~
 readout_noise=0 photon_to_adu=1  soft=NONE
$ ech_result ech_rdctn=ech_test inptim=test_obj result_type=EXTOBJ ~
 result_format=ASCII ech_rducd=spectrum tune_archive=no ~
 idx_num_orders=0 soft=NONE
$ APPEND test11c.log ECHOMOP_OUTPUT.TAB
LOGFILE LOGFILENAME=test11c OUTPUT=test11c.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test11d
$ ech_qextr ech_rdctn=ech_test arc=test_arc ffield=test_flat  ~
  idx_num_ordes=0 inptim=test_obj ~
 readout_noise=0 photon_to_adu=1  soft=NONE
$ ech_result ech_rdctn=ech_test inptim=test_obj result_type=EXTOBJ ~
 result_format=ASCII ech_rducd=spectrum tune_archive=no ~
 idx_num_orders=0 soft=NONE
$ APPEND test11d.log ECHOMOP_OUTPUT.TAB
LOGFILE LOGFILENAME=test11d OUTPUT=test11d.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test11e
$ ech_extrct ech_rdctn=ech_test arc=test_arc ffield=test_flat extract_mode=O ~
  idx_num_ordes=0 inptim=test_obj ~
 readout_noise=20 photon_to_adu=1  soft=NONE
$ ech_result ech_rdctn=ech_test inptim=test_obj result_type=EXTOBJ ~
 result_format=ASCII ech_rducd=spectrum tune_archive=no ~
 idx_num_orders=0 soft=NONE
$ APPEND test11e.log ECHOMOP_OUTPUT.TAB
LOGFILE LOGFILENAME=test11e OUTPUT=test11e.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test11f
$ ech_extrct ech_rdctn=ech_test arc=test_arc ffield=test_flat extract_mode=O ~
  idx_num_ordes=0 inptim=test_obj ~
 readout_noise=5 photon_to_adu=5  soft=NONE
$ ech_result ech_rdctn=ech_test inptim=test_obj result_type=EXTOBJ ~
 result_format=ASCII ech_rducd=spectrum tune_archive=no ~
 idx_num_orders=0 soft=NONE
$ APPEND test11f.log ECHOMOP_OUTPUT.TAB
LOGFILE LOGFILENAME=test11f OUTPUT=test11f.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
PRINT "Comparing results with reference."
$ DIFFERENCE ECHOMOP_TEST:test11a.ref test11a
$ DIFFERENCE ECHOMOP_TEST:test11b.ref test11b
$ DIFFERENCE ECHOMOP_TEST:test11c.ref test11c
$ DIFFERENCE ECHOMOP_TEST:test11d.ref test11d
$ DIFFERENCE ECHOMOP_TEST:test11e.ref test11e
$ DIFFERENCE ECHOMOP_TEST:test11f.ref test11f
{  }
{  }
{  }
INPUT Name of output graphics device NONE
PRINT "Copying test images to local directory"
$ COPY/REPLACE ECHOMOP_DEMO:*test*.sdf *.*
PRINT "This test checks the cloning facilities for the major processing"
PRINT "options. Results are cloned from ech_test.sdf to ech_copy.sdf"
PRINT "and are checked using the FIGARO EXAM command."
REPORT test12a
$ echmenu ech_rdctn=ech_copy tracim=test_flat inptim=test_obj arc=test_arc ~
  tune_clone=ech_test tune_autloc=yes tune_automate="1,EXIT"
EXAM ech_copy..
LOGFILE LOGFILENAME=test12a OUTPUT=test12a.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test12b
$ ech_trace ech_rdctn=ech_copy idx_num_orders=0 tracim=test_flat trcfit=poly ~
 tune_clone=ech_test trc_npoly=7 trace_mode=c soft=NONE
EXAM ech_copy..
LOGFILE LOGFILENAME= test12b OUTPUT=test12b.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test12c
$ ech_fitord ech_rdctn=ech_copy idx_num_orders=0 trcfit=poly trc_interact=no  ~
  tune_clone=ech_test trc_npoly=7 soft=NONE
EXAM ech_copy..
LOGFILE LOGFILENAME= test12c OUTPUT=test12c.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test12d
$ ech_spatial ech_rdctn=ech_copy slitim=test_flat pfl_interact=no ~
  tune_clone=ech_test inptim=test_obj soft=NONE
EXAM ech_copy..
LOGFILE LOGFILENAME= test12d OUTPUT=test12d.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test12e
$ ech_ffield ech_rdctn=ech_copy idx_num_orders=0 ffield=test_flat fltfit=mean ~
   tune_clone=ech_test soft=NONE
EXAM ech_copy..
LOGFILE LOGFILENAME= test12e OUTPUT=test12e.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test12f
$ ech_sky ech_rdctn=ech_copy idx_num_orders=0 ffield=test_flat ~
  tune_clone=ech_test skyfit=mean inptim=test_obj  ~
  readout_noise=0 photon_to_adu=1 soft=NONE
EXAM ech_copy..
LOGFILE LOGFILENAME= test12f OUTPUT=test12f.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test12g
$ ech_profile ech_rdctn=ech_copy inptim=test_obj tune_use_nxf=0.2 ~
  tune_clone=ech_test soft=NONE
EXAM ech_copy..
LOGFILE LOGFILENAME=test12g OUTPUT=test12g.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test12h
$ ech_extrct ech_rdctn=ech_copy arc=test_arc ffield=test_flat extract_mode=S ~
 tune_clone=ech_test inptim=test_obj ~
 readout_noise=0 photon_to_adu=1  soft=NONE
EXAM ech_copy..
LOGFILE LOGFILENAME=test12h OUTPUT=test12h.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test12i
$ ech_linloc ech_rdctn=ech_copy arc=test_arc  ~
  tune_clone=ech_test  tune_rflnthr=1.25 soft=NONE
EXAM ech_copy..
LOGFILE LOGFILENAME= test12i OUTPUT=test12i.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test12j
$ ech_idwave ech_rdctn=ech_copy arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes wavfit=poly w_npoly=7 ~
  tune_clone=ech_test min_dispersion=0.02 max_dispersion=0.1 low_wave=4200 ~
  hi_wave=4500  soft=NONE
EXAM ech_copy..
LOGFILE LOGFILENAME= test12j OUTPUT=test12j.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
PRINT "Comparing results with reference."
$ DIFFERENCE ECHOMOP_TEST:test12a.ref test12a
$ DIFFERENCE ECHOMOP_TEST:test12b.ref test12b
$ DIFFERENCE ECHOMOP_TEST:test12c.ref test12c
$ DIFFERENCE ECHOMOP_TEST:test12d.ref test12d
$ DIFFERENCE ECHOMOP_TEST:test12e.ref test12e
$ DIFFERENCE ECHOMOP_TEST:test12f.ref test12f
$ DIFFERENCE ECHOMOP_TEST:test12g.ref test12g
$ DIFFERENCE ECHOMOP_TEST:test12h.ref test12h
$ DIFFERENCE ECHOMOP_TEST:test12i.ref test12i
$ DIFFERENCE ECHOMOP_TEST:test12j.ref test12j
{  }
{  }
{  }
