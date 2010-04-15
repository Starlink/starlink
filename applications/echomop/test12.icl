FIGARO
LOAD ECHOMOP_EXEC:ECHOMOP
DEFINE LOGFILE ADAM_EXE:LOGFILE
{  }
{  }
INPUT Name of output graphics device NONE
PRINT "Copying test images to local directory"
$ COPY/REPLACE ECHOMOP_DEMO:*test*.sdf *.*
PRINT "This test checks the `cloning facilities for the major processing"
PRINT "options. Results are cloned from ech_test.sdf to ech_copy.sdf"
PRINT "and are checked using the FIGARO `exam command."
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
