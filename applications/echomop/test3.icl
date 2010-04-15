FIGARO
LOAD ECHOMOP_EXEC:ECHOMOP
DEFINE LOGFILE ADAM_EXE:LOGFILE
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
