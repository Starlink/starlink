FIGARO
LOAD ECHOMOP_EXEC:ECHOMOP
DEFINE LOGFILE ADAM_EXE:LOGFILE
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
