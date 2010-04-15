FIGARO
LOAD ECHOMOP_EXEC:ECHOMOP
DEFINE LOGFILE ADAM_EXE:LOGFILE
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
