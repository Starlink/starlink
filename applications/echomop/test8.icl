FIGARO
LOAD ECHOMOP_EXEC:ECHOMOP
DEFINE LOGFILE ADAM_EXE:LOGFILE
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
