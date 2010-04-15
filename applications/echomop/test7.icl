FIGARO
LOAD ECHOMOP_EXEC:ECHOMOP
DEFINE LOGFILE ADAM_EXE:LOGFILE
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
