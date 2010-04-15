FIGARO
LOAD ECHOMOP_EXEC:ECHOMOP
DEFINE LOGFILE ADAM_EXE:LOGFILE
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
