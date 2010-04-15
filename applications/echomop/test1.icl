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
  tune_autloc=yes tune_fcheck=no tune_automate="1,EXIT"
LOGFILE LOGFILENAME=test1a OUTPUT=test1a.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test1b
$ echmenu ech_rdctn=ech_test tracim=test_flat inptim=test_obj arc=test_arc ~
  tune_autloc=yes tune_fcheck=yes tune_automate="1,EXIT"
LOGFILE LOGFILENAME=test1b OUTPUT=test1b.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
PRINT "Comparing results with reference."
$ DIFFERENCE ECHOMOP_TEST:test1a.ref test1a
$ DIFFERENCE ECHOMOP_TEST:test1b.ref test1b
