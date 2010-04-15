FIGARO
LOAD ECHOMOP_EXEC:ECHOMOP
DEFINE LOGFILE ADAM_EXE:LOGFILE
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
