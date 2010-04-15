FIGARO
LOAD ECHOMOP_EXEC:ECHOMOP
DEFINE LOGFILE ADAM_EXE:LOGFILE
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
