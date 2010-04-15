FIGARO
LOAD ECHOMOP_EXEC:ECHOMOP
DEFINE LOGFILE ADAM_EXE:LOGFILE
{  }
{  }
INPUT Name of output graphics device NONE
PRINT "Copying test images to local directory"
$ COPY/REPLACE ECHOMOP_DEMO:*test*.sdf *.*
PRINT "This test checks the behaviour of the automatic wavelength calibration"
PRINT "task. It first runs arc line candidate location to ensure a good"
PRINT "selection of identifiable lines. Various search ranges for dispersion"
PRINT "and wavelength are then tested. Polynomial and spline fits are used."
$ ech_linloc ech_rdctn=ech_test arc=test_arc  ~
   tune_rflnthr=1.25 soft=NONE
REPORT test9a
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes wavfit=poly w_npoly=7 ~
  min_dispersion=0.02 max_dispersion=0.1 low_wave=4200 ~
  hi_wave=4500  soft=NONE
LOGFILE LOGFILENAME= test9a OUTPUT=test9a.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
$ DELOBJ ech_test.more.echelle.id_lines
$ DELOBJ ech_test.more.echelle.id_count
REPORT test9b
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes wavfit=poly w_npoly=7 ~
  min_dispersion=0.01 max_dispersion=0.2 low_wave=4200 ~
  hi_wave=4500  soft=NONE
LOGFILE LOGFILENAME= test9b OUTPUT=test9b.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
$ DELOBJ ech_test.more.echelle.id_lines
$ DELOBJ ech_test.more.echelle.id_count
REPORT test9c
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes w_npoly=7 ~
  min_dispersion=0.02 max_dispersion=0.1 wavfit=poly  low_wave=4000 ~
  hi_wave=5000  soft=NONE
LOGFILE LOGFILENAME= test9c OUTPUT=test9c.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
$ DELOBJ ech_test.more.echelle.id_lines
$ DELOBJ ech_test.more.echelle.id_count
REPORT test9d
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes  w_npoly=7 ~
  min_dispersion=0.02 max_dispersion=0.1 wavfit=poly  low_wave=3800 ~
  hi_wave=9000  soft=NONE
LOGFILE LOGFILENAME= test9d OUTPUT=test9d.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
$ DELOBJ ech_test.more.echelle.id_lines
$ DELOBJ ech_test.more.echelle.id_count
REPORT test9e
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes  w_npoly=20 ~
  min_dispersion=0.02 max_dispersion=0.1 wavfit=spline  low_wave=4200 ~
  hi_wave=4500  soft=NONE
LOGFILE LOGFILENAME= test9e OUTPUT=test9e.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
$ DELOBJ ech_test.more.echelle.id_lines
$ DELOBJ ech_test.more.echelle.id_count
REPORT test9f
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes w_npoly=20 ~
  min_dispersion=0.01 max_dispersion=0.2 wavfit=spline  low_wave=4200 ~
  hi_wave=4500  soft=NONE
LOGFILE LOGFILENAME= test9f OUTPUT=test9f.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
$ DELOBJ ech_test.more.echelle.id_lines
$ DELOBJ ech_test.more.echelle.id_count
REPORT test9g
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes  w_npoly=20 ~
  min_dispersion=0.02 max_dispersion=0.1 wavfit=spline  low_wave=4000 ~
  hi_wave=5000  soft=NONE
LOGFILE LOGFILENAME= test9g OUTPUT=test9g.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
$ DELOBJ ech_test.more.echelle.id_lines
$ DELOBJ ech_test.more.echelle.id_count
REPORT test9h
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes w_npoly=20 ~
  min_dispersion=0.02 max_dispersion=0.1 wavfit=spline  low_wave=3800 ~
  hi_wave=9000  soft=NONE
LOGFILE LOGFILENAME= test9h OUTPUT=test9h.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
$ DELOBJ ech_test.more.echelle.id_lines
$ DELOBJ ech_test.more.echelle.id_count
REPORT test9i
$ ech_idwave ech_rdctn=ech_test arc_type=ARCDIRS:THAR ~
  ech_ftrdb=ARCDIRS:THAR auto_id=yes wavfit=poly w_npoly=7 ~
  min_dispersion=0.02 max_dispersion=0.1 low_wave=4200 ~
  hi_wave=4500  soft=NONE
LOGFILE LOGFILENAME= test9i OUTPUT=test9i.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
PRINT "Comparing results with reference."
$ DIFFERENCE ECHOMOP_TEST:test9a.ref test9a
$ DIFFERENCE ECHOMOP_TEST:test9b.ref test9b
$ DIFFERENCE ECHOMOP_TEST:test9c.ref test9c
$ DIFFERENCE ECHOMOP_TEST:test9d.ref test9d
$ DIFFERENCE ECHOMOP_TEST:test9e.ref test9e
$ DIFFERENCE ECHOMOP_TEST:test9f.ref test9f
$ DIFFERENCE ECHOMOP_TEST:test9g.ref test9g
$ DIFFERENCE ECHOMOP_TEST:test9h.ref test9h
$ DIFFERENCE ECHOMOP_TEST:test9i.ref test9i
{  }
{  }
{  }
