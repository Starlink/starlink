FIGARO
LOAD ECHOMOP_EXEC:ECHOMOP
DEFINE LOGFILE ADAM_EXE:LOGFILE
{  }
{  }
INPUT Name of output graphics device NONE
PRINT "Copying test images to local directory"
$ COPY/REPLACE ECHOMOP_DEMO:*test*.sdf *.*
PRINT "This test checks the various extraction algorithms available."
PRINT "It also varies the tunable parameters controlling the ADU and"
PRINT "readout-noise and the the effect of disabling the flat-field"
PRINT "and the arc extraction. All these tests use the ECH_RESULT task to"
PRINT "create an ascii listing of the output spectrum."
REPORT test11a
$ ech_extrct ech_rdctn=ech_test arc=test_arc ffield=test_flat extract_mode=S ~
 idx_num_orders=0 inptim=test_obj ~
 readout_noise=0 photon_to_adu=1  soft=NONE
$ ech_result ech_rdctn=ech_test inptim=test_obj result_type=EXTOBJ ~
 result_format=ASCII ech_rducd=spectrum tune_archive=no ~
 idx_num_orders=0 soft=NONE
$ APPEND test11a.log ECHOMOP_OUTPUT.TAB
LOGFILE LOGFILENAME=test11a OUTPUT=test11a.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test11b
$ ech_extrct ech_rdctn=ech_test arc=test_arc ffield=test_flat extract_mode=P ~
 idx_num_orders=0 inptim=test_obj ~
 readout_noise=0 photon_to_adu=1  soft=NONE
$ ech_result ech_rdctn=ech_test inptim=test_obj result_type=EXTOBJ ~
 result_format=ASCII ech_rducd=spectrum tune_archive=no ~
 idx_num_orders=0 soft=NONE
$ APPEND test11b.log ECHOMOP_OUTPUT.TAB
LOGFILE LOGFILENAME=test11b OUTPUT=test11b.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test11c
$ ech_extrct ech_rdctn=ech_test arc=test_arc ffield=test_flat extract_mode=O ~
 idx_num_orders=0 inptim=test_obj ~
 readout_noise=0 photon_to_adu=1  soft=NONE
$ ech_result ech_rdctn=ech_test inptim=test_obj result_type=EXTOBJ ~
 result_format=ASCII ech_rducd=spectrum tune_archive=no ~
 idx_num_orders=0 soft=NONE
$ APPEND test11c.log ECHOMOP_OUTPUT.TAB
LOGFILE LOGFILENAME=test11c OUTPUT=test11c.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test11d
$ ech_qextr ech_rdctn=ech_test arc=test_arc ffield=test_flat  ~
 idx_num_orders=0 inptim=test_obj ~
 readout_noise=0 photon_to_adu=1  soft=NONE
$ ech_result ech_rdctn=ech_test inptim=test_obj result_type=EXTOBJ ~
 result_format=ASCII ech_rducd=spectrum tune_archive=no ~
 idx_num_orders=0 soft=NONE
$ APPEND test11d.log ECHOMOP_OUTPUT.TAB
LOGFILE LOGFILENAME=test11d OUTPUT=test11d.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test11e
$ ech_extrct ech_rdctn=ech_test arc=test_arc ffield=test_flat extract_mode=O ~
 idx_num_orders=0 inptim=test_obj ~
 readout_noise=20 photon_to_adu=1  soft=NONE
$ ech_result ech_rdctn=ech_test inptim=test_obj result_type=EXTOBJ ~
 result_format=ASCII ech_rducd=spectrum tune_archive=no ~
 idx_num_orders=0 soft=NONE
$ APPEND test11e.log ECHOMOP_OUTPUT.TAB
LOGFILE LOGFILENAME=test11e OUTPUT=test11e.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
REPORT test11f
$ ech_extrct ech_rdctn=ech_test arc=test_arc ffield=test_flat extract_mode=O ~
 idx_num_orders=0 inptim=test_obj ~
 readout_noise=5 photon_to_adu=5  soft=NONE
$ ech_result ech_rdctn=ech_test inptim=test_obj result_type=EXTOBJ ~
 result_format=ASCII ech_rducd=spectrum tune_archive=no ~
 idx_num_orders=0 soft=NONE
$ APPEND test11f.log ECHOMOP_OUTPUT.TAB
LOGFILE LOGFILENAME=test11f OUTPUT=test11f.log TASKS=ALL LABELS=ALL DTNS=xxxS SINCE=S
PRINT "Comparing results with reference."
$ DIFFERENCE ECHOMOP_TEST:test11a.ref test11a
$ DIFFERENCE ECHOMOP_TEST:test11b.ref test11b
$ DIFFERENCE ECHOMOP_TEST:test11c.ref test11c
$ DIFFERENCE ECHOMOP_TEST:test11d.ref test11d
$ DIFFERENCE ECHOMOP_TEST:test11e.ref test11e
$ DIFFERENCE ECHOMOP_TEST:test11f.ref test11f
{  }
{  }
