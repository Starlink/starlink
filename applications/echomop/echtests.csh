#!/bin/csh
#
onintr quit;
cat <<End_of_intro
ECHOMOP test script.

Please ensure that this script is run in an empty directory.

Log files are created for each test and compared to reference copies
in the ECHOMOP installation.  After each test the differences between
the log and reference results are displayed.  You can review the log
files after all the tests have run.

End_of_intro
#
echo -n "Initialising...";
alias echo 'echo >/dev/null';
figaro;
unalias echo;
set rm_old = `alias rm`;
set cp_old = `alias cp`;
unalias rm;
unalias cp;
#
# This ensures that we don't interfere with the contents of the
# ADAM_USER directory for the invoker of the script.  It also makes sure
# that we avoid any problem files in that directory.
setenv ADAM_USER $cwd;
echo "done.";
#
echo "";
echo -n "Copying test data to the working directory...";
cp -f $ECHOMOP_DEMO/*test*.sdf .;
mv -f ech_test.sdf ech_temp.sdf;
echo "done.";
#
#
test1:
#
# Test 1.
# ======
#
#echo "";
#echo -n "Copying test data to the working directory...";
#cp -f $ECHOMOP_DEMO/*test*.sdf .;
#mv -f ech_test.sdf ech_temp.sdf;
#echo "done.";
#
echo "";
echo "TEST 1..........................................................";
echo "This test runs automatic order location with and without";
echo "optional frame checking (bad rows or columns, saturation).";
#
echo "";
echo -n "1a...";
echmenu ech_rdctn=ech_temp tracim=test_flat inptim=test_obj arc=test_arc \
  SOFT="NONE" display=FALSE tune_autloc=yes tune_fcheck=no \
  tune_automate="'1,EXIT'" > test1a.log;
#
echo -n "1b...";
echmenu ech_rdctn=ech_temp tracim=test_flat inptim=test_obj arc=test_arc \
  SOFT="NONE" tune_autloc=yes tune_fcheck=yes \
  tune_automate="'1,EXIT'" > test1b.log;
echo "done.";
#
echo "";
echo "Comparing results with reference:";
foreach f ( test1?.log )
   diff $ECHOMOP_TEST/$f:r.ref $f;
end
echo "\!\!  End of comparison.";
test2:
#
# Test 2.
# ======
#
#echo "";
#echo -n "Copying test data to the working directory...";
#cp -f $ECHOMOP_DEMO/*test*.sdf .;
#mv -f ech_test.sdf ech_temp.sdf;
#echo "done.";
echo "";
echo "TEST 2..........................................................";
echo "This test runs order tracing using all the available methods.";
echo "Note that some methods do not trace the test data particularly";
echo "well, producing lost traces and untraceable order warnings.";
#
echo "";
echo -n "2a...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=poly \
 trc_npoly=7 trace_mode=tc tune_maxpoly=50 display=FALSE soft=NONE \
 > test2a.log;
#
echo -n "2b...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=poly \
 trc_npoly=7 trace_mode=c tune_maxpoly=50 soft=NONE > test2b.log;
#
echo -n "2c...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=poly \
 trc_npoly=7 trace_mode=b tune_maxpoly=50 soft=NONE > test2c.log;
#
echo -n "2d...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=poly \
 trc_npoly=7 trace_mode=g tune_maxpoly=50 soft=NONE > test2d.log;
#
echo -n "2e...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=poly \
 trc_npoly=7 trace_mode=tc tune_maxpoly=50 soft=NONE > test2e.log;
#
echo -n "2f...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=poly \
 trc_npoly=7 trace_mode=tb tune_maxpoly=50 soft=NONE > test2f.log;
#
echo -n "2g...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=poly \
 trc_npoly=7 trace_mode=tg tune_maxpoly=50 soft=NONE > test2g.log;
#
echo -n "2h...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=poly \
 trc_npoly=7 trace_mode=tag tune_maxpoly=50 soft=NONE > test2h.log;
echo "done.";
echo "";
echo "Repeating tests using spline fitting...";
#
echo -n "2i...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=spline \
 trc_npoly=16 trace_mode=e tune_maxpoly=50 soft=NONE > test2i.log;
#
echo -n "2j...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=spline \
 trc_npoly=16 trace_mode=c tune_maxpoly=50 soft=NONE > test2j.log;
#
echo -n "2k...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=spline \
 trc_npoly=16 trace_mode=b tune_maxpoly=50 soft=NONE > test2k.log;
#
echo -n "2l...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=spline \
 trc_npoly=16 trace_mode=g tune_maxpoly=50 soft=NONE > test2l.log;
#
echo -n "2m...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=spline \
 trc_npoly=16 trace_mode=tc tune_maxpoly=50 soft=NONE > test2m.log;
#
echo -n "2n...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=spline \
 trc_npoly=16 trace_mode=tb tune_maxpoly=50 soft=NONE > test2n.log;
#
echo -n "2o...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=spline \
 trc_npoly=16 trace_mode=tg tune_maxpoly=50 soft=NONE > test2o.log;
#
echo -n "2p...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=spline \
 trc_npoly=16 trace_mode=tag tune_maxpoly=50 soft=NONE > test2p.log;
echo "done.";
#
echo "";
echo "Comparing results with reference:";
foreach f ( test2?.log )
   diff $ECHOMOP_TEST/$f:r.ref $f;
end
echo "\!\!  End of comparison.";
test3:
#
# Test 3.
# ======
#
#echo "";
#echo -n "Copying test data to the working directory...";
#cp -f $ECHOMOP_DEMO/*test*.sdf .;
#mv -f ech_test.sdf ech_temp.sdf;
#echo "done.";
#
echo "";
echo "TEST 3..........................................................";
echo "This test checks the order-fit refinement using both polynomials";
echo "and splines for a variety of clipping limits.";
#
echo "";
echo -n "3a...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=poly \
 trc_npoly=7 trace_mode=tc tune_maxpoly=50 soft=NONE display=FALSE >/dev/null;
ech_fitord ech_rdctn=ech_temp idx_num_orders=0 trcfit=poly trc_interact=no  \
  trc_npoly=7 tune_maxpoly=50 soft=NONE display=FALSE > test3a.log;
#
echo -n "3b...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=poly \
 trc_npoly=7 trace_mode=tc tune_maxpoly=50 soft=NONE >/dev/null;
ech_fitord ech_rdctn=ech_temp idx_num_orders=0 trcfit=poly trc_interact=no  \
  trc_npoly=7 tune_maxpoly=50 tune_clpmxdev=1.0 soft=NONE > test3b.log;
#
echo -n "3c...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=poly \
 trc_npoly=7 trace_mode=tc tune_maxpoly=50 soft=NONE >/dev/null;
ech_fitord ech_rdctn=ech_temp idx_num_orders=0 trcfit=poly trc_interact=no  \
  trc_npoly=7 tune_maxpoly=50 tune_clpmxdev=0.1 soft=NONE > test3c.log;
#
echo -n "3d...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=poly \
 trc_npoly=7 trace_mode=tc tune_maxpoly=50 soft=NONE >/dev/null;
ech_fitord ech_rdctn=ech_temp idx_num_orders=0 trcfit=poly trc_interact=no  \
  trc_npoly=7 tune_maxpoly=50 tune_clpmxdev=0.05 soft=NONE > test3d.log;
#
echo -n "3e...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=poly \
 trc_npoly=7 trace_mode=tc tune_maxpoly=50 soft=NONE >/dev/null;
ech_fitord ech_rdctn=ech_temp idx_num_orders=0 trcfit=spline trc_interact=no  \
  trc_npoly=20 tune_maxpoly=50 soft=NONE > test3e.log;
#
echo -n "3f...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=poly \
 trc_npoly=7 trace_mode=tc tune_maxpoly=50 soft=NONE >/dev/null;
ech_fitord ech_rdctn=ech_temp idx_num_orders=0 trcfit=spline trc_interact=no  \
  trc_npoly=20 tune_maxpoly=50 tune_clpmxdev=1.0 soft=NONE > test3f.log;
#
echo -n "3g...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=poly \
 trc_npoly=7 trace_mode=tc tune_maxpoly=50 soft=NONE >/dev/null;
ech_fitord ech_rdctn=ech_temp idx_num_orders=0 trcfit=spline trc_interact=no  \
  trc_npoly=20 tune_maxpoly=50 tune_clpmxdev=0.1 soft=NONE > test3g.log;
#
echo -n "3h...";
ech_trace ech_rdctn=ech_temp idx_num_orders=0 tracim=test_flat trcfit=poly \
 trc_npoly=7 trace_mode=tc tune_maxpoly=50 soft=NONE >/dev/null;
ech_fitord ech_rdctn=ech_temp idx_num_orders=0 trcfit=spline trc_interact=no  \
  trc_npoly=20 tune_maxpoly=50 tune_clpmxdev=0.05 soft=NONE > test3h.log;
echo "done.";
#
echo "";
echo "Comparing results with reference:";
foreach f ( test3?.log )
   diff $ECHOMOP_TEST/$f:r.ref $f;
end
echo "\!\!  End of comparison.";
test4:
#
# Test 4.
# ======
#
#echo "";
#echo -n "Copying test data to the working directory...";
#cp -f $ECHOMOP_DEMO/*test*.sdf .;
#mv -f ech_test.sdf ech_temp.sdf;
#echo "done.";
#
echo "";
echo "TEST 4..........................................................";
echo "This test checks the behaviour of the dekker and object spatial-";
echo "limit calculations.  It then tests the ability to set the limits";
echo "directly using tunable parameters.";
#
echo "";
echo -n "4a...";
ech_spatial ech_rdctn=ech_temp slitim=test_flat pfl_interact=no \
  inptim=test_obj tune_maxpoly=50 soft=NONE display=FALSE > test4a.log;
#
echo -n "4b...";
ech_spatial ech_rdctn=ech_temp slitim=test_flat pfl_interact=no \
  inptim=test_obj tune_maxpoly=50 tune_dekthr=0.6 soft=NONE > test4b.log;
#
echo -n "4c...";
ech_spatial ech_rdctn=ech_temp slitim=test_flat pfl_interact=no \
  inptim=test_obj tune_maxpoly=50 tune_dekthr=0.4 soft=NONE > test4c.log;
#
echo -n "4d...";
ech_spatial ech_rdctn=ech_temp slitim=test_flat pfl_interact=no \
  inptim=test_obj tune_maxpoly=50 tune_dekthr=0.2 soft=NONE > test4d.log;
#
echo -n "4e...";
ech_spatial ech_rdctn=ech_temp slitim=test_flat pfl_interact=no \
  inptim=test_obj tune_maxpoly=50 tune_skyhilim=1.01 soft=NONE > test4e.log;
#
echo -n "4f...";
ech_spatial ech_rdctn=ech_temp slitim=test_flat pfl_interact=no \
  inptim=test_obj tune_maxpoly=50 tune_dekthr=0.6  tune_skyhilim=1.25 \
  soft=NONE > test4f.log;
#
echo -n "4g...";
ech_spatial ech_rdctn=ech_temp slitim=test_flat pfl_interact=no \
  inptim=test_obj tune_maxpoly=50 tune_dekthr=0.4  tune_skyhilim=1.5 \
  soft=NONE > test4g.log;
#
echo -n "4h...";
ech_spatial ech_rdctn=ech_temp tune_skyhilim=1.05 slitim=test_flat \
  pfl_interact=no inptim=test_obj tune_maxpoly=50 tune_dekthr=0.2 \
  soft=NONE > test4h.log;
#
echo -n "4i...";
ech_spatial ech_rdctn=ech_temp slitim=test_flat pfl_interact=no \
  inptim=test_obj soft=NONE tune_dekblw=-9 tune_maxpoly=50 \
  tune_dekabv=9 tune_objblw=-7 tune_objabv=3 > test4i.log;
echo "done.";
#
echo "";
echo "Comparing results with reference:";
foreach f ( test4?.log )
   diff $ECHOMOP_TEST/$f:r.ref $f;
end
echo "\!\!  End of comparison.";
test5:
#
# Test 5.
# ======
#
echo "";
echo -n "Copying test data to the working directory...";
cp -f $ECHOMOP_DEMO/*test*.sdf .;
mv -f ech_test.sdf ech_temp.sdf;
echo "done.";
#
echo "";
echo "TEST 5..........................................................";
echo "This test checks the flat-field photometric correction calculations.";
echo "Models with X/Y components and Y-only components are checked.";
echo "Functions used during fitting are mean, median, smooth, slope,";
echo "spline and poly.";
echo "";
#
echo -n "5a...";
ech_ffield ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat fltfit=none \
   soft=NONE tune_ffsubsmp=no tune_maxpoly=50 display=FALSE > test5a.log;
#
echo -n "5b...";
ech_ffield ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat fltfit=mean \
   tune_maxpoly=50 soft=NONE > test5b.log;
#
echo -n "5c...";
ech_ffield ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat fltfit=median \
   tune_maxpoly=50 soft=NONE > test5c.log;
#
echo -n "5d...";
ech_ffield ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat fltfit=smooth \
   tune_maxpoly=50 soft=NONE > test5d.log;
#
echo -n "5e...";
ech_ffield ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat fltfit=slope \
   tune_maxpoly=50 soft=NONE > test5e.log;
#
echo -n "5f...";
ech_ffield ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat fltfit=mean \
   tune_maxpoly=50 soft=NONE tune_fflsmp=31 > test5f.log;
#
echo -n "5g...";
ech_ffield ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat fltfit=median \
   tune_maxpoly=50 soft=NONE tune_fflsmp=31 > test5g.log;
#
echo -n "5h...";
ech_ffield ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat fltfit=smooth \
   tune_maxpoly=50 soft=NONE tune_fflsmp=31 > test5h.log;
#
echo -n "5i...";
ech_ffield ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat fltfit=slope \
   tune_maxpoly=50 soft=NONE tune_fflsmp=31 > test5i.log;
echo "";
#
echo -n "5j...";
ech_ffield ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat fltfit=poly \
   tune_maxpoly=50 soft=NONE tune_ffnxply=0 > test5j.log;
#
echo -n "5k...";
ech_ffield ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat fltfit=poly \
   tune_maxpoly=50 soft=NONE tune_ffnxply=5 > test5k.log;
#
echo -n "5l...";
ech_ffield ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat fltfit=poly \
   tune_maxpoly=50 soft=NONE tune_ffnyply=3 > test5l.log;
#
echo -n "5m...";
ech_ffield ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat fltfit=poly \
   tune_maxpoly=50 soft=NONE tune_ffsubsmp=yes > test5m.log;
#
echo -n "5n...";
ech_ffield ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat fltfit=spline \
   tune_maxpoly=50 soft=NONE tune_ffnxply=0 >  test5n.log;
#
echo -n "5o...";
ech_ffield ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat fltfit=spline \
   tune_maxpoly=50 soft=NONE tune_ffnxply=18 > test5o.log;
#
echo -n "5p...";
ech_ffield ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat fltfit=spline \
   tune_maxpoly=50 soft=NONE tune_ffnyply=16 > test5p.log;
#
echo -n "5q...";
ech_ffield ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat fltfit=spline \
   tune_maxpoly=50 soft=NONE tune_ffsubsmp=yes > test5q.log;
echo "done.";
#
echo "";
echo "Comparing results with reference:";
foreach f ( test5?.log )
   diff $ECHOMOP_TEST/$f:r.ref $f;
end
echo "\!\!  End of comparison.";
test6:
#
# Test 6.
# ======
#
echo "";
echo -n "Copying test data to the working directory...";
cp -f $ECHOMOP_DEMO/*test*.sdf .;
mv -f ech_test.sdf ech_temp.sdf;
echo "done.";
#
echo "";
echo "TEST 6..........................................................";
echo "This test checks the sky modelling functions.  A variety of options";
echo "are used, including mean, polynomial and spline fitting.  Functions";
echo "in both X- and Y-directions are tested and the optional monte-carlo";
echo "simulation mode is invoked.";
echo "";
#
echo -n "6a...";
ech_sky ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat \
  skyfit=mean inptim=test_obj display=FALSE tune_maxpoly=50 \
  readout_noise=0 photon_to_adu=1 soft=NONE > test6a.log;
#
echo -n "6b...";
ech_sky ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat \
  skyfit=poly tune_skypoly=3 inptim=test_obj tune_maxpoly=50 \
  readout_noise=0 photon_to_adu=1 soft=NONE > test6b.log;
#
echo -n "6c...";
ech_sky ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat \
  skyfit=spline tune_skypoly=20 inptim=test_obj tune_maxpoly=50 \
  readout_noise=0 photon_to_adu=1 soft=NONE > test6c.log;
#
echo -n "6d...";
ech_sky ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat \
  skyfit=poly tune_skypoly=3 tune_skyxply=5 inptim=test_obj \
  tune_maxpoly=50 readout_noise=0 photon_to_adu=1 soft=NONE > test6d.log;
#
echo -n "6e...";
ech_sky ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat \
  skyfit=poly tune_skypoly=3 tune_skyxply=5 tune_skyrthr=100 inptim=test_obj \
  tune_maxpoly=50 readout_noise=0 photon_to_adu=1 soft=NONE > test6e.log;
#
echo -n "6f...";
ech_sky ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat \
  skyfit=poly tune_skyxply=5 tune_skyrthr=5 tune_skysim=yes inptim=test_obj \
  tune_maxpoly=50 readout_noise=0 photon_to_adu=1 soft=NONE > test6f.log;
#
echo -n "6g...";
ech_sky ech_rdctn=ech_temp idx_num_orders=0 ffield=test_flat \
  skyfit=NONE inptim=test_obj tune_maxpoly=50 \
  readout_noise=0 photon_to_adu=1 soft=NONE > test6g.log;
echo "done.";
#
echo "";
echo "Comparing results with reference:";
foreach f ( test6?.log )
   diff $ECHOMOP_TEST/$f:r.ref $f;
end
echo "\!\!  End of comparison.";
test7:
#
# Test 7.
# ======
#
echo "";
echo -n "Copying test data to the working directory...";
cp -f $ECHOMOP_DEMO/*test*.sdf .;
mv -f ech_test.sdf ech_temp.sdf;
echo "done.";
#
echo "";
echo "TEST 7..........................................................";
echo "This test checks the behaviour of the object-profile calculation.";
echo "Options tested include all-order-average, per-order-profile,";
echo "a variety of sample region sizes, and the optional polynomial";
echo "fitting in the wavelength direction.";
echo "";
#
echo -n "7a...";
ech_profile ech_rdctn=ech_temp inptim=test_obj tune_use_nxf=0.2 \
  tune_maxpoly=50 tune_objpoly=0 soft=NONE display=FALSE > test7a.log;
#
echo -n "7b...";
ech_profile ech_rdctn=ech_temp inptim=test_obj tune_use_nxf=0.5 \
  tune_maxpoly=50 soft=NONE > test7b.log;
#
echo -n "7c...";
ech_profile ech_rdctn=ech_temp inptim=test_obj tune_use_nxf=0.8 \
  tune_maxpoly=50 soft=NONE > test7c.log;
#
echo -n "7d...";
ech_profile ech_rdctn=ech_temp inptim=test_obj tune_use_nxf=1.0 \
  tune_maxpoly=50 soft=NONE > test7d.log;
#
echo -n "7e...";
ech_profile ech_rdctn=ech_temp inptim=test_obj tune_objpoly=5 \
  tune_maxpoly=50 pfl_interact=no objfit=poly soft=NONE > test7e.log;
#
echo -n "7f...";
ech_profile ech_rdctn=ech_temp inptim=test_obj tune_use_nxf=0.2 \
  tune_maxpoly=50 tune_objpoly=0 soft=NONE >/dev/null;
echo "done.";
#
echo "";
echo "Comparing results with reference:";
foreach f ( test7?.log )
   diff $ECHOMOP_TEST/$f:r.ref $f;
end
echo "\!\!  End of comparison.";
test8:
#
# Test 8.
# ======
#
#echo "";
#echo -n "Copying test data to the working directory...";
#cp -f $ECHOMOP_DEMO/*test*.sdf .;
#mv -f ech_test.sdf ech_temp.sdf;
#echo "done.";
#
echo "";
echo "TEST 8..........................................................";
echo "This test checks the process of arc line-candidate location.";
echo "A variety of threshold settings are used leading to different";
echo "sets of line candidates.";
echo "";
#
echo -n "8a...";
ech_linloc ech_rdctn=ech_temp arc=test_arc tune_maxpoly=50 \
   tune_rflnthr=1.25 soft=NONE display=FALSE > test8a.log;
#
echo -n "8b...";
ech_linloc ech_rdctn=ech_temp arc=test_arc tune_maxpoly=50 \
   tune_rflnthr=2.0 soft=NONE > test8b.log;
#
echo -n "8c...";
ech_linloc ech_rdctn=ech_temp arc=test_arc tune_maxpoly=50 \
   tune_rflnthr=5.0 soft=NONE > test8c.log;
#
echo -n "8d...";
ech_linloc ech_rdctn=ech_temp arc=test_arc tune_maxpoly=50 \
   tune_rflnthr=25.0 soft=NONE > test8d.log;
echo "done.";
#
echo "";
echo "Comparing results with reference:";
foreach f ( test8?.log )
   diff $ECHOMOP_TEST/$f:r.ref $f;
end
echo "\!\!  End of comparison.";
test9:
#
# Test 9.
# ======
#
#echo "";
#echo -n "Copying test data to the working directory...";
#cp -f $ECHOMOP_DEMO/*test*.sdf .;
#mv -f ech_test.sdf ech_temp.sdf;
#echo "done.";
#
echo "";
echo "TEST 9..........................................................";
echo "This test checks the behaviour of the automatic wavelength-calibration";
echo "task.  It first runs arc line-candidate location to ensure a good";
echo "selection of identifiable lines.  Various search ranges for dispersion";
echo "and wavelength are then tested.  Polynomial and spline fits are used.";
echo "";
#
echo -n "9a...";
ech_linloc ech_rdctn=ech_temp arc=test_arc tune_maxpoly=50 \
   tune_rflnthr=1.25 soft=NONE display=FALSE >/dev/null;
delobj ech_temp.more.echelle.id_lines;
delobj ech_temp.more.echelle.id_count;
ech_idwave ech_rdctn=ech_temp tune_maxpoly=50 \
  ech_ftrdb=\$ARCDIRS/THAR auto_id=yes wavfit=poly w_npoly=7 \
  min_dispersion=0.02 max_dispersion=0.1 low_wave=0 \
  hi_wave=0 soft=NONE display=FALSE > test9a.log;
#
echo -n "9b...";
delobj ech_temp.more.echelle.id_lines;
delobj ech_temp.more.echelle.id_count;
ech_idwave ech_rdctn=ech_temp tune_maxpoly=50 \
  ech_ftrdb=\$ARCDIRS/THAR auto_id=yes wavfit=poly w_npoly=7 \
  min_dispersion=0.01 max_dispersion=0.2 low_wave=0 \
  hi_wave=0 soft=NONE > test9b.log;
#
echo -n "9c...";
delobj ech_temp.more.echelle.id_lines;
delobj ech_temp.more.echelle.id_count;
ech_idwave ech_rdctn=ech_temp tune_maxpoly=50 \
  ech_ftrdb=\$ARCDIRS/THAR auto_id=yes w_npoly=7 \
  min_dispersion=0.02 max_dispersion=0.1 wavfit=poly low_wave=0 \
  hi_wave=0 soft=NONE > test9c.log;
#
echo -n "9d...";
delobj ech_temp.more.echelle.id_lines;
delobj ech_temp.more.echelle.id_count;
ech_idwave ech_rdctn=ech_temp tune_maxpoly=50 \
  ech_ftrdb=\$ARCDIRS/THAR auto_id=yes  w_npoly=7 \
  min_dispersion=0.02 max_dispersion=0.1 wavfit=poly low_wave=0 \
  hi_wave=0 soft=NONE > test9d.log;
#
echo -n "9e...";
delobj ech_temp.more.echelle.id_lines;
delobj ech_temp.more.echelle.id_count;
ech_idwave ech_rdctn=ech_temp tune_maxpoly=50 \
  ech_ftrdb=\$ARCDIRS/THAR auto_id=yes w_npoly=20 \
  min_dispersion=0.02 max_dispersion=0.1 wavfit=spline low_wave=0 \
  hi_wave=0 soft=NONE > test9e.log;
#
echo -n "9f...";
delobj ech_temp.more.echelle.id_lines;
delobj ech_temp.more.echelle.id_count;
ech_idwave ech_rdctn=ech_temp tune_maxpoly=50 \
  ech_ftrdb=\$ARCDIRS/THAR auto_id=yes w_npoly=20 \
  min_dispersion=0.01 max_dispersion=0.2 wavfit=spline low_wave=0 \
  hi_wave=0 soft=NONE > test9f.log;
#
echo -n "9g...";
delobj ech_temp.more.echelle.id_lines;
delobj ech_temp.more.echelle.id_count;
ech_idwave ech_rdctn=ech_temp tune_maxpoly=50 \
  ech_ftrdb=\$ARCDIRS/THAR auto_id=yes w_npoly=20 \
  min_dispersion=0.02 max_dispersion=0.1 wavfit=spline low_wave=0 \
  hi_wave=0 soft=NONE > test9g.log;
#
echo -n "9h...";
delobj ech_temp.more.echelle.id_lines;
delobj ech_temp.more.echelle.id_count;
ech_idwave ech_rdctn=ech_temp tune_maxpoly=50 \
  ech_ftrdb=\$ARCDIRS/THAR auto_id=yes w_npoly=20 \
  min_dispersion=0.02 max_dispersion=0.1 wavfit=spline low_wave=0 \
  hi_wave=0 soft=NONE > test9h.log;
#
echo -n "9i...";
delobj ech_temp.more.echelle.id_lines;
delobj ech_temp.more.echelle.id_count;
ech_idwave ech_rdctn=ech_temp tune_maxpoly=50 \
  ech_ftrdb=\$ARCDIRS/THAR auto_id=yes wavfit=poly w_npoly=7 \
  min_dispersion=0.02 max_dispersion=0.1 low_wave=0 \
  hi_wave=0 soft=NONE > test9i.log;
echo "done.";
#
echo "";
echo "Comparing results with reference:";
foreach f ( test9?.log )
   diff $ECHOMOP_TEST/$f:r.ref $f;
end
echo "\!\!  End of comparison.";
test10:
#
# Test 10.
# =======
#
#echo "";
#echo -n "Copying test data to the working directory...";
#cp -f $ECHOMOP_DEMO/*test*.sdf .;
#mv -f ech_test.sdf ech_temp.sdf;
#echo "done.";
#
echo "";
echo "TEST 10.........................................................";
echo "This test checks the behaviour of the scrunching task in both";
echo "global and per-order bin sizes.  The options for automatically-";
echo "calculated and user-specified wavelength scales are tested,";
echo "as are options for conserving (or not) flux.";
echo "";
#
echo -n "10a...";
ech_scrunch ech_rdctn=ech_temp set_wscale=yes bin_size=0 start_wave=0  \
  scrunch_type=obj soft=NONE tune_maxpoly=50 display=FALSE > test10a.log;
#
echo -n "10b...";
ech_scrunch ech_rdctn=ech_temp set_wscale=no bin_size=0 start_wave=0  \
  scrunch_type=obj soft=NONE tune_maxpoly=50 > test10b.log;
#
echo -n "10c...";
ech_scrunch ech_rdctn=ech_temp set_wscale=yes bin_size=1 start_wave=4200  \
  scrunch_type=obj soft=NONE tune_maxpoly=50 > test10c.log;
#
echo -n "10d...";
#ech_scrunch ech_rdctn=ech_temp set_wscale=no bin_size=1 start_wave=4200  \
#  scrunch_type=obj soft=NONE tune_maxpoly=50 > test10d.log;
#
echo -n "10e...";
ech_scrunch ech_rdctn=ech_temp set_wscale=yes bin_size=0 start_wave=0  \
  tune_flux=yes scrunch_type=obj soft=NONE tune_maxpoly=50 > test10e.log;
echo "done.";
#
echo "";
echo "Comparing results with reference:";
foreach f ( test10?.log )
   diff $ECHOMOP_TEST/$f:r.ref $f;
end
echo "\!\!  End of comparison.";
test11:
#
# Test 11.
# =======
#
echo "";
echo -n "Copying test data to the working directory...";
cp -f $ECHOMOP_DEMO/*test*.sdf .;
mv -f ech_test.sdf ech_temp.sdf;
echo "done.";
#
echo "";
echo "TEST 11.........................................................";
echo "This test checks the various extraction algorithms available.";
echo "It also varies the tunable parameters controlling the ADU and";
echo "readout-noise and the the effect of disabling the flat-field";
echo "and the arc extraction.  All these tests use the ECH_RESULT task";
echo "to create an ASCII listing of the output spectrum.";
echo "";
#
echo -n "11a...";
ech_extrct ech_rdctn=ech_temp arc=test_arc ffield=test_flat extract_mode=S \
 idx_num_orders=0 inptim=test_obj display=FALSE tune_maxpoly=50 \
 readout_noise=0 photon_to_adu=1 soft=NONE > test11a.log;
ech_result ech_rdctn=ech_temp result_type=EXTOBJ \
 idx_num_orders=0 result_format=ASCII ech_rducd=spectrum \
 soft=NONE display=FALSE ASCII_FILE=echomop_output.txt >> test11a.log;
more echomop_output.txt >> test11a.log;
rm echomop_output.txt;
rm spectrum.sdf;
#
echo -n "11b...";
ech_extrct ech_rdctn=ech_temp arc=test_arc ffield=test_flat extract_mode=P \
 idx_num_orders=0 inptim=test_obj tune_maxpoly=50 \
 readout_noise=0 photon_to_adu=1 soft=NONE > test11b.log;
ech_result ech_rdctn=ech_temp result_type=EXTOBJ \
 idx_num_orders=0 result_format=ASCII ech_rducd=spectrum \
 soft=NONE ASCII_FILE=echomop_output.txt >> test11b.log;
cat echomop_output.txt >> test11b.log;
rm echomop_output.txt;
rm spectrum.sdf;
#
echo -n "11c...";
ech_extrct ech_rdctn=ech_temp arc=test_arc ffield=test_flat extract_mode=O \
 idx_num_orders=0 inptim=test_obj tune_maxpoly=50 \
 readout_noise=0 photon_to_adu=1 soft=NONE > test11c.log;
ech_result ech_rdctn=ech_temp result_type=EXTOBJ \
 idx_num_orders=0 result_format=ASCII ech_rducd=spectrum \
 soft=NONE ASCII_FILE=echomop_output.txt >> test11c.log;
cat echomop_output.txt >> test11c.log;
rm spectrum.sdf;
rm echomop_output.txt;
#
echo -n "11d...";
ech_qextr ech_rdctn=ech_temp arc=test_arc ffield=test_flat  \
 idx_num_orders=0 inptim=test_obj tune_maxpoly=50 \
 readout_noise=0 photon_to_adu=1 display=FALSE soft=NONE > test11d.log;
ech_result ech_rdctn=ech_temp result_type=EXTOBJ \
 idx_num_orders=0 result_format=ASCII ech_rducd=spectrum \
 soft=NONE ASCII_FILE=echomop_output.txt >> test11d.log;
cat echomop_output.txt >> test11d.log;
rm spectrum.sdf;
rm echomop_output.txt;
#
echo -n "11e...";
ech_extrct ech_rdctn=ech_temp arc=test_arc ffield=test_flat extract_mode=O \
 idx_num_orders=0 inptim=test_obj tune_maxpoly=50 \
 readout_noise=20 photon_to_adu=1 soft=NONE > test11e.log;
ech_result ech_rdctn=ech_temp result_type=EXTOBJ \
 idx_num_orders=0 result_format=ASCII ech_rducd=spectrum \
 soft=NONE ASCII_FILE=echomop_output.txt >> test11e.log;
cat echomop_output.txt >> test11e.log;
rm spectrum.sdf;
rm echomop_output.txt;
#
echo -n "11f...";
ech_extrct ech_rdctn=ech_temp arc=test_arc ffield=test_flat extract_mode=O \
 idx_num_orders=0 inptim=test_obj tune_maxpoly=50 \
 readout_noise=5 photon_to_adu=5 soft=NONE > test11f.log;
ech_result ech_rdctn=ech_temp result_type=EXTOBJ \
 idx_num_orders=0 result_format=ASCII ech_rducd=spectrum \
 soft=NONE ASCII_FILE=echomop_output.txt >> test11f.log;
cat echomop_output.txt >> test11f.log;
rm spectrum.sdf;
rm echomop_output.txt;
echo "done.";
#
echo "";
echo "Comparing results with reference:";
foreach f ( test11?.log )
   diff $ECHOMOP_TEST/$f:r.ref $f;
end
echo "\!\!  End of comparison.";
test12:
#
# Test 12.
# =======
#
echo "";
echo -n "Copying test data to the working directory...";
cp -f $ECHOMOP_DEMO/*test*.sdf .;
mv -f ech_test.sdf ech_temp.sdf;
echo "done.";
#
echo "";
echo "TEST 12.........................................................";
echo "This test checks the cloning facilities for the major processing";
echo "options.  Results are cloned from ech_test.sdf to ech_copy.sdf";
echo "and are checked using hdstrace.";
echo "";
#
echo -n "12a...";
echmenu ech_rdctn=ech_copy tracim=test_flat inptim=test_obj arc=test_arc \
  tune_clone=ech_temp tune_autloc=yes tune_automate="'1,EXIT'" > test12a.log;
hdstrace ech_copy.more.echelle nlines=4 >> test12a.log;
#
echo -n "12b...";
ech_trace ech_rdctn=ech_copy idx_num_orders=0 tracim=test_flat trcfit=poly \
 tune_clone=ech_temp trc_npoly=7 tune_maxpoly=50 trace_mode=c \
 soft=NONE > test12b.log;
hdstrace ech_copy.more.echelle nlines=4 >> test12b.log;
#
echo -n "12c...";
ech_fitord ech_rdctn=ech_copy idx_num_orders=0 trcfit=poly trc_interact=no  \
  tune_clone=ech_temp trc_npoly=7 tune_maxpoly=50 soft=NONE > test12c.log;
hdstrace ech_copy.more.echelle nlines=4 >> test12c.log;
#
echo -n "12d...";
ech_spatial ech_rdctn=ech_copy slitim=test_flat pfl_interact=no \
  tune_clone=ech_temp inptim=test_obj tune_maxpoly=50 soft=NONE > test12d.log;
hdstrace ech_copy.more.echelle nlines=4 >> test12d.log;
#
echo -n "12e...";
ech_ffield ech_rdctn=ech_copy idx_num_orders=0 ffield=test_flat fltfit=mean \
   tune_clone=ech_temp tune_maxpoly=50 soft=NONE > test12e.log;
hdstrace ech_copy.more.echelle nlines=4 >> test12e.log;
#
echo -n "12f...";
ech_sky ech_rdctn=ech_copy idx_num_orders=0 ffield=test_flat \
  tune_clone=ech_temp skyfit=mean inptim=test_obj tune_maxpoly=50 \
  readout_noise=0 photon_to_adu=1 soft=NONE > test12f.log;
hdstrace ech_copy.more.echelle nlines=4 >> test12f.log;
#
echo -n "12g...";
ech_profile ech_rdctn=ech_copy inptim=test_obj tune_use_nxf=0.2 \
  tune_clone=ech_temp tune_maxpoly=50 soft=NONE > test12g.log;
hdstrace ech_copy.more.echelle nlines=4 >> test12g.log;
#
echo -n "12h...";
ech_extrct ech_rdctn=ech_copy arc=test_arc ffield=test_flat extract_mode=S \
 tune_clone=ech_temp inptim=test_obj tune_maxpoly=50 \
 readout_noise=0 photon_to_adu=1 soft=NONE > test12h.log;
hdstrace ech_copy.more.echelle nlines=4 >> test12h.log;
#
echo -n "12i...";
ech_linloc ech_rdctn=ech_copy arc=test_arc tune_maxpoly=50 \
  tune_clone=ech_temp tune_rflnthr=1.25 soft=NONE > test12i.log;
hdstrace ech_copy.more.echelle nlines=4 >> test12i.log;
#
echo -n "12j...";
ech_idwave ech_rdctn=ech_copy tune_maxpoly=50 \
  ech_ftrdb=\$ARCDIRS/THAR auto_id=yes wavfit=poly w_npoly=7 \
  tune_clone=ech_temp min_dispersion=0.02 max_dispersion=0.1 low_wave=4200 \
  hi_wave=4500 soft=NONE > test12j.log;
hdstrace ech_copy.more.echelle nlines=4 >> test12j.log;
echo "done.";
#
echo "";
echo "Comparing results with reference:"
foreach f ( test12?.log )
   diff $ECHOMOP_TEST/$f:r.ref $f;
end
echo "\!\!  End of comparison.";
#
quit:
alias rm $rm_old;
alias cp $cp_old;
echo "\!\!  End of tests.";
#
