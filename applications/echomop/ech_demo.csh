
#
# ECHOMOP demo script
#
# Must be SOURCED, not executed.
#
# History:
#    ???? (Dave Mills?)
#       Original version
#    14-Jan-2002 (Norman Gray)
#       Tidied up interaction with user; removed apparently redundant
#       tuning parameter from call to ech_idwave.

set softdefault = 'xw'
setenv ADAM_USER $cwd

# Check ECHOMOP_DIR is defined.
if (! $?ECHOMOP_DIR) then
    setenv ECHOMOP_DIR $STARLINK/bin/echomop
endif
# Check that ECHOMOP_BASE is defined, and if it isn't source the startup file
# (we don't need this variable below, but it serves as a marker that
# the startup file has been sourced)
if (! $?ECHOMOP_BASE) then
    source $ECHOMOP_DIR/echomop.csh
endif
# Check that ECHOMOP is in the path, and add it if it isn't.
if ( `expr $ECHOMOP_DIR : $PATH` == 0 ) then
    setenv PATH ${ECHOMOP_DIR}:$PATH
endif

# Call echmenu with the `version' parameter.  This produces the
# ECHOMOP version number and immediately exits.  This isn't much use
# directly, but if it fails, it indicates that echmenu (along with the
# other commands) isn't in the path, and hence that the previous
# stanza's setup went wrong somehow.  There ought to be a cleaner way
# of testing for this, but csh doesn't seem to provide it.
echmenu version=true
if ($status) then
    echo "Can't find echmenu in PATH."
    echo "Set up ECHOMOP with the command `echomop',"
    echo "and source ech_demo.csh again."
    exit 1
endif


# Ensure that ECHOMOP_DEMO is defined
if (! $?ECHOMOP_DEMO) then
    setenv ECHOMOP_DEMO $ECHOMOP_DIR/demo
endif


# Print a message and wait for the user to press return.
alias wait_to_start 'echo "Press <return> to begin this step"; set line = "$<"; cat $ECHOMOP_DEMO/next.txt'
alias wait_to_continue 'echo "Press <return> to continue"; set line = "$<";clear'

echo -n "Copying demonstration frames to local directory...";
cp $ECHOMOP_DEMO/test*.sdf .;
echo "done.";
#
more $ECHOMOP_DEMO/intro.txt

#  Get SOFT device name.
if ( "$1" == "" ) then
   echo -n "SOFT - Produce plots on screen /'$softdefault'/ > ";
   set soft = "$<";
   if ( "$soft" == "" ) then
      set soft = $softdefault;
   endif

else
   set soft = "$1";
endif


#
more $ECHOMOP_DEMO/ech_locate.txt
wait_to_start
echmenu ech_rdctn=ech_test tracim=test_obj inptim=test_obj arc=test_arc \
   tune_autloc=yes tune_automate="'1,EXIT'" display=NO soft=$soft
wait_to_continue
#
more $ECHOMOP_DEMO/ech_trace.txt
wait_to_start
ech_trace ech_rdctn=ech_test idx_num_orders=0 tracim=test_obj trcfit=poly \
   trc_npoly=7 trace_mode=tc soft=$soft display=NO
wait_to_continue
#
more $ECHOMOP_DEMO/ech_fitord.txt
wait_to_start
ech_fitord ech_rdctn=ech_test idx_num_orders=0 trcfit=poly trc_interact=no  \
   trc_npoly=7 soft=$soft display=NO
wait_to_continue
#
more $ECHOMOP_DEMO/ech_spatial.txt
wait_to_start
ech_spatial ech_rdctn=ech_test slitim=test_flat pfl_interact=no \
   inptim=test_obj tune_mxskypix=21 soft=$soft display=NO
wait_to_continue
#
more $ECHOMOP_DEMO/ech_ffield.txt
wait_to_start
ech_ffield ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat fltfit=mean \
   soft=$soft display=NO
wait_to_continue
#
more $ECHOMOP_DEMO/ech_sky.txt
wait_to_start
ech_sky ech_rdctn=ech_test idx_num_orders=0 ffield=test_flat skyfit=mean inptim=test_obj \
   readout_noise=0 photon_to_adu=1 soft=$soft display=NO
wait_to_continue
#
more $ECHOMOP_DEMO/ech_profile.txt
wait_to_start
ech_profile ech_rdctn=ech_test inptim=test_obj soft=$soft display=NO
wait_to_continue
#
more $ECHOMOP_DEMO/ech_extrct.txt
wait_to_start
ech_extrct ech_rdctn=ech_test arc=test_arc ffield=test_flat extract_mode=O \
   inptim=test_obj readout_noise=0 photon_to_adu=1 soft=$soft display=NO
wait_to_continue
#
more $ECHOMOP_DEMO/ech_linloc.txt
wait_to_start
ech_linloc ech_rdctn=ech_test arc=test_arc soft=$soft display=NO
wait_to_continue
#
more $ECHOMOP_DEMO/ech_idwave.txt
wait_to_start
# A previous version of this demo had tune_wcal_index=5000, but this
# must have been removed at some point in the past.
ech_idwave ech_rdctn=ech_test arc_type=$ARCDIRS/THAR.ARC \
   ech_ftrdb=$ARCDIRS/THAR auto_id=yes \
   min_dispersion=0.02 max_dispersion=0.1 wavfit=poly w_npoly=7 low_wave=4200 \
   hi_wave=4500 soft=$soft display=NO
wait_to_continue
#
more $ECHOMOP_DEMO/ech_scrunch.txt
wait_to_start
ech_scrunch ech_rdctn=ech_test set_wscale=yes bin_size=0 start_wave=0 \
  scrunch_type=obj soft=$soft display=NO
wait_to_continue
#
more $ECHOMOP_DEMO/ech_result.txt
wait_to_start
ech_result ech_rdctn=ech_test inptim=test_obj result_type=EXTOBJ \
   result_format=NDF ech_rducd=spectrum tune_archive=no \
   soft=$soft display=NO
#
wait_to_continue
more $ECHOMOP_DEMO/summary.txt
