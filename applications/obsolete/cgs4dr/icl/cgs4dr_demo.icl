{+
{ cgs4dr_demo.icl
{
{ Author: Phil Daly, JAC (pnd@jach.hawaii.edu)
{-

{
{ Remove any previuosly reduced groups
print 'Removing previously reduced groups and restoring configuration files'
sh rm $RGDIR/rg941011_43*.sdf $RGDIR/rg941011_110*.sdf $RGDIR/rg941011_118*.sdf ${CGS4_INDEX}/*.index
restore_config cgs4dr_demo
set_reduction_sequence YES NO YES NO YES YES NO YES YES NO YES
set_extract_spectrum 18 19 29 29 39 40 FALSE BRIGHT

{
{ Cancel all the queue entries and clear the screen(s)
cancel_all_qentries
print 'Starting the auto-reduction ... please wait'
sh clear
p4_clearall

{
{ Write a header
enter_string 'DRCOMMENT ** WELCOME TO THE PORTABLE-CGS4DR DEMONSTRATION **'
enter_string 'DRCOMMENT '

{
{ Reduce BIAS and DARK frames
enter_string 'DRCOMMENT ** THE FOLLOWING SEQUENCE REDUCES A BIAS FRAME FOR LATER USE **'
enter_obs_range 1 1
enter_string 'DISPLAY DATA=$RODIR/ro941011_1 PORT=0'

{
{ Reduce a standard star
enter_string 'DRCOMMENT '
enter_string 'DRCOMMENT ** THE FOLLOWING SEQUENCE REDUCES AN ECHELLE SPECTRUM OF BS 6998 AT 2.20 um **'
enter_string 'DRCOMMENT ** THE DATA WAS ACQUIRED IN STARE+NDR MODE AT AN AIRMASS OF 1.685 **'
enter_string 'DRCOMMENT ** THE STANDARD APPEARS IN ROWS 24 AND 34 BY NODDING UKIRT ALONG THE SLIT **'
enter_string 'DRCOMMENT ** THE GROUP CONSISTS OF THE AUTOMATED ADDITION OF 8 REDUCED OBSERVATIONS **'
enter_obs_range 43 50
enter_string 'DISPLAY DATA=$RGDIR/rg941011_43 PORT=7'
enter_string 'DISPLAY DATA=$RGDIR/rg941011_43 PORT=8'

{
{ Reduce a standard for NGC1068 (to follow)
enter_string 'DRCOMMENT '
enter_string 'DRCOMMENT ** THE FOLLOWING SEQUENCE REDUCES A SPECTRUM OF SAO 129752 AT 3.44 um **'
enter_string 'DRCOMMENT ** THE DATA WAS ACQUIRED IN CHOP MODE AT AN AIRMASS OF 1.084  **'
enter_string 'DRCOMMENT ** THE POSITIVE BEAM (ROW 29) IS FLANKED BY TWO NEGATIVE BEAMS (ROWS 19/39) **'
enter_string 'DRCOMMENT ** THE GROUP CONSISTS OF THE AUTO-ADDITION OF 8 REDUCED OBSERVATIONS **'
enter_obs_range 110 117
enter_string 'DISPLAY DATA=$RGDIR/rg941011_110_imspc PORT=0'

{
{ Reduce NGC 1068
enter_string 'DRCOMMENT '
enter_string 'DRCOMMENT ** THE FOLLOWING SEQUENCE REDUCES A SPECTRUM OF GALAXY NGC 1068 AT 3.44 um **'
enter_string 'DRCOMMENT ** THE DATA WAS ACQUIRED IN CHOP+NOD MODE AT AN AIRMASS OF 1.063 **'
enter_string 'DRCOMMENT ** THE DATA REDUCTION FINDS THE REDUCED BIAS OBSERVATION AUTOMATICALLY **'
enter_string 'DRCOMMENT ** THE GROUP CONSISTS OF THE AUTOMATED ADDITION OF 20 REDUCED OBSERVATIONS **'
enter_obs_range 118 137
enter_string 'DISPLAY DATA=$RGDIR/rg941011_118_imspc PORT=0'

enter_string 'DRCOMMENT '
enter_string 'DRCOMMENT ** NOW RUN CGS4DR_DEMO2 **'
start_autoreduce
