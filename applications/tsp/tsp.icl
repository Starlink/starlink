{+
{  Name:
{     tsp.icl
{
{  Purpose:
{     Defines TSP commands and help for ICL usage.
{
{  Type of Module:
{     ICL procedure
{
{  Invocation:
{     load $TSP_DIR/tsp
{
{  Description:
{     This procedure starts TSP for use from UNIX ICL by defining
{     the TSP commands needed to execute each application or to get
{     help about TSP.
{
{  Authors:
{     JAB: Jeremy Bailey (AAO)
{     BLY: M.J.Bly, (Starlink, RAL)
{     {enter_new_authors_here}
{
{  History:
{     1996 May 1 (JAB):
{        Original version.
{     1997 Aug 23 (BLY):
{        Added IRISPOLC commands.
{     {enter_further_changes_here}
{
{  Bugs:
{     {note_any_bugs_here}
{
{-

{ Define TSP commands,

      define build3d ${TSP_DIR}/tsp_mon
      define tsp_build3d ${TSP_DIR}/tsp_mon

      define calfit ${TSP_DIR}/tsp_mon
      define tsp_calfit ${TSP_DIR}/tsp_mon

      define calfitpa ${TSP_DIR}/tsp_mon
      define tsp_calfitpa ${TSP_DIR}/tsp_mon

      define calib ${TSP_DIR}/tsp_mon
      define tsp_calib ${TSP_DIR}/tsp_mon

      define calpa ${TSP_DIR}/tsp_mon
      define tsp_calpa ${TSP_DIR}/tsp_mon

      define ccd2pol ${TSP_DIR}/tsp_mon
      define tsp_ccd2pol ${TSP_DIR}/tsp_mon

      define ccd2stokes ${TSP_DIR}/tsp_mon
      define tsp_ccd2stokes ${TSP_DIR}/tsp_mon

      define ccdphot ${TSP_DIR}/tsp_mon
      define tsp_ccdphot ${TSP_DIR}/tsp_mon

      define ccdpol ${TSP_DIR}/tsp_mon
      define tsp_ccdpol ${TSP_DIR}/tsp_mon

      define cgs4pol ${TSP_DIR}/tsp_mon
      define tsp_cgs4pol ${TSP_DIR}/tsp_mon

      define cmult ${TSP_DIR}/tsp_mon
      define tsp_cmult ${TSP_DIR}/tsp_mon

      define combine ${TSP_DIR}/tsp_mon
      define tsp_combine ${TSP_DIR}/tsp_mon

      define display ${TSP_DIR}/tsp_mon
      define tsp_display ${TSP_DIR}/tsp_mon

      define divide ${TSP_DIR}/tsp_mon
      define tsp_divide ${TSP_DIR}/tsp_mon

      define dstokes ${TSP_DIR}/tsp_mon
      define tsp_dtokes ${TSP_DIR}/tsp_mon

      define eplot ${TSP_DIR}/tsp_mon
      define tsp_eplot ${TSP_DIR}/tsp_mon

      define extin ${TSP_DIR}/tsp_mon
      define tsp_extin ${TSP_DIR}/tsp_mon

      define floconv ${TSP_DIR}/tsp_mon
      define tsp_flconv ${TSP_DIR}/tsp_mon

      define flip ${TSP_DIR}/tsp_mon
      define tsp_flip ${TSP_DIR}/tsp_mon

      define fplot ${TSP_DIR}/tsp_mon
      define tsp_fplot ${TSP_DIR}/tsp_mon

      define imotion ${TSP_DIR}/tsp_mon
      define tsp_imotion ${TSP_DIR}/tsp_mon

      define impol ${TSP_DIR}/tsp_mon
      define tsp_impol ${TSP_DIR}/tsp_mon

      define ipcs2stokes ${TSP_DIR}/tsp_mon
      define tsp_ipcs2stokes ${TSP_DIR}/tsp_mon

      define irflux ${TSP_DIR}/tsp_mon
      define tsp_irflux ${TSP_DIR}/tsp_mon

      define irisap ${TSP_DIR}/tsp_mon
      define tsp_irisap ${TSP_DIR}/tsp_mon

      define irisapc ${TSP_DIR}/tsp_mon
      define tsp_irisapc ${TSP_DIR}/tsp_mon

      define irispol ${TSP_DIR}/tsp_mon
      define tsp_irispol ${TSP_DIR}/tsp_mon

      define irispolc ${TSP_DIR}/tsp_mon
      define tsp_irispolc ${TSP_DIR}/tsp_mon

      define lhatpol ${TSP_DIR}/tsp_mon
      define tsp_lhatpol ${TSP_DIR}/tsp_mon

      define lmerge ${TSP_DIR}/tsp_mon
      define tsp_lmerge ${TSP_DIR}/tsp_mon

      define ltcorr ${TSP_DIR}/tsp_mon
      define tsp_ltcorr ${TSP_DIR}/tsp_mon

      define phaseplot ${TSP_DIR}/tsp_mon
      define tsp_phaseplot ${TSP_DIR}/tsp_mon

      define pplot ${TSP_DIR}/tsp_mon
      define tsp_pplot ${TSP_DIR}/tsp_mon

      define ptheta ${TSP_DIR}/tsp_mon
      define tsp_ptheta ${TSP_DIR}/tsp_mon

      define qplot ${TSP_DIR}/tsp_mon
      define tsp_qplot ${TSP_DIR}/tsp_mon

      define qumerge ${TSP_DIR}/tsp_mon
      define tsp_qumerge ${TSP_DIR}/tsp_mon

      define quplot ${TSP_DIR}/tsp_mon
      define tsp_quplot ${TSP_DIR}/tsp_mon

      define qusub ${TSP_DIR}/tsp_mon
      define tsp_qusub ${TSP_DIR}/tsp_mon

      define rccdts ${TSP_DIR}/tsp_mon
      define tsp_rccdts ${TSP_DIR}/tsp_mon

      define rcgs2 ${TSP_DIR}/tsp_mon
      define tsp_rcgs2 ${TSP_DIR}/tsp_mon

      define reverse ${TSP_DIR}/tsp_mon
      define tsp_reverse ${TSP_DIR}/tsp_mon

      define rfigaro ${TSP_DIR}/tsp_mon
      define tsp_rfigaro ${TSP_DIR}/tsp_mon

      define rhathsp ${TSP_DIR}/tsp_mon
      define tsp_rhathsp ${TSP_DIR}/tsp_mon

      define rhatpol ${TSP_DIR}/tsp_mon
      define tsp_rhatpol ${TSP_DIR}/tsp_mon

      define rhdsplot ${TSP_DIR}/tsp_mon
      define tsp_rhdsplot ${TSP_DIR}/tsp_mon

      define rhsp3 ${TSP_DIR}/tsp_mon
      define tsp_rhsp3 ${TSP_DIR}/tsp_mon

      define rirps ${TSP_DIR}/tsp_mon
      define tsp_rirps ${TSP_DIR}/tsp_mon

      define rotpa ${TSP_DIR}/tsp_mon
      define tsp_rotpa ${TSP_DIR}/tsp_mon

      define rturku ${TSP_DIR}/tsp_mon
      define tsp_rturku ${TSP_DIR}/tsp_mon

      define scrunch ${TSP_DIR}/tsp_mon
      define tsp_scrunch ${TSP_DIR}/tsp_mon

      define shiftadd ${TSP_DIR}/tsp_mon
      define tsp_shiftadd ${TSP_DIR}/tsp_mon

      define skysub ${TSP_DIR}/tsp_mon
      define tsp_skysub ${TSP_DIR}/tsp_mon

      define slist ${TSP_DIR}/tsp_mon
      define tsp_slist ${TSP_DIR}/tsp_mon

      define spflux ${TSP_DIR}/tsp_mon
      define tsp_spflux ${TSP_DIR}/tsp_mon

      define splot ${TSP_DIR}/tsp_mon
      define tsp_splot ${TSP_DIR}/tsp_mon

      define subset ${TSP_DIR}/tsp_mon
      define tsp_subset ${TSP_DIR}/tsp_mon

      define subtract ${TSP_DIR}/tsp_mon
      define tsp_subtract ${TSP_DIR}/tsp_mon

      define tbin ${TSP_DIR}/tsp_mon
      define tsp_tbin ${TSP_DIR}/tsp_mon

      define tcadd ${TSP_DIR}/tsp_mon
      define tsp_tcadd ${TSP_DIR}/tsp_mon

      define tderiv ${TSP_DIR}/tsp_mon
      define tsp_tderiv ${TSP_DIR}/tsp_mon

      define textin ${TSP_DIR}/tsp_mon
      define tsp_textin ${TSP_DIR}/tsp_mon

      define tlist ${TSP_DIR}/tsp_mon
      define tsp_tlist ${TSP_DIR}/tsp_mon

      define tmerge ${TSP_DIR}/tsp_mon
      define tsp_tmerge ${TSP_DIR}/tsp_mon

      define tsetbad ${TSP_DIR}/tsp_mon
      define tsp_tsetbad ${TSP_DIR}/tsp_mon

      define tsextract ${TSP_DIR}/tsp_mon
      define tsp_tsextract ${TSP_DIR}/tsp_mon

      define tshift ${TSP_DIR}/tsp_mon
      define tsp_tshift ${TSP_DIR}/tsp_mon

      define tsplot ${TSP_DIR}/tsp_mon
      define tsp_tsplot ${TSP_DIR}/tsp_mon

      define tsprofile ${TSP_DIR}/tsp_mon
      define tsp_tsprofile ${TSP_DIR}/tsp_mon

      define xcopy ${TSP_DIR}/tsp_mon
      define tsp_xcopy ${TSP_DIR}/tsp_mon

print  " "
print  "   TSP commands are now available -- (Version PKG_VERS)"
print  " "

