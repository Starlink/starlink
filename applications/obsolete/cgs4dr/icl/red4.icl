{+
{
{ red4.icl
{
{ author: Phil Daly
{ date:   12-Dec-1994
{
{-

{
{ Do not save procedures
set nosave
set nocheckpars

{
{ Get some semi-global variables
red4_alias  = getenv("PID") & 'red4'
red4_date   = getenv("RED4_DATE")
red4_index  = '$CGS4_INDEX/cgs4_' & getenv("CGS4_DATE") & '.index'
red4_format = getenv("CGS4_FORMAT")

{
{ Defstring the basic commands
defstring red4_add_int obeyw (red4_alias) add_int
defstring red4_add_obs obeyw (red4_alias) add_obs
defstring red4_add_pair obeyw (red4_alias) add_pair
defstring red4_adjoin obeyw (red4_alias) adjoin
defstring red4_archive_obs obeyw (red4_alias) archive_obs
defstring red4_apply_mask obeyw (red4_alias) apply_mask
defstring red4_black_body obeyw (red4_alias) black_body
defstring red4_clean_obs obeyw (red4_alias) clean_obs
defstring red4_close_log obeyw (red4_alias) close_log
defstring red4_cre_error_mask obeyw (red4_alias) cre_error_mask
defstring red4_cre_thresh_mask obeyw (red4_alias) cre_thresh_mask
defstring red4_cre_window_mask obeyw (red4_alias) cre_window_mask
defstring red4_divide_by_std obeyw (red4_alias) divide_by_std
defstring red4_edit_mask obeyw (red4_alias) edit_mask
defstring red4_efficiency obeyw (red4_alias) efficiency
defstring red4_emlt obeyw (red4_alias) emlt
defstring red4_extract3 obeyw (red4_alias) extract3
defstring red4_extract4 obeyw (red4_alias) extract4
defstring red4_extract_mask obeyw (red4_alias) extract_mask
defstring red4_file_calib obeyw (red4_alias) file_calib
defstring red4_file_obs obeyw (red4_alias) file_obs
defstring red4_file_std obeyw (red4_alias) file_standard
defstring red4_file_standard obeyw (red4_alias) file_standard
defstring red4_iadd4 obeyw (red4_alias) iadd4
defstring red4_idiv4 obeyw (red4_alias) idiv4
defstring red4_isub4 obeyw (red4_alias) isub4
defstring red4_imult4 obeyw (red4_alias) imult4
defstring red4_iand4 obeyw (red4_alias) iand4
defstring red4_ieor4 obeyw (red4_alias) ieor4
defstring red4_ior4 obeyw (red4_alias) ior4
defstring red4_inot4 obeyw (red4_alias) inot4
defstring red4_init obeyw (red4_alias) init
defstring red4_irflat obeyw (red4_alias) irflat
defstring red4_list_index obeyw (red4_alias) list_index
defstring red4_mend obeyw (red4_alias) mend
defstring red4_nodextract4 obeyw (red4_alias) nodextract4
defstring red4_normalise_ff obeyw (red4_alias) normalise_ff
defstring red4_poke_fits obeyw (red4_alias) poke_fits
defstring red4_polyfit obeyw (red4_alias) polyfit
defstring red4_red_int obeyw (red4_alias) red_int
defstring red4_red_obs obeyw (red4_alias) red_obs
defstring red4_remove_int obeyw (red4_alias) remove_int
defstring red4_remove_obs obeyw (red4_alias) remove_obs
defstring red4_remove_pair obeyw (red4_alias) remove_pair
defstring red4_reset obeyw (red4_alias) reset
defstring red4_set_format obeyw (red4_alias) set_format
defstring red4_set_verbose obeyw (red4_alias) set_verbose
defstring red4_stats obeyw (red4_alias) stats
defstring red4_status obeyw (red4_alias) status

{
{ Defstring some miscellaneous commands
defstring red4_loadw     loadw $CGS4_EXE/red4 (red4_alias)
defstring red4_killw     killw (red4_alias)
defstring red4_verbose   obeyw (red4_alias) set_verbose true
defstring red4_noverbose obeyw (red4_alias) set_verbose false
defstring red4_subtract  obeyw (red4_alias) isub4
defstring cgs4list       !${CGS4DR_ROOT}/list_uktdata cgs4   ${ODIR}
defstring cgs3list       !${CGS4DR_ROOT}/list_uktdata cgs3   ${ODIR}
defstring ircam3list     !${CGS4DR_ROOT}/list_uktdata ircam3 ${OSIR}

{
{ Defstring some commands rather than use procedures
defstring list_index obeyw (red4_alias) list_index index_file=(red4_index) oformat=1 \
defstring list_index1 obeyw (red4_alias) list_index index_file=(red4_index) oformat=1 \
defstring list_index2 obeyw (red4_alias) list_index index_file=(red4_index) oformat=2 \
defstring set_obsbad obeyw (red4_alias) file_obs type='BAD'
defstring bad_observation obeyw (red4_alias) file_obs type='BAD'
defstring file_calibration obeyw (red4_alias) file_calib
defstring file_calib obeyw (red4_alias) file_calib
defstring file_observation obeyw (red4_alias) file_obs
defstring file_obs obeyw (red4_alias) file_obs type='WHATEVER_IT_IS'
defstring array_tests analyze_array (red4_alias)

{ PROC ANALYZE_ARRAY
proc analyze_array red4_alias number

  {+
  { This procedure follows the ARRAY_TESTS exec and logs the read noise and
  { dark current. The exec does the following:
  {   Obs 1: BIAS
  {   Obs 2: BIAS
  {   Obs 3: NDSTARE 1.0 sec
  {   Obs 4: NDSTARE 1.0 sec
  {   Obs 5: STARE   5.0 sec
  {   Obs 6: STARE  60.0 sec
  {
  { The read noise is calculated:
  {   1. Create an image = Obs3 - Obs4
  {   2. Do STATS on all good pixels between -50 and +50 on that image
  {   3. Read standard deviation (std.dev) from STATS output
  {   4. READ NOISE = ( (STD.DEV)/SQRT(2) ) * 6
  {      where 6 is the number of electrons / data number
  {   A typical answer is 40 electrons
  {   Nominal value is between +30 and +50 elec.
  {
  { The dark current is calculated:
  {   1. Create an image = Obs6 - Obs5
  {   2. Do STATS on that image
  {   3. Read median (mdn) of all good pixels from HISTAT output
  {   4. DARK CURRENT = ( MDN / 55.0 ) * 6
  {      where 6 is the number of electrons / data number
  {   A typical answer is less than 0.5 electrons / second
  {   Nominal value is between -0.5 and +2.0 elec/sec.
  {-

  { Get the UTdate for these observations
  utdate = getenv("CGS4_DATE")
  utdate = INTEGER(utdate)
  atst = INTEGER(number)
  cgs4_eng_file = getenv("CGS4_INDEX") & '/cgs4_array_tests.results'

  { Print out a message
  print ' '
  print '*** ARRAY_TESTS ANALYSIS ***'
  print ' '
  print ' '
  print 'Analyzing ARRAY_TESTS data for'(utdate:6)'starting at obs #'(atst:3)

  { Write output to file
  if file_exists(cgs4_eng_file)
    print 'Writing results to ' (cgs4_eng_file)
    append ofile (cgs4_eng_file)
  else
    print 'Creating file ' (cgs4_eng_file)
    create ofile (cgs4_eng_file)
    dl = 'UTdate   Read Noise  Dark Current'
    write ofile (dl)
    dl = '------   ----------  ------------'
    write ofile (dl)
  end if

  { Subtract the first two images
  in1 = '$RODIR/ro'&(utdate)&'_'&(atst+2)
  in2 = '$RODIR/ro'&(utdate)&'_'&(atst+3)
  out = '$RODIR/im3minus4'
  print 'Subtracting'(in2)'from'(in1)'to create'(out)
  red4_subtract IMAGE1=(in1) IMAGE2=(in2) OUTPUT=(out) ERRORS="GAUSSIAN"

  { Do stats on them
  print 'Evaluating statistics on output array'
  red4_stats DATA="$RODIR/im3minus4" PLANE="DATA" WHOLE=T AUTOSCALE=F LOW=-50 HIGH=50 MEAN=0 SIGMA=0 MEDIAN=0 MODE=0

  { Calculate the read noise
  sigma = ' '
  get (red4_alias) stats:sigma (sigma)
  stare_rn = ( REAL(sigma)/SQRT(2) ) * 6.0
  stare_rn = REAL(stare_rn)

  { Subtract the next two images
  in1 = '$RODIR/ro'&(utdate)&'_'&(atst+5)
  in2 = '$RODIR/ro'&(utdate)&'_'&(atst+4)
  out = '$RODIR/im6minus5'
  print 'Subtracting'(in2)'from'(in1)'to create'(out)
  red4_subtract IMAGE1=(in1) IMAGE2=(in2) OUTPUT=(out) ERRORS="GAUSSIAN"

  { Do stats on them
  print 'Evaluating statistics on output array'
  red4_stats DATA="$RODIR/im3minus4" PLANE="DATA" WHOLE=TRUE AUTOSCALE=TRUE MEAN=0 SIGMA=0 MEDIAN=0 MODE=0

  { Get the stats
  median = ' '
  mode = ' '
  get (red4_alias) stats:median (median)
  get (red4_alias) stats:mode (mode)

  { Calculate the dark currents
  median_dc = ( (median)/55.0 ) * 6.0
  median_dc = REAL(median_dc)
  modal_dc   = ( (mode)/55.0 ) * 6.0
  modal_dc   = REAL(modal_dc)

  { Report findings
  if stare_rn < 30.0
    print ' '
    print '*** DOUBLE CORRELATED READ NOISE ('(stare_rn)'electrons ) IS LOW ***'
  else if stare_rn > 50.0
    print ' '
    print '*** DOUBLE CORRELATED READ NOISE ('(stare_rn)'electrons ) IS HIGH ***'
  else
    print ' '
    print '*** DOUBLE CORRELATED READ NOISE ('(stare_rn)'electrons ) IS NOMINAL ***'
  endif

  if median_dc < -0.5
    print ' '
    print '*** MEDIAN DARK CURRENT ('(median_dc)'electrons/second ) IS LOW ***'
  else if median_dc > 2.0
    print ' '
    print '*** MEDIAN DARK CURRENT ('(median_dc)'electrons/second ) IS HIGH ***'
  else
    print ' '
    print '*** MEDIAN DARK CURRENT ('(median_dc)'electrons/second ) IS NOMINAL ***'
  endif

  if modal_dc < -0.5
    print ' '
    print '*** MODAL DARK CURRENT ('(modal_dc)'electrons/second ) IS LOW ***'
  else if modal_dc > 2.0
    print ' '
    print '*** MODAL DARK CURRENT ('(modal_dc)'electrons/second ) IS HIGH ***'
  else
    print ' '
    print '*** MODAL DARK CURRENT ('(modal_dc)'electrons/second ) IS NOMINAL ***'
  endif

  { Record in file
  dl = (utdate:6)&'  '&(stare_rn:10:6)&'  '&(median_dc:10.6)&'  '
  print (dl)
  write ofile (dl)
  close ofile
end proc


{
{ PROC FILE_STANDARD
proc file_standard p1 p2 p3 p4 p5
  {
  { Get the reduced group or observation
  if UNDEFINED(p1)
    input 'Group (or reduced observation) to file as a standard?  > ' (obs)
  else
    obs = p1
  endif
  {
  { Get the effective temperature
  if UNDEFINED(p2)
    inputr 'Effective temperature? > ' (tef)
  else
    tef = REAL(p2)
  endif
  {
  { Get the reference wavelength
  if UNDEFINED(p3)
    inputr 'Reference wavelength?  > ' (rfw)
  else
    rfw = REAL(p3)
  endif
  {
  { Get the start row
  if UNDEFINED(p4)
    inputr 'Start row for extraction?  > ' (yst)
  else
    yst = REAL(p4)
  endif
  {
  { Get the end row
  if UNDEFINED(p5)
    inputr 'End row for extraction?  > ' (yen)
  else
    yen = REAL(p5)
  endif
  {
  { File it
  whl = 'FALSE'
  print 'Filing' (obs) 'as a STANDARD'
  red4_file_std (obs) (tef) (rfw) (whl) ystart=(yst) yend=(yen) oper='AND' mend=T
endproc

{
{ PROC CALIBRATE
proc calibrate p1 p2 p3 p4 p5 p6
  {
  { Get the reduced observation and derive its number
  if UNDEFINED(p1)
    input 'Reduced observation to be calibrated?  > ' (obs)
  else
    obs = p1
  endif
  inobs = '$RODIR/' & SUBSTR( obs, INDEX(obs,'/')+1, LEN(obs) )
  spc   = '$RODIR/' & SUBSTR( obs, INDEX(obs,'/')+1, LEN(obs) ) & '_1d'
  clb   = '$RODIR/ca' & SUBSTR( obs, INDEX(obs,'/')+3, LEN(obs) )
  rbs   = '$ODIR/' & SUBSTR( obs, INDEX(obs,'/')+2, LEN(obs) )
  {
  { Get the start row
  if UNDEFINED(p2)
    inputr 'Start row for extraction?  > ' (yst)
  else
    yst = REAL(p2)
  endif
  {
  { Get the end row
  if UNDEFINED(p3)
    inputr 'End row for extraction?  > ' (yen)
  else
    yen = REAL(p3)
  endif
  {
  { Get the ARC type
  if UNDEFINED(p4)
    input 'Lamp? (argon, krypton, xenon, oh)  > ' (lmp)
  else
    lmp = p4
  endif
  {
  { Get the order
  if UNDEFINED(p5)
    inputr 'Order of polynomial for fit?  > ' (ord)
  else
    ord = REAL(p5)
  endif
  {
  { Get the sigma
  if UNDEFINED(p6)
    inputr 'Arcline half-width in pixels?  > ' (sig)
  else
    sig = REAL(p6)
  endif
  {
  { Extract the observation into a 1-D spectrum
  print 'Extracting' (inobs) 'into' (spc)
  red4_extract4 image=(inobs) ystart=(yst) yend=(yen) spectrum=(spc)
  {
  { Invoke Figaro ARC function
  arc = 'cgs4_' & (lmp) & '.arc'
  figaro
  print 'Invoking ARC to calibrate' (spc) 'into' (clb)
  arc spectrum=(spc) arctype=(arc) sigma=(sig) order=(ord) output=(clb) previous=F
  print 'Dividing result by 10000.0 to convert from Angstroms to Microns'
  xcdiv image=(clb) factor=10000.0 output=(clb)
  print 'Filing' (rbs) 'as a calibration'
  file_calibration observation=(rbs) change_label='TRUE' newlabel='Wavelength' newunits='microns'
endproc

{
{ PROC RED4_LOAD
proc red4_load
  red4_loadw
  red4_status
end proc

{
{ PROC RED4_KILL
proc red4_kill
  red4_close_log
  red4_killw
end proc

{
{ PROC EXIT
proc exit
  inputl 'EXIT - Are you sure ? (Y/N)  > ' (sure)
  if sure
    red4_kill
    #exit
  end if
end proc

{
{ load and initialise the task with no password
print ' '
print '      Welcome to CGS4DR Reduction'
print ' '

red4_load
red4_init
red4_set_format (red4_format)
