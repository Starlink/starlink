{+
{
{ cred4.icl
{
{ author: Phil Daly
{ date:   08-Dec-1994
{
{-

{
{ Do not save procedures or check parameters
set nosave
set nocheckpars

{
{ Get some semi-global variables
cred4_alias    = getenv("PID") & 'cred4'
cred4_nb_alias = 'c' & getenv("PID") & '_ctrlnb'
cred4_df       = getenv("CGS4_CONFIG")&'/default.cred4'

{
{ Defstring some commands
defstring cred4_loadw loadw $CGS4_EXE/cred4 (cred4_alias)
defstring cred4_killw killw (cred4_alias)
defstring cred4_set_nb send (cred4_alias) set noticeboard (cred4_nb_alias)
defstring cred4_close_nb obeyw (cred4_alias) close_nb
defstring cred4_close_qfile obeyw (cred4_alias) close_qfile
defstring cred4_init obeyw (cred4_alias) init
defstring cred4_list_nb obeyw (cred4_alias) list_nb
defstring cred4_open_nb obeyw (cred4_alias) open_nb
defstring cred4_open_qfile obeyw (cred4_alias) open_qfile
defstring cred4_reduce send  (cred4_alias) obey reduce
defstring cancel_reduce send  (cred4_alias) cancel reduce
defstring cred4_reset obeyw (cred4_alias) reset
defstring cred4_restore obeyw (cred4_alias) restore_config
defstring cred4_save obeyw (cred4_alias) save_config
defstring cred4_set_verbose obeyw (cred4_alias) set_verbose
defstring cred4_status obeyw (cred4_alias) status
defstring cred4_verbose obeyw (cred4_alias) set_verbose true
defstring cred4_noverbose obeyw (cred4_alias) set_verbose false
defstring cred4_saved obeyw (cred4_alias) save_config config_file=(cred4_df)
defstring cred4_restored obeyw (cred4_alias) restore_config config_file=(cred4_df)
defstring cred4_rmfile sh rm (cred4_df)
defstring start_autoreduce send (cred4_alias) obey reduce
defstring stop_autoreduce send (cred4_alias) cancel reduce

{
{ Defstring some PUTNBS/GETNBS commands
defstring drpause    putnbs ((cred4_nb_alias)&'.flags.pause_reduction') true
defstring drcontinue putnbs ((cred4_nb_alias)&'.flags.pause_reduction') false
defstring set_subtract_bias putnbs ((cred4_nb_alias)&'.reduction.subtract_bias.execute')
defstring set_subtract_dark putnbs ((cred4_nb_alias)&'.reduction.subtract_dark.execute')
defstring set_add_int putnbs ((cred4_nb_alias)&'.reduction.add_int.execute')
defstring set_archive_obs putnbs ((cred4_nb_alias)&'.reduction.archive_obs.execute')
defstring set_file_obs putnbs ((cred4_nb_alias)&'.reduction.file_obs.execute')
defstring set_normalise_ff putnbs ((cred4_nb_alias)&'.reduction.normalise_ff.execute')
defstring set_div_by_ff putnbs ((cred4_nb_alias)&'.reduction.divide_by_ff.execute')
defstring set_add_obs putnbs ((cred4_nb_alias)&'.reduction.add_obs.execute')
defstring set_to_wavelength putnbs ((cred4_nb_alias)&'.reduction.to_wavelength.execute')
defstring set_div_by_std putnbs ((cred4_nb_alias)&'.reduction.divide_by_std.execute')
defstring set_ext_spc putnbs ((cred4_nb_alias)&'.reduction.extract_spc.execute')
defstring set_ff_method putnbs ((cred4_nb_alias)&'.reduction.normalise_ff.method')
defstring set_ff_order putnbs ((cred4_nb_alias)&'.reduction.normalise_ff.order')
defstring set_ff_boxsize putnbs ((cred4_nb_alias)&'.reduction.normalise_ff.boxsize')
defstring set_wave_method putnbs ((cred4_nb_alias)&'.reduction.to_wavelength.method')
defstring set_mask putnbs ((cred4_nb_alias)&'.miscellaneous.mask')
defstring set_spc_r1s putnbs ((cred4_nb_alias)&'.reduction.extract_spc.row1s')
defstring set_spc_r1e putnbs ((cred4_nb_alias)&'.reduction.extract_spc.row1e')
defstring set_spc_r2s putnbs ((cred4_nb_alias)&'.reduction.extract_spc.row2s')
defstring set_spc_r2e putnbs ((cred4_nb_alias)&'.reduction.extract_spc.row2e')
defstring set_spc_r3s putnbs ((cred4_nb_alias)&'.reduction.extract_spc.row3s')
defstring set_spc_r3e putnbs ((cred4_nb_alias)&'.reduction.extract_spc.row3e')
defstring set_spc_invert putnbs ((cred4_nb_alias)&'.reduction.extract_spc.invert')
defstring set_spc_algorithm putnbs ((cred4_nb_alias)&'.reduction.extract_spc.algorithm')
defstring set_intp0 putnbs ((cred4_nb_alias)&'.display.int_p0')
defstring set_intp1 putnbs ((cred4_nb_alias)&'.display.int_p1')
defstring set_intp2 putnbs ((cred4_nb_alias)&'.display.int_p2')
defstring set_intp3 putnbs ((cred4_nb_alias)&'.display.int_p3')
defstring set_intp4 putnbs ((cred4_nb_alias)&'.display.int_p4')
defstring set_intp5 putnbs ((cred4_nb_alias)&'.display.int_p5')
defstring set_intp6 putnbs ((cred4_nb_alias)&'.display.int_p6')
defstring set_intp7 putnbs ((cred4_nb_alias)&'.display.int_p7')
defstring set_intp8 putnbs ((cred4_nb_alias)&'.display.int_p8')
defstring set_obsp0 putnbs ((cred4_nb_alias)&'.display.obs_p0')
defstring set_obsp1 putnbs ((cred4_nb_alias)&'.display.obs_p1')
defstring set_obsp2 putnbs ((cred4_nb_alias)&'.display.obs_p2')
defstring set_obsp3 putnbs ((cred4_nb_alias)&'.display.obs_p3')
defstring set_obsp4 putnbs ((cred4_nb_alias)&'.display.obs_p4')
defstring set_obsp5 putnbs ((cred4_nb_alias)&'.display.obs_p5')
defstring set_obsp6 putnbs ((cred4_nb_alias)&'.display.obs_p6')
defstring set_obsp7 putnbs ((cred4_nb_alias)&'.display.obs_p7')
defstring set_obsp8 putnbs ((cred4_nb_alias)&'.display.obs_p8')
defstring set_grpp0 putnbs ((cred4_nb_alias)&'.display.grp_p0')
defstring set_grpp1 putnbs ((cred4_nb_alias)&'.display.grp_p1')
defstring set_grpp2 putnbs ((cred4_nb_alias)&'.display.grp_p2')
defstring set_grpp3 putnbs ((cred4_nb_alias)&'.display.grp_p3')
defstring set_grpp4 putnbs ((cred4_nb_alias)&'.display.grp_p4')
defstring set_grpp5 putnbs ((cred4_nb_alias)&'.display.grp_p5')
defstring set_grpp6 putnbs ((cred4_nb_alias)&'.display.grp_p6')
defstring set_grpp7 putnbs ((cred4_nb_alias)&'.display.grp_p7')
defstring set_grpp8 putnbs ((cred4_nb_alias)&'.display.grp_p8')
defstring set_spcp0 putnbs ((cred4_nb_alias)&'.display.spc_p0')
defstring set_spcp1 putnbs ((cred4_nb_alias)&'.display.spc_p1')
defstring set_spcp2 putnbs ((cred4_nb_alias)&'.display.spc_p2')
defstring set_spcp3 putnbs ((cred4_nb_alias)&'.display.spc_p3')
defstring set_spcp4 putnbs ((cred4_nb_alias)&'.display.spc_p4')
defstring set_spcp5 putnbs ((cred4_nb_alias)&'.display.spc_p5')
defstring set_spcp6 putnbs ((cred4_nb_alias)&'.display.spc_p6')
defstring set_spcp7 putnbs ((cred4_nb_alias)&'.display.spc_p7')
defstring set_spcp8 putnbs ((cred4_nb_alias)&'.display.spc_p8')
defstring set_pftyp putnbs ((cred4_nb_alias)&'.miscellaneous.pf_polyfit')
defstring set_pfwei putnbs ((cred4_nb_alias)&'.miscellaneous.pf_weight')
defstring set_pfdeg putnbs ((cred4_nb_alias)&'.miscellaneous.pf_degree')
defstring set_pfrej putnbs ((cred4_nb_alias)&'.miscellaneous.pf_nreject')
defstring set_pfs1s putnbs ((cred4_nb_alias)&'.miscellaneous.pf_says1')
defstring set_pfs1e putnbs ((cred4_nb_alias)&'.miscellaneous.pf_saye1')
defstring set_pfs2s putnbs ((cred4_nb_alias)&'.miscellaneous.pf_says2')
defstring set_pfs2e putnbs ((cred4_nb_alias)&'.miscellaneous.pf_saye2')
defstring set_pfs3s putnbs ((cred4_nb_alias)&'.miscellaneous.pf_says3')
defstring set_pfs3e putnbs ((cred4_nb_alias)&'.miscellaneous.pf_saye3')
defstring set_pfs4s putnbs ((cred4_nb_alias)&'.miscellaneous.pf_says4')
defstring set_pfs4e putnbs ((cred4_nb_alias)&'.miscellaneous.pf_saye4')
defstring set_bmode putnbs ((cred4_nb_alias)&'.miscellaneous.bias_mode')
defstring set_dmode putnbs ((cred4_nb_alias)&'.miscellaneous.dark_mode')
defstring set_fmode putnbs ((cred4_nb_alias)&'.miscellaneous.flat_mode')
defstring set_cmode putnbs ((cred4_nb_alias)&'.miscellaneous.calib_mode')
defstring set_smode putnbs ((cred4_nb_alias)&'.miscellaneous.standard_mode')
defstring set_specb putnbs ((cred4_nb_alias)&'.miscellaneous.specified_bias')
defstring set_specd putnbs ((cred4_nb_alias)&'.miscellaneous.specified_dark')
defstring set_specf putnbs ((cred4_nb_alias)&'.miscellaneous.specified_flat')
defstring set_specc putnbs ((cred4_nb_alias)&'.miscellaneous.specified_calib')
defstring set_specs putnbs ((cred4_nb_alias)&'.miscellaneous.specified_std')
defstring get_subtract_bias getnbs ((cred4_nb_alias)&'.reduction.subtract_bias.execute')
defstring get_subtract_dark getnbs ((cred4_nb_alias)&'.reduction.subtract_dark.execute')
defstring get_add_int getnbs ((cred4_nb_alias)&'.reduction.add_int.execute')
defstring get_archive_obs getnbs ((cred4_nb_alias)&'.reduction.archive_obs.execute')
defstring get_file_obs getnbs ((cred4_nb_alias)&'.reduction.file_obs.execute')
defstring get_normalise_ff getnbs ((cred4_nb_alias)&'.reduction.normalise_ff.execute')
defstring get_div_by_ff getnbs ((cred4_nb_alias)&'.reduction.divide_by_ff.execute')
defstring get_add_obs getnbs ((cred4_nb_alias)&'.reduction.add_obs.execute')
defstring get_to_wavelength getnbs ((cred4_nb_alias)&'.reduction.to_wavelength.execute')
defstring get_div_by_std getnbs ((cred4_nb_alias)&'.reduction.divide_by_std.execute')
defstring get_ext_spc getnbs ((cred4_nb_alias)&'.reduction.extract_spc.execute')
defstring get_ff_method getnbs ((cred4_nb_alias)&'.reduction.normalise_ff.method')
defstring get_ff_order getnbs ((cred4_nb_alias)&'.reduction.normalise_ff.order')
defstring get_ff_boxsize getnbs ((cred4_nb_alias)&'.reduction.normalise_ff.boxsize')
defstring get_wave_method getnbs ((cred4_nb_alias)&'.reduction.to_wavelength.method')
defstring get_mask getnbs ((cred4_nb_alias)&'.miscellaneous.mask')
defstring get_spc_r1s getnbs ((cred4_nb_alias)&'.reduction.extract_spc.row1s')
defstring get_spc_r1e getnbs ((cred4_nb_alias)&'.reduction.extract_spc.row1e')
defstring get_spc_r2s getnbs ((cred4_nb_alias)&'.reduction.extract_spc.row2s')
defstring get_spc_r2e getnbs ((cred4_nb_alias)&'.reduction.extract_spc.row2e')
defstring get_spc_r3s getnbs ((cred4_nb_alias)&'.reduction.extract_spc.row3s')
defstring get_spc_r3e getnbs ((cred4_nb_alias)&'.reduction.extract_spc.row3e')
defstring get_spc_invert getnbs ((cred4_nb_alias)&'.reduction.extract_spc.invert')
defstring get_spc_algorithm getnbs ((cred4_nb_alias)&'.reduction.extract_spc.algorithm')
defstring get_intp0 getnbs ((cred4_nb_alias)&'.display.int_p0')
defstring get_intp1 getnbs ((cred4_nb_alias)&'.display.int_p1')
defstring get_intp2 getnbs ((cred4_nb_alias)&'.display.int_p2')
defstring get_intp3 getnbs ((cred4_nb_alias)&'.display.int_p3')
defstring get_intp4 getnbs ((cred4_nb_alias)&'.display.int_p4')
defstring get_intp5 getnbs ((cred4_nb_alias)&'.display.int_p5')
defstring get_intp6 getnbs ((cred4_nb_alias)&'.display.int_p6')
defstring get_intp7 getnbs ((cred4_nb_alias)&'.display.int_p7')
defstring get_intp8 getnbs ((cred4_nb_alias)&'.display.int_p8')
defstring get_obsp0 getnbs ((cred4_nb_alias)&'.display.obs_p0')
defstring get_obsp1 getnbs ((cred4_nb_alias)&'.display.obs_p1')
defstring get_obsp2 getnbs ((cred4_nb_alias)&'.display.obs_p2')
defstring get_obsp3 getnbs ((cred4_nb_alias)&'.display.obs_p3')
defstring get_obsp4 getnbs ((cred4_nb_alias)&'.display.obs_p4')
defstring get_obsp5 getnbs ((cred4_nb_alias)&'.display.obs_p5')
defstring get_obsp6 getnbs ((cred4_nb_alias)&'.display.obs_p6')
defstring get_obsp7 getnbs ((cred4_nb_alias)&'.display.obs_p7')
defstring get_obsp8 getnbs ((cred4_nb_alias)&'.display.obs_p8')
defstring get_grpp0 getnbs ((cred4_nb_alias)&'.display.grp_p0')
defstring get_grpp1 getnbs ((cred4_nb_alias)&'.display.grp_p1')
defstring get_grpp2 getnbs ((cred4_nb_alias)&'.display.grp_p2')
defstring get_grpp3 getnbs ((cred4_nb_alias)&'.display.grp_p3')
defstring get_grpp4 getnbs ((cred4_nb_alias)&'.display.grp_p4')
defstring get_grpp5 getnbs ((cred4_nb_alias)&'.display.grp_p5')
defstring get_grpp6 getnbs ((cred4_nb_alias)&'.display.grp_p6')
defstring get_grpp7 getnbs ((cred4_nb_alias)&'.display.grp_p7')
defstring get_grpp8 getnbs ((cred4_nb_alias)&'.display.grp_p8')
defstring get_spcp0 getnbs ((cred4_nb_alias)&'.display.spc_p0')
defstring get_spcp1 getnbs ((cred4_nb_alias)&'.display.spc_p1')
defstring get_spcp2 getnbs ((cred4_nb_alias)&'.display.spc_p2')
defstring get_spcp3 getnbs ((cred4_nb_alias)&'.display.spc_p3')
defstring get_spcp4 getnbs ((cred4_nb_alias)&'.display.spc_p4')
defstring get_spcp5 getnbs ((cred4_nb_alias)&'.display.spc_p5')
defstring get_spcp6 getnbs ((cred4_nb_alias)&'.display.spc_p6')
defstring get_spcp7 getnbs ((cred4_nb_alias)&'.display.spc_p7')
defstring get_spcp8 getnbs ((cred4_nb_alias)&'.display.spc_p8')
defstring get_pftyp getnbs ((cred4_nb_alias)&'.miscellaneous.pf_polyfit')
defstring get_pfwei getnbs ((cred4_nb_alias)&'.miscellaneous.pf_weight')
defstring get_pfdeg getnbs ((cred4_nb_alias)&'.miscellaneous.pf_degree')
defstring get_pfrej getnbs ((cred4_nb_alias)&'.miscellaneous.pf_nreject')
defstring get_pfs1s getnbs ((cred4_nb_alias)&'.miscellaneous.pf_says1')
defstring get_pfs1e getnbs ((cred4_nb_alias)&'.miscellaneous.pf_saye1')
defstring get_pfs2s getnbs ((cred4_nb_alias)&'.miscellaneous.pf_says2')
defstring get_pfs2e getnbs ((cred4_nb_alias)&'.miscellaneous.pf_saye2')
defstring get_pfs3s getnbs ((cred4_nb_alias)&'.miscellaneous.pf_says3')
defstring get_pfs3e getnbs ((cred4_nb_alias)&'.miscellaneous.pf_saye3')
defstring get_pfs4s getnbs ((cred4_nb_alias)&'.miscellaneous.pf_says4')
defstring get_pfs4e getnbs ((cred4_nb_alias)&'.miscellaneous.pf_saye4')
defstring get_bmode getnbs ((cred4_nb_alias)&'.miscellaneous.bias_mode')
defstring get_dmode getnbs ((cred4_nb_alias)&'.miscellaneous.dark_mode')
defstring get_fmode getnbs ((cred4_nb_alias)&'.miscellaneous.flat_mode')
defstring get_cmode getnbs ((cred4_nb_alias)&'.miscellaneous.calib_mode')
defstring get_smode getnbs ((cred4_nb_alias)&'.miscellaneous.standard_mode')
defstring get_specb getnbs ((cred4_nb_alias)&'.miscellaneous.specified_bias')
defstring get_specd getnbs ((cred4_nb_alias)&'.miscellaneous.specified_dark')
defstring get_specf getnbs ((cred4_nb_alias)&'.miscellaneous.specified_flat')
defstring get_specc getnbs ((cred4_nb_alias)&'.miscellaneous.specified_calib')
defstring get_specs getnbs ((cred4_nb_alias)&'.miscellaneous.specified_std')

{
{ Set the task aliases
defstring cred4_set_q4_alias send (cred4_alias) set qman_alias (qman_alias)
defstring cred4_set_q4_pwrd  send (cred4_alias) set qman_pwrd (pwrd)
defstring cred4_set_q4_lwrd  send (cred4_alias) set qman_lwrd (lwrd)
defstring cred4_set_p4_alias send (cred4_alias) set p4_alias (p4_alias)
defstring cred4_set_r4_alias send (cred4_alias) set red4_alias (red4_alias)

{
{ PROC SET_REDUCTION_SEQUENCE
proc set_reduction_sequence p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11
  if UNDEFINED(p1)
    input 'Subtract BIAS observation?  > ' (subtract_bias)
  else
    subtract_bias = p1
  endif
  if UNDEFINED(p2)
    input 'Subtract DARK observation>  > ' (subtract_dark)
  else
    subtract_dark = p2
  endif
  if UNDEFINED(p3)
    input 'Co-add each observation?  > ' (add_int)
  else
    add_int = p3
  endif
  if UNDEFINED(p4)
    input 'Archive each observation?  > ' (archive_obs)
  else
    archive_obs = p4
  endif
  if UNDEFINED(p5)
    input 'File each observation?  > ' (file_obs)
  else
    file_obs = p5
  endif
  if UNDEFINED(p6)
    input 'Normalise FLAT field?  > ' (normalise_ff)
  else
    normalise_ff = p6
  endif
  if UNDEFINED(p7)
    input 'Divide by FLAT field?  > ' (div_by_ff)
  else
    div_by_ff = p7
  endif
  if UNDEFINED(p8)
    input 'Wavelength calibrate?  > ' (to_wavelength)
  else
    to_wavelength = p8
  endif
  if UNDEFINED(p9)
    input 'Add observations into groups?  > ' (add_obs)
  else
    add_obs = p9
  endif
  if UNDEFINED(p10)
    input 'Divide by a STANDARD source?  > ' (div_by_std)
  else
    div_by_std = p10
  endif
  if UNDEFINED(p11)
    input 'Extract nodded spectrum?  > ' (extract_spc)
  else
    extract_spc = p11
  endif
  {
  { Set them
  subtract_bias = UPCASE(subtract_bias)
  set_subtract_bias (subtract_bias)
  subtract_dark = UPCASE(subtract_dark)
  set_subtract_dark (subtract_dark)
  add_int = UPCASE(add_int)
  set_add_int (add_int)
  archive_obs = UPCASE(archive_obs)
  set_archive_obs (archive_obs)
  file_obs = UPCASE(file_obs)
  set_file_obs (file_obs)
  normalise_ff = UPCASE(normalise_ff)
  set_normalise_ff (normalise_ff)
  div_by_ff = UPCASE(div_by_ff)
  set_div_by_ff (div_by_ff)
  add_obs = UPCASE(add_obs)
  set_add_obs (add_obs)
  to_wavelength = UPCASE(to_wavelength)
  set_to_wavelength (to_wavelength)
  div_by_std = UPCASE(div_by_std)
  set_div_by_std (div_by_std)
  extract_spc = UPCASE(extract_spc)
  set_ext_spc (extract_spc)
endproc

{
{ PROC GET_REDUCTION_SEQUENCE
proc get_reduction_sequence
  {
  { Get required items
  subtract_bias = ' '
  get_subtract_bias (subtract_bias)
  print 'Subtract BIAS frame          = ' (subtract_bias)
  subtract_dark = ' '
  get_subtract_dark (subtract_dark)
  print 'Subtract DARK frame          = ' (subtract_dark)
  add_int = ' '
  get_add_int (add_int)
  print 'Co-add each integration      = ' (add_int)
  archive_obs = ' '
  get_archive_obs (archive_obs)
  print 'Archive each observation     = ' (archive_obs)
  file_obs = ' '
  get_file_obs (file_obs)
  print 'File each observation        = ' (file_obs)
  normalise_ff = ' '
  get_normalise_ff (normalise_ff)
  print 'Normalise each FLAT field    = ' (normalise_ff)
  div_by_ff = ' '
  get_div_by_ff (div_by_ff)
  print 'Divide by FLAT field         = ' (div_by_ff)
  add_obs = ' '
  get_add_obs (add_obs)
  print 'Add observations into groups = ' (add_obs)
  to_wavelength = ' '
  get_to_wavelength (to_wavelength)
  print 'Wavelength calibrate         = ' (to_wavelength)
  div_by_std = ' '
  get_div_by_std (div_by_std)
  print 'Divide by STANDARD source    = ' (div_by_std)
  extract_spc = ' '
  get_ext_spc (extract_spc)
  print 'Extract (nodded) spectrum    = ' (extract_spc)
endproc

{
{ PROC SET_FF_NORMALISATION
proc set_ff_normalisation p1 p2 p3
  if UNDEFINED(p1)
    input 'Method to use for normalising FLAT fields?  > ' (ff_method)
  else
    ff_method = p1
  endif
  if UNDEFINED(p2)
    inputi 'Order of polynomial to fit?  > ' (ff_order)
  else
    ff_order = INTEGER(p2)
  endif
  if UNDEFINED(p3)
    inputi 'Size of smoothing box?  > ' (ff_boxsize)
  else
    ff_boxsize = INTEGER(p3)
  endif
  ff_method = UPCASE(ff_method)
  set_ff_method (ff_method)
  set_ff_order (ff_order)
  set_ff_boxsize (ff_boxsize)
endproc

{
{ PROC GET_FF_NORMALISATION
proc get_ff_normalisation
  ff_method = ' '
  get_ff_method (ff_method)
  print 'Method set to normalise FLAT fields = ' (ff_method)
  ff_order = ' '
  get_ff_order (ff_order)
  print 'Order of polynomial = ' (ff_order)
  ff_boxsize = ' '
  get_ff_boxsize (ff_boxsize)
  print 'Smoothing box size = ' (ff_boxsize)
endproc

{
{ PROC SET_WAVELENGTH_CALIB
proc set_wavelength_calib p1
  if UNDEFINED(p1)
    input 'Wavelength calibration method?  > ' (wc_method)
  else
    wc_method = p1
  endif
  wc_method = UPCASE(wc_method)
  set_wave_method (wc_method)
endproc

{
{ PROC GET_WAVELENGTH_CALIB
proc get_wavelength_calib
  wc_method = ' '
  get_wave_method (wc_method)
  print 'Wavelength calibration method = ' (wc_method)
endproc

{ PROC SET_BAD_PIXEL_MASK
proc set_bad_pixel_mask p1
  if UNDEFINED(p1)
    input 'Bad pixel mask?  > ' (bpm)
  else
    bpm = p1
  endif
  set_mask (bpm)
endproc

{
{ PROC GET_BAD_PIXEL_MASK
proc get_bad_pixel_mask
  bpm = ' '
  get_mask (bpm)
  print 'Bad pixel mask = ' (bpm)
endproc

{
{ PROC SET_EXTRACT_SPECTRUM
proc set_extract_spectrum p1 p2 p3 p4 p5 p6 p7 p8
  if UNDEFINED(p1)
    inputr 'Start of first row to be extracted?  > ' (r1s)
  else
    r1s = REAL(p1)
  endif
  if UNDEFINED(p2)
    inputr 'End of first row to be extracted?  > ' (r1e)
  else
    r1e = REAL(p2)
  endif
  if UNDEFINED(p3)
    inputr 'Start of second row to be extracted?  > ' (r2s)
  else
    r2s = REAL(p3)
  endif
  if UNDEFINED(p4)
    inputr 'End of second row to be extracted?  > ' (r2e)
  else
    r2e = REAL(p4)
  endif
  if UNDEFINED(p5)
    inputr 'Start of third row to be extracted?  > ' (r3s)
  else
    r3s = REAL(p5)
  endif
  if UNDEFINED(p6)
    inputr 'End of third row to be extracted?  > ' (r3e)
  else
    r3e = REAL(p6)
  endif
  if UNDEFINED(p7)
    inputl 'Invert the output spectrum?  > ' (invert)
  else
    invert = LOGICAL(p7)
  endif
  if UNDEFINED(p8)
    input 'Algorithm to use for extraction?  > ' (algorithm)
  else
    algorithm = p8
  endif
  algorithm = UPCASE(algorithm)
  set_spc_r1s (r1s)
  set_spc_r1e (r1e)
  set_spc_r2s (r2s)
  set_spc_r2e (r2e)
  set_spc_r3s (r3s)
  set_spc_r3e (r3e)
  set_spc_invert (invert)
  set_spc_algorithm (algorithm)
endproc

{
{ PROC GET_EXTRACT_SPECTRUM
proc get_extract_spectrum
  r1s = ' '
  get_spc_r1s (r1s)
  print 'Start of first row to extract = ' (r1s)
  r1e = ' '
  get_spc_r1e (r1e)
  print 'End of first row to extract = ' (r1e)
  r2s = ' '
  get_spc_r2s (r2s)
  print 'Start of second row to extract = ' (r2s)
  r2e = ' '
  get_spc_r2e (r2e)
  print 'End of second row to extract = ' (r2e)
  r3s = ' '
  get_spc_r3s (r3s)
  print 'Start of third row to extract = ' (r3s)
  r3e = ' '
  get_spc_r3e (r3e)
  print 'End of third row to extract = ' (r3e)
  invert = ' '
  get_spc_invert (invert)
  print 'Invert spectrum upon output = ' (invert)
  algorithm = ' '
  get_spc_algorithm (algorithm)
  print 'Extraction algorithm = ' (algorithm)
endproc

{
{ PROC SET_FILE_SELECTION
proc set_file_selection p1 p2 p3 p4 p5 p6 p7 p8 p9 p10
  if UNDEFINED(p1)
    input 'Direction of search for BIAS frames?  > ' (bmode)
  else
    bmode = (p1)
  endif
  if UNDEFINED(p2)
    input 'Direction of search for DARK frames?  > ' (dmode)
  else
    dmode = (p2)
  endif
  if UNDEFINED(p3)
    input 'Direction of search for FLAT frames?  > ' (fmode)
  else
    fmode = (p3)
  endif
  if UNDEFINED(p4)
    input 'Direction of search for CALIB frames?  > ' (cmode)
  else
    cmode = (p4)
  endif
  if UNDEFINED(p5)
    input 'Direction of search for STANDARD frames?  > ' (smode)
  else
    smode = (p5)
  endif

  ubmode = UPCASE(bmode)
  set_bmode (ubmode)
  udmode = UPCASE(dmode)
  set_dmode (udmode)
  ufmode = UPCASE(fmode)
  set_fmode (ufmode)
  ucmode = UPCASE(cmode)
  set_cmode (ucmode)
  usmode = UPCASE(smode)
  set_smode (usmode)

  if UNDEFINED(p6) OR ubmode = 'SPECIFIED'
    input 'Specified BIAS frame?  > ' (specb)
  else
    specb = (p6)
  endif
  if specb = ''
    specb = 'ROyymmdd_oooo'
  endif
  if UNDEFINED(p7) OR udmode = 'SPECIFIED'
    input 'Specified DARK frame?  > ' (specd)
  else
    specd = (p7)
  endif
  if specd = ''
    specd = 'ROyymmdd_oooo'
  endif
  ufmode = UPCASE(fmode)
  if UNDEFINED(p8) OR ufmode = 'SPECIFIED'
    input 'Specified FLAT frame?  > ' (specf)
  else
    specf = (p8)
  endif
  if specf = ''
    specf = 'ROyymmdd_oooo'
  endif
  ucmode = UPCASE(cmode)
  if UNDEFINED(p9) OR ucmode = 'SPECIFIED'
    input 'Specified CALIB frame?  > ' (specc)
  else
    specc = (p9)
  endif
  if specc = ''
    specc = 'CAyymmdd_oooo'
  endif
  usmode = UPCASE(smode)
  if UNDEFINED(p10) OR usmode = 'SPECIFIED'
    input 'Specified STANDARD frame?  > ' (specs)
  else
    specs = (p10)
  endif
  if specs = ''
    specs = 'STyymmdd_oooo'
  endif

  set_specb (specb)
  set_specd (specd)
  set_specf (specf)
  set_specc (specc)
  set_specs (specs)
endproc

{
{ PROC GET_FILE_SELECTION
proc get_file_selection
  fval = ' '
  get_bmode (fval)
  print 'BIAS frame selection mode = ' (fval)
  fval = ' '
  get_dmode (fval)
  print 'DARK frame selection mode = ' (fval)
  fval = ' '
  get_fmode (fval)
  print 'FLAT frame selection mode = ' (fval)
  fval = ' '
  get_cmode (fval)
  print 'CALIB frame selection mode = ' (fval)
  fval = ' '
  get_smode (fval)
  print 'STANDARD frame selection mode = ' (fval)
  fval = ' '
  get_specb (fval)
  print 'Specified BIAS frame = ' (fval)
  fval = ' '
  get_specd (fval)
  print 'Specified DARK frame = ' (fval)
  fval = ' '
  get_specf (fval)
  print 'Specified FLAT frame = ' (fval)
  fval = ' '
  get_specc (fval)
  print 'Specified CALIB frame = ' (fval)
  fval = ' '
  get_specs (fval)
  print 'Specified STANDARD frame = ' (fval)
endproc

{
{ PROC SET_POLYFIT
proc set_polyfit p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12
  if UNDEFINED(p1)
    input 'Type of polyfit to be performed?  > ' (pftyp)
  else
    pftyp = (p1)
  endif
  if UNDEFINED(p2)
    inputi 'Degree of polynomial to be fitted?  > ' (pfdeg)
  else
    pfdeg = INTEGER(p2)
  endif
  if UNDEFINED(p3)
    inputi 'Number of points to reject?  > ' (pfrej)
  else
    pfrej = INTEGER(p3)
  endif
  if UNDEFINED(p4)
    inputl 'Weight the fit?  > ' (pfwei)
  else
    pfwei = LOGICAL(p4)
  endif
  if UNDEFINED(p5)
    inputi 'Start of first sky area?  > ' (pfs1s)
  else
    pfs1s = INTEGER(p5)
  endif
  if UNDEFINED(p6)
    inputi 'End of first sky area?  > ' (pfs1e)
  else
    pfs1e = INTEGER(p6)
  endif
  if UNDEFINED(p7)
    inputi 'Start of second sky area?  > ' (pfs2s)
  else
    pfs2s = INTEGER(p7)
  endif
  if UNDEFINED(p8)
    inputi 'End of second sky area?  > ' (pfs2e)
  else
    pfs2e = INTEGER(p8)
  endif
  if UNDEFINED(p9)
    inputi 'Start of third sky area?  > ' (pfs3s)
  else
    pfs3s = INTEGER(p9)
  endif
  if UNDEFINED(p10)
    inputi 'End of third sky area?  > ' (pfs3e)
  else
    pfs3e = INTEGER(p10)
  endif
  if UNDEFINED(p11)
    inputi 'Start of fourth sky area?  > ' (pfs4s)
  else
    pfs4s = INTEGER(p11)
  endif
  if UNDEFINED(p12)
    inputi 'End of fourth sky area?  > ' (pfs4e)
  else
    pfs4e = INTEGER(p12)
  endif

  set_pftyp '           '
  pftyp1 = UPCASE(pftyp)
  set_pftyp (pftyp1)
  set_pfdeg (pfdeg)
  set_pfrej (pfrej)
  set_pfwei (pfwei)
  set_pfs1s (pfs1s)
  set_pfs1e (pfs1e)
  set_pfs2s (pfs2s)
  set_pfs2e (pfs2e)
  set_pfs3s (pfs3s)
  set_pfs3e (pfs3e)
  set_pfs4s (pfs4s)
  set_pfs4e (pfs4e)
endproc

{
{ PROC GET_POLYFIT
proc get_polyfit
  pfvar = ' '
  get_pftyp (pfvar)
  print 'Polyfitting enabled for ' (pfvar)
  pfvar = ' '
  get_pfdeg (pfvar)
  print 'Degree of polynomial = ' (pfvar)
  pfvar = ' '
  get_pfrej (pfvar)
  print 'Number of reject points = ' (pfvar)
  pfvar = ' '
  get_pfwei (pfvar)
  print 'Weighting enabled = ' (pfvar)
  pfvar = ' '
  get_pfs1s (pfvar)
  print 'Start of first sky area = ' (pfvar)
  pfvar = ' '
  get_pfs1e (pfvar)
  print 'End of first sky area = ' (pfvar)
  pfvar = ' '
  get_pfs2s (pfvar)
  print 'Start of second sky area = ' (pfvar)
  pfvar = ' '
  get_pfs2e (pfvar)
  print 'End of second sky area = ' (pfvar)
  pfvar = ' '
  get_pfs3s (pfvar)
  print 'Start of third sky area = ' (pfvar)
  pfvar = ' '
  get_pfs3e (pfvar)
  print 'End of third sky area = ' (pfvar)
  pfvar = ' '
  get_pfs4s (pfvar)
  print 'Start of fourth sky area = ' (pfvar)
  pfvar = ' '
  get_pfs4e (pfvar)
  print 'End of fourth sky area = ' (pfvar)
endproc

{
{ PROC SAVE_CONFIG
proc save_config p1
  if UNDEFINED(p1)
    input 'Configuration filename only?  > ' (file)
  else
    file = p1
  endif
  cred4_file = getenv("CGS4_CONFIG") & '/' & (file) & '.cred4'
  print 'Saving DR config to' (cred4_file)
  if FILE_EXISTS( cred4_file )
    sh rm (cred4_file)
  endif
  cred4_save config_file=(cred4_file)
  p4_file = getenv("CGS4_CONFIG") & '/' & (file) & '.p4'
  print 'Saving display config to' (p4_file)
  if FILE_EXISTS( p4_file)
    sh rm (p4_file)
  endif
  p4_save file=(p4_file) port=-1
endproc

{
{ PROC RESTORE_CONFIG
proc restore_config p1
  if UNDEFINED(p1)
    input 'Configuration filename only?  > ' (file)
  else
    file = p1
  endif
  cred4_file = getenv("CGS4_CONFIG") & '/' & (file) & '.cred4'
  print 'Restoring DR config from' (cred4_file)
  if FILE_EXISTS(cred4_file)
    cred4_restore config_file=(cred4_file)
  else
    print 'File' (cred4_file) 'does not exist!'
  endif
  p4_file = getenv("CGS4_CONFIG") & '/' & (file) & '.p4'
  print 'Restoring display config from' (p4_file)
  if FILE_EXISTS(p4_file)
    p4_restore file=(p4_file) port=-1
  else
    print 'File' (p4_file) 'does not exist!'
  endif
endproc

{
{ PROC SET_INTEGRATION_DISPLAY
proc set_integration_display p0 p1 p2 p3 p4 p5 p6 p7 p8
  {
  { Get options
  if UNDEFINED(p0)
    input 'Display in port 0?  > ' (int_p0)
  else
    int_p0 = p0
  endif
  if UNDEFINED(p1)
    input 'Display in port 1?  > ' (int_p1)
  else
    int_p1 = p1
  endif
  if UNDEFINED(p2)
    input 'Display in port 2?  > ' (int_p2)
  else
    int_p2 = p2
  endif
  if UNDEFINED(p3)
    input 'Display in port 3?  > ' (int_p3)
  else
    int_p3 = p3
  endif
  if UNDEFINED(p4)
    input 'Display in port 4?  > ' (int_p4)
  else
    int_p4 = p4
  endif
  if UNDEFINED(p5)
    input 'Display in port 5?  > ' (int_p5)
  else
    int_p5 = p5
  endif
  if UNDEFINED(p6)
    input 'Display in port 6?  > ' (int_p6)
  else
    int_p6 = p6
  endif
  if UNDEFINED(p7)
    input 'Display in port 7?  > ' (int_p7)
  else
    int_p7 = p7
  endif
  if UNDEFINED(p8)
    input 'Display in port 8?  > ' (int_p8)
  else
    int_p8 = p8
  endif
  {
  { Set them
  int_p0 = UPCASE(int_p0)
  set_intp0 (int_p0)
  int_p1 = UPCASE(int_p1)
  set_intp1 (int_p1)
  int_p2 = UPCASE(int_p2)
  set_intp2 (int_p2)
  int_p3 = UPCASE(int_p3)
  set_intp3 (int_p3)
  int_p4 = UPCASE(int_p4)
  set_intp4 (int_p4)
  int_p5 = UPCASE(int_p5)
  set_intp5 (int_p5)
  int_p6 = UPCASE(int_p6)
  set_intp6 (int_p6)
  int_p7 = UPCASE(int_p7)
  set_intp7 (int_p7)
  int_p8 = UPCASE(int_p8)
  set_intp8 (int_p8)
endproc

{
{ PROC GET_INTEGRATON_DISPLAY
proc get_integration_display
  intp0 = ' '
  get_intp0 (intp0)
  print 'Display reduced integration in port 0 = ' (intp0)
  intp1 = ' '
  get_intp1 (intp1)
  print 'Display reduced integration in port 1 = ' (intp1)
  intp2 = ' '
  get_intp2 (intp2)
  print 'Display reduced integration in port 2 = ' (intp2)
  intp3 = ' '
  get_intp3 (intp3)
  print 'Display reduced integration in port 3 = ' (intp3)
  intp4 = ' '
  get_intp4 (intp4)
  print 'Display reduced integration in port 4 = ' (intp4)
  intp5 = ' '
  get_intp5 (intp5)
  print 'Display reduced integration in port 5 = ' (intp5)
  intp6 = ' '
  get_intp6 (intp6)
  print 'Display reduced integration in port 6 = ' (intp6)
  intp7 = ' '
  get_intp7 (intp7)
  print 'Display reduced integration in port 7 = ' (intp7)
  intp8 = ' '
  get_intp8 (intp8)
  print 'Display reduced integration in port 8 = ' (intp8)
endproc

{
{ PROC SET_OBSERVATION_DISPLAY
proc set_observation_display p0 p1 p2 p3 p4 p5 p6 p7 p8
  {
  { Get options
  if UNDEFINED(p0)
    input 'Display in port 0?  > ' (obs_p0)
  else
    obs_p0 = p0
  endif
  if UNDEFINED(p1)
    input 'Display in port 1?  > ' (obs_p1)
  else
    obs_p1 = p1
  endif
  if UNDEFINED(p2)
    input 'Display in port 2?  > ' (obs_p2)
  else
    obs_p2 = p2
  endif
  if UNDEFINED(p3)
    input 'Display in port 3?  > ' (obs_p3)
  else
    obs_p3 = p3
  endif
  if UNDEFINED(p4)
    input 'Display in port 4?  > ' (obs_p4)
  else
    obs_p4 = p4
  endif
  if UNDEFINED(p5)
    input 'Display in port 5?  > ' (obs_p5)
  else
    obs_p5 = p5
  endif
  if UNDEFINED(p6)
    input 'Display in port 6?  > ' (obs_p6)
  else
    obs_p6 = p6
  endif
  if UNDEFINED(p7)
    input 'Display in port 7?  > ' (obs_p7)
  else
    obs_p7 = p7
  endif
  if UNDEFINED(p8)
    input 'Display in port 8?  > ' (obs_p8)
  else
    obs_p8 = p8
  endif
  {
  { Set them
  obs_p0 = UPCASE(obs_p0)
  set_obsp0 (obs_p0)
  obs_p1 = UPCASE(obs_p1)
  set_obsp1 (obs_p1)
  obs_p2 = UPCASE(obs_p2)
  set_obsp2 (obs_p2)
  obs_p3 = UPCASE(obs_p3)
  set_obsp3 (obs_p3)
  obs_p4 = UPCASE(obs_p4)
  set_obsp4 (obs_p4)
  obs_p5 = UPCASE(obs_p5)
  set_obsp5 (obs_p5)
  obs_p6 = UPCASE(obs_p6)
  set_obsp6 (obs_p6)
  obs_p7 = UPCASE(obs_p7)
  set_obsp7 (obs_p7)
  obs_p8 = UPCASE(obs_p8)
  set_obsp8 (obs_p8)
endproc

{
{ PROC GET_OBSERVATION_DISPLAY
proc get_observation_display
  obsp0 = ' '
  get_obsp0 (obsp0)
  print 'Display reduced observation in port 0 = ' (obsp0)
  obsp1 = ' '
  get_obsp1 (obsp1)
  print 'Display reduced observation in port 1 = ' (obsp1)
  obsp2 = ' '
  get_obsp2 (obsp2)
  print 'Display reduced observation in port 2 = ' (obsp2)
  obsp3 = ' '
  get_obsp3 (obsp3)
  print 'Display reduced observation in port 3 = ' (obsp3)
  obsp4 = ' '
  get_obsp4 (obsp4)
  print 'Display reduced observation in port 4 = ' (obsp4)
  obsp5 = ' '
  get_obsp5 (obsp5)
  print 'Display reduced observation in port 5 = ' (obsp5)
  obsp6 = ' '
  get_obsp6 (obsp6)
  print 'Display reduced observation in port 6 = ' (obsp6)
  obsp7 = ' '
  get_obsp7 (obsp7)
  print 'Display reduced observation in port 7 = ' (obsp7)
  obsp8 = ' '
  get_obsp8 (obsp8)
  print 'Display reduced observation in port 8 = ' (obsp8)
endproc

{
{ PROC SET_GROUP_DISPLAY
proc set_group_display p0 p1 p2 p3 p4 p5 p6 p7 p8
  {
  { Get options
  if UNDEFINED(p0)
    input 'Display in port 0?  > ' (grp_p0)
  else
    grp_p0 = p0
  endif
  if UNDEFINED(p1)
    input 'Display in port 1?  > ' (grp_p1)
  else
    grp_p1 = p1
  endif
  if UNDEFINED(p2)
    input 'Display in port 2?  > ' (grp_p2)
  else
    grp_p2 = p2
  endif
  if UNDEFINED(p3)
    input 'Display in port 3?  > ' (grp_p3)
  else
    grp_p3 = p3
  endif
  if UNDEFINED(p4)
    input 'Display in port 4?  > ' (grp_p4)
  else
    grp_p4 = p4
  endif
  if UNDEFINED(p5)
    input 'Display in port 5?  > ' (grp_p5)
  else
    grp_p5 = p5
  endif
  if UNDEFINED(p6)
    input 'Display in port 6?  > ' (grp_p6)
  else
    grp_p6 = p6
  endif
  if UNDEFINED(p7)
    input 'Display in port 7?  > ' (grp_p7)
  else
    grp_p7 = p7
  endif
  if UNDEFINED(p8)
    input 'Display in port 8?  > ' (grp_p8)
  else
    grp_p8 = p8
  endif
  {
  { Set them
  grp_p0 = UPCASE(grp_p0)
  set_grpp0 (grp_p0)
  grp_p1 = UPCASE(grp_p1)
  set_grpp1 (grp_p1)
  grp_p2 = UPCASE(grp_p2)
  set_grpp2 (grp_p2)
  grp_p3 = UPCASE(grp_p3)
  set_grpp3 (grp_p3)
  grp_p4 = UPCASE(grp_p4)
  set_grpp4 (grp_p4)
  grp_p5 = UPCASE(grp_p5)
  set_grpp5 (grp_p5)
  grp_p6 = UPCASE(grp_p6)
  set_grpp6 (grp_p6)
  grp_p7 = UPCASE(grp_p7)
  set_grpp7 (grp_p7)
  grp_p8 = UPCASE(grp_p8)
  set_grpp8 (grp_p8)
endproc

{
{ PROC GET_GROUP_DISPLAY
proc get_group_display
  grpp0 = ' '
  get_grpp0 (grpp0)
  print 'Display reduced group in port 0 = ' (grpp0)
  grpp1 = ' '
  get_grpp1 (grpp1)
  print 'Display reduced group in port 1 = ' (grpp1)
  grpp2 = ' '
  get_grpp2 (grpp2)
  print 'Display reduced group in port 2 = ' (grpp2)
  grpp3 = ' '
  get_grpp3 (grpp3)
  print 'Display reduced group in port 3 = ' (grpp3)
  grpp4 = ' '
  get_grpp4 (grpp4)
  print 'Display reduced group in port 4 = ' (grpp4)
  grpp5 = ' '
  get_grpp5 (grpp5)
  print 'Display reduced group in port 5 = ' (grpp5)
  grpp6 = ' '
  get_grpp6 (grpp6)
  print 'Display reduced group in port 6 = ' (grpp6)
  grpp7 = ' '
  get_grpp7 (grpp7)
  print 'Display reduced group in port 7 = ' (grpp7)
  grpp8 = ' '
  get_grpp8 (grpp8)
  print 'Display reduced group in port 8 = ' (grpp8)
endproc

{
{ PROC SET_SPECTRUM_DISPLAY
proc set_spectrum_display p0 p1 p2 p3 p4 p5 p6 p7 p8
  {
  { Get options
  if UNDEFINED(p0)
    input 'Display in port 0?  > ' (spc_p0)
  else
    spc_p0 = p0
  endif
  if UNDEFINED(p1)
    input 'Display in port 1?  > ' (spc_p1)
  else
    spc_p1 = p1
  endif
  if UNDEFINED(p2)
    input 'Display in port 2?  > ' (spc_p2)
  else
    spc_p2 = p2
  endif
  if UNDEFINED(p3)
    input 'Display in port 3?  > ' (spc_p3)
  else
    spc_p3 = p3
  endif
  if UNDEFINED(p4)
    input 'Display in port 4?  > ' (spc_p4)
  else
    spc_p4 = p4
  endif
  if UNDEFINED(p5)
    input 'Display in port 5?  > ' (spc_p5)
  else
    spc_p5 = p5
  endif
  if UNDEFINED(p6)
    input 'Display in port 6?  > ' (spc_p6)
  else
    spc_p6 = p6
  endif
  if UNDEFINED(p7)
    input 'Display in port 7?  > ' (spc_p7)
  else
    spc_p7 = p7
  endif
  if UNDEFINED(p8)
    input 'Display in port 8?  > ' (spc_p8)
  else
    spc_p8 = p8
  endif
  {
  { Set them
  spc_p0 = UPCASE(spc_p0)
  set_spcp0 (spc_p0)
  spc_p1 = UPCASE(spc_p1)
  set_spcp1 (spc_p1)
  spc_p2 = UPCASE(spc_p2)
  set_spcp2 (spc_p2)
  spc_p3 = UPCASE(spc_p3)
  set_spcp3 (spc_p3)
  spc_p4 = UPCASE(spc_p4)
  set_spcp4 (spc_p4)
  spc_p5 = UPCASE(spc_p5)
  set_spcp5 (spc_p5)
  spc_p6 = UPCASE(spc_p6)
  set_spcp6 (spc_p6)
  spc_p7 = UPCASE(spc_p7)
  set_spcp7 (spc_p7)
  spc_p8 = UPCASE(spc_p8)
  set_spcp8 (spc_p8)
endproc

{
{ PROC GET_SPECTRUM_DISPLAY
proc get_spectrum_display
  spcp0 = ' '
  get_spcp0 (spcp0)
  print 'Display reduced spectra in port 0 = ' (spcp0)
  spcp1 = ' '
  get_spcp1 (spcp1)
  print 'Display reduced spectra in port 1 = ' (spcp1)
  spcp2 = ' '
  get_spcp2 (spcp2)
  print 'Display reduced spectra in port 2 = ' (spcp2)
  spcp3 = ' '
  get_spcp3 (spcp3)
  print 'Display reduced spectra in port 3 = ' (spcp3)
  spcp4 = ' '
  get_spcp4 (spcp4)
  print 'Display reduced spectra in port 4 = ' (spcp4)
  spcp5 = ' '
  get_spcp5 (spcp5)
  print 'Display reduced spectra in port 5 = ' (spcp5)
  spcp6 = ' '
  get_spcp6 (spcp6)
  print 'Display reduced spectra in port 6 = ' (spcp6)
  spcp7 = ' '
  get_spcp7 (spcp7)
  print 'Display reduced spectra in port 7 = ' (spcp7)
  spcp8 = ' '
  get_spcp8 (spcp8)
  print 'Display reduced spectra in port 8 = ' (spcp8)
endproc

{
{ PROC EXIT
proc exit
  inputl 'EXIT - Are you sure? (Y/N)  > ' (sure)
  if sure
    cred4_kill
    #exit
  endif
endproc

{
{ PROC CRED4_LOAD
proc cred4_load
  cred4_loadw
  cred4_init
  cred4_status
endproc

{
{ PROC CRED4_KILL
proc cred4_kill
  cred4_close_qfile
  cred4_close_nb
  cred4_killw
endproc

{
{ Invoke it
print ' '
print '      Welcome to CGS4DR Mission Control'
print ' '

cred4_load
cred4_set_nb
cred4_set_q4_alias
cred4_set_q4_pwrd
cred4_set_q4_lwrd
cred4_set_p4_alias
cred4_set_r4_alias
cred4_open_nb
cred4_open_qfile
