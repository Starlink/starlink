{+
{
{ qman.icl
{
{ author: Phil Daly
{ date:   05-Dec-1994
{
{-

{
{ Do not save procedures
set nosave
set nocheckpars

{
{ Get some semi-global variables
qman_alias = getenv("PID") & 'qman'
qman_date  = getenv("QMAN_DATE")
pwrd       = getenv("QMAN_PASS")
lwrd       = getenv("QMAN_LOCK")
home       = getenv("HOME")
qman_df    = getenv("HOME") & '/cgs4dr_configs/default.qman'

{
{ load and initialise the task with no password
print ' '
print '      Welcome to Qman'
print ' '

{
{ Defstring some basic commands
defstring qman_init      obeyw (qman_alias) init password=(pwrd) lockword=(lwrd)
defstring qman_delete    obeyw (qman_alias) delete password=(pwrd) lockword=(lwrd)
defstring qman_read      obeyw (qman_alias) read password=(pwrd) lockword=(lwrd)
defstring qman_lock      obeyw (qman_alias) lock password=(pwrd) lockword=(lwrd)
defstring qman_unlock    obeyw (qman_alias) unlock password=(pwrd) lockword=(lwrd)
defstring qman_order     obeyw (qman_alias) order password=(pwrd) lockword=(lwrd)
defstring qman_restore   obeyw (qman_alias) restore password=(pwrd) lockword=(lwrd)
defstring qman_save      obeyw (qman_alias) save password=(pwrd) lockword=(lwrd)
defstring qman_sort      obeyw (qman_alias) sort password=(pwrd) lockword=(lwrd)
defstring qman_write     obeyw (qman_alias) write password=(pwrd) lockword=(lwrd)
defstring qman_list      obeyw (qman_alias) list password=(pwrd) lockword=(lwrd)
defstring qman_reset     obeyw (qman_alias) reset password=(pwrd) lockword=(lwrd)
defstring qman_status    obeyw (qman_alias) status password=(pwrd) lockword=(lwrd)
defstring qman_verbose   obeyw (qman_alias) verbose password=(pwrd) lockword=(lwrd)
defstring qman_noverbose obeyw (qman_alias) noverbose password=(pwrd) lockword=(lwrd)
defstring qman_saved     obeyw (qman_alias) save password=(pwrd) lockword=(lwrd) file=(qman_df)
defstring qman_restored  obeyw (qman_alias) restore password=(pwrd) lockword=(lwrd) file=(qman_df)

{
{ Defstring some miscellaneous commands
defstring qman_loadw    loadw $QMAN_EXE/qman (qman_alias)
defstring qman_killw    killw (qman_alias)
defstring qman_set_file send  (qman_alias) set file (qman_df)
defstring qman_rmfile   sh rm (qman_df)

{
{ PROC QMAN_LOAD
proc qman_load
  qman_loadw
  qman_init
  qman_restored
  qman_rmfile
  qman_status
end proc

{
{ PROC QMAN_KILL
proc qman_kill
  qman_saved save_mode='new'
  qman_killw
end proc

{
{ PROC EXIT
proc exit
  inputl 'EXIT - Are you sure ? (Y/N)  > ' (sure)
  if sure
    qman_kill
    #exit
  end if
end proc

{
{ PROC CANCEL_ALL_QENTRIES
proc cancel_all_qentries
  qman_lock
  qman_delete delete_mode='ALL'
  qman_unlock
endproc

{
{ PROC LIST_QUEUE
proc list_queue
  qman_lock
  qman_sort sort_mode='DESCENDING'
  qman_list list_mode='ALL'
  qman_unlock
endproc

{
{ PROC ENTER_STRING
proc enter_string p1 p2
  if UNDEFINED(p1)
    input 'Input string?  > ' (i_string)
  else
    i_string = p1
  endif
  if UNDEFINED(p2)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p2)
  endif
  {
  w_string = '"' & (i_string) & '"'
  qman_lock
  qman_write string=(w_string) qposition=(qpos)
  qman_unlock
endproc

{
{ PROC ENTER_MASK
proc enter_mask p1 p2
  if UNDEFINED(p1)
    input 'Name of new mask?  > ' (mask)
  else
    mask = p1
  endif
  if UNDEFINED(p2)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p2)
  endif
  {
  w_string = '"DRMASK '&(mask)&'"'
  qman_lock
  qman_write string=(w_string) qposition=(qpos)
  qman_unlock
endproc

{
{ PROC ENTER_CONFIG
proc enter_config p1 p2
  if UNDEFINED(p1)
    input 'Name of new config?  > ' (config)
  else
    config = p1
  endif
  if UNDEFINED(p2)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p2)
  endif
  {
  w_string = '"DRCONFIG '&(config)&'"'
  qman_lock
  qman_write string=(w_string) qposition=(qpos)
  qman_unlock
endproc

{
{ PROC ENTER_SKYWT
proc enter_skywt p1 p2
  if UNDEFINED(p1)
    skywt = '1.0'
  else
    skywt = p1
  endif
  if UNDEFINED(p2)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p2)
  endif
  {
  w_string = '"DRSKYWT '&(skywt)&'"'
  qman_lock
  qman_write string=(w_string) qposition=(qpos)
  qman_unlock
endproc

{
{ PROC ENTER_VARWT
proc enter_varwt p1 p2
  if UNDEFINED(p1)
    varwt = 'false'
  else
    varwt = p1
  endif
  if UNDEFINED(p2)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p2)
  endif
  {
  w_string = '"DRVARWT '&(varwt)&'"'
  qman_lock
  qman_write string=(w_string) qposition=(qpos)
  qman_unlock
endproc

{
{ PROC ENTER_ENDGROUP
proc enter_endgroup p1 p2
  if UNDEFINED(p1)
    grpnum = 1
  else
    grpnum = p1
  endif
  if UNDEFINED(p2)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p2)
  endif
  {
  w_string = '"ENDGROUP rg'&getenv("QMAN_DATE")&'_'&(grpnum)&'"'
  qman_lock
  qman_write string=(w_string) qposition=(qpos)
  qman_unlock
endproc

{
{ PROC ENTER_INT_RANGE
proc enter_int_range p1 p2 p3 p4
  if UNDEFINED(p1)
    obsnum = 1
  else
    obsnum = p1
  endif
  if UNDEFINED(p2)
    int_start = 1
  else
    int_start = INTEGER(p2)
  endif
  if UNDEFINED(p3)
    int_end = int_start
  else
    int_end = INTEGER(p3)
  endif
  if UNDEFINED(p4)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p4)
  endif
  root = 'REDUCE i'&getenv("QMAN_DATE")&'_'&(obsnum)&'_'
  {
  qman_lock
  if qpos = 'OLDEST'
    loop for i = int_start to int_end step 1
      w_string = '"' & (root) & STRING(i) & '"'
      qman_write string=(w_string) qposition=(qpos)
    endloop
  {
  else if qpos = 'NEWEST'
    loop for i = int_end to int_start step -1
      w_string = '"' & (root) & STRING(i) & '"'
      qman_write string=(w_string) qposition=(qpos)
    endloop
  endif
  qman_unlock
endproc

{
{ PROC ENTER_OBS_RANGE
proc enter_obs_range p1 p2 p3
  if UNDEFINED(p1)
    obs_start = 1
  else
    obs_start = INTEGER(p1)
  endif
  if UNDEFINED(p2)
    obs_end = obs_start
  else
    obs_end = INTEGER(p2)
  endif
  if UNDEFINED(p3)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p3)
  endif
  r_root = 'REDUCE o'&getenv("QMAN_DATE")&'_'
  e_root = 'END o'&getenv("QMAN_DATE")&'_'
  {
  qman_lock
  if qpos = 'OLDEST'
    loop for i = obs_start to obs_end step 1
      w_string = '"' & (r_root) & STRING(i) & '"'
      qman_write string=(w_string) qposition=(qpos)
      w_string = '"' & (e_root) & STRING(i) & '"'
      qman_write string=(w_string) qposition=(qpos)
    endloop
  {
  else if qpos = 'NEWEST'
    loop for i = obs_end to obs_start step -1
      w_string = '"' & (e_root) & STRING(i) & '"'
      qman_write string=(w_string) qposition=(qpos)
      w_string = '"' & (r_root) & STRING(i) & '"'
      qman_write string=(w_string) qposition=(qpos)
    endloop
  endif
  qman_unlock
endproc

{
{ PROC ENTER_END_RANGE
proc enter_end_range p1 p2 p3
  if UNDEFINED(p1)
    obs_start = 1
  else
    obs_start = INTEGER(p1)
  endif
  if UNDEFINED(p2)
    obs_end = obs_start
  else
    obs_end = INTEGER(p2)
  endif
  if UNDEFINED(p3)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p3)
  endif
  e_root = 'END o'&getenv("QMAN_DATE")&'_'
  {
  qman_lock
  if qpos = 'OLDEST'
    loop for i = obs_start to obs_end step 1
      w_string = '"' & (e_root) & STRING(i) & '"'
      qman_write string=(w_string) qposition=(qpos)
    endloop
  {
  else if qpos = 'NEWEST'
    loop for i = obs_end to obs_start step -1
      w_string = '"' & (e_root) & STRING(i) & '"'
      qman_write string=(w_string) qposition=(qpos)
    endloop
  endif
  qman_unlock
endproc

{
{ PROC CANCEL_STRING
proc cancel_string p1 p2
  if UNDEFINED(p1)
    input 'Input string?  > ' (i_string)
  else
    i_string = p1
  endif
  if UNDEFINED(p2)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p2)
  endif
  {
  d_string = '"' & (i_string) & '"'
  qman_lock
  qman_read read_mode='SEARCH' search_mode=(qpos) string=(d_string) destructive=T
  qman_unlock
endproc


{
{ PROC CANCEL_MASK
proc cancel_mask p1 p2
  if UNDEFINED(p1)
    input 'Name of old mask?  > ' (mask)
  else
    mask = p1
  endif
  if UNDEFINED(p2)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p2)
  endif
  {
  d_string = '"DRMASK '&(mask)&'"'
  qman_lock
  qman_read read_mode='SEARCH' search_mode=(qpos) string=(d_string) destructive=T
  qman_unlock
endproc

{
{ PROC CANCEL_CONFIG
proc cancel_config p1 p2
  if UNDEFINED(p1)
    input 'Name of old config?  > ' (config)
  else
    config = p1
  endif
  if UNDEFINED(p2)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p2)
  endif
  {
  d_string = '"DRCONFIG '&(config)&'"'
  qman_lock
  qman_read read_mode='SEARCH' search_mode=(qpos) string=(d_string) destructive=T
  qman_unlock
endproc

{
{ PROC CANCEL_SKYWT
proc cancel_skywt p1 p2
  if UNDEFINED(p1)
    skywt = '1.0'
  else
    skywt = p1
  endif
  if UNDEFINED(p2)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p2)
  endif
  {
  d_string = '"DRSKYWT '&(skywt)&'"'
  qman_lock
  qman_read read_mode='SEARCH' search_mode=(qpos) string=(d_string) destructive=T
  qman_unlock
endproc

{
{ PROC CANCEL_VARWT
proc cancel_varwt p1 p2
  if UNDEFINED(p1)
    varwt = 'false'
  else
    varwt = p1
  endif
  if UNDEFINED(p2)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p2)
  endif
  {
  d_string = '"DRVARWT '&(varwt)&'"'
  qman_lock
  qman_read read_mode='SEARCH' search_mode=(qpos) string=(d_string) destructive=T
  qman_unlock
endproc

{
{ PROC CANCEL_ENDGROUP
proc cancel_endgroup p1 p2
  if UNDEFINED(p1)
    grpnum = 1
  else
    grpnum = p1
  endif
  if UNDEFINED(p2)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p2)
  endif
  {
  d_string = '"ENDGROUP rg'&getenv("QMAN_DATE")&'_'&(grpnum)&'"'
  qman_lock
  qman_read read_mode='SEARCH' search_mode=(qpos) string=(d_string) destructive=T
  qman_unlock
endproc

{
{ PROC CANCEL_INT_RANGE
proc cancel_int_range p1 p2 p3 p4
  if UNDEFINED(p1)
    obsnum = 1
  else
    obsnum = p1
  endif
  if UNDEFINED(p2)
    int_start = 1
  else
    int_start = INTEGER(p2)
  endif
  if UNDEFINED(p3)
    int_end = int_start
  else
    int_end = INTEGER(p3)
  endif
  if UNDEFINED(p4)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p4)
  endif
  {
  root = 'REDUCE i'&getenv("QMAN_DATE")&'_'&(obsnum)&'_'
  qman_lock
  if qpos = 'OLDEST'
    loop for i = int_start to int_end step 1
      d_string = '"' & (root) & STRING(i) & '"'
      qman_read read_mode='SEARCH' search_mode='OLDEST' string=(d_string) destructive=TRUE
    endloop
  {
  else if qpos = 'NEWEST'
    loop for i = int_end to int_start step -1
      d_string = '"' & (root) & STRING(i) & '"'
      qman_read read_mode='SEARCH' search_mode='NEWEST' string=(d_string) destructive=TRUE
    endloop
  endif
  qman_unlock
endproc

{
{ PROC CANCEL_OBS_RANGE
proc cancel_obs_range p1 p2 p3
  if UNDEFINED(p1)
    obs_start = 1
  else
    obs_start = INTEGER(p1)
  endif
  if UNDEFINED(p2)
    obs_end = obs_start
  else
    obs_end = INTEGER(p2)
  endif
  if UNDEFINED(p3)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p3)
  endif
  r_root = 'REDUCE o'&getenv("QMAN_DATE")&'_'
  e_root = 'END o'&getenv("QMAN_DATE")&'_'
  qman_lock
  {
  if qpos = 'OLDEST'
    loop for i = obs_start to obs_end step 1
      d_string = '"' & (r_root) & STRING(i) & '"'
      qman_read read_mode='SEARCH' search_mode='OLDEST' string=(d_string) destructive=TRUE
      d_string = '"' & (e_root) & STRING(i) & '"'
      qman_read read_mode='SEARCH' search_mode='OLDEST' string=(d_string) destructive=TRUE
    endloop
  {
  else if qpos = 'NEWEST'
    loop for i = obs_end to obs_start step -1
      d_string = '"' & (e_root) & STRING(i) & '"'
      qman_read read_mode='SEARCH' search_mode='NEWEST' string=(d_string) destructive=TRUE
      d_string = '"' & (r_root) & STRING(i) & '"'
      qman_read read_mode='SEARCH' search_mode='NEWEST' string=(d_string) destructive=TRUE
    endloop
  endif
  qman_unlock
endproc

{
{ PROC CANCEL_END_RANGE
proc cancel_end_range p1 p2 p3
  if UNDEFINED(p1)
    obs_start = 1
  else
    obs_start = INTEGER(p1)
  endif
  if UNDEFINED(p2)
    obs_end = obs_start
  else
    obs_end = INTEGER(p2)
  endif
  if UNDEFINED(p3)
    qpos = 'OLDEST'
  else
    qpos = UPCASE(p3)
  endif
  e_root = 'END o'&getenv("QMAN_DATE")&'_'
  qman_lock
  {
  if qpos = 'OLDEST'
    loop for i = obs_start to obs_end step 1
      d_string = '"' & (e_root) & STRING(i) & '"'
      qman_read read_mode='SEARCH' search_mode='OLDEST' string=(d_string) destructive=TRUE
    endloop
  {
  else if qpos = 'NEWEST'
    loop for i = obs_end to obs_start step -1
      w_string = '"' & (e_root) & STRING(i) & '"'
      qman_read read_mode='SEARCH' search_mode='NEWEST' string=(d_string) destructive=TRUE
    endloop
  endif
  qman_unlock
endproc

{
{ load it
qman_load
