{+
{ cgs4dr
{
{ author: Phil Daly
{ date:   08-Dec-1994
{
{-

print 'Loading the P4 procedures'
load ${CGS4DR_ROOT}/p4.icl

print 'Loading the QMAN procedures'
load ${CGS4DR_ROOT}/qman.icl

print 'Loading the RED4 procedures'
load ${CGS4DR_ROOT}/red4.icl

print 'Loading the CRED4 procedures'
load ${CGS4DR_ROOT}/cred4.icl

{
{ Defstring some commands
defstring cgs4dr       print Portable-CGS4DR must be run from the shell!
defstring cgs4dr_demo  load ${CGS4DR_ROOT}/demo/cgs4dr_demo.icl
defstring cgs4dr_demo1 load ${CGS4DR_ROOT}/demo/cgs4dr_demo.icl
defstring cgs4dr_demo2 load ${CGS4DR_ROOT}/demo/cgs4dr_demo2.icl

{
{ PROC EXIT
proc exit
  inputl 'EXIT - Are you sure? (Y/N)  > ' (sure)
  if sure
    p4_rmfile
    p4_saved
    p4_close_nb
    qman_saved save_mode='new'
    cred4_close_qfile
    cred4_rmfile
    cred4_saved
    cred4_close_nb
    #exit
  endif
endproc

print 'To start Unix-CGS4DR, type start_autoreduce'
