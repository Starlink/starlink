{+
{ cgs4dr_demo2.icl
{
{ Author: Phil Daly, JAC (pnd@jach.hawaii.edu)
{-

{
{ Cancel all the queue entries and clear the screen(s)
cancel_all_qentries
sh clear
p4_clearall

{
{ Write a header
print ' ** WELCOME TO THE PORTABLE-CGS4DR DEMONSTRATION **'
print ' '

{
{ File rg941011_110 as a standard
file_standard rg941011_110 6030 3.44 29 29

{
{ Set a new reduction seqeunce to include divide by standard
set_reduction_sequence YES NO YES NO YES YES NO YES YES YES YES
set_extract_spectrum 18 19 29 29 39 40 FALSE BRIGHT

{
{ Divide_by_standard
enter_endgroup 118

{
{ Display the result
enter_string 'DISPLAY DATA=$RGDIR/rg941011_118_dbs_imspc PORT=0'

{
{ Last message
enter_string 'DRCOMMENT ** THE FINAL REDUCED GROUP FOR NGC 1068 CONTAINS A LARGE ABSORPTION FEATURE **'
enter_string 'DRCOMMENT ** DUE TO INTERSTELLAR GRAINS IDENTIFIED WITH C_H STRETCHING IN ORGANICS **'
enter_string 'DRCOMMENT '
enter_string 'DRCOMMENT ** END OF PORTABLE-CGS4DR DEMO **'

