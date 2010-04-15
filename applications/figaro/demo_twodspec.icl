{+
{  Name:
{     demo_twodspec.icl

{  Purpose:
{     Demonstration procedure for Twodspec.

{  Type of Module:
{     ICL procedure

{  Invocation:
{     load $FIG_DIR/demo_twodspec

{  Arguments:
{     none.

{  Description:
{     A short test of Twodspec

{  Authors:
{     tdca: Tim Ash (RAL, Starlink)
{     {enter_new_authors_here}

{  History:
{     24 Jun 1997 (tdca):
{        Original version.
{     14 Feb 2002 (BLY)
{        Modified to copy test data to working directory to avoid
{           datafile access errors.
{        Cosmetic tidyup.
{     {enter_further_changes_here}

{  Bugs:
{     {note_any_bugs_here}

{-
{.

print ' '
print 'Running TWODSPEC test'
print ' '

setenv FIGARO_MODE BATCH

{  Copy the test data.
! cp $FIG_DIR/test_data.sdf .

{  Run longslit on it.
longslit test_data r ITERATION=0 MAXGAUSS=2 MAXLINES=1

{  Remove the test data

! rm test_data.sdf

{  End of demo.icl
