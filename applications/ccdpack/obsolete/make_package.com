$!+
$!  Name:
$!     MAKE_PACKAGE

$!  Purpose:
$!     To make up all the components of the CCDPACK package.

$!  Language:
$!     DCL

$!  Description:
$!     This routine requires the presence of a subroutine library,
$!     a task library and an IFL library. A monolith IFL file is created
$!     then the monolith itself. Each task is then extracted and an 
$!     atask is created.

$!  Authors:
$!     PDRAPER: Peter Draper (STARLINK)
$!     {enter_new_authors_here}

$!  History:
$!     28-MAY-1991 (PDRAPER):
$!        Original version.
$!     {enter_changes_here}

$!-
$!
$! setup CCDPACK for development
$!
$if f$trnlnm("CCDPACK_DEV_STARTED") .eqs. "" then @CCDPACK_DEV
$!
$! make the monolith IFL file
$@make_monolith_ifl
$!
$! link the monolith
$!
$@make_monolith
$!
$! extract the tasks from the text library
$!
$@make_atasks
$exit
$! $Id$
