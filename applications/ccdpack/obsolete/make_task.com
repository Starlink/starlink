c
$!+
$!  Name:
$!     MAKE_ATASK

$!  Purpose:
$!     To to relink a CCDPACK atask.

$!  Language:
$!     DCL

$!  Authors:
$!     PDRAPER: Peter Draper (STARLINK)
$!     {enter_new_authors_here}

$!  History:
$!     28-MAY-1991 (PDRAPER):
$!        Original version.
$!     {enter_changes_here}
$!-
$!
$if p1.eqs."" then exit
$fort 'p1'
$alink 'p1',-
ccdpack_libdir:ccdpack/opt,-
ccdpack_libdir:ccdpack/lib,-    ! Some unresolved references
ccdpack_libdir:irg_link/opt,-
psx_dir:psx_link_adam/opt,-
nag_lib/lib,-
ccdpack_libdir:ccdpack/lib,-    ! Some unresolved references
trn_link/opt,-
prm_link/opt
$exit
$! $Id$
