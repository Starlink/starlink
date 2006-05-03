      SUBROUTINE sgs_SELCH (NCHDEV)
*+
*  Name:
*     SELCH

*  Purpose:
*     Set the current SGS choice device.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     NCHDEV = INTEGER (Given)
*         SGS choice device

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Written To Common:
*     NCHODV      i      current SGS choice device

*-

      IMPLICIT NONE

      INCLUDE 'sgscom'


      INTEGER NCHDEV



      NCHODV = NCHDEV

      END
