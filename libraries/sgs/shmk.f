      SUBROUTINE sgs_SHMK (S)
*+
*  Name:
*     SHMK

*  Purpose:
*     Specify marker size.

*  Language:
*     Starlink Fortran 77

*  Description:
*     This routine is obsolete and does nothing other than store the
*     specified size in common.

*  Arguments:
*     S = REAL (Given)
*         Marker height (=width)

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
*     HMK       r      current marker height

*-

      IMPLICIT NONE
      REAL S

      INCLUDE 'sgscom'




      HMK=S

      END
