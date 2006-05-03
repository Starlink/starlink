      SUBROUTINE sgs_IPEN (NPEN)
*+
*  Name:
*     IPEN

*  Purpose:
*     Inquire current SGS pen number.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     NPEN = INTEGER (Returned)
*         SGS pen number

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

*  Read From Common:
*     IPEN     i    SGS pen number

*-

      IMPLICIT NONE

      INTEGER NPEN

      INCLUDE 'sgscom'




      NPEN = IPEN

      END
