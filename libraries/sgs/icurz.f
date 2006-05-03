      SUBROUTINE sgs_ICURZ (IZONID)
*+
*  Name:
*     ICURZ

*  Purpose:
*     Return zone identifier for current zone.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     IZONID = INTEGER (Returned)
*         Zone identifier

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
*     ISZID      i     current zone ID

*-

      IMPLICIT NONE

      INTEGER IZONID

      INCLUDE 'sgscom'




      IZONID=ISZID

      END
