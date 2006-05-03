      SUBROUTINE sgs_ICURW (IWKID)
*+
*  Name:
*     ICURW

*  Purpose:
*     Inquire GKS workstation ID for currently selected zone.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     IWKID = INTEGER (Returned)
*         Workstation ID

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
*     IZWT     i()   zone table - SGS workstation ID
*     IWTID    i()   workstation table - GKS workstation ID
*     ISZID    i     current zone ID

*-

      IMPLICIT NONE

      INTEGER IWKID

      INCLUDE 'sgscom'




      IWKID=IWTID(ABS(IZTW(ISZID)))

      END
