      SUBROUTINE sgs_1HSTAT (JSTAT)
*+
*  Name:
*     HSTAT

*  Purpose:
*     Handle incoming status.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     internal routine

*  Description:
*     If SGS not in 'inherited status' mode, set JSTAT to zero;
*     otherwise do nothing.

*  Arguments:
*     JSTAT = INTEGER (Given & Returned)
*         Incoming status (if enabled)
*         Status (if non-inherited)

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
*     JSOPT     i      status handling mode

*-

      IMPLICIT NONE

      INTEGER JSTAT

      INCLUDE 'sgscom'



      IF (JSOPT.EQ.0) JSTAT=0

      END
