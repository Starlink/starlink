      SUBROUTINE snx_AGWV
*+
*  Name:
*     AGWV

*  Purpose:
*     Set up NCAR AUTOGRAPH graph window to match the
*     current GKS viewport.

*  Language:
*     Starlink Fortran 77

*  Externals:
*     GQCNTN, GQNT, AGSETP

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-APR-1986 (PTW):
*        Original.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      IMPLICIT NONE

      REAL WIND(4),VIEWP(4)
      INTEGER J

      INTEGER NCT



*  Inquire the current GKS transformation number
      CALL GQCNTN(J,NCT)

*  Find out what part of the NDC square is available
      CALL GQNT(NCT,J,WIND,VIEWP)

*  Establish the AUTOGRAPH coordinate system
      CALL AGSETP('GRAPH.',VIEWP,4)

      END
