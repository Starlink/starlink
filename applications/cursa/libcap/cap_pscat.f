      SUBROUTINE CAP_PSCAT (PGSYMB, PGCOL, PTS, XVAL, YVAL, STATUS)
*+
*  Name:
*     CAP_PSCAT
*  Purpose:
*     Plot a set of points as a scatterplot.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PSCAT (PGSYMB, PGCOL, PTS, XVAL, YVAL; STATUS)
*  Description:
*     Plot a set of points as a scatterplot.  The axis etc. should
*     already have been set up.
*  Arguments:
*     PGSYMB  =  INTEGER (Given)
*        PGPLOT symbol to be used to plot the points.
*     PGCOL  =  INTEGER (Given)
*        PGPLOT colour index to be used to plot the points.
*     PTS  =  INTEGER (Given)
*        Number of points.
*     XVAL(PTS)  =  REAL (Given)
*        X values of the points.
*     YVAL(PTS)  =  REAL (Given)
*        Y values of the points.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the required colour.
*     Plot the points.
*     Restore the default colour.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     9/7/98 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      INTEGER
     :  PGSYMB,
     :  PGCOL,
     :  PTS
      REAL
     :  XVAL(PTS),
     :  YVAL(PTS)
*  Status:
      INTEGER STATUS             ! Global status
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Set the required colour index.

         CALL PGSCI (PGCOL)

*
*       Plot the points.

         CALL PGPOINT (PTS, XVAL, YVAL, PGSYMB)

*
*       Restore the default colour.

         CALL PGSCI (1)

      END IF

      END
