      SUBROUTINE CAP_SAXES (XMIN, XMAX, YMIN, YMAX, TITLE, XLABEL,
     :  YLABEL, STATUS)
*+
*  Name:
*     CAP_SAXES
*  Purpose:
*     Plot the axes for a scatterplot.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_SAXES (XMIN, XMAX, YMIN, YMAX, TITLE, XLABEL,
*       YLABEL; STATUS)
*  Description:
*     Plot the axes for a scatterplot.
*  Arguments:
*     XMIN  =  REAL (Given)
*        Minimum value of the X axis.
*     XMAX  =  REAL (Given)
*        Maximum value of the X axis.
*     YMIN  =  REAL (Given)
*        Minimum value of the Y axis.
*     YMAX   =  REAL (Given)
*        Maximum value of the Y axis.
*     TITLE   =  CHARACTER*(*) (Given)
*        Title.
*     XLABEL   =  CHARACTER*(*) (Given)
*        Label for the X axis.
*     YLABEL   =  CHARACTER*(*) (Given)
*        Label for the Y axis.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Define the viewport.
*     Define the window.
*     Draw the axes.
*     Label the axes.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     9/7/98  (ACD): Original version.
*     10/7/98 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard Starlink constants.
*  Arguments Given:
      REAL
     :  XMIN,
     :  XMAX,
     :  YMIN,
     :  YMAX
      CHARACTER
     :  TITLE*(*),
     :  XLABEL*(*),
     :  YLABEL*(*)
*  Status:
      INTEGER STATUS             ! Global status.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Define the viewport.

         CALL PGVSTD

*
*       Define the window.

         CALL PGSWIN (XMIN, XMAX, YMIN, YMAX)

*
*       Draw the axes.

         CALL PGBOX ('BCNST', 0.0E0, 0, 'BCNST', 0.0E0, 0)

*
*       Label the axes.

         CALL PGLAB (XLABEL, YLABEL, TITLE)

      END IF

      END
