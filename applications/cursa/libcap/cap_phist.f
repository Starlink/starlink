      SUBROUTINE CAP_PHIST (PGCOL, PTS, HXVAL, HYVAL, WPTS, XWORK,
     :  YWORK, STATUS)
*+
*  Name:
*     CAP_PHIST
*  Purpose:
*     Plot a set of points as the line of a histogram.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PHIST (PGCOL, PTS, HXVAL, HYVAL, WPTS; XWORK, YWORK;
*       STATUS)
*  Description:
*     Plot a set of points as the line of a histogram.  The axis etc.
*     should already have been set up.
*  Arguments:
*     PGCOL  =  INTEGER (Given)
*        PGPLOT colour index to be used to plot the line.
*     PTS  =  INTEGER (Given)
*        Number of points.
*     HXVAL(PTS)  =  REAL (Given)
*        X values of points in the histogram.
*     HYVAL(PTS)  =  REAL (Given)
*        Y values of points in the histogram.
*     WPTS  =  INTEGER (Given)
*        Number of points in the constituting the histogram
*        ( = (2*PTS)+1 ).
*     XWORK(WPTS)  =  REAL (Used)
*        X coordinates in points in the histogram line.
*     YWORK(WPTS)  =  REAL (Used)
*        Y coordinates in points in the histogram line.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Assemble the work arrays of X and Y values.
*     Set the required colour.
*     Plot the line.
*     Restore the default colour.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/9/99 (ACD): Original version.
*     16/9/99 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      INTEGER
     :  PGCOL,
     :  PTS,
     :  WPTS
      REAL
     :  HXVAL(PTS),
     :  HYVAL(PTS)
*  Arguments Returned:
      REAL
     :  XWORK(WPTS),
     :  YWORK(WPTS)
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Variables:
      INTEGER
     :  CWPT,   ! Current point in the work arrays.
     :  LOOP    ! Loop index.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Assemble the work arrays of X and Y values which will be
*       plotted as the histogram.

         XWORK(1) = HXVAL(1) - ((HXVAL(2) - HXVAL(1)) * 5.0E-1)
         YWORK(1) = HYVAL(1)

         CWPT = 1

         DO LOOP = 1, PTS - 1
            CWPT = CWPT + 1

            XWORK(CWPT) = (HXVAL(LOOP) + HXVAL(LOOP + 1)) * 5.0E-1
            YWORK(CWPT) = HYVAL(LOOP)

            CWPT = CWPT + 1

            XWORK(CWPT) = XWORK(CWPT - 1)
            YWORK(CWPT) = HYVAL(LOOP + 1)
         END DO

         XWORK(WPTS) = HXVAL(PTS) +
     :     ((HXVAL(PTS) - HXVAL(PTS - 1)) * 5.0E-1)
         YWORK(WPTS) = HYVAL(PTS)

*
*       Set the required colour index.

         CALL PGSCI (PGCOL)

*
*       Plot the histogram line.

         CALL PGLINE (WPTS, XWORK, YWORK)

*
*       Restore the default colour.

         CALL PGSCI (1)

      END IF

      END
