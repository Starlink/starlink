      SUBROUTINE CAP_GPCLS (STATUS)
*+
*  Name:
*     CAP_GPCLS
*  Purpose:
*     Close a any catview plot (scatterplot or histogram).
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GPCLS (STATUS)
*  Description:
*     Close a any catview plot (scatterplot or histogram).
*
*     Note that this routine deliberately does not check whether
*     there is a catalogue open.
*  Arguments:
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is an open scatterplot then
*       Close the plot.
*       Release any work space.
*       Reset the plot flags.
*     end if
*     If there is an open histogram then
*       Close the plot.
*       Release any work space.
*       Reset the plot flags.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     9/7/98  (ACD): Original version.
*     10/7/98 (ACD): First stable version.
*     15/9/99 (ACD): Added option for closing histograms.  Changed
*        the name, from CAP_GSCLS, to reflect this change of functionality.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
*  Global Variables:
      INCLUDE 'SPLOT_CMN'         ! catview scatterplot common block.
*  Status:
      INTEGER STATUS              ! Global status.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check whether there is a scatterplot open.

         IF (OPEN__SPLOT) THEN

*
*          Close the plot.

            CALL PGEND

*
*          Release any work space.

            IF (PTS__SPLOT .GT. 0) THEN
               CALL CAP_FREAR (XPTR__SPLOT, STATUS)
               CALL CAP_FREAR (YPTR__SPLOT, STATUS)
            END IF

*
*          Reset the plot flags.  Only OPEN__SPLOT is important; the
*          rest are set for completeness.

            OPEN__SPLOT = .FALSE.
            XPRS__SPLOT = .FALSE.
            AXPL__SPLOT = .FALSE.
            AUTO__SPLOT = .TRUE.

         END IF

*
*       Check whether there is a histogram open.

         IF (OPEN__HIST) THEN

*
*          Close the plot.

            CALL PGEND

*
*          Release any work space.

            IF (PTS__HIST .GT. 0) THEN
               CALL CAP_FREAR (XPTR__HIST, STATUS)
            END IF

*
*          Reset the plot flags.  Only OPEN__HIST is important; the
*          rest are set for completeness.

            OPEN__HIST = .FALSE.
            AXPL__HIST = .FALSE.
            NORM__HIST = .FALSE.
            AUTO__HIST = .TRUE.
            BINSP__HIST = .FALSE.

         END IF

      END IF

      END
