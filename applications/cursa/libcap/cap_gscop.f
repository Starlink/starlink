      SUBROUTINE CAP_GSCOP (GDEVIC, TITLE, XEXPR, YEXPR, STATUS)
*+
*  Name:
*     CAP_GSCOP
*  Purpose:
*     Open a catview scatterplot.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSCOP (GDEVIC, TITLE, XEXPR, YEXPR; STATUS)
*  Description:
*     Open a catview scatterplot.
*  Arguments:
*     GDEVIC  =  CHARACTER*(*) (Given)
*        Name of the required graphics device.
*     TITLE  =  CHARACTER*(*) (Given)
*        Title of the scatter plot.  If the string is blank a title
*        will be invented.
*     XEXPR  =  CHARACTER*(*) (Given)
*        Expression for the quantity to be plotted on the X axis.
*     YEXPR  =  CHARACTER*(*) (Given)
*        Expression for the quantity to be plotted on the Y axis.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is an open catalogue then
*       Close any existing plot.
*       Initialise the scatterplot flags; a plot has not yet been
*       produced.
*       Attempt to get an identifier for the X axis expression.
*       Attempt to get an identifier for the Y axis expression.
*       If ok then
*         Attempt to open the required graphics device.
*         If ok then
*           Set the PGPLOT prompt state.
*           Create the axis labels.
*           If ok then
*             Set the scatterplot flags.
*           else
*             Report an error.
*           end if
*         else
*           Report an error.
*         end if
*       else
*         Report an error.
*       end if
*     else
*       Report a message; there is no open catalogue.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     9/7/98   (ACD): Original version.
*     19/11/98 (ACD): First stable version.
*     13/9/99  (ACD): Corrected token name in the error reports.
*     14/9/99  (ACD): Added closing of plot.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'SGZ_PAR'           ! catview parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! catview common block.
      INCLUDE 'SPLOT_CMN'         ! catview scatterplot common block.
*  Arguments Given:
      CHARACTER
     :  GDEVIC*(*),
     :  TITLE*(*),
     :  XEXPR*(*),
     :  YEXPR*(*)
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER PGBEGIN
*  Local Constants:
      INTEGER PGOK   ! Success status for PGBEGIN.
      PARAMETER (PGOK = 1)
*  Local Variables:
      INTEGER
     :  XID,         ! Identifier for the X axis expression.
     :  YID,         !     "       "   "  Y  "       "     .
     :  PGSTAT       ! Status returned from PGBEGIN.
      CHARACTER
     :  XLABEL*40,   ! Label for the X axis.
     :  YLABEL*40    !   "    "   "  Y  "  .
*.


      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Close any existing plot.

            CALL CAP_GPCLS (STATUS)

*
*          Initialise the common block scatterplot flags to indicate
*          that there is no scatterplot open.  Only OPEN__SPLOT is
*          important; the rest are set for completeness.

            OPEN__SPLOT = .FALSE.
            XPRS__SPLOT = .FALSE.
            AXPL__SPLOT = .FALSE.
            AUTO__SPLOT = .TRUE.

*
*          Also initialise the plotting range.

            XMIN__SPLOT = 0.0E0
            XMAX__SPLOT = 0.0E0
            YMIN__SPLOT = 0.0E0
            YMAX__SPLOT = 0.0E0

*
*          Attempt to get an identifiers for the X and Y axis
*          expressions and proceed if ok.

            CALL CAT_EIDNT (CI__SGZ, XEXPR, XID, STATUS)
            CALL CAT_EIDNT (CI__SGZ, YEXPR, YID, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN

*
*             Attempt to open the required graphics device and proceed
*             if ok.

               PGSTAT = PGBEGIN (0, GDEVIC, 1, 1)
               IF (PGSTAT .EQ. PGOK) THEN

*
*                Set the PGPLOT prompt state, so that a prompt is
*                not issued when PGPAGE clears the screen.

                  CALL PGASK (.FALSE.)

*
*                Create the axis labels and proceed if ok.

                  CALL CAP_CAXLB (CI__SGZ, XEXPR, XLABEL, STATUS)
                  CALL CAP_CAXLB (CI__SGZ, YEXPR, YLABEL, STATUS)

                  IF (STATUS .EQ. SAI__OK) THEN

*
*                   A plot has been opened ok; set the common block
*                   flags.

                     OPEN__SPLOT = .TRUE.
                     XPRS__SPLOT = .TRUE.
                     AXPL__SPLOT = .FALSE.
                     AUTO__SPLOT = .TRUE.

*
*                   Set the common block variables defining the
*                   scatterplot.

                     XID__SPLOT = XID
                     YID__SPLOT = YID

                     XLABL__SPLOT = XLABEL
                     YLABL__SPLOT = YLABEL

                     TITLE__SPLOT = TITLE

                     PTS__SPLOT = 0
                     XPTR__SPLOT = 0
                     YPTR__SPLOT = 0

                  ELSE
                     CALL ERR_REP ('CAP_GSCOP_AXL', 'Error generating '/
     :                 /'axis labels.', STATUS)

                  END IF

               ELSE
                  STATUS = SAI__ERROR

                  CALL MSG_SETC ('GDEVIC', GDEVIC)
                  CALL MSG_SETI ('PGSTAT', PGSTAT)

                  CALL ERR_REP ('CAP_GSCOP_OPG', 'PGPLOT error '/
     :              /'opening graphics device  ^GDEVIC (code: '/
     :              /'^PGSTAT).', STATUS)

               END IF

            ELSE
               CALL ERR_REP ('CAP_GSCOP_AXE', 'Failed to get '/
     :           /'identifiers for the columns to be plotted.',
     :           STATUS)

            END IF

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

      END IF

      END
