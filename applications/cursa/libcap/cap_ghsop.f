      SUBROUTINE CAP_GHSOP (GDEVIC, TITLE, XEXPR, STATUS)
*+
*  Name:
*     CAP_GHSOP
*  Purpose:
*     Open a catview histogram.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GHSOP (GDEVIC, TITLE, XEXPR; STATUS)
*  Description:
*     Open a catview histogram.
*  Arguments:
*     GDEVIC  =  CHARACTER*(*) (Given)
*        Name of the required graphics device.
*     TITLE  =  CHARACTER*(*) (Given)
*        Title of the scatter plot.  If the string is blank a title
*        will be invented.
*     XEXPR  =  CHARACTER*(*) (Given)
*        Expression for the quantity to be plotted on the X axis.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is an open catalogue then
*       Close any existing plot.
*       Initialise the scatterplot flags; a plot has not yet been
*       produced.
*       Attempt to get an identifier for the X axis expression.
*       If ok then
*         Attempt to open the required graphics device.
*         If ok then
*           Set the PGPLOT prompt state.
*           Create the X axis label.
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
*     14/9/99  (ACD): Original version (from CAP_GSCOP).
*     18/11/99 (ACD): First stable version.
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
     :  XEXPR*(*)
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
     :  PGSTAT       ! Status returned from PGBEGIN.
      CHARACTER
     :  XLABEL*40    ! Label for the X axis.
*.


      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN

*
*          Close any existing plot.

            CALL CAP_GPCLS (STATUS)

*
*          Initialise the common block histogram flags to indicate
*          that there is no histogram open.  Only OPEN__HIST is
*          important; the rest are set for completeness.

            OPEN__HIST = .FALSE.
            AXPL__HIST = .FALSE.
            NORM__HIST = .FALSE.
            AUTO__HIST = .TRUE.
            BINSP__HIST = .FALSE.

            NBINS__HIST = 40

*
*          Also initialise the plotting range.  Note that the initial
*          Y range is deliberately set to an impossible value for a
*          histogram.

            XMIN__HIST = 0.0E0
            XMAX__HIST = 0.0E0
            YMIN__HIST = 0.0E0
            YMAX__HIST = -1.0E0

*
*          Attempt to get an identifier for the X axis expressions and
*          proceed if ok.

            CALL CAT_EIDNT (CI__SGZ, XEXPR, XID, STATUS)

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
*                Create the X axis label and proceed if ok.

                  CALL CAP_CAXLB (CI__SGZ, XEXPR, XLABEL, STATUS)

                  IF (STATUS .EQ. SAI__OK) THEN

*
*                   A histogram has been opened ok; set the common block
*                   flags.

                     OPEN__HIST = .TRUE.
                     AXPL__HIST = .FALSE.
                     NORM__HIST = .FALSE.
                     AUTO__HIST = .TRUE.
                     BINSP__HIST = .FALSE.

                     NBINS__HIST = 40

*
*                   Set the common block variables defining the
*                   histogram.

                     XID__HIST = XID
                     XLABL__HIST = XLABEL

                     TITLE__HIST = TITLE

                     PTS__HIST = 0
                     XPTR__HIST = 0

                  ELSE
                     CALL ERR_REP ('CAP_GHSOP_AXL', 'Error generating '/
     :                 /'X axis label.', STATUS)

                  END IF

               ELSE
                  STATUS = SAI__ERROR

                  CALL MSG_SETC ('GDEVIC', GDEVIC)
                  CALL MSG_SETI ('PGSTAT', PGSTAT)

                  CALL ERR_REP ('CAP_GHSOP_OPG', 'PGPLOT error '/
     :              /'opening graphics device  ^GDEVIC (code: '/
     :              /'^PGSTAT).', STATUS)

               END IF

            ELSE
               CALL ERR_REP ('CAP_GHSOP_AXE', 'Failed to get '/
     :           /'identifier for the column to be histogrammed.',
     :           STATUS)

            END IF

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

      END IF

      END
