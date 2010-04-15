      SUBROUTINE CAP_PLTST (GAI, MCENTR, TITLE, STATUS)
*+
*  Name:
*     CAP_PLTST
*  Purpose:
*     Draw the annotation and border and set up for plotting lists.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PLTST (GAI, MCENTR, TITLE; STATUS)
*  Description:
*     Draw the annotation and border and set up for plotting the
*     graphics attributes lists.
*  Arguments:
*     GAI  =  INTEGER (Given)
*        Identifier to the graphics attributes list.
*     MCENTR =  LOGICAL (Given)
*        Flag indicating whether a central cross is to be plotted,
*        coded as follows:
*        .TRUE.   -  plot the cross,
*        .FALSE.  -  do not plot the cross.
*     TITLE  =  CHARACTER*(*) (Given)
*        Title for the plot.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the text style.
*     Set the viewport and window for the annotation.
*     Plot the annotation.
*     Plot the orientation.
*     Initialise the Y position for the legends.
*     Attempt to compute the standard coordinates of the window for
*     the chart.
*     Set the ranges for the viewport and window for the chart.
*     Plot the scale bar.
*     Set the viewport and window for the chart.
*     Draw the bounding box around the chart.
*     If required then
*       Plot the central cross.
*     end if
*     Initialise the list of symbols available as the default symbol.
*     Set the current default symbol.
*     Set the fill area style to solid.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     18/8/96 (ACD): Original version.
*     3/6/97  (ACD): Converted for CURSA.
*     6/6/97  (ACD): First stable version.
*     20/4/01 (ACD): Modified the way that the window for the chart is
*       computed.
*     8/5/01  (ACD): Fixed a bug in calculating the limits for the chart
*       window.
*-
*  Type Definitions:
      IMPLICIT NONE       ! No implicit typing
*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CIO_PAR'   ! CIO parametric constants.
      INCLUDE 'CHART_PAR' ! CATCHART parametric constants.
*  Global Variables:
      INCLUDE 'CHART_CMN' ! CATCHART common block.
*  Arguments Given:
      INTEGER
     :  GAI
      LOGICAL
     :  MCENTR
      CHARACTER
     :  TITLE*(*)
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      REAL
     :  XTEXT,    ! Current X text position.
     :  YTEXT,    !    "    Y  "      "    .
     :  XORINT,   ! X coord. of plate orientation cross.
     :  YORINT,   ! Y   "  . "    "        "        "  .
     :  RANGE     ! Window range.
      INTEGER
     :  LENGTH,   ! Length of string (excl. trail. blanks).
     :  LBUFF,    ! Current position in BUFFER.
     :  LOOP,     ! Loop index.
     :  SLASTT,   ! SLA status.
     :  ROWS,     ! Number of rows in the graphics attributes list.
     :  ROW,      ! Current row in the graphics attributes list.
     :  RAI,      ! Identifier for Right Ascension column.
     :  DECI      !      "      "  Declination       "   .
      DOUBLE PRECISION
     :  RA,       ! Right Ascension of current object.
     :  DEC,      ! Declination     "     "      "   .
     :  XI,       ! } Standard (tangent plane) coordinates of current
     :  ETA       ! } object.
      DOUBLE PRECISION
     :  MAXXI,    ! Maximum xi.
     :  MAXETA,   !    "    eta.
     :  MINXI,    ! Minimum xi.
     :  MINETA,   !    "    eta.
     :  RADIUS,   ! Radius of the plot in standard coordinates.
     :  MAXDEC,   ! Maximum Declination.
     :  MINDEC,   ! Minimum      "     .
     :  RADDEC    ! Declination radius.
      LOGICAL
     :  MORE,     ! Flag; more rows to process?
     :  NULFLG    ! Null value flag.
      CHARACTER
     :  BUFFER*100  ! Buffer for output text.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Set the text style.

         CALL PGSCF (2)

*
*       Set the viewport and window for the annotation.

         AVXMN__CIO = 0.0E0
         AVXMX__CIO = 1.0E0
         AVYMN__CIO = 0.0E0
         AVYMX__CIO = 1.0E0

         AWXMN__CIO = 0.0E0
         AWXMX__CIO = 1.0E0
         AWYMN__CIO = 0.0E0
         AWYMX__CIO = 1.0E0

         CALL PGVPORT (AVXMN__CIO, AVXMX__CIO, AVYMN__CIO, AVYMX__CIO)
         CALL PGWINDOW (AWXMN__CIO, AWXMX__CIO, AWYMN__CIO, AWYMX__CIO)

*
*       Plot the annotation.
*
*       First the title, which consists of the coordinates of the
*       field centre.

         BUFFER = ' '
         LBUFF = 0

         CALL CHR_PUTC ('Field Centre:  Right Ascension ', BUFFER,
     :     LBUFF)

         IF (RAC__CIO .NE. ' ') THEN
            LENGTH = CHR_LEN (RAC__CIO)
            CALL CHR_PUTC (RAC__CIO(1 : LENGTH), BUFFER, LBUFF)
         ELSE
            CALL CHR_PUTC ('<blank>', BUFFER, LBUFF)
         END IF

         CALL CHR_PUTC (', Declination ', BUFFER, LBUFF)

         IF (DECC__CIO .NE. ' ') THEN
            LENGTH = CHR_LEN (DECC__CIO)
            CALL CHR_PUTC (DECC__CIO(1 : LENGTH), BUFFER, LBUFF)
         ELSE
            CALL CHR_PUTC ('<blank>', BUFFER, LBUFF)
         END IF

         IF (EQUNX__CIO .NE. ' '  .OR.  EPOCH__CIO .NE. ' ') THEN
            CALL CHR_PUTC (' (', BUFFER, LBUFF)

            IF (EQUNX__CIO .NE. ' ') THEN
               CALL CHR_PUTC ('Equinox ', BUFFER, LBUFF)
               LENGTH = CHR_LEN (EQUNX__CIO)
               CALL CHR_PUTC (EQUNX__CIO(1 : LENGTH), BUFFER, LBUFF)
            END IF

            IF (EQUNX__CIO .NE. ' '  .AND.  EPOCH__CIO .NE. ' ') THEN
               CALL CHR_PUTC (', ', BUFFER, LBUFF)
            END IF

            IF (EPOCH__CIO .NE. ' ') THEN
               CALL CHR_PUTC ('Epoch ', BUFFER, LBUFF)
               LENGTH = CHR_LEN (EPOCH__CIO)
               CALL CHR_PUTC (EPOCH__CIO(1 : LENGTH), BUFFER, LBUFF)
            END IF

            CALL CHR_PUTC (').', BUFFER, LBUFF)
         END IF

         CALL PGTEXT (5.0E-2, 9.5E-1, BUFFER(1 : LBUFF) )

         IF (TITLE .NE. ' ') THEN
            LENGTH = CHR_LEN(TITLE)
            CALL PGPTEXT (3.67E-1, 5.0E-2, 0.0E0, 5.0E-1, TITLE(1 :
     :        LENGTH) )
         END IF

*
*      Plot the orientation.

         BUFFER = ' '
         LBUFF = 0

         XTEXT = 7.0E-1
         YTEXT = 9.0E-1 - (CIO__TINCR * 5.0E-1)

         CALL PGTEXT (XTEXT, YTEXT, 'Orientation: ')

         XORINT = 8.5E-1
         YORINT = YTEXT

         CALL PGTEXT (XORINT, YORINT+CIO__HINCR, 'N')
         CALL PGTEXT (XORINT-CIO__HINCR, YORINT, 'E')
         CALL PGTEXT (XORINT+CIO__HINCR, YORINT, 'W')
         CALL PGTEXT (XORINT, YORINT-CIO__HINCR, 'S')

*
*       Initialise the Y position for the legends.

         YTEXT__CIO = YTEXT - CIO__TINCR

*
*       Compute the coordinates of the window for the chart in
*       standard coordinates.  First examine every point in the list,
*       calculate their standard coordinates and find the extrema.

         CALL CAT_TROWS (GAI, ROWS, STATUS)

         CALL CAT_TIDNT (GAI, 'RA', RAI, STATUS)
         CALL CAT_TIDNT (GAI, 'DEC', DECI, STATUS)

         MAXXI = 0.0D0
         MAXETA = 0.0D0
         MINXI = 0.0D0
         MINETA = 0.0D0

         ROW = 0
         MORE = .TRUE.

         DO WHILE (MORE)
            ROW = ROW + 1
            CALL CAT_RGET (GAI, ROW, STATUS)

            CALL CAT_EGT0D (RAI, RA, NULFLG, STATUS)
            CALL CAT_EGT0D (DECI, DEC, NULFLG, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN
               CALL SLA_DS2TP (RA, DEC, RA__CIO, DEC__CIO, XI, ETA,
     :           SLASTT)
               IF (SLASTT .EQ. 0) THEN
                  IF (XI .GT. MAXXI) THEN
                     MAXXI = XI
                  END IF

                  IF (XI .LT. MINXI) THEN
                     MINXI = XI
                  END IF

                  IF (ETA .GT. MAXETA) THEN
                     MAXETA = ETA
                  END IF

                  IF (ETA .LT. MINETA) THEN
                     MINETA = ETA
                  END IF

                  IF (ROW .EQ. 1) THEN
                     MAXDEC = DEC
                     MINDEC = DEC

                  ELSE
                     IF (DEC .GT. MAXDEC) THEN
                        MAXDEC = DEC
                     END IF

                     IF (DEC .LT. MINDEC) THEN
                        MINDEC = DEC
                     END IF
                  END IF

               END IF
            END IF

*
*          If all the rows have been read then set the termination flag.

            IF (ROW .GE. ROWS) THEN
               MORE = .FALSE.
            END IF

*
*          If the status is bad then set the termination flag.

            IF (STATUS .NE. SAI__OK) THEN
               MORE = .FALSE.
            END IF
         END DO

*
*       Compute the radius.  Note that the minimum permitted radius is
*       1 second of arc (expressed in radians).

         MAXXI = ABS(MAXXI)
         MINXI = ABS(MINXI)
         MAXETA = ABS(MAXETA)
         MINETA = ABS(MINETA)

         RADIUS = MAX(MAXXI, MINXI, MAXETA, MINETA)
         RADIUS = RADIUS * 1.05D0
         RADIUS = MAX(RADIUS, 4.8481368D-6)

*
*       Set the ranges for the viewport and window for the chart.
*       Note that the X axis is `flipped' in order to plot the
*       Right Ascension increasing from right to left.

         CWXMN__CIO = RADIUS
         CWXMX__CIO = -RADIUS
         CWYMN__CIO = -RADIUS
         CWYMX__CIO = RADIUS

         CVXMN__CIO = 5.0E-2
         CVXMX__CIO = 6.75E-1
         CVYMN__CIO = 1.0E-1
         CVYMX__CIO = 9.0E-1

*
*       Plot the scale bar.

         RADDEC = (MAXDEC - MINDEC) / 2.0D0
         RADDEC = MAX(RADDEC, 4.8481368D-6)
         CALL CAP_SCLBR (RADDEC, STATUS)

*
*       Set the viewport and window for the chart.

         CALL PGVPORT (CVXMN__CIO, CVXMX__CIO, CVYMN__CIO,
     :     CVYMX__CIO)
         CALL PGWNAD (CWXMN__CIO, CWXMX__CIO, CWYMN__CIO, CWYMX__CIO)

*
*       Draw the bounding box around the chart.

         CALL PGBOX ('BC', 0.0E0, 0, 'BC', 0.0E0, 0)

*
*       Plot the central cross if it is required.  Note that by
*       definition the coordinates of the central cross are 0,0.

         IF (MCENTR) THEN
            RANGE = CWXMX__CIO - CWXMN__CIO

            CALL CAP_GNSGT (0.0E0, 0.0E0, RANGE, STATUS)
         END IF

*
*       Initialise the list of symbols available as the default symbol;
*       only the symbols with a single 'SIZEn' attribute as available
*       as the default symbol.

         AVSYM__CIO(1) = .FALSE.
         AVSYM__CIO(2) = .FALSE.

         DO LOOP = 3, 13
            AVSYM__CIO(LOOP) = .TRUE.
         END DO

         DO LOOP = 14, 18
            AVSYM__CIO(LOOP) = .FALSE.
         END DO

*
*       Set the current default symbol.

         DSYMB__CIO = CIO__SOPCR

*
*       Set the fill area style to solid.  Note that a solid fill
*       area is the default, so this call should not be necessary.
*       However, it is included for robustness.

         CALL PGSFS (1)

      END IF

      END
