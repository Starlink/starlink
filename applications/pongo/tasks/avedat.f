      SUBROUTINE AVEDAT( STATUS )
*+
*  Name:
*     AVEDAT

*  Purpose:
*     Average the data in the XCOL and YCOL areas.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AVEDAT( STATUS )

*  Description:
*     Rebin the XCOL and YCOL data, averaging the data in each sample,
*     and puts the result back into the XCOL and YCOL areas. The
*     standard deviations of the averages are put into the EXCOL and
*     EYCOL areas.  There are two ways in which the averaging may be
*     done:
*
*       - the data may be split into N equally sized bins over the X
*         range, and the values in each bin averaged;
*       - the data may be averaged in groups of N data.

*  Usage:
*     avedat action [nbin]

*  ADAM Parameters:
*     ACTION = _CHAR (Read)
*        The type of binning be used for the averaging. If "X", the
*        data are divided into NBIN bins over the X range.  If "N",
*        bins of varying widths with each containing NBIN data points
*        are formed.
*
*        [The value will be prompted for. It has the default "X".]
*     NBIN = _INTEGER (Read)
*        Depending upon the value of ACTION, either the number of bins
*        (ACTION="X"), or the number of points per bin (ACTION="N").
*
*        [The value will be prompted for. It has the default 10.]
*     XMIN = _REAL (Read)
*        The minimum X value to be used in the average.
*
*        [The value of the global parameter PONGO_XMIN is used. If
*        PONGO_XMIN is not defined, the default value 0.0 is used.]
*     XMAX = _REAL (Read)
*        The maximum X value to be used in the average.
*
*        [The value of the global parameter PONGO_XMAX is used. If
*        PONGO_XMAX is not defined, the default value 1.0 is used.]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-APR-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     2-JUN-1994 (PDRAPER):
*        Added explicit type changes (DBLE to REAL and vice versa).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 1 ) ACTION   ! Type of averaging

      INTEGER NBIN               ! Number of bins/ number of points to
                                 ! average
      INTEGER I                  ! Counter
      INTEGER IDAT               ! Counter
      INTEGER IDAT2
      INTEGER INDEX( NDATMAX )   ! Indices of sorted data array
      INTEGER NDATTEMP           ! Number of averaged data points
      INTEGER NEND               ! Number of points in end of average
      INTEGER NSTART             ! Number of points in start of average

      REAL XMAXP                 ! X limits
      REAL XMINP                 ! X limits
      REAL XRANGE                ! Bin size
      REAL XSTART
      REAL XSUM                  ! Sum of X
      REAL XXSUM                 ! Sum of X**2
      REAL YSUM                  ! Sum of Y
      REAL YYSUM                 ! Sum of Y**2

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL PAR_GET0C( 'ACTION', ACTION, STATUS )
      CALL CHR_UCASE( ACTION )
      CALL PAR_GET0I( 'NBIN', NBIN, STATUS )
      CALL PAR_GET0R( 'XMIN', XMINP, STATUS )
      CALL PAR_GET0R( 'XMAX', XMAXP, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

         IF ( ACTION .EQ. 'X' ) THEN

*  Calculate the X range for the average.
            XRANGE = ( XMAXP - XMINP ) / NBIN
         END IF

*  Sort the data values.
         CALL INDEXX( NDAT, XDATA, INDEX )

*  Pass over any values lower than XMINP.
         IDAT = 1

         DO WHILE ( XDATA( INDEX( IDAT ) ) .LT. XMINP )
            IDAT = IDAT + 1
         END DO

         IF ( ACTION .EQ. 'X' ) THEN
            NDATTEMP = 0
            IDAT2 = 0
            XSTART = XMINP
            YSUM = 0.0
            YYSUM = 0.0
            XSUM = 0.0
            XXSUM = 0.0

*  Fudge to stop errors on the final loop.
            INDEX( NDAT + 1 ) = 1

            DO WHILE ( ( IDAT .LE. NDAT ) .AND.
     :                 ( XDATA( INDEX( IDAT ) ) .LE. XMAXP ) )

               DO WHILE ( ( XDATA( INDEX( IDAT ) ) .GE. XSTART ) .AND.
     :                    ( IDAT .LE. NDAT ) .AND.
     :                    ( XDATA( INDEX( IDAT ) ) .LE. XSTART+XRANGE ))
                  YSUM = YSUM + REAL( YDATA( INDEX( IDAT ) ) )
                  YYSUM = YYSUM + REAL( YDATA( INDEX( IDAT ) )**2 )
                  XSUM = XSUM + REAL( XDATA( INDEX( IDAT ) ) )
                  XXSUM = XXSUM + REAL( XDATA( INDEX( IDAT ) )**2 )
                  NDATTEMP = NDATTEMP + 1
                  IDAT = IDAT + 1
               END DO

               IF ( NDATTEMP .NE. 0 ) THEN
                  IDAT2 = IDAT2 + 1
                  XDATATEMP( IDAT2 ) = XSUM / NDATTEMP
                  YDATATEMP( IDAT2 ) = YSUM / NDATTEMP
                  ERRX( IDAT2 ) = SQRT( MAX( 0.0,
     :                 REAL( XXSUM/NDATTEMP
     :                 -XDATATEMP( IDAT2 )**2
     :                 ) ) )
                  ERRY( IDAT2 ) = SQRT( MAX( 0.0,
     :                 REAL( YYSUM/NDATTEMP
     :                 -YDATATEMP( IDAT2 )**2
     :                 ) ) )
                  YSUM = 0.0
                  YYSUM = 0.0
                  XSUM = 0.0
                  XXSUM = 0.0
                  NDATTEMP = 0
                  XSTART = XSTART + XRANGE
               ELSE IF ( XDATA( INDEX( IDAT ) )
     :                 .GT. XSTART+XRANGE ) THEN
                  XSTART = XSTART + XRANGE
               ELSE
                  IDAT = IDAT + 1
               END IF
            END DO
         ELSE IF ( ACTION .EQ. 'N' ) THEN

*  Calculate the numbers of points to be included at the ends.
            NEND = MOD( NDAT-IDAT+1, NBIN )
            NSTART = NEND / 2
            NEND = NEND - NSTART
            YSUM = 0.0
            YYSUM = 0.0
            XSUM = 0.0
            XXSUM = 0.0

            IF ( NSTART .NE. 0 ) THEN

               DO I = IDAT, IDAT+NSTART-1
                  YSUM = YSUM + REAL( YDATA( INDEX( I ) ) )
                  YYSUM = YYSUM + REAL( YDATA( INDEX( I ) )**2 )
                  XSUM = XSUM + REAL( XDATA( INDEX( I ) ) )
                  XXSUM = XXSUM + REAL( XDATA( INDEX( I ) )**2 )
               END DO

               IDAT2 = 1
               XDATATEMP( IDAT2 ) = XSUM / NSTART
               YDATATEMP( IDAT2 ) = YSUM / NSTART
               ERRX( IDAT2 ) =
     :    SQRT( MAX( 0.0, REAL( XXSUM/NSTART -XDATATEMP( IDAT2 )**2 )))
               ERRY( IDAT2 ) =
     :    SQRT( MAX( 0.0, REAL( YYSUM/NSTART -YDATATEMP( IDAT2 )**2 )))
            ELSE
               IDAT2 = 0
            END IF

            IDAT = IDAT + NSTART

            DO WHILE ( IDAT .LE. NDAT-NEND-NBIN+1 )
               YSUM = 0.0
               YYSUM = 0.0
               XSUM = 0.0
               XXSUM = 0.0

               DO I = IDAT, IDAT+NBIN-1
                  YSUM = YSUM + REAL( YDATA( INDEX( I ) ) )
                  YYSUM = YYSUM + REAL( YDATA( INDEX( I ) )**2 )
                  XSUM = XSUM + REAL( XDATA( INDEX( I ) ) )
                  XXSUM = XXSUM + REAL( XDATA( INDEX( I ) )**2 )
               END DO

               IDAT2 = IDAT2 + 1
               IDAT = IDAT + NBIN
               XDATATEMP( IDAT2 ) = XSUM / NBIN
               YDATATEMP( IDAT2 ) = YSUM / NBIN
               ERRX( IDAT2 ) =
     :       SQRT( MAX( 0.0, REAL( XXSUM/NBIN -XDATATEMP( IDAT2 )**2 )))
               ERRY(IDAT2) =
     :       SQRT( MAX( 0.0, REAL( YYSUM/NBIN -YDATATEMP( IDAT2 )**2 )))
            END DO

            IF ( NEND .NE. 0 ) THEN
               YSUM = 0.0
               YYSUM = 0.0
               XSUM = 0.0
               XXSUM = 0.0

               DO I = IDAT, IDAT+NEND-1
                  YSUM = YSUM + REAL( YDATA( INDEX( I ) ) )
                  YYSUM = YYSUM + REAL( YDATA( INDEX( I ) )**2 )
                  XSUM = XSUM + REAL( XDATA( INDEX( I ) ) )
                  XXSUM = XXSUM + REAL( XDATA( INDEX( I ) )**2 )
               END DO

               IDAT2 = IDAT2 + 1
               XDATATEMP( IDAT2 ) = XSUM / NEND
               YDATATEMP( IDAT2 ) = YSUM / NEND
               ERRX( IDAT2 ) =
     :       SQRT( MAX( 0.0, REAL( XXSUM/NEND -XDATATEMP( IDAT2 )**2 )))
               ERRY( IDAT2 ) =
     :       SQRT( MAX( 0.0, REAL( YYSUM/NEND -YDATATEMP( IDAT2 )**2 )))
            END IF
         END IF

*  Values averaged, so now put back into main X and Y arrays.
         NDAT=IDAT2

         DO IDAT = 1, NDAT
            XDATA( IDAT ) = DBLE( XDATATEMP( IDAT ) )
            YDATA( IDAT ) = DBLE( YDATATEMP( IDAT ) )
         END DO
      END IF

*  Check the returned status and report a contextual error message if
*  necessary.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'AVEDAT_END',
     :     'AVEDAT: Cannot rebin the data in the XCOL and YCOL data ' //
     :     'areas.', STATUS )

      END
* $Id$
