      SUBROUTINE ECH_WAVE_POLYFIT( CHANS, WAVES, STATI, WEIGHTS, RMSES,
     :           FITS, AUTOCLIP, NLID, NX, NC, DRMS, NCOEFF, COEFFS,
     :           RMS )
*+
*  Name:
*     ECHOMOP - ECH_WAVE_POLYFIT

*  Purpose:
*     Fit identified lines.

*  Description:
*     Performs a fit to the identified lines and list the results.

*  Invocation:
*      CALL ECH_WAVE_POLYFIT( CHANS, WAVES, STATI, WEIGHTS, RMSES,
*     :     FITS, AUTOCLIP, NLID, NX, NC, DRMS, NCOEFF, COEFFS, RMS )

*  Arguments:
*     CHANS = REAL (Given)
*        The centers of the identified lines, in pixel numbers.
*     WAVES = REAL (Given)
*        The wavelengths of the identified lines.
*     STATI = INTEGER (Given)
*        The statuses of the identified lines.
*     WEIGHTS = REAL (Given)
*        The weights for the identified arc lines.
*     NLID = INTEGER (Given)
*        The number of identified lines.
*     NX = INTEGER (Given)
*        The number of pixels in the arc data.
*     NC = INTEGER (Given)
*        The maximum number of polynomial coefficients.
*     DRMS = LOGICAL (Given)
*        True if the RMS if omitted statistic is to be output for each line.
*     NCOEFF = INTEGER (Given)
*        Passed as the initial number of parameters used for the fit,
*        returned as the final number used.
*     COEFFS = DOUBLE (Returned)
*        The coefficients of the final fit.

*  Authors :
*     KS: Keith Shortridge (CIT/AAO)
*     WFL: William Lupton (AAO)
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_further_authors_here}

*  History:
*     14-JUN-1983 (KS):
*       First version.
*     04-SEP-1985 (KS):
*       Option to repeat with a different NCOEFF removed.
*       Now incorporated in main routine.  WEIGHTS parameter added,
*       and 'fit without this line' figure added. DRMS parameter added.
*     30-JUN-1986 (KS):
*       No longer modifies NCOEFF if not enough lines have been selected.
*     31-MAY-1988 (WFL):
*       Avoid crash if NLID is too small.
*     01-SEP-1992 (DMILLS):
*       Initial ECHOMOP release.
*     08-APR-1996 (MJC):
*       Tidy up, standard prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'ECH_FEATURE.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Parameters:
      INTEGER NLID
      REAL CHANS( NLID )
      REAL WAVES( NLID )
      INTEGER STATI( NLID )
      REAL WEIGHTS( NLID )
      REAL RMSES( NLID )
      REAL FITS( NLID )
      LOGICAL AUTOCLIP
      INTEGER NX
      INTEGER NC
      LOGICAL DRMS
      INTEGER NCOEFF
      DOUBLE PRECISION COEFFS( NC )
      REAL RMS

*  Functions Called:
      INTEGER CHR_LEN

*  Local Variables:
      DOUBLE PRECISION FIT

      REAL TWAVES( 500 )
      REAL DEVS( 500 )
      REAL RDUMMY( 2 )
      REAL RMS_POSS
      REAL ADISP
      REAL END
      REAL RMSX
      REAL ST
      REAL VALUE
      REAL VALUE1
      REAL REQ_IMPROVEMENT
      REAL MEAN
      REAL SIGMA
      REAL SUMSQ

      INTEGER I
      INTEGER IGNORE
      INTEGER NC1
      INTEGER NC3
      INTEGER NEWNCF
      INTEGER STATUS
      INTEGER WORST
      INTEGER NSPLINE
      INTEGER CCOEFF
      INTEGER II
      INTEGER IL
      INTEGER IT
      INTEGER NCHAR

      CHARACTER CBLENDS( 1024 )
      CHARACTER*72 CHARS
      CHARACTER*16 REF_STR
      CHARACTER*6 FITTER

      LOGICAL BAD
*.

   1  CONTINUE
      REQ_IMPROVEMENT = 0.5
      FITTER = 'POLY'
      IF ( NCOEFF .GT. 15 ) FITTER = 'SPLINE'
      IF ( FITTER .EQ. 'POLY' .AND. NLID .LE. NCOEFF ) THEN
         NCOEFF = NLID - 1

      ELSE IF ( FITTER .EQ. 'SPLINE' .AND.
     :          ( NLID + 7 ) * 2 .LE. NCOEFF ) THEN
         NCOEFF = ( NLID + 7 ) * 2
      END IF
      NSPLINE = NCOEFF / 2 - 7
      NEWNCF = NCOEFF

      IF ( NCOEFF .GT. 1 ) THEN

*  Perform fit.
      DO I = 1, NC
         COEFFS( I ) = 0.0
      END DO

   2  CONTINUE
      CHARS = 'REAL-' // FITTER
      CALL ECH_FITTER( CHARS, NEWNCF, COEFFS, NLID,
     :     CHANS, WAVES, WEIGHTS, 0, 1000., STATUS )
      NCOEFF = NEWNCF

      DO I = 1, MIN( NLID, 1024 )
         IF ( IAND( STATI( I ), FTR_POSIB_BLEND ) .NE. 0 ) THEN
            CBLENDS( I ) = 'B'

         ELSE
            CBLENDS( I ) = ' '
         END IF
      END DO

*  Display results.
      CALL ECH_REPORT( 0, ' ' )
      IF ( FITTER .EQ. 'POLY' ) THEN
         CALL CHR_ITOC( NEWNCF - 1, REF_STR, NCHAR )
         IF ( NEWNCF .EQ.2 ) THEN
            CHARS = ' 1st'
            NCHAR = 4

         ELSE IF ( NEWNCF .EQ. 3 ) THEN
            CHARS = ' 2nd'
            NCHAR = 4

         ELSE IF ( NEWNCF .EQ. 4 ) THEN
            CHARS = ' 3rd'
            NCHAR = 4

         ELSE
            CHARS = ' ' // REF_STR( :NCHAR ) // 'th'
            NCHAR = NCHAR + 3
         END IF

         CHARS = CHARS( :NCHAR ) // '-order polynomial fit.'

      ELSE
         CALL CHR_ITOC( NSPLINE, REF_STR, NCHAR )
         CHARS = REF_STR( :NCHAR ) // '-knot spline fit.'
      END IF
      CALL ECH_REPORT( 0, CHARS )
      CALL ECH_REPORT( 0,' Coefficients of fit are:' )

*  Print coefficents.
      NC3 = 0
      DO WHILE ( NC3 .LT. NEWNCF )
         NC1 = NC3 + 1
         NC3 = MIN( NC3 + 3, NEWNCF )
         WRITE ( CHARS, '(3(1PE13.5))',ERR=320)(COEFFS(I),I=NC1,NC3)
  320    CALL ECH_REPORT( 0, CHARS )
      END DO
      CALL ECH_REPORT( 0,' ' )

*  Evaluate mean dispersion.
      RDUMMY( 1 ) = 1.0

*  Cludge by MJC 01-MAY-1996.
      WAVELENGTH_UNITS = 'Angstroms'
      IL = CHR_LEN( WAVELENGTH_UNITS )

      CALL ECH_FEVAL( FITTER, NCOEFF, COEFFS, 1, RDUMMY, ST, STATUS )
      VALUE = FLOAT( NX )
      RDUMMY( 1 ) = VALUE
      CALL ECH_FEVAL( FITTER, NCOEFF, COEFFS, 1, RDUMMY, END, STATUS )
      ADISP = ABS( END - ST ) / FLOAT( NX )
      WRITE ( CHARS, '( A, 1PE9.2, 1X, A )', ERR = 330 )
     :      ' Mean dispersion    =', ADISP,
     :      WAVELENGTH_UNITS( :IL ) // '/channel.'
  330 CALL ECH_REPORT( 0, CHARS )

*  And also give start, central, and end wavelengths.
      RDUMMY( 1 ) = VALUE / 2.0
      CALL ECH_FEVAL( FITTER, NCOEFF, COEFFS, 1, RDUMMY, VALUE, STATUS )
      WRITE ( CHARS, '( A, F9.3, 1X, A )', ERR = 340 )
     :      ' Start wavelength   =',
     :      ST, WAVELENGTH_UNITS( :IL ) // '.'
  340 CALL ECH_REPORT( 0, CHARS )
      WRITE ( CHARS, '( A, F9.3, 1X, A )', ERR = 350 )
     :      ' Central wavelength =',
     :      VALUE, WAVELENGTH_UNITS( :IL ) // '.'
  350 CALL ECH_REPORT( 0, CHARS )
      WRITE ( CHARS, '( A, F9.3, 1X, A )', ERR = 360 )
     :      ' End wavelength     =',
     :      END, WAVELENGTH_UNITS( :IL ) // '.'
  360 CALL ECH_REPORT( 0, CHARS )

*  Produce title for table.
      CALL ECH_REPORT(0,' ')
      IF (DRMS) THEN
         CHARS = '           Line    Wavelength  Calculated ' //
     :         'Discrepancy    RMS if'
         CALL ECH_REPORT( 0, CHARS )
         CHARS = '                               Wavelength' //
     :         '                omitted'
         CALL ECH_REPORT( 0, CHARS )

      ELSE
         CHARS = '           Line    Wavelength  Calculated Discrepancy'
         CALL ECH_REPORT( 0, CHARS )
         CHARS = '                               Wavelength'
         CALL ECH_REPORT( 0, CHARS )
      END IF
      CALL ECH_REPORT( 0,' ' )

*  Now print table and work out RMS error on the way.
      RMS = 0.0
      MEAN = 0.0
      SUMSQ = 0.0
      RMS_POSS = 1.0E20
      ADISP = 1.0 / ( ADISP * ADISP * 0.25 )
      DO I = 1, NLID
         CALL ECH_FEVAL( FITTER, NCOEFF, COEFFS, 1, CHANS( I ),
     :        VALUE, STATUS )
         FITS( I ) = VALUE
            VALUE1 = VALUE - WAVES( I )
         RMS = RMS + VALUE1 * VALUE1
         IF ( DRMS ) THEN
            IF ( NLID .LE. 2 ) THEN
               RMSX = 0.0

            ELSE
               CALL ARC_ARFITX( I, CHANS, WAVES, WEIGHTS, NLID,
     :              NEWNCF, RMSX )
            END IF
            IF ( IAND( STATI( I ), FTR_MANU_IDENT ) .EQ. 0 ) THEN
               MEAN = MEAN + RMSX
               SUMSQ = SUMSQ + RMSX * RMSX
               RMSES( I ) = RMSX
               DEVS( I ) = VALUE1
               IF ( RMS_POSS .GT. RMSX ) THEN
                  RMS_POSS = RMSX
                  WORST = I
               END IF
            END IF
            IF ( I .LE. 1024 ) THEN
               WRITE ( CHARS, '( I4, 4F12.3, F12.5, 1X, A1 )',
     :                 IOSTAT = IGNORE )
     :               I, CHANS( I ), WAVES( I ), VALUE, VALUE1, RMSX,
     :               CBLENDS( I )
            END IF

         ELSE
            IF ( I .LE. 1024 ) THEN
               WRITE ( CHARS, '( I4, 4F12.3, 1X, A1 )',
     :                 IOSTAT = IGNORE )
     :               I, CHANS( I ), WAVES( I ), VALUE, VALUE1,
     :               CBLENDS( I )
            END IF
         END IF
         CALL ECH_REPORT( 0, CHARS )
      END DO

*   And print out RMS error.
      MEAN = MEAN / FLOAT( NLID )
      SIGMA = SQRT( ABS( SUMSQ / FLOAT( NLID ) - MEAN * MEAN ) )
      CALL ECH_REPORT( 0, ' ' )
      RMS = SQRT( RMS / FLOAT( NLID ) )
      CALL CHR_RTOC( FLOAT( INT( RMS * 10000.0 ) ) / 10000.0, REF_STR,
     :     NCHAR )
      CHARS = ' RMS error: ' // REF_STR( :NCHAR ) // '.'
      CALL ECH_REPORT( 0, CHARS )

      IF ( ccoeff .GT. 15 ) ccoeff = ccoeff / 2 - 7
      req_improvement = mean - 2.0 * sigma
      IF ( autoclip .AND. RMS .GT. 0.0 ) THEN
         IF ( rms_poss .LE. req_improvement .AND.
     :        rms_poss / rms .GT. 0.0  .AND.
     :        rms_poss / rms .LT. 0.8 .AND.
     .        nlid .GE. ccoeff*1.5 ) THEN
            CALL ECH_REPORT(0,' Re-evaluating after a clip.')
            rmsx = ABS(devs(worst)/MAX(0.001,rmses(worst)))
            DO ii = 1, nlid
               IF ( IAND ( stati(ii),ftr_manu_ident ) .EQ. 0 ) THEN
                  IF ( rmses (ii) .LT. 1.2*rmses(worst) .AND.
     :                 ABS( devs(ii)/MAX(0.001,rmses(ii)) ) .GT.
     :                 rmsx ) THEN
                     worst = ii
                     rmsx = ABS( devs(ii)/MAX(0.001,rmses(ii)) )
                  END IF
               END IF
            END DO
            DO ii = worst + 1, nlid
               waves( ii-1 ) = waves( ii )
               chans( ii-1 ) = chans( ii )
               stati( ii-1 ) = stati( ii )
            ENDDO
            waves( nlid ) = 0.0
            chans( nlid ) = 0.0
            stati( nlid ) = 0
            nlid = nlid - 1
            GO TO 1
         END IF
      END IF

      RDUMMY( 1 ) = 1.0
      CALL ECH_FEVAL( FITTER, NCOEFF, COEFFS, 1, RDUMMY, ST, STATUS )
      RDUMMY( 1 ) = 2.0
      CALL ECH_FEVAL( FITTER, NCOEFF, COEFFS, 1, RDUMMY, END, STATUS )
      ST = END - ST
      BAD = .FALSE.
      it = 0
      DO i = 3, nx, MAX( 1, nx / 500 + 1 )
         it = it + 1
         twaves( it ) = FLOAT( i )
      END DO
      CALL ECH_FEVAL( FITTER, NCOEFF, COEFFS, IT, TWAVES, DEVS, STATUS )
      DO i = 2, it
         fit = devs( i ) - devs( i - 1 )
         IF ( st .GT. 0.0 ) THEN
            IF ( fit .LT. 0.0D0 ) bad = .TRUE.

         ELSE
            IF ( fit .GT. 0.0D0 ) bad = .TRUE.
         END IF
      END DO
         IF ( bad .AND. newncf .GT. 2 ) THEN
            CALL ECH_REPORT( 0,
     :           ' Lowering degrees of freedom due to instability.' )
            newncf = newncf - 1
            IF ( fitter .EQ. 'SPLINE' ) newncf = newncf - 1
            GO TO 2
         END IF

      ELSE
         CALL ECH_REPORT( 0,' No degrees of freedom: no fit.' )
      END IF

      END
