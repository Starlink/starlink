      SUBROUTINE USR_AESHIFT( STATUS )
*+
*  Name:
*     SUBROUTINE USR_AESHIFT

*  Purpose:
*     Automated determination of the ESHIFT parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_AESHIFT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     A set of absorption features of known wavelengths are located in
*     the current spectrum and their wavelngths measured.  An ESHIFT for
*     each is calculated and the median ESHIFT value of these is applied to
*     the whole dataset.

*  Authors:
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     16-APR-95 (MJC):
*       Template version.
*     20-APR-95 (MJC):
*       Modified median selection to (n+2)/2th entry of array.
*       Modified so that the profile window re-centres on the smallest
*       flux found in the window prior to continuum estimation.
*       Modified so that the full Net Spectrum array can be searched for
*       a line.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMHEAD'       ! Dataset administration information.
      INCLUDE 'CMSAVE'       ! Order spectra data.
      INCLUDE 'CMDISP'       ! Dispersion data.
      INCLUDE 'CMECOR'       ! Echelle correction data.
      INCLUDE 'CMNET'        ! Net Spectrum arrays.
      INCLUDE 'CMWAV'        ! Wavelength arrays.

*  Local Constants:
      REAL*8 DELTAMAX        ! Maximum window half-width.
      PARAMETER ( DELTAMAX = 8.0 )

      INTEGER MAXAES         ! Maximum number of lines to average over.
      PARAMETER ( MAXAES = 64 )

      INTEGER MAXWN          ! Size of median calculation array.
      PARAMETER ( MAXWN = 1200 )

      INTEGER HIORD          ! Highest acceptable order number.
      INTEGER LOORD          ! Lowest acceptable order number.
      PARAMETER ( HIORD = 125, LOORD = 65 )

*  Status:
      INTEGER STATUS         ! Global status.

*  External References:
      EXTERNAL CAHI          ! HIRES calibration.
      LOGICAL STR_SIMLR      ! Caseless string equality.

*  Local Variables:
      REAL*8 FVAL( MAXAES )  ! Parameter values.
      REAL*8 CWAVE( MAXAES ) ! Line wavelengths.
      REAL*8 DWAVE( MAXAES ) ! Line search window half-widths.
      REAL*8 FCWAVE( MAXAES )! Line centres found.
      REAL*8 FLUXES( MAXWN ) ! Flux array for median calculation.
      REAL*8 CENWORD         ! Central wavelength of an order.
      REAL*8 LODELTAW        ! Line to order-centre delta wavelength.
      REAL*8 LOCALMIN        ! Flux value in line center.
      REAL*8 LOCALMED        ! Median flux value in window.
      REAL*8 SWP             ! Swap space.
      REAL*8 HHFLUX          ! Flux at line half-height.
      REAL*8 HHWAVE( 2 )     ! Line half-height points.
      REAL*8 WSCALE          ! Estimated wavelength scale.

      LOGICAL NEWESH         ! Whether current spectrum calibration changes.

      INTEGER LINEORD( MAXAES ) ! Best order for each selected line.
      INTEGER NLINE          ! Number of lines to be fitted.
      INTEGER ACTVAL         ! Parameter value count.
      INTEGER I              ! Loop index.
      INTEGER J              ! Loop index.
      INTEGER K              ! Loop index.
      INTEGER L              ! Loop index.
      INTEGER NFLUX          ! Number of points for median calculation.
      INTEGER NMIN           ! Index of minimum in window.
      INTEGER IAPER          ! Aperture index.
      INTEGER ISTAT          ! Local status.
      INTEGER STARTC         ! Index into Mean Spectrum.
      INTEGER ENDC           ! Index into Mean Spectrum.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get Calibration.
      CALL DASSOC( '\\', '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999

      ELSE IF ( NAPER .LT. 1 ) THEN
         CALL ERROUT( 'Error: no apertures defined\\', STATUS )
         GO TO 999

      ELSE
         NEWESH = .FALSE.
      END IF

*  Get Net Spectra.
      CALL DASSOC( 'S\\', 'T\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999
      END IF

*  Aperture/resolution.
      CALL DEFAPR( 0, IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: aperture/resolution invalid\\',
     :                STATUS )
         GO TO 999
      END IF

*  AESHIFT is only valid for HIRES.
      IF ( .NOT. STR_SIMLR( 'HIRES\\', RESOL ) ) THEN
         CALL ERROUT( 'Error: AESHIFT only applies to HIRES data\\',
     :                STATUS )
         GO TO 999
      END IF


*  Get Line wavelengths.
      DO WHILE ( .TRUE. )
         CALL RDPARF( 'CENTREWAVE\\', .FALSE., MAXAES, FVAL, ACTVAL,
     :                STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'CENTREWAVE\\', STATUS )
            GO TO 999

         ELSE
            DO I = 1, ACTVAL
               IF ( FVAL( I ) .LE. 1100.0 .OR.
     :              FVAL( I ) .GT. 3000.0 ) THEN
               CALL ERRPAR( 'CENTREWAVE\\' )
               CALL ERROUT( ': out of range\\', STATUS )
               END IF
            END DO
         END IF

         IF ( STATUS .EQ. SAI__OK ) THEN
            DO I = 1, ACTVAL
               CWAVE( I ) = FVAL ( I )
            END DO
            NLINE = ACTVAL
            GO TO 100

         ELSE
            CALL CNPAR( 'CENTREWAVE\\', ISTAT )
            IF ( ISTAT .NE. SAI__OK ) THEN
               CALL PCANER( 'CENTREWAVE\\', STATUS )
               GO TO 999
            END IF
         END IF
      END DO
 100  CONTINUE

*  Get Line search window widths.
      DO WHILE ( .TRUE. )
         CALL RDPARF( 'DELTAWAVE\\', .FALSE., MAXAES, FVAL, ACTVAL,
     :                STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'DELTAWAVE\\', STATUS )
            GO TO 999


         ELSE IF ( ACTVAL .NE. NLINE ) THEN
            IF ( ACTVAL .GT. 0 ) THEN
               DO I = ACTVAL + 1, NLINE
                  FVAL( I ) = FVAL( ACTVAL )
               END DO

            ELSE
               DO I = 1, NLINE
                  FVAL( I ) = 1.0
               END DO
            END IF
         END IF

         DO I = 1, ACTVAL
            IF ( FVAL( I ) .LE. 0.0 .OR.
     :           FVAL( I ) .GT. DELTAMAX ) THEN
               CALL ERRPAR( 'DELTAWAVE\\' )
               CALL ERROUT( ': out of range\\', STATUS )
            END IF
         END DO

         IF ( STATUS .EQ. SAI__OK ) THEN
            DO I = 1, NLINE
               DWAVE( I ) = FVAL( I )
            END DO
            GO TO 200

         ELSE
            CALL CNPAR( 'DELTAWAVE\\', ISTAT )
            IF ( ISTAT .NE. SAI__OK ) THEN
               CALL PCANER( 'DELTAWAVE\\', STATUS )
               GO TO 999
            END IF
         END IF
      END DO
 200  CONTINUE

*  Check for order spectra.
      IF ( NOSPEC ) THEN
         CALL ERROUT( 'No Net Spectra\\', STATUS )
         GO TO 999
      END IF

*  Calculate best order for each of selected lines using order data.
      DO I = 1, NLINE
         LODELTAW = 10000.0
         DO J = 1, NORDER
            CENWORD = ( WAV1S( J ) + WAV2S( J ) ) / 2.0
            IF ( ABS( CWAVE( I ) - CENWORD ) .LE. LODELTAW ) THEN
               LODELTAW = ABS( CWAVE( I ) - CENWORD )
               LINEORD( I ) = ORDERS( J )
            END IF
         END DO
      END DO

*  List lines with selected orders.
      CALL LINE_WCONT( '%p Selected lines:\\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WCONT( '%p Line    Window Order\\' )
      CALL PRTBUF( STATUS )
      DO I = 1, NLINE
         CALL LINE_WRITF( '%p %7.2f \\', CWAVE( I ) )
         CALL LINE_WRITF( '%6.2f \\', DWAVE( I ) )
         CALL LINE_WRITI( '  %3i\\', LINEORD( I ) )
         CALL PRTBUF( STATUS )
      END DO

*  Fit lines.
      DO I = 1, NLINE
         CALL LINE_WRITF( '%p Line: %.2f.', CWAVE( I ) )
         CALL PRTBUF( STATUS )

*     Load Net Spectrum arrays with chosen Order Spectrum.
         CALL RDORD( LINEORD( I ), CAHI, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Missing Order Spectrum\\', STATUS )
            GO TO 999
         END IF
         STARTC = NWAV
         DO WHILE ( ( CWAVE( I ) - DWAVE( I ) ) .LT. WAVAIR( STARTC )
     :              .AND. STARTC .GT. 1 )
            STARTC = STARTC - 1
         END DO
         ENDC = 1
         DO WHILE ( ( CWAVE( I ) + DWAVE( I ) ) .GT. WAVAIR( ENDC )
     :              .AND. ENDC .LT. NWAV )
            ENDC = ENDC + 1
         END DO
         WSCALE = ( WAVAIR( ENDC ) - WAVAIR( STARTC ) ) /
     :            DBLE ( ENDC - STARTC )
         LOCALMIN = SNET( STARTC )
         NMIN = STARTC

*  Find minimum flux level in wavelength window.
         DO J = STARTC, ENDC
            IF ( QNET( J ) .EQ. 0 ) THEN

*           Mark minimum position if this is the minimum.
               IF ( SNET( J ) .LT. LOCALMIN ) THEN
                  LOCALMIN = SNET( J )
                  NMIN = J
               END IF
            END IF
         END DO

*  Re-centre the window on the minimum flux found.
         STARTC = NMIN - DWAVE( I ) / WSCALE + 1
         IF ( STARTC .LT. 1 ) STARTC = 1
         ENDC = NMIN + DWAVE( I ) / WSCALE
         IF ( ENDC .GT. NWAV ) ENDC = NWAV
         K = 1
         DO J = STARTC, ENDC
            IF ( QNET( J ) .EQ. 0 ) THEN

*           Add to median array if this is a valid datum.
               FLUXES( K ) = SNET( J )
               K = K + 1
            END IF
         END DO
         NFLUX = K - 1

*     Sort fluxes and find median.
         DO J = 1, NFLUX - 1
            DO L = J + 1, NFLUX
               IF ( FLUXES( L ) .LT. FLUXES( J ) ) THEN
                  SWP = FLUXES( L )
                  FLUXES( L ) = FLUXES( J )
                  FLUXES( J ) = SWP
               END IF
            END DO
         END DO
         LOCALMED = FLUXES( ( NFLUX + 2 ) / 2 )
         HHFLUX = ( LOCALMED - LOCALMIN ) / 2.0 + LOCALMIN

*     Print minimum and median.
         CALL LINE_WRITF( '%p   Minimum flux: %.3f\\', LOCALMIN )
         CALL LINE_WRITF( ' at %.3fA.\\', WAVAIR( NMIN ) )
         CALL PRTBUF( STATUS )
         CALL LINE_WRITF( '%p   Median flux: %.3f\\', LOCALMED )
         CALL PRTBUF( STATUS )
         CALL LINE_WRITF( '%p   Flux half-height: %.3f\\', HHFLUX )
         CALL PRTBUF( STATUS )

*     Step backwards through Mean Spectrum to locate half-height point.
         J = NMIN
         DO WHILE ( SNET( J ) .LT. HHFLUX .AND. J .GE. STARTC )
            J = J - 1
         END DO
         IF ( J .LT. STARTC ) THEN
            CALL ERROUT( 'Incomplete line profile\\', STATUS )
            GO TO 999
         END IF
         HHWAVE( 1 ) = ( DBLE( J - 1 ) + ( SNET( J ) - HHFLUX ) /
     :                 ( SNET( J ) - SNET( J + 1 ) ) )

*     Step forwards through Mean Spectrum to locate half-height point.
         J = NMIN
         DO WHILE ( SNET( J ) .LT. HHFLUX .AND. J .LE. ENDC )
            J = J + 1
         END DO
         IF ( J .GT. ENDC ) THEN
            CALL ERROUT( 'Incomplete line profile\\', STATUS )
            GO TO 999
         END IF
         HHWAVE( 2 ) = ( DBLE( J - 2 ) + ( HHFLUX - SNET( J - 1 ) ) /
     :                 ( SNET( J ) - SNET( J - 1 ) ) )

*     Calculate line centre found (NET units).
         FCWAVE( I ) = WSCALE * ( HHWAVE( 1 ) + HHWAVE( 2 ) ) / 2.0 +
     :                 WAVAIR( 1 )

*     Show HH points.
         CALL LINE_WRITF( '%p   Half-height points: %.3f and \\',
     :                    WSCALE * HHWAVE( 1 ) + WAVAIR( 1 ) )
         CALL LINE_WRITF( '%.3fA.\\', WSCALE * HHWAVE( 2 ) +
     :                    WAVAIR( 1 ) )
         CALL PRTBUF( STATUS )
         CALL LINE_WRITF( '%p   Line centre: %.3fA.\\', FCWAVE( I ) )
         CALL PRTBUF( STATUS )

*     Calculate wavelength shift and ESHIFT.
         CALL LINE_WRITF( '%p   Wavelength shift: %.3fA.\\',
     :                    FCWAVE( I ) - CWAVE( I ) )
         CALL PRTBUF( STATUS )
         CALL LINE_WRITF( '%p   Relative ESHIFT: %.3f.\\',
     :                    ( CWAVE( I ) - FCWAVE( I ) ) *
     :                    DBLE( LINEORD( I ) ) )
         CALL PRTBUF( STATUS )
         CALL LINE_WCONT( '%p Done.' )
         CALL PRTBUF( STATUS )
      END DO

*  Display results.
      CALL LINE_WCONT( '%p \\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WCONT( '%p Results:\\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WCONT(
     :     '%p Line     Window  Order   Centre  Rel. ESHIFT\\' )
      CALL PRTBUF( STATUS )
      DO I = 1, NLINE
         CALL LINE_WRITF( '%p %7.2f  \\', CWAVE( I ) )
         CALL LINE_WRITF( '%6.2f  \\', DWAVE( I ) )
         CALL LINE_WRITI( '  %3i  \\', LINEORD( I ) )
         CALL LINE_WRITF( '%7.2f  \\', FCWAVE( I ) )
         FCWAVE( I ) = ( CWAVE( I ) - FCWAVE( I ) ) *
     :                 DBLE( LINEORD( I ) )
         CALL LINE_WRITF( '%6.2f\\', FCWAVE( I ) )
         CALL PRTBUF( STATUS )
      END DO

*  Find median ESHIFT.
      DO J = 1, NLINE - 1
         DO L = J + 1, NLINE
            IF ( FCWAVE( L ) .LT. FCWAVE( J ) ) THEN
               SWP = FCWAVE( L )
               FCWAVE( L ) = FCWAVE( J )
               FCWAVE( J ) = SWP
            END IF
         END DO
      END DO
      LOCALMED = FCWAVE( ( NLINE + 2 ) / 2 )
      CALL LINE_WCONT( '%p \\' )
      CALL PRTBUF( STATUS )

*  Set ESHIFT.
      IF ( LOCALMED .NE. ECOR( IAPER ) ) THEN
         ECOR( IAPER ) = ECOR( IAPER ) + LOCALMED
         NEWESH = .TRUE.
      END IF

      CALL LINE_WRITF( '%p ESHIFT set to %.3f.\\', ECOR( IAPER ) )
      CALL PRTBUF( STATUS )

*  Recalibrate if ESHIFT has changed.
      IF ( NEWESH ) THEN
         CALL MODCAL
         CALL RECAL( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: recalibrating spectrum\\', STATUS )
            GO TO 999
         END IF
      END IF

 999  CONTINUE

      END
