      SUBROUTINE USR_BARKER( STATUS )
*+
*  Name:
*     SUBROUTINE USR_BARKER

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_BARKER( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine takes the available spectra for adjacent orders and
*     uses the intensities in their regions  of overlap to fit ripple
*     parameters.

*  Method:
*     This is acts as a controlling interface to the actual implementation.
*     It makes the DATASET available, checks that there ARE some orders,
*     and then passes the data for each pair of adjacent orders to the
*     fitting routine. For orders M and M+1, get parameters appropriate
*     for order M+0.5. When all order pairs have been fitted, a global
*     smoothing is performed, and the actual dataset updated (CMRIP
*     Common Block). The Dataset Calibration file (UEC) is also marked
*     as requiring update.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-APR-85 (JRG):
*       IUEDR Vn. 1.3
*     03-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     27-JUL-94 (MJC):
*       IUEDR Vn. 3.1-2
*     18-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Local Constants:
      INTEGER ERR          ! Error status.
      INTEGER HORD         ! Highest order.
      INTEGER HIRES        ! HIRES index.
      INTEGER LORD         ! Lowest order.
      INTEGER MAXWAV       ! Maximum length of wavelength arrays.
      INTEGER SWP          ! SWP index.
      PARAMETER ( ERR = -3, HORD = 125, HIRES = 2, LORD = 65,
     :           MAXWAV = 1200, SWP = 3 )

*  Status:
      INTEGER STATUS       ! Global status.

*  External References:
      REAL*8 MSC_EVPOLY    ! Evaluate polynomial.

*  Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMSAVE'
      INCLUDE 'CMRIP'
      INCLUDE 'CMCUT'

*  Local Variables:
      REAL*8 KM( LORD:HORD - 1 ) ! Interorder Ripple Ks between ORD:ORD+1.
      REAL*8 KS( LORD:HORD)      ! Ks for actual orders.
      REAL*8 WAV1( MAXWAV )      ! Wavelengths for low order.
      REAL*8 WAV2( MAXWAV )      ! Wavelengths for high order.
      REAL*8 WCUT1( 2 )          ! Cutoff wavelengths for low order.
      REAL*8 WCUT2( 2 )          ! Cutoff wavelengths for high order.

      INTEGER ACTVAL       ! Parameter value count.
      INTEGER I            ! Wavelength index.
      INTEGER ICAM         ! Camera index.
      INTEGER IORDC        ! General cutoff order index.
      INTEGER IORDR        ! Ripple index for order.
      INTEGER IORD1        ! Low order index.
      INTEGER IORD2        ! High order index.
      INTEGER IRES         ! Resolution index.
      INTEGER ORDS( 2 )    ! Pair of orders defining range.
      INTEGER ORD1         ! Low echelle  order in pair.
      INTEGER ORD2         ! High echelle order in pair.
      INTEGER Q1( MAXWAV ) ! Quality of low order net.
      INTEGER Q2( MAXWAV ) ! Quality of high order net.

      LOGICAL NOKM( LORD:HORD - 1 )  ! Whether Barker has defined KM.
      LOGICAL NOKS( LORD:HORD )      ! Whether actual order Ks defined.
      LOGICAL BRKOK                  ! Whether MERC or WOOF have worked.
      LOGICAL TRAKED( LORD:HORD )    ! Whether order traked.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get Calibration and Spectrum (raw).
      CALL DASSOC( 'S\\', 'T\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999
      END IF

*  Check Resolution.
      CALL IUE_RESN( RESOL, IRES, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: invalid resolution\\', STATUS )
         GO TO 999

      ELSE IF ( IRES .NE. HIRES ) THEN
         CALL ERROUT( 'Error: only works for HIRES\\', STATUS )
         GO TO 999
      END IF

*  Check Camera.
      CALL IUE_CAMN( CAMERA, ICAM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: invalid camera\\', STATUS )
         GO TO 999

      ELSE IF ( ICAM .NE. SWP ) THEN
         CALL ERROUT( 'Error: only works for SWP\\', STATUS )
         GO TO 999
      END IF

*  Check that there are SOME orders in spectrum.
      IF ( NOSPEC .OR. NORDER.LE.0 ) THEN
         CALL ERROUT( 'Error: no orders defined\\', STATUS )
         GO TO 999
      END IF

*  ORDERS.
      CALL GET_ORDERS( .TRUE., ORDS, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

*  TRAKed orders.
      DO ORD1 = LORD, HORD
         CALL FNORD( ORD1, IORD1 )
         TRAKED( ORD1 ) = ( IORD1 .GT. 0 )
      END DO

*  Initially, none of RIPKB is defined.
      DO ORD1 = LORD, HORD - 1
         NOKM( ORD1 ) = .TRUE.
      END DO

*  Go through looking for adjacent orders.
      DO ORD1 = ORDS( 1 ), ORDS( 2 ) - 1
         ORD2 = ORD1 + 1
         CALL FNORD( ORD1, IORD1 )
         CALL FNORD( ORD2, IORD2 )

*     ORD1 and ORD2 both exist - BARKER is possible.

         IF ( IORD1.GT.0 .AND. IORD2.GT.0 ) THEN

*        Get calibrated AIR wavelengths for ORD1.
            DO I = 1, NWAVS( IORD1 )
               WAV1( I ) = WAVS( I, IORD1 )
               CALL DQ_UTOI( QNETS( I, IORD1 ), Q1( I ) )
            END DO

            CALL CAWAV( 1, ORD1, NWAVS( IORD1 ), WAV1 )

*        Get calibrated AIR wavelengths for ORD2.
            DO I = 1, NWAVS( IORD2 )
               WAV2( I ) = WAVS( I, IORD2 )
               CALL DQ_UTOI( QNETS( I, IORD2 ), Q2( I ) )
            END DO

            CALL CAWAV( 1, ORD2, NWAVS( IORD2 ), WAV2 )

*        Global initial value for K (between ORD1 and ORD2).
            KM( ORD1 ) = MSC_EVPOLY( NRIPM, RIPM, DBLE( ORD1 ) + 0.5d0 )

*        Cutoff AIR wavelengths for ORD1.
            CALL FNCUT( ORD1, IORDC )
            IF ( IORDC .GT. 0 ) THEN
               WCUT1( 1 ) = CUTW1( IORDC )
               WCUT1( 2 ) = CUTW2( IORDC )

            ELSE
               WCUT1( 1 ) = WAV1( 1 )
               WCUT1( 2 ) = WAV1( NWAVS( IORD1 ) )
            END IF

*        Cutoff AIR wavelengths for ORD2.
            CALL FNCUT( ORD2, IORDC )
            IF ( IORDC .GT. 0 ) THEN
               WCUT2( 1 ) = CUTW1( IORDC )
               WCUT2( 2 ) = CUTW2( IORDC )

            ELSE
               WCUT2( 1 ) = WAV2( 1 )
               WCUT2( 2 ) = WAV2( NWAVS( IORD2 ) )
            END IF

*        Invoke Barker's method.
            CALL BRK_WOOF( RIPALF, ORD1, NWAVS( IORD1 ), WAV1,
     :                     SNETS( 1, IORD1 ), Q1, WCUT1, ORD2,
     :                     NWAVS( IORD2 ), WAV2, SNETS( 1, IORD2 ),
     :                     Q2, WCUT2, KM( ORD1 ), BRKOK )
            NOKM( ORD1 ) = ( .NOT. BRKOK )
         END IF
      END DO

*  Smooth results.
      CALL BRK_MERC( ORDS, LORD, HORD, TRAKED, KM, NOKM, KS, NOKS,
     :               BRKOK )
      IF ( .NOT. BRKOK ) THEN
         CALL ERROUT( 'Error: MERC failed\\', STATUS )

      ELSE

*     Update CMRIP.
         DO ORD1 = LORD, HORD
            IF ( .NOT. NOKS( ORD1 ) ) THEN
               CALL FNRIP( ORD1, IORDR )

               IF ( IORDR .GT. 0 ) THEN
                  RIPKS( IORDR ) = KS( ORD1 )
                  RIPAS( IORDR ) = RIPALF
                  NRIPCS( IORDR ) = 1
                  RIPCS( 1, IORDR ) = 1.0

               ELSE
                  CALL ALRIP( ORD1, IORDR )
                  IF ( IORDR .LE. 0 ) THEN
                     CALL LINE_WRITI(
     :                    '%p Warning : no room for order %i\\', ORD1 )
                     CALL LINE_WCONT( ' ripple parameters\\' )
                     CALL PRTBUF( STATUS )

                  ELSE
                     RIPKS( IORDR ) = KS( ORD1 )
                     RIPAS( IORDR ) = RIPALF
                  END IF
               END IF
            END IF
         END DO

*     Update any current spectral order.
         CALL RECAL( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Warning: failed updating current order\\',
     :                   STATUS )
            STATUS = SAI__OK
         END IF

*     Mark Calibration File for update.
         CALL MODCAL
      END IF

 999  CONTINUE

      END
