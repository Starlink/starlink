      SUBROUTINE LOCOMB( STATUS )
*+
*  Name:
*     SUBROUTINE LOCOMB

*  Purpose:
*     Combine LORES aperture spectrum.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOCOMB( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     31-DEC-81 (JRG):
*       IUEDR Vn. 1.0
*     04-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     30-SEP-94 (MJC):
*       IUEDR Vn. 3.1-6
*     13-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMCOMB'
      INCLUDE 'CMSAVE'
      INCLUDE 'CMSPEC'
      INCLUDE 'CMWAV'
      INCLUDE 'CMFLX'
      INCLUDE 'CMTEMP'

*  Local Constants:
      INTEGER MAXLABEL   ! Maximum length of plot label.
      INTEGER MAXPOINT   ! Maximum number of points plotted.
      INTEGER MAXTITLE   ! Maximum length of plot title.
      PARAMETER ( MAXLABEL = 40, MAXPOINT = 27800, MAXTITLE = 80 )

*  Status:
      INTEGER STATUS        ! Global status.

*  External References:
      INTEGER DQ_AND        ! 32 bit integer AND operation.

*  Local Variables:
      REAL*8 XLIM( 2 )      ! X-grid wavelength limits.
      REAL*8 XSTEP          ! Global grid step.

      LOGICAL COVER         ! Whether gaps covered by other orders.
      LOGICAL FILL          ! Whether gaps filled within order.
      LOGICAL RESET         ! Whether existing grid is reset.

      INTEGER ACTVAL        ! Parameter value count.
      INTEGER I             ! Loop index.
      INTEGER INDS( 2 )     ! Indices of grid changed by MASPEC.
      INTEGER QT( MAXPOINT ) ! Temporary Q.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access LORES spectrum.
      CALL RDLO( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: accessing spectrum\\', STATUS )
         GO TO 999
      END IF

*  RM parameter.
      CALL RDPARL( 'RM\\', .FALSE., 1, RESET, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'RM\\', STATUS )
         GO TO 999

      ELSE IF ( RESET ) THEN
         CALL LINE_WCONT( '%p New Spectrum.\\' )
         CALL PRTBUF( STATUS )
         NOCOMB = .TRUE.

      ELSE IF ( .NOT. NOCOMB ) THEN
         CALL LINE_WCONT( '%p Will combine with existing spectrum.\\' )
         CALL PRTBUF( STATUS )
         DO I = 1, NCOMB
            IF ( QCOMB(I) .EQ. 0 ) YCOMB(I) = YCOMB(I) * WCOMB(I)
         END DO
      END IF

*  Print aperture to be mapped.
      CALL LINE_WRITS( '%p Using %s aperture.\\', APERS(1, ORDER) )
      CALL PRTBUF( STATUS )

*  Construct grid if none already defined.
      IF ( NOCOMB ) THEN

*     ML parameter.
         DO WHILE ( .TRUE. )
            XSTEP = (WAVAIR(NWAV) - WAVAIR(1)) / DBLE(NWAV - 1)
            XLIM(1) = WAVAIR(1)
            XLIM(2) = WAVAIR(NWAV)
            CALL RDPARF( 'ML\\', .TRUE., 2, XLIM, ACTVAL, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PARFER( 'ML\\', STATUS )
               GO TO 999

            ELSE IF ( ACTVAL .LT. 2 ) THEN
               CALL ERRPAR( 'ML\\' )
               CALL ERROUT( ': too few values specified\\', STATUS )

            ELSE IF ( XLIM(1) .EQ. XLIM(2) ) THEN
               CALL ERRPAR( 'ML\\' )
               CALL ERROUT( ': must be specified\\', STATUS )

            ELSE
               IF ( XLIM(1) .GT. XLIM(2) )
     :             CALL MSC_DSWAP( XLIM(1), XLIM(2) )

*           Sample rate from orders contained in range.
               XSTEP = (WAVAIR(NWAV) - WAVAIR(1)) / DBLE(NWAV - 1)
               GO TO 100
            END IF

            CALL CNPAR( 'ML\\', STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PCANER( 'ML\\', STATUS )
               GO TO 999
            END IF
         END DO
 100     CONTINUE

*     MSAMP parameter.
         DO WHILE ( .TRUE. )
            CALL RDPARF('MSAMP\\', .TRUE., 1, XSTEP, ACTVAL, STATUS)
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PARFER( 'MSAMP\\', STATUS )
               GO TO 999

            ELSE IF ( XSTEP .GE. (XLIM(2) - XLIM(1)) ) THEN
               CALL ERROUT( 'Error: grid spacing too large\\', STATUS )

            ELSE IF ( XSTEP .EQ. 0.0 ) THEN
               CALL ERROUT( 'Error: grid spacing too small\\', STATUS )

            ELSE
               NCOMB = (XLIM(2) - XLIM(1)) / XSTEP + 1.0
               IF ( NCOMB .LT. 2 ) THEN
                  CALL ERROUT( 'Error: too few points in grid\\',
     :                         STATUS )

               ELSE IF ( NCOMB .GT. MAXPOINT ) THEN
                  CALL ERROUT( 'Error: too many points in grid\\',
     :                         STATUS )

               ELSE
                  GO TO 200
               END IF
            END IF
         END DO
 200     CONTINUE

*     Fill in missing details of grid.
         XCOMB1 = XLIM( 1 )
         DXCOMB = XSTEP

         DO I = 1, NCOMB
            XCOMB( I ) = DBLE( I - 1 ) * DXCOMB + XCOMB1
            YCOMB( I ) = 0.0
            WCOMB( I ) = 0.0
            QCOMB( I ) = 1
         END DO
      END IF

*  Print actual grid.
      CALL LINE_WRITF( '%p Wavelength grid is (%.3f,\\', XCOMB1 )
      CALL LINE_WRITF( '%.3f,\\', REAL(NCOMB - 1) * DXCOMB + XCOMB1 )
      CALL LINE_WRITF( '%.3f).\\', DXCOMB )
      CALL PRTBUF( STATUS )

*  Enlarge limits to include border.
      XLIM( 1 ) = XCOMB1 - DXCOMB
      XLIM( 2 ) = DBLE( NCOMB ) * DXCOMB + XCOMB1

*  Set combined spectrum undefined during change.
      NOCOMB = .TRUE.

*  FILLGAP parameter.
      CALL RDPARL( 'FILLGAP\\', .FALSE., 1, FILL, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'FILLGAP\\', STATUS )
         GO TO 999
      END IF

*  COVERGAP parameter.
      CALL RDPARL( 'COVERGAP\\', .FALSE., 1, COVER, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'COVERGAP\\', STATUS )
         GO TO 999
      END IF

*  Give account of how filling will take place.
      IF ( FILL .AND. COVER ) THEN
         CALL LINE_WCONT(
     :       '%p Gaps can be filled by adjacent points/spectra.\\' )

      ELSE IF ( FILL ) THEN
         CALL LINE_WCONT(
     :       '%p Gaps can be filled by adjacent points.\\' )

      ELSE IF (COVER) THEN
         CALL LINE_WCONT(
     :       '%p Gaps can be filled by adjacent spectra.\\' )

      ELSE
         CALL LINE_WCONT( '%p Gaps are not filled.\\' )
      END IF

      CALL PRTBUF( STATUS )

*  Map spectrum.
      IF ( .NOT.(WAVAIR(NWAV).LT.XLIM(1) .OR.
     :                  WAVAIR(1).GT.XLIM(2)) ) THEN
         DO I = 1, NCOMB
            TMPWRK2( I ) = 0.0
            TMPWRK1( I ) = 0.0
            QT( I ) = 1
         END DO

         CALL MASPEC( FILL, 3, NCOMB, XCOMB, TMPWRK2, TMPWRK1, QT,
     :                INDS )

         DO I = INDS( 1 ), INDS( 2 )
            IF ( QT(I).EQ.0 .AND. QCOMB(I).NE.3 ) THEN
               YCOMB( I ) = YCOMB( I ) + TMPWRK2( I )
               WCOMB( I ) = WCOMB( I ) + TMPWRK1( I )
               QCOMB( I ) = 0
            END IF
         END DO

         IF ( .NOT. COVER ) THEN
            DO I = INDS( 1 ), INDS( 2 )
               IF ( QT( I ) .NE. 0 ) THEN
                  YCOMB( I ) = 0.0
                  WCOMB( I ) = 0.0
                  QCOMB( I ) = 3
               END IF
            END DO
         END IF
      END IF

*  Normalise spectrum.
      DO I = 1, NCOMB
         IF ( DQ_AND( QCOMB( I ), 1 ).EQ.0 .AND.
     :        WCOMB( I ).GT.0.0 ) THEN
            YCOMB( I ) = YCOMB( I ) / WCOMB( I )

         ELSE IF ( QCOMB( I ) .NE. 3 ) THEN
            YCOMB( I ) = 0.0
            WCOMB( I ) = 0.0
            QCOMB( I ) = 1
         END IF
      END DO

*  Some labels.
      CALL STR_MOVE( TITLE, MAXTITLE, MTITLE )
      CALL STR_MOVE( WLABEL, MAXLABEL, XMLAB )
      CALL STR_MOVE( WUNITS, MAXLABEL, XMUN )
      CALL STR_MOVE( FLABEL, MAXLABEL, YMLAB )
      CALL STR_MOVE( FUNITS, MAXLABEL, YMUN )

*  Set the combined spectrum defined.
      NOCOMB = .FALSE.
      CALL MODMAP

 999  CONTINUE

      END
