      SUBROUTINE RDMAP( FD, STATUS )
*+
*  Name:
*     SUBROUTINE RDMAP
*
*  Description:
*     This reads the mapped spectrum part of the dataset from the file
*     described by FD.
*
*  History:
*     Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*     Paul Rees          22-SEP-88     IUEDR Vn. 2.0
*     Martin Clayton     29-SEP-94     IUEDR Vn. 3.1-5
*       Corrected OLDDATA logic error. SAEised

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  CMCOMB:
      INCLUDE 'CMCOMB'

*  Local Constants:
      INTEGER MAXPOINT   ! Maximum number of points in spectrum.
      PARAMETER ( MAXPOINT = 27800 )

*  Arguments Given:
      INTEGER FD         ! File dscriptor.

*  Status:
      INTEGER STATUS     ! Global status.

*  Local variables:
      REAL RYCOMB( MAXPOINT ) ! Fluxes.
      REAL RWCOMB( MAXPOINT ) ! Folding weights.
      REAL RXCOMB1       ! Start wavelength.
      REAL RDXCOMB       ! Wavelength step.

      BYTE VNAME( 16 )   ! Item name.
      BYTE VTYPE( 16 )   ! Item type.

      INTEGER I          ! Loop index.

      LOGICAL OLDDATA
      COMMON / CMVERSION / OLDDATA
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL RDPART( FD, VNAME, NOCOMB, VTYPE, STATUS )
      IF ( STATUS .NE. SAI__OK) THEN
         NOCOMB = .TRUE.
         RETURN

      ELSE IF ( .NOT. NOCOMB ) THEN
         NOCOMB = .TRUE.
         IF ( OLDDATA ) THEN
            READ( FD, IOSTAT=STATUS ) NCOMB, RXCOMB1, RDXCOMB, XMLAB,
     :                                XMUN, YMLAB, YMUN, MTITLE
            XCOMB1 = DBLE(RXCOMB1)
            DXCOMB = DBLE(RDXCOMB)

         ELSE
            READ( FD, IOSTAT=STATUS ) NCOMB, XCOMB1, DXCOMB, XMLAB,
     :                                XMUN, YMLAB, YMUN, MTITLE
         END IF
         IF ( STATUS .NE. SAI__OK) THEN
            CALL ERROUT( 'Error: reading NCOMB\\', STATUS )
            RETURN

         ELSE IF ( NCOMB .GT. MAXPOINT ) THEN
            CALL ERROUT( 'Error: spectrum too long\\', STATUS )

         ELSE IF ( NCOMB .GT. 0 ) THEN
            IF ( .NOT. OLDDATA ) THEN
               READ( FD, IOSTAT=STATUS ) ( YCOMB(I), I = 1, NCOMB),
     :                                    (WCOMB(I), I = 1, NCOMB),
     :                                    (QCOMB(I), I = 1, NCOMB )
            ELSE
               READ( FD, IOSTAT=STATUS ) ( RYCOMB(I), I = 1, NCOMB),
     :                                    (RWCOMB(I), I = 1, NCOMB),
     :                                    (QCOMB(I), I = 1, NCOMB )
               DO I = 1, NCOMB
                 YCOMB(I) = DBLE(RYCOMB(I))
                 WCOMB(I) = DBLE(RWCOMB(I))
               END DO
            END IF

            IF ( STATUS .NE. SAI__OK) THEN
               CALL ERROUT( 'Error: reading spectrum\\', STATUS )
               RETURN
            END IF

            DO I = 1, NCOMB
               XCOMB(I) = DBLE(I - 1) * DXCOMB + XCOMB1
            END DO

            NOCOMB = .FALSE.

         END IF
      END IF
      END
