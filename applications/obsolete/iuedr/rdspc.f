      SUBROUTINE RDSPC( FD, STATUS )

*+
*
*   Name:
*      SUBROUTINE RDSPC
*
*   Description:
*      This reads the spectrum part of the dataset from the file
*      described by FD.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          22-SEP-88     IUEDR Vn. 2.0
*      Martin Clayton     29-SEP-94     IUEDR Vn. 3.1-5
*        Fixed old-style data read buffer size error.
*
*   Method:
*      Use Fortran I/O.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Import:
      INTEGER FD         ! file dscriptor

*   Export:
      INTEGER STATUS     ! status return

*   CMSAVE:
      INCLUDE 'CMSAVE'

*   Local variables:
      REAL RWAVS(1200)
      REAL RSNETS(1200)

      BYTE VNAME(16)     ! name
      BYTE VTYPE(16)     ! type

      INTEGER I          ! loop index
      INTEGER IORDER     ! order index

      LOGICAL OLDDATA
      COMMON / CMVERSION / OLDDATA

*   Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL RDPART( FD, VNAME, NOSPEC, VTYPE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         NOSPEC = .TRUE.
         RETURN

      ELSE IF ( .NOT. NOSPEC ) THEN
         NOSPEC = .TRUE.
         READ( FD, IOSTAT=STATUS ) NORDER
         IF ( STATUS .NE. SAI__OK) THEN
            CALL ERROUT( 'Error: reading NORDER\\', STATUS )
            RETURN
         ELSE IF ( NORDER .GT. 100 ) THEN
            CALL ERROUT( 'Error: too many orders\\', STATUS )
         ELSE IF ( NORDER .GT. 0 ) THEN
            READ( FD, IOSTAT=STATUS ) ( ORDERS(I), NWAVS(I), I = 1,
     :                                 NORDER )
            IF ( STATUS .NE. SAI__OK) THEN
               CALL ERROUT( 'Error: reading order information\\',
     :                      STATUS )
               RETURN
            END IF

            DO IORDER = 1, NORDER
               IF ( NWAVS(IORDER) .GT. 1200 ) THEN
                  CALL ERRSTR( 'Error: order \\' )
                  CALL ERRINT( ORDERS(IORDER) )
                  CALL ERROUT( ' is too long\\', STATUS )
                  RETURN

               ELSE IF ( NWAVS(IORDER) .GT. 0 ) THEN
                  IF ( .NOT. OLDDATA ) THEN
                     READ( FD, IOSTAT=STATUS )
     :                  ( WAVS(I, IORDER), I = 1, NWAVS(IORDER)),
     :                  (SNETS(I, IORDER), I = 1, NWAVS(IORDER)),
     :                  (QNETS(I, IORDER), I = 1, NWAVS(IORDER) )

                  ELSE
                     READ( FD, IOSTAT=STATUS )
     :                  ( RWAVS(I), I = 1, NWAVS(IORDER)),
     :                  (RSNETS(I), I = 1, NWAVS(IORDER)),
     :                  (QNETS(I,IORDER), I = 1, NWAVS(IORDER) )
                     DO I = 1, NWAVS(IORDER)
                        WAVS(I, IORDER) = DBLE(RWAVS(I))
                        SNETS(I, IORDER) = DBLE(RSNETS(I))
                     END DO
                  END IF

                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERRSTR( 'Error: reading order data \\' )
                     CALL ERRINT( ORDERS(IORDER) )
                     CALL ERROUT( '\\', STATUS )
                     RETURN
                  END IF

                  WAV1S(IORDER) = WAVS(1, IORDER)
                  WAV2S(IORDER) = WAVS(NWAVS(IORDER), IORDER)
               END IF
            END DO
            NOSPEC = .FALSE.
         END IF
      END IF
      END
