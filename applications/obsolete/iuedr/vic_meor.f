      SUBROUTINE VIC_MEOR( IORDER, RECLEN, NREC, DATA, STATUS )
*+
*  Name:
*     SUBROUTINE VIC_MEOR
*
*  Description:
*     Translate IUESIPS order and store NET in CMSAVE.
*
*  History:
*     Jack Giddings      02-DEC-81     AT4 version
*     Paul Rees          07-NOV-88     IUEDR Vn. 2.0
*
*  Method:
*     Transfer the Wavelengths, Net and Data Quality from IUESIPS record
*     into CMSAVE.
*     The Swver value is used to determine how the data quality
*     and wavelength blocks should be interpretted.
*
*-

*  Implicit:
      IMPLICIT NONE

*  Import:
      INTEGER IORDER                   ! order index
      INTEGER RECLEN                   ! record size in short words
      INTEGER NREC                     ! number of records

      INTEGER*2 DATA( RECLEN, NREC )   ! data array

*  Export:
      INTEGER STATUS                   ! status return

*  Global variables:
      INCLUDE 'CMSAVE'
      INCLUDE 'CMUEZ1'
      INCLUDE 'CMSPC'

*  Local variables:
      REAL*8 WSTEP                       ! even wavelength step

      INTEGER DQ                       ! IUESIPS data quality value
      INTEGER I                        ! loop index
      INTEGER IQ                       ! temporary data quality
      INTEGER OFFSET                   ! offset record for order
      INTEGER QCODE                    ! IUE data quality value

*  Record offset.
      OFFSET = ( IORDER - 1 ) * NRECRD + 1

*  Zero out data quality.
      DO I = 1, NWAVS( IORDER )
         QNETS( I, IORDER ) = 0
      END DO

*  Data Quality.
      IF ( SWVER .EQ. 1 ) THEN
         DO I = 1, NWAVS( IORDER )
            DQ = DATA( I + 2, OFFSET + 2 )
            IF ( DQ .LT. -800 ) THEN
               QNETS( I, IORDER ) = 1

            ELSE IF ( DQ .LT. 0 ) THEN
               QNETS( I, IORDER ) = 5

            ELSE
               QNETS( I, IORDER ) = 0
            END IF
         END DO

      ELSE IF ( SWVER .EQ. 2 ) THEN
         DO I = 1, NWAVS( IORDER )
            DQ = DATA( I + 2, OFFSET + 2 )
            IF ( DQ .EQ. 100 ) THEN
               QNETS( I, IORDER ) = 0

            ELSE IF ( DQ .EQ. -200 ) THEN
               QNETS( I, IORDER ) = 1

            ELSE IF ( DQ .EQ. -220 ) THEN
               QNETS( I, IORDER ) = 2

            ELSE IF ( DQ .EQ. -250 ) THEN
               QNETS( I, IORDER ) = 3

            ELSE IF ( DQ .EQ. -300 ) THEN
               QNETS( I, IORDER ) = 4

            ELSE IF ( DQ .EQ. -800 ) THEN
               QNETS( I, IORDER ) = 5

            ELSE IF ( DQ .EQ. -1600 ) THEN
               QNETS( I, IORDER ) = 7

            ELSE
               QNETS( I, IORDER ) = 8
            END IF
         END DO
      END IF

*  Translate Qgross data quality values into patterns.
      DO I = 1, NWAVS( IORDER )
         QCODE = QNETS( I, IORDER )
         IQ = 0
         QNETS( I, IORDER ) = 0
         IF ( QCODE .LT. 8 ) THEN
            CALL DQ_WRPK( QCODE, 4, 3, IQ )
            IF ( QCODE .GE. 3 ) THEN
               CALL DQ_WRPK( 1, 2, 1, IQ )
            END IF

         ELSE
            CALL DQ_WRPK( 1, 1, 1, IQ )
         END IF

         CALL DQ_ITOU( IQ, QNETS( I, IORDER ) )
      END DO

*  Wavelength block.
      DO I = 1, NWAVS( IORDER )
         WAVS( I, IORDER ) = DBLE( DATA ( I + 2, OFFSET + 1 ) ) *
     :                       DATSCL( 1 ) + WOFFS( IORDER )
      END DO

*  Convert wavelengths to vacuum (assumed air).
      CALL IUE_TVAC( NWAVS( IORDER ), WAVS( 1, IORDER ) )

*  Even out wavelength grid for new software.
      IF ( SWVER .EQ. 2 ) THEN
         WSTEP = ( WAVS( NWAVS( IORDER ), IORDER ) - WAVS( 1, IORDER ) )
     :           / ( NWAVS( IORDER ) - 1 )

         DO I = 2, NWAVS( IORDER ) - 1
            WAVS( I, IORDER ) = DBLE( I - 1 ) * WSTEP +
     :                          WAVS( 1, IORDER )
         END DO
      END IF

*  Net.
      DO I = 1, NWAVS( IORDER )
         SNETS( I, IORDER ) = DBLE( DATA( I + 2, OFFSET + 5 ) ) *
     :                        DATSCL( 5 )
      END DO

*  Global wavelength limits.
      WAV1S( IORDER ) = WAVS( 1, IORDER )
      WAV2S( IORDER ) = WAVS( NWAVS( IORDER ), IORDER )
      STATUS = 0

      END
