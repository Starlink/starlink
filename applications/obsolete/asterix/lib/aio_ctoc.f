*+  AIO_CTOC - Convert character element of array to character string
      SUBROUTINE AIO_CTOC( S1, S2, S3, S4, S5, S6, S7, DATA,
     :                          INDICES, FMT, STR, STATUS )
*
*    Description :
*
*     "Converts" a character string to a character string.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      3 Jan 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER               S1,S2,S3,S4,S5,S6,S7       ! Array dimensions
      CHARACTER*(*)         DATA(S1,S2,S3,S4,S5,S6,S7) !
      INTEGER               INDICES(DAT__MXDIM)        ! Element to convert
      CHARACTER*(*)         FMT                        ! Conversion format
*
*    Export :
*
      CHARACTER*(*)         STR                        ! Output datum
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER               FLEN                       ! # chars in format
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN

*       Get format length from format string
         CALL CHR_CTOI( FMT(3:(INDEX(FMT,')')-1)), FLEN, STATUS )

*       Transfer datum
         STR = DATA(INDICES(1), INDICES(2), INDICES(3), INDICES(4),
     :     INDICES(5), INDICES(6), INDICES(7))(:MIN(LEN(STR),FLEN))

      END IF

      END



*+  AIO_ITOC - Convert integer element of array to character string
      SUBROUTINE AIO_ITOC( S1, S2, S3, S4, S5, S6, S7, DATA,
     :                          INDICES, FMT, STR, STATUS )
*
*    Description :
*
*     Converts an integer value to a character string.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      3 Jan 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER               S1,S2,S3,S4,S5,S6,S7       ! Array dimensions
      INTEGER               DATA(S1,S2,S3,S4,S5,S6,S7) !
      INTEGER               INDICES(DAT__MXDIM)        ! Element to convert
      CHARACTER*(*)         FMT                        ! Conversion format
*
*    Export :
*
      CHARACTER*(*)         STR                        ! Output datum
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER               DATUM                      ! Value
      INTEGER               FSTAT                      ! i/o status
      INTEGER               IBIT,JBIT,NBIT,BIT         ! Binary conversion
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get datum
      DATUM = DATA(INDICES(1), INDICES(2), INDICES(3), INDICES(4),
     :                         INDICES(5), INDICES(6), INDICES(7))

*    Choose format
      IF ( FMT(1:2) .NE. '(B' ) THEN
         WRITE( STR, FMT, IOSTAT=FSTAT ) DATUM

      ELSE

*       Binary conversion - fill string with 0 bits
         CALL CHR_CTOI( FMT(3:(INDEX(FMT,')')-1)), NBIT, STATUS )
         CALL CHR_FILL( '0', STR(:NBIT) )

*       Scan number bit by bit ( HA HA )
         IBIT = 0
         DO WHILE ( IBIT .LE. (NBIT - 1 ) )
            IF ( IBIT .LT. (NBIT-1) ) THEN
               BIT=2**IBIT
            ELSE
              IF ( NBIT .EQ. 8 ) THEN
                  BIT = -128
              ELSE IF ( NBIT .EQ. 16 ) THEN
                  BIT = -32768
              ELSE
                  BIT = -32768*256
              END IF

            END IF
            JBIT = NBIT - IBIT
            IF ((DATUM.AND.BIT)/BIT) THEN
               STR(JBIT:JBIT)='1'
            END IF
            IBIT = IBIT + 1
         END DO

         STR = ' '//STR(:NBIT)

      END IF

      END



*+  AIO_RTOC - Convert real element of array to character string
      SUBROUTINE AIO_RTOC( S1, S2, S3, S4, S5, S6, S7, DATA,
     :                          INDICES, FMT, STR, STATUS )
*
*    Description :
*
*     Converts a real value to a character string.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      3 Jan 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER               S1,S2,S3,S4,S5,S6,S7       ! Array dimensions
      REAL                  DATA(S1,S2,S3,S4,S5,S6,S7) !
      INTEGER               INDICES(DAT__MXDIM)        ! Element to convert
      CHARACTER*(*)         FMT                        ! Conversion format
*
*    Export :
*
      CHARACTER*(*)         STR                        ! Output datum
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER               FSTAT                      ! i/o status
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN

*       Write datum
         WRITE( STR, FMT, IOSTAT=FSTAT ) DATA(INDICES(1), INDICES(2),
     :       INDICES(3), INDICES(4),INDICES(5), INDICES(6), INDICES(7))

      END IF

      END



*+  AIO_DTOC - Convert double precision element of array to character string
      SUBROUTINE AIO_DTOC( S1, S2, S3, S4, S5, S6, S7, DATA,
     :                          INDICES, FMT, STR, STATUS )
*
*    Description :
*
*     Converts a double precision value to a character string.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      3 Jan 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER               S1,S2,S3,S4,S5,S6,S7       ! Array dimensions
      DOUBLE PRECISION      DATA(S1,S2,S3,S4,S5,S6,S7) !
      INTEGER               INDICES(DAT__MXDIM)        ! Element to convert
      CHARACTER*(*)         FMT                        ! Conversion format
*
*    Export :
*
      CHARACTER*(*)         STR                        ! Output datum
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER               FSTAT                      ! i/o status
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN

*       Write datum
         WRITE( STR, FMT, IOSTAT=FSTAT ) DATA(INDICES(1), INDICES(2),
     :       INDICES(3), INDICES(4),INDICES(5), INDICES(6), INDICES(7))

      END IF

      END



*+  AIO_LTOC - Convert logical element of array to character string
      SUBROUTINE AIO_LTOC( S1, S2, S3, S4, S5, S6, S7, DATA,
     :                          INDICES, FMT, STR, STATUS )
*
*    Description :
*
*     Converts a logical value to a character string.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      3 Jan 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER               S1,S2,S3,S4,S5,S6,S7       ! Array dimensions
      LOGICAL               DATA(S1,S2,S3,S4,S5,S6,S7) !
      INTEGER               INDICES(DAT__MXDIM)        ! Element to convert
      CHARACTER*(*)         FMT                        ! Conversion format
*
*    Export :
*
      CHARACTER*(*)         STR                        ! Output datum
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER               FSTAT                      ! i/o status
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN

*       Write datum
         WRITE( STR, FMT, IOSTAT=FSTAT ) DATA(INDICES(1), INDICES(2),
     :       INDICES(3), INDICES(4),INDICES(5), INDICES(6), INDICES(7))

      END IF

      END
