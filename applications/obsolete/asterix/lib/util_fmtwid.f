*+  UTIL_FMTWID - Width in characters of a restricted range of Fortran formats
      SUBROUTINE UTIL_FMTWID( FMT, WID, STATUS )
*
*    Description :
*
*     Finds the width in characters of a Fortran format. Formats processed
*     conform to,
*
*       format = <format_item> {, <format_item>}
*       format_item = <number>X
*                   = <number><core_format>
*                   = <number>P<core_format>
*       core_format = G<width>.<places>
*                   = F<width>.<places>
*                   = I<width> [.<places>]
*                   = A<width>
*
*    Method :
*     <description of how the subroutine works - for programmer info>
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     20 Aug 91 : Original (DJA)
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
      CHARACTER*(*)            FMT                ! Format to analyse
*
*    Export :
*
      INTEGER                  WID                ! Width
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER                  CHR_LEN

      LOGICAL                  CHR_ISDIG
*
*    Local variables :
*
      CHARACTER                C               ! A single element of FMT

      INTEGER                  BEG             ! Beginning of a number
      INTEGER                  FLEN            ! Logical length of FMT
      INTEGER                  I               ! Loop over input format
      INTEGER                  MULTIPLIER      !
      INTEGER                  NUM             ! A number in the field
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      I = 1
      WID = 0
      FLEN = CHR_LEN( FMT )

*    Loop over format sections
      DO WHILE ( I .LE. FLEN )

*      Skip whitespace
        DO WHILE ( ( I .LE. FLEN ) .AND. ( FMT(I:I) .LE. ' ' ) )
          I = I + 1
        END DO

*      Start with a digit?
        IF ( CHR_ISDIG( FMT(I:I) ) ) THEN

*        Scan to end of number
          BEG = I
          DO WHILE ( ( I .LE. FLEN ) .AND. CHR_ISDIG( FMT(I:I) ) )
            I = I + 1
          END DO
          IF ( I .GT. FLEN ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'End of format met', STATUS )
          ELSE
            CALL CHR_CTOI( FMT(BEG:I-1), NUM, STATUS )
          END IF
          IF ( STATUS .NE. SAI__OK ) GOTO 99

*        The trap for "X" formats
          IF ( ( FMT(I:I) .EQ. 'X' ) .OR. ( FMT(I:I) .EQ. 'x' ) ) THEN
            WID = WID + NUM
            GOTO 50

*        Traps P which doesn't effect field width
          ELSE IF ( ( FMT(I:I) .EQ. 'P' ) .OR.
     :                 ( FMT(I:I) .EQ. 'p' ) ) THEN
            MULTIPLIER = 1
            I = I + 1

*        Otherwise character is multiplier for a format
          ELSE
            MULTIPLIER = NUM

          END IF

        ELSE
          MULTIPLIER = 1

        END IF

*      Read a core format
        C = FMT(I:I)
        IF ( INDEX( 'AEFGIZHaefgizh', C ) .NE. 0 ) THEN

*        Next character must be digit
          I = I + 1
          IF ( CHR_ISDIG( FMT(I:I) ) ) THEN

*          This is the total field width
            BEG = I
            DO WHILE ( ( I .LE. FLEN ) .AND. CHR_ISDIG( FMT(I:I) ) )
              I = I + 1
            END DO
            CALL CHR_CTOI( FMT(BEG:I-1), NUM, STATUS )

*          Skip the possible decimal point and following number
            DO WHILE ( ( I .LE. FLEN ) .AND. ( FMT(I:I) .NE. ',' ) )
              I = I + 1
            END DO

*          Scale by the multiplier
            WID = WID + NUM * MULTIPLIER
            I = I - 1

          ELSE

            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FMT', FMT(I-1:) )
            CALL ERR_REP( ' ', 'Invalid format /^FMT/', STATUS )

          END IF

        ELSE
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'C', C )
          CALL ERR_REP( ' ', 'Invalid code /^C/ in format', STATUS )
        END IF

*      Next character
 50     I = I + 1

*      Skip comma
        IF ( I .LE. FLEN ) THEN
          IF ( FMT(I:I) .EQ. ',' ) THEN
            I = I + 1
          ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'C', FMT(I:I) )
            CALL ERR_REP( ' ', 'Invalid character in format /^C/',
     :                    STATUS )
          END IF
        END IF

      END DO

 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from UTIL_FMTWID', STATUS )
      END IF

      END
