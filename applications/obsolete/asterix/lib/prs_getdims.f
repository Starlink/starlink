*+  PRS_GETDIMS - Get dimensions from a text string
      SUBROUTINE PRS_GETDIMS( TEXT, MAXDIM, NDIM, DIMS, STATUS )
*
*    Description :
*
*     Parses a text string to extract a dimension specification. The string
*     may optionally start with a "[" or "(", in which case the last non-
*     blank character must be "]" or ")" respectively. This routine can be
*     used for parsing either a dimensions specification or a single element
*     index. Numbers must be separated by either spaces or commas&spaces.
*
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      4 Dec 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      CHARACTER*(*)            TEXT               ! Input string
      INTEGER                  MAXDIM             ! Max number of dimensions
*
*    Export :
*
      INTEGER                  NDIM               ! Dimensionality
      INTEGER                  DIMS(*)            ! Dimensions
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      LOGICAL                  CHR_ISDIG
*
*    Local constants :
*
      INTEGER                  NBRAK              ! # valid delimiters
        PARAMETER              ( NBRAK = 2 )
*
*    Local variables :
*
      INTEGER                  BEG                ! Beginning of number
      INTEGER                  IBRAK              ! Kind of delimiter
      INTEGER                  IC                 ! Loop over TEXT

      LOGICAL                  MORE               ! More numbers to read
*
*    Local data :
*
      CHARACTER*(NBRAK)        LHS, RHS
      DATA                     LHS,RHS/'[(','])'/
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      NDIM = 0

*    Skip white space
      IC = 1
      CALL CHR_FIWS( TEXT, IC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Delimiter present
      IBRAK = INDEX( LHS, TEXT(IC:IC) )
      IF ( IBRAK .NE. 0 ) THEN
        IC = IC + 1

*      Skip space
        CALL CHR_FIWS( TEXT, IC, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

      END IF

*    Get numbers until end met. IC should point to the beginning of a
*    number at the top of this loop
      MORE = .TRUE.
      DO WHILE ( (STATUS.EQ.SAI__OK) .AND. MORE )

*      Read number
        BEG = IC
        DO WHILE ( CHR_ISDIG(TEXT(IC:IC)) .AND. (IC.LE.LEN(TEXT)) )
          IC = IC + 1
        END DO
        CALL CHR_CTOI( TEXT(BEG:IC-1), DIMS(NDIM+1), STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
        NDIM = NDIM + 1

*      Find next significant character
        CALL CHR_FIWS( TEXT, IC, STATUS )

*      End of string met?
        IF ( STATUS .NE. SAI__OK ) THEN
          STATUS = SAI__OK
          MORE = .FALSE.

*      Met delimiter?
        ELSE IF ( (IBRAK.GT.0) .AND.
     :                    (TEXT(IC:IC).EQ.RHS(IBRAK:IBRAK)) ) THEN
          MORE = .FALSE.

*      Should be a digit
        ELSE IF ( .NOT. CHR_ISDIG(TEXT(IC:IC)) ) THEN
          CALL MSG_SETC( 'FOUND', TEXT(IC:MIN(LEN(TEXT),IC+10)) )
          CALL MSG_PRNT( '! /^FOUND/ found where number expected' )
          STATUS = SAI__ERROR

*      Too many dimensions?
        ELSE IF ( NDIM .EQ. MAXDIM ) THEN

          CALL MSG_SETI( 'MDIM', MAXDIM )
          CALL MSG_PRNT( '! Maximum number of dimensions exceeded' )
          STATUS = SAI__ERROR

        END IF

      END DO

*    Abort
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from PRS_GETDIMS', STATUS )
      END IF

      END
