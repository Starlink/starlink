*+  TASK_DEC0I - decode a character string as a value
      SUBROUTINE TASK_DEC0I ( STRING, IVAL, STATUS )
 
*    Description :
*     Convert the given character string into a value of type
*     INTEGER and return it in IVAL.
*     A routine exists for each type C, D, L, I, R.
 
*    Invocation :
*     CALL TASK_DEC0I ( STRING, IVAL, STATUS )
 
*    Parameters :
*     STRING=CHARACTER*(*) (given)
*           the string to be decoded
*     IVAL=INTEGER (returned)
*           the returned value
*     STATUS=INTEGER
 
*    Method :
*     Use CHR_CTOI. If that fails try CHR_CTOD and INT. 
*     (This is a change from previous behaviour which found the nearest
*     integer but it now does the same as SUBPAR)
*     If that fails try CHR_CTOL setting 1 if true and 0 if false.
*     (This is a change, previously these values would not be converted.) 

*    Deficiencies :
*     <description of any deficiencies>
 
*    Bugs :
*     <description of any "bugs" which have not been fixed>
 
*    Authors :
*     W.F.Lupton (AAOEPP::WFL)
*     A J Chpperifeld (RLVAD::AJC)
 
*    History :
*     29.04.1989:  original (AAOEPP::WFL)
*      4.10.1992:  use CHR for portability (RLVAD::AJC)
*      2.09.1993:  remove unused ISTAT (RLVAD::AJC)
 
*    Type Definitions :
      IMPLICIT NONE
 
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'TASK_ERR'
 
*    Import :
      CHARACTER*(*) STRING  ! the character string to be decoded
 
*    Export :
      INTEGER IVAL         ! the returned value
 
*    Status :
      INTEGER STATUS
 
*    Local variables :
      DOUBLE PRECISION DVAL ! value in double precision
      LOGICAL LVAL          ! value as logical
 
*-
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*   Use appropriate CHR routine
      CALL CHR_CTOI( STRING, IVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
      CALL EMS_ANNUL( STATUS )
*     Attempt conversion via DOUBLE PRECISION
         CALL CHR_CTOD( STRING, DVAL, STATUS )

*     If OK find integer part of
         IF ( STATUS .EQ. SAI__OK ) THEN
            IVAL = INT( DVAL )

*     If it failed, try going via logical
         ELSE
            CALL EMS_ANNUL( STATUS )
            CALL CHR_CTOL( STRING, LVAL, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( LVAL ) THEN
                  IVAL = 1
               ELSE
                  IVAL = 0
               END IF

            ELSE
               CALL EMS_SETC( 'STR', STRING )
               CALL ERR_REP( 'TSK_DEC0L1',
     :         'TASK_DEC0I: Failed to convert ^STR to INTEGER',
     :          STATUS )
            END IF
         END IF

      END IF
      END
