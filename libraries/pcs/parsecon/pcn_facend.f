*+  PARSECON_FACEND - ENDINTERFACE action
      SUBROUTINE PARSECON_FACEND ( STATUS )
*    Description :
*     Check that defined 'positions' are correct and clear
*     the action name from the error report common block.
*    Invocation :
*     CALL PARSECON_FACEND ( STATUS )
*    Parameters :
*     STATUS=INTEGER
*    Method :
*     Compare the highest position specified (saved in COMMON) with
*     the number of parameters - report if it is greater.
*     Also check that the positions are a continuous set starting at 1.
*     Set ACNAME to blank.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     A.J.Chipperfield
*     A J Chipperfield (STARLINK)
*    History :
*     16.08.1990: Original (RLVAD::AJC)
*     20.11.1991: Add check on contiguous position nos. (RLVAD::AJC)
*      6.03.1192: Correct above checking (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PARSECON_ERR'

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'SUBPAR_CMN'
      INCLUDE 'PARSECON3_CMN'
      INCLUDE 'PARSECON4_CMN'

*    Local variables :
      INTEGER I                         ! Loop counter
      INTEGER J                         ! Loop counter
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( HIPOS .GT. PARPTR ) THEN
*     Position storage for task overflowed
         STATUS = PARSE__NCPOS
         CALL EMS_SETI( 'POS', HIPOS )
         CALL EMS_REP( 'PCN_FACEND1',
     :   'PARSECON: Parameter "position" specified (^POS) '//
     :   'exceeds the number of parameters', 
     :    STATUS )
*     Clear erroneous positions - ie those from the end of this
*     task to the start+HIPOS
         DO J = PROGADD(1,ACTPTR)+PARPTR, PROGADD(1,ACTPTR)+HIPOS
            PARPOS(J) = 0
         ENDDO

      ELSEIF ( HIPOS .NE. 0 ) THEN
*     Check for contiguous set of positions - starting at 1
*     For each element of PARPOS from PROGADD(1,ACTPTR) to HIPOS
*     check that it has been used.
         DO 10, I = PROGADD(1,ACTPTR), HIPOS

            IF ( PARPOS(I) .EQ. 0 ) THEN
*           The element wasn't used
*           i.e. the set isn't contiguous
               STATUS = PARSE__NCPOS
               CALL EMS_SETI( 'POS', I-PROGADD(1,ACTPTR)+1 )
               CALL EMS_REP( 'PCN_FACEND2',
     :         'PARSECON: Parameter "position" ^POS not allocated', 
     :          STATUS )
               CALL EMS_REP( 'PCN_FACEND3',
     :         'Non-contiguous set of positions', STATUS )
            ENDIF

10       ENDDO
      ENDIF

*  Reset the name if status is OK - otherwise rely on a following
*  INTERFACE to reset it so that any error report refers to this
*  action.
      IF ( STATUS .EQ. SAI__OK ) ACNAME = ' '

      END
