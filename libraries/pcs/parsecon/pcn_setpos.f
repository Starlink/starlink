*+  PARSECON_SETPOS - Sets-up parameter command-line position
      SUBROUTINE PARSECON_SETPOS ( ENTRY, STATUS )
*    Description :
*     Loads the code number for the most recently declared program 
*     parameter into the POSITION store at the position indicated.
*    Invocation :
*     CALL PARSECON_SETPOS ( ENTRY, STATUS )
*    Parameters :
*     ENTRY=CHARACTER*(*) (given)
*           Numeric character string, indicating the position on the 
*           command line for the parameter value
*     STATUS=INTEGER
*    Method :
*     The given string is converted to an integer which defines the 
*     position in the array defining parameter positions. If the
*     position within the array is the highest so far for this task,
*     save it in COMMON for later checking.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*    History :
*     19.09.1984:  Original (REVAD::BDK)
*     23.08.1985:  handle monoliths (REVAD::BDK)
*     16.10.1990:  Use CHR for conversion 
*                  it's portable and stricter (RLVAD::AJC)
*     12.11.1991:  Save highest position number used so it can be
*                  checked at the end (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PARSECON_ERR'

*    Import :
      CHARACTER*(*) ENTRY             ! the keyword string

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'PARSECON4_CMN'
      INCLUDE 'SUBPAR_CMN'

*    Local variables :
      INTEGER NUMBER
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Convert the string to integer. 
      CALL CHR_CTOI( ENTRY, NUMBER, STATUS )

      IF ( STATUS .EQ. SAI__ERROR ) THEN

         STATUS = PARSE__IVPOS
         CALL EMS_REP( 'PCN_SETPOS1',
     :   'PARSECON: Illegal "position" specifier', STATUS )

      ELSE

*      Add the offset for the current program within a monolith - nb 
*      same sum works for non-monoliths.
*      Check the position isn't already allocated.
         NUMBER = NUMBER + PROGADD(1,ACTPTR) - 1

*      Save NUMBER if it is higher than any previous one for this task
         IF ( NUMBER .GT. HIPOS ) HIPOS = NUMBER

*      Now store the pointer to the current parameter in the appropriate
*      PARPOS element
         IF ( PARPOS(NUMBER) .EQ. 0 ) THEN
            PARPOS(NUMBER) = PARPTR
         ELSE
            STATUS = PARSE__OLDPOS
            CALL EMS_REP( 'PCN_SETPOS2',
     :      'PARSECON: "position" number is already allocated',
     :       STATUS )

         ENDIF

      ENDIF

      END
