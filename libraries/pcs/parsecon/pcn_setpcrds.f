*+  PARSECON_SETPCRDS - Sets-up parameter menu position
      SUBROUTINE PARSECON_SETPCRDS ( ENTRY, STATUS )
*    Description :
*     Loads the coordinate for the most recently declared program 
*     parameter into the PARCOORDS store at the position indicated.
*    Invocation :
*     CALL PARSECON_SETPCRDS ( ENTRY, STATUS )
*    Parameters :
*     ENTRY=CHARACTER*(*) (given)
*           Numeric character string, indicating the position in the 
*           menu for the parameter 
*     STATUS=INTEGER
*    Method :
*     The given string is converted to an integer which defines a 
*     screen coordinate. If the X-coordinate hasn't been set yet, assume 
*     the X-coordinate is being given. Otherwise, assume it is the 
*     Y-coordinate.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*    History :
*     13.05.1986:  Original (REVAD::BDK)
*     16.10.1990:  Use CHR for conversion to integer
*                  it's portable and stricter (RLVAD::AJC)
*     20.01.1992:  Renamed from _SETPCOORDS (RLVAD::AJC)
*     25.02.1991:  Report errors (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PARSECON_ERR'

*    Import :
      CHARACTER*(*) ENTRY             ! the numeric string

*    Status :
      INTEGER STATUS

*    External references :
*     None

*    Global variables :
      INCLUDE 'SUBPAR_CMN'

*    Local variables :
      INTEGER NUMBER
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Convert the string to integer. 
      CALL CHR_CTOI( ENTRY, NUMBER, STATUS )

      IF ( STATUS .EQ. SAI__ERROR ) THEN

         STATUS = PARSE__IVCOORD
         CALL EMS_REP ( 'PCN_SETPCRDS1',
     :   'PARSECON: Parameter coordinates must be INTEGER',
     :    STATUS )

      ELSE
*      If Xcoord free, assume it is that. Otherwise, put it into Y.
         IF ( PARCOORDS(1,PARPTR) .EQ. -1 ) THEN
            PARCOORDS(1,PARPTR) = NUMBER
         ELSE
            PARCOORDS(2,PARPTR) = NUMBER
         ENDIF

      ENDIF

      END
