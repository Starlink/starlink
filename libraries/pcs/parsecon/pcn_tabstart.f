*+  PARSECON_TABSTART - Fill the parse state-tables with error states
      SUBROUTINE PARSECON_TABSTART ( STATUS )
*    Description :
*     The two parse-state tables are set so that all their values 
*     correspond to invalid parse-states. This is the first stage in 
*     initialising the tables.
*    Invocation :
*     CALL PARSECON_TABSTART ( STATUS )
*    Parameters :
*     STATUS=INTEGER
*    Method :
*     The parse-state is stored in two BYTE arrays -
*       ACTTAB - action code
*       STATETAB - new parse state to be entered.
*     All the elements of these are initialised to
*       ACTTAB - ERROR
*       STATETAB - FACEGOT
*     Positions in the tables corresponding to valid parse states will 
*     be subsequently overwritten.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     18.09.1984:  Original (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'PARSECON_CMN'

*    Local variables :
      INTEGER I
      INTEGER J

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO J = 1, PARSE__NUMTOK
         DO I = 1, PARSE__NUMSTATE
            ACTTAB(I,J) = ERROR
            STATETAB(I,J) = FACEGOT
         ENDDO
      ENDDO

      END
