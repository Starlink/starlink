*+  PARSECON_NEWPCRDS - initialise parameter menu coordinates
      SUBROUTINE PARSECON_NEWPCRDS ( STATUS )
*    Description :
*     Initialise the menu coordinates for the most recently declared 
*     parameter.
*    Invocation :
*     CALL PARSECON_NEWPCRDS ( STATUS )
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*     STATUS=INTEGER
*    Method :
*     Set the common block array PARCOORDS to (-1,-1) at the position 
*     for the latest parameter.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*    History :
*     13.05.1986:  Original (REVAD::BDK)
*     20.01.1992:  Renamed from NEWPCOORDS (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'SUBPAR_CMN'
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      PARCOORDS(1,PARPTR) = -1
      PARCOORDS(2,PARPTR) = -1

      END
