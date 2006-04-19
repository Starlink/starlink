      SUBROUTINE PARSECON_NEWACRDS ( STATUS )
*+
*  Name:
*     PARSECON_NEWACRDS

*  Purpose:
*     initialise action menu coordinates.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_NEWACRDS ( STATUS )

*  Description:
*     Initialise the menu coordinates for the most recently declared
*     action.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     Set the common block array ACTCOORDS to (-1,-1) at the position
*     for the latest parameter.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13.05.1986:  Original (REVAD::BDK)
*     20.01.1992:  Renamed from _NEWACOORDS (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      ACTCOORDS(1,ACTPTR) = -1
      ACTCOORDS(2,ACTPTR) = -1

      END
