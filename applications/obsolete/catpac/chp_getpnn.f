      SUBROUTINE
     : CHP_GETPNN( INPUT, PNAMES, NUMPARS, STATUS )
*+
*  Name:
*     CHP_GETPNN

*  Purpose:
*     GET Paramater Names and Number of parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_GETPNN( INPUT, PNAMES, NUMPARS, STATUS )
*
*  Description:
*     Get the names of all the parameters in a catalogue. Also count the
*     number of parameters found.

*  Arguments:
*     INPUT = CHARACTER * ( CHP__SZNAME ) (Given)
*        Name of the catalogue from which the parameter names are
*        required.
*     PNAMES( CHP__NUMPARS ) = CHARACTER * ( CHP__SZPNAME ) (Returned)
*        Names of the parameters in the catalogue.
*     NUMPARS = INTEGER (Returned)
*        Number of parameters in the catalogue.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     CHP__CATNOTFND

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1991 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHP_PAR'   ! Standard CHP constants
      INCLUDE 'CHP_ERR'   ! Standard CHP errors

*  Global Variables:
      INCLUDE 'CHP_CMN'   ! Standard CHP variables

*  Arguments Given:
      CHARACTER * ( CHP__SZNAME ) INPUT

*  Arguments Returned:
      INTEGER NUMPARS
      CHARACTER * ( CHP__SZPNAME ) PNAMES( CHP__NUMPARS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CD
      INTEGER PCOUNT

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*
      call chp_getcd(input, .FALSE. ,cd, status)
      numpars = CPnumpars(cd)
      do pcount= 1, numpars
        pnames(pcount) = PPname(cd,pcount)
      enddo
*
      END
