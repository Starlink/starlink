      SUBROUTINE PAR_GET0C ( PARAM, VALUE, STATUS )
*+
*  Name:
*     PAR_GET0x
 
*  Purpose:
*     Obtains a scalar value from a parameter.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL PAR_GET0x( PARAM, VALUE, STATUS )
 
*  Description:
*     This routine obtains a scalar value from a parameter.  If it is
*     necessary, the value is converted to the required type.
 
*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     VALUE = ? (Returned)
*        The parameter value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for each of the data types character,
*     double precision, integer, logical, and real: replace "x" in the
*     routine name by C, D, I, L, or R respectively as appropriate.  The
*     VALUE argument must have the corresponding data type.
*     -  Note that a scalar (0-dimensional) parameter is different from
*     a vector (1-dimensional) parameter containing a single value.
 
*  Algorithm:
*     Call the underlying parameter-system primitives.
 
*  Authors:
*     BDK: B D Kelly (REVAD::BDK)
*     AJC: A J Chipperfield (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     26-OCT-1984 (BDK):
*        Original version.
*     1-JUN-1988 (AJC):
*        Revised prologue.
*     9-NOV-1990 (AJC):
*        Revised prologue again
*     1992 March 27 (MJC):
*        Used SST prologues.
*     1992 November 13 (MJC):
*        Commented the code, and renamed the NAMECODE identifier.
*        Re-tidied the prologue.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
 
*  Arguments Given:
      CHARACTER * ( * ) PARAM
 
*  Arguments Returned:
      CHARACTER*(*) VALUE
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER NAMCOD             ! Code number for the parameter
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Find the parameter-system pointer to the internal parameter space
*  associated with the parameter.
      CALL SUBPAR_FINDPAR( PARAM, NAMCOD, STATUS )
 
*  Use the pointer to get the value.
      CALL SUBPAR_GET0C( NAMCOD, VALUE, STATUS )
 
      END
