      SUBROUTINE PAR_DEF1R ( PARAM, NVAL, VALUES, STATUS )
*+
*  Name:
*     PAR_DEF1x
 
*  Purpose:
*     Sets a vector of values as the dynamic default for a parameter.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL PAR_DEF1x( PARAM, NVAL, VALUES, STATUS )
 
*  Description:
*     This routine sets a 1-dimensional array of values as the dynamic
*     default for a parameter. The dynamic default may be used as the
*     parameter value by means of appropriate specifications in the
*     interface file.
 
*     If the declared parameter type differs from the type of the
*     array supplied, then conversion is performed.
 
*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of a parameter of primitive type.
*     NVAL = INTEGER (Given)
*        The number of default values.
*     VALUES( NVAL ) = ? (Given)
*        The array to contain the default values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for each of the data types character,
*     double precision, integer, logical, and real: replace "x" in the
*     routine name by C, D, I, L, or R respectively as appropriate.  The
*     VALUES argument must have the corresponding data type.
 
*  Algorithm:
*     Call the underlying parameter-system primitives.
 
*  Authors:
*     BDK: B D Kelly (REVAD::BDK)
*     AJC: A J Chipperfield (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     10-DEC-1984 (BDK):
*        Original version.
*     1-JUN-1988 (AJC):
*        Revised prologue
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
      IMPLICIT NONE              ! No implict typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAE Constants
 
*  Arguments Given:
      CHARACTER * ( * ) PARAM    ! Parameter Name
 
      INTEGER NVAL               ! Number of values
 
      REAL VALUES( * )         ! Scalar to supply values
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER NAMCOD             ! Pointer to parameter
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Find the parameter-system pointer to the internal parameter space
*  associated with the parameter.
      CALL SUBPAR_FINDPAR( PARAM, NAMCOD, STATUS )
 
*  Use the pointer to set the dynamic defaults.
      CALL SUBPAR_DEF1R( NAMCOD, NVAL, VALUES, STATUS )
 
      END
