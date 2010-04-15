      SUBROUTINE SPOSB2( PARAM, ASTAVL, LBND, UBND, IDA, STATUS )
*+
*  Name:
*     SPOSB2

*  Purpose:
*     Get astrometry information from an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPOSB2( PARAM, ASTAVL, LBND, UBND, IDA, STATUS )

*  Description:
*     If ASTAVL is supplied true then an immediate return is made.
*     Otherwise an NDF is accessed for READ using the supplied
*     parameter. An attempt is then made to import the astrometry
*     information from the NDF into the IRA system. If this is
*     succesful, ASTAVL is returned true, otherwise the IRA identifiers
*     are annulled. Either way, the NDF is closed before returning.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter to associate with the NDF.
*     ASTAVL = LOGICAL (Given and Returned)
*        If supplied true, then no action is taken, it being assumed
*        that IDA already identifies valid astrometry information.
*        ASTAVL is returned true if IDA is valid on exit.
*     LBND( 2 ) = INTEGER (Returned)
*        The lower bounds of the NDF.
*     UBND( 2 ) = INTEGER (Returned)
*        The upper bounds of the NDF.
*     IDA = INTEGER (Returned)
*        The IRA identifier for the astrometry information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-JAN-1993 (DSB):
*        Original version.
*     1-OCT-1993 (DSB):
*        Arguments LBND and UBND added.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Given and Returned:
      LOGICAL ASTAVL

*  Arguments Returned:
      INTEGER LBND( 2 )
      INTEGER UBND( 2 )
      INTEGER IDA

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER INDF               ! NDF identifier.
      INTEGER NDIM               ! No. of dimensions in the NDF
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do nothing if IDA is already valid.
      IF( .NOT. ASTAVL ) THEN

*  Start an NDF context.
         CALL NDF_BEGIN

*  Loop round until an NDF is obtained.
 10      CONTINUE
         CALL NDF_ASSOC( PARAM, 'READ', INDF, STATUS )

*  If a null value was supplied, annul the error, cancel the parameter
*  and loop back for a new value.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL PAR_CANCL( PARAM, STATUS )
            GO TO 10
         END IF

*  Attempt to read in the astrometry information.
         CALL IRA_IMPRT( INDF, IDA, STATUS )

*  If all has gone OK, indicate that astrometry information is now
*  available, and get the bounds of the NDF.
         IF( STATUS .EQ. SAI__OK ) THEN
            ASTAVL = .TRUE.
            CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS )

*  Otherwise, annul the IRA identifier.
         ELSE
            CALL IRA_ANNUL( IDA, STATUS )
         END IF

*  End the NDF context.
         CALL NDF_END( STATUS )

      END IF

      END
