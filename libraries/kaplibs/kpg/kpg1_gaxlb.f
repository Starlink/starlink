      SUBROUTINE KPG1_GAXLB( NDF, IAXIS, PNAXL, DEFAUL, AXSLAB, STATUS )
*+
*  Name:
*     KPG1_GAXLB

*  Purpose:
*     Obtains an axis annotation for an NDF axis.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GAXLB( NDF, IAXIS, PNAXL, DEFAUL, AXSLAB, STATUS )

*  Description:
*     This routine obtains an axis annotation from the parameter system.
*     The suggested default is of the form "label (units)" when there
*     is both an axis label and units, or "label" if the label but not
*     the units are present.  If neither are present a supplied default
*     is used instead.  If a bad status, other than abort, is returned
*     by the parameter system when getting the value, the error is
*     annulled and the output annotation is the suggested default value.

*  Arguments:
*     NDF = INTEGER (Given)
*        The NDF identifier.
*     IAXIS = INTEGER (Given)
*        The number of the axis whose character components are to be
*        used.
*     PNAXL = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter from which the annotation will
*        be obtained.
*     DEFAUL = CHARACTER * ( * ) (Given)
*        The suggested default when the NDF axis does not have a label,
*        and the actual value returned when a null (!) value or any
*        other non-abort bad status is obtained from the parameter
*        system.
*     AXSLAB = CHARACTER * ( * ) (Returned)
*        The annotation obtained from the parameter system.  The dynamic
*        default is limited to 128 characters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 February 24 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors

*  Arguments Given:
      INTEGER
     :  NDF,
     :  IAXIS

      CHARACTER * ( * )
     :  PNAXL,
     :  DEFAUL

*  Arguments Returned:
      CHARACTER * ( * )
     :  AXSLAB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * 128            ! Limitation by the parameter system
     :  AXSDEF                   ! Default annotation

*.

*  Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Start a new error context.

      CALL ERR_MARK

*    Create a default axis annotation from the NDF axis label and
*    units.

      CALL KPG1_AXANO( NDF, IAXIS, DEFAUL, AXSDEF, STATUS )

*    Set the value from the NDF axis as the suggested default.

      CALL PAR_DEF0C( PNAXL, AXSDEF, STATUS )

*    Get the axis label.

      CALL PAR_GET0C( PNAXL, AXSLAB, STATUS )

*    Look for the null condition.

      IF ( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         AXSLAB = AXSDEF
      END IF

*    Release the new error context.

      CALL ERR_RLSE

      END
