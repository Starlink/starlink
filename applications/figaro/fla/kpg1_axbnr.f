      SUBROUTINE KPG1_AXBNR( EL, CENTRE, AXLCO, AXUCO, STATUS )
*+
*  Name:
*     KPG1_AXBNx

*  Purpose:
*     Find the bounds of an NDF's axis centre co-ordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_AXBNx( EL, CENTRE, AXLCO, AXUCO, STATUS )

*  Description:
*     This routine determines the lowest and highest centre
*     co-ordinate for an NDF axis array component.  Currently, it
*     assumes that these will be the first and last elements.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements in the axis.
*     CENTRE( EL ) = ? (Given)
*        The NDF axis centre co-ordinates.
*     AXLCO = ? (Returned)
*        The lowest centre co-ordinates for the axis.
*     AXUCO = ? (Returned)
*        The highest centre co-ordinate for the axis.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  There is a routine for the data types real or double precision:
*     replace "x" in the routine name by R or D respectively, as
*     appropriate.  The centre array and the returned values
*     should have this data type as well.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 February 12 (MJC):
*        Original version.
*     1991 May 31 (MJC):
*        Converted to generic.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER
     :  EL

      REAL
     :  CENTRE( EL )

*  Arguments Returned:
      REAL
     :  AXLCO,
     :  AXUCO

*  Status:
      INTEGER STATUS             ! Global status

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Assume that the axis centre co-ordinates increase monotonically
*    from beginning to end.

      AXLCO = MIN( CENTRE( 1 ), CENTRE( EL ) )
      AXUCO = MAX( CENTRE( 1 ), CENTRE( EL ) )

      END
