      SUBROUTINE KPG1_CCPRO( PNCOMP, COMP, NDFI, NDFO, STATUS )
*+
*  Name:
*     KPG1_CCPRO

*  Purpose:
*     Gets a character component for an output NDF with optional
*     propagation from another NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_CCPRO( PNCOMP, COMP, NDFI, NDFO, STATUS )

*  Description:
*     This routine uses the parameter sysyem to obtain a value for a
*     selected character component of an output or updated NDF.  If the
*     null value is supplied, the character component is copied from
*     another NDF to the output NDF, unless the component is undefined,
*     whereupon ablank string is used.

*  Arguments:
*     PNCOMP = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter used to obtain the character
*        component's value.
*     COMP = CHARACTER * ( * ) (Given)
*        The name of the character component.  It must be 'TITLE',
*        'LABEL', or 'UNITS'.
*     NDFI = INTEGER (Given)
*        The identifier of the NDF from which a character component
*        is to be copied to the output NDF.
*     NDFO = INTEGER (Given)
*        The identifier of the output or updated NDF to which a
*        character component is to be written.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1994 September 27 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PNCOMP
      CHARACTER * ( * ) COMP
      INTEGER NDFI
      INTEGER NDFO

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) VALUE   ! Value of the character component

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the value of the component.  This remains the value if
*  there is no such component in the input NDF.
      VALUE = ' '
      
*  Get the value of the character component from the 
      CALL NDF_CGET( NDFI, COMP, VALUE, STATUS )

*  Assign a default value for the character component in the output
*  NDF.
      CALL NDF_CPUT( VALUE, NDFO, COMP, STATUS )

*  Obtain the value for the character component from the parameter
*  system.  If a null value is supplied, the component takes the
*  default value obtained above.
      CALL NDF_CINP( PNCOMP, NDFO, COMP, STATUS )

      END
