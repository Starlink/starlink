      SUBROUTINE SETUNITS( STATUS )
*+
*  Name:
*     SETUNITS

*  Purpose:
*     Sets a new units value for an NDF data structure.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SETUNITS( STATUS )

*  Description:
*     This routine sets a new value for the units component of an
*     existing NDF data structure. The NDF is accessed in update mode
*     and any pre-existing units component is over-written with a new
*     value.  Alternatively, if a `null' value (!) is given for the
*     UNITS parameter, then the NDF's units component will be erased.

*  Usage:
*     setunits ndf units

*  ADAM Parameters:
*     NDF = NDF (Read and Write)
*        The NDF data structure whose units component is to be
*        modified.
*     UNITS = LITERAL (Read)
*        The value to be assigned to the NDF's units component (e.g.
*        "J/(m**2*Ang*s)" or "count/s").  This value may later be used
*        by other applications for labelling graphs and other forms of
*        display where the NDF's data values are shown.  The suggested
*        default is the current value.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Related Applications:
*     KAPPA: AXUNITS, SETLABEL, SETTITLE.

*  Examples:
*     setunits ngc1342 "count/s"
*        Sets the units component of the NDF structure ngc1342 to have
*        the value "count/s".
*     setunits ndf=spect units="J/(m**2*Ang*s)"
*        Sets the units component of the NDF structure spect to have
*        the value "J/(m**2*Ang*s)".
*     setunits datafile units=!
*        By specifying a null value (!), this example erases any
*        previous value of the units component in the NDF structure
*        datafile.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-JUN-1990 (RFWS):
*        Original version.
*     1995 April 21 (MJC):
*        Made usage and examples lowercase.  Added closing error
*        report and Related Applications.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NDF                ! NDF identifier

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an identifier for the NDF to be modified.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', NDF, STATUS )

*  Reset any existing units component.
      CALL NDF_RESET( NDF, 'Units', STATUS )

*  Obtain a new value for the units component.
      CALL NDF_CINP( 'UNITS', NDF, 'Units', STATUS )

*  Annul the NDF identifier.
      CALL NDF_ANNUL( NDF, STATUS )

*  Write the closing error message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SETUNITS_ERR',
     :     'SETUNITS: Error modifying the label of an NDF.', STATUS )
      END IF

      END
