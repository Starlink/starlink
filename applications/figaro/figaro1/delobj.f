      SUBROUTINE DELOBJ( STATUS )
*+
*  Name:
*     DELOBJ

*  Purpose:
*     Delete an object in an HDS file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DELOBJ( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine deletes an HDS object (structure or primitive, scalar
*     or array) in an HDS file.

*  Usage:
*     delobj object

*  ADAM Parameters:
*     OBJECT = HDSOBJECT (Read)
*        The object to be deleted. Specify beginning with directory and
*        file name in the syntax of the operating system, followed by
*        the dot-separated structure hierarchy. Elements of structure
*        arrays are specified in ordinary brackets (). An array element
*        cannot be deleted.

*  Examples:
*     delobj file.axis(2).units
*        The file in question is in the current working directory and
*        has the standard extension ".sdf". The deleted structure is the
*        UNITS string in the 2nd element of the structure array AXIS.
*        Note that it would be impossible to delete AXIS(2), but one
*        could delete AXIS as a whole.
*     delobj @"/home/resun02/myname/data/file.dst".z.label
*        Here the file is specified with its complete Unix directory and
*        with its non-standard extension ".dst". The deleted structure
*        is the LABEL within the Z structure.

*  Authors:
*     KS: Keith Shortridge (AAO)
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     19-APR-1984 (KS):
*        Original version.
*     26-MAR-1991 (KS):
*        Now allows for the possibility of more thanone possible default
*        extension.
*     01-OCT-1992 (HME):
*        Rewritten completely in terms of HDS.
*     12-MAR-1993 (HME):
*        Removed the contextual error report.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Should be `s easy as that.
      CALL DAT_DELET( 'OBJECT', STATUS )

      END
