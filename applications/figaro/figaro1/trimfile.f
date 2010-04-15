      SUBROUTINE TRIMFILE( STATUS )
*+
*  Name:
*     TRIMFILE

*  Purpose:
*     Creates a copy of an HDS file without unused space.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL TRIMFILE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Certain changes to HDS files may cause them to contain unused
*     space, deleted or temporary structures, etc. This routine will
*     create a new copy of the file which will only contain actually
*     used structures. This is in fact only a call to HDS_COPY.

*  Usage:
*     trimfile in out

*  ADAM Parameters:
*     IN = HDSOBJECT (Read)
*        The HDS file suspected to contain a lot of garbage. The default
*        extension is .sdf. For other extensions use the @-sign and
*        double quotes as in @"file.dst".
*     OUT = _CHAR (Read)
*        The name of the new, clean copy of the HDS file. The default
*        extension is .sdf. For other extensions just specify them as in
*        file2.dst or "file2.dst", but not @"file2.dst".

*  Authors:
*     KS: Keith Shortridge (AAO)
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     22-FEB-1989 (KS):
*        Original version.
*     30-SEP-1992 (HME):
*        Rewritten in terms of HDS calls.
*     12-MAR-1993 (HME):
*        Removed contextual error report.
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

*  Local Variables:
      CHARACTER * ( 132 ) OUT
      CHARACTER * ( DAT__SZLOC ) LOC1 ! Locator to input file

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the first file, get the name for the second, copy the first
*  into the second.
      CALL DAT_ASSOC( 'IN', 'READ', LOC1, STATUS )
      CALL PAR_GET0C( 'OUT', OUT, STATUS )
      CALL HDS_COPY( LOC1, OUT, 'OUTPUT', STATUS )

      END
