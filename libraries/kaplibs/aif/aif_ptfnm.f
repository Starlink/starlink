      SUBROUTINE AIF_PTFNM( PARNAM, FILNAM, STATUS )
*+
*  Name:
*     AIF_PTFNM

*  Purpose:
*     Writes the name of an HDS file to a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL AIF_PTFNM( PARNAM, FILNAM, STATUS )

*  Description:
*     Normally, the handles to HDS data files are locators and files
*     are obtained via the parameter system.  However, some
*     applications can generate sensible names, especially when dealing
*     a long series of files that are to be created without manual
*     intervention.  There is no direct mechanism in the user-level
*     parameter-system library to put a name into the associated
*     parameter.  This routine provides that functionality.

*  Arguments:
*     PARNAM = CHARACTER * ( * ) (Given)
*        Parameter name associated with the data object whose name is
*        to be written.
*     FILNAM = CHARACTER * ( * ) (Given)
*        The name of the file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This cannot be used to obtain the names of objects within
*        an HDS file.

*  Algorithm:
*     -  Get the ADAM internal code that refers to the piece of
*        parameter space associated with the input parameter.
*     -  Associate the file name with the ADAM internal pointer.

*  Prior Requirements:
*     -  Must have obtained the file via the input parameter.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 Feb 21 (MJC):
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
      CHARACTER * ( * )
     :  PARNAM,                  ! Parameter name of the file whose
                                 ! name is required
     :  FILNAM                   ! File name to be written to the
                                 ! parameter

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  IMCODE                   ! ADAM internal parameter pointer.
*.

*    Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get the ADAM internal code that refers to the piece of
*    parameter space associated with the input parameter PARNAM.

      CALL SUBPAR_FINDPAR( PARNAM, IMCODE, STATUS )

*    Associate the file name with the ADAM internal pointer.

      CALL SUBPAR_PUTNAME( IMCODE, FILNAM, STATUS )

      END
