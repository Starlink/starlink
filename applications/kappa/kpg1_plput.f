      SUBROUTINE KPG1_PLPUT( CI1, CI2, ARRAY, STATUS )
*+
*  Name:
*     KPG1_PLPUT

*  Purpose:
*     Put a section of the current colour palette into the supplied array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PLPUT( CI1, CI2, ARRAY, STATUS )

*  Description:
*     This routine puts a specified section of the colour palette for the 
*     currently opened graphics device into the supplied array. Other
*     elements of the array are left unchanged.
*
*  Arguments:
*     CI1 = INTEGER (Given)
*        The lowest colour index to change in the array.
*     CI2 = INTEGER (Given)
*        The highest colour index to change in the array.
*     ARRAY( 3, 0 : CI2 ) = REAL (Given and Returned)
*        The array to recieved the palette.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A graphics device must previously have been opened using SGS/GKS.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-OCT-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CTM_PAR'          ! Colout Table Management constants

*  Arguments Given:
      INTEGER CI1
      INTEGER CI2

*  Arguments Given and Returned:
      REAL ARRAY( 3, 0 : CI2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER GSTAT              ! GKS status
      INTEGER IWKID              ! GKS workstation identifier
      INTEGER I                  ! Loop count
  
*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the workstation identifier for the currently open graphics device.
      CALL SGS_ICURW( IWKID ) 

*  Inquire whether GKS/SGS has reported an error
      CALL GKS_GSTAT( STATUS )

*  Inquire the palette colour indices, and store in the array.
      DO  I = CI1, CI2
         CALL GQCR( IWKID, I, 1, GSTAT, ARRAY( 1, I ), ARRAY( 2, I ), 
     :              ARRAY( 3, I ) )
      END DO

*  See if within GKS an error has occurred.
      CALL GKS_GSTAT( STATUS )

      END
