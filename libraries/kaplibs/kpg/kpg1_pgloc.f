      SUBROUTINE KPG1_PGLOC( LOC1, LOC2, STATUS )
*+
*  Name:
*     KPG1_PGLOC

*  Purpose:
*     Locate a component of an HDS structure relating to the currently
*     opened PGPLOT device.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PGLOC( LOC1, LOC2, STATUS )

*  Description:
*     LOC1 is an locator for an HDS structure which contains components
*     relating to one or more PGPLOT devices. These components, for
*     instance, may contain the colour palette or colour table to be used 
*     with the corresponding PGPLOT device. This routine searches the
*     structure for a component relating to the currently opened PGPLOT 
*     device, and returns a locator for it if found. If not found, a
*     search is made for a component relating to a device of the same 
*     type, but maybe with a different file. If no such device is found,
*     (or if an error occurs) DAT__NOLOC is returned.
*
*     For instance, if the currently opened graphics device is "x2windows" 
*     (i.e. "xwindows2/GWM"), a search is made first for a component called 
*     AGI_3801_2. If this is not found, a search is made for a component
*     with a name corresponding to any /GWM device (e.g. AGI_3800_1 which
*     corresponds to "xwindows/GWM", or one of the other xwindows sevices).
*
*     The component names used are the same as the names uses for the device 
*     within the AGI database (e.g. "AGI_3801_2", etc).

*  Arguments:
*     LOC1 = CHARACTER * ( * ) (Given)
*        A locator for the object to be searched.
*     LOC2 = CHARACTER * ( * ) (Returned)
*        A locator for the found component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A PGPLOT device must previously have been opened using AGI.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-NOV-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Given:
      CHARACTER LOC1*(*)

*  Arguments Returned:
      CHARACTER LOC2*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER AGINAM*(DAT__SZNAM)! AGI workstation name
      CHARACTER PTYPE*30         ! PGPLOT device type (with leading slash)
      CHARACTER SPEC*60          ! Full PGPLOT device spec
      INTEGER I                  ! Component index
      INTEGER LENGTH             ! Length of string returned by PGQINF
      INTEGER NC                 ! Number of characters in the buffer
      INTEGER NCOMP              ! Number of components in LOC
      LOGICAL THERE              ! Does component exist?
*.

*  Initialize.
      LOC2 = DAT__NOLOC

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the AGI name for the currently opened device.
      CALL AGP_CURAG( AGINAM, STATUS )

*  If a component with this name exists within the supplied HDS structure,
*  get a locator to it.
      CALL DAT_THERE( LOC1, AGINAM, THERE, STATUS )
      IF( THERE ) THEN
         CALL DAT_FIND( LOC1, AGINAM, LOC2, STATUS )

*  Otherwise, we look for a component for a similar device...
      ELSE

*  Get the corresponding PGPLOT device type.
         CALL AGP_ASPEC( AGINAM, .FALSE., SPEC, STATUS )
         PTYPE = SPEC( INDEX( SPEC, '/' ): )

*  Loop round all components in the supplied object.
         CALL DAT_NCOMP( LOC1, NCOMP, STATUS ) 
         DO I = 1, NCOMP

*  Get a locator to this component, and its name (an AGI workstation name).
            CALL DAT_INDEX( LOC1, I, LOC2, STATUS ) 
            CALL DAT_NAME( LOC2, AGINAM, STATUS ) 

*  Convert the AGI name into a PGPLOT device specification, and compare 
*  to the device type for the currently opened device. If they are the 
*  same, return with the current component locator.
            CALL AGP_ASPEC( AGINAM, .FALSE., SPEC, STATUS )
            IF( PTYPE .EQ. SPEC( INDEX( SPEC, '/' ): ) ) THEN
               GO TO 999

*  Otherwise, annul the locator.
            ELSE
               CALL DAT_ANNUL( LOC2, STATUS )
            END IF

         END DO
         
      END IF

 999  CONTINUE

      END
