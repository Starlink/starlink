      SUBROUTINE IMG_CHECK( STATUS )
*+
* Name:
*    IMG_CHECK

*  Purpose:
*     Checks the current internal state of IMG.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG_CHECK( STATUS )

*  Description:
*     This routine checks the IMG Parameter Control Block and reports
*     its contents. It is usual to call this routine in situations when
*     the current state of IMG is uncertain (i.e. during development).

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine executes even if STATUS is set on entry.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     15-JUL-1994 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      
*  Global Variables:
      INCLUDE 'IMG_PCB'          ! IMG Parameter Control Block
*        PCB_PARAM( IMG__MXPAR ) = CHARACTER * ( IMG__SZPAR ) (Read)
*           Parameter names. Blank when not used.
*        PCB_INDF( IMG__MXPAR ) = (  (Read)
*           NDF identifiers. Set to NDF__NOID when released.
*        PCB_PNTR( IMG__MXPAR) = INTEGER (Read)
*           Pointers to the 'DATA' components of the NDFs. Set to
*           IMG_NOPTR when released.
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      
*.

*  Start an error block.
      CALL ERR_BEGIN( STATUS )
      
*  Loop over all the slots in the PCB block and look for any non-blank
*  parameters.
      DO 1 I = 1, IMG__MXPAR
         IF ( PCB_PARAM( I ) .NE. ' ' ) THEN

*  Slot associated with an NDF. Write a message about the slot number.
            CALL MSG_SETI( 'SLOT', I )
            CALL MSG_SETC( 'PARAM', PCB_PARAM( I ) )
            CALL MSG_OUT( ' ',
     :'  Slot ^SLOT is in use by parameter ^PARAM', STATUS )

*  Get the name of the NDF and write this out also.
            IF ( PCB_INDF( I ) .NE. NDF__NOID ) THEN 
               CALL NDF_MSG( 'NDF', PCB_INDF( I ) )
               CALL MSG_OUT( ' ',
     :'  ...and references NDF ^NDF', STATUS )
            ELSE

*  Slot mustn't have been cleared properly
               CALL MSG_OUT( ' ',
     :'  ...warning no NDF is referenced (this is an error)', STATUS )
            END IF

*  Check that the pointer is valid too.
            IF ( PCB_PNTR( I ) .EQ. IMG__NOPTR ) THEN
               CALL MSG_OUT( ' ',
     :'   ...warning slot references a null data pointer (this is ' //
     :'an error )', STATUS )
            END IF
         END IF
 1    CONTINUE

*  End the error block.
      CALL ERR_END( STATUS )
      END
* $Id$
