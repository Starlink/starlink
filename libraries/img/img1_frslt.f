      SUBROUTINE IMG1_FRSLT( SLOT, KEEP, STATUS )
*+
*  Name:
*     IMG1_FRSLT

*  Purpose:
*     Free the resources associated with a parameter slot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_FRSLT( SLOT, KEEP, STATUS )

*  Description:
*     The routine frees all the NDF_ and IMG_ system resources
*     associated with a specified parameter slot and then frees the
*     slot itself. This makes the associated data inaccessible. Any NDF
*     associated with the slot may be kept or deleted, as required. Any
*     existing parameter association remains and can be re-used if
*     necessary.

*  Arguments:
*     SLOT = INTEGER (Given)
*        PCB slot number to be freed.
*     KEEP = LOGICAL (Given)
*        Whether any associated NDF is to be kept (as opposed to being
*        deleted). This does not apply to temporary NDFs, which are
*        always deleted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Copyright:
*     Copyright (C) 1991, 1992, 1993, 1994 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     25-JAN-1991 (RFWS):
*        Original version.
*     18-FEB-1992 (RFWS):
*        Renamed and added handling of temporary NDFs and bad pixel
*        flags.
*     2-MAR-1993 (RFWS):
*        Added the KEEP argument.
*     13-JUL-1994 (PDRAPER):
*        Now checks that NDFs may be deleted before making the attempt.
*        An error is issued if the NDF may not be deleted.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ private constants
      INCLUDE 'IMG_ERR'          ! IMG_ error codes
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Global Variables:
      INCLUDE 'IMG_PCB'          ! IMG_ Parameter Control Block
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Read and Write)
*           NDF identifier.
*        PCB_PARAM( IMG__MXPAR ) = CHARACTER * ( IMG__SZPAR ) (Write)
*           Parameter name.
*        PCB_PNTR( IMG__MXPAR ) = INTEGER (Write)
*           Pointer to mapped data.

*  Arguments Given:
      INTEGER SLOT
      LOGICAL KEEP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_INIT         ! Initialise common blocks

*  Local Variables:
      LOGICAL BAD                ! Bad pixel values present?
      LOGICAL MOD                ! NDF modified?
      LOGICAL TEMP               ! NDF is temporary?
      LOGICAL VALID              ! NDF identifier valid?
      LOGICAL OK                 ! It's OK to delete NDF

*.

*  Begin a new error reporting environment.
      CALL ERR_BEGIN( STATUS )

*  Check that the slot number supplied is valid. Report an error if it
*  is not.
      IF ( ( SLOT .LT. 1 ) .OR. ( SLOT .GT. IMG__MXPAR ) ) THEN
         STATUS = IMG__FATIN
         CALL MSG_SETI( 'SLOT', SLOT )
         CALL ERR_REP( 'IMG1_FRSLT_SLOT',
     :                 'Routine IMG1_FRSLT called with an invalid ' //
     :                 'SLOT argument of ^SLOT (internal ' //
     :                 'programming error).', STATUS )

*  Check if the NDF identifier associated with the slot is valid. If
*  not, then there is nothing to do.
      ELSE
         CALL NDF_VALID( PCB_INDF( SLOT ), VALID, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If it is valid, then determine if this is a temporary NDF and
*  whether WRITE access is available (in which case the NDF will have
*  been modified).
            IF ( VALID ) THEN
               CALL NDF_ISTMP( PCB_INDF( SLOT ), TEMP, STATUS )
               CALL NDF_ISACC( PCB_INDF( SLOT ), 'WRITE', MOD, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If it is not a temporary NDF and has been modified and is to be
*  kept, then search its data array to see if the "bad" pixel value is
*  present. Set the final bad pixel flag appropriately.
                  IF ( ( .NOT. TEMP ) .AND. MOD .AND. KEEP ) THEN
                     CALL NDF_SBAD( .TRUE., PCB_INDF( SLOT ), 'Data',
     :                              STATUS )
                     CALL NDF_BAD( PCB_INDF( SLOT ), 'Data', .TRUE.,
     :                             BAD, STATUS )
                     CALL NDF_SBAD( BAD, PCB_INDF( SLOT ), 'Data',
     :                              STATUS )
                  END IF
               END IF

*  Annul the NDF identifier, thereby unmapping its data, or delete it
*  explicitly if required.
               IF ( KEEP ) THEN
                  CALL NDF_ANNUL( PCB_INDF( SLOT ), STATUS )
               ELSE

*  Before attempting a delete check that this is possible. If not then
*  annul the NDF identifier and issue an error message.
                  CALL NDF_ISACC( PCB_INDF( SLOT ), 'DELETE', OK,
     :                            STATUS )
                  IF ( OK ) THEN
                     CALL NDF_DELET( PCB_INDF( SLOT ), STATUS )
                  ELSE
                     CALL NDF_MSG( 'NDF', PCB_INDF( SLOT ) )
                     CALL NDF_ANNUL( PCB_INDF( SLOT ), STATUS )
                     STATUS = IMG__NODEL
                     CALL ERR_REP( 'IMG1_FRSLTNODEL', 'Delete ' //
     :                    'access is not available for the NDF ^NDF.',
     :                    STATUS )
                  END IF
               END IF
            END IF
         END IF

*  Free the PCB slot for re-use.
         PCB_INDF( SLOT ) = NDF__NOID
         PCB_PARAM( SLOT ) = ' '
         PCB_PNTR( SLOT ) = IMG__NOPTR
      END IF

*  End the error reporting environment.
      CALL ERR_END( STATUS )

      END
* $Id$
