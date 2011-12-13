      SUBROUTINE IMG1_PRNDF( PARAM1, PARAM2, TYPE, PNTR, STATUS )
*+
*  Name:
*     IMG1_PRNDF

*  Purpose:
*     Propagate NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_PRNDF( PARAM1, PARAM2, TYPE, PNTR, STATUS )

*  Description:
*     The routine creates output images, one per parameter name
*     supplied, by making copies of an existing NDF. In copying the
*     NDF, it propagates the Title, Label, Data, Quality, Axis and
*     History components and all extensions.  The data array of each
*     new output NDF is mapped for UPDATE access ready for use, and a
*     pointer to the mapped data is returned.

*  Arguments:
*     PARAM1 = CHARACTER * ( * ) (Given)
*        Parameter name for the input NDF (case insensitive).
*     PARAM2 = CHARACTER * ( * ) (Given)
*        Comma-separated list of parameter names for the output NDFs
*        (case insensitive).
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric type to be used to map the output data arrays.
*     PNTR( * ) = INTEGER (Returned)
*        Pointers to the mapped output data values, one per parameter
*        supplied.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     18-FEB-1992 (RFWS):
*        Original version.
*     20-FEB-1992 (RFWS):
*        Improved cleanup procedure after an error.
*     27-FEB-1992 (RFWS):
*        Adapted to handle comma-separated lists of output parameter
*        names.
*     18-OCT-1994 (PDRAPER):
*        Changed to use 'UPDATE' access when mapping the data component.
*        This allows a copy of the NDF to be made without also having to
*        copy the data.
*     16-NOV-1994 (PDRAPER):
*        Changed to handle input NDF data arrays being mapped for UPDATE
*        or WRITE access. Copies mapped data directly for efficiency
*        purposes.
*     28-NOV-1994 (PDRAPER):
*        Re-tracked slightly. Input NDFs can be readonly, no major
*        changes as existing code works for this case too.
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
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Global Variables:
      INCLUDE 'IMG_PCB'          ! IMG_ Parameter Control Block
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Read and Write)
*           NDF identifier.
*        PCB_PNTR( IMG__MXPAR ) = INTEGER (Write)
*           Pointer to mapped data.
*        PCB_TYPE( IMG__MXPAR ) = CHARACTER * ( * ) (Read and Write)
*           Record of data type used for mapping.

*  Arguments Given:
      CHARACTER * ( * ) PARAM1
      CHARACTER * ( * ) PARAM2
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      INTEGER PNTR( * )

*  External References:
      EXTERNAL IMG1_INIT         ! Initialise common blocks

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( IMG__SZPAR ) VPAR1 ! Validated input parameter
      CHARACTER * ( IMG__SZPAR ) VPAR2 ! Validated output parameter
      INTEGER EL                 ! Number of mapped array elements
      INTEGER F                  ! First character position
      INTEGER I1                 ! Position of start of field
      INTEGER I2                 ! Position of end of field
      INTEGER L                  ! Last character position
      INTEGER NPAR               ! Number of non-blank parameter names
      INTEGER SLOT1              ! Input PCB slot number
      INTEGER SLOT2              ! Output PCB slot number
      INTEGER TPNTR( 1 )         ! Temporary pointer variable
      LOGICAL NEW1               ! Input slot is new?
      LOGICAL NEW2               ! Output slot is new?
      LOGICAL MAPPED             ! Input NDF data array already mapped?

*.

*  Set a null value for the initial PNTR element.
      PNTR( 1 ) = IMG__NOPTR

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the input parameter name and find the parameter slot for
*  the input image.
      CALL IMG1_VPAR( PARAM1, VPAR1, STATUS )
      CALL IMG1_GTSLT( VPAR1, .FALSE., SLOT1, NEW1, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the output parameter count.
         NPAR = 0

*  Initialise the character pointer to the start of the output
*  parameter list.  Then loop to extract each element from the list.
         I1 = 1
 1       CONTINUE                ! Start of "DO WHILE" loop
         IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :        ( I1 .LE. LEN( PARAM2 ) ) ) THEN

*  Find the final character of the next element in the parameter list
*  (the last character before a comma or end of string).
            I2 = INDEX( PARAM2( I1 : ), ',' )
            IF ( I2 .EQ. 0 ) THEN
               I2 = LEN( PARAM2 )
            ELSE
               I2 = I2 + I1 - 2
            END IF
            IF ( I2 .GE. I1 ) THEN

*  Locate the first and last non-blank characters in the element,
*  checking that it is not entirely blank.
               CALL CHR_FANDL( PARAM2( I1 : I2 ), F, L )
               IF ( L .GE. F ) THEN
                  F = F + I1 - 1
                  L = L + I1 - 1

*  Increment the output parameter count and initialise the returned
*  pointer value.
                  NPAR = NPAR + 1
                  PNTR( NPAR ) = IMG__NOPTR

*  Validate the output parameter and find a slot for the output image.
*  Report an error if a new slot was not allocated, as this indicates
*  that the output parameter name is already in use.
                  CALL IMG1_VPAR( PARAM2( F : L ), VPAR2, STATUS )
                  CALL IMG1_GTSLT( VPAR2, .TRUE., SLOT2, NEW2, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( .NOT. NEW2 ) THEN
                        STATUS = IMG__PARIN
                        CALL ERR_REP( 'IMG1_PRNDF_OUT',
     :                                'The output parameter name ' //
     :                                '''^VPAR'' is already in use ' //
     :                                '(possible programming error).',
     :                                STATUS )

*  Propagate the input NDF to create a new one, saving the new
*  identifier in the output slot.
                     ELSE

*  Need to check if the data component of the NDF we're propagating is
*  mapped. If so we need to copy this to save re-mapping.
                        IF ( PCB_PNTR( SLOT1 ) .NE. IMG__NOPTR ) THEN

*  Data array already mapped so need to copy this by hand.
                           MAPPED = .TRUE.
                           CALL NDF_PROP( PCB_INDF( SLOT1 ),
     :                     'Title, Label, Quality, Axis, History',
     :                                    VPAR2, PCB_INDF( SLOT2 ),
     :                                    STATUS )
                        ELSE

*  Not mapped, just let the NDF library take care of this.
                           MAPPED = .FALSE.
                           CALL NDF_PROP( PCB_INDF( SLOT1 ),
     :                     'Title, Label, Data, Quality, Axis, History',
     :                                    VPAR2, PCB_INDF( SLOT2 ),
     :                                    STATUS )
                        END IF
                        IF ( STATUS .EQ. SAI__OK ) THEN

*  Map the data array of the new NDF for UPDATE access (zero if doesn't
*  exist).
                           CALL NDF_MAP( PCB_INDF( SLOT2 ), 'Data',
     :                                   TYPE, 'UPDATE/ZERO', TPNTR,
     :                                   EL, STATUS )

*  Now copy the data if this is necessary.
                           IF ( MAPPED ) THEN
                              CALL IMG1_CPY( PCB_PNTR( SLOT1 ),
     :                                       PCB_TYPE( SLOT1 ), EL,
     :                                       TYPE, TPNTR( 1 ), STATUS )
                           END IF

*  If successful, store the pointer to the mapped data in the PCB and
*  return its value. Also store the type used for mapping.
                           IF ( STATUS .EQ. SAI__OK ) THEN
                              PCB_PNTR( SLOT2 ) = TPNTR( 1 )
                              PNTR( NPAR ) = TPNTR( 1 )
                              PCB_TYPE( SLOT2 ) = TYPE

*  If an error occurred after creating the output NDF, then clean up by
*  deleting the new NDF and cancelling the parameter association. Do
*  this inside a new error reporting environment.
                           ELSE
                              CALL ERR_BEGIN( STATUS )
                              CALL NDF_DELET( PCB_INDF( SLOT2 ),
     :                                        STATUS )
                              CALL DAT_CANCL( VPAR2, STATUS )
                              CALL ERR_END( STATUS )
                           END IF
                        END IF

*  If an error occurred after allocating a new output PCB slot, then
*  release the slot.
                        IF ( STATUS .NE. SAI__OK ) THEN
                           CALL IMG1_FRSLT( SLOT2, .TRUE., STATUS )
                        END IF
                     END IF
                  END IF
               END IF
            END IF

*  Increment the character pointer to the start of the next element in
*  the output parameter list and return to process the next element.
            I1 = I2 + 2
            GO TO 1
         END IF

*  If no error has occurred, but no non-blank output parameter names
*  have been processed, then report an error.
         IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NPAR .EQ. 0 ) ) THEN
            STATUS = IMG__PARIN
            CALL ERR_REP( 'IMG1_PRNDF_NOPAR',
     :                    'No output parameter name specified ' //
     :                    '(possible programming error).', STATUS )
         END IF
      END IF

      END
* $Id$
