      SUBROUTINE IMG1_NWNDF( PARAM, TYPE, NDIM, DIM, PNTR, STATUS )
*+
*  Name:
*     IMG1_NWNDF

*  Purpose:
*     Create new NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_NWNDF( PARAM, TYPE, NDIM, DIM, PNTR, STATUS )

*  Description:
*     This routine creates new NDFs for output. For each parameter name
*     supplied, it creates an NDF of the specified numeric type,
*     associates it with the parameter and returns a pointer to its
*     data, mapped for WRITE access using the same numeric type. The
*     data values are not initialised.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Comma-separated list of parameter names (case insensitive).
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric type of the NDFs to be created.
*     NDIM = INTEGER (Given)
*        Number of NDF dimensions.
*     DIM( NDIM ) = INTEGER (Given)
*        NDF dimension sizes.
*     PNTR( * ) = INTEGER (Given)
*        Pointers to the NDFs' mapped data arrays, one for each
*        parameter supplied.
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
*     21-FEB-1992 (RFWS):
*        Original version.
*     27-FEB-1992 (RFWS):
*        Adapted to handle comma-separated lists of parameter names.
*     8-SEP-1994 (PDRAPER):
*        Change map mode to 'WRITE/ZERO' (from 'WRITE').
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
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Global Variables:
      INCLUDE 'IMG_PCB'          ! IMG_ Parameter Control Block
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Write)
*           NDF indentifier.
*        PCB_PNTR( IMG__MXPAR ) = INTEGER (Write)
*           Pointer to mapped data array.
*        PCB_TYPE( IMG__MXPAR ) = CHARACTER *( * ) (Write)
*           Record of data type used for mapping.

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) TYPE
      INTEGER NDIM
      INTEGER DIM( NDIM )

*  Arguments Returned:
      INTEGER PNTR( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_INIT         ! Initialise common blocks

*  Local Variables:
      CHARACTER * ( IMG__SZPAR ) VPAR ! Validated parameter name
      INTEGER EL                 ! No. of array elements mapped
      INTEGER F                  ! First character position
      INTEGER I1                 ! Position of start of field
      INTEGER I2                 ! Position of end of field
      INTEGER L                  ! Last character position
      INTEGER NPAR               ! Number of non-blank parameter names
      INTEGER SLOT               ! PCB slot allocated
      INTEGER TPNTR( 1 )         ! Temporary pointer variable
      LOGICAL WASNEW             ! New PCB slot allocated?

*.

*  Set a null value for the initial PNTR element.
      PNTR( 1 ) = IMG__NOPTR

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the parameter count.
      NPAR = 0

*  Initialise the character pointer to the start of the parameter list.
*  Then loop to extract each element from the parameter list.
      I1 = 1
 1    CONTINUE                   ! Start of "DO WHILE" loop
      IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :     ( I1 .LE. LEN( PARAM ) ) ) THEN

*  Find the final character of the next element in the parameter list
*  (the last character before a comma or end of string).
         I2 = INDEX( PARAM( I1 : ), ',' )
         IF ( I2 .EQ. 0 ) THEN
            I2 = LEN( PARAM )
         ELSE
            I2 = I2 + I1 - 2
         END IF
         IF ( I2 .GE. I1 ) THEN

*  Locate the first and last non-blank characters in the element,
*  checking that it is not entirely blank.
            CALL CHR_FANDL( PARAM( I1 : I2 ), F, L )
            IF ( L .GE. F ) THEN
               F = F + I1 - 1
               L = L + I1 - 1

*  Increment the parameter count and initialise the returned pointer
*  value.
               NPAR = NPAR + 1
               PNTR( NPAR ) = IMG__NOPTR

*  Validate the parameter name and allocate a new PCB slot for it.
               CALL IMG1_VPAR( PARAM( F : L ), VPAR, STATUS )
               CALL IMG1_GTSLT( VPAR, .TRUE., SLOT, WASNEW, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that a new slot was allocated. If not, then the parameter name
*  is already in use, so report an error.
                  IF ( .NOT. WASNEW ) THEN
                     STATUS = IMG__PARIN
                     CALL ERR_REP( 'IMG1_NWNDF_OUT',
     :                             'The parameter name ''^VPAR'' ' //
     :                             'is already in use (possible ' //
     :                             'programming error).', STATUS )

*  Create a new NDF.
                  ELSE
                     CALL NDF_CREP( VPAR, TYPE, NDIM, DIM,
     :                              PCB_INDF( SLOT ), STATUS )

*  Map the NDF's data array for writing.
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        CALL NDF_MAP( PCB_INDF( SLOT ), 'Data', TYPE,
     :                                'WRITE/ZERO', TPNTR, EL, STATUS )

*  If successful, store the pointer value in the PCB and return its
*  value. Also record the data type.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           PCB_PNTR( SLOT ) = TPNTR( 1 )
                           PNTR( NPAR ) = TPNTR( 1 )
                           PCB_TYPE( SLOT ) = TYPE

*  If an error occurred after acquiring the NDF identifier, then annul
*  the identifier and cancel the parameter association.  Do all this
*  inside a new error reporting context, since we are cleaning up after
*  a previous error.
                        ELSE
                           CALL ERR_BEGIN( STATUS )
                           CALL NDF_ANNUL( PCB_INDF( SLOT ), STATUS )
                           CALL DAT_CANCL( VPAR, STATUS )
                           CALL ERR_END( STATUS )
                        END IF
                     END IF

*  If an error occurred after allocating a new PCB slot, then release
*  the slot.
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL IMG1_FRSLT( SLOT, .TRUE., STATUS )
                     END IF
                  END IF
               END IF
            END IF
         END IF

*  Increment the character pointer to the start of the next element in
*  the parameter list and return to process the next element.
         I1 = I2 + 2
         GO TO 1
      END IF

*  If no error has occurred, but no non-blank parameter names have been
*  processed, then report an error.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NPAR .EQ. 0 ) ) THEN
         STATUS = IMG__PARIN
         CALL ERR_REP( 'IMG1_NWNDF_NOPAR',
     :                 'No parameter name specified (possible ' //
     :                 'programming error).', STATUS )
      END IF

      END
* $Id$
