      SUBROUTINE IMG1_GTSLT( VPAR, NEW, SLOT, WASNEW, STATUS )
*+
*  Name:
*     IMG1_GTSLT

*  Purpose:
*     Get a slot number for a validated parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_GTSLT( VPAR, NEW, SLOT, WASNEW, STATUS )

*  Description:
*     The routine searches the PCB to identify a slot which refers to
*     the specified parameter, whose name has previously been
*     validated. If no such slot is found, then a new one will be
*     initialised if NEW is .TRUE., otherwise STATUS will be set and an
*     an appropriate error will be reported.

*  Arguments:
*     VPAR = CHARACTER * ( * ) (Given)
*        Validated parameter name (left justified, in upper case).
*     NEW = LOGICAL (Given)
*        If a slot already allocated to the parameter cannot be found,
*        then the routine will allocate a new one if NEW is .TRUE..
*        Otherwise, an error will be reported.
*     SLOT = INTEGER (Returned)
*        Number of the PCB slot associated with the parameter. A value
*        of zero will be returned if the parameter entry cannot be
*        found in the parameter control block and no new slot was
*        allocated.
*     WASNEW = LOGICAL (Returned)
*        Returns .TRUE. if a new slot was allocated for the parameter,
*        otherwise it returns .FALSE..
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     22-JAN-1991 (RFWS):
*        Original version.
*     17-FEB-1992 (RFWS):
*        Added the NEW and WASNEW arguments.
*     18-FEB-1992 (RFWS):
*        Added EXTERNAL statement for block data routine to ensure
*        common blocks are initialised.
*     20-FEB-1992 (RFWS):
*        Improved error message and test for first free slot.
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
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Write)
*           NDF identifier.
*        PCB_PARAM( IMG__MXPAR ) = CHARACTER * ( IMG__SZPAR ) (Read and
*        Write)
*           Parameter name.
*        PCB_PNTR( IMG__MXPAR ) = INTEGER (Write)
*           Pointer to mapped data.

*  Arguments Given:
      CHARACTER * ( * ) VPAR
      LOGICAL NEW

*  Arguments Returned:
      INTEGER SLOT
      LOGICAL WASNEW

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_INIT         ! Initialise common blocks

*  Local Variables:
      INTEGER I                  ! Loop counter for PCB entries
      INTEGER NEWSLT             ! First free slot number

*.

*  Set initial values for the SLOT and WASNEW arguments.
      SLOT = 0
      WASNEW = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop to search for the parameter.
      NEWSLT = 0
      DO 1 I = 1, IMG__MXPAR

*  Return the slot number of the first PCB entry whose parameter name
*  matches.
         IF ( PCB_PARAM( I ) .EQ. VPAR ) THEN
            SLOT = I
            GO TO 2

*  Also note the number of the first slot with a blank parameter name,
*  in case a new slot must be allocated.
         ELSE IF ( NEWSLT .EQ. 0 ) THEN
            IF ( PCB_PARAM( I ) .EQ. ' ' ) NEWSLT = I
         END IF
 1    CONTINUE
 2    CONTINUE

*  If the required parameter name was not found but a new slot can be
*  allocated, then see if a spare slot was available.
      IF ( SLOT .EQ. 0 ) THEN
         IF ( NEW ) THEN
            IF ( NEWSLT .NE. 0 ) THEN

*  If so, then set WASNEW to .TRUE., return the new slot number and
*  initialise the PCB entry.
               WASNEW = .TRUE.
               SLOT = NEWSLT
               PCB_INDF( SLOT ) = NDF__NOID
               PCB_PARAM( SLOT ) = VPAR
               PCB_PNTR( SLOT ) = IMG__NOPTR

*  If no new slot was available, then report an error.
            ELSE
               STATUS = IMG__XSPAR
               CALL MSG_SETI( 'MXPAR', IMG__MXPAR )
               CALL ERR_REP( 'IMG1_GTSLT_XS',
     :                       'The maximum number of ^MXPAR IMG_ ' //
     :                       'parameter names is already in use; no ' //
     :                       'room for new parameters.', STATUS )
            END IF

*  If no slot was found and allocation of a new slot is not required,
*  then report an error.
         ELSE
            STATUS = IMG__PARIN
            CALL MSG_SETC( 'VPAR', VPAR )
            CALL ERR_REP( 'IMG1_GTSLT_NO',
     :                    'The parameter name ''^VPAR'' is not ' //
     :                    'currently in use (possible programming ' //
     :                    'error).', STATUS )
         END IF
      END IF

      END
* $Id$
