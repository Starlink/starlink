      SUBROUTINE CCD1_MKTMP( QUAN, TYPE, ID, STATUS )
*+
*  Name:
*     CCD1_MKTMP

*  Purpose:
*     Creates a temporary HDS object for use as workspace.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MKTMP( TYPE, SIZE, ID, STATUS )

*  Description:
*     Use this routine when allocating non-NDF temporary disk-file
*     space in CCDPACK. Access is given to vectorised quantities of any
*     numeric type. This routine just returns an identifier to the
*     workspace which may be used with the functions.
*
*         CCD1_MPTMP   - which maps in the workspace
*         CCD1_UNTMP   - which unmaps the workspace
*         CCD1_FRTMP   - which frees the workspace (unmapping and
*                        releasing the locator)
*
*     Only use these routines or not all at all. The UNTMP and FRTMP
*     routines accept -1 as their first argument (instead of an
*     identifier) this either unmaps ALL workspace or unmaps and annuls
*     ALL workspace (no nesting). This routine should always be called
*     at least once before using the deallocation routines.

*  Arguments:
*     QUAN = INTEGER (Given)
*        The quantity of workspace required in units of TYPE.
*     TYPE = CHARACTER * ( * ) (Given)
*        The numeric type of the workspace which is required. Can only
*        be one of _BYTE, _UBYTE, _WORD, _UWORD, _INTEGER, _INT64, _REAL,
*        _DOUBLE and _LOGICAL.
*     ID = INTEGER (Returned)
*        The identifier to the quantity of workspace.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-MAY-1993 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameterisations
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations

*  Global Variables:
      INCLUDE 'CCD1_TMPCM'       ! Temporary workspace common block
*        CCD1_TMPPO( CCD1__MXPNT ) = INTEGER (Write)
*           Array of pointers to any data which is allocated. This is
*           updated with the value of the pointer on mapping and the
*           value is removed on unmapping.
*        CCD1_TMPLO( CCD1__MXPNT ) = CHARACTER (Write)
*           Array of locators to any data objects created by this
*           routine. This is updated when objects are annulled.

*  Arguments Given:
      INTEGER QUAN
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      INTEGER ID

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      LOGICAL FIRST              ! True on first call of this routine
                                 ! or when common block is cleared
      INTEGER I                  ! Loop variable
      INTEGER SLOT               ! Position in pointer common block to
                                 ! store current reference

*  Save the value of first for all time
      SAVE FIRST

*  Local Data:
      DATA FIRST / .TRUE. /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set default identifier value.
      ID = -1

*  If FIRST is true then initialise the common block.
      IF ( FIRST ) THEN
         DO 1 I = 1, CCD1__MXPNT
            CCD1_TMPPO( I ) = -1
            CCD1_TMPLO( I ) = DAT__NOLOC
 1       CONTINUE

*  Set free slot to 1
         SLOT = 1

*  Stop future initialisations until all workspace is released.
         FIRST = .FALSE.
      ELSE

*  Look for a free slot in the common block
         SLOT = -1
         DO 2 I = 1, CCD1__MXPNT
            IF ( CCD1_TMPLO( I ) .EQ. DAT__NOLOC ) THEN
               SLOT = I
               GO TO 3
            END IF
 2       CONTINUE

*  Arrive here if no SLOT is found. Issue error message.
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_MKTMP1',
     :      '  CCD1_MKTMP: Cannot allocate any further workspace'//
     :      ' internal resources exhausted', STATUS )
            GO TO 99

*  Arrive here if a slot has been found.
 3       CONTINUE
      END IF

*  Try allocating the required amount of workspace
*  Check the input value for sense.
      IF ( QUAN .GT. 0 ) THEN

*  Attempt to create the temporary object.
         CALL AIF_TEMP( TYPE, 1, QUAN, CCD1_TMPLO( SLOT ), STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Failed to access the workspace - set status and exit.
            CALL MSG_SETI( 'QUAN', QUAN )
            CALL MSG_SETC( 'TYPE', TYPE )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_MKTMP2',
     :      '  CCD1_MKTMP: Could not create disk resident workspace'//
     :      ' (^QUAN elements of type ^TYPE)', STATUS )

*  Set pointer to locator back to null.
            CCD1_TMPLO( SLOT ) = DAT__NOLOC
            GO TO 99
         ELSE

*  Created object, pass slot no back as identifier.
            ID = SLOT
         END IF
      ELSE

*  Invalid input value - set status and exit.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_MALL3',
     :   '  CCD1_MALL: Requested workspace less than 1 element',
     :   STATUS )
         GO TO 99
      END IF

 99   CONTINUE
      END
* $Id$
