      SUBROUTINE ARY1_CHCMA( IACB, MODE, STATUS )
*+
*  Name:
*     ARY1_CHCMA

*  Purpose:
*     Check for conflicting mapped access to an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_CHCMA( IACB, MODE, STATUS )

*  Description:
*     The routine checks that conflicting mapped access to an array
*     will not occur if the requested access is granted.  Separate
*     distinct regions of the same array may be individually mapped
*     with any access mode, and multiple mapped READ access to
*     overlapping regions of an array is also allowed. However, WRITE
*     or UPDATE access to any part of an array may not overlap
*     spatially with any other mapped access to the same array. The
*     routine reports an error if such a conflict will occur. Otherwise
*     it returns without action.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the ACB entry for the new array which is to be mapped.
*     MODE = CHARACTER * ( * ) (Given)
*        The access mode required
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior requirements:
*     -  The array mapping region information must first have been set
*     up in the MCB for the new array to be mapped.

*  Algorithm:
*     -  Check that a mapping transfer region exists for the requested
*     access; there can be no conflict otherwise.
*     -  Check whether any other mapping is currently associated with
*     the same data object.
*     -  If so, loop through all active ACB entries, selecting those
*     which are associated with the same data object and have active
*     mappings.
*     -  Ignore those cases where neither the entry being tested nor the
*     current request require that the data object be written to.
*     -  Test to see if the data transfer regions of potentially
*     conflicting mappings overlap spatially. If so, then a conflict
*     exists, so report an error.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JUN-1989 (RFWS):
*        Original version.
*     16-AUG-1989 (RFWS):
*        Added STATUS check after call to ARY1_NXTSL.
*     12-SEP-1989 (RFWS):
*        Improved error messsage.
*     2-MAR-1990 (RFWS):
*        Changed to use all ARY__MXDIM dimensions when comparing array
*        mapping transfer regions for intersection.
*     14-MAR-1990 (RFWS):
*        Changed ARY__CMAPA error code to ARY__CFLAC.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to data object.
*        DCB_NREAD( ARY__MXDCB ) = INTEGER (Read)
*           Number of current mappings with read access.
*        DCB_NWRIT( ARY__MXDCB ) = INTEGER (Read)
*           Number of current mappings with write access.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB 
*        ACB_IMCB( ARY__MXACB ) = INTEGER (Read)
*           Index to mapping entry in MCB

      INCLUDE 'ARY_MCB'          ! ARY_ Mapping Control Block
*        MCB_MTREX( ARY__MXMCB ) = LOGICAL (Read)
*           Whether a mapping transfer region exists.
*        MCB_AMM( ARY__MXMCB ) = CHARACTER * ( ARY__SZAMM ) (Read)
*           Active mapping mode.
*        MCB_LMTR( ARY__MXDIM, ARY__MXMCB ) = INTEGER (Read)
*           Lower bound of mapping transfer region.
*        MCB_UMTR( ARY__MXDIM, ARY__MXMCB ) = INTEGER (Read)
*           Upper bound of mapping transfer region.

*  Arguments Given:
      INTEGER IACB
      CHARACTER * ( * ) MODE

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local variables:
      CHARACTER * ( ARY__SZMOD ) UMODE ! Upper case access mode string
      INTEGER IACBT              ! Index of ACB entry to test
      INTEGER IDCB               ! Index to data object entry in DCB
      INTEGER IDCBT              ! Index of DCB entry to test
      INTEGER IMCB               ! Index to mapping entry in MCB
      INTEGER IMCBT              ! Index of MCB entry to test
      INTEGER LX( ARY__MXDIM )   ! Lower bound of intersection region
      INTEGER NEXT               ! Next ACB slot number
      INTEGER UX( ARY__MXDIM )   ! Upper bound of intersection region
      LOGICAL ISECT              ! Whether array bounds intersect
      LOGICAL NEEDWR             ! Whether write access is needed

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  The can be no conflict if the requested mapping does not have a
*  mapping transfer region.
      IMCB = ACB_IMCB( IACB )
      IF ( MCB_MTREX( IMCB ) ) THEN

*  Find the number of current read and write mapped accesses. There can
*  be no conflict if these numbers are zero.
         IDCB = ACB_IDCB( IACB )
         IF ( ( DCB_NWRIT( IDCB ) .GT. 0 ) .OR.
     :        ( DCB_NREAD( IDCB ) .GT. 0 ) ) THEN

*  Decide if the requested mapping will need to write to the data
*  object.
            NEEDWR = CHR_SIMLR( MODE, 'WRITE' ) .OR.
     :               CHR_SIMLR( MODE, 'UPDATE' )

*  Loop to inspect all other active entries in the ACB for possible
*  conflicts.
            NEXT = 0
            IACBT = 0
1           CONTINUE
            CALL ARY1_NXTSL( ARY__ACB, IACBT, NEXT, STATUS )
            IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NEXT .NE. 0 ) ) THEN
               IACBT = NEXT

*  Get the DCB and MCB indices for each entry being tested.
               IDCBT = ACB_IDCB( IACBT )
               IMCBT = ACB_IMCB( IACBT )

*  Select entries which differ from the one requesting access, but which
*  refer to the same actual data object and currently have data mapped.
               IF ( ( IACBT .NE. IACB ) .AND.
     :              ( IDCBT .EQ. IDCB ) .AND.
     :              ( IMCBT .NE. 0 ) ) THEN

*  There can only be a conflict if the ACB entry being tested has a
*  mapping transfer window and either the requested mapping, or the one
*  being tested needs to write to the data object.
                  IF ( ( MCB_MTREX( IMCBT ) ) .AND.
     :                 ( NEEDWR .OR.
     :                   ( MCB_AMM( IMCBT ) .EQ. 'WRITE' ) .OR.
     :                   ( MCB_AMM( IMCBT ) .EQ. 'UPDATE' ) ) ) THEN

*  If there may still be a conflict, then see if the contending mapping
*  transfer regions overlap spatially.
                     CALL ARY1_XSBND( ARY__MXDIM, MCB_LMTR( 1, IMCB ),
     :                                MCB_UMTR( 1, IMCB ),
     :                                ARY__MXDIM, MCB_LMTR( 1, IMCBT ),
     :                                MCB_UMTR( 1, IMCBT ),
     :                                ARY__MXDIM, LX, UX, ISECT,
     :                                STATUS )

*  If they intersect, then there is a conflict, so report an error.
                     IF ( ISECT ) THEN
                        STATUS = ARY__CFLAC
                        CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                        UMODE = MODE
                        CALL CHR_UCASE( UMODE )
                        CALL MSG_SETC( 'MODE', MODE )
                        CALL ERR_REP( 'ARY1_CHCMA_ERR',
     :                  'Requested ^MODE access to the array ^ARRAY ' //
     :                  'conflicts with existing mapped access to ' //
     :                  'the same data object (possible programming ' //
     :                  'error).', STATUS )
                        GO TO 9999
                     END IF
                  END IF
               END IF
               GO TO 1
            END IF
         END IF
      END IF
9999  CONTINUE
             
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_CHCMA', STATUS )

      END
