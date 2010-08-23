      SUBROUTINE KPG1_NACVT( LOC, STATUS )
*+
*  Name:
*     KPG1_NACVT

*  Purpose:
*     Converts an HDS object hierarchy to native data representation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_NACVT( LOC, STATUS )

*  Description:
*     The routine recursively descends an HDS object hierarchy,
*     converting any primitive objects within it to have the
*     appropriate native data representation, as provided by the host
*     machine. This will minimise subsequent access time for this
*     machine.

*  Arguments:
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given and Returned)
*        Locator for the object or structure whose contents are to be
*        converted. If the object is primitive, then this locator may
*        be replaced by a new one on output (as the object may need to
*        be erased and re-created).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     The routine implements a recursive algorithm to descend the HDS
*     structure, recursion being implemented by saving current values
*     on a stack and returning to the start of the routine. A
*     subsequent return from the recursively-invoked algorithm then
*     involves returning to the centre of the routine (following the
*     point of invocation) and popping the stack. This requires
*     branching back into the range of several loops, so all looping is
*     in this routine is implemented using GO TO statements rather than
*     DO loops.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     5-MAY-1992 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants

*  Arguments Given and Returned:
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXSTK              ! Recursion stack size
      PARAMETER ( MXSTK = 200 )

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LCELL( MXSTK ) ! Array cell locator
      CHARACTER * ( DAT__SZLOC ) LCMP( MXSTK ) ! Component locator
      CHARACTER * ( DAT__SZLOC ) LSTART( MXSTK ) ! Initial locator
      CHARACTER * ( DAT__SZLOC ) LVEC( MXSTK ) ! Vectorised locator
      INTEGER DIM( 1 )           ! Cell index
      INTEGER EL( MXSTK )        ! Number of array elements
      INTEGER ICMP( MXSTK )      ! Component index
      INTEGER IEL( MXSTK )       ! Array element index
      INTEGER STK                ! Recursion stack pointer
      INTEGER NCMP( MXSTK )      ! Number of structure components
      LOGICAL PRIM               ! Object primitive?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the stack pointer and copy the input locator.
      STK = 1
      LSTART( STK ) = LOC

*  An invocation of the basic algorithm starts here. Check the
*  inherited status.
 1    CONTINUE
      IF ( STATUS .EQ. SAI__OK ) THEN

*  See if the initial object is primitive. If so, then convert it.
         CALL DAT_PRIM( LSTART( STK ), PRIM, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( PRIM ) THEN
               CALL KPG1_PRCVT( LSTART( STK ), STATUS )

*  If it is a structure or a structure array, then vectorise it and
*  determine how many elements it has.
            ELSE
               CALL DAT_VEC( LSTART( STK ), LVEC( STK ), STATUS )
               CALL DAT_SIZE( LVEC( STK ), EL( STK ), STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop to process each array element.
                  IEL( STK ) = 0
 2                CONTINUE       ! Start of "DO WHILE" loop
                  IF ( ( IEL( STK ) .LT. EL( STK ) ) .AND.
     :                 ( STATUS .EQ. SAI__OK ) ) THEN
                     IEL( STK ) = IEL( STK ) + 1

*  Obtain a locator for each array cell and determine how many
*  components it has.
                     DIM( 1 ) = IEL( STK )
                     CALL DAT_CELL( LVEC( STK ), 1, DIM, LCELL( STK ),
     :                              STATUS )
                     CALL DAT_NCOMP( LCELL( STK ), NCMP( STK ), STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop to process each structure component, obtaining a locator for
*  it.
                        ICMP( STK ) = 0
 3                      CONTINUE ! Start of "DO WHILE" loop
                        IF ( ( ICMP( STK ) .LT. NCMP( STK ) ) .AND.
     :                       ( STATUS .EQ. SAI__OK ) ) THEN
                           ICMP( STK ) = ICMP( STK ) + 1
                           CALL DAT_INDEX( LCELL( STK ), ICMP( STK ),
     :                                     LCMP( STK ), STATUS )

*  We must now recursively invoke the original algorithm to convert the
*  resulting object (which may be a primitive, a structure or a
*  structure array).  Check that the stack pointer will not overflow.
                           IF ( STK .GE. MXSTK ) THEN
                              STATUS = SAI__ERROR
                              CALL DAT_MSG( 'OBJECT', LCMP( STK ) )
                              CALL MSG_SETI( 'MXSTK', MXSTK )
                              CALL ERR_REP( 'KPG1_NACVT_2DEEP',
     :   'Unable to convert the HDS object ^OBJECT to native ' //
     :   'data representation - object is nested more than ^MXSTK ' //
     :   'levels deep.', STATUS )
                           ELSE

*  Copy the component locator for use as the initial locator in the
*  next invocation. Then increment the stack pointer and branch back to
*  the start.
                              LSTART( STK + 1 ) = LCMP( STK )
                              STK = STK + 1
                              GO TO 1

*  Arrive back here after returning from a recursive invocation of the
*  algorithm. Decrement the stack pointer.
 4                            CONTINUE
                              STK = STK - 1

*  See if the object's locator has been changed. If so, then it was a
*  primitive which has been converted, so decrement the component count
*  and its upper limit (this accounts for the fact that this component
*  will now be at the end of the component list).
                              IF ( LSTART( STK + 1 ) .NE.
     :                             LCMP( STK ) ) THEN
                                 ICMP( STK ) = ICMP( STK ) - 1
                                 NCMP( STK ) = NCMP( STK ) - 1
                              END IF

*  Annul the (new) componebnt locator.
                              CALL DAT_ANNUL( LSTART( STK + 1 ),
     :                                        STATUS )
                           END IF

*  Return to process the next component,
                           GO TO 3
                        END IF   ! End of loop
                     END IF

*  Annul the array element locaator and return to process the next
*  element.
                     CALL DAT_ANNUL( LCELL( STK ), STATUS )
                     GO TO 2
                  END IF         ! End of loop

*  Annul the vectorised array locator.
               END IF
               CALL DAT_ANNUL( LVEC( STK ), STATUS )
            END IF
         END IF
      END IF

*  After completing an invocation of the basic algorithm, decrement the
*  stack pointer and return to the point where it was invoked. If this
*  is the top-level invocation, then exit.
      IF ( STK .GT. 1 ) GO TO 4

*  Return the final locator.
      LOC = LSTART( STK )

      END
