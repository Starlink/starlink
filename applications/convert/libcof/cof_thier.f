      SUBROUTINE COF_THIER( SNAME, LOC, FUNIT, STATUS )
*+
*  Name:
*     COF_THIER

*  Purpose:
*     Converts an HDS object hierarchy into FITS binary tables.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_THIER( SNAME, LOC, FUNIT, STATUS )

*  Description:
*     The routine recursively descends an HDS object hierarchy,
*     and converts each structure in a single-row binary table.

*  Arguments:
*     SNAME = CHARACTER * ( * ) (Given)
*        The name of structure.  It is used to form the EXTNAME
*        keyword.
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        Locator for the object or structure whose contents are to be
*        converted.
*     FUNIT = INTEGER (Given)
*        The logical unit number of the output FITS file.
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

*  Prior Requirements:
*     -  A primary HDU unit exists in the FITS file, and the file is
*     open.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1997, 2002 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1994 June 18 (MJC):
*        Original version.
*     1997 November 15 (MJC):
*        Added support for primitive objects.
*     2002 March 13 (AJC):
*        Pass unvectorised parent to COF_H2BIN to get correct shape.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants

*  Arguments Given:
      CHARACTER * ( * ) SNAME
      CHARACTER * ( * ) LOC
      INTEGER FUNIT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXSTK              ! Recursion stack size
      PARAMETER ( MXSTK = 200 )

*  Local Variables:
      INTEGER DIM( 1 )           ! Cell index
      INTEGER EL( MXSTK )        ! Number of array elements
      INTEGER EXTLEV             ! Extension level
      INTEGER ICMP( MXSTK )      ! Component index
      INTEGER IEL( MXSTK )       ! Array element index
      CHARACTER * ( DAT__SZLOC ) LCELL( MXSTK ) ! Array cell locator
      CHARACTER * ( DAT__SZLOC ) LCMP( MXSTK ) ! Component locator
      CHARACTER * ( DAT__SZLOC ) LSTART( MXSTK ) ! Initial locator
      CHARACTER * ( DAT__SZLOC ) LVEC( MXSTK ) ! Vectorised locator
      INTEGER NCMP( MXSTK )      ! Number of structure components
      LOGICAL PRIM               ! Object primitive?
      LOGICAL SARRAY( MXSTK )    ! Object an array of structures?
      INTEGER STK                ! Recursion stack pointer
      LOGICAL WRITTN             ! True if the binary table is written

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITS extension level.
      EXTLEV = 1

*  Initialise the stack pointer and copy the input locator.
      STK = 1
      LSTART( STK ) = LOC

*  Initialise the primitive-object flags.
      CALL DAT_PRIM( LOC, PRIM, STATUS )

*  Convert the primitive component to a one-column, one-line binary
*  table.
      IF ( PRIM ) THEN
         CALL COF_H2BIN( LOC, DAT__NOLOC, FUNIT, EXTLEV, ' ', WRITTN,
     :                   STATUS )
      ELSE

*  Basic algorithm.
*  ================

*  An invocation of the basic algorithm starts here.  Check the
*  inherited status.
   1     CONTINUE
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Vectorise the structure, and determine how many elements it has.
            CALL DAT_VEC( LSTART( STK ), LVEC( STK ), STATUS )
            CALL DAT_SIZE( LVEC( STK ), EL( STK ), STATUS )

*  Test to see if it is a structure array.
            SARRAY( STK ) = EL( STK ) .GT. 1

*  Loop to process each array element.
            IEL( STK ) = 0

*  2 is the start of a "DO WHILE" loop.
   2        CONTINUE
            IF ( ( IEL( STK ) .LT. EL( STK ) ) .AND.
     :           ( STATUS .EQ. SAI__OK ) ) THEN
               IEL( STK ) = IEL( STK ) + 1

*  Obtain a locator for each array cell.
               DIM( 1 ) = IEL( STK )
               CALL DAT_CELL( LVEC( STK ), 1, DIM, LCELL( STK ),
     :                        STATUS )

*  Trace through the primitive elements and create a FITS binary table
*  for them.
               CALL COF_H2BIN( LCELL( STK ), LSTART( STK ), FUNIT,
     :                         EXTLEV, ' ', WRITTN, STATUS )

*  Although some processing could be avoid if the structure does not
*  contain any further structures, it is easier to follow if each is
*  searched, and the extension levels are adjusted.
*
*  Find the number of components within the structure.
               CALL DAT_NCOMP( LCELL( STK ), NCMP( STK ), STATUS )

*  Loop through all objects within this structure to find the next
*  structure object to get a locator to it.  Keep a record of the
*  current component location on a stack.
               ICMP( STK ) = 0

*  Start of a "DO WHILE" loop.
   3           CONTINUE
               IF ( ( ICMP( STK ) .LT. NCMP( STK ) ) .AND.
     :              ( STATUS .EQ. SAI__OK ) ) THEN

*  Increment the current component location for the current structure.
                  ICMP( STK ) = ICMP( STK ) + 1
                  CALL DAT_INDEX( LCELL( STK ), ICMP( STK ),
     :                            LCMP( STK ), STATUS )

*  Determine whether or not it is primitive.
                  CALL DAT_PRIM( LCMP( STK ), PRIM, STATUS )
                  IF ( .NOT. PRIM ) THEN

*  We must now recursively invoke the original algorithm to convert the
*  resulting object (which may be a primitive, a structure or a
*  structure array).  Check that the stack pointer will not overflow.
                     IF ( STK .GE. MXSTK ) THEN
                        STATUS = SAI__ERROR
                        CALL DAT_MSG( 'OBJECT', LCMP( STK ) )
                        CALL MSG_SETI( 'MXSTK', MXSTK )
                        CALL ERR_REP( 'COF_THIER_2DEEP',
     :   'Unable to convert the HDS object ^OBJECT to a FITS ' //
     :   'binary-table---the object is nested more than ^MXSTK ' //
     :   'levels deep.', STATUS )

                     ELSE

*  Copy the component locator for use as the initial locator in the
*  next invocation.  Increment the extension level.  Then increment the
*  stack pointer and branch back to the start.
                        LSTART( STK + 1 ) = LCMP( STK )
                        STK = STK + 1
                        EXTLEV = EXTLEV + 1
                        GO TO 1

*  Arrive back here after returning from a recursive invocation of the
*  algorithm. Decrement the stack pointer and extension level.
   4                    CONTINUE
                        STK = STK - 1
                        EXTLEV = EXTLEV - 1

                     END IF
                  END IF

*  Annul the component locator.
                  CALL DAT_ANNUL( LCMP( STK ), STATUS )

*  Return to process the next component,
                  GO TO 3
               END IF   ! End of loop

*  Annul the array element locator and return to process the next
*  element.
               CALL DAT_ANNUL( LCELL( STK ), STATUS )
               GO TO 2
            END IF         ! End of loop

*  Annul the vectorised array locator.
            CALL DAT_ANNUL( LVEC( STK ), STATUS )
         END IF

*  After completing an invocation of the basic algorithm, decrement the
*  stack pointer and return to the point where it was invoked. If this
*  is the top-level invocation, then exit.
         IF ( STK .GT. 1 ) GO TO 4

*  Return the final locator.  ***Why is this here?  LOC is "Given". If
*  LOC might be annulled, better to clone it.***
         LOC = LSTART( STK )
      END IF

*  Exit point if something has gone wrong.
  999 CONTINUE

      END
