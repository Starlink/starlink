      SUBROUTINE IRQ1_ADD( LOCS, SLOT, QNAME, DEFLT, COMMNT, STATUS )
*+
*  Name:
*     IRQ1_ADD

*  Purpose:
*     Add a new quality name to the QUALITY_NAMES structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_ADD( LOCS, SLOT, QNAME, DEFLT, COMMNT, STATUS )

*  Description:
*     The supplied quality name is stored in the QUAL array at the
*     index specified by SLOT. This slot must not be currently in use
*     (i.e.  the NAME must be equal to the value of symbolic constant
*     IRQ__SFREE). If the slot is currently in use an error is
*     reported. The quality is initialy assumed to be held by all or
*     none of the NDF pixels (as determined by DEFLT). Therefore FIXED
*     is given a true value, and VALUE is set equal to DEFLT. The
*     associated QUALITY bit is set to the illegal value of zero. The
*     quality name is mad "read-write" (i.e. it can be deleted using
*     IRQ_REMQN). The number of free slots in QUAL is decremented, and
*     if necessary, the index of the last used slot in QUAL is modified.

*  Arguments:
*     LOCS( 5 ) = CHARACTER * ( * ) (Given)
*        A set of 5 HDS locators. LOCS( 1 ) locates a temporary
*        structure holding a cloned NDF identifier. LOCS(2) locates the
*        QUAL array. LOCS(3) locates the LAST_USED value, holding the
*        index of the last used slot in the QUAL array. LOCS(4) locates
*        the NFREE value, holding the number of free slots in the QUAL
*        array. LOCS(5) locates the FREE array, which contains a stack
*        of the NFREE slot indices corresponding to free slots. This
*        stack is accessed in a "First-In-Last-Out" method.
*     SLOT = INTEGER (Given)
*        The index at which to store the name in the QUAL array.
*     QNAME = CHARACTER * ( * ) (Given)
*        The quality name.
*     DEFLT = LOGICAL (Given)
*        If true, then the quality is assumed to be held by all pixels
*        by default.  If false, then the quality is assumed to be held
*        by no pixels.
*     COMMNT = CHARACTER * ( * ) (Given)
*        A descriptive comment to store with the name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-JUL-1991 (DSB):
*        Original version.
*     15-FEB-2008 (DSB):
*        Added a read-only flag to the QUAL structure.
*     4-MAR-2008 (DSB):
*        Added a "fixed bit" flag to the QUAL structure.
*     24-APR-2016 (DSB):
*        Ensure that the comment string passed to CMP_PUT0C is
*        not above the maximum allowed length. Truncate it if
*        it is to avoid an error being reported that would
*        usually cause the application to abort.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Arguments Given:
      CHARACTER LOCS(5)*(*)
      INTEGER SLOT
      CHARACTER QNAME*(*)
      LOGICAL DEFLT
      CHARACTER COMMNT*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER LAST               ! The last free slot given in the list
                                 ! of free slots.
      INTEGER LUSED              ! Index of last used slot in QUAL.
      INTEGER NFREE              ! No. of free slots in QUAL.
      CHARACTER * ( IRQ__SZCOM ) LCOMM ! Local copy of comment
      CHARACTER * ( IRQ__SZQNM ) OLDNAM ! Current NAME string stored in
                                 ! the specified slot of QUAL.
      CHARACTER * ( DAT__SZLOC ) QCLOC ! Locator to the requested cell
                                 ! in the QUAL array.
      CHARACTER * ( DAT__SZLOC ) TLOC ! Temporary HDS locator.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check for reserved names.
      IF( QNAME .EQ. 'ANY' .OR.
     :    QNAME .EQ. IRQ__SFREE .OR.
     :    QNAME .EQ. IRQ__SBAD ) THEN

         STATUS = IRQ__BADNM
         CALL MSG_SETC( 'NAM', QNAME )
         CALL ERR_REP( 'IRQ1_ADD_ERR1',
     : 'IRQ1_ADD: The string "^NAM" cannot be used as a quality name.',
     :                 STATUS )
         GO TO 999

      END IF

*  Check for names containing full stops.
      IF( INDEX( QNAME, '.' ) .NE. 0 ) THEN

         STATUS = IRQ__BADNM
         CALL MSG_SETC( 'NAM', QNAME )
         CALL ERR_REP( 'IRQ1_ADD_ERR1',
     : 'IRQ1_ADD: The name "^NAM" contains the illegal character "."',
     :                 STATUS )
         GO TO 999

      END IF

*  Get a locator to the requested slot in the QUAL array.
      CALL DAT_CELL( LOCS(2), 1, SLOT, QCLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the current value of the NAME.
      CALL CMP_GET0C( QCLOC, IRQ__NMNAM, OLDNAM, STATUS )

*  If the name could not be obtained, signal an incomplete structure.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         OLDNAM = IRQ__SBAD
      END IF

*  Report an error if the slot has an incomplete structure.
      IF( OLDNAM .EQ. IRQ__SBAD ) THEN
         STATUS = IRQ__BADST
         CALL ERR_REP( 'IRQ1_ADD_ERR2',
     :                 'IRQ1_ADD: Incomplete slot structure found',
     :                 STATUS )

*  Report an error if the slot is currently in use.
      ELSE IF( OLDNAM .NE. IRQ__SFREE ) THEN
         STATUS = IRQ__INTER
         CALL MSG_SETC( 'OLD', OLDNAM )
         CALL ERR_REP( 'IRQ1_ADD_ERR3',
     :'IRQ1_ADD: Internal IRQ error. Attempt to overwrite name ^OLD',
     :                 STATUS )
      END IF

*  Ensure the comment is no longer than IRQ__SZCOM (CMP_PUT0C will report
*  an error if it is).
      LCOMM = COMMNT

*  Store the new values.
      CALL CMP_PUT0C( QCLOC, IRQ__NMNAM, QNAME, STATUS )
      CALL CMP_PUT0L( QCLOC, IRQ__FXNAM, .TRUE., STATUS )
      CALL CMP_PUT0L( QCLOC, IRQ__VLNAM, DEFLT, STATUS )
      CALL CMP_PUT0C( QCLOC, IRQ__CMNAM, LCOMM, STATUS )
      CALL CMP_PUT0I( QCLOC, IRQ__BTNAM, 0, STATUS )
      CALL CMP_PUT0L( QCLOC, IRQ__RONAM, .FALSE., STATUS )
      CALL CMP_PUT0L( QCLOC, IRQ__FBNAM, .FALSE., STATUS )

*  If neccessary, update the last used slot number.
      CALL DAT_GET0I( LOCS(3), LUSED, STATUS )
      IF( SLOT .GT. LUSED ) CALL DAT_PUT0I( LOCS(3), SLOT, STATUS )

*  Decrement the number of free slots.
      CALL DAT_GET0I( LOCS(4), NFREE, STATUS )
      CALL DAT_PUT0I( LOCS(4), NFREE - 1, STATUS )

*  Check that the slot was the last one on the free slot list, and
*  replace it with zero.
      CALL DAT_CELL( LOCS(5), 1, NFREE, TLOC, STATUS )
      CALL DAT_GET0I( TLOC, LAST, STATUS )
      CALL DAT_PUT0I( TLOC, 0, STATUS )
      CALL DAT_ANNUL( TLOC, STATUS )

      IF( LAST .NE. SLOT .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRQ__INTER
         CALL MSG_SETI( 'L', LAST )
         CALL MSG_SETI( 'S', SLOT )
         CALL ERR_REP( 'IRQ1_ADD_ERR4',
     :                 'IRQ1_ADD: Internal IRQ error (^L,^S)',
     :                 STATUS )
      END IF

*  Annul the locator to the QUAL cell.
 999  CONTINUE

      CALL DAT_ANNUL( QCLOC, STATUS )

      END
