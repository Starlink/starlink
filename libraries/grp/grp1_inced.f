      SUBROUTINE GRP1_INCED( SLOT2, FIRST, LAST, GRPEXP, START, KSTART,
     :                       KEND, NEXT, STATUS )
*+
*  Name:
*     GRP1_INCED

*  Purpose:
*     Incorporate editing specifications into a name

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_INCED( SLOT2, FIRST, LAST, GRPEXP, START, KSTART, KEND,
*                      NEXT, STATUS )

*  Description:
*     All elements stored in the group given by SLOT2 with indices
*     between FIRST and LAST (inclusive) are modified by the addition
*     of a prefix and a suffix, derived from the supplied group
*     expression.  The prefix is any string occuring between index
*     START and KSTART.  The suffix is any string occuring between
*     index KEND and NEXT.

*  Arguments:
*     SLOT2 = INTEGER (Given)
*        The slot number for the group containing the elements to be
*        modified.
*     FIRST = INTEGER (Given)
*        The index of the first element within the group to be modified.
*     LAST = INTEGER (Given)
*        The index of the last element within the group to be modified.
*     GRPEXP = CHARACTER * ( * ) (Given)
*        The group expression.
*     START = INTEGER (Given)
*        The index of the first character to be considered in the
*        supplied group expression.
*     KSTART = INTEGER (Given)
*        The index of the first character in the kernel from which the
*        elements being modified are derived.
*     KEND = INTEGER (Given)
*        The index of the last character in the kernel from which the
*        elements being modified are derived.
*     NEXT = INTEGER (Given)
*        The index of the element delimiter which follows the kernel. If
*        this is supplied as zero, then it is assumed that there is no
*        delimiter following the kernel.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     19-JAN-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP public constants

*  Arguments Given:
      INTEGER SLOT2
      INTEGER FIRST
      INTEGER LAST
      CHARACTER GRPEXP*(*)
      INTEGER START
      INTEGER KSTART
      INTEGER KEND
      INTEGER NEXT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Function giving used length of a string.

*  Local Variables:
      CHARACTER ELEM*(GRP__SZNAM)! Current element
      INTEGER ELEN               ! Used length of ELEM
      INTEGER END                ! Index of last character to be checked
      INTEGER DEP                ! Indirection depth for current name
      INTEGER IFILE              ! Index of indirection file for current
                                 ! name
      INTEGER II                 ! Loop count
      INTEGER MODGP              ! Basis group GRP id. for current name
      INTEGER MODIN              ! Basis group index for current name
      CHARACTER TEXT*(GRP__SZNAM)! The modified name
      INTEGER TLEN               ! Index of last character in TEXT

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the index of the last character to be considered from the
*  group expression.
      IF( NEXT .GT. 0 ) THEN
         END = NEXT - 1
      ELSE
         END = CHR_LEN( GRPEXP )
      END IF

*  Loop round the specified elements of the group.
      DO II = FIRST, LAST

*  Get the current element.
         CALL GRP1_GTELM( SLOT2, II, ELEM, DEP, IFILE, MODGP,
     :                    MODIN, STATUS )

*  Get the used length of the current element.
         ELEN = CHR_LEN( ELEM )

*  Initialise a text string to hold the prefix (if one exists).
         IF( KSTART .GT. START ) THEN
            TEXT = GRPEXP( START : KSTART - 1 )
            TLEN = KSTART - START
         ELSE
            TEXT = ' '
            TLEN = 0
         END IF

*  Append the element (so long as it does not have zero length).
         IF( ELEN .GT. 0 ) CALL CHR_APPND( ELEM( : ELEN ), TEXT, TLEN )

*  Append the suffix (if there is one).
         IF( KEND .LT. END .AND. KEND .GE. 0 )
     :                        CALL CHR_APPND( GRPEXP( KEND + 1 : END ),
     :                                        TEXT, TLEN )

*  Store the modified element (or a blank if the modified element has
*  zero length).
         IF( TLEN .GT. 0 ) THEN
            CALL GRP1_PTELM( SLOT2, II, TEXT( : TLEN ), DEP, IFILE,
     :                       MODGP, MODIN, STATUS )
         ELSE
            CALL GRP1_PTELM( SLOT2, II, ' ', DEP, IFILE, MODGP,
     :                       MODIN, STATUS )
         END IF

      END DO

      END
