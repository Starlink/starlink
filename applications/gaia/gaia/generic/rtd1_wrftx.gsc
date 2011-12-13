      SUBROUTINE RTD1_WRFT<T>( ITEM, VALUE, COMMEN, IPHEAD, NHEAD, 
     :                         AVAIL, STATUS )
*+
* Name:
*    RTD1_WRFTx

*  Purpose:
*    Writes the value of a FITS item.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RTD1_WRFT<T>( ITEM, VALUE, COMMEN, IPHEAD, NHEAD, AVAIL, 
*                        STATUS)

*  Description:
*     This routine writes the value associated with a FITS keyword into
*     a FITS block. A FITS block must have been already been created.
*     If the keyword already exists in the FITS block
*     then it will be superceded (unless it is one of the specials
*     'COMMENT', 'HISTORY' or ' '), otherwise the new value will be
*     appended.

*  Arguments:
*     ITEM = CHARACTER * ( * ) (Given)
*        The FITS keyword. This may be heirarchical.
*     VALUE = <COMM> (Given)
*        The value to be associated with the keyword.
*     COMMEN = CHARACTER * ( * ) (Given)
*        A comment to write with the value.
*     IPHEAD = INTEGER (Given and Returned)
*        Pointer to FITS block.
*     NHEAD = INTEGER (Given and Returned)
*        Number of cards written to in block.
*     AVAIL = INTEGER (Given and Returned)
*        Number of cards available in FITS block.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - this routine has a version for the types C, L, D, R and I.

*  Authors:
*     PWD: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  Copyright:
*     Copyright (C) 1994-2005 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  History:
*     22-JUL-1994 (PWD):
*        Original version.
*     7-SEP-1994 (PWD):
*        Added check for status return from GKEY.
*     20-SEP-1994 (PWD):
*        Removed PSX calls.
*     22-NOV-1996 (PWD):
*        Converted for RTD from IMG.
*     02-SEP-2004 (PWD):
*        Converted to use CNF_PVAL for pointers.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'CNF_PAR'          ! CNF functions

*   Arguments Given:
      CHARACTER * ( * ) ITEM
      CHARACTER * ( * ) COMMEN
      <TYPE> VALUE

*   Arguments Given and Returned:
      INTEGER IPHEAD
      INTEGER NHEAD
      INTEGER AVAIL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) LKEY    ! Local copy of keyword
      LOGICAL THERE              ! Item has been located in FITS block
      LOGICAL NEWEND             ! Whether a new END record is required
      INTEGER AT                 ! The record number of the item
      <LTYPE> LVAL               ! Local to receive value if found
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Note the %VAL(80)'s following the last genuine arguments of certain
*  calls. This is the usual method used by compilers for passing the
*  lengths of strings on UNIX and needs to be used here since the
*  characters are mapped.
*
*  Before proceeding we need to find out if the keyword already exists,
*  if not then the memory allocated to the FITS block needs to be
*  extended to allow an extra record. Trap errors to do with format
*  conversion etc. from GKEY. THERE is still true if the keyword is
*  found in this state (we will overwrite regardless).
*
*  Avoid these checks if the keyword is one of the specials (COMMENT,
*  HISTORY or ' '). In this case a new card is always appended to any
*  existing ones (or more precisely just before the 'END' keyword).
      LKEY = ITEM
      CALL CHR_UCASE( LKEY )
      CALL CHR_LDBLK( LKEY )
      IF ( LKEY .EQ. 'COMMENT' .OR. LKEY .EQ. 'HISTORY' .OR.
     :     LKEY .EQ. ' ' ) THEN
         CALL ERR_MARK
         CALL RTD1_GKEY<T>( NHEAD, %VAL( CNF_PVAL( IPHEAD ) ), 1, 
     :                      'END', 0, THERE,  LVAL, AT, STATUS, 
     :                      %VAL( CNF_CVAL( 80 ) ) )
         IF ( THERE .AND. STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         CALL ERR_RLSE
         THERE = .FALSE.
      ELSE
         CALL ERR_MARK
         CALL RTD1_GKEY<T>( NHEAD, %VAL( CNF_PVAL( IPHEAD ) ), 1, LKEY, 
     :                      0, THERE, LVAL, AT, STATUS, 
     :                      %VAL( CNF_CVAL( 80 ) ) )
         IF ( THERE .AND. STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         CALL ERR_RLSE
      END IF
      IF ( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN

*  If necessary extend the FITS block in memory to allow for an extra card.
         IF ( NHEAD + 1 .GE. AVAIL ) THEN 
            AVAIL = AVAIL + 256
            CALL PSX_REALLOC( AVAIL * 80, IPHEAD, STATUS )
         END IF
      END IF

*  Now write in the value.
      NEWEND = .NOT. THERE
      IF ( .NOT. THERE ) NHEAD = NHEAD + 1
      CALL RTD1_WKEY<T>( NHEAD, %VAL( CNF_PVAL( IPHEAD ) ), AT, NEWEND, 
     :                   LKEY, COMMEN, VALUE, STATUS, 
     :                   %VAL( CNF_CVAL( 80 ) ) )
      END

