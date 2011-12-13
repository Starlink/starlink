      SUBROUTINE IMG1_NEX( SLOT, ESLOT, N, ITEM, STATUS )
*+
* Name:
*    IMG1_NEX

*  Purpose:
*     Returns the name of the "Nth" extension item.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_NEX( SLOT, ESLOT, N, ITEM, STATUS )

*  Description:
*     This routine returns the Nth primitive object from an extension.
*     The extension is represented by a stack of all the locators to the
*     primitives within the extension. This routine merely indexes this
*     stack and uses the locator to derive the name.

*  Arguments:
*     SLOT = INTEGER (Given)
*        NDF slot number.
*     ESLOT = INTEGER (Given)
*        Extension slot number.
*     N = INTEGER (Given and Returned)
*        The index of the required item.
*     ITEM = CHARACTER * ( * ) (Returned)
*        The full name of the extension primitive.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The N'th extension item is really the N'th primitive object,
*     this might not be what's required if only scalar primitives are
*     needed.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     28-JUL-1994 (PDRAPER):
*        Original version.
*     10-AUG-1994 (PDRAPER):
*        Added ability to count the number of primitives if N is -1.
*     2-SEP-1994 (PDRAPER):
*        Complete re-write to use a more efficient method for indexing
*        the primitives in an extension. Previous versions walked the
*        extension tree for every inquiry.
*     20-APR-1999 (PDRAPER):
*        Modified to use CNF_PVAL to deference C memory pointers.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'IMG_CONST'       ! IMG_ constants
      INCLUDE 'IMG_ERR'         ! IMG_ error codes
      INCLUDE 'NDF_PAR'         ! NDF_ constants
      INCLUDE 'DAT_PAR'         ! HDS/DAT parameters
      INCLUDE 'CNF_PAR'         ! CNF parameters

*  Global Variables:
      INCLUDE 'IMG_ECB'         ! IMG Extension Control Block
*        ECB_XNLEN( IMG__MXPAR, IMG__MXEXT ) = INTEGER (Read)
*        Length of the (hds_)trace of the extension locator.
*
*        ECB_XPSTK( IMG__MXPAR, IMG__MXEXT ) = INTEGER (Read)
*        Pointers to the stack of extension locators.
*
*        ECB_XNSTK( IMG__MXPAR, IMG__MXEXT ) = INTEGER (Read)
*        The number of locators in an extension stack.

*  Arguments Given:
      INTEGER SLOT
      INTEGER ESLOT

*  Arguments Given and Returned:
      INTEGER N

*  Arguments Returned:
      CHARACTER * ( * ) ITEM

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL IMG1_INIT        ! Initialise common blocks
      EXTERNAL IMG1_NCEL
      CHARACTER * ( DAT__SZLOC ) IMG1_NCEL ! Nth element of character
                                           ! array

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to required component
      CHARACTER * ( 132 ) PATH  ! Trace of extension path
      CHARACTER * ( 132 ) FILE  ! Name of file
      INTEGER LSTAT             ! Local status
      INTEGER NLEV              ! Dummy
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the required locator. Note the %VAL(DAT__SZLOC) follows the last
*  genuine argument. This is the usual method used by compilers for
*  passing the lengths of strings on UNIX.
      IF ( N .LE. ECB_XNSTK( SLOT, ESLOT ) ) THEN
         LOC = IMG1_NCEL( %VAL( CNF_PVAL( ECB_XPSTK( SLOT, ESLOT ) ) ),
     :                    ECB_XNSTK( SLOT, ESLOT ), N, STATUS,
     :                    %VAL( CNF_CVAL( DAT__SZLOC )) )

*  Construct the item's name. This is the trace of the current object
*  with characters which make up the extension trace removed. The value
*  of ECB_XNLEN() should be the length of the extension locator trace
*  (+1).
         CALL HDS_TRACE( LOC, NLEV, PATH, FILE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CHR_COPY( PATH( ECB_XNLEN( SLOT, ESLOT ): ), .FALSE.,
     :                     ITEM, LSTAT )

*  Check that all went well. If not report an error.
            IF ( LSTAT .NE. 0 ) THEN
               STATUS = IMG__TRUNC
               CALL MSG_SETC( 'NAME', PATH( ECB_XNLEN( SLOT,ESLOT ): ))
               CALL MSG_SETI( 'LEN', LEN( ITEM ) )
               CALL ERR_REP( 'IMG1_NEX_TRUNC', 'Failed to copy ' //
     :              'the item name ''(^NAME)'' into character ' //
     :              'string of length ^LEN ' //
     :              '(possible programming error).', STATUS )
               ITEM = ' '
            END IF
         END IF
      END IF
      END
* $Id$
