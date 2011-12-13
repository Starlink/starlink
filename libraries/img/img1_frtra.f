      SUBROUTINE IMG1_FRTRA( SLOT, ESLOT, STATUS )
*+
*  Name:
*     IMG1_FRTRA

*  Purpose:
*     Frees any resources associated with a trace of an extension

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_FRTRA( SLOT, ESLOT, STATUS )

*  Description:
*     This routine frees any resources allocated by the IMG1_TRACE
*     routine. If should be called for each non-FITS extension before
*     a program exits (normally by IMG1_FREXT).

*  Arguments:
*     SLOT = INTEGER (Given)
*        NDF slot number.
*     ESLOT = INTEGER (Given)
*        Extension slot number.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*     24-OCT-2000 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'IMG_CONST'       ! IMG_ constants
      INCLUDE 'IMG_ERR'         ! IMG_ error codes
      INCLUDE 'NDF_PAR'         ! NDF_ constants
      INCLUDE 'DAT_PAR'         ! HDS/DAT parameters
      INCLUDE 'CNF_PAR'         ! CNF parameters

*  Global Variables:
      INCLUDE 'IMG_ECB'         ! IMG Extension Control Block
*        ECB_XNAME( IMG__MXPAR, IMG__MXEXT ) =
*           CHARACTER * ( NDF__SZXNM ) (Read)
*        The name of the extension
*
*        ECB_XLOC( IMG__MXPAR, IMG__MXEXT ) =
*           CHARACTER ( DAT__SZLOC ) (Read)
*        The locator to the extension.
*
*        ECB_XPSTK( IMG__MXPAR, IMG__MXEXT ) = INTEGER (Write)
*        Pointers to extension locator stacks
*
*        ECB_XNSTK( IMG__MXPAR, IMG__MXEXT ) = INTEGER (Write)
*        Number of locator in extension stacks.
*
*        ECB_XNLEN( IMG__MXPAR, IMG__MXEXT ) = INTEGER (Write)
*        Length of the (hds_)trace of the extension locator.

*  External References:
      EXTERNAL IMG1_NCEL
      CHARACTER * ( DAT__SZLOC ) IMG1_NCEL ! Returns element from
                                           ! character array

*  Arguments Given:
      INTEGER SLOT
      INTEGER ESLOT

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL IMG1_INIT        ! Initialise common blocks

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! Temporary locator
      INTEGER I                 ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do nothing unless the extension has been traced.
      IF ( ECB_XNSTK( SLOT, ESLOT ) .GE. 0 ) THEN

*  The count may be zero if a extension is traced but has no primitives,
*  check for this state. If 0 locators are present memory still needs to
*  be freed.
         IF ( ECB_XNSTK( SLOT, ESLOT ) .GT. 0 ) THEN
            DO 1 I = 1, ECB_XNSTK( SLOT, ESLOT )
               LOC = IMG1_NCEL(
     :                     %VAL( CNF_PVAL( ECB_XPSTK(SLOT,ESLOT) ) ),
     :                     ECB_XNSTK(SLOT, ESLOT), I, STATUS,
     :                     %VAL( CNF_CVAL( DAT__SZLOC ) ) )
               CALL DAT_ANNUL( LOC, STATUS )
 1          CONTINUE
         END IF

*  Release the memory and reset the stack counter to show that this is
*  now not in use.
         CALL IMG1_CFREE( ECB_XPSTK( SLOT, ESLOT ), STATUS )
         ECB_XNSTK( SLOT, ESLOT ) = -1
         ECB_XNLEN( SLOT, ESLOT ) = 0
      END IF
      END
* $Id$
