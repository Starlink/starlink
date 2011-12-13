      SUBROUTINE IMG1_DLEX( SLOT, ESLOT, ITEM, STATUS )
*+
* Name:
*    IMG1_DLEX

*  Purpose:
*    Deletes an object from an extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_DLEX( SLOT, ESLOT, ITEM, STATUS )

*  Description:
*     This routine locates the HDS object ITEM in the extension (ESLOT)
*     of an NDF (SLOT) and erases it. The name of the object may be
*     hiearchical. The final object must be be primitive to be deleted.
*     If no primitive item is found no error is reported.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The slot number in the Parameter and Extension control blocks
*        of the NDF.
*     ESLOT = INTEGER (Given)
*        The slot number of the extension.
*     ITEM = CHARACTER * ( * ) (Given)
*        The name of the object which is to be deleted. This may be a
*        hierarchical list of objects (whose root is at the NDF
*        extension object), the final object is deleted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     27-JUL-1994 (PDRAPER):
*        Original version.
*     7-SEP-1994 (PDRAPER):
*        Added code to look after the locator stack traces.
*     12-SEP-1994 (PDRAPER):
*        Now doesn't worry when item isn't found.
*     12-SEP-1994 (PDRAPER):
*        Added check so that only primitives may be deleted (breaks
*        locator stack otherwise since a deleted parent deletes all
*        children. Alternative is to process stack looking for all
*        children of object to be deleted).
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
*        ECB_XNAME( IMG__MXPAR, IMG__MXEXT ) =
*           CHARACTER * ( NDF__SZXNM ) (Read)
*        The name of the extension
*
*        ECB_XLOC( IMG__MXPAR, IMG__MXEXT ) =
*           CHARACTER ( DAT__SZLOC ) (Read)
*        The locator to the extension.
*
*        ECB_XPSTK( IMG__MXPAR, IMG__MXEXT ) = INTEGER (Read and Write)
*        Pointers to the stack of extension locators.
*
*        ECB_XNSTK( IMG__MXPAR, IMG__MXEXT ) = INTEGER (Read and Write)
*        The number of locators in an extension stack.

      INCLUDE 'IMG_PCB'         ! IMG Parameter Control Block
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Read)
*           NDF identifiers

*  Arguments Given:
      INTEGER SLOT
      INTEGER ESLOT
      CHARACTER * ( * ) ITEM

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL IMG1_INIT        ! Initialise common blocks
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used length of string
      EXTERNAL IMG1_NCEL
      CHARACTER * ( DAT__SZLOC ) IMG1_NCEL ! Returns Nth character
                                           ! element of array

*  Local Variables:
      CHARACTER * ( 132 ) FILE  ! Name of file
      CHARACTER * ( 132 ) OPATH ! Full path name of object
      CHARACTER * ( 132 ) SPATH ! Full path name of object
      CHARACTER * ( 2 * DAT__SZNAM ) OBJECT ! Name of "current" object
      CHARACTER * ( DAT__SZLOC ) LOC1 ! Primary locator
      CHARACTER * ( DAT__SZLOC ) LOC2 ! Secondary locator
      CHARACTER * ( DAT__SZLOC ) TMPLOC ! Temporary copy of locator from stack
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Current start position in ITEM
      INTEGER INOW              ! New position of period
      INTEGER NLEV              ! Dummy
      LOGICAL MORE              ! ITEM string has more periods
      LOGICAL PRIM              ! Object is primitive
      LOGICAL YES               ! Object exists
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise MORE variable for first pass.
      MORE = .TRUE.

*  Current position in the ITEM string.
      IAT = 1

*  Clone the extension locator (so we can safely annul/re-use this
*  later).
      CALL DAT_CLONE( ECB_XLOC( SLOT, ESLOT ), LOC1, STATUS )

*  The basic idea is to loop locating each period in the item name until
*  no more are left. At each level getting a locator to the new object.
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( STATUS .EQ. SAI__OK .AND. MORE ) THEN

*  Look for a period in the current string.
         INOW = INDEX( ITEM( IAT: ), '.' )
         IF ( INOW .NE. 0 ) THEN

*  Extract the name of the object to be accessed.
            INOW = IAT + INOW - 2
            OBJECT = ITEM( IAT: INOW )

*  Try to get a locator to this "component".
            CALL IMG1_FOBJ( LOC1, OBJECT, YES, LOC2, STATUS )
            IF ( .NOT. YES .AND. STATUS .EQ. SAI__OK ) THEN

*  The specified item doesn't exist. Give up quietly.
               MORE = .FALSE.
            ELSE IF ( YES .AND. STATUS .EQ. SAI__OK ) THEN

*  Now release the locator to the previous object and make this the
*  current object.
               CALL DAT_ANNUL( LOC1, STATUS )
               LOC1 = LOC2
               IAT = INOW + 2
            ELSE

*  Serious error when attempting to locate the desired object. Beef up
*  error message to make context clear.
               CALL MSG_SETC( 'ITEM', ITEM )
               CALL MSG_SETC( 'EXT', ECB_XNAME( SLOT, ESLOT ) )
               CALL NDF_MSG( 'NDF', PCB_INDF( SLOT ) )
               CALL ERR_REP( 'IMG1_DLEX_FINDERR', 'Cannot delete ' //
     :              'the header item ^ITEM in the ^EXT extension of ' //
     :              'the NDF ^NDF.', STATUS )
            END IF
         ELSE

*  No period. Final object name (or first if not hierarchical) is
*  ITEM( IAT: )
            MORE = .FALSE.
            INOW = CHR_LEN( ITEM )
            OBJECT = ITEM( IAT: INOW )

*  Test for the existence of the object.
            CALL IMG1_FOBJ( LOC1, OBJECT, YES, LOC2, STATUS )
            IF ( YES .AND. STATUS .EQ. SAI__OK ) THEN

*  Check that object is a primitive.
               CALL DAT_PRIM( LOC2, PRIM, STATUS )
               IF ( PRIM ) THEN

*  If a "trace" of the extension has been taken then we need to update
*  this so that this object is no longer present.
                  IF ( ECB_XNSTK( SLOT, ESLOT ) .GT. 0 ) THEN
                     CALL HDS_TRACE( LOC2, NLEV, OPATH, FILE, STATUS )
                     DO 2 I = 1, ECB_XNSTK( SLOT, ESLOT )
                        TMPLOC = IMG1_NCEL(
     :                           %VAL(CNF_PVAL( ECB_XPSTK(SLOT,ESLOT))),
     :                           ECB_XNSTK( SLOT, ESLOT ), I,
     :                           STATUS, %VAL( CNF_CVAL( DAT__SZLOC )) )

*  Get the complete path name for this object and compare it with the
*  name of the object which we are about to delete.
                        CALL HDS_TRACE( TMPLOC, NLEV, SPATH, FILE,
     :                                  STATUS )
                        IF ( SPATH .EQ. OPATH ) THEN

*  Have a match need to remove this. Unfortunately this means re-sorting
*  the locator stack to remove the gap created by this erasure.
                           CALL IMG1_DCEL( ECB_XNSTK( SLOT, ESLOT ), I,
     :                          %VAL(CNF_PVAL( ECB_XPSTK(SLOT,ESLOT)) ),
     :                          STATUS, %VAL( CNF_CVAL( DAT__SZLOC )) )

*  Adjust the locator stack to remove the extra element.
                           ECB_XNSTK( SLOT, ESLOT ) =
     :                          ECB_XNSTK( SLOT, ESLOT ) - 1
                           IF ( ECB_XNSTK( SLOT, ESLOT ) .GT. 0 ) THEN
                              CALL IMG1_CREAL( DAT__SZLOC,
     :                                         ECB_XNSTK( SLOT, ESLOT ),
     :                                         ECB_XPSTK( SLOT, ESLOT ),
     :                                         STATUS )
                           END IF

*  And pass on.
                           GO TO 3
                        END IF
 2                   CONTINUE
 3                   CONTINUE
                  END IF

*  Now Erase the object by getting its component name. Note that if
*  object is a slice or cell the whole object will be deleted, it isn't
*  possible to delete part of a component.
                  CALL DAT_NAME( LOC2, OBJECT, STATUS )
                  CALL DAT_ANNUL( LOC2, STATUS )
                  CALL DAT_ERASE( LOC1, OBJECT, STATUS )
               ELSE

*  Cannot delete non-primitives. So do nothing.
                  MORE = .FALSE.
                  CALL DAT_ANNUL( LOC2, STATUS )
               END IF
            ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  The specified item doesn't exist. Give up quietly.
               MORE = .FALSE.
            ELSE

*  Serious error when attempting to locate the final object. Beef up
*  error message to make context clear.
               CALL MSG_SETC( 'ITEM', ITEM )
               CALL MSG_SETC( 'EXT', ECB_XNAME( SLOT, ESLOT ) )
               CALL NDF_MSG( 'NDF', PCB_INDF( SLOT ) )
               CALL ERR_REP( 'IMG1_DLEX_FINDERR', 'Cannot delete ' //
     :              'the header item ^ITEM in the ^EXT extension of ' //
     :              'the NDF ^NDF.', STATUS )
            END IF
         END IF

*  Return for next loop.
         GO TO 1
      END IF

*  Release the locator.
      CALL DAT_ANNUL( LOC1, STATUS )
      END
* $Id$
