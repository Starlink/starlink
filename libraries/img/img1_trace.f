      SUBROUTINE IMG1_TRACE( SLOT, ESLOT, STATUS )
*+
* Name:
*    IMG1_TRACE

*  Purpose:
*     Performs a trace of an extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_TRACE( SLOT, ESLOT, STATUS )

*  Description:
*     This routine walks an NDF extension HDS "tree" and tests each
*     object to see if it is a primitive, if it is then this counts as
*     an "header item" and a locator to it is recorded in a stack which
*     is available for indexing. This considerably speeds the getting
*     of the name of "Nth" objects (using the locator the name is
*     easily obtained using the HDS_TRACE routine and by removing the
*     extension path from this). However, this stack of locators must
*     be maintained by routines which make modifications to the
*     extension. The length of the extension path is also recorded to be
*     used when forming the name of the item (it is assumed that the
*     name consists of characters in the path returned by HDS_TRACE
*     after this position).
*
*     If an extension is already traced then no action is performed.

*  Arguments:
*     SLOT = INTEGER (Given)
*        NDF slot number.
*     ESLOT = INTEGER (Given)
*        Extension slot number.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - The essence of this algorithm is a recursion which traverses
*     all the nodes of the "HDS tree" rooted at the extension locator.
*     Recursion is limited to a depth of DEPTH. The main concepts are
*     those of the number of objects whose locators are open, the level
*     at which travel around the HDS tree is currently at and the
*     number of primitives located so far. The level should never be
*     greater than the number of objects currently open. Array
*     structures are dealt with as a special case, each element being
*     considered as a new node at a lower (higher level number) level.
*
*     - Although the recursion stack size is fixed (for efficiency as
*     well as convience), the number of locators to primitives is not
*     limited, extra memory for storing these is allocated (using
*     IMG1_CREAL) as required (using a parameter which defines how much
*     memory to grab as a chunk).

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
*     2-SEP-1994 (PDRAPER):
*        Original version based on a version IMG_NEX. This is part of
*        the reorganisation to speed indexing of large extensions.
*     20-APR-1999 (PDRAPER):
*        Modified to use CNF_PVAL to deference C memory pointers.
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

      INCLUDE 'IMG_PCB'         ! IMG Parameter Control Block
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Read)
*        NDF identifiers

*  Arguments Given:
      INTEGER SLOT
      INTEGER ESLOT

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL IMG1_INIT        ! Initialise common blocks
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used length of string

*  Local Constants:
      INTEGER DEPTH
      PARAMETER ( DEPTH = 100 ) ! Maximum recursion depth.
      INTEGER CHUNK
      PARAMETER ( CHUNK = 100 ) ! Size of locator array extension

*  Local Variables:
      CHARACTER * ( 132 ) FILE  ! Name of container file
      CHARACTER * ( 132 ) PATH  ! Trace of extension path name
      CHARACTER * ( DAT__SZLOC ) LOCSTK ( DEPTH ) ! Locators to active
                                ! objects (recursion stack)
      CHARACTER * ( DAT__SZLOC ) NEWLOC ! Temporary locator
      INTEGER ALLOC             ! Amount of storage current available
      INTEGER DIM( DAT__MXDIM ) ! Dimensions of object
      INTEGER DONE( DEPTH )     ! Number of component processed at this
                                ! level
      INTEGER I                 ! Loop variable
      INTEGER IPLOC             ! Pointer to locator storage space
      INTEGER LEVEL             ! Current component level (recursion
                                ! depth)
      INTEGER NCOMP( DEPTH )    ! Number of components at this level
      INTEGER NDIM              ! Number of object dimensions
      INTEGER NLEV              ! Number of levels in name traces
      INTEGER NOBJ              ! Number of locators on stack
      INTEGER NPRIM             ! Number of primitives encountered
      LOGICAL ISPRIM            ! Object is a primitive
      LOGICAL SCALAR( DEPTH )   ! Object is scalar

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check if the extension has already been traced, if it has do nothing.
      NOBJ = 0
      IF ( ECB_XNSTK( SLOT, ESLOT ) .NE. -1 ) GO TO 99

*  First initialise the recursion stacks.
      IPLOC = IMG__NOPTR
      DO 5 I = 1, DEPTH
         LOCSTK( I ) = DAT__NOLOC
         NCOMP( I ) = 0
         SCALAR( I ) = .TRUE.
 5    CONTINUE

*  Set the first locator to be the extension locator.
      CALL DAT_CLONE( ECB_XLOC( SLOT, ESLOT ), LOCSTK( 1 ), STATUS )
      NOBJ = 1

*  Allocate memory for storing locators (permanent).
      CALL IMG1_CALLO( DAT__SZLOC, CHUNK, IPLOC, STATUS )
      ALLOC = CHUNK

*  Before proceeding any further check the number of objects
*  immediately below the extension locator. Should have at least one.
      CALL DAT_NCOMP( LOCSTK( 1 ), NCOMP( 1 ), STATUS )
      DONE( 1 ) = 0
      IF ( NCOMP( 1 ) .LE. 0 .AND. STATUS .EQ. SAI__OK ) THEN

*  Problems, set status and abort.
         STATUS = IMG__NOITM
         CALL MSG_SETC( 'EXT', ECB_XNAME( SLOT, ESLOT ) )
         CALL NDF_MSG( 'NDF', PCB_INDF( SLOT ) )
         CALL ERR_REP( 'IMG1_NEX_NOCHILD', 'The ^EXT extension of ' //
     :        'the NDF ^NDF does not contain any header items ' //
     :        '(possible programming error).', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  See if the first object is a non-scalar structure.
      CALL DAT_PRIM( LOCSTK( 1 ), ISPRIM, STATUS )
      IF ( .NOT. ISPRIM ) THEN

*  Must be a structure. Could be non-scalar, so check the
*  dimensionality.
         CALL DAT_SHAPE( LOCSTK( NOBJ ), DAT__MXDIM, DIM, NDIM, STATUS )
         IF ( NDIM .NE. 0 ) THEN

*  It's an array of structures. Need to use DAT_CELL to get at its
*  parts. So set the array of structures stack to indicate this.
*  Vectorise the structure to remove any problems with dimensionality.
            SCALAR( 1 ) = .FALSE.
            CALL DAT_VEC( LOCSTK( 1 ), NEWLOC, STATUS )
            CALL DAT_ANNUL( LOCSTK( 1 ), STATUS )
            LOCSTK( 1 ) = NEWLOC
         END IF
      END IF

*  Initialise the counter for number of primitives.
      NPRIM = 0

*  Initialise the component level indicator.
      LEVEL = 1

*  Start inspecting the objects. Keep processing until LEVEL returns to
*  0, which indicates that we have walked the tree completely.
 1    CONTINUE
      IF ( LEVEL .GT. 0 .AND. LEVEL .LE. DEPTH .AND.
     :     NOBJ .LT. DEPTH .AND. STATUS .EQ. SAI__OK ) THEN

*  Get the number of components at this level, if we havn't done so
*  already. If the object is an array of structures then find out how
*  many elements it has.
         IF ( NCOMP( LEVEL ) .EQ. 0 ) THEN
            IF ( SCALAR( NOBJ ) ) THEN
               CALL DAT_NCOMP( LOCSTK( NOBJ ), NCOMP( LEVEL ), STATUS )
            ELSE
               CALL DAT_SIZE( LOCSTK( NOBJ ), NCOMP( LEVEL ), STATUS )
            END IF
            DONE( LEVEL ) = 0
         END IF
 2       CONTINUE

*  Try to process these components.
         IF ( DONE( LEVEL) .LT. NCOMP( LEVEL ) .AND.
     :        DONE( LEVEL ) .GE. 0 .AND. NCOMP( LEVEL ) .GT. 0 ) THEN

*  Increment the component number at this level.
            DONE( LEVEL ) = DONE( LEVEL ) + 1

*  Get a locator to this component, or cell if processing an array of
*  structures.
            IF ( SCALAR( NOBJ ) ) THEN
               CALL DAT_INDEX( LOCSTK( NOBJ ), DONE( LEVEL ),
     :                         LOCSTK( NOBJ + 1 ), STATUS )
            ELSE
               CALL DAT_CELL( LOCSTK( NOBJ ), 1, DONE( LEVEL ),
     :                        LOCSTK( NOBJ + 1 ), STATUS )
            END IF
            NOBJ = NOBJ + 1

*  Is it a primitive?
            CALL DAT_PRIM( LOCSTK( NOBJ ), ISPRIM, STATUS )
            IF ( ISPRIM ) THEN

*  Increment number of primitives.
               NPRIM = NPRIM + 1

*  And record it's locator.
               IF ( ALLOC .LT. NPRIM ) THEN

*  Need to extend memory.
                  ALLOC = ALLOC + CHUNK
                  CALL IMG1_CREAL( DAT__SZLOC, ALLOC,
     :                             IPLOC, STATUS )
               END IF
               CALL IMG1_WCEL( ALLOC, NPRIM, LOCSTK( NOBJ ),
     :                         %VAL( CNF_PVAL( IPLOC ) ), STATUS,
     :                         %VAL( CNF_CVAL( DAT__SZLOC ) ) )

*  Now try for another object at this level.
               LOCSTK( NOBJ ) = DAT__NOLOC
               NOBJ = NOBJ - 1
               GO TO 2
            ELSE

*  Must be a structure. Could be non-scalar, so check the
*  dimensionality.
               CALL DAT_SHAPE( LOCSTK( NOBJ ), DAT__MXDIM, DIM, NDIM,
     :                         STATUS )
               IF ( NDIM .NE. 0 ) THEN

*  It's an array of structures. Need to use DAT_CELL to get at its
*  parts. So set the array of structures stack to indicate this.
*  Vectorise the structure to remove any problems with dimensionality.
                  SCALAR( NOBJ ) = .FALSE.
                  CALL DAT_VEC( LOCSTK( NOBJ ), NEWLOC, STATUS )
                  CALL DAT_ANNUL( LOCSTK( NOBJ ), STATUS )
                  LOCSTK( NOBJ ) = NEWLOC
               END IF

*  Set up to process new level.
               LEVEL = LEVEL + 1
               GO TO 1
            END IF
         ELSE
            IF ( DONE( LEVEL) .GE. NCOMP( LEVEL ))
     :           THEN

*  Release the locator to the parent of this level as this is now
*  finished with (primitives at this level are already released).
               IF ( LEVEL .GE. 1 ) THEN
                  CALL DAT_ANNUL( LOCSTK( NOBJ ), STATUS )
                  SCALAR( NOBJ ) = .TRUE.
                  NOBJ = NOBJ - 1
               END IF
            END IF

*  Now try for a new object at the previous level.
            NCOMP( LEVEL ) = 0
            LEVEL = LEVEL - 1
            GO TO 1
         END IF

*  Return to head of main loop (no components).
         GO TO 1
      END IF

*  If NOBJ is greater than equal to DEPTH then we ran out of stack
*  space.
      IF ( NOBJ .GE. DEPTH .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IMG__FATIN
         CALL ERR_REP( 'IMG1_NEX_EXHAUST', 'IMG1_NEX: Stack space ' //
     :        'exhausted while traversing extension (fatal internal ' //
     :        'error). Extension structure too complex.', STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( NPRIM .EQ. 0 ) THEN

*  No primitive found in extension. So that this will look ok to other
*  routines (extension does exist and a trace has been attempted, other
*  routines should deal with the 0 primitives state) we will keep a
*  token 1 element.
            CALL IMG1_CREAL( DAT__SZLOC, 1, IPLOC, STATUS )
         ELSE IF ( ALLOC .NE. NPRIM ) THEN

*  Adjust memory so that we are at the end position.
            CALL IMG1_CREAL( DAT__SZLOC, NPRIM, IPLOC, STATUS )
         END IF

*  Copy pointer to common block.
         ECB_XPSTK( SLOT, ESLOT ) = IPLOC
         IPLOC = IMG__NOPTR

*  Record number of primitives.
         ECB_XNSTK( SLOT, ESLOT ) = NPRIM

*  Get length of extension name. Record this +2 for extra "." in
*  extended names.
         CALL HDS_TRACE( ECB_XLOC( SLOT, ESLOT), NLEV, PATH, FILE,
     :                   STATUS )
         ECB_XNLEN( SLOT, ESLOT ) = CHR_LEN( PATH ) + 2
      END IF

*  Exit in error label.
 99   CONTINUE

*  Release permanent locator storage if exiting in error.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( IPLOC .NE. IMG__NOPTR ) CALL IMG1_CFREE( IPLOC, STATUS )
         ECB_XNSTK( SLOT, ESLOT ) = -1
      END IF

*  And release any object locators.
      IF ( NOBJ .GT. 0 ) THEN
         DO 11 I = 1, MIN( DEPTH, NOBJ )
            CALL DAT_ANNUL( LOCSTK( I ), STATUS )
 11      CONTINUE
      END IF
      END
* $Id$
