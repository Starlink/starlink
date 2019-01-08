      SUBROUTINE KPG1_PRCVT( LOC, STATUS )
*+
*  Name:
*     KPG1_PRCVT

*  Purpose:
*     Converts an HDS primitive to a native data representation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PRCVT( LOC, STATUS )

*  Description:
*     The routine converts a primitive HDS object so that it is stored
*     using the appropriate native data representation provided by the
*     host machine.

*  Arguments:
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given and Returned)
*        Locator for the object to be converted. This may be modified
*        on exit (as the original object may have to be erased and
*        re-created).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine returns without action if the object supplied is not
*     primitive, or if it does not need conversion (i.e. if it has
*     already been converted or was created on the current machine). In
*     this case the LOC argument will be returned unchanged.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1998, 2004 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     5-MAY-1992 (RFWS):
*        Original version.
*     12-OCT-1998 (DSB):
*        Check _REAL and _DOUBLE objects for IEEE NaN and Inf values,
*        converting them to the appropriate Starlink bad value if any
*        are found.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL
*     8-JAN-2019 (DSB):
*        Use DAT_DSAME (added at version 6 of the HDS library) instead
*        of DAT_DREP, since DAT_DREP is not available for files that use
*        version 5 of the HDS format.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given and Returned:
      CHARACTER * ( DAT__SZLOC ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER SZFILE             ! Max. size of file name
      PARAMETER ( SZFILE = 256 )
      INTEGER SZPATH             ! Max. size of path name
      PARAMETER ( SZPATH = 256 )

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCP ! Parent structure locator
      CHARACTER * ( DAT__SZLOC ) LOCSCR ! Scratch object locator
      CHARACTER * ( DAT__SZLOC ) LOCTMP ! Temporary structure locator
      CHARACTER * ( DAT__SZNAM ) NAME ! Object name
      CHARACTER * ( DAT__SZTYP ) TYPE ! Object type
      CHARACTER * ( SZFILE ) FILE ! File name
      CHARACTER * ( SZPATH ) PATH ! Path name
      INTEGER DIM( DAT__MXDIM )  ! Object dimension sizes
      INTEGER LENGTH             ! Object length
      INTEGER NDIM               ! Number of object dimensions
      INTEGER NLEV               ! Nesting level
      INTEGER PNTR               ! Pointer to mapped scratch data
      INTEGER SIZE( 1 )          ! Object size
      LOGICAL FIRST              ! First invocation?
      LOGICAL PRIM               ! Object primitive?
      LOGICAL DSAME              ! Are the data representations equal?
      SAVE LOCTMP
      SAVE FIRST

*  Local Data:
      DATA FIRST / .TRUE. /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the locator supplied refers to a primitive object. There
*  is nothing else to do if it does not.
      PRIM = .FALSE.
      CALL DAT_PRIM( LOC, PRIM, STATUS )
      IF ( ( STATUS .EQ. SAI__OK ) .AND. PRIM ) THEN

*  If this is the first conversion to be performed, then create a
*  temporary HDS structure for use as a work area, saving its locator.
         IF ( FIRST ) THEN
            DIM( 1 ) = 0
            CALL DAT_TEMP( ' ', 0, DIM, LOCTMP, STATUS )
            IF ( STATUS .EQ. SAI__OK ) FIRST = .FALSE.
         END IF

*  Obtain the type of the object to be converted. Create a scratch
*  scalar object of the same type and obtain a locator to it.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL DAT_TYPE( LOC, TYPE, STATUS )
            DIM( 1 ) = 0
            CALL DAT_NEW( LOCTMP, 'SCRATCH', TYPE, 0, DIM, STATUS )
            CALL DAT_FIND( LOCTMP, 'SCRATCH', LOCSCR, STATUS )

*  See if the data representation is the same for both objects. Annul
*  the scratch object locator and erase the associated object.
            CALL DAT_DSAME( LOC, LOCSCR, DSAME, STATUS )
            CALL DAT_ANNUL( LOCSCR, STATUS )
            CALL DAT_ERASE( LOCTMP, 'SCRATCH', STATUS )
         END IF

*  Check if the object needs to be converted. If so, obtain its name,
*  shape, size and length.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( .NOT. DSAME ) THEN
               CALL DAT_NAME( LOC, NAME, STATUS )
               CALL DAT_SHAPE( LOC, DAT__MXDIM, DIM, NDIM, STATUS )
               CALL DAT_SIZE( LOC, SIZE( 1 ), STATUS )
               CALL DAT_LEN( LOC, LENGTH, STATUS )

*  If this is a character object, then create a scratch object with the
*  same type and size and obtain a locator to it.
               IF ( TYPE( : 5 ) .EQ. '_CHAR' ) THEN
                  CALL DAT_NEW( LOCTMP, 'SCRATCH', TYPE, 1, SIZE,
     :                          STATUS )
                  CALL DAT_FIND( LOCTMP, 'SCRATCH', LOCSCR, STATUS )

*  Map the scratch object for writing and read the values out of the
*  original object into it (thus performing the conversion).
                  CALL DAT_MAPV( LOCSCR, TYPE, 'WRITE', PNTR, SIZE( 1 ),
     :                           STATUS )
                  CALL KPG1_NAGTC( %VAL( CNF_PVAL( PNTR ) ),
     :                             LOC, NDIM, DIM, STATUS,
     :                             %VAL( CNF_CVAL( LENGTH ) ) )

*  If it is not a character object, then allocate memory for the
*  converted values and read the original object valuies into it (thus
*  performing the conversion).
               ELSE
                  CALL PSX_MALLOC( LENGTH * SIZE( 1 ), PNTR, STATUS )
                  CALL DAT_GET( LOC, TYPE, NDIM, DIM,
     :                          %VAL( CNF_PVAL( PNTR ) ),
     :                          STATUS )
               END IF

*  Determine the original object's nesting level and file and path
*  names.
               CALL HDS_TRACE( LOC, NLEV, PATH, FILE, STATUS )

*  If it is a top-level object, then erase the container file and
*  create a new one with the same name (the resulting object will then
*  have the correct data representation).
               IF ( NLEV. LE. 1 ) THEN
                  CALL HDS_ERASE( LOC, STATUS )
                  CALL HDS_NEW( FILE, NAME, TYPE, NDIM, DIM, LOC,
     :                          STATUS )

*  If it is not a top-level object, then find its parent structure and
*  erase the original object within it.
               ELSE
                  CALL DAT_PAREN( LOC, LOCP, STATUS )
                  CALL DAT_ANNUL( LOC, STATUS )
                  CALL DAT_ERASE( LOCP, NAME, STATUS )

*  Create a new object with the same name and obtain a locator for it.
*  Annul the parent locator.
                  CALL DAT_NEW( LOCP, NAME, TYPE, NDIM, DIM, STATUS )
                  CALL DAT_FIND( LOCP, NAME, LOC, STATUS )
                  CALL DAT_ANNUL( LOCP, STATUS )
               END IF

*  If this is a character object, then write the converted values into
*  the new object, annul the scratch object locator and erase the
*  associated object.
               IF ( TYPE( : 5 ) .EQ. '_CHAR' ) THEN
                  CALL KPG1_NAPTC( %VAL( CNF_PVAL( PNTR ) ),
     :                             LOC, NDIM, DIM, STATUS,
     :                             %VAL( CNF_CVAL( LENGTH ) ) )
                  CALL DAT_ANNUL( LOCSCR, STATUS )
                  CALL DAT_ERASE( LOCTMP, 'SCRATCH', STATUS )

*  If it is not a character object, then write the converted values and
*  free the allocated memory.
               ELSE
                  CALL DAT_PUT( LOC, TYPE, NDIM, DIM,
     :                          %VAL( CNF_PVAL( PNTR ) ),
     :                          STATUS )
                  CALL PSX_FREE( PNTR, STATUS )
               END IF
            END IF

*  Check for IEEE NaN and Inf values, converting them to the appropriate
*  Starlink bad value. This is always done for _REAL or _DOUBLE data,
*  even if no format conversion was necessary above.
            IF( STATUS .EQ. SAI__OK .AND.
     :             ( TYPE( : 5 ) .EQ. '_REAL' .OR.
     :               TYPE( : 7 ) .EQ. '_DOUBLE' ) ) THEN

*  Map the object for update.
               CALL DAT_MAPV( LOC, TYPE, 'UPDATE', PNTR, SIZE( 1 ),
     :                        STATUS )

*  Do the conversion.
               IF( TYPE( : 5 ) .EQ. '_REAL' ) THEN
                  CALL FTS1_RNANR( SIZE( 1 ), %VAL( CNF_PVAL( PNTR ) ),
     :                             STATUS )
               ELSE
                  CALL FTS1_RNAND( SIZE( 1 ), %VAL( CNF_PVAL( PNTR ) ),
     :                             STATUS )
               END IF

*  Unmap the object.
               CALL DAT_UNMAP( LOC, STATUS )
            END IF
         END IF
      END IF

      END
