      SUBROUTINE ARY1_RETYP( PAREN, NAME, TYPE, STATE, BAD, NDIM, DIM,
     :                       NTYPE, LOC, DCE, STATUS )
*+
*  Name:
*     ARY1_RETYP

*  Purpose:
*     Change the data type of a primitive numeric HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_RETYP( PAREN, NAME, TYPE, STATE, BAD, NDIM, DIM, NTYPE,
*     LOC, DCE, STATUS )

*  Description:
*     The routine changes the data type of a primitive numeric HDS
*     object while (optionally) preserving its contents. If the
*     contents are to be preserved, then data type conversion will be
*     performed on them.

*  Arguments:
*     PAREN = CHARACTER * ( * ) (Given)
*        HDS locator to the object's parent structure.
*     NAME = CHARACTER * ( * ) (Given)
*        HDS name of the object whose type is to be changed.
*     TYPE = CHARACTER * ( * ) (Given)
*        Initial data type of the HDS object. This must be a primitive
*        numeric HDS data type string (case insensitive).
*     STATE = LOGICAL (Given)
*        The HDS state of the data object (.TRUE. for defined, .FALSE.
*        for undefined). This argument determines whether the contents
*        of the object are to be preserved. They are only preserved if
*        its value is .TRUE..
*     BAD = LOGICAL (Given)
*        Whether checks for "bad" data values must be made if data type
*        conversion is performed. This argument is not used if STATE is
*        set to .FALSE..
*     NDIM = INTEGER (Given)
*        Number of object dimensions.
*     DIM( NDIM ) = INTEGER (Given)
*        Object dimension sizes.
*     NTYPE = CHARACTER * ( * ) (Given)
*        The new data type required for the object. This must be a
*        primitive numeric HDS data type string (case insensitive).
*     LOC = CHARACTER * ( * ) (Given and Returned)
*        HDS locator to the object whose type is to be changed. Note
*        that type conversion involves erasing the object and creating
*        a new version, so this locator will be altered if the new type
*        differs from the initial one.
*     DCE = LOGICAL (Returned)
*        Whether data conversion errors occurred. This can only happen
*        if STATE is set to .TRUE..
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The routine will execute most rapidly if STATE is set to
*     .FALSE., since no data values then need to be converted.

*  Algorithm:
*     -  Initialise the DCE value and check if the new type matches the
*     the old type. If so, then the data object's type does not need
*     changing. Ensure that it is in the expected state by resetting it
*     if appropriate.
*     -  If the object's data type needs changing, but STATE is
*     .FALSE., then erase the original object and create a new one of
*     the required type. Obtain a locator to it.
*     -  If STATE is .TRUE., then create a temporary structure and move
*     the original data object into it.
*     -  Create a new data object of the required type and obtain a
*     locator to it.
*     -  Map both the old and new objects.
*     -  Calculate the number of data values that need converting.
*     -  Compare the new data type with each permitted value in turn and
*     call the appropriate routine to convert the old data into the new
*     type.
*     -  Unmap the new data object.
*     -  Annul the locator to the old data and erase the temporary
*     structure containing it.

*  Implementation Deficiencies:
*     -  This routine requires a parent locator in order to function, so
*     it cannot be used on a top-level HDS object.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-AUG-1989 (RFWS):
*        Original version.
*     31-AUG-1989 (RFWS):
*        Changed to ensure that the data object is set to the expected
*        state (defined or undefined) even if its data type does not
*        need alteration.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNV_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Arguments Given:
      CHARACTER * ( * ) PAREN
      CHARACTER * ( * ) NAME
      CHARACTER * ( * ) TYPE
      LOGICAL STATE
      LOGICAL BAD
      INTEGER NDIM
      INTEGER DIM( NDIM )
      CHARACTER * ( * ) NTYPE

*  Arguments Given and Returned:
      CHARACTER * ( * ) LOC

*  Arguments Returned:
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) OLDLOC ! Locator to old data
      CHARACTER * ( DAT__SZLOC ) TLOC ! Locator to temporary structure
      INTEGER DUMMY( 1 )         ! Dummy dimension array
      INTEGER EL                 ! Number of data elements to convert
      INTEGER I                  ! Loop counter for dimensions
      INTEGER OLDPTR             ! Pointer to mapped old data
      INTEGER PNTR               ! Pointer to mapped new data

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the DCE value and check whether the new type matches the
*  old type. If so, then the data object type does not need changing.
*  Ensure that the object is in the expected state by resetting it if
*  appropriate.
      DCE = .FALSE.
      IF ( CHR_SIMLR( TYPE, NTYPE ) ) THEN
         IF ( .NOT. STATE ) THEN
            CALL DAT_RESET( LOC, STATUS )
         END IF

*  If the data object type needs changing, but STATE is .FALSE., then
*  there are no data values to convert. Annul the locator to the old
*  object and erase it.
      ELSE
         IF ( .NOT. STATE ) THEN
            CALL DAT_ANNUL( LOC, STATUS )
            LOC = ARY__NOLOC
            CALL DAT_ERASE( PAREN, NAME, STATUS )

*  Create a new object of the required type and obtain a locator to it.
            CALL DAT_NEW( PAREN, NAME, NTYPE, NDIM, DIM, STATUS )
            CALL DAT_FIND( PAREN, NAME, LOC, STATUS )

*  If STATE is .TRUE., then the object's data values must be converted.
*  Create a temporary structure and move the old data object into it.
         ELSE
            DUMMY( 1 ) = 0
            TLOC = ARY__NOLOC
            CALL ARY1_TEMP( ' ', 0, DUMMY, TLOC, STATUS )
            CALL DAT_MOVE( LOC, TLOC, 'TEMP', STATUS )
            LOC = ARY__NOLOC

*  Get a locator to the old object in its new structure.
            OLDLOC = ARY__NOLOC
            CALL DAT_FIND( TLOC, 'TEMP', OLDLOC, STATUS )

*  Create a new object with the required type and obtain a locator to
*  it.
            CALL DAT_NEW( PAREN, NAME, NTYPE, NDIM, DIM, STATUS )
            CALL DAT_FIND( PAREN, NAME, LOC, STATUS )

*  Map both objects, the old one for reading and the new one for
*  writing.
            CALL DAT_MAP( OLDLOC, TYPE, 'READ', NDIM, DIM, OLDPTR,
     :                    STATUS )
            CALL DAT_MAP( LOC, NTYPE, 'WRITE', NDIM, DIM, PNTR, STATUS )

*  Calculate the number of data elements that must be converted.
            EL = 1
            DO 1 I = 1, NDIM
               EL = EL * DIM( I )
1           CONTINUE

*  Compare the new data type with each permitted value in turn and call
*  the appropriate routine to convert the old data into the new type.
            IF ( CHR_SIMLR( NTYPE, '_BYTE' ) ) THEN
               CALL ARY1_CVTB( BAD, EL, TYPE, OLDPTR,
     :                         %VAL( CNF_PVAL( PNTR ) ), DCE, STATUS )
 
            ELSE IF ( CHR_SIMLR( NTYPE, '_UBYTE' ) ) THEN
               CALL ARY1_CVTUB( BAD, EL, TYPE, OLDPTR,
     :                          %VAL( CNF_PVAL( PNTR ) ), DCE, STATUS )
 
            ELSE IF ( CHR_SIMLR( NTYPE, '_DOUBLE' ) ) THEN
               CALL ARY1_CVTD( BAD, EL, TYPE, OLDPTR,
     :                         %VAL( CNF_PVAL( PNTR ) ), DCE, STATUS )
 
            ELSE IF ( CHR_SIMLR( NTYPE, '_INTEGER' ) ) THEN
               CALL ARY1_CVTI( BAD, EL, TYPE, OLDPTR,
     :                         %VAL( CNF_PVAL( PNTR ) ), DCE, STATUS )
 
            ELSE IF ( CHR_SIMLR( NTYPE, '_REAL' ) ) THEN
               CALL ARY1_CVTR( BAD, EL, TYPE, OLDPTR,
     :                         %VAL( CNF_PVAL( PNTR ) ), DCE, STATUS )
 
            ELSE IF ( CHR_SIMLR( NTYPE, '_WORD' ) ) THEN
               CALL ARY1_CVTW( BAD, EL, TYPE, OLDPTR,
     :                         %VAL( CNF_PVAL( PNTR ) ), DCE, STATUS )
 
            ELSE IF ( CHR_SIMLR( NTYPE, '_UWORD' ) ) THEN
               CALL ARY1_CVTUW( BAD, EL, TYPE, OLDPTR,
     :                          %VAL( CNF_PVAL( PNTR ) ), DCE, STATUS )
            END IF

*  Unmap the new object.
            CALL ARY1_HUNMP( LOC, STATUS )

*  Annul the locator to the old data and erase the temporary structure
*  containing it.
            CALL DAT_ANNUL( OLDLOC, STATUS )
            OLDLOC = ARY__NOLOC
            CALL ARY1_ANTMP( TLOC, STATUS )
         END IF
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_RETYP', STATUS )

      END
