      SUBROUTINE ARY1_SBND( NDIM, LBND, UBND, IACB, STATUS )
*+
*  Name:
*     ARY1_SBND

*  Purpose:
*     Set new array bounds for an ACB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_SBND( NDIM, LBND, UBND, IACB, STATUS )

*  Description:
*     The routine sets new bounds for an array identified by its ACB
*     entry. If the array is a section, then its bounds are simply
*     changed and no other ACB ebtries are affected. However, if the
*     array is a base array, then the actual data object bounds are
*     altered and this change is reflected in all other ACB entries
*     which refer to it. If the array's data values are defined, then
*     any pixels lying within both the old and new bounds will be
*     retained and any new pixels introduced will be assigned the "bad"
*     value.  Pixels excluded by the new bounds are lost, and cannot be
*     regained by further changes to the array bounds (in the case of
*     changes to a base array, they are lost permanently and can never
*     be recovered).  If bad pixels are introduced (i.e. if the new
*     bounds extend outside the old bounds), then the array's bad pixel
*     flag will be updated to reflect this.

*  Arguments:
*     NDIM = INTEGER (Given)
*        New number of dimensions for the array.
*     LBND( NDIM ) = INTEGER (Given)
*        New lower bounds for the array.
*     UBND( NDIM ) = INTEGER (Given)
*        New upper bounds for the array.
*     IACB = INTEGER (Given)
*        Index to the array's ACB entry.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The bounds of an array section cannot be changed while mapped
*     access to it is in effect. In the case of a base array, no access
*     to any part of it can be in effect.

*  Algorithm:
*     -  If the bounds of an array section are being changed, then check
*     that it is not mapped for access. Report an error if it is.
*     -  Subtract the ACB accumulated pixel shifts from the new bounds
*     to convert to the reference frame pixel index system.
*     -  Set the ACB bounds to the new values, padding them with 1's if
*     necessary.
*     -  Set the new number of dimensions.
*     -  If a data transfer window exists, then find the intersection
*     of this with the new bounds (converted to the reference frame),
*     storing the result as the new data transfer window. Note whether
*     a data transfer window now exists.
*     -  If the array is a base array, then check to see if any mapped
*     access to it is in effect. Report an error if it is.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Set the new bounds for the data object.
*     -  If the new bounds differed from the old bounds, then loop to
*     find all the ACB entries which are affected.
*     -  Select those base array entries which refer to the same data
*     object.
*     -  Set the new bounds in the selected ACB entries, padding with
*     1's if necessary.
*     -  Set the new number of dimensions in the selected ACB entries.
*     -  Check to see whether any bad pixels may have been introduced.
*     Do this by explicitly checking the new array bounds against the
*     region of retained data if necessary.
*     -  If bad pixels may have been introduced, then set the bad pixel
*     flag.

*  Side Effects:
*     -  Any data lost as a result of changing the bounds of a base
*     array will cease to be available to any array which refers to that
*     base array.
*     -  If bad pixels are introduced into a base array as a result of
*     calling this routine, then the bad pixel flag will be set and
*     this may be apparent through enquiries made about other arrays
*     which refer to the same base array.

*  Implementation Deficiencies:
*     -  This routine takes a rather pessimistic view of bad pixels; it
*     considers that if bad pixels are introduced by changing the array
*     bounds then they may be positioned anywhere within the new
*     bounds.

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
*     5-DEC-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.
*        DCB_NREAD( ARY__MXDCB ) = INTEGER (Read)
*           Number of ACB entries with read access to the data object.
*        DCB_NWRIT( ARY__MXDCB ) = INTEGER (Read)
*           Number of ACB entries with write access to the data object.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_CUT( ARY__MXACB ) = LOGICAL (Read)
*           Whether the array is a cut (i.e. section) from a base
*           array.
*        ACB_DTWEX( ARY__MXACB ) = LOGICAL (Read and Write)
*           Whether a data transfer window exists.
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to the data object entry in the DCB.
*        ACB_IMCB( ARY__MXACB ) = INTEGER (Read)
*           Index to the mapping entry in the MCB.
*        ACB_LBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read and Write)
*           Lower array bounds.
*        ACB_LDTW( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read and Write)
*           Lower bounds of the data transfer window.
*        ACB_NDIM( ARY__MXACB ) = INTEGER (Read and Write)
*           Number of array dimensions.
*        ACB_SFT( ARY__MXACB ) = INTEGER (Read)
*           Accumulated pixel index shifts applied to the ACB entry.
*        ACB_UBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read and Write)
*           Upper array bounds.
*        ACB_UDTW( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read and Write)
*           Upper bounds of the data transfer window.

*  Arguments Given:
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IACBT              ! ACB index to test
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IDCBT              ! DCB index to test
      INTEGER LDTW( ARY__MXDIM ) ! New lower data transfer window bounds
      INTEGER LX( ARY__MXDIM )   ! Lower bounds of retained data region
      INTEGER NEXT               ! Next ACB entry to test
      INTEGER UDTW( ARY__MXDIM ) ! New upper data transfer window bounds
      INTEGER UX( ARY__MXDIM )   ! Upper bounds of retained data region
      LOGICAL BAD                ! Bad pixels may have been introduced?
      LOGICAL DRX                ! Whether data have been retained
      LOGICAL SAME               ! New bounds same as the old ones?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the the bounds of an array section are being changed, then check
*  that the section is not mapped for access.
      IF ( ACB_CUT( IACB ) ) THEN
         IF ( ACB_IMCB( IACB ) .NE. 0 ) THEN

*  Report an error if it is.
            STATUS = ARY__ISMAP
            IDCB = ACB_IDCB( IACB )
            CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
            CALL ERR_REP( 'ARY1_SBND_MAP',
     :      'The array structure ^ARRAY is mapped for access ' //
     :      'through the identifier supplied (possible programming ' //
     :      'error).' , STATUS )

*  Subtract the ACB pixel index shifts from the new bounds to convert
*  them to reference frame pixel indices.
         ELSE
            DO 1 I = 1, NDIM
               LDTW( I ) = LBND( I ) - ACB_SFT( I, IACB )
               UDTW( I ) = UBND( I ) - ACB_SFT( I, IACB )

*  Set the ACB bounds to the new values.
               ACB_LBND( I, IACB ) = LBND( I )
               ACB_UBND( I, IACB ) = UBND( I )
1           CONTINUE

*  Pad with 1's if necessary.
            DO 2 I = NDIM + 1, ARY__MXDIM
               LDTW( I ) = 1
               UDTW( I ) = 1
               ACB_LBND( I, IACB ) = 1
               ACB_UBND( I, IACB ) = 1
2           CONTINUE

*  Set the new number of dimensions.
            ACB_NDIM( IACB ) = NDIM

*  If a data transfer window exists, then find the intersection of this
*  window with the new array bounds (converted to the reference frame
*  pixel index system). Note whether a data transfer window now exists.
            IF ( ACB_DTWEX( IACB ) ) THEN
               CALL ARY1_XSBND( ARY__MXDIM, LDTW, UDTW, ARY__MXDIM,
     :                          ACB_LDTW( 1, IACB ),
     :                          ACB_UDTW( 1, IACB ), ARY__MXDIM,
     :                          ACB_LDTW( 1, IACB ),
     :                          ACB_UDTW( 1, IACB ), ACB_DTWEX( IACB ),
     :                          STATUS )
            END IF
         END IF

*  If the array is a base array, then the actual data object bounds must
*  be altered. Obtain an index to the data object entry in the DCB.
      ELSE
         IDCB = ACB_IDCB ( IACB )

*  Check that there is no current access to the array.
         IF ( ( DCB_NREAD( IDCB ) .NE. 0 ) .OR.
     :        ( DCB_NWRIT( IDCB ) .NE. 0 ) ) THEN

*  Report an error if there is.
            STATUS = ARY__ISMAP
            CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
            CALL ERR_REP( 'ARY1_SBND_BMAP',
     :      'The base array structure ^ARRAY is mapped for access, ' //
     :      'perhaps through another identifier (possible ' //
     :      'programming error).', STATUS )

*  Set the new bounds for the data object.
         ELSE
            CALL ARY1_DSBND( NDIM, LBND, UBND, IDCB, SAME, DRX, LX, UX,
     :                       STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If the new bounds were the same as the old ones, then there is
*  nothing more to do.
               IF ( .NOT. SAME ) THEN

*  Otherwise, loop to find all the ACB entries which are affected by the
*  change.
                  IACBT = 0
                  NEXT = 0
3                 CONTINUE       ! Start of 'DO WHILE' loop
                  CALL ARY1_NXTSL( ARY__ACB, IACBT, NEXT, STATUS )
                  IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :                 ( NEXT .NE. 0 ) ) THEN
                     IACBT = NEXT

*  For each ACB entry, obtain an index to the data object entry in the
*  DCB. Check if the ACB entry describes a base array and refers to the
*  data object which has just been altered.
                     IDCBT = ACB_IDCB( IACBT )
                     IF ( ( .NOT. ACB_CUT( IACBT ) ) .AND.
     :                    ( IDCBT .EQ. IDCB ) ) THEN

*  If so, then store the new bounds in the ACB entry.
                        DO 4 I = 1, NDIM
                           ACB_LBND( I, IACBT ) = LBND( I )
                           ACB_UBND( I, IACBT ) = UBND( I )
4                       CONTINUE

*  Pad with 1's if necessary.
                        DO 5 I = NDIM + 1, ARY__MXDIM
                           ACB_LBND( I, IACBT ) = 1
                           ACB_UBND( I, IACBT ) = 1
5                       CONTINUE

*  Store the new number of dimensions.
                        ACB_NDIM( IACBT ) = NDIM
                     END IF
                     GO TO 3
                  END IF

*  Check to see if any bad pixels may have been introduced due to the
*  change in bounds. There will be bad pixels if no data values have
*  been retained.
                  BAD = .NOT. DRX

*  Otherwise, check the bounds of the region of retained data against
*  the new bounds to see if padding with bad pixels has been necessary.
                  IF ( .NOT. BAD ) THEN
                     DO 6 I = 1, ACB_NDIM( IACB )
                        IF ( ( LX( I ) .GT. ACB_LBND( I, IACB ) ) .OR.
     :                       ( UX( I ) .LT. ACB_UBND( I, IACB ) ) ) THEN
                           BAD = .TRUE.
                           GO TO 7
                        END IF
6                    CONTINUE
7                    CONTINUE
                  END IF

*  If bad pixels may have been introduced, then set the bad pixel flag.
                  IF ( BAD ) CALL ARY1_SBD( .TRUE., IACB, STATUS )
               END IF
            END IF
         END IF
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_SBND', STATUS )

      END
