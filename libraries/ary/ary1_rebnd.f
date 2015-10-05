      SUBROUTINE ARY1_REBND( DEFER, PAREN, NAME, TYPE, STATE, NDIM,
     :                       LBND, UBND, NNDIM, NLBND, NUBND, LOC, SAME,
     :                       DRX, LX, UX, STATUS )
*+
*  Name:
*     ARY1_REBND

*  Purpose:
*     Change the bounds of an HDS object containing an array component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_REBND( DEFER, PAREN, NAME, TYPE, STATE, NDIM, LBND,
*                      UBND, NNDIM, NLBND, NUBND, LOC, SAME, DRX, LX,
*                      UX, STATUS )

*  Description:
*     The routine changes the bounds of a primitive numeric HDS object
*     containing data for an array component, while (optionally)
*     preserving its contents. If the contents are to be preserved,
*     then the data will be re-distributed in the data object and
*     padded with "bad" values (if necessary) to retain the initial
*     N-dimensional distribution of data, but stored in the reshaped
*     array. Note that no data values can be preserved (i.e. the new
*     array will contain only "bad" values) if the old and new array
*     bounds do not intersect.

*  Arguments:
*     DEFER = LOGICAL (Given)
*        If .TRUE., then the array being resized has not yet been created.
*        In this case, this routine behaves as normal except that no
*        attempt is made to actually resize the HDS object.
*     PAREN = CHARACTER * ( * ) (Given)
*        HDS locator to the object's parent structure.
*     NAME = CHARACTER * ( * ) (Given)
*        HDS name of the object whose bounds are to be changed.
*     TYPE = CHARACTER * ( * ) (Given)
*        Data type of the object whose bounds are to be changed. This
*        must be a primitive numeric HDS data type string (case
*        insensitive).
*     STATE = LOGICAL (Given)
*        The HDS state of the data object (.TRUE. for defined, .FALSE.
*        for undefined). This argument determines whether the contents
*        of the object are to be preserved. They are only preserved if
*        its value is .TRUE..
*     NDIM = INTEGER (Given)
*        Initial number of object dimensions.
*     LBND( NDIM ) = INTEGER (Given)
*        Initial lower bounds of the array whose component is stored in
*        the data object.
*     UBND( NDIM ) = INTEGER (Given)
*        Initial upper bound of the array whose component is stored in
*        the data object.
*     NNDIM = INTEGER (Given)
*        New number of data object dimensions.
*     NLBND( NNDIM ) = INTEGER (Given)
*        New lower bounds for the array.
*     NUBND( NNDIM ) = INTEGER (Given)
*        New upper bounds for the array.
*     LOC = CHARACTER * ( * ) (Given and Returned)
*        HDS locator to the object whose bounds are to be changed. Note
*        that changing its bounds may involve erasing the original
*        object and creating a new one, so this locator may be changed.
*        If DEFER is .TRUE., the supplied value is ignored, and is
*        unchanged on exit.
*     SAME = LOGICAL (Returned)
*        Returns the value .TRUE. if the new array bounds are the same
*        as the old array bounds, so that the routine had nothing to do.
*     DRX = LOGICAL (Returned)
*        This argument returns a value of .TRUE. if the data regions of
*        the old and new arrays intersect, so that at least some of the
*        original data has been retained in the new array. A value of
*        .FALSE. is returned if STATE is set to .FALSE. or if DEFER is
*        set to .TRUE.
*     LX( ARY__MXDIM ) = INTEGER (Returned)
*        If DRX is returned with a value of .TRUE., then this argument
*        returns the lower bounds of the region in the new array which
*        contains data retained from the old array.
*     UX( ARY__MXDIM ) = INTEGER (Returned)
*        If DRX is returned with a value of .TRUE., then this argument
*        returns the upper bounds of the region in the new array which
*        contains data retained from the old array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Determine whether the new array bounds are the same as the old
*     bounds.
*     -  If so, then set values for the returned arguments and ensure
*     that the data object is in the expected state by resetting it if
*     appropriate.
*     -  If the new and old array bounds differ, then determine whether
*     it will be necessary to create a new data object by seeing whether
*     the new object dimensions differ from the old ones in any except
*     the final dimension.
*     -  Decide whether it will be necessary to make a temporary copy
*     of the data. This depends on (a) whether there are any data
*     values present, (b) whether the new and old array bounds have any
*     region in common and (c) whether the data values will occupy
*     different locations in the new array to those they had in the old
*     array.
*     -  If a temporaray copy of the data is required, then create and
*     map a temporary object to hold it.
*     -  Read the sub-region of data which is to be retained from the
*     data object into the temporary copy.
*     -  If a new data object is to be created, then erase the old one
*     and create the new one with an appropraite shape.
*     -  If the old object is to be re-used, then alter its shape and
*     ensure it is in the expected state by resetting it if
*     appropriate.
*     -  If the array's data values are defined and a new data object
*     has been created and/or a temporary copy of data was necessary,
*     then the (new) object may need initialisation before the data are
*     returned if these will not fill it entirely. Check to see whether
*     the object will be filled by the returned data.
*     -  If it will not be filled, then map the object and initiallise
*     it by filling it with "bad" values. Then unmap it.
*     -  If the old data object has been re-used and its data values
*     have remained in their original locations, but its final
*     dimension size is larger than it was originally, then there will
*     be a region at the end of the object requiring initialisation.
*     Calculate the positions of the first and last new elements in the
*     vectorised object which will need to be initialised.
*     -  Vectorise the object and locate the slice needing
*     initialisation.
*     -  Map the slice and set it to "bad" values. Then annul the slice
*     and vectorised object locators.
*     -  If a temporary copy of the data is being held, then write the
*     data values back into the appropriate sub-region of the new
*     array.  Then erase the temporary object holding the copy.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     18-SEP-1989 (RFWS):
*        Original version.
*     20-OCT-1989 (RFWS):
*        Added code to perform initialisation of new regions of an
*        object which has been re-used, but whose extent has been
*        enlarged.
*     5-DEC-1989 (RFWS):
*        Fixed logical problem which caused the temporary data copy not
*        to be returned to the array if the same object was being
*        re-used.
*     6-DEC-1989 (RFWS):
*        Changed the dimension of the LX and UX arrays to be NNDIM and
*        improved the commenting and prologue. Also added initialisation
*        of the DRX argument if STATE is .FALSE..
*     7-DEC-1989 (RFWS):
*        Changed the dimensions of LX and UX back to ARY__MXDIM as the
*        previous change introduced a bug in the call to ARY1_GTN.
*     18-JUL-2006 (DSB):
*        Check for null LOC values (e.g. supplied if creation of the HDS
*        arrays has been deferred - as is done by ARY_DUPE).
*     1-SEP-2006 (DSB):
*        Add an explicit DEFER argument.
*     2-OCT-2015 (DSB):
*        If the returned array uses a slice from the original array to 
*        hold its values, ensure that the slice does not include elements 
*        before the start of the original array. Previously, this was the 
*        case if there was a gap between the output and input arrays on 
*        the last dimension. 
*     {enter_further_changes_here}

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

*  Arguments Given:
      LOGICAL DEFER
      CHARACTER * ( * ) PAREN
      CHARACTER * ( * ) NAME
      CHARACTER * ( * ) TYPE
      LOGICAL STATE
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER NNDIM
      INTEGER NLBND( NNDIM )
      INTEGER NUBND( NNDIM )

*  Arguments Given and Returned:
      CHARACTER * ( * ) LOC

*  Arguments Returned:
      LOGICAL SAME
      LOGICAL DRX
      INTEGER LX( ARY__MXDIM )
      INTEGER UX( ARY__MXDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCS ! Locator to vector slice
      CHARACTER * ( DAT__SZLOC ) LOCV ! Locator to vectorised object
      CHARACTER * ( DAT__SZLOC ) TLOC ! Temporary object locator
      INTEGER DIM( ARY__MXDIM )  ! Object dimensions
      INTEGER EL                 ! Number of data elements
      INTEGER I                  ! Loop counter for dimensions
      INTEGER LSLICE( 1 )        ! Lower bound of vector slice
      INTEGER PNTR               ! Pointer to mapped data
      INTEGER STRIDE             ! Stride of final object dimension
      INTEGER TPNTR              ! Pointer to mapped temporary data copy
      INTEGER USLICE( 1 )        ! Upper bound of vector slice
      LOGICAL CPYDAT             ! Whether temporary data copy is needed
      LOGICAL DCE                ! Whether data conversion errors occur
      LOGICAL FULL               ! Temporary data copy fills new array?
      LOGICAL NEWOBJ             ! Whether a new data object is needed
      LOGICAL THERE              ! Does the named component exist?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine whether the new array bounds are the same as the old
*  bounds. First see if the number of dimensions are the same.
      SAME = NNDIM .EQ. NDIM
      IF ( SAME ) THEN

*  If so, then test the bounds of each dimension for equality.
         DO 1 I = 1, NDIM
            IF ( ( LBND( I ) .NE. NLBND( I ) ) .OR.
     :           ( UBND( I ) .NE. NUBND( I ) ) ) THEN
               SAME = .FALSE.
               GO TO 2
            END IF
1        CONTINUE
2        CONTINUE
      END IF

*  If the new bounds are the same as the old ones, then no reshaping of
*  the array is necessary. Set values for the returned arguments.
      IF ( SAME ) THEN
         DRX = .TRUE.
         DO 3 I = 1, NNDIM
            LX( I ) = NLBND( I )
            UX( I ) = NUBND( I )
3        CONTINUE
         DO 4 I = NNDIM + 1, ARY__MXDIM
            LX( I ) = 1
            UX( I ) = 1
4        CONTINUE

*  Ensure that the data object is in the expected state by resetting it
*  if appropriate.
         IF ( .NOT. STATE .AND. .NOT. DEFER ) THEN
            CALL DAT_RESET( LOC, STATUS )
         END IF

*  If the new and old bounds are not the same, then see if it is
*  necessary to create a new data object to hold the new array (as
*  opposed to simply altering the existing object). A new object is
*  necessary if the number of dimensions will change, as HDS does not
*  support this.
      ELSE
         NEWOBJ = NNDIM .NE. NDIM

*  If the number of dimensions match, then the same data object can be
*  re-used only if the dimension sizes of all except the last dimension
*  are the same in the new array as the old one. Check each of these
*  sizes for equality.
         IF ( .NOT. NEWOBJ ) THEN
            DO 5 I = 1, NDIM - 1
               IF ( ( UBND( I ) - LBND( I ) ) .NE.
     :              ( NUBND( I ) - NLBND( I ) ) ) THEN
                  NEWOBJ = .TRUE.
                  GO TO 6
               END IF
5           CONTINUE
6           CONTINUE
         END IF

*  Now decide whether it will be necessary to make a temporary copy of
*  the array's data while its bounds are altered. This will not be
*  necessary if the data values are currently undefined.
         IF ( .NOT. STATE .OR. DEFER ) THEN
            CPYDAT = .FALSE.
            DRX = .FALSE.

*  If the data values are defined, then see if there is any region in
*  common between the new array and the old array.
         ELSE
            CALL ARY1_XSBND( NDIM, LBND, UBND, NNDIM, NLBND, NUBND,
     :                       ARY__MXDIM, LX, UX, DRX, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If a new data object is to be created, then a temporary copy of the
*  data will be required if the new array will contain any part of the
*  data from the old array.
               IF ( NEWOBJ ) THEN
                  CPYDAT = DRX

*  Otherwise (i.e. the same object will be re-used) if there is no
*  region in common between the new and old arrays, then a temporary
*  copy will not be needed.
               ELSE IF ( .NOT. DRX ) THEN
                  CPYDAT = .FALSE.

*  In all other cases, a temporary copy of the data will be needed
*  unless the data values will occupy the same elements in the new
*  array as in the old. This is true if all dimension bounds except the
*  final upper bound are the same in the new and old arrays. Test for
*  bounds equality in all except the final dimension.
               ELSE
                  CPYDAT = .FALSE.
                  DO 7 I = 1, NDIM - 1
                     IF ( ( LBND( I ) .NE. NLBND( I ) ) .OR.
     :                    ( UBND( I ) .NE. NUBND( I ) ) ) THEN
                        CPYDAT = .TRUE.
                        GO TO 8
                     END IF
7                 CONTINUE
8                 CONTINUE

*  Test for lower bounds equality in the final dimension.
                  CPYDAT = CPYDAT .OR.
     :                     ( LBND( NDIM ) .NE. NLBND( NDIM ) )
               END IF
            END IF
         END IF

*  If a temporary copy of the data is required, then determine the
*  dimension sizes of the region of data in common between the old and
*  new arrays.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( CPYDAT ) THEN
               DO 9 I = 1, MIN( NDIM, NNDIM )
                  DIM( I ) = UX( I ) - LX( I ) + 1
9              CONTINUE

*  Create and map workspace to hold this region of data.
               CALL ARY1_CMTMP( TYPE, MIN( NDIM, NNDIM ), DIM, TLOC,
     :                          TPNTR, STATUS )

*  Read the appropriate part of the old array into this workspace.
               CALL ARY1_GTN( .FALSE., TYPE, LOC, NDIM, LBND, UBND,
     :                        LX, UX, TYPE, LX, UX, .FALSE.,
     :                        DAT__NOLOC, TPNTR, DCE, STATUS )
            END IF

*  Calculate the dimension sizes for the new array.
            DO 10 I = 1, NNDIM
               DIM( I ) = NUBND( I ) - NLBND( I ) + 1
10          CONTINUE

*  Do not change the HDS object if the creation of the HDS data array
*  has been deferred.
            IF( .NOT. DEFER ) THEN

*  If a new data object is required, then annul the locator to the
*  original object and erase it.
               IF ( NEWOBJ ) THEN
                  CALL DAT_ANNUL( LOC, STATUS )
                  LOC = ARY__NOLOC

                  CALL DAT_THERE( PAREN, NAME, THERE, STATUS )
                  IF( THERE ) CALL DAT_ERASE( PAREN, NAME, STATUS )

*  Create the new object and obtain a locator to it.
                  CALL DAT_NEW( PAREN, NAME, TYPE, NNDIM, DIM, STATUS )
                  CALL DAT_FIND( PAREN, NAME, LOC, STATUS )

*  If the same data object is being re-used, then alter its shape.
               ELSE
                  CALL DAT_ALTER( LOC, NNDIM, DIM, STATUS )

*  Ensure that the object is in the expected state by resetting it if
*  appropriate.
                  IF ( .NOT. STATE ) THEN
                     CALL DAT_RESET( LOC, STATUS )
                  END IF
               END IF

*  If the array's data values are defined, and a new data object has
*  been created and/or a temporary data copy was made, then the (new)
*  object may require initialisation before the data are returned if
*  these will not fill it entirely.
               IF( STATE ) THEN
                  IF ( NEWOBJ .OR. CPYDAT ) THEN

*  Check to see if the temporary data copy will completely fill the
*  object so that there is no need to initialise.
                     FULL = CPYDAT
                     IF ( FULL ) THEN
                        DO 11 I = 1, NNDIM
                           IF ( ( LX( I ) .GT. NLBND( I ) ) .OR.
     :                          ( UX( I ) .LT. NUBND( I ) ) ) THEN
                              FULL = .FALSE.
                              GO TO 12
                           END IF
11                      CONTINUE
12                      CONTINUE
                     END IF

*  If the object will not be filled by the returned data, then map it
*  and initialise it to "bad" values, then unmap it.
                     IF ( .NOT. FULL ) THEN
                        CALL DAT_MAPV( LOC, TYPE, 'WRITE', PNTR, EL,
     :                                 STATUS )
                        CALL ARY1_VBAD( TYPE, EL, PNTR, STATUS )
                        CALL ARY1_HUNMP( LOC, STATUS )
                     END IF

*  If the same data object has been re-used and the data have remained
*  in their original locations (i.e. no temporary copy was necessary)
*  but the final dimension has been increased, then there will be a
*  region at the end of the object to be initialised.
                  ELSE IF ( NUBND( NNDIM ) .GT. UBND( NNDIM ) ) THEN

*  Calculate the stride for the object's final dimension (the amount by
*  which the vectorised array index increases when the index of the
*  final dimension increases by 1).
                     STRIDE = 1
                     DO 13 I = 1, NNDIM - 1
                        STRIDE = STRIDE * ( NUBND( I ) - NLBND( I )
     :                                                           + 1 )
13                   CONTINUE

*  Calculate the first and last elements of the region to be initialised
*  in the vectorised data object.
                     LSLICE( 1 ) = STRIDE *
     :                            ( UBND( NNDIM ) - NLBND( NNDIM ) + 1 )
     :                            + 1
                     USLICE( 1 ) = STRIDE *
     :                           ( NUBND( NNDIM ) - NLBND( NNDIM ) + 1 )

*  If there is a gap between the two arrays (i.e. they do not overlap on
*  the last pixel axis), UBND( NNDIM ) will be less than NLBND( NNDIM )
*  and so the above LSLICE( 1 ) value will be negative, causing DAT_SLICE
*  to report an error.
                     LSLICE( 1 ) = MAX( 1, LSLICE( 1 ) )

*  Vectorise the data object.
                     LOCV = ARY__NOLOC
                     CALL DAT_VEC( LOC, LOCV, STATUS )

*  Locate a slice from this vector containing the region to be
*  initialised.
                     LOCS = ARY__NOLOC
                     CALL DAT_SLICE( LOCV, 1, LSLICE, USLICE, LOCS,
     :                               STATUS )

*  Map the slice and set it to "bad" values.
                     CALL DAT_MAPV( LOCS, TYPE, 'WRITE', PNTR, EL,
     :                              STATUS )
                     CALL ARY1_VBAD( TYPE, EL, PNTR, STATUS )

*  Annul the slice and vector locators.
                     CALL DAT_ANNUL( LOCS, STATUS )
                     LOCS = ARY__NOLOC
                     CALL DAT_ANNUL( LOCV, STATUS )
                     LOCV = ARY__NOLOC
                  END IF

*  If a temporary copy of data is being held, then write it back into
*  the appropriate region of the new object. Then erase the workspace.
                  IF ( CPYDAT ) THEN
                     CALL ARY1_PTN( .FALSE., NNDIM, LX, UX, TYPE, TPNTR,
     :                              LX, UX, NLBND, NUBND, TYPE, LOC,
     :                              DCE, STATUS )
                     CALL ARY1_ANTMP( TLOC, STATUS )
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_REBND', STATUS )

      END
