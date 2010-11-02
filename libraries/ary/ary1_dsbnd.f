      SUBROUTINE ARY1_DSBND( NDIM, LBND, UBND, IDCB, SAME, DRX, LX, UX,
     :                       STATUS )
*+
*  Name:
*     ARY1_DSBND

*  Purpose:
*     Change the bounds of an array data object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DSBND( NDIM, LBND, UBND, IDCB, SAME, DRX, LX, UX,
*     STATUS )

*  Description:
*     The routine changes the lower and upper bounds of an array data
*     object identified by its DCB entry. If the array's data values
*     are defined, then any data which lie within both the original and
*     new array bounds will be preserved in the correct pixel location
*     within the re-structured array. Any new pixel locations created
*     by the imposition of new bounds will be filled with the bad value
*     (unless the array's data values are undefined). The bad pixel
*     flag is not altered by this routine.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The new number of array dimensions.
*     LBND( NDIM ) = INTEGER (Given)
*        The new lower pixel index bounds for the array.
*     UBND( NDIM ) = INTEGER (Given)
*        The new upper pixel index bounds for the array.
*     IDCB = INTEGER (Given)
*        The DCB entry for the array.
*     SAME = LOGICAL (Returned)
*        Returned as .TRUE. if the new array bounds are the same as the
*        old bounds (in which case the routine will have returned
*        without action as there is nothing to do).
*     DRX = LOGICAL (Returned)
*        Returned as .TRUE. if there is at least 1 pixel value
*        which lies within both the old and new pixel index bounds. A
*        value of .FALSE. is returned if the array's data values are
*        undefined.
*     LX( ARY__MXDIM ) = INTEGER (Returned)
*        The lower pixel index bounds of the region in common between
*        the old and new array bounds. Not used if DRX is .FALSE..
*     UX( ARY__MXDIM ) = INTEGER (Given)
*        The upper pixel index bounds of the region in common between
*        the old and new array bounds. Not used if DRX is .FALSE..
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Ensure that storage form information is available in the DCB.
*     -  Handle each form of array in turn.
*     -  For primitive arrays, ensure that data type, state and bounds
*     information is available for the data object in the DCB.
*     -  See if conversion from primitive to simple storage form is
*     required.  Conversion is required if any of the new lower array
*     bounds is not equal to 1.
*     -  If no conversion is needed, then the bounds of the primitive
*     array must be changed. This may involve erasing the existing
*     object and creating a new one, so annul the data component
*     locator which will be re-acquired later.
*     -  Obtain a locator to the array's parent structure and obtain
*     the name of the array.
*     -  Change the array bounds, possibly obtaining a new data object
*     locator as a result.
*     -  Derive a new non-imaginary component locator by cloning the
*     data object locator.
*     -  If conversion from primitive to simple storage form is needed,
*     then perform the conversion.
*     -  Report context information if the conversion failed.
*     -  Otherwise, change the bounds of the non-imaginary data
*     component in what is now a simple array.
*     -  Create an ORIGIN component in the data object and enter the
*     new origin values.
*     -  For simple arrays, ensure that data type, state and bounds
*     information is available in the DCB.
*     -  Change the bounds of the non-imaginary array component.
*     -  If the array holds complex values, then change the bounds of
*     its imaginary component.
*     -  See if the array's origin has changed. It has if the number of
*     dimensions has changed. Otherwise, check the lower bound of each
*     dimension to see if that has changed.
*     -  If the origin has changed, then see if an origin component is
*     present in the data object. If not, then create one.
*     -  Obtain a locator to the origin component.
*     -  If the component previously existed, but now has the wrong
*     number of elements, then change the number of elements.
*     -  Enter the origin values and annul the origin locator.
*     -  If the DCB form entry was not recognised, then report an
*     error.
*     -  Modify the DCB bounds information to reflect the changes made
*     to the data object.
*     -  Note if bounds information is now available in the DCB.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     All Rights Reserved.
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. All Rights Reserved.
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     4-DEC-1989 (RFWS):
*        Original version.
*     7-DEC-1989 (RFWS):
*        Changed the dimension sizes of the LX and UX arrays as they
*        were otherwise too small for ARY1_REBND to use.
*     13-FEB-1990 (RFWS):
*        Installed support for primitive arrays.
*     24-SEP-1990 (RFWS):
*        Added annulling of the parent locator for simple arrays, which
*        had been omitted.
*     10-OCT-1990 (RFWS):
*        Changed to call ARY1_PAREN as a temporary work around for
*        problems with DAT_PAREN.
*     5-MAY-2006 (DSB):
*        Installed support for scaled arrays.
*     17-JUL-2006 (DSB):
*        Guard against null DCB_DLOC locators.
*     1-SEP-2006 (DSB):
*        Add DEFER argument to calls to ARY1_REBND.
*     1-NOV-2010 (DSB):
*        Include support for delta compressed arrays.
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
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_CPX( ARY__MXDCB ) = LOGICAL (Read)
*           Whether the array holds complex data.
*        DCB_DLOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Non-imaginary component locator.
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           Array storage form.
*        DCB_ILOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Imaginary component locator.
*        DCB_KBND( ARY__MXDCB ) = LOGICAL (Write)
*           Whether the array bounds information in the DCB is up to
*           date.
*        DCB_LBND( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read and Write)
*           Lower pixel index bounds of the array.
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Data object locator.
*        DCB_NDIM( ARY__MXDCB ) = INTEGER (Read and Write)
*           Number of array dimensions.
*        DCB_STA( ARY__MXDCB ) = LOGICAL (Read)
*           Whether the array's data values are defined.
*        DCB_TYP( ARY__MXDCB ) = CHARACTER * ( ARY__SZTYP ) (Read)
*           Array numeric data type.
*        DCB_UBND( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read and Write)
*           Upper pixel index bounds of the array.

*  Arguments Given:
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER IDCB

*  Arguments Returned:
      LOGICAL SAME
      LOGICAL DRX
      INTEGER LX( ARY__MXDIM )
      INTEGER UX( ARY__MXDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL ARY1_DEFR

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! Origin component locator
      CHARACTER * ( DAT__SZLOC ) LOCP ! Parent structute locator
      CHARACTER * ( DAT__SZNAM ) NAME ! Object name
      INTEGER DIM( 1 )           ! Size of the origin component
      INTEGER I                  ! Loop counter for dimensions
      LOGICAL CHORIG             ! Whether the origin has changed
      LOGICAL CVT                ! Whether form conversion is needed
      LOGICAL DEFER              ! Creation of data array deferred?
      LOGICAL THERE              ! Whether origin component present

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that form information is available in the DCB.
      CALL ARY1_DFRM( IDCB, STATUS )

*  Set a flag indicating if creation of the HDS primitive data array
*  has been deferrred until the array is mapped.
      DEFER = ARY1_DEFR( IDCB, STATUS )

*  Handle each form of array in turn...
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Primitive arrays.
*  ================
         IF ( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) THEN

*  Ensure that data type, state and bounds information is available for
*  the data object in the DCB.
            CALL ARY1_DTYP( IDCB, STATUS )
            CALL ARY1_DSTA( IDCB, STATUS )
            CALL ARY1_DBND( IDCB, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  See if conversion from primitive to simple storage form is required.
               CVT = .FALSE.
               DO 1 I = 1, NDIM

*  Conversion is required if any of the new lower array bounds is not
*  equal to 1.
                  IF ( LBND( I ) .NE. 1 ) THEN
                     CVT = .TRUE.
                     GO TO 2
                  END IF
1              CONTINUE
2              CONTINUE

*  If no conversion is needed, then the bounds of the primitive array
*  must be changed. This may involve erasing the existing object and
*  creating a new one, so annul the non-imaginary component locator
*  which will be re-acquired later.
               IF ( .NOT. CVT ) THEN
                  IF( DCB_DLOC( IDCB ) .NE. ARY__NOLOC ) THEN
                     CALL DAT_ANNUL( DCB_DLOC( IDCB ), STATUS )
                     DCB_DLOC( IDCB ) = ARY__NOLOC
                  END IF

*  Obtain a locator to the array's parent structure and obtain the name
*  of the array.
                  LOCP = ARY__NOLOC
                  CALL DAT_PAREN( DCB_LOC( IDCB ), LOCP, STATUS )
                  CALL DAT_NAME( DCB_LOC( IDCB ), NAME, STATUS )

*  Change the array bounds, possibly obtaining a new data object locator
*  as a result.
                  CALL ARY1_REBND( DEFER, LOCP, NAME, DCB_TYP( IDCB ),
     :                             DCB_STA( IDCB ), DCB_NDIM( IDCB ),
     :                             DCB_LBND( 1, IDCB ),
     :                             DCB_UBND( 1, IDCB ), NDIM, LBND,
     :                             UBND, DCB_LOC( IDCB ), SAME, DRX,
     :                             LX, UX, STATUS )

*  Derive a new non-imaginary component locator by cloning the data object
*  locator. We do not do this if the creation of the HDS data array has
*  been deferred since the a null value for DLOC is one of the things
*  that flags a deferred array (see ARY1_DEFR).
                  IF( .NOT. DEFER ) THEN
                     CALL DAT_CLONE( DCB_LOC( IDCB ), DCB_DLOC( IDCB ),
     :                               STATUS )
                  END IF

*  Annul the parent locator.
                  CALL DAT_ANNUL( LOCP, STATUS )
                  LOCP = ARY__NOLOC

*  If conversion from primitive to simple storage form is needed, then
*  perform the conversion.
               ELSE
                  CALL ARY1_DP2S( IDCB, STATUS )

*  Report context information if the conversion failed.
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_REP( 'ARY1_DSBND_CVT',
     :               'Unable to perform implicit conversion from ' //
     :               '''PRIMITIVE'' to ''SIMPLE'' array storage form.',
     :               STATUS )
                  ELSE

*  Otherwise, change the bounds of the non-imaginary data component in
*  what is now a simple array.
                     CALL ARY1_REBND( DEFER, DCB_LOC( IDCB ), 'DATA',
     :                                DCB_TYP( IDCB ), DCB_STA( IDCB ),
     :                                DCB_NDIM( IDCB ),
     :                                DCB_LBND( 1, IDCB ),
     :                                DCB_UBND( 1, IDCB ),
     :                                NDIM, LBND, UBND,
     :                                DCB_DLOC( IDCB ), SAME, DRX,
     :                                LX, UX, STATUS )

*  Create an ORIGIN component in the data object and enter the new
*  origin values.
                     CALL DAT_NEW1I( DCB_LOC( IDCB ), 'ORIGIN', NDIM,
     :                               STATUS )
                     CALL CMP_PUT1I( DCB_LOC( IDCB ), 'ORIGIN', NDIM,
     :                               LBND, STATUS )
                  END IF
               END IF
            END IF

*  Simple and scaled arrays.
*  =========================
         ELSE IF ( DCB_FRM( IDCB ) .EQ. 'SIMPLE' .OR.
     :             DCB_FRM( IDCB ) .EQ. 'SCALED' ) THEN

*  Ensure that data type, state and bounds information is available for
*  the data object in the DCB.
            CALL ARY1_DTYP( IDCB, STATUS )
            CALL ARY1_DSTA( IDCB, STATUS )
            CALL ARY1_DBND( IDCB, STATUS )

*  Change the bounds of the non-imaginary data component.
            CALL ARY1_REBND( DEFER, DCB_LOC( IDCB ), 'DATA',
     :                       DCB_TYP( IDCB ), DCB_STA( IDCB ),
     :                       DCB_NDIM( IDCB ), DCB_LBND( 1, IDCB ),
     :                       DCB_UBND( 1, IDCB ), NDIM, LBND, UBND,
     :                       DCB_DLOC( IDCB ), SAME, DRX, LX, UX,
     :                       STATUS )

*  If the array holds complex data, then change the bounds of the
*  imaginary data component.
            IF ( DCB_CPX( IDCB ) ) THEN
               CALL ARY1_REBND( DEFER, DCB_LOC( IDCB ),
     :                          'IMAGINARY_DATA', DCB_TYP( IDCB ),
     :                          DCB_STA( IDCB ), DCB_NDIM( IDCB ),
     :                          DCB_LBND( 1, IDCB ),
     :                          DCB_UBND( 1, IDCB ), NDIM, LBND, UBND,
     :                          DCB_ILOC( IDCB ), SAME, DRX, LX, UX,
     :                          STATUS )
            END IF

*  See if the array origin has changed. It will have done if the number
*  of dimensions has changed.
            IF ( STATUS .EQ. SAI__OK ) THEN
               CHORIG = NDIM .NE. DCB_NDIM( IDCB )

*  Otherwise, check the new lower bound of the array in each dimension
*  to see if that has changed.
               IF ( .NOT. CHORIG ) THEN
                  DO 3 I = 1, NDIM
                     IF ( LBND( I ) .NE. DCB_LBND( I, IDCB ) ) THEN
                        CHORIG = .TRUE.
                        GO TO 4
                     END IF
3                 CONTINUE
4                 CONTINUE
               END IF

*  If the origin has changed, then see if an ORIGIN component is present
*  in the data object.
               IF ( CHORIG ) THEN
                  CALL DAT_THERE( DCB_LOC( IDCB ), 'ORIGIN', THERE,
     :                            STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If not, then create one with the required number of elements.
                     IF ( .NOT. THERE ) THEN
                        CALL DAT_NEW1I( DCB_LOC( IDCB ), 'ORIGIN',
     :                                  NDIM, STATUS )
                     END IF

*  Obtain a locator to the ORIGIN component.
                     LOC = ARY__NOLOC
                     CALL DAT_FIND( DCB_LOC( IDCB ), 'ORIGIN', LOC,
     :                              STATUS )

*  If this component was initially present, but now has the wrong
*  number of elements, then change the number of elements.
                     IF ( THERE .AND.
     :                    ( NDIM .NE. DCB_NDIM( IDCB ) ) ) THEN
                        DIM( 1 ) = NDIM
                        CALL DAT_ALTER( LOC, 1, DIM, STATUS )
                     END IF

*  Enter the origin values.
                     CALL DAT_PUT1I( LOC, NDIM, LBND, STATUS )

*  Annul the origin locator.
                     CALL DAT_ANNUL( LOC, STATUS )
                     LOC = ARY__NOLOC
                  END IF
               END IF
            END IF


*  Delta arrays
*  ============
         ELSE IF( DCB_FRM( IDCB ) .EQ. 'DELTA' ) THEN

*  Delta arrays cannot be changed, so report an error if this routine is
*  called.
            IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = ARY__FATIN
               CALL ERR_REP( 'ARY1_DSBND_CVT', 'Illegal attempt to '//
     :                       'change the bounds of a DELTA compressed'//
     :                       ' array (programming error).', STATUS )
            END IF


*  If the DCB form value was not recognised, then report an error.
         ELSE
            STATUS = ARY__FATIN
            CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB ) )
            CALL ERR_REP( 'ARY1_DSBND_ERR',
     :      'Invalid array form ''^BADFORM'' found in Data ' //
     :      'Control Block (internal programming error).', STATUS )
         END IF
      END IF

*  Modify the DCB bounds information to reflect the changes made to the
*  data object.
      IF ( STATUS .EQ. SAI__OK ) THEN
         DO 5 I = 1, NDIM
            DCB_LBND( I, IDCB ) = LBND( I )
            DCB_UBND( I, IDCB ) = UBND( I )
5        CONTINUE

*  Pad the bounds with 1's if necessary.
         DO 6 I = NDIM + 1, ARY__MXDIM
            DCB_LBND( I, IDCB ) = 1
            DCB_UBND( I, IDCB ) = 1
6        CONTINUE
         DCB_NDIM( IDCB ) = NDIM
      END IF

*  Note if bounds information is now available in the DCB.
      DCB_KBND( IDCB ) = STATUS .EQ. SAI__OK

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DSBND', STATUS )

      END
