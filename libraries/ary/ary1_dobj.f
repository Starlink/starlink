      SUBROUTINE ARY1_DOBJ( IDCB, STATUS )
*+
*  Name:
*     ARY1_DOBJ

*  Purpose:
*     Ensure that the HDS primitive arrays holding the real and (if
*     necessary) imaginary values have been created.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DOBJ( IDCB, STATUS )

*  Description:
*     The routine ensures that HDS arrays exist to hold the real and (if
*     necessary) imaginary parts of the array data values. This may not
*     be the case if creation of these objects was deferred when calling
*     ARY1_DCRE or ARY1_DCREP.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index of the data object entry in the DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. All Rights Reserved.

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
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     17-JUK-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
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
*           Whether the array is complex.
*        DCB_DLOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to non-imaginary data component.
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           Form of the array.
*        DCB_ILOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to imaginary data component.
*        DCB_KBND( ARY__MXDCB ) = LOGICAL (Read)
*           Whether the DCB bounds information is up to date.
*        DCB_KFRM( ARY__MXDCB ) = LOGICAL (Read)
*           Whether the DCB storage form information is up to date.
*        DCB_KTYP( ARY__MXDCB ) = LOGICAL (Read)
*           Whether the DCB type information is up to date.
*        DCB_LBND( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read)
*           Lower bounds of array.
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and Write)
*           Locator to data object.
*        DCB_NDIM( ARY__MXDCB ) = INTEGER (Read)
*           Number of array dimensions.
*        DCB_TYP( ARY__MXDCB ) = CHARACTER (Read)
*           The DCB type information.
*        DCB_UBND( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read)
*           Upper bounds of array.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL ARY1_DEFR          ! Array creation deferred?

*  Local variables:
      CHARACTER LOCP*( DAT__SZLOC ) ! Locator to parent object
      CHARACTER NAME*( DAT__SZNAM ) ! Name of data object
      INTEGER DIM( ARY__MXDIM )  ! Dimensions of array
      INTEGER I                  ! Loop counter for dimensions

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do nothing if the creation of hte arrays is not deferred.
      IF( ARY1_DEFR( IDCB, STATUS ) ) THEN

*  Report an error if required information is not available. The
*  only situation in which creation is deferred is if ARY1_DCRE(P) is
*  called with its DEFER parameter set TRUE. In this case, all the
*  required information should already be available because ARY1_DCRE(P)
*  will have set it up.
         IF( .NOT. DCB_KTYP( IDCB ) ) THEN
            STATUS = ARY__UNDEF
            CALL ERR_REP( 'ARY1_DOBJ_ERR1', 'ARY1_DOBJ: Deferred '//
     :                    'creation of an array is not possible '//
     :                    'because no type information is available '//
     :                    '(ARY programming error).', STATUS )

         ELSE IF( .NOT. DCB_KBND( IDCB ) ) THEN
            STATUS = ARY__UNDEF
            CALL ERR_REP( 'ARY1_DOBJ_ERR2', 'ARY1_DOBJ: Deferred '//
     :                    'creation of an array is not possible '//
     :                    'because no bounds information is available'//
     :                    ' (ARY programming error).', STATUS )

         ELSE IF( .NOT. DCB_KFRM( IDCB ) ) THEN
            STATUS = ARY__UNDEF
            CALL ERR_REP( 'ARY1_DOBJ_ERR3', 'ARY1_DOBJ: Deferred '//
     :                    'creation of an array is not possible '//
     :                    'because no form information is available'//
     :                    ' (ARY programming error).', STATUS )
         END IF

         IF ( STATUS .NE. SAI__OK ) GO TO 9999

*  Primitive arrays.
*  ================
         IF ( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) THEN

*  Obtain a locator to the placeholder object's parent structure.
            LOCP = ARY__NOLOC
            CALL DAT_PAREN( DCB_LOC( IDCB ), LOCP, STATUS )

*  Obtain the placeholder object's name. Then annul its locator and
*  erase the object.
            CALL DAT_NAME( DCB_LOC( IDCB ), NAME, STATUS )
            CALL DAT_ANNUL( DCB_LOC( IDCB ), STATUS )
            DCB_LOC( IDCB ) = ARY__NOLOC
            CALL DAT_ERASE( LOCP, NAME, STATUS )

*  Create a new primitive array of the required type and shape in its place
*  and obtain a new locator to it.
            CALL DAT_NEW( LOCP, NAME,  DCB_TYP( IDCB ),
     :                    DCB_NDIM( IDCB ), DCB_UBND( 1, IDCB ),
     :                    STATUS )
            CALL DAT_FIND( LOCP, NAME, DCB_LOC( IDCB ), STATUS )

*  Link this locator into a private group to prevent external events
*  annulling it.
            CALL HDS_LINK( DCB_LOC( IDCB ), 'ARY_DCB', STATUS )

*  Obtain a non-imaginary component locator by cloning the data object
*  locator.
            CALL DAT_CLONE( DCB_LOC( IDCB ), DCB_DLOC( IDCB ), STATUS )

*  Simple and scaled arrays.
*  =========================
         ELSE IF ( DCB_FRM( IDCB ) .EQ. 'SIMPLE' .OR.
     :             DCB_FRM( IDCB ) .EQ. 'SCALED' ) THEN

*  Calculate the axis dimensions.
            DO 1 I = 1, DCB_NDIM( IDCB )
               DIM( I ) = DCB_UBND( I, IDCB ) - DCB_LBND( I, IDCB ) + 1
1           CONTINUE

*  Create the non-imaginary data component and obtain a locator to it.
*  Store the locator in the DCB.
            CALL DAT_NEW( DCB_LOC( IDCB ), 'DATA', DCB_TYP( IDCB ),
     :                    DCB_NDIM( IDCB ), DIM, STATUS )
            CALL DAT_FIND( DCB_LOC( IDCB ), 'DATA', DCB_DLOC( IDCB ),
     :                     STATUS )

*  If a complex array is required, then create and locate the imaginary
*  component similarly.
            IF ( DCB_CPX( IDCB ) ) THEN
               CALL DAT_NEW( DCB_LOC( IDCB ), 'IMAGINARY_DATA',
     :                       DCB_TYP( IDCB ), DCB_NDIM( IDCB ), DIM,
     :                       STATUS )
               CALL DAT_FIND( DCB_LOC( IDCB ), 'IMAGINARY_DATA',
     :                        DCB_ILOC( IDCB ), STATUS )
            END IF

*  If the form information in the DCB was not valid, then report an
*  error.
*  ================================================================
         ELSE
            STATUS = ARY__FATIN
            CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB ) )
            CALL ERR_REP( 'ARY1_DOBJ_ERR3', 'Unsupported array form '//
     :                    '''^BADFORM'' found in Data Control Block '//
     :                    '(internal programming error).', STATUS )
         END IF

      END IF

9999  CONTINUE

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DOBJ', STATUS )

      END
