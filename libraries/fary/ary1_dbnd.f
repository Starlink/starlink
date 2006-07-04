      SUBROUTINE ARY1_DBND( IDCB, STATUS )
*+
*  Name:
*     ARY1_DBND

*  Purpose:
*     Ensure that bounds (and dimensionality) information is available
*     for a data object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DBND( IDCB, STATUS )

*  Description:
*     The routine ensures that bounds and dimensionality information is
*     available for an entry in the DCB. The routine does nothing if
*     this information is already available. Otherwise, it obtains the
*     information by inspecting the data object itself and stores the
*     results in the DCB. Only those checks necessary to obtain the
*     bounds information are performed on the data object.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index of the data object entry in the DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Do nothing if the information is already present in the DCB.
*     -  Ensure that form information is available for the data object.
*     -  Handle each form of array separately.
*     -  For primitive arrays, ensure that data type information is
*     available, then obtain the shape of the array.
*     -  Check that it is not scalar, and report an error if it is.
*     -  Enter the dimensionality and bounds information into the DCB,
*     padding the bounds with 1's if necessary.
*     -  For simple arrays, ensure that data type information and
*     component locators are available.
*     -  Obtain the shape of the non-imaginary component and check it is
*     not scalar. Report an error if it is.
*     -  If the array is complex, then obtain the shape of the imaginary
*     component and check it is the same as the non-imaginary component.
*     Report an error if it is not.
*     -  If there is no ORIGIN component, then supply default values for
*     it.
*     -  If there is an ORIGIN component, then obtain its type and shape
*     and check that these are valid, reporting an error if they are
*     not. Obtain the ORIGIN values.
*     -  Store the array bounds information in the DCB and note this
*     fact.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     12-JUN-1989 (RFWS):
*        Original version.
*     16-AUG-1989 (RFWS):
*        Changed initialisation of locators to use global constant.
*     23-AUG-1989 (RFWS):
*        Added missing space to error message.
*     12-FEB-1990 (RFWS):
*        Added support for primitive arrays.
*     26-APR-2006 (DSB):
*        Added support for scaled arrays.
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
*        DCB_DLOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to non-imaginary data component.
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           Form of the array.
*        DCB_ILOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to imaginary data component.
*        DCB_KBND( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Whether the DCB bounds information is up to date.
*        DCB_LBND( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Write)
*           Lower bounds of array.
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to data object.
*        DCB_NDIM( ARY__MXDCB ) = INTEGER (Write)
*           Number of array dimensions.
*        DCB_UBND( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Write)
*           Upper bounds of array.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( DAT__SZLOC ) LOCOR ! Locator to ORIGIN component
      CHARACTER * ( DAT__SZTYP ) TYPE ! HDS data type
      INTEGER DIMD( ARY__MXDIM ) ! Dimensions of DATA component
      INTEGER DIMI( ARY__MXDIM ) ! Dimensions of IMAGINARY_DATA compnt
      INTEGER DIMOR( 1 )         ! Dimensions of ORIGIN component
      INTEGER I                  ! Loop counter for dimensions
      INTEGER NDIMD              ! No. of DATA dimensions
      INTEGER NDIMI              ! No. of IMAGINARY_DATA dimensions
      INTEGER NDIMOR             ! Number of ORIGIN dimensions
      INTEGER ORIG( ARY__MXDIM ) ! Origin values
      LOGICAL THERE              ! Whether a component exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do nothing if bounds information is ready available in the DCB.
      IF ( .NOT. DCB_KBND( IDCB ) ) THEN

*  Ensure that form information is available.
         CALL ARY1_DFRM( IDCB, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 9999

*  Primitive arrays.
*  ================
         IF ( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) THEN

*  Ensure that data type information is available, then obtain the
*  shape of the array.
            CALL ARY1_DTYP( IDCB, STATUS )
            CALL DAT_SHAPE( DCB_DLOC( IDCB ), ARY__MXDIM, DIMD, NDIMD,
     :                      STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 9999

*  Check that it is not scalar, and report an error if it is.
            IF ( NDIMD .EQ. 0 ) THEN
               STATUS = ARY__NDMIN
               CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
               CALL ERR_REP( 'ARY1_DBND_PSCLR',
     :         'The array ^ARRAY is a scalar; it should have at ' //
     :         'least one dimension.', STATUS )
               GO TO 9999
            END IF

*  Enter the dimensionality and bounds information into the DCB, padding
*  the bounds with 1's if necessary.
            DCB_NDIM( IDCB ) = NDIMD
            DO 1 I = 1, NDIMD
               DCB_LBND( I , IDCB ) = 1
               DCB_UBND( I , IDCB ) = DIMD( I )
1           CONTINUE
            DO 2 I = NDIMD + 1, ARY__MXDIM
               DCB_LBND( I, IDCB ) = 1
               DCB_UBND( I, IDCB ) = 1
2           CONTINUE

*  Simple and scaled arrays.
*  =========================
         ELSE IF ( DCB_FRM( IDCB ) .EQ. 'SIMPLE' .OR.
     :             DCB_FRM( IDCB ) .EQ. 'SCALED' ) THEN

*  Ensure that data type information is available, then obtain the shape
*  of the non-imaginary array component.
            CALL ARY1_DTYP( IDCB, STATUS )
            CALL DAT_SHAPE( DCB_DLOC( IDCB ), ARY__MXDIM, DIMD, NDIMD,
     :      STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 9999

*  Check that it is not scalar, and report an error if it is.
            IF ( NDIMD .EQ. 0 ) THEN
               STATUS = ARY__NDMIN
               CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
               CALL ERR_REP( 'ARY1_DBND_SCLR',
     :         'The DATA component in the array structure ^ARRAY ' //
     :         'is a scalar; it should have at least one dimension.',
     :         STATUS )
               GO TO 9999
            END IF

*  If the array is complex, then obtain the shape of the complex
*  component.
            IF ( DCB_CPX( IDCB ) ) THEN
               CALL DAT_SHAPE( DCB_ILOC( IDCB ), ARY__MXDIM, DIMI,
     :         NDIMI, STATUS )
               IF ( STATUS .NE. SAI__OK ) GO TO 9999

*  Report an error if the two components have different
*  dimensionalities.
               IF ( NDIMI .NE. NDIMD ) THEN
                  STATUS = ARY__NDMIN
                  CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                  CALL MSG_SETI( 'NDIMD', NDIMD )
                  CALL MSG_SETI( 'NDIMI', NDIMI )
                  CALL ERR_REP( 'ARY1_DBND_NDIM',
     :            'The IMAGINARY_DATA component in the array ' //
     :            'structure ^ARRAY has a different number of ' //
     :            'dimensions (^NDIMI) to the corresponding DATA ' //
     :            'component (^NDIMD).', STATUS )
                  GO TO 9999

*  Report an error if any of the dimension sizes is different in the
*  two components.
               ELSE
                  DO 3 I = 1, NDIMD
                     IF ( DIMI( I ) .NE. DIMD( I ) ) THEN
                        STATUS = ARY__DIMIN
                        CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                        CALL MSG_SETI( 'I', I )
                        CALL MSG_SETI( 'DIMD', DIMD( I ) )
                        CALL MSG_SETI( 'DIMI', DIMI( I ) )
                        CALL ERR_REP( 'ARY1_DBND_DIM',
     :                  'Dimension no. ^I of the IMAGINARY_DATA ' //
     :                  'component in the array structure ^ARRAY ' //
     :                  'has a different size (^DIMI) to the ' //
     :                  'corresponding dimension of the array''s ' //
     :                  'DATA component (^DIMD).', STATUS )
                        GO TO 9999
                     END IF
3                 CONTINUE
               END IF
            END IF

*  See if there is an ORIGIN component present.
            CALL DAT_THERE( DCB_LOC( IDCB ), 'ORIGIN', THERE, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 9999

*  If not, then supply default origin values.
            IF ( .NOT. THERE ) THEN
               DO 4 I = 1, NDIMD
                  ORIG( I ) = 1
4              CONTINUE

*  If there is, then obtain its type and shape.
            ELSE
               LOCOR = ARY__NOLOC
               CALL DAT_FIND( DCB_LOC( IDCB ), 'ORIGIN', LOCOR, STATUS )
               CALL DAT_TYPE( LOCOR, TYPE, STATUS )
               CALL DAT_SHAPE( LOCOR, 1, DIMOR, NDIMOR, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Report an error if the ORIGIN component is not of type '_INTEGER'.
                  IF ( TYPE .NE. '_INTEGER' ) THEN
                     STATUS = ARY__TYPIN
                     CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                     CALL MSG_SETC( 'BADTYPE', TYPE )
                     CALL ERR_REP( 'ARY1_DBND_OTYP',
     :               'The ORIGIN component in the array structure ' //
     :               '^ARRAY has an invalid HDS type of ' //
     :               '''^BADTYPE''; its type should be ''_INTEGER''.',
     :               STATUS )

*  Report an error if it is not 1-dimensional.
                  ELSE IF ( NDIMOR .NE. 1 ) THEN
                     STATUS = ARY__NDMIN
                     CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                     CALL MSG_SETI( 'NDIM', NDIMOR )
                     CALL ERR_REP( 'ARY1_DBND_ONDI',
     :               'The ORIGIN component in the array structure ' //
     :               '^ARRAY has an invalid number of dimensions ' //
     :               '(^NDIM); this component should be 1-dimensional.',
     :               STATUS )

*  Report an error if it has the wrong number of elements.
                  ELSE IF ( DIMOR( 1 ) .NE. NDIMD ) THEN
                     STATUS = ARY__DIMIN
                     CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                     CALL MSG_SETI( 'DIM', DIMOR( 1 ) )
                     CALL MSG_SETI( 'NDIMD', NDIMD )
                     CALL ERR_REP( 'ARY1_DBND_ODIM',
     :               'The ORIGIN component in the array structure ' //
     :               '^ARRAY has an invalid number of elements ' //
     :               '(^DIM); this number should match the ' //
     :               'dimensionality of the array''s DATA component ' //
     :               '(^NDIMD).', STATUS )
                  END IF

*  Obtain the ORIGIN values.
                  CALL DAT_GET1I( LOCOR, ARY__MXDIM, ORIG, DIMOR( 1 ),
     :            STATUS )
               END IF

*  Annul the locator to the ORIGIN component.
               CALL DAT_ANNUL( LOCOR, STATUS )
               LOCOR = ARY__NOLOC
            END IF
            IF ( STATUS .NE. SAI__OK ) GO TO 9999

*  Enter the dimensionality and bounds information into the DCB, padding
*  the bounds with 1's if necessary.
            DCB_NDIM( IDCB ) = NDIMD
            DO 5 I = 1, NDIMD
               DCB_LBND( I , IDCB ) = ORIG( I )
               DCB_UBND( I , IDCB ) = ORIG( I ) + DIMD( I ) - 1
5           CONTINUE
            DO 6 I = NDIMD + 1, ARY__MXDIM
               DCB_LBND( I, IDCB ) = 1
               DCB_UBND( I, IDCB ) = 1
6           CONTINUE

*  If the form information in the DCB was not valid, then report an
*  error.
         ELSE
            STATUS = ARY__FATIN
            CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB ) )
            CALL ERR_REP( 'ARY1_DBND_FORM',
     :      'Unsupported array form ''^BADFORM'' found in Data ' //
     :      'Control Block (internal programming error).', STATUS )
         END IF

*  Note if the bounds information is now known.
9999     CONTINUE
         DCB_KBND( IDCB ) = STATUS .EQ. SAI__OK
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DBND', STATUS )

      END
