      SUBROUTINE ARY1_DSTP( TYPE, CMPLX, IDCB, DCE, STATUS )
*+
*  Name:
*     ARY1_DSTP

*  Purpose:
*     Change the type of a data object identified by its DCB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DSTP( TYPE, CMPLX, IDCB, DCE, STATUS )

*  Description:
*     The routine changes the data type of an object identified by its
*     entry in the DCB. If the object's state is "defined", then the
*     data values which it contains undergo type conversion. If it is
*     "undefined", then no conversion is necessary. The routine can
*     convert between any numeric data type and also between complex
*     and non-complex types (and vice versa).

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        New numeric data type for the object; this should be a
*        primitive numeric HDS type string (case insensitive).
*     CMPLX = LOGICAL (Given)
*        Whether the new object data type should be complex (i.e.
*        whether it should contain an imaginary component).
*     IDCB = INTEGER (Given)
*        Index to the data object entry in the DCB.
*     DCE = LOGICAL (Returned)
*        Whether conversion errors occurred during type conversion of
*        data values (if this happens the affected data are assigned
*        "bad" values). This can only happen if the object's state is
*        "defined".
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Ensure that form information is available in the DCB.
*     -  Handle each form of array in turn.
*     -  For primitive arrays, ensure that data type, object state and
*     bounds information is available in the DCB.
*     -  Obtain the dimension sizes of the array and the total number
*     of pixels.
*     -  If the new data type is not complex, then the array form can
*     be left as primitive and its numeric data type changed. This may
*     involve erasing the existing object and creating a new one, so
*     annul the non-imaginary component locator which will re
*     re-acquired later.
*     -  Obtain a locator to the array's parent structure and obtain
*     the name of the array.
*     -  Change the array's numeric data type, possibly obtaining a new
*     data object locator as a result.
*     -  Re-acquire the non-imaginary component locator by cloning the
*     data object locator.
*     -  If the new data type is complex, then the array must be
*     converted to simple storage form.
*     -  Report context information if the conversion failed.
*     -  Otherwise, change the numeric type of the non-imaginary
*     component in what is now a simple array.
*     -  Create a new imaginary component and obtain a locator to it
*     for storage in the DCB.
*     -  If the data object state is "defined", then map the new
*     imaginary component and fill it with zeros. Then unmap it.
*     -  For simple arrays, ensure that data type, bad pixel flag,
*     object state and bounds information is available in the DCB.
*     -  Calculate the dimension sizes of the array components and the
*     number of data elements in a component.
*     -  Change the numeric data type of the non-imaginary component.
*     -  If conversion is from a complex type to a complex type, then
*     change the numeric data type of the imaginary component.
*     -  If from a complex type to a non-complex type, then annul the
*     non-imaginary component locator and erase the component.
*     -  If from a non-complex type to a complex type, then create and
*     locate a new imaginary component. If the object's state is
*     "defined", then map this new component and fill it with zeros,
*     then unmap it.
*     -  Determine whether any data conversion errors occurred.
*     -  If the form entry in the DCB was not recognised, then report an
*     error.
*     -  Store the new data type information in the DCB.
*     -  Note whether this information is up to date.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     All Rights Reserved.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     8-AUG-1989 (RFWS):
*        Original version.
*     31-AUG-1989 (RFWS):
*        Minor correction to comments.
*     13-FEB-1990 (RFWS):
*        Installed support for primitive arrays.
*     24-SEP-1990 (RFWS):
*        Added annulling of the parent locator for simple arrays, which
*        had been omitted.
*     10-OCT-1990 (RFWS):
*        Changed to call ARY1_PAREN as a temporary work around for
*        problems with DAT_PAREN.
*     8-MAY-2006 (DSB):
*        Installed support for scaled arrays.
*     17-JUL-2006 (DSB):
*        Cater for arrays in which the creation of the data arrays has
*        been deferred.
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
*        DCB_BAD( ARY__MXDCB ) = LOGICAL (Read)
*           Data object bad pixel flag.
*        DCB_CPX( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Whether data object is complex.
*        DCB_DLOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Non-imaginary component locator.
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           Data object form.
*        DCB_ILOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Imaginary component locator.
*        DCB_KTYP( ARY__MXDCB ) = LOGICAL (Write)
*           Whether the DCB type (and complexity) information is up to
*           date.
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Data object locator.
*        DCB_NDIM( ARY__MXDCB ) = INTEGER (Read)
*           Number of data object dimensions.
*        DCB_STA( ARY__MXDCB ) = LOGICAL (Read)
*           Data object state.
*        DCB_TYP( ARY__MXDCB ) = CHARACTER * ( ARY__SZTYP ) (Read and
*        Write)
*           Data object numeric type.

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      LOGICAL CMPLX
      INTEGER IDCB

*  Arguments Returned:
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCC ! Component locator
      CHARACTER * ( DAT__SZLOC ) LOCP ! Parent structure locator
      CHARACTER * ( DAT__SZNAM ) NAME ! Object name
      INTEGER DIM( ARY__MXDIM )  ! Data component dimension sizes
      INTEGER EL                 ! Number of data elements in component
      INTEGER I                  ! Loop counter for dimensions
      INTEGER PNTR               ! Pointer to mapped component data
      LOGICAL IDCE               ! Imaginary data conversion error?
      LOGICAL DEFER              ! Has creation of HDS arrays been deferred?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that form information is available in the DCB.
      CALL ARY1_DFRM( IDCB, STATUS )

*  Handle each form of array in turn.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  See if the creation of the HDS arrays holding the array values is
*  being deferred until the array is mapped.
         DEFER = ( DCB_DLOC( IDCB ) .EQ. ARY__NOLOC )

*  Primitive arrays.
*  ================
         IF ( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) THEN

*  Ensure that data type, object state and bounds information is
*  available in the DCB.
            CALL ARY1_DTYP( IDCB, STATUS )
            CALL ARY1_DSTA( IDCB, STATUS )
            CALL ARY1_DBND( IDCB, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain the dimension sizes of the array and the total number of
*  pixels.
               EL = 1
               DO 1 I = 1, DCB_NDIM( IDCB )
                  DIM( I ) = DCB_UBND( I, IDCB )
                  EL = EL * DIM( I )
1              CONTINUE

*  If the new data type is not complex, then the array form can be left
*  as primitive and its numeric data type changed. This may involve
*  erasing the existing object and creating a new one, so annul the
*  non-imaginary component locator which will be re-acquired later.
               IF ( .NOT. CMPLX ) THEN
                  IF( .NOT. DEFER ) THEN 
                     CALL DAT_ANNUL( DCB_DLOC( IDCB ), STATUS )
                     DCB_DLOC( IDCB ) = ARY__NOLOC
                  END IF

*  Obtain a locator to the array's parent structure and obtain the name
*  of the array.
                  LOCP = ARY__NOLOC
                  CALL DAT_PAREN( DCB_LOC( IDCB ), LOCP, STATUS )
                  CALL DAT_NAME( DCB_LOC( IDCB ), NAME, STATUS )

*  Change the array's numeric data type, possibly obtaining a new data
*  object locator as a result.
                  CALL ARY1_RETYP( LOCP, NAME, DCB_TYP( IDCB ),
     :                             DCB_STA( IDCB ), .TRUE.,
     :                             DCB_NDIM( IDCB ), DIM, TYPE,
     :                             DEFER, DCB_LOC( IDCB ), DCE, STATUS )

*  Re-acquire the non-imaginary component locator by cloning the data object 
*  locator. We leave a null locator of the creation of the HDS array has been 
*  deferred, but we create a placeholder to indicate that a primitive
*  array should be created.
                  IF( .NOT. DEFER ) THEN  
                     CALL DAT_CLONE( DCB_LOC( IDCB ), DCB_DLOC( IDCB ),
     :                               STATUS )
                  ELSE
                     CALL ARY1_DFPPL( LOCP, NAME, DCB_LOC( IDCB ), 
     :                                STATUS )
                  END IF

*  Annul the parent locator.
                  CALL DAT_ANNUL( LOCP, STATUS )
                  LOCP = ARY__NOLOC

*  If the new data type is complex, then the array must be converted to
*  simple storage form.
               ELSE
                  CALL ARY1_DP2S( IDCB, STATUS )

*  Report context information if the conversion failed.
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_REP( 'ARY1_DSTP_CVT',
     :               'Unable to perform implicit conversion from ' //
     :               '''PRIMITIVE'' to ''SIMPLE'' array storage form.',
     :               STATUS )
                  ELSE

*  Otherwise, change the numeric type of the non-imaginary component in
*  what is now a simple array.
                     CALL ARY1_RETYP( DCB_LOC( IDCB ), 'DATA',
     :                               DCB_TYP( IDCB ), DCB_STA( IDCB ),
     :                               .TRUE., DCB_NDIM( IDCB ), DIM,
     :                               TYPE, DEFER, DCB_DLOC( IDCB ), DCE,
     :                               STATUS )

*  Create a new imaginary component and obtain a locator to it for
*  storage in the DCB.Only do this if the non-imaginary component is 
*  available (if not, the creation of the HDS arrays is being deferred).
                     DCB_ILOC( IDCB ) = ARY__NOLOC
                     IF( .NOT. DEFER ) THEN
                        CALL DAT_NEW( DCB_LOC( IDCB ), 'IMAGINARY_DATA',
     :                                TYPE, DCB_NDIM( IDCB ), DIM, 
     :                                STATUS )
                        CALL DAT_FIND( DCB_LOC( IDCB ), 
     :                                 'IMAGINARY_DATA',
     :                                 DCB_ILOC( IDCB ), STATUS )

*  If the data object state is "defined", then map the new imaginary
*  component and fill it with zeros. Then unmap it.
                        IF ( DCB_STA( IDCB ) ) THEN
                           CALL DAT_MAP( DCB_ILOC( IDCB ), TYPE, 
     :                                   'WRITE', DCB_NDIM( IDCB ), DIM,
     :                                   PNTR, STATUS )
                           CALL ARY1_VZERO( TYPE, EL, PNTR, STATUS )
                           CALL ARY1_HUNMP( DCB_ILOC( IDCB ), STATUS )
                        END IF
                     END IF
                  END IF
               END IF
            END IF

*  Simple arrays.
*  =============
         ELSE IF ( DCB_FRM( IDCB ) .EQ. 'SIMPLE' ) THEN

*  Ensure that data type, bad pixel flag, object state and bounds
*  information is available in the DCB.
            CALL ARY1_DTYP( IDCB, STATUS )
            CALL ARY1_DBAD( IDCB, STATUS )
            CALL ARY1_DSTA( IDCB, STATUS )
            CALL ARY1_DBND( IDCB, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Calculate the dimension sizes of the data object components and the
*  total number of their elements.
               EL = 1
               DO 2 I = 1, DCB_NDIM( IDCB )
                  DIM( I ) = DCB_UBND( I, IDCB ) - DCB_LBND( I, IDCB )
     :                       + 1
                  EL = EL * DIM( I )
2              CONTINUE

*  Change the numeric type of the non-imaginary component.
               CALL ARY1_RETYP( DCB_LOC( IDCB ), 'DATA',
     :                          DCB_TYP( IDCB ), DCB_STA( IDCB ),
     :                          DCB_BAD( IDCB ), DCB_NDIM( IDCB ), DIM,
     :                          TYPE, DEFER, DCB_DLOC( IDCB ), DCE, 
     :                          STATUS )
               IDCE = .FALSE.
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If a complex type is required and the type was originally complex,
*  then change the numeric type of the imaginary component.
                  IF ( CMPLX .AND. DCB_CPX( IDCB ) ) THEN
                     CALL ARY1_RETYP( DCB_LOC( IDCB ), 'IMAGINARY_DATA',
     :                               DCB_TYP( IDCB ), DCB_STA( IDCB ),
     :                               DCB_BAD( IDCB ), DCB_NDIM( IDCB ),
     :                               DIM, TYPE, DEFER, DCB_ILOC( IDCB ),
     :                               IDCE, STATUS )

*  If a non-complex type is required, but the type was originally
*  complex, then annul the imaginary component locator and erase the
*  component.
                  ELSE IF ( ( .NOT. CMPLX ) .AND. DCB_CPX( IDCB ) ) THEN
                     IF( .NOT. DEFER ) THEN
                        CALL DAT_ANNUL( DCB_ILOC( IDCB ), STATUS )
                        DCB_ILOC( IDCB ) = ARY__NOLOC
                        CALL DAT_ERASE( DCB_LOC( IDCB ), 
     :                                  'IMAGINARY_DATA', STATUS )
                     END IF

*  If a complex type is required but the type was originally
*  non-complex, then create and locate a new imaginary component (unless
*  the creation of the HDS arrays has been deferred).
                  ELSE IF ( CMPLX .AND. ( .NOT. DCB_CPX( IDCB ) ) ) THEN
                     IF( .NOT. DEFER ) THEN
                        CALL DAT_NEW( DCB_LOC( IDCB ), 'IMAGINARY_DATA',
     :                                TYPE, DCB_NDIM( IDCB ), DIM, 
     :                                STATUS )
                        DCB_ILOC( IDCB ) = ARY__NOLOC
                        CALL DAT_FIND( DCB_LOC( IDCB ), 
     :                                 'IMAGINARY_DATA',
     :                                 DCB_ILOC( IDCB ), STATUS )

*  If the data object state is "defined", then map the new imaginary
*  component and fill it with zeros. Then unmap it.
                        IF ( DCB_STA( IDCB ) ) THEN
                           CALL DAT_MAP( DCB_ILOC( IDCB ), TYPE, 
     :                                   'WRITE', DCB_NDIM( IDCB ), DIM,
     :                                   PNTR, STATUS )
                           CALL ARY1_VZERO( TYPE, EL, PNTR, STATUS )
                           CALL ARY1_HUNMP( DCB_ILOC( IDCB ), STATUS )
                        END IF
                     END IF
                  END IF

*  Determine whether any data conversion errors occurred.
                  DCE = DCE .OR. IDCE
               END IF
            END IF

*  Scaled arrays.
*  =============
         ELSE IF ( DCB_FRM( IDCB ) .EQ. 'SCALED' ) THEN

*  Ensure that scaling and data type information is available in the DCB.
            CALL ARY1_DSCL( IDCB, STATUS )
            CALL ARY1_DTYP( IDCB, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Change the external data type. This is the data type of the scale and
*  zero terms (the data type of the arrays of scaled values is left
*  unchanged).
               CALL DAT_FIND( DCB_SCLOC( IDCB ), 'SCALE', LOCC, STATUS )
               CALL DAT_RETYP( LOCC, TYPE, STATUS )
               CALL DAT_ANNUL( LOCC, STATUS )

               CALL DAT_FIND( DCB_SCLOC( IDCB ), 'ZERO', LOCC, STATUS ) 
               CALL DAT_RETYP( LOCC, TYPE, STATUS )
               CALL DAT_ANNUL( LOCC, STATUS )

               DCE = .FALSE.

*  If a non-complex type is required, but the type was originally
*  complex, then annul the imaginary component locator and erase the
*  component.
               IF ( ( .NOT. CMPLX ) .AND. DCB_CPX( IDCB ) ) THEN
                  IF( .NOT. DEFER ) THEN 
                     CALL DAT_ANNUL( DCB_ILOC( IDCB ), STATUS )
                     DCB_ILOC( IDCB ) = ARY__NOLOC
                     CALL DAT_ERASE( DCB_LOC( IDCB ), 'IMAGINARY_DATA',
     :                               STATUS )
                  END IF

*  If a complex type is required but the type was originally
*  non-complex, then report an error.
               ELSE IF ( CMPLX .AND. ( .NOT. DCB_CPX( IDCB ) ) ) THEN
                  STATUS = ARY__USFRM
                  CALL ERR_REP( 'ARY1_DSTP_SCMX', 'Complex scaled '//
     :                          'arrays are currently unsupported '//
     :                          'by the ARY library.', STATUS )
               END IF
            END IF

*  If the form entry in the DCB was not recognised, then report an
*  error.
         ELSE
            STATUS = ARY__FATIN
            CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB ) )
            CALL ERR_REP( 'ARY1_DSTP_FORM',
     :      'Unsupported array form ''^BADFORM'' found in Data ' //
     :      'Control Block (internal programming error).', STATUS )
         END IF

*  Store the new data type (in upper case) and complexity information
*  in the DCB.
         IF ( STATUS .EQ. SAI__OK ) THEN
            DCB_TYP( IDCB ) = TYPE
            CALL CHR_UCASE( DCB_TYP( IDCB ) )
            DCB_CPX( IDCB ) = CMPLX
         ENDIF

*  Note whether the information is up to date.
         DCB_KTYP( IDCB ) = STATUS .EQ. SAI__OK
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DSTP', STATUS )

      END
