      SUBROUTINE ARY1_DFRM( IDCB, STATUS )
*+
*  Name:
*     ARY1_DFRM

*  Purpose:
*     Ensure that form information is available for a data object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DFRM( IDCB, STATUS )

*  Description:
*     The routine ensures that form information is available for an
*     array. It does nothing if this information is already present in
*     the DCB. Otherwise, it determines the form by inspecting the data
*     object itself and enters the resulting information into the DCB.
*     Only those checks necessary for determining and validating the
*     form are performed on the data object.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the data object entry in the DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  If the required information is already present in the DCB, do
*     nothing.
*     -  Otherwise, determine if the data object is primitive. If so,
*     then the form is 'PRIMITIVE'.
*     -  If the object is non-primitive, then check it is of type
*     'ARRAY' and is scalar. Report an error if it is not.
*     -  See if a VARIANT component exists. If not, then supply a
*     default value of 'SIMPLE'.
*     -  If it exists, check that the VARIANT component is of type
*     _CHAR and is scalar. Report an error if it is not.
*     -  Obtain the value of the VARIANT component.
*     -  Classify the variant value to derive the form, reporting an
*     error if the value is not recognised.
*     -  Note if form information is now available in the DCB.

*  Copyright:
*     Copyright (C) 1989, 1992 Science & Engineering Research Council.
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
*     DSB: David S. Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     9-JUN-1989  (RFWS):
*        Original version.
*     16-AUG-1989 (RFWS):
*        Changed initialisation of locators to use global constant.
*     18-AUG-1989 (RFWS):
*        Re-structured IF blocks which were producing wrong results for
*        simple arrays. Also made minor corrections to the error
*        messages.
*     23-AUG-1989 (RFWS):
*        Re-structured again to access the VARIANT component by mapping,
*        to allow long values to be handled properly. Also added a check
*        that the ARRAY structure is scalar.
*     14-JAN-1992 (RFWS):
*        Added handling of character string length for the mapped
*        variance value to ensure UNIX compatibility.
*     17-JUL-2006 (DSB):
*        Include "PRIMITIVE" as a valid Variant value (this variant is
*        an internal flag used to indicate that the ARRAY structure will 
*        be replaced at some point by a primitive array).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNV_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Write)
*           Form of the array structure.
*        DCB_KFRM( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Whether the DCB form information is up to date.
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to data object.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local variables:
      CHARACTER * ( DAT__SZLOC ) LOCVAR ! Locator to VARIANT component
      CHARACTER * ( DAT__SZTYP ) TYPE ! HDS type string
      INTEGER DIM( DAT__MXDIM )  ! HDS dimension array
      INTEGER DUMMY( 1 )         ! Dummy dimension array
      INTEGER LENV               ! Length of mapped variance value
      INTEGER NDIM               ! Number of HDS dimensions
      INTEGER PNTR               ! Pointer to mapped VARIANT value
      LOGICAL PRIM               ! Whether data object is primitive
      LOGICAL THERE              ! Whether a data component is there

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the form information is unknown, then inspect the data object.
      IF ( .NOT. DCB_KFRM( IDCB ) ) THEN

*  See if the object is primitive. If so, then the form is 'PRIMITIVE'.
         CALL DAT_PRIM( DCB_LOC( IDCB ), PRIM, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( PRIM ) THEN
               DCB_FRM( IDCB ) = 'PRIMITIVE'

*  If it is not primitive, then obtain its type and shape.
            ELSE
               CALL DAT_TYPE( DCB_LOC( IDCB ), TYPE, STATUS )
               CALL DAT_SHAPE( DCB_LOC( IDCB ), DAT__MXDIM, DIM, NDIM,
     :                         STATUS )

*  Check its type is 'ARRAY'. If not, then report an error.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( TYPE .NE. 'ARRAY' ) THEN
                     STATUS = ARY__TYPIN
                     CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                     CALL MSG_SETC( 'BADTYPE', TYPE )
                     CALL ERR_REP( 'ARY1_DFRM_TYPE',
     :               'The array structure ^ARRAY has an invalid ' //
     :               'data type of ''^BADTYPE''.', STATUS )

*  Check it is a scalar. Report an error if it is not.
                  ELSE IF ( NDIM .NE. 0 ) THEN
                     STATUS = ARY__NDMIN
                     CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                     CALL MSG_SETI( 'BADNDIM', NDIM )
                     CALL ERR_REP( 'ARY1_DFRM_NDMA',
     :               'The array structure ^ARRAY is ' //
     :               '^BADNDIM-dimensional; it should be a ' //
     :               'scalar.', STATUS )

*  If the structure is OK, then see if a VARIANT component is present,
*  supplying a default form of 'SIMPLE' if not.
                  ELSE
                     CALL DAT_THERE( DCB_LOC( IDCB ), 'VARIANT', THERE,
     :                               STATUS ) 
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        IF ( .NOT. THERE ) THEN
                           DCB_FRM( IDCB ) = 'SIMPLE'
                        ELSE

*  Obtain a locator to the VARIANT component and obtain its type and
*  shape.
                           LOCVAR = ARY__NOLOC
                           CALL DAT_FIND( DCB_LOC( IDCB ), 'VARIANT',
     :                                    LOCVAR, STATUS )
                           CALL DAT_TYPE( LOCVAR, TYPE, STATUS )
                           CALL DAT_SHAPE( LOCVAR, DAT__MXDIM, DIM,
     :                                     NDIM, STATUS )

*  Check that the VARIANT is a character object and report an error if
*  it is not.
                           IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                              STATUS = ARY__TYPIN
                              CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                              CALL MSG_SETC( 'BADTYPE', TYPE )
                              CALL ERR_REP( 'ARY1_DFRM_VTYP',
     :                        'The VARIANT component in the array ' //
     :                        'structure ^ARRAY has an invalid HDS ' //
     :                        'type of ''^BADTYPE''; it should be ' //
     :                        'of type ''_CHAR''.', STATUS )

*  Check that it is scalar and report an error if it is not.
                           ELSE IF ( NDIM .NE. 0 ) THEN
                              STATUS = ARY__NDMIN
                              CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                              CALL MSG_SETI( 'BADNDIM', NDIM )
                              CALL ERR_REP( 'ARY1_DFRM_NDMV',
     :                        'The VARIANT component in the array ' //
     :                        'structure ^ARRAY is ' //
     :                        '^BADNDIM-dimensional; it should be a ' //
     :                        'scalar.', STATUS )

*  If the VARIANT component is OK, then map it and obtain its mapped
*  length.
                           ELSE
                              CALL DAT_MAPC( LOCVAR, 'READ', 0, DUMMY,
     :                                       PNTR, STATUS )
                              CALL DAT_CLEN( LOCVAR, LENV, STATUS )

*  Classify the VARIANT value to obtain the form information.

*  ...simple array.
                              IF ( STATUS .EQ. SAI__OK ) THEN
                                 IF ( CHR_SIMLR(
     :                                %VAL( CNF_PVAL( PNTR ) ),
     :                                'SIMPLE',
     :                                %VAL( CNF_CVAL( LENV ) ) ) ) 
     :                           THEN
                                    DCB_FRM( IDCB ) = 'SIMPLE'

*  ...scaled array.
                                 ELSE IF ( CHR_SIMLR(
     :                                     %VAL( CNF_PVAL( PNTR ) ),
     :                                     'SCALED',
     :                                     %VAL( CNF_CVAL( LENV ) ) ) ) 
     :                           THEN
                                    DCB_FRM( IDCB ) = 'SCALED'

*  ...spaced array.
                                 ELSE IF ( CHR_SIMLR(
     :                                     %VAL( CNF_PVAL( PNTR ) ),
     :                                     'SPACED',
     :                                     %VAL( CNF_CVAL( LENV ) ) ) ) 
     :                           THEN
                                    DCB_FRM( IDCB ) = 'SPACED'

*  ...sparse array.
                                 ELSE IF ( CHR_SIMLR(
     :                                     %VAL( CNF_PVAL( PNTR ) ),
     :                                     'SPARSE',
     :                                     %VAL( CNF_CVAL( LENV ) ) ) ) 
     :                           THEN
                                    DCB_FRM( IDCB ) = 'SPARSE'

*  ...polynomial array.
                                 ELSE IF ( CHR_SIMLR(
     :                                     %VAL( CNF_PVAL( PNTR ) ),
     :                                     'POLYNOMIAL',
     :                                     %VAL( CNF_CVAL( LENV ) ) ) ) 
     :                           THEN
                                    DCB_FRM( IDCB ) = 'POLYNOMIAL'

*  ...defered primitive array.
                                 ELSE IF ( CHR_SIMLR(
     :                                     %VAL( CNF_PVAL( PNTR ) ),
     :                                     'PRIMITIVE',
     :                                     %VAL( CNF_CVAL( LENV ) ) ) ) 
     :                           THEN
                                    DCB_FRM( IDCB ) = 'PRIMITIVE'

*  If the VARIANT value is not recognised, then report an error.
                                 ELSE
                                    STATUS = ARY__VARIN
                                    CALL DAT_MSG( 'ARRAY',
     :                                            DCB_LOC( IDCB ) )
                                    CALL ARY1_SETC(
     :                                         %VAL( CNF_PVAL( PNTR ) ),
     :                                              'BADVARIANT',
     :                                        %VAL( CNF_CVAL( LENV ) ) )
                                    CALL ERR_REP( 'ARY1_DFRM_VRNT',
     :                              'The VARIANT component in the ' //
     :                              'array structure ^ARRAY has an ' //
     :                              'invalid value of ''^BADVARIANT''.',
     :                              STATUS ) 
                                 END IF
                              END IF
                           END IF

*  Annul the locator to the VARIANCE component (this also unmaps it if
*  necessary).
                           CALL DAT_ANNUL( LOCVAR, STATUS )
                           LOCVAR = ARY__NOLOC
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF

*  Note if form information is now available in the DCB.
         DCB_KFRM( IDCB ) = STATUS .EQ. SAI__OK
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DFRM', STATUS )

      END
