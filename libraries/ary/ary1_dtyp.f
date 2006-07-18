      SUBROUTINE ARY1_DTYP( IDCB, STATUS )
*+
*  Name:
*     ARY1_DTYP

*  Purpose:
*     Ensure that type information and data component locators are
*     available for a data object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DTYP( IDCB, STATUS )

*  Description:
*     The routine ensures that type information is available for an
*     array (including whether it is complex or not) together with
*     locators to the data components(s) present.  It does nothing if
*     this information is already available in the DCB. Otherwise, it
*     determines the type by inspecting the data object itself and
*     enters the resulting information and locators into the DCB.  Only
*     those checks necessary for determining and validating the type
*     are performed on the data object.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the data object entry in the DCB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  If the required information is already present in the DCB, do
*     nothing.
*     -  Otherwise, ensure that form information is available.
*     -  Handle each supported array form.
*     -  For primitive arrays, obtain a non-imaginary component locator
*     by cloning the data object locator. Set a null value for the
*     imaginary component locator.
*     -  Obtain the data type of the non-imaginary component for
*     storage in the DCB and note that the array is not complex.
*     -  If the type is not numeric, then report an error.
*     -  If there was an error, then annul any locators.
*     -  For simple arrays, find the non-imaginary array component and
*     its type.
*     -  Check the type is numeric and report an error if it is not.
*     -  See if an imaginary component exists. If so, obtain its type.
*     -  Check that the imaginary component has the same data type as
*     the non-imaginary component and report an error if it does not.
*     -  Note whether the array is complex or not in the DCB.
*     -  Note whether type information is now available in the DCB.

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
*     David S Berry (JAC):
*     {enter_new_authors_here}

*  History:
*     9-JUN-1989  (RFWS):
*        Original version.
*     16-AUG-1989 (RFWS):
*        Changed initialisation of locators to use global constant.
*     23-AUG-1989 (RFWS):
*        Tidied the code a little.
*     13-FEB-1990 (RFWS):
*        Installed support for primitive arrays.
*     1-MAR-1990 (RFWS):
*        Removed declaration of un-referenced function.
*     8-MAY-2006 (DSB):
*        Installed support for scaled arrays.
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
*        DCB_CPX( ARY__MXDCB ) = LOGICAL (Write)
*           Whether the array is complex.
*        DCB_DLOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to the non-imaginary array component.
*        DCB_ILOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to the imaginary array component.
*        DCB_KTYP( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Whether the DCB type information is up to date.
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to data object.
*        DCB_TYP( ARY__MXDCB ) = CHARACTER * ( DAT__SZTYP ) (Write)
*           The array data type.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL ARY1_DEFR          ! Array creation deferred?

*  Local variables:
      CHARACTER * ( DAT__SZTYP ) TYPE ! HDS data type
      LOGICAL NUMER              ! Whether the data type is numeric

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If type information is not available, then inspect the data object.
      IF ( .NOT. DCB_KTYP( IDCB ) ) THEN

*  Report an error if the creation of the data arrays has been deferred.
*  The only situation in which creation is deferred is if ARY1_DCRE(P) i
*  called with its DEFER parameter set TRUE. In this case, all the 
*  required information should already be available because ARY1_DCRE(P) 
*  will have set it up.
         IF( ARY1_DEFR( IDCB, STATUS ) ) THEN
            STATUS = ARY__UNDEF
            CALL ERR_REP( 'ARY1_DTYP_ERR1', 'ARY1_DTYP: Cannot get '//
     :                    'type information because the creation of '//
     :                    'the supplied array has been deferred '//
     :                    '(ARY programming error).', STATUS )
         END IF

*  Ensure that form information is available.
         CALL ARY1_DFRM( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Primitive arrays.
*  ================
            IF ( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) THEN

*  Obtain a non-imaginary component locator by cloning the data object
*  locator. Set a null value for the imaginary component locator.
               DCB_DLOC( IDCB ) = ARY__NOLOC
               CALL DAT_CLONE( DCB_LOC( IDCB ), DCB_DLOC( IDCB ),
     :                         STATUS )
               DCB_ILOC( IDCB ) = ARY__NOLOC

*  Obtain the data type of the non-imaginary component for storage in
*  the DCB and note that the array is not complex.
               CALL DAT_TYPE( DCB_DLOC( IDCB ), DCB_TYP( IDCB ),
     :                        STATUS )
               DCB_CPX( IDCB ) = .FALSE.

*  If the type is not numeric, then report an error.
               CALL ARY1_INTYP( DCB_TYP( IDCB ), NUMER, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( .NOT. NUMER ) THEN
                     STATUS = ARY__TYPIN
                     CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                     CALL MSG_SETC( 'BADTYPE', DCB_TYP( IDCB ) )
                     CALL ERR_REP( 'ARY1_DTYP_PDTYP',
     :               'The array ^ARRAY has an invalid data type of ' //
     :               '''^BADTYPE''; it should have a numeric type.',
     :               STATUS )
                  END IF
               END IF

*  If there was an error, then annul any locators.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL DAT_ANNUL( DCB_DLOC( IDCB ), STATUS )
                  DCB_DLOC( IDCB ) = ARY__NOLOC
               END IF

*  Simple and scaled arrays.
*  =========================
            ELSE IF ( DCB_FRM( IDCB ) .EQ. 'SIMPLE' .OR.
     :                DCB_FRM( IDCB ) .EQ. 'SCALED' ) THEN

*  Find the DATA component, storing a locator to it in the DCB, and
*  obtain its type, which is also stored in the DCB.
               DCB_DLOC( IDCB ) = ARY__NOLOC
               DCB_ILOC( IDCB ) = ARY__NOLOC
               CALL DAT_FIND( DCB_LOC( IDCB ), 'DATA', DCB_DLOC( IDCB ),
     :                        STATUS )
               CALL DAT_TYPE( DCB_DLOC( IDCB ), DCB_TYP( IDCB ),
     :                        STATUS )

*  If the type is not numeric, then report an error.
               CALL ARY1_INTYP( DCB_TYP( IDCB ), NUMER, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( .NOT. NUMER ) THEN
                     STATUS = ARY__TYPIN
                     CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
                     CALL MSG_SETC( 'BADTYPE', DCB_TYP( IDCB ) )
                     CALL ERR_REP( 'ARY1_DTYP_DTYP',
     :               'The DATA component in the array structure ' //
     :               '^ARRAY has an invalid HDS type of ' //
     :               '''^BADTYPE''; it should have a numeric type.',
     :               STATUS )
                  ELSE

*  See if there is an IMAGINARY_DATA component present. If so, then the
*  array is complex, so record this fact in the DCB.
                     CALL DAT_THERE( DCB_LOC( IDCB ), 'IMAGINARY_DATA',
     :                               DCB_CPX( IDCB ), STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Is so, report an error if we are creating a scaled array.
                        IF ( DCB_CPX( IDCB ) ) THEN
                           IF( DCB_FRM( IDCB ) .EQ. 'SCALED' ) THEN
                              STATUS = ARY__USFRM
                              CALL ERR_REP( 'ARY1_DSTP_SCMX', 
     :                                 'Complex scaled arrays are '//
     :                                 'currently unsupported by the '//
     :                                 'ARY library.', STATUS )
                           END IF 

*  Otherwise, get a locator to it for the DCB and obtain its type.
                           CALL DAT_FIND( DCB_LOC( IDCB ),
     :                                    'IMAGINARY_DATA',
     :                                    DCB_ILOC( IDCB ), STATUS )
                           CALL DAT_TYPE( DCB_ILOC( IDCB ), TYPE,
     :                                    STATUS )
                           IF ( STATUS .EQ. SAI__OK ) THEN

*  If the type does not match that of the non-imaginary component, then
*  report an error.
                              IF ( TYPE .NE. DCB_TYP( IDCB ) ) THEN
                                 STATUS = ARY__TYPIN
                                 CALL DAT_MSG( 'ARRAY',
     :                                         DCB_LOC( IDCB ) )
                                 CALL MSG_SETC( 'BADTYPE', TYPE )
                                 CALL MSG_SETC( 'DTYPE',
     :                                          DCB_TYP( IDCB ) )
                                 CALL ERR_REP( 'ARY1_DTYP_IMAG',
     :                           'The IMAGINARY_DATA component in ' //
     :                           'the array structure ^ARRAY has an ' //
     :                           'invalid HDS type of ''^BADTYPE''; ' //
     :                           'its type should match that of the ' //
     :                           'DATA component (''^DTYPE'').',
     :                           STATUS )
                              END IF
                           END IF
                        END IF
                     END IF
                  END IF
               END IF

*  If there was an error, then annul any locators.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL DAT_ANNUL( DCB_DLOC( IDCB ), STATUS )
                  DCB_DLOC( IDCB ) = ARY__NOLOC
                  CALL DAT_ANNUL( DCB_ILOC( IDCB ), STATUS )
                  DCB_ILOC( IDCB ) = ARY__NOLOC
               END IF

*  If the form entry in the DCB is not supported, then report an error.
            ELSE
               STATUS = ARY__FATIN
               CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB ) )
               CALL ERR_REP( 'ARY1_DTYP_FRM',
     :         'Unsupported array form ''^BADFORM'' encountered in ' //
     :         'Data Control Block (internal programming error).',
     :         STATUS )
            END IF

*  Note if type information is now available in the DCB.
            DCB_KTYP( IDCB ) = STATUS .EQ. SAI__OK
         END IF
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DTYP', STATUS )

      END
