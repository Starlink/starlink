      SUBROUTINE ARY1_DCREP( TYPE, NDIM, UBND, TEMP, LOC, IDCB, STATUS )
*+
*  Name:
*     ARY1_DCREP

*  Purpose:
*     Create a primitive array with an entry in the DCB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DCREP( TYPE, NDIM, UBND, TEMP, LOC, IDCB, STATUS )

*  Description:
*     The routine converts an array placeholder object into a primitive
*     array and creates a new entry in the DCB to refer to it. The
*     placeholder object is passed by an HDS locator, which may be
*     annulled afterwards.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        Data type of the array to be created; an HDS primitive numeric
*        data type string (case insensitive).
*     NDIM = INTEGER (Given)
*        Number of array dimensions.
*     UBND( NDIM ) = INTEGER (Given)
*        Upper bounds of the array (the lower bounds are taken to be 1).
*     TEMP = LOGICAL (Given)
*        Whether the array is temporary (this is used to set its
*        disposal mode entry in the DCB).
*     LOC = CHARACTER * ( * ) (Given and Returned)
*        Locator to an array placeholder object (an empty scalar data
*        structure of type ARRAY); it should not be a cell or a slice.
*        A new locator is returned, since the original structure will
*        be erased and a new primitive array created in its place.
*     IDCB = INTEGER (Returned)
*        Index to the DCB entry which refers to the new data object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Obtain a free slot in the DCB. Return a DCB index of zero if
*     no slot could be found.
*     -  Obtain a locator to the placeholder object's parent structure.
*     -  Obtain the placeholder object's name. Then annul its locator
*     and erase the object.
*     -  Create a new primitive array of the required type and shape in
*     its place and obtain a new locator to it.
*     -  Annul the parent structure locator.
*     -  Clone a locator to the array for storage in the DCB. Link this
*     locator into a private group to prevent external events annulling
*     it.
*     -  Obtain the new data object file and path names and enter them
*     into the DCB.
*     -  Obtain a non-imaginary component locator by cloning the data
*     object locator and set a null imaginary component locator.
*     -  If there was an error, then clean up by annulling all the
*     locators which may have been acquired. Release the allocated DCB
*     slot.
*     -  If there was no error, then initialise the DCB entry for the
*     array.
*     -  Set the disposal mode according to whether it is a temporary
*     object or not.
*     -  Set the bad pixel flag to .TRUE..
*     -  Store the data type (and complexity) information in upper
*     case.
*     -  Store the number of dimensions and the array bounds
*     information, padding with 1's if necessary.
*     -  Initialise the accumulated pixel shifts to zero.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     12-FEB-1990 (RFWS):
*        Original, derived from the ARY1_DCRE routine.
*     12-MAR-1990 (RFWS):
*        Changed the placeholder type to ARRAY.
*     10-OCT-1990 (RFWS):
*        Changed to call ARY1_PAREN as a temporary work around for
*        problems with DAT_PAREN.
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
      INCLUDE 'ARY_CONST'        ! ARY_ private constant

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_BAD( ARY__MXDCB ) = LOGICAL (Write)
*           Data object bad pixel flag.
*        DCB_CPX( ARY__MXDCB ) = LOGICAL (Write)
*           Whether data object is a complex array.
*        DCB_DLOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to non-imaginary data component.
*        DCB_DSP( ARY__MXDCB ) = CHARACTER * ( ARY__SZDSP ) (Write)
*           Data object disposal mode.
*        DCB_FILE( ARY__MXDCB ) = CHARACTER * ( ARY__SZFIL ) (Write)
*           Data object container file name.
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Write)
*           Form of data object.
*        DCB_ILOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to imaginary data component.
*        DCB_INIT( ARY__MXDCB ) = LOGICAL (Write)
*           Whether the data object's values have been initialised.
*        DCB_KBAD( ARY__MXDCB ) = LOGICAL (Write)
*           Whether bad pixel flag information is available for the data
*           object.
*        DCB_KBND( ARY__MXDCB ) = LOGICAL (Write)
*           Whether bounds information is available for the data object.
*        DCB_KFRM( ARY__MXDCB ) = LOGICAL (Write)
*           Whether form information is available for the data object.
*        DCB_KMOD( ARY__MXDCB ) = LOGICAL (Write)
*           Whether access mode information is available for the data
*           object.
*        DCB_KSTA( ARY__MXDCB ) = LOGICAL (Write)
*           Whether state information is available for the data object.
*        DCB_KTYP( ARY__MXDCB ) = LOGICAL (Write)
*           Whether data type information and component locators are
*           available for the data object.
*        DCB_LBND( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Write)
*           Lower bounds of data object.
*        DCB_LOC( ARY_MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to data object.
*        DCB_NDIM( ARY__MXDCB ) = INTEGER (Write)
*           Number of data object dimensions.
*        DCB_MOD( ARY__MXDCB ) = CHARACTER * ( ARY__SZMOD ) (Write)
*           Data object access mode.
*        DCB_NREAD( ARY__MXDCB ) = INTEGER (Write)
*           Number of current mapped accesses which read the data
*           object.
*        DCB_NWRIT( ARY__MXDCB ) = INTEGER (Write)
*           Number of current mapped accesses which write to the data
*           object.
*        DCB_PATH( ARY__MXDCB ) = CHARACTER * ( ARY__SZPTH ) (Write)
*           Data object path name.
*        DCB_REFCT( ARY__MXDCB ) = INTEGER (Write)
*           Number of ACB entries which refer to the data object.
*        DCB_SFT( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Write)
*           Accumulated pixel shifts for the data object.
*        DCB_STA( ARY__MXDCB ) = LOGICAL (Write)
*           Data object state (defined/undefined).
*        DCB_TYP( ARY__MXDCB ) = CHARACTER * ( ARY__SZTYP ) (Write)
*           Data object numeric data type.
*        DCB_UBND( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Write)
*           Upper bounds of data object.

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER NDIM
      INTEGER UBND( NDIM )
      LOGICAL TEMP

*  Arguments Given and Returned:
      CHARACTER * ( * ) LOC

*  Arguments Returned:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCP ! Parent structure locator
      CHARACTER * ( DAT__SZNAM ) NAME ! Object name
      INTEGER I                  ! Loop counter for dimensions
      INTEGER NLEV               ! Levels in HDS path name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain a free slot in the DCB. Return a DCB index of zero if no slot
*  could be found.
      CALL ARY1_FFS( ARY__DCB, IDCB, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IDCB = 0

*  Obtain a locator to the placeholder object's parent structure.
      ELSE
         LOCP = ARY__NOLOC
         CALL DAT_PAREN( LOC, LOCP, STATUS )

*  Obtain the placeholder object's name. Then annul its locator and
*  erase the object.
         CALL DAT_NAME( LOC, NAME, STATUS )
         CALL DAT_ANNUL( LOC, STATUS )
         LOC = ARY__NOLOC
         CALL DAT_ERASE( LOCP, NAME, STATUS )
         
*  Create a new primitive array of the required type and shape in its
*  place and obtain a new locator to it.
         CALL DAT_NEW( LOCP, NAME, TYPE, NDIM, UBND, STATUS ) 
         CALL DAT_FIND( LOCP, NAME, LOC, STATUS )

*  Annul the parent structure locator.
         CALL DAT_ANNUL( LOCP, STATUS )
         LOCP = ARY__NOLOC

*  Clone a locator to the array for storage in the DCB. Link this
*  locator into a private group to prevent external events annulling
*  it.
         DCB_LOC( IDCB ) = ARY__NOLOC
         CALL DAT_CLONE( LOC, DCB_LOC( IDCB ), STATUS )
         CALL HDS_LINK( DCB_LOC( IDCB ), 'ARY_DCB', STATUS )

*  Obtain the new data object file and path names and enter them into
*  the DCB.
         CALL HDS_TRACE( DCB_LOC( IDCB ), NLEV, DCB_PATH( IDCB ),
     :                   DCB_FILE( IDCB ), STATUS )

*  Obtain a non-imaginary component locator by cloning the data object
*  locator and set a null imaginary component locator.
         DCB_DLOC( IDCB ) = ARY__NOLOC
         CALL DAT_CLONE( DCB_LOC( IDCB ), DCB_DLOC( IDCB ), STATUS )
         DCB_ILOC( IDCB ) = ARY__NOLOC

*  If there was an error, then clean up by annulling all the locators
*  which may have been acquired.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL DAT_ANNUL( DCB_LOC( IDCB ), STATUS )
            DCB_LOC( IDCB ) = ARY__NOLOC
            CALL DAT_ANNUL( DCB_DLOC( IDCB ), STATUS )
            DCB_DLOC( IDCB ) = ARY__NOLOC

*  Release the allocated DCB slot.
            CALL ARY1_RLS( ARY__DCB, IDCB, STATUS )
            IDCB = 0

*  If there was no error, then initialise the DCB entry for the array.
         ELSE
            DCB_REFCT( IDCB ) = 0
            DCB_NREAD( IDCB ) = 0
            DCB_NWRIT( IDCB ) = 0

*  The form is known to be PRIMITIVE, the access mode to be UPDATE and
*  the state to be "undefined", since the array has just been created.
            DCB_FRM( IDCB ) = 'PRIMITIVE'
            DCB_KFRM( IDCB ) = .TRUE.
            DCB_MOD( IDCB ) = 'UPDATE'
            DCB_KMOD( IDCB ) = .TRUE.
            DCB_STA( IDCB ) = .FALSE.
            DCB_INIT( IDCB ) = .FALSE.
            DCB_KSTA( IDCB ) = .TRUE.

*  Set the disposal mode according to whether it is a temporary object
*  or not.
            IF ( TEMP ) THEN
               DCB_DSP( IDCB ) = 'TEMP'
            ELSE
               DCB_DSP( IDCB ) = 'KEEP'
            END IF

*  Set the bad pixel flag to .TRUE..
            DCB_BAD( IDCB ) = .TRUE.
            DCB_KBAD( IDCB ) = .TRUE.

*  Store the data type (and complexity) information in upper case.
            DCB_TYP( IDCB ) = TYPE
            CALL CHR_UCASE( DCB_TYP( IDCB ) )
            DCB_CPX( IDCB ) = .FALSE.
            DCB_KTYP( IDCB ) = .TRUE.

*  Store the number of dimensions and the array bounds information,
*  padding with 1's if necessary.
            DCB_NDIM( IDCB ) = NDIM
            DO 2 I = 1, NDIM
               DCB_LBND( I, IDCB ) = 1
               DCB_UBND( I, IDCB ) = UBND( I )
2           CONTINUE
            DO 3 I = NDIM + 1, ARY__MXDIM
               DCB_LBND( I, IDCB ) = 1
               DCB_UBND( I, IDCB ) = 1
3           CONTINUE
            DCB_KBND( IDCB ) = .TRUE.

*  Initialise the accumulated pixel shifts to zero.
            DO 4 I = 1, ARY__MXDIM
               DCB_SFT( I, IDCB ) = 0
4           CONTINUE
         END IF
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DCREP', STATUS )

      END
