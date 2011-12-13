      SUBROUTINE ARY1_DCPY( IDCB1, TEMP, LOC, IDCB2, STATUS )
*+
*  Name:
*     ARY1_DCPY

*  Purpose:
*     Copy a data object to a new (or temporary) HDS location and
*     create a DCB entry for it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DCPY( IDCB1, TEMP, LOC, IDCB2, STATUS )

*  Description:
*     The routine copies a data object, identified by its entry in the
*     DCB, to create a new array in place of an array placeholder
*     object and creates a new DCB entry to describe it. Only valid
*     data components are copied (rogue components are ignored).
*     Otherwise, the new object is identical to the original.

*  Arguments:
*     IDCB1 = INTEGER (Given)
*        Index to the DCB entry describing the data object to be copied.
*     TEMP = LOGICAL (Given)
*        Whether the new array is temporary (this is used to set its
*        disposal mode entry in the DCB).
*     LOC = CHARACTER * ( * ) (Given and Returned)
*        Locator to an array placeholder object (an empty scalar data
*        structure of type ARRAY). In the case of a primitive array,
*        the associated structure will be erased and replaced with a
*        new object; a new locator will therefore be returned and the
*        original placeholder object should not be a cell or a slice.
*     IDCB2 = INTEGER (Returned)
*        Index to the DCB entry which refers to the new data object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A value of zero will be returned for the IDCB2 argument if the
*     routine is called with STATUS set, although no further processing
*     will occur.
*     -  A value of zero will also be returned for the IDCB2 argument if
*     the routine fails for any reason.

*  Algorithm:
*     -  Set an initial value of zero for the IACB2 argument, before
*     checking the inherited status.
*     -  Obtain a new slot in the DCB. Reset the IACB2 argument if it
*     could not be obtained.
*     -  Ensure that form information is available in the DCB.
*     -  Handle each form of array separately.
*     -  For primitive arrays, ensure that data type and bounds
*     information are available in the DCB.
*     -  Obtain a locator to the placeholder object's parent structure.
*     -  Obtain the placeholder object's name. Then annul the object's
*     locator and erase the object.
*     -  Copy the input array into its place and obtain a locator to
*     the new object.
*     -  Annul the parent structure locator.
*     -  Clone a locator to the new array for storage in the DCB. Link
*     this locator into a private group to prevent external events
*     annulling it.
*     -  Obtain the data object file and path names and store these in
*     the DCB.
*     -  Obtain a non-imaginary data component locator by cloning the
*     data object locator and set a null imaginary component locator.
*     -  For simple arrays, ensure that type, bounds and bad pixel flag
*     information are available in the DCB.
*     -  Clone a locator to the new data object for storage in the DCB.
*     Link the locator into a private group.
*     -  Obtain the new data object file and path name information and
*     enter this into the DCB.
*     -  Copy the imaginary data component to the new array.
*     -  If the array is complex, then copy the non-imaginary component
*     also.
*     -  Copy the ORIGIN and BAD_PIXEL components if present.
*     -  If the form entry in the DCB was not recognised, then report an
*     error.
*     -  If an error occurred, then annul any locators which may have
*     been acquired, release the new DCB slot and reset the IACB2
*     argument to zero.
*     -  Initialise the new DCB entry, copying information from the
*     input array's DCB entry if appropriate.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     8-AUG-1989 (RFWS):
*        Original version.
*     31-AUG-1989 (RFWS):
*        Changed to avoid calling DAT_CCOPY, which has been omitted from
*        the HDS sharable image (by mistake?), so can't be used at
*        present.
*     14-SEP-1989 (RFWS):
*        Added code to obtain the new data object file and path name
*        information and enter this into the DCB.
*     18-SEP-1989 (RFWS):
*        Installed the DCB_INIT array and added code to support it.
*     9-OCT-1989 (RFWS):
*        Converted to use a placeholder object to indicate where the
*        new array should reside in the data system.
*     12-FEB-1990 (RFWS):
*        Added support for primitive arrays.
*     9-MAR-1990 (RFWS):
*        Added annulling of locators to cleanup code.
*     12-MAR-1990 (RFWS):
*        Changed the placeholder type to ARRAY.
*     10-OCT-1990 (RFWS):
*        Changed to call ARY1_PAREN as a temporary work around for
*        problems with DAT_PAREN.
*     26-APR-2006 (DSB):
*        Add support for scaled arrays.
*     17-JUL-2006 (DSB):
*        Ensure the data object arrays are available.
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
*        DCB_BAD( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Data object bad pixel flag.
*        DCB_CPX( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Whether data object is a complex array.
*        DCB_DLOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Locator to non-imaginary data component.
*        DCB_DSP( ARY__MXDCB ) = CHARACTER * ( ARY__SZDSP ) (Write)
*           Data object disposal mode.
*        DCB_FILE( ARY__MXDCB ) = CHARACTER * ( ARY__SZFIL ) (Write)
*           Data object container file name.
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read and
*        Write)
*           Form of data object.
*        DCB_ILOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Locator to imaginary data component.
*        DCB_INIT( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Whether the data object's values have been initialised.
*        DCB_KBAD( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Whether bad pixel flag information is available for the data
*           object.
*        DCB_KBND( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Whether bounds information is available for the data object.
*        DCB_KFRM( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Whether form information is available for the data object.
*        DCB_KMOD( ARY__MXDCB ) = LOGICAL (Write)
*           Whether access mode information is available for the data
*           object.
*        DCB_KSTA( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Whether state information is available for the data object.
*        DCB_KTYP( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Whether data type information and component locators are
*           available for the data object.
*        DCB_LBND( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read and Write)
*           Lower bounds of data object.
*        DCB_LOC( ARY_MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Locator to data object.
*        DCB_NDIM( ARY__MXDCB ) = INTEGER (Read and Write)
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
*        DCB_SFT( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read and Write)
*           Accumulated pixel shifts for the data object.
*        DCB_STA( ARY__MXDCB ) = LOGICAL (Read and Write)
*           Data object state (defined/undefined).
*        DCB_TYP( ARY__MXDCB ) = CHARACTER * ( ARY__SZTYP ) (Read and
*        Write)
*           Data object numeric data type.
*        DCB_UBND( ARY__MXDIM, ARY__MXDCB ) = INTEGER (Read and Write)
*           Upper bounds of data object.

*  Arguments Given:
      INTEGER IDCB1
      LOGICAL TEMP

*  Arguments Given and Returned:
      CHARACTER * ( * ) LOC

*  Arguments Returned:
      INTEGER IDCB2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCP ! Parent structure locator
      CHARACTER * ( DAT__SZNAM ) NAME ! Object name
      INTEGER I                  ! Loop counter for dimensions
      INTEGER NLEV               ! Levels in HDS path name

*.

*  Set an initial value of zero for the IDCB2 argument.
      IDCB2 = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain a new slot in the DCB, resetting the IDCB2 argument to zero if
*  none could be obtained.
      CALL ARY1_FFS( ARY__DCB, IDCB2, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IDCB2 = 0
      ELSE

*  Ensure the data object arrays are available.
         CALL ARY1_DOBJ( IDCB1, STATUS )

*  Ensure that form information is available in the DCB.
         CALL ARY1_DFRM( IDCB1, STATUS )

*  Handle each form of array in turn.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Primitive arrays.
*  ================
            IF ( DCB_FRM( IDCB1 ) .EQ. 'PRIMITIVE' ) THEN

*  Ensure that data type and bounds information are available in the
*  DCB.
               CALL ARY1_DTYP( IDCB1, STATUS )
               CALL ARY1_DBND( IDCB1, STATUS )

*  Obtain a locator to the placeholder object's parent structure.
               LOCP = ARY__NOLOC
               CALL DAT_PAREN( LOC, LOCP, STATUS )

*  Obtain the placeholder object's name. Then annul the object's
*  locator and erase the object.
               CALL DAT_NAME( LOC, NAME, STATUS )
               CALL DAT_ANNUL( LOC, STATUS )
               LOC = ARY__NOLOC
               CALL DAT_ERASE( LOCP, NAME, STATUS )

*  Copy the input array into its place and obtain a locator to the new
*  object.
               CALL DAT_COPY( DCB_DLOC( IDCB1 ), LOCP, NAME, STATUS )
               CALL DAT_FIND( LOCP, NAME, LOC, STATUS )

*  Annul the parent structure locator.
               CALL DAT_ANNUL( LOCP, STATUS )
               LOCP = ARY__NOLOC

*  Clone a locator to the new array for storage in the DCB. Link this
*  locator into a private group to prevent external events annulling
*  it.
               DCB_LOC( IDCB2 ) = ARY__NOLOC
               CALL DAT_CLONE( LOC, DCB_LOC( IDCB2 ), STATUS )
               CALL HDS_LINK( DCB_LOC( IDCB2 ), 'ARY_DCB', STATUS )

*  Obtain the data object file and path names and store these in the
*  DCB.
               CALL HDS_TRACE( DCB_LOC( IDCB2 ), NLEV,
     :                         DCB_PATH( IDCB2 ), DCB_FILE( IDCB2 ),
     :                         STATUS )

*  Obtain a non-imaginary data component locator by cloning the data
*  object locator and set a null imaginary component locator.
               DCB_DLOC( IDCB2 ) = ARY__NOLOC
               CALL DAT_CLONE( DCB_LOC( IDCB2 ), DCB_DLOC( IDCB2 ),
     :                         STATUS )
               DCB_ILOC( IDCB2 ) = ARY__NOLOC

*  There is no scaling information
               DCB_KSCL( IDCB2 ) = .FALSE.

*  Simple and scaled arrays.
*  =========================
            ELSE IF ( DCB_FRM( IDCB1 ) .EQ. 'SIMPLE' .OR.
     :                DCB_FRM( IDCB1 ) .EQ. 'SCALED' .OR.
     :                DCB_FRM( IDCB1 ) .EQ. 'DELTA' ) THEN

*  Ensure that data type, bounds and bad pixel flag information are
*  available in the DCB.
               CALL ARY1_DTYP( IDCB1, STATUS )
               CALL ARY1_DBND( IDCB1, STATUS )
               CALL ARY1_DBAD( IDCB1, STATUS )

*  Clone a locator to the new data object for storage in the DCB. Link
*  this locator into a private group to prevent external events
*  annulling it.
               DCB_LOC( IDCB2 ) = ARY__NOLOC
               CALL DAT_CLONE( LOC, DCB_LOC( IDCB2 ), STATUS )
               CALL HDS_LINK( DCB_LOC( IDCB2 ), 'ARY_DCB', STATUS )

*  Obtain the data object file and path names and store these in the
*  DCB.
               CALL HDS_TRACE( DCB_LOC( IDCB2 ), NLEV,
     :                         DCB_PATH( IDCB2 ), DCB_FILE( IDCB2 ),
     :                         STATUS )

*  Copy the non-imaginary data component to the new array and obtain a
*  locator to it.
               DCB_DLOC( IDCB2 ) = ARY__NOLOC
               CALL DAT_COPY( DCB_DLOC( IDCB1 ), DCB_LOC( IDCB2 ),
     :                        'DATA', STATUS )
               CALL DAT_FIND( DCB_LOC( IDCB2 ), 'DATA',
     :                        DCB_DLOC( IDCB2 ), STATUS )

*  If the input array is complex, then copy the imaginary component
*  also.
               IF ( DCB_CPX( IDCB1 ) ) THEN
                  DCB_ILOC( IDCB2 ) = ARY__NOLOC
                  CALL DAT_COPY( DCB_ILOC( IDCB1 ), DCB_LOC( IDCB2 ),
     :                           'IMAGINARY_DATA', STATUS )
                  CALL DAT_FIND( DCB_LOC( IDCB2 ), 'IMAGINARY_DATA',
     :                           DCB_ILOC( IDCB2 ), STATUS )
               END IF

*  Copy the BAD_PIXEL and ORIGIN components if they exist.
               CALL ARY1_CPYNC( DCB_LOC( IDCB1 ), 'BAD_PIXEL',
     :                          DCB_LOC( IDCB2 ), STATUS )
               CALL ARY1_CPYNC( DCB_LOC( IDCB1 ), 'ORIGIN',
     :                          DCB_LOC( IDCB2 ), STATUS )

*  Transfer the other components of a SCALED array.
               IF( DCB_FRM( IDCB1 ) .EQ. 'SCALED' ) THEN
                  CALL ARY1_CPSCL( IDCB1, IDCB2, STATUS )

*  Transfer the other components of a DELTA array.
               ELSE IF( DCB_FRM( IDCB1 ) .EQ. 'DELTA' ) THEN
                  CALL ARY1_CPDLT( IDCB1, IDCB2, STATUS )
               END IF

*  If the form information in the DCB was not recognised, then report an
*  error.
            ELSE
               STATUS = ARY__FATIN
               CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB1 ) )
               CALL ERR_REP( 'ARY1_DCPY_FORM',
     :         'Unsupported array form ''^BADFORM'' found in Data ' //
     :         'Control Block (internal programming error).', STATUS )
            END IF
         END IF

*  If there was an error, then annul any locators which may have been
*  acquired, release the new DCB slot and reset the IDCB2 argument to
*  zero.
         IF ( STATUS .NE. SAI__OK ) THEN
            IF ( IDCB2 .NE. 0 ) THEN
               CALL DAT_ANNUL( DCB_DLOC( IDCB2 ), STATUS )
               DCB_DLOC( IDCB2 ) = ARY__NOLOC
               CALL DAT_ANNUL( DCB_ILOC( IDCB2 ), STATUS )
               DCB_ILOC( IDCB2 ) = ARY__NOLOC
               CALL DAT_ANNUL( DCB_LOC( IDCB2 ), STATUS )
               DCB_LOC( IDCB2 ) = ARY__NOLOC
               CALL ARY1_RLS( ARY__DCB, IDCB2, STATUS )
               IDCB2 = 0
            END IF

*  Set the reference and mapping counts for the new data object to zero.
         ELSE
            DCB_REFCT( IDCB2 ) = 0
            DCB_NREAD( IDCB2 ) = 0
            DCB_NWRIT( IDCB2 ) = 0

*  Set the form, access mode and state information for the new data
*  object.
            DCB_FRM( IDCB2 ) = DCB_FRM( IDCB1 )
            DCB_KFRM( IDCB2 ) = DCB_KFRM( IDCB1 )
            DCB_MOD( IDCB2 ) = 'UPDATE'
            DCB_KMOD( IDCB2 ) = .TRUE.
            DCB_STA( IDCB2 ) = DCB_STA( IDCB1 )
            DCB_INIT( IDCB2 ) = DCB_INIT( IDCB1 )
            DCB_KSTA( IDCB2 ) = DCB_KSTA( IDCB1 )

*  Set the disposal mode.
            IF ( TEMP ) THEN
               DCB_DSP( IDCB2 ) = 'TEMP'
            ELSE
               DCB_DSP( IDCB2 ) = 'KEEP'
            END IF

*  Copy the bad pixel flag and data type information.
            DCB_BAD( IDCB2 ) = DCB_BAD( IDCB1 )
            DCB_KBAD( IDCB2 ) = DCB_KBAD( IDCB1 )
            DCB_TYP( IDCB2 ) = DCB_TYP( IDCB1 )
            DCB_CPX( IDCB2 ) = DCB_CPX( IDCB1 )
            DCB_KTYP( IDCB2 ) = DCB_KTYP( IDCB1 )

*  Copy the array bounds information.
            DCB_NDIM( IDCB2 ) = DCB_NDIM( IDCB1 )
            DO 1 I = 1, DCB_NDIM( IDCB2 )
               DCB_LBND( I, IDCB2 ) = DCB_LBND( I, IDCB1 )
               DCB_UBND( I, IDCB2 ) = DCB_UBND( I, IDCB1 )
1           CONTINUE
            DO 2 I = DCB_NDIM( IDCB2 ) + 1, ARY__MXDIM
               DCB_LBND( I, IDCB2 ) = 1
               DCB_UBND( I, IDCB2 ) = 1
2           CONTINUE
            DCB_KBND( IDCB2 ) = DCB_KBND( IDCB1 )

*  Copy the accumulated pixel shift information.
            DO 3 I = 1, ARY__MXDIM
               DCB_SFT( I, IDCB2 ) = DCB_SFT( I, IDCB1 )
3           CONTINUE
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DCPY', STATUS )

      END
