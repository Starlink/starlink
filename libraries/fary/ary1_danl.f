      SUBROUTINE ARY1_DANL( DISPOS, IDCB, STATUS )
*+
*  Name:
*     ARY1_DANL

*  Purpose:
*     Perform an "annul" operation on a data object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DANL( DISPOS, IDCB, STATUS )

*  Description:
*     The routine performs an "annul" operation on a DCB entry and
*     optionally disposes of the associated data object. This operation
*     is normally required when an ACB entry is annulled.  The
*     reference count for the data object is decremented and if this is
*     still non-zero, then no further action is taken. However, if the
*     reference count reaches zero, and the DISPOS argument is set to
*     .TRUE., then all locators contained in the DCB entry are annulled
*     (thereby removing any reference to the data object) and the DCB
*     entry is released. If the DISPOS argument is set to .TRUE., the
*     data object will also be disposed of according to the disposal
*     mode specified in the DCB (it is either kept or deleted). If the
*     reference count reaches zero and the DISPOS argument is .FALSE.,
*     then the DCB entry is released, but the data object is not
*     disposed of.

*  Arguments:
*     DISPOS = LOGICAL (Given)
*        Whether to dispose of the data object. A value of .FALSE.
*        indicates that the data object will remain in use by the ARY_
*        system; the intention being simply to release the specified
*        DCB entry.
*     IDCB = INTEGER (Given and Returned)
*        Index to the DCB entry to be anulled. If the data object
*        reference count falls to zero, then the DCB entry will be
*        released and a value of zero will be returned for this
*        argument (if the DISPOS argument is set to .TRUE., the data
*        object will also be disposed of). Otherwise this argument will
*        be unchanged on exit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The routine attempts to execute even if STATUS is set on entry,
*     although no further error report will be made if it subsequently
*     fails under these circumstances.
*     -  An error will be reported and STATUS set if a call to this
*     routine results in an attempt to dispose of a data object whose
*     disposal mode is 'KEEP', but whose values are undefined when the
*     ARY_ system has UPDATE access to it (and could therefore have
*     written values to it); the disposal will nevertheless succeed. No
*     such error will be reported if the DISPOS argument is set to
*     .FALSE. or if UPDATE access is not available.

*  Algorithm:
*     -  Save the error context on entry.
*     -  Decrement the reference count for the data object.
*     -  If the reference count falls to zero, then ensure that form
*     information is available in the DCB.
*     -  If the array is to be disposed of with a disposal mode of
*     'KEEP', then ensure that access mode and state information is
*     available in the DCB.
*     -  Handle the release of components for each form of array
*     separately.
*     -  If the array is primitive and a data component locator has
*     been acquired, then annul it.
*     -  If the array is simple and data component locators have been
*     acquired, then annul them.
*     -  If the data object is to be disposed of with a disposal mode
*     of 'KEEP', then check that it is in the "defined" state. If not,
*     and UPDATE access is available, then assign the array name to a
*     message token for use in constructing a later error message.
*     -  Release the array by annulling the main locator to it.
*     -  If an array in an "undefined" state is being disposed of with
*     a disposal mode of 'KEEP' and UPDATE access is available, then
*     report an error.
*     -  If the object is being disposed of with a disposal mode other
*     than 'KEEP', then annul it as if it were a temporary object,
*     causing it to be erased.
*     -  If the data object is not being disposed of, then simply annul
*     the DCB locator to it.
*     -  Clear the data object file and path name entries from the DCB.
*     -  Release the DCB slot associated with the object and reset the
*     IDCB argument to zero.
*     -  Restore the error context.

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
*     31-JUL-1989 (RFWS):
*        Original version.
*     13-SEP-1989 (RFWS):
*        Changed so that an error report is made if an array which is
*        to be kept is released from the ARY_ system in an undefined
*        state.
*     14-SEP-1989 (RFWS):
*        Added code to clear the data object file and path name entries
*        from the DCB.
*     14-SEP-1989 (RFWS):
*        Added the DISPOS argument and code to support it.
*     18-SEP-1989 (RFWS):
*        Made minor improvement to error message.
*     12-FEB-1990 (RFWS):
*        Installed support for primitive arrays.
*     16-MAR-1990 (RFWS):
*        Changed so that an error is reported on release of an undefined
*        array only if UPDATE access is available.
*     26-APR-2006 (DSB):
*        Add support for scaled arrays.
*     17-JUL-2006 (DSB):
*        Watch out for deferred arrays that have no data.
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
*           Whether the data object is complex.
*        DCB_DLOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Non-imaginary data component locator.
*        DCB_DSP( ARY__MXDCB ) = CHARACTER * ( ARY__SZDSP ) (Read)
*           Data object disposal mode.
*        DCB_FILE( ARY_MXDCB ) = CHARACTER * ( ARY__SZFIL ) (Write)
*           Data object container file name.
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           Form of data object.
*        DCB_ILOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Imaginary data component locator.
*        DCB_KTYP( ARY__MXDCB ) = LOGICAL (Read)
*           Whether data type information and component locators are
*           available for the data object in the DCB.
*        DCB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Main locator to data object.
*        DCB_MOD( NDF__MXDCB ) = CHARACTER * ( ARY__SZMOD ) (Read)
*           Data object access mode.
*        DCB_PATH( ARY__MXDCB ) = CHARACTER * ( ARY__SZPTH ) (Write)
*           Data object path name.
*        DCB_REFCT( ARY__MXDCB ) = INTEGER (Read and Write)
*           Data object reference count.
*        DCB_STA( ARY__MXDCB ) = LOGICAL (Read)
*           Data object state (defined/undefined).

*  Arguments Given:
      LOGICAL DISPOS

*  Arguments Given and Returned:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER TSTAT              ! Temporary status variable

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  Decrement the data object reference count.
      STATUS = SAI__OK
      DCB_REFCT( IDCB ) = DCB_REFCT( IDCB ) - 1

*  If the reference count falls to zero, then the DCB entry must be
*  released.
      IF ( DCB_REFCT( IDCB ) .LE. 0 ) THEN

*  Ensure that form information is available in the DCB.
         CALL ARY1_DFRM( IDCB, STATUS )

*  If the data object is to be disposed of and has a disposal mode of
*  'KEEP', then ensure that access mode and state information (i.e.
*  whether its data values are defined or undefined) is available in
*  the DCB.
         IF ( DISPOS .AND. ( DCB_DSP( IDCB ) .EQ. 'KEEP' ) ) THEN
            CALL ARY1_DMOD( IDCB, STATUS )
            CALL ARY1_DSTA( IDCB, STATUS )
         END IF
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Primitive arrays.
*  ================
            IF ( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) THEN

*  If a data component locator has been acquired for the DCB, then
*  annul it.
               IF ( DCB_KTYP( IDCB ) .AND.
     :              DCB_DLOC( IDCB ) .NE. ARY__NOLOC ) THEN
                  CALL DAT_ANNUL( DCB_DLOC( IDCB ), STATUS )
                  DCB_DLOC( IDCB ) = ARY__NOLOC
               END IF

*  Simple and scaled arrays.
*  =========================
            ELSE IF ( DCB_FRM( IDCB ) .EQ. 'SIMPLE' .OR.
     :                DCB_FRM( IDCB ) .EQ. 'SCALED' ) THEN

*  If data component locators have been acquired for the DCB, then annul
*  the non-imaginary component locator.
               IF ( DCB_KTYP( IDCB ) .AND.
     :              DCB_DLOC( IDCB ) .NE. ARY__NOLOC ) THEN
                  CALL DAT_ANNUL( DCB_DLOC( IDCB ), STATUS )
                  DCB_DLOC( IDCB ) = ARY__NOLOC

*  If it exists, then annul the imaginary component locator as well.
                  IF ( DCB_CPX( IDCB ) ) THEN
                     CALL DAT_ANNUL( DCB_ILOC( IDCB ), STATUS )
                     DCB_ILOC( IDCB ) = ARY__NOLOC
                  END IF
               END IF

*  Annul any object holding scale information.
               IF( DCB_KSCL( IDCB ) .AND.
     :             DCB_SCLOC( IDCB ) .NE. DAT__NOLOC ) THEN
                  CALL DAT_ANNUL( DCB_SCLOC( IDCB ), STATUS )
               END IF

*  If the DCB form information was not recognised, then report an error.
            ELSE
               STATUS = ARY__FATIN
               CALL MSG_SETC( 'BADFORM', DCB_FRM( IDCB ) )
               CALL ERR_REP( 'ARY1_DANL_FORM',
     :         'Unsupported array form ''^BADFORM'' found in Data ' //
     :         'Control Block (internal programming error).', STATUS )
            END IF

*  If the data object is to be disposed of and has a disposal mode of
*  'KEEP', then check that it is in a "defined" state. If not, and
*  UPDATE access to the data object is available, then an error will be
*  reported after the array has been released, so assign the array name
*  to a message token for use in constructing the error message.
            IF ( DISPOS .AND. ( DCB_DSP( IDCB ) .EQ. 'KEEP' ) ) THEN
               IF ( ( .NOT. DCB_STA( IDCB ) ) .AND.
     :              ( DCB_MOD( IDCB ) .EQ. 'UPDATE' ) ) THEN
                  CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
               END IF

*  Release the array by annulling the main locator to it.
               CALL DAT_ANNUL( DCB_LOC( IDCB ), STATUS )
               DCB_LOC( IDCB ) = ARY__NOLOC

*  If an array is in an undefined state and UPDATE access is available,
*  then report an error, as this probably indicates a programming error.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( ( .NOT. DCB_STA( IDCB ) ) .AND.
     :                 ( DCB_MOD( IDCB ) .EQ. 'UPDATE' ) ) THEN
                     STATUS = ARY__UNDEF
                     CALL ERR_REP( 'ARY1_DANL_UNDEF',
     :               'The array ^ARRAY has been released from the ' //
     :               'ARY_ system in an undefined state (possible ' //
     :               'programming error).',
     :               STATUS )
                  END IF
               END IF

*  If the array is being disposed of with a disposal mode other than
*  'KEEP, then annul its locator as if it were a temporary object (it
*  may be a temporary object, or it may be an a non-temporary object
*  which is to be deleted). This will cause it to be erased. In this
*  case, it does not matter if the data values are undefined.
            ELSE IF ( DISPOS ) THEN
               CALL ARY1_ANTMP( DCB_LOC( IDCB ), STATUS )

*  If the array is not to be disposed of, then simply annul the DCB
*  locator to it.
            ELSE
               CALL DAT_ANNUL( DCB_LOC( IDCB ), STATUS )
               DCB_LOC( IDCB ) = ARY__NOLOC
            END IF

*  Clear the data object file and path name entries from the DCB.
            DCB_FILE( IDCB ) = ' '
            DCB_PATH( IDCB ) = ' '

*  Release the DCB slot associated with the data object and reset the
*  IDCB argument to zero.
            CALL ARY1_RLS( ARY__DCB, IDCB, STATUS )
            IDCB = 0
         END IF
      END IF

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT

*  Call error tracing routine if appropriate.
         ELSE
            CALL ARY1_TRACE( 'ARY1_DANL', STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Release error stack.
      CALL ERR_RLSE

      END
