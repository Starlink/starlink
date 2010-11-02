      SUBROUTINE ARY1_DIMP( LOC, IDCB, STATUS )
*+
*  Name:
*     ARY1_DIMP

*  Purpose:
*     Import a data object, creating a new DCB entry for it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_DIMP( LOC, IDCB, STATUS )

*  Description:
*     The routine imports an array structure into the ARY_ system,
*     creating a new DCB entry for it.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        HDS locator to the array structure to be imported.
*     IDCB = INTEGER (Returned)
*        Index to the new DCB entry for the data object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The routine makes a "cloned" copy of the HDS locator supplied;
*     the latter may later be annulled without affecting the operation
*     of the ARY_ system.
*     -  If STATUS is set on entry, then the routine will return a
*     value of zero for the IDCB argument, although no further
*     processing will occur.
*     -  A value of zero will also be returned for the IDCB argument if
*     the routine fails.

*  Algorithm:
*     -  Set an initial value of zero for the IDCB argument before
*     checking the inherited status.
*     -  Clone the locator supplied and obtain an index to a free slot
*     in the DCB.
*     -  Store the data locator in the DCB and initialise the DCB entry.
*     -  Obtain form information for the array.
*     -  If the array has a supported form, then obtain the data object
*     file and path names and enter them into the DCB.
*     -  If the array form is not supported, then report an error.
*     -  If there was an error, then annul the cloned locator, free the
*     allocated slot in the DCB and set a value of zero for the IDCB
*     argument.

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
*     1-AUG-1989 (RFWS):
*        Original version.
*     23-AUG-1989 (RFWS):
*        Changed "unsupported form" error message to include the name
*        of the array.
*     14-SEP-1989 (RFWS):
*        Added code to obtain the data object file and path names and
*        enter them into the DCB.
*     14-SEP-1989 (RFWS):
*        Added call to HDS_LINK to break locator association with any
*        externally-defined groups.
*     12-FEB-1990 (RFWS):
*        Added support for primitive arrays.
*     26-APR-2006 (DSB):
*        Added support for scaled arrays.
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
*        DCB_DSP( ARY__MXDCB ) = CHARACTER * ( ARY__SZDSP ) (Write)
*           Data object disposal mode.
*        DCB_FILE( ARY__MXDCB ) = CHARACTER * ( ARY__SZFIL ) (Write)
*           Data object container file name.
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           Form of data object.
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
*        DCB_LOC( ARY_MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to data object.
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

*  Arguments Given:
      CHARACTER * ( * ) LOC

*  Arguments Returned:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC1 ! Cloned locator
      INTEGER I                  ! Loop counter for dimensions
      INTEGER NLEV               ! Levels in HDS path name

*.

*  Set an initial value of zero for the IDCB argument.
      IDCB = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Clone the locator supplied and link the cloned locator into a
*  private group (to prevent any external events from annulling it
*  without the ARY_ system's knowledge). Obtain an index to a free slot
*  in the DCB.
      LOC1 = ARY__NOLOC
      CALL DAT_CLONE( LOC, LOC1, STATUS )
      CALL HDS_LINK( LOC1, 'ARY_DCB', STATUS )
      CALL ARY1_FFS( ARY__DCB, IDCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Store the data object locator in the DCB and initialise the DCB flags
*  to indicate that no information about the object is yet available.
         DCB_LOC( IDCB ) = LOC1
         DCB_KFRM( IDCB ) = .FALSE.
         DCB_KTYP( IDCB ) = .FALSE.
         DCB_KBND( IDCB ) = .FALSE.
         DCB_KMOD( IDCB ) = .FALSE.
         DCB_KSTA( IDCB ) = .FALSE.
         DCB_KBAD( IDCB ) = .FALSE.
         DCB_KSCL( IDCB ) = .FALSE.

*  Initialise the reference and mapping counts and set the disposal
*  mode to 'KEEP', indicating that this is not a temporary object.
         DCB_REFCT( IDCB ) = 0
         DCB_NREAD( IDCB ) = 0
         DCB_NWRIT( IDCB ) = 0
         DCB_DSP( IDCB ) = 'KEEP'

*  Initialise the accumulated pixel shifts to zero.
         DO 1 I = 1, ARY__MXDIM
            DCB_SFT( I, IDCB ) = 0
1        CONTINUE

*  Obtain form information for the array, which is written into the DCB.
         CALL ARY1_DFRM( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If the array has a supported form, then obtain the data object file
*  and path names and enter them into the DCB.
            IF ( ( DCB_FRM( IDCB ) .EQ. 'PRIMITIVE' ) .OR.
     :           ( DCB_FRM( IDCB ) .EQ. 'SCALED' ) .OR.
     :           ( DCB_FRM( IDCB ) .EQ. 'DELTA' ) .OR.
     :           ( DCB_FRM( IDCB ) .EQ. 'SIMPLE' ) ) THEN
               CALL HDS_TRACE( DCB_LOC( IDCB ), NLEV, DCB_PATH( IDCB ),
     :                         DCB_FILE( IDCB ), STATUS )

*  If the array form is not one of those supported, then report an
*  error.
            ELSE
               STATUS = ARY__USFRM
               CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
               CALL MSG_SETC( 'USFORM', DCB_FRM( IDCB ) )
               CALL ERR_REP( 'ARY1_DIMP_USF',
     :         'Sorry, the array structure ^ARRAY is of ' //
     :         '''^USFORM'' form; this is not currently supported ' //
     :         'by the ARY_ routines.', STATUS )
            END IF
         END IF
      END IF

*  If there was an error, then annul the cloned locator and release the
*  slot allocated in the DCB. Reset the IDCB argument to zero.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( LOC1, STATUS )
         LOC1 = ARY__NOLOC
         CALL ARY1_RLS( ARY__DCB, IDCB, STATUS )
         IDCB = 0
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_DIMP', STATUS )

      END
