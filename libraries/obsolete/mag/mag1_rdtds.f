      SUBROUTINE MAG1_RDTDS(ELOC, DEVICE, FILE, START, BLOCK, STATUS)
*+
*  Name:
*     MAG1_RDTDS

*  Purpose:
*     read MAG dataset contents.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG1_RDTDS(ELOC, DEVICE, FILE, START, BLOCK, STATUS)

*  Description:
*     The contents of the specified tape device data structure are read into
*     local storage.

*  Arguments:
*     ELOC=CHARACTER*(*) (Given)
*        Contains the locator to the tape device data structure
*     DEVICE=CHARACTER*(*) (Returned)
*        Will contain the tape device name
*     FILE=INTEGER (Returned)
*        Will contain the current file number
*     START=LOGICAL (Returned)
*        Will contain the current file-start flag
*     BLOCK=INTEGER (Returned)
*        Will contain the current block number relative to file-start flag
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.

*  Algorithm:
*     The contents of an MT dataset are read.
*     The FILE, POSITION and BLOCK variables are set undefined if they have
*     no values in the dataset.

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
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
*     Jack Giddings    (UCL::JWG)
*     {enter_new_authors_here}

*  History:
*     Jack Giddings      01-MAY-81
*     K.F.Hartley        08-Nov-91  Remove commented out code
*     08-Nov-1991: (RAL::KFH)
*            Change to new style prologues
*            Change all fac_$name to fac1_name
*            Replace tabs in end-of-line comments
*            Remove /nolist in INCLUDE
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     4-FEB-1993 (PMA):
*        Add INCLUDE 'DAT_PAR'
*     {enter_further_changes_here}

*  Notes:
*     Formerly known as MAG_$RDTDS

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'MAG_ERR'         ! MAG Errors

*  Arguments Given:
      CHARACTER*(*) ELOC        ! locator to dataset env.

*  Arguments Returned:
      CHARACTER*(*) DEVICE      ! device name
      INTEGER FILE              ! current file number
      LOGICAL START             ! position in current file
      INTEGER BLOCK             ! block number
      INTEGER STATUS            ! status return

*  Local Variables:
      CHARACTER*(DAT__SZLOC) LOC   ! locator to tape dataset

*.


      DEVICE = ' '
      FILE = 0
      START = .TRUE.
      BLOCK = 0

*    DEVICE - device name string
      CALL DAT_FIND(ELOC, 'DEVICE', LOC, STATUS)
      CALL DAT_GET0C(LOC, DEVICE, STATUS)
      CALL DAT_ANNUL(LOC, STATUS)
      IF ( STATUS.NE.SAI__OK ) THEN
         STATUS = MAG__RDERR
         CALL MAG1_ERNAM(DEVICE, STATUS)
         RETURN
      END IF

*    FILE - current tape file number
      CALL DAT_FIND(ELOC, 'FILE', LOC, STATUS)
      CALL DAT_GET0I(LOC, FILE, STATUS)
      IF ( STATUS.NE.SAI__OK ) THEN
         FILE = 0
         CALL ERR_ANNUL(STATUS)
      END IF
      CALL DAT_ANNUL(LOC, STATUS)

*    START - if block number relative to start of file.
      CALL DAT_FIND(ELOC, 'START', LOC, STATUS)
      CALL DAT_GET0L(LOC, START, STATUS)
      IF ( STATUS.NE.SAI__OK ) THEN
         START = .TRUE.
         CALL ERR_ANNUL(STATUS)
      END IF
      CALL DAT_ANNUL(LOC, STATUS)

*    BLOCK - block number relative to position
      CALL DAT_FIND(ELOC, 'BLOCK', LOC, STATUS)
      CALL DAT_GET0I(LOC, BLOCK, STATUS)
      IF ( STATUS.NE.SAI__OK ) THEN
         BLOCK = 0
         CALL ERR_ANNUL(STATUS)
      END IF
      CALL DAT_ANNUL(LOC, STATUS)

      RETURN
      END
