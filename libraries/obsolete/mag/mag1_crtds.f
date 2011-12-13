      SUBROUTINE MAG1_CRTDS(ELOC, DNAME, DEVICE, STATUS)
*+
*  Name:
*     MAG1_CRTDS

*  Purpose:
*     create MAG dataset.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG1_CRTDS(ELOC, DNAME, DEVICE, STATUS)

*  Description:
*     This routine creates a new tape device data structure

*  Arguments:
*     ELOC=CHARACTER*(*) (Given)
*        Contains the locator to the current devices data structure
*     DNAME=CHARACTER*(*) (Given)
*        Contains the name of the tape device data structure
*     DEVICE=CHARACTER*(*) (Given)
*        Contains the name of the tape device
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.

*  Algorithm:
*     The contents of an MT dataset are created:-
*       The DEVICE variable is written.
*       The other components are left precisely undefined.

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
*     Jack Giddings  (UCL::JWG)
*     {enter_new_authors_here}

*  History:
*     Jack Giddings      01-MAY-81
*     A.Chipperfield     03-JUN-86 ADAM version - use DAT_CCTYP
*     K F Hartley        08-NOV-91 Remove commented out code
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
*     Formerly known as MAG_$CRTDS

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'MAG_SYS'         ! MAG Internal Constants
      INCLUDE 'MAG_ERR'         ! MAG Errors

*  Arguments Given:
      CHARACTER*(*) ELOC        ! locator to dataset env.
      CHARACTER*(*) DNAME       ! dataset name
      CHARACTER*(*) DEVICE      ! device name

*  Arguments Returned:
      INTEGER STATUS            ! status return

*  Local Variables:
      CHARACTER*(DAT__SZLOC) LOC   ! locator to tape dataset
      CHARACTER*(DAT__SZLOC) TLOC  ! temporary locator
      CHARACTER*(DAT__SZTYP) TYPE

*.


*    Is it already there ?
      CALL DAT_FIND(ELOC, DNAME, LOC, STATUS)
      IF ( STATUS.NE.SAI__OK ) THEN
         CALL ERR_ANNUL(STATUS)
*       Create Mag. Tape dataset
         CALL DAT_NEW0(ELOC, DNAME, 'tapedrive', STATUS)
         CALL DAT_FIND(ELOC, DNAME, LOC, STATUS)
      END IF

*    Create components :

*    DEVICE - device name string
      CALL DAT_FIND(LOC, 'DEVICE', TLOC, STATUS)
      IF ( STATUS.NE.SAI__OK ) THEN
         CALL ERR_ANNUL(STATUS)
         CALL DAT_CCTYP(MAG__SZNAM, TYPE)
         CALL DAT_NEW0(LOC, 'DEVICE', TYPE, STATUS)
         CALL DAT_FIND(LOC, 'DEVICE', TLOC, STATUS)
      END IF
      CALL DAT_PUT0C(TLOC, DEVICE, STATUS)
      CALL DAT_ANNUL(TLOC, STATUS)

*    FILE - current tape file number
      CALL DAT_FIND(LOC, 'FILE', TLOC, STATUS)
      IF ( STATUS.NE.SAI__OK ) THEN
         CALL ERR_ANNUL(STATUS)
         CALL DAT_NEW0(LOC, 'FILE', '_INTEGER', STATUS)
         CALL DAT_FIND(LOC, 'FILE', TLOC, STATUS)
      ELSE
         CALL DAT_RESET(TLOC, STATUS)
      END IF
      CALL DAT_ANNUL(TLOC, STATUS)

*    START - if block number relative to start of file.
      CALL DAT_FIND(LOC, 'START', TLOC, STATUS)
      IF ( STATUS.NE.SAI__OK ) THEN
         CALL ERR_ANNUL(STATUS)
         CALL DAT_NEW0(LOC, 'START', '_LOGICAL', STATUS)
         CALL DAT_FIND(LOC, 'START', TLOC, STATUS)
      ELSE
         CALL DAT_RESET(TLOC, STATUS)
      END IF
      CALL DAT_ANNUL(TLOC, STATUS)

*    BLOCK - block number relative to position
      CALL DAT_FIND(LOC, 'BLOCK', TLOC, STATUS)
      IF ( STATUS.NE.SAI__OK ) THEN
         CALL ERR_ANNUL(STATUS)
         CALL DAT_NEW0(LOC, 'BLOCK', '_INTEGER', STATUS)
         CALL DAT_FIND(LOC, 'BLOCK', TLOC, STATUS)
      ELSE
         CALL DAT_RESET(TLOC, STATUS)
      END IF
      CALL DAT_ANNUL(TLOC, STATUS)

      CALL DAT_ANNUL(LOC, STATUS)

      IF ( STATUS.NE.SAI__OK ) THEN
         STATUS = MAG__CRERR
         CALL MAG1_ERNAM(DNAME, STATUS)
      END IF

      RETURN
      END
