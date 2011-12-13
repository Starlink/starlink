      SUBROUTINE MAG_OPEN(TAPE, ACMODE, TP, STATUS)
*+
*  Name:
*     MAG_OPEN

*  Purpose:
*     Access tape device.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG_OPEN(DEVICE, MODE, TD, STATUS)

*  Description:
*     A tape descriptor is returned for the specified device.
*     The device can be a Logical Name pointing at the actual
*     VMS device.
*     If no channel has been assigned for this device, then this
*     is done.

*  Arguments:
*     DEVICE=CHARACTER*(*) (Given)
*        Expression specifying the name of a Tape Device .
*     MODE=CHARACTER*(*) (Given)
*        Expression specifying the access mode: 'READ', 'WRITE' or
*        'UPDATE' (read and write).
*     TD=INTEGER (Returned)
*        A variable to contain the tape descriptor.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.

*  Algorithm:
*     A new tape descriptor is obtained (if one is available), and the
*     tape drive is made ready for use.

*  Copyright:
*     Copyright (C) 1983, 1988, 1991, 1993 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     Sid Wright (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     15-Jul-1983:  Original.  (UCL::SLW)
*     04-Nov-1988:  Correct MODE documentation - remove UPDATE. (RAL::AJC)
*     14-Nov-1991:  Changed to new-style prologue (RAL::KFH)
*           Replaced tabs in end-of-line comments (RAL::KFH)
*           Replaced fac_$name by fac1_name (RAL::KFH)
*           Inserted implicit none (RAL::KFH)
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*      1-OCT-1996: Restore UPDATE mode (RAL::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definition:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MAG_SYS'         ! MAG Internal Constants
      INCLUDE 'MAG_ERR'         ! MAG Error codes

*  Arguments Given:
      CHARACTER*(*) TAPE        ! Physical or Logical device name
      CHARACTER*(*) ACMODE      ! Access mode

*  Arguments Returned:
      INTEGER TP                ! tape descriptor
*    Status return :
      INTEGER STATUS            ! status return

*  External References:
      EXTERNAL MAG1_BLK          ! Block data subprogram that
                                 ! initializes MAGINT
*  Global Variables:
      INCLUDE 'MAGIO_CMN'       ! MAG library states

*  Local Variables:
      INTEGER TD                ! Physical tape descriptor
      INTEGER I                 ! Loop index

*.


*    Allowed to execute ?
      IF ( STATUS.NE.SAI__OK ) RETURN

*    Initialised ?
      IF ( .NOT.MAGINT ) CALL MAG1_INIT(STATUS)

*    Get a free tape descriptor
      TP = 0
      DO 100 I = 1, MAG__MXDEV
         IF ( TFREE(I) ) THEN
            TP = I
            GO TO 1
         END IF
 100  CONTINUE
      STATUS = MAG__TOOTD
      CALL MAG1_ERR(STATUS)
      RETURN

 1    CONTINUE
*    Attempt to access tape drive
      CALL MIO_OPEN(TAPE, ACMODE, TD, STATUS)
      IF ( STATUS.EQ.SAI__OK ) THEN
         TTD(TP) = TD
         TSTART(TP) = .TRUE.
!        Tfile(tp) = MAG__UNDEF
         TFILE(TP) = 0
!        Tblock(tp) = MAG__UNDEF
         TBLOCK(TP) = 0
         TMOD(TP) = .FALSE.
         TNAME(TP) = TAPE
         TFREE(TP) = .FALSE.
      ELSE
         TFREE(TP) = .TRUE.
         CALL MAG1_ERNAM(TAPE, STATUS)
      END IF

      RETURN
      END
