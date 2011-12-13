      SUBROUTINE MAG_CLOSE(TP, STATUS)
*+
*  Name:
*     MAG_CLOSE

*  Purpose:
*     Close a tape device given a tape descriptor.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG_CLOSE(TP, STATUS)

*  Description:
*     The device specified by the tape descriptor is closed, and
*     forgotten by the MAG library.

*  Arguments:
*     TP=INTEGER (Given)
*        A variable containing the tape descriptor.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If the routine fails to
*        complete, this variable will be set to an appropriate error number.
*        If this variable is not SAI__OK on input, then the routine will
*        still attempt to execute, but will return with STATUS set to the
*        import value.

*  Algorithm:
*     Shut down the specified tape drive and release common block data.

*  Copyright:
*     Copyright (C) 1983, 1991, 1993 Science & Engineering Research Council.
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
*     11-Jul-1983:  Original.  (UCL::SLW)
*     14-Nov-1991:  Changed to new-style prologue (RAL::KFH)
*           Replaced tabs in end-of-line comments (RAL::KFH)
*           Replaced fac_$name by fac1_name (RAL::KFH)
*           Inserted implicit none (RAL::KFH)
*    22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definition:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MAG_SYS'         ! MAG Internal Constants
      INCLUDE 'MAG_ERR'         ! MAG Errors

*  Arguments Given:
      INTEGER TP                ! tape descriptor
*    Status return :
      INTEGER STATUS            ! status return

*  Global Variables:
      INCLUDE 'MAGIO_CMN'       ! MAG library states

*  External References:
      EXTERNAL MAG1_BLK          ! Block data subprogram that
                                 ! initializes MAGINT
*  Local Variables:
      INTEGER ISTAT             ! local status
      INTEGER TD                ! tape descriptor to physical device

*.


      ISTAT = STATUS
      STATUS = SAI__OK
      CALL MAG1_GETTD(TP, TD, STATUS)
      IF ( STATUS.EQ.SAI__OK ) THEN
         CALL MIO_CLOSE(TD, STATUS)
         IF ( STATUS.NE.SAI__OK ) CALL MAG1_ERRTP(TP, STATUS)
         TFREE(TP) = .TRUE.
         TTD(TP) = 0
         TSTART(TP) = .TRUE.
!        Tfile(tp) = MAG__UNDEF
         TFILE(TP) = 0
!        Tblock(tp) = MAG__UNDEF
         TBLOCK(TP) = 0
         TMOD(TP) = .FALSE.
         TNAME(TP) = ' '
      END IF

      IF ( ISTAT.NE.SAI__OK ) STATUS = ISTAT

      RETURN
      END
