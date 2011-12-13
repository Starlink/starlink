
      SUBROUTINE MAG_JEOV(TP, STATUS)
*+
*  Name:
*     MAG_JEOV

*  Purpose:
*     Jump over an EOV condition (2 consecutive tape marks).

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG_JEOV(TD, STATUS)

*  Description:
*     This jumps over an end-of-volume (EOV) condition, assuming that
*     the tape is currently positioned between its two consecutive
*     tape marks.

*  Arguments:
*     TD=INTEGER (Given)
*        A variable containing the tape descriptor.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.
*        If this variable is not SAI__OK on input, then the routine
*        will return without action.
*        If the routine fails to complete, this variable will be set
*        to an appropriate error number.

*  Algorithm:
*     Obtain a physical tape descriptor pointing at the tape drive and
*     execute a skip EOV function.
*     Re-adjust current tape position information.

*  Copyright:
*     Copyright (C) 1981, 1983, 1986, 1991, 1993 Science & Engineering Research Council.
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
*     Jack Giddings (UCL::JRG)
*     Sid Wright (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     11-SEP-1981:  Original.  (UCL::JRG)
*     01-FEB-1983:  Fortran 77 Version. (UCL::JRG)
*     11-Jun-1983:  Remove machine-dependent I/O and tidy up. (UCL::SLW)
*      6-Nov-1986:  Shorten comment lines for DOMAN  (RAL::AJC)
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
      INCLUDE 'MIO_ERR'         ! MIO Errors

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
      INTEGER TD                ! Physical tape descriptor

*.


*    Allowed to execute ?
      IF ( STATUS.NE.SAI__OK ) RETURN

      CALL MAG1_GETTD(TP, TD, STATUS)
      IF ( STATUS.EQ.SAI__OK ) THEN
         CALL MIO_SKIPV(TD, STATUS)
         TMOD(TP) = .TRUE.
         IF ( STATUS.EQ.MIO__EOV .OR. STATUS.EQ.MIO__EOF ) THEN
            IF ( TFILE(TP).GT.0 ) TFILE(TP) = TFILE(TP) + 1
            TSTART(TP) = .TRUE.
            TBLOCK(TP) = 1
            STATUS = SAI__OK
         ELSE IF ( STATUS.EQ.SAI__OK .OR. STATUS.EQ.MIO__PARIT .OR.
     :             STATUS.EQ.MIO__DATOV ) THEN
            STATUS = MAG__NOEOV
            IF ( TBLOCK(TP).GT.0 ) THEN
               IF ( TSTART(TP) ) THEN
                  TBLOCK(TP) = TBLOCK(TP) + 1
               ELSE IF ( .NOT.TSTART(TP) ) THEN
                  TBLOCK(TP) = TBLOCK(TP) - 1
               ELSE
                  TBLOCK(TP) = 0
               END IF
            END IF
         ELSE
            TFILE(TP) = 0
            TBLOCK(TP) = 0
         END IF
         IF ( STATUS.NE.SAI__OK ) CALL MAG1_ERRTP(TP, STATUS)
      END IF

      RETURN
      END
