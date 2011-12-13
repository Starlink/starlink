      SUBROUTINE MAG_JUMP(TP, NBLOCK, STATUS)
*+
*  Name:
*     MAG_JUMP

*  Purpose:
*     Skip a specified number of physical blocks.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG_JUMP(TD, NBLOCK, STATUS)

*  Description:
*     The tape is moved a specified number of blocks;  a negative
*     number indicating backwards movement.

*  Arguments:
*     TD=INTEGER (Given)
*        A variable containing the tape descriptor.
*     NBLOCK=INTEGER (Given)
*        Expression specifying the number of blocks to be skipped.
*        A negative number indicates that the tape is to be moved
*        in the reverse direction (towards its load point).
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.
*        If this variable is not SAI__OK on input, then the routine
*        will return without action.
*        If the routine fails to complete, this variable will be set
*        to an appropriate error number.

*  Algorithm:
*     Obtain a physical tape descriptor pointing at the tape drive and
*     execute skip block functions.
*     After each successful skip the tape position details are
*     updated in MAG_IO.
*     It is an error to attempt to jump across a Tape Mark with
*     this routine.

*  Copyright:
*     Copyright (C) 1980, 1981, 1983, 1986, 1991, 1993 Science & Engineering Research Council.
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
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     14-APR-1980:  Original.  (UCL::SLW)
*     11-SEP-1981:  Added code to remember tape position.  (UCL::JRG)
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
      INCLUDE 'MIO_ERR'         ! MIO Errors

*  Arguments Given:
      INTEGER TP                ! tape descriptor
      INTEGER NBLOCK            ! number of blocks to be skipped
*    Status return :
      INTEGER STATUS            ! status return

*  Global Variables:
      INCLUDE 'MAGIO_CMN'       ! MAG library states

*  External References:
      EXTERNAL MAG1_BLK          ! Block data subprogram that
                                 ! initializes MAGINT
*  Local Variables:
      INTEGER TD                ! Physical tape descriptor
      INTEGER NLEFT             ! number of blocks to be skipped
      INTEGER DIR               ! direction of skip

*.


*    Allowed to execute ?
      IF ( STATUS.NE.SAI__OK ) RETURN

      CALL MAG1_GETTD(TP, TD, STATUS)
      IF ( STATUS.EQ.SAI__OK ) THEN
         IF ( NBLOCK.NE.0 ) THEN
            IF ( NBLOCK.LT.0 ) THEN
               DIR = -1
            ELSE
               DIR = 1
            END IF
            NLEFT = NBLOCK
            DO WHILE ( NLEFT.NE.0 )
               NLEFT = NLEFT - DIR
               CALL MIO_SKIPB(TD, DIR, STATUS)
               TMOD(TP) = .TRUE.
               IF ( STATUS.EQ.SAI__OK ) THEN
                  IF ( TBLOCK(TP).GT.0 ) THEN
                     IF ( TSTART(TP) ) THEN
                        TBLOCK(TP) = TBLOCK(TP) + DIR
                     ELSE IF ( .NOT.TSTART(TP) ) THEN
                        TBLOCK(TP) = TBLOCK(TP) - DIR
                     END IF
                  END IF
               ELSE IF ( STATUS.EQ.MIO__EOF .OR. STATUS.EQ.MIO__EOV )
     :                   THEN
                  IF ( TFILE(TP).GT.0 ) TFILE(TP) = TFILE(TP) + DIR
                  IF ( DIR.GT.0 ) THEN
                     TSTART(TP) = .TRUE.
                  ELSE
                     TSTART(TP) = .FALSE.
                  END IF
                  TBLOCK(TP) = 1
               ELSE
                  TFILE(TP) = 0
                  TBLOCK(TP) = 0
               END IF
               IF ( STATUS.NE.SAI__OK ) THEN
                  CALL MAG1_ERRTP(TP, STATUS)
                  GO TO 1
               END IF
            END DO
 1          CONTINUE
         END IF
      END IF

      RETURN
      END
