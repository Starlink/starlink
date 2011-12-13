      SUBROUTINE MAG_WTM(TP, STATUS)
*+
*  Name:
*     MAG_WTM

*  Purpose:
*     Write tape mark.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG_WTM(TD, STATUS)

*  Description:
*     A tape mark is written at the current tape position.

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
*     Obtain the tape descriptor for the physical tape drive and write
*     a tape mark. Re-adjust the current tape position if necessary.

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


      IF ( STATUS.EQ.SAI__OK ) THEN
         CALL MAG1_GETTD(TP, TD, STATUS)
         IF ( STATUS.EQ.SAI__OK ) THEN
            CALL MIO_WTM(TD, STATUS)
            TMOD(TP) = .TRUE.
            IF ( STATUS.EQ.SAI__OK ) THEN
               IF ( TFILE(TP).GT.0 ) TFILE(TP) = TFILE(TP) + 1
               TSTART(TP) = .TRUE.
               TBLOCK(TP) = 1
            ELSE
               TFILE(TP) = 0
               TBLOCK(TP) = 0
               CALL MAG1_ERRTP(TP, STATUS)
            END IF
         END IF
      END IF

      RETURN
      END
