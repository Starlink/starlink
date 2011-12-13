      SUBROUTINE MAG1_INIT(STATUS)
*+
*  Name:
*     MAG1_INIT

*  Purpose:
*     Initialise MAG Common blocks.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG1_INIT(STATUS)

*  Description:
*     Initialise MAG Common blocks.

*  Arguments:
*     STATUS=INTEGER (Given and Returned)
*        Variable to hold the status value.
*        N.B. This routine does not report its own errors.

*  Algorithm:
*     Set common block variables to null states.

*  Copyright:
*     Copyright (C) 1980, 1983, 1991, 1993 Science & Engineering Research Council.
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
*     30-Jul-1980: Original. (UCL::SLW)
*     10-May-1983: Tidy up for Starlink version. (UCL::SLW)
*     14-Nov-1991:  Changed to new-style prologue (RAL::KFH)
*           Replaced tabs in end-of-line comments (RAL::KFH)
*           Replaced fac_$name by fac1_name (RAL::KFH)
*           Inserted implicit none (RAL::KFH)
*    22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}

*  Notes:
*     Formerly known as MAG_$INIT

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definition:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MAG_SYS'         ! MAG Internal symbols and errors.

*  Arguments Returned:

*  Status:
      INTEGER STATUS            ! status return

*  Global Variables:
      INCLUDE 'MAGIO_CMN'

*  External References:
      EXTERNAL MAG1_BLK          ! Block data subprogram that
                                 ! initializes MAGINT
*  Local Variables:
      INTEGER TP                ! loop index

*.


*    Initialise common block
      DO 100 TP = 1, MAG__MXDEV
         TFREE(TP) = .TRUE.
         TTD(TP) = 0
         TSTART(TP) = .TRUE.
!        Tfile(tp) = MAG__UNDEF
         TFILE(TP) = 0
!        Tblock(tp) = MAG__UNDEF
         TBLOCK(TP) = 0
         TMOD(TP) = .FALSE.
         TNAME(TP) = ' '
 100  CONTINUE
      MAGINT = .TRUE.

      RETURN
      END
