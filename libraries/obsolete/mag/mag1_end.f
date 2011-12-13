      SUBROUTINE MAG1_END(STATUS)
*+
*  Name:
*     MAG1_END

*  Purpose:
*     Close down MAG.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG1_END(STATUS)

*  Description:
*     Close all open files.

*  Arguments:
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If the routine fails to
*        complete, this variable will be set to an appropriate error number.
*        If this variable is not SAI__OK on input, then the routine will
*        still attempt to execute, but will return with STATUS set to the
*        import value.
*        N.B. This routine does not report its own errors.

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
*     Formerly known as MAG_$END

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
      INTEGER ISTAT
      INTEGER I                 ! loop index

*.


      ISTAT = STATUS
      STATUS = SAI__OK
      DO 100 I = 1, MAG__MXDEV
         IF ( .NOT.TFREE(I) ) CALL MAG_CLOSE(I, STATUS)
 100  CONTINUE
      MAGINT = .FALSE.

      IF ( ISTAT.NE.SAI__OK ) STATUS = ISTAT

      RETURN
      END
