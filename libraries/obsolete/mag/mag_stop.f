      SUBROUTINE MAG_STOP(STATUS)
*+
*  Name:
*     MAG_STOP

*  Purpose:
*     terminate MAG library.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG_STOP( STATUS )

*  Description:
*     The SCL version of the MAG library is de-initialised for
*     the end of an executable image.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The Global status.

*  Algorithm:
*     Return without action.

*  Copyright:
*     Copyright (C) 1981, 1983, 1991, 1993 Science & Engineering Research Council.
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
*     Sid Wright  (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     15-OCT-1981:  Original.  (UCL::SLW)
*     17-Apr-1983:  Starlink Version. (UCL::SLW)
*     08-Nov-1991: (RAL::KFH)
*            Change to new style prologues
*            Change all fac_$name to fac1_name
*            Replace tabs in end-of-line comments
*            Remove /nolist in INCLUDE
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     4-FEB-1993 (PMA):
*        Add INCLUDE 'DAT_PAR'
*        Add INCLUDE 'PAR_PAR'
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'PAR_PAR'          ! Parameter system constants
      INCLUDE 'MAG_SYS'         ! MAG Internal Constants
      INCLUDE 'MAGSCL_PAR'      ! MAG_SCL Internal Constants
*    Status return :
      INTEGER STATUS            ! status return

*  Global Variables:
      INCLUDE 'MAGPA_SCL'       ! MAG Parameter Table

*  Local Variables:
      INTEGER ISTAT             ! Local status
      INTEGER I                 ! loop index

*.


C      print *,'mag_stop:  status ', status
      ISTAT = STATUS
      STATUS = SAI__OK

      DO 100 I = 1, MAG__MXPAR
         IF ( .NOT.PFREE(I) ) CALL MAG_CANCL(PTNAME(I), STATUS)
 100  CONTINUE

      IF ( ISTAT.NE.SAI__OK ) STATUS = ISTAT

C      print *,'mag_stop:  status ', status
      RETURN
      END
