      SUBROUTINE MAG_START(STATUS)
*+
*  Name:
*     MAG_START

*  Purpose:
*     initialise MAG library.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG_START( STATUS )

*  Description:
*     The SCL version of the MAG library is initialised for
*     the start of an executable image.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The Global status.

*  Algorithm:
*     The table of assigned tape devices in the MAG_IO Common Block
*     is initialised.
*     The MAG error codes are given to SEM.
*     The MAG Exit Handler is established.

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
      INTEGER STATUS            ! Status

*  Global Variables:
      INCLUDE 'MAGPA_SCL'       ! MAG Parameter Table

*  Local Variables:
      INTEGER I                 ! loop index

*.


C      print *,'mag_start:status', status
*    Execution allowed ?
      IF ( STATUS.NE.SAI__OK ) RETURN

      DO 100 I = 1, MAG__MXDEV
         PFREE(I) = .TRUE.
         PDESC(I) = 0
         PTNAME(I) = ' '
         PDLOC(I) = ' '
 100  CONTINUE

C      print *,'mag_start:  completed'
      RETURN
      END
