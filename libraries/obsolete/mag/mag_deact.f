      SUBROUTINE MAG_DEACT(STATUS)
*+
*  Name:
*     MAG_DEACT

*  Purpose:
*     Annul all tape descriptors

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG_DEACT(STATUS)

*  Description:
*     MAG\_ANNUL is performed for all open tape devices which were obtained
*     with MAG\_ASSOC. The associated parameters are not cancelled.

*  Arguments:
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.
*        If the routine fails to complete, this variable will be set
*        to an appropriate error number.
*        If this variable is not SAI__OK on input, then the routine
*        will still attempt to execute, but will return with STATUS
*        set to the import value.

*  Algorithm:
*     Return without action.

*  Copyright:
*     Copyright (C) 1981, 1983, 1986, 1989, 1991, 1993 Science & Engineering Research Council.
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
*      6-Nov-1986:  Shorten comment lines for DOMAN  (RAL::AJC)
*     24-Jan-1989:  Improve documentation  (RAL::AJC)
*      8-Nov-1991:  Remove commented out code (RAL::KFH)
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
      INCLUDE 'MAG_ERR'         ! MAG Error codes
      INCLUDE 'MAGSCL_PAR'      ! MAG_SCL Internal Constants
*    Status return :
      INTEGER STATUS            ! status return

*  Global Variables:
      INCLUDE 'MAGGO_SCL'       ! MAG Initialisation Switch
      INCLUDE 'MAGPA_SCL'       ! MAG Parameter Table

*  External References:
      EXTERNAL MAG_BLK           ! Block data subprogram that
                                 ! initializes MAGSLP
*  Local Variables:
      INTEGER ISTAT             ! Local status
      INTEGER I                 ! loop index

*.


C      print *,'mag_deact:  status ', status
      ISTAT = STATUS
      STATUS = SAI__OK


      DO 100 I = 1, MAG__MXPAR
         IF ( .NOT.PFREE(I) ) CALL MAG_ANNUL(PDESC(I), STATUS)
 100  CONTINUE

      IF ( ISTAT.NE.SAI__OK ) STATUS = ISTAT

*    Set MAG asleep
*     Magslp = .true.
C      print *,'mag_deact:  status ', status

      RETURN
      END
