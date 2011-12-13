      SUBROUTINE MAG_ACTIV(STATUS)
*+
*  Name:
*     MAG_ACTIV

*  Purpose:
*     initialise MAG library for SCL application.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG_ACTIV(STATUS)

*  Description:
*     The SCL version of the MAG library is initialised for
*     the start of an executable image.

*  Arguments:
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.
*        If this variable is not SAI__OK on input, then the routine
*        will return without action.
*        If the routine fails to complete, this variable will be set
*        to an appropriate error number.

*  Algorithm:
*     The table of assigned tape devices in the MAG_IO Common Block
*     is initialised.
*     The MAG Exit Handler is established.

*  Copyright:
*     Copyright (C) 1981, 1983, 1984, 1985, 1986, 1991, 1993 Science & Engineering Research Council.
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
*     01-Nov-1984:  Provide linkage to USRDEVDATA  (RLVAD::TGD)
*     01-Feb-1985:  Table size is MAG__MXPAR not MXDEV (RLVAD::AJC)
*     03-Jun-1986:  ADAM version  (RLVAD::AJC)
*     16-Jul-1986:  Remove check for temp devices data structure (RLVAD::AJC)
*      6-Nov-1986:  Activate MIO & shorten prologue lines (RLVAD::AJC)
*     08-Nov-1991:  Remove commented out code (RAL::KFH)
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
*        Remove local variables SET and LOC.
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

*  Status return :
      INTEGER STATUS            ! Status

*  External References:
      EXTERNAL MAG_BLK           ! Block data subprogram that
                                 ! initializes MAGSLP
*  Global Variables:
      INCLUDE 'MAGPA_SCL'       ! MAG Parameter Table
      INCLUDE 'MAGGO_SCL'       ! MAG Initialisation Switch

*  Local Variables:
      INTEGER I                 ! loop index

*.


C      print *,'mag_activ:status', status
*    Execution allowed ?
      IF ( STATUS.NE.SAI__OK ) RETURN

*    Initialise MIO
      CALL MIO_ACTIV(STATUS)

      DO 100 I = 1, MAG__MXPAR
         PFREE(I) = .TRUE.
         PDESC(I) = 0
         PTNAME(I) = ' '
         PACMOD(I) = ' '
         PDLOC(I) = ' '
 100  CONTINUE


*    Declare MAG awake
      MAGSLP = .FALSE.
C      print *,'mag_activ:  completed'

      RETURN
      END
