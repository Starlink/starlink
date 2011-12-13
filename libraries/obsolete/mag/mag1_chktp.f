      SUBROUTINE MAG1_CHKTP(TP, RTP, STATUS)
*+
*  Name:
*     MAG1_CHKTP

*  Purpose:
*     Check tape parameter descriptor.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG1_CHKTP(TP, RTP, STATUS)

*  Description:
*     Check that the tape descriptor is valid.

*  Arguments:
*     TP=INTEGER (Given)
*        A variable to contain the tape parameter descriptor.
*     RTP=INTEGER (Returned)
*        A variable to contain the relative tape parameter descriptor.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.

*  Algorithm:
*     Check the tape tables to see if tape has already been accessed

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
*     Sid Wright    (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     12-Sep-1981:  Original.  (UCL::SLW)
*     05-Feb-1983:  Starlink-ised version  (UCL::SLW)
*     02-Jun-1986: ADAM Version MAG__BASE=0
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

*  Notes:
*     Formerly known as MAG_$CHKTP

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type definitions
      IMPLICIT NONE

      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'PAR_PAR'          ! Parameter system constants
      INCLUDE 'MAG_SYS'         ! MAG Internal Constants
      INCLUDE 'MAG_ERR'         ! MAG Errors
      INCLUDE 'MAGSCL_PAR'      ! MAG_SCL Constants
*    Local Constant
      INTEGER MAG__BASE
      PARAMETER (MAG__BASE=0)

*  Arguments Given:
      INTEGER TP                ! tape descriptor

*  Arguments Returned:
      INTEGER RTP               ! relative tape descriptor
*    Status return
      INTEGER STATUS            ! status return

*  Global Variables:
      INCLUDE 'MAGPA_SCL'       ! MAG Parameter Table

*.


*    Allowed to execute ?
      IF ( STATUS.NE.SAI__OK ) RETURN

      RTP = TP - MAG__BASE
      IF ( RTP.LT.1 .OR. RTP.GT.MAG__MXPAR ) THEN
         STATUS = MAG__ILLTD
      ELSE IF ( PFREE(RTP) ) THEN
         STATUS = MAG__NTACT
      END IF

C      print *,'mag1_chktp:tp,rtp,status',tp,rtp,status
      RETURN
      END
