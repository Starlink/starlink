      SUBROUTINE MAG1_CHKTD(TD, TP, RTP, STATUS)
*+
*  Name:
*     MAG1_CHKTD

*  Purpose:
*     Get tape parameter descriptor from tape descriptor.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG1_CHKTD(TD, TP, RTP, STATUS)

*  Description:
*     Given the MAG Tape Descriptor, the corresponding Tape parameter
*     descriptor is found.

*  Arguments:
*     TD=INTEGER (Given)
*        The import tape descriptor
*     TP=INTEGER (Returned)
*        A variable to contain the tape parameter descriptor.
*     RTP=INTEGER (Returned)
*        A variable to contain the relative tape parameter descriptor.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.

*  Algorithm:
*     The Tape Descriptor is looked up in the MAG_PA common block.
*     If it is found, then the parameter descriptor corrseponding to
*     its position in the table is returned.

*  Copyright:
*     Copyright (C) 1983, 1986, 1991, 1993 Science & Engineering Research Council.
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
*     05-APR-1983:  Original.  (UCL::JRG)
*     14-Jul-1983: Vermagn for new parameter system. (UCL::SLW)
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
*     Formerly known as MAG_$CHKTD

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'PAR_PAR'          ! Parameter system constants
      INCLUDE 'MAG_SYS'          ! MAG Internal Symbolic Constants
      INCLUDE 'MAG_ERR'          ! MAG Error codes
      INCLUDE 'MAGSCL_PAR'       ! MAG Environment Symbolic Constants
*    Local Constant
      INTEGER MAG__BASE
      PARAMETER (MAG__BASE=0)

*  Arguments Given:
      INTEGER TD                ! Tape Descriptor

*  Arguments Returned:
      INTEGER TP                ! tape parameter descriptor
      INTEGER RTP               ! relative tape parameter descriptor
*    Status return :
      INTEGER STATUS            ! status return

*  Global Variables:
      INCLUDE 'MAGPA_SCL'       ! MAG Parameter Table

*  Local Variables:
      INTEGER I                 ! Loop index

*.


C      print *,'mag1_chktd:  td ', td
      STATUS = MAG__UNKPA
      DO 100 I = 1, MAG__MXPAR
         IF ( .NOT.PFREE(I) ) THEN
            IF ( TD.EQ.PDESC(I) ) THEN
               RTP = I
               TP = RTP + MAG__BASE
               STATUS = SAI__OK
               GO TO 1
            END IF
         END IF
 100  CONTINUE
 1    CONTINUE
C      print *,'mag1_chktd:  tp,rtp ', tp,rtp

      RETURN
      END
