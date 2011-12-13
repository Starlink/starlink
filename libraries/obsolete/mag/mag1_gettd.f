      SUBROUTINE MAG1_GETTD(TP, TD, STATUS)
*+
*  Name:
*     MAG1_GETTD

*  Purpose:
*     get channel from tape descriptor.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG1_GETTD(TP, TD, STATUS)

*  Description:
*     The channel associated with the given tape descriptor is returned.

*  Arguments:
*     TP=INTEGER (Given)
*        A variable containing the tape descriptor
*     TD=INTEGER (Returned)
*        A variable containing the tape channel descriptor
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.

*  Algorithm:
*     The channel number is obtained from MAG_IO.

*  Copyright:
*     Copyright (C) 1983, 1991, 1993 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     11-Jul-1983:  Original.  (UCL::SLW)
*     14-Nov-1991:  Changed to new-style prologue (RAL::KFH)
*           Replaced tabs in end-of-line comments (RAL::KFH)
*           Replaced fac_$name by fac1_name (RAL::KFH)
*           Inserted implicit none (RAL::KFH)
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     29-Jun-1995:  Use FACER not ^STATUS (RAL::AJC)
*     {enter_further_changes_here}

*  Notes:
*     Formerly known as MAG_$GETTD

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definition:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MAG_SYS'         ! MAG Internal Constants
      INCLUDE 'MAG_ERR'         ! MAG Errors

*  Arguments Given:
      INTEGER TP                ! tape descriptor

*  Arguments Returned:
      INTEGER TD                ! tape device descriptor
*    Status return :
      INTEGER STATUS            ! status return

*  Global Variables:
      INCLUDE 'MAGIO_CMN'       ! MAG library states

*  External References:
      EXTERNAL MAG1_BLK          ! Block data subprogram that
                                 ! initializes MAGINT
*.


      IF ( TP.LT.1 .OR. TP.GT.MAG__MXDEV ) THEN
         STATUS = MAG__ILLTD
         CALL MSG_SETI('DSC', TP)
         CALL ERR_FACER('STAT', STATUS)
         CALL ERR_REP('MAG_INVDSC', '^DSC : ^STAT', STATUS)
      ELSE IF ( TFREE(TP) ) THEN
         STATUS = MAG__NTOPN
         CALL MSG_SETI('DSC', TP)
         CALL ERR_FACER('STAT', STATUS)
         CALL ERR_REP('MAG_INVDSC', '^DSC : ^STAT', STATUS)
      ELSE
         TD = TTD(TP)
      END IF


      RETURN
      END
