
      SUBROUTINE MAG1_GTMPL(LOC, STATUS)
*+
*  Name:
*     MAG1_GTMPL

*  Purpose:
*     get locator to temporary Mag. Tape Dataset.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG1_GTMPL(LOC, STATUS)

*  Description:
*     A locator to the users own devices data structure is obtained.
*     Initially an attempt to translate environment variable USRDEVDATA
*     is made. If that fails, file USRDEVDATA in the users ADAM_USER
*     directory is used. ADAM_USER is determined by SUBPAR_ADMUS which
*     attempts to translate environment variable ADAM_USER and, if that
*     fails, uses $HOME/adam.

*  Arguments:
*     LOC=CHARACTER*(*) (Returned)
*        Will contain the locator to the users devices data structure
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.

*  Algorithm:
*     Get a locator to the user's devices data structure (USRDEVDATA).  If
*     not there, then create it first.

*  Copyright:
*     Copyright (C) 1991, 1993 Science & Engineering Research Council.
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
*     Sid Wright  (UCL:SLW)
*     Trevor Dimbylow  (RLVAD::TGD)
*     {enter_new_authors_here}

*  History:
*     Sid Wright         22-Apr-82
*     Trevor Dimbylow    15-Oct-84
*     A.Chipperfield     03-Jun-86  ADAM version. Use hds_open etc.
*     K.F.Hartley        08-Nov-91  Remove commented out code
*     08-Nov-1991: (RAL::KFH)
*            Change to new style prologues
*            Change all fac_$name to fac1_name
*            Replace tabs in end-of-line comments
*            Remove /nolist in INCLUDE
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*      4-May-1995:  Use environment variable USRDEVDATA or, failing that,
*                   file $ADAM_USER/USRDEVDATA
*     {enter_further_changes_here}

*  Notes:
*     Formerly known as MAG_$GTMPL

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
!      INCLUDE 'MAG_ERR'         ! MAG Errors

*  Arguments Returned:
      CHARACTER*(*) LOC         ! locator to tape device dataset
      INTEGER STATUS            ! status return

*  Local Variables:
      INTEGER ADMLEN
      CHARACTER*200 USRDEVDATA
      CHARACTER*200 ADMUSR


*.

*  Determine the name of the user's device dataset.
*  First attempt to translate USRDEVDATA.
*  If that doesn't work, assume USRDEVDATA in ADAM_USER
      CALL PSX_GETENV( 'USRDEVDATA', USRDEVDATA, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL SUBPAR_ADMUS( ADMUSR,  ADMLEN, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            USRDEVDATA = ADMUSR(1:ADMLEN) // 'USRDEVDATA'
         ELSE
            CALL ERR_ANNUL( STATUS )
            USRDEVDATA = 'USRDEVDATA'
         ENDIF
      ENDIF

      CALL HDS_OPEN(USRDEVDATA, 'UPDATE', LOC, STATUS)
      IF ( STATUS.NE.SAI__OK ) THEN
         CALL ERR_ANNUL(STATUS)
         CALL HDS_NEW(USRDEVDATA, 'DEVICES', 'DEVICES', 0, 0, LOC,
     :                STATUS)
      END IF

C      print *,'mag1_gtmpl:status',status

      RETURN
      END
