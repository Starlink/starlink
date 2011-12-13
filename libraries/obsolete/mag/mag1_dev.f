      LOGICAL FUNCTION MAG1_DEV(TP, DEVICE)
*+
*  Name:
*     MAG1_DEV

*  Purpose:
*     get VMS device name name from tape descriptor.

*  Language:
*     Starlink Fortran

*  Invocation:
*     STATUS = MAG1_DEV(TP, DEVICE)

*  Description:
*     The name of the VMS device associated with the supplied tape
*     descriptor, TP, is returned in DEVICE.

*  Arguments:
*     TP=INTEGER (Given)
*        A variable containing the tape descriptor
*     DEVICE=CHARACTER*(*) (Returned)
*        VMS Device name of the tape unit

*  Returned Value:
*     MAG1_DEV = LOGICAL
*        Set .TRUE. if legal request

*  Algorithm:
*     Use TP to index directly into the arrays in the MAG_IO Common Block.

*  Copyright:
*     Copyright (C) 1983, 1991, 1993 Science & Engineering Research Council.
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
*     Jack Giddings (ZUVAD::JRG)
*     Sid Wright (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (ZUVAD::JRG)
*     14-Jul-1983:  Re-organised version. (UCL::SLW)
*     14-Nov-1991:  Changed to new-style prologue (RAL::KFH)
*           Replaced tabs in end-of-line comments (RAL::KFH)
*           Replaced fac_$name by fac1_name (RAL::KFH)
*           Inserted implicit none (RAL::KFH)
*    22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}

*  Notes:
*     Formerly known as MAG_$DEV

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definition:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MAG_SYS'         ! MAG Internal Constants

*  Arguments Given:
      INTEGER TP                ! Tape descriptor

*  Arguments Returned:
      CHARACTER*(*) DEVICE      ! VMS device name

*  Global Variables:
      INCLUDE 'MAGIO_CMN'       ! MAG library states

*  External References:
      EXTERNAL MAG1_BLK          ! Block data subprogram that
                                 ! initializes MAGINT
*.


      MAG1_DEV = (TP.GE.1) .AND. (TP.LE.MAG__MXDEV)
      IF ( MAG1_DEV ) THEN
         MAG1_DEV = .NOT.TFREE(TP)
         IF ( MAG1_DEV ) DEVICE = TNAME(TP)
      END IF

      RETURN
      END
