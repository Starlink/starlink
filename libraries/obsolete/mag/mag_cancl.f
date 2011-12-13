      SUBROUTINE MAG_CANCL(PNAME, STATUS)
*+
*  Name:
*     MAG_CANCL

*  Purpose:
*     Close a tape device defined by a parameter

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG_CANCL(DEVICE, STATUS)

*  Description:
*     Release the tape drive associated with the parameter, annul the
*     associated tape descriptor and cancel the parameter.

*  Arguments:
*     DEVICE=CHARACTER*(*) (Given)
*        Expression specifying the name of a Tape Device Parameter,
*        which has previously been associated with a device using
*        MAG_ASSOC.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.
*        If the routine fails to complete, this variable will be set
*        to an appropriate error number.
*        If this variable is not SAI__OK on input, then the routine
*        will still attempt to execute, but will return with STATUS
*        set to the import value.

*  Algorithm:
*     The tape descriptor associated with this parameter is found using
*     MAG1_PENT, then the device is deassigned using MAG_CLOSE, then the
*     the Parameter is annuled using PAR_ANNUL.

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
*     Sid Wright  (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     16-Apr-1983:  Original.  (UCL::SLW)
*      6-Nov-1986:  Shorten comment lines for DOMAN  (RAL::AJC)
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
*        Add INCLUDE 'DAT_PAR'
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
      INCLUDE 'MAGSCL_PAR'      ! MAG_SCL Constants

*  Arguments Given:
      CHARACTER*(*) PNAME       ! Tape Parameter Tape
*    Status return :
      INTEGER STATUS            ! status return

*  Global Variables:
      INCLUDE 'MAGGO_SCL'       ! MAG Initialisation Switch
      INCLUDE 'MAGPA_SCL'       ! MAG Parameter Table

*  External References:
      EXTERNAL MAG_BLK           ! Block data subprogram that
                                 ! initializes MAGSLP
*  Local Variables:
      INTEGER TP                ! tape parameter descriptor
      INTEGER RTP               ! relative tape parameter descriptor
      INTEGER ISTAT             ! local status

*.


      ISTAT = STATUS
      STATUS = SAI__OK

*    Initialised ?
      IF ( MAGSLP ) THEN
         CALL MAG_ACTIV(STATUS)
         IF ( STATUS.NE.SAI__OK ) RETURN
      END IF

      CALL MAG1_FNDTP(PNAME, TP, RTP, STATUS)
      IF ( STATUS.NE.SAI__OK ) THEN
         CALL MAG1_ERNAM(PNAME, STATUS)
      ELSE
         CALL MAG_ANNUL(PDESC(RTP), STATUS)
         CALL PAR_CANCL(PNAME, STATUS)
      END IF

      IF ( ISTAT.NE.SAI__OK ) STATUS = ISTAT

      RETURN
      END
