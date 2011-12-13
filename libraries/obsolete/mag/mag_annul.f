      SUBROUTINE MAG_ANNUL(TD, STATUS)
*+
*  Name:
*     MAG_ANNUL

*  Purpose:
*     Annul tape descriptor, releasing any associated tape drive

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG_ANNUL(TD, STATUS)

*  Description:
*     Release the tape drive  associated with the tape descriptor.
*     Annul tape descriptor but do not cancel the associated parameter.

*  Arguments:
*     TD=INTEGER (Given)
*        A variable containing the tape descriptor.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.
*        If the routine fails to complete, this variable will be set
*        to an appropriate error number.
*        If this variable is not SAI__OK on input, then the routine
*        will still attempt to execute, but will return with STATUS
*        set to the import value.

*  Algorithm:
*     Check that the tape descriptor is valid, and is active and then
*     release relevant table entries.

*  Implementation Deficiencies:
*     Data-system enbedded files not yet supported.

*  Copyright:
*     Copyright (C) 1983, 1986, 1989, 1991, 1993 Science & Engineering Research Council.
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
*     16-Apr-1983:  Original.  (UCL::SLW)
*     15-Jul-1986:  HDS_CLOSE USRDEVDATA to match MAG1_GTMPL. (RAL::AJC)
*        6-Nov-1986:  Shorten comment lines for DOMAN  (RAL::AJC)
*     24-jan-1989:  Improve documentation  (RAL::AJC)
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
      INCLUDE 'MAGSCL_PAR'      ! MAG_SCL Constants

*  Arguments Given:
      INTEGER TD                ! tape descriptor

*  Arguments Returned:

*  Status:
      INTEGER STATUS            ! status return

*  Global Variables:
      INCLUDE 'MAGPA_SCL'       ! MAG Parameter Table

*  Local Variables:
      INTEGER TP                ! tape parameter descriptor
      INTEGER RTP               ! relative tape parameter descriptor
      INTEGER ISTAT             ! local status value
      INTEGER FILE
      LOGICAL START
      INTEGER BLOCK
      LOGICAL MOVED

*.


      ISTAT = STATUS
      STATUS = SAI__OK

      CALL MAG1_CHKTD(TD, TP, RTP, STATUS)
      IF ( STATUS.EQ.SAI__OK ) THEN
         CALL MAG_POS(TD, FILE, START, BLOCK, MOVED, STATUS)
         IF ( MOVED ) CALL MAG1_WRTDS(PDLOC(RTP), FILE, START, BLOCK,
     :                                STATUS)
                              ! update the dataset if modified
         IF ( PDESC(RTP).NE.0 ) CALL MAG_CLOSE(PDESC(RTP), STATUS)
*       Annul locator to tape dataset
         CALL DAT_ANNUL(PDLOC(RTP), STATUS)
*       Annul locator to user's device dataset and close if
*       no other locators to it.
         CALL HDS_CLOSE(PLOC(RTP), STATUS)
*       Set current state
         PTNAME(RTP) = ' '
         PACMOD(RTP) = ' '
         PDESC(RTP) = 0
         PFREE(RTP) = .TRUE.
      END IF

      IF ( ISTAT.NE.SAI__OK ) STATUS = ISTAT

      RETURN
      END
