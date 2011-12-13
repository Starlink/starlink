      SUBROUTINE MAG1_CPTDS(ELOC, NAME, LOC, STATUS)
*+
*  Name:
*     MAG1_CPTDS

*  Purpose:
*     copy system-wide tape dataset to local storage.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MAG1_CPTDS(ELOC, NAME, LOC, STATUS)

*  Description:
*     This routine copies the specified structure in the system-wide device
*     dataset to the structure located by the given locator (usually the
*     users own DEVICES data structure). The system-wide device dataset is
*     defined by the environment variable DEVDATASET or. failing that, at
*     ../etc/devdataset.sdf relative to one of the directories on the user's
*     PATH.
*     If there is no system-wide device dataset, or the specified structure
*     cannot be found in it, a new structure is created in the users file
*     with the given name and assuming that the required device has the same
*     name (in lower case).

*  Arguments:
*     ELOC=CHARACTER*(*) Given)
*        Locator to the (user) tape data structure
*     NAME=CHARACTER*(*) (Given)
*        Name of the tape device parameter
*     LOC=CHARACTER*(*) (Returned)
*        Locator to the (system) tape device data structure
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.

*  Algorithm:
*     A locator to the tape device data set within the system defined devices
*     data structure is obtained.   The tape device data set is then copied to
*     a user specified data structure.

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
*     Sid Wright  (UCL::SLW)
*     {enter_new_authors_here}

*  History:
*     Sid Wright         22-Apr-82
*     A.Chipperfield 03-Jun-86  ADAM version use hds_open
*     A.Chipperfield 18-Jul-86  Close devdataset at end
*     A.Chipperfield  4-Nov-88  Use DAT_COPY not RCOPY
*     A.Chipperfield 12-Jan-90  Differentiate MAG__LCERR and UNKDV
*        and add ERR_REPs on setting status.
*     08-Nov-1991: (RAL::KFH)
*            Change to new style prologues
*            Change all fac_$name to fac1_name
*            Replace tabs in end-of-line comments
*            Remove /nolist in INCLUDE
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     4-FEB-1993 (PMA):
*        Add INCLUDE 'DAT_PAR'
*     5-MAR-1993 (PMA):
*        Change the name of the file devdataset to /star/etc/devdataset.
*     4-MAY-1995 (AJC):
*        Create structure if system-wide one doesn't exist
*        Avoid hardwired /star/etc/devdataset by using EMS1_STARF
*     {enter_further_changes_here}

*  Notes:
*     Formerly known as MAG_$CPTDS

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'MAG_ERR'         ! MAG Errors

*  Arguments Given:
      CHARACTER*(DAT__SZLOC) ELOC
      CHARACTER*(*) NAME        ! parameter name

*  Arguments Returned:
      CHARACTER*(*) LOC         ! locator to tape device dataset
      INTEGER STATUS            ! status return

*  Local Variables:
      INTEGER ISTAT             ! local status
      INTEGER PATHLEN
      CHARACTER*(DAT__SZLOC) TLOC
      CHARACTER*200 DATASET
      CHARACTER*200 UNAME
*.

      IF ( STATUS.NE.SAI__OK ) RETURN
      CALL ERR_MARK

*  Look for a device dataset defined by the DEVDATASET environment variable
*    If that didn't produce anything, look along PATH
      CALL EMS1_STARF( 'DEVDATASET', ' ', 'R',
     :                  DATASET, PATHLEN )

      IF ( DATASET .EQ. ' ' ) THEN
         CALL EMS1_STARF( 'PATH', '../etc/devdataset.sdf', 'R',
     :                    DATASET, PATHLEN )
      END IF

*   Set error if device dataset not found
      IF ( DATASET .EQ. ' ' ) THEN
         STATUS = MAG__LCERR
      END IF

*   If we have the device dataset
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL HDS_OPEN( DATASET, 'READ', TLOC, STATUS)
         IF ( STATUS.EQ.SAI__OK ) THEN
*      Get locator to device entry in DEVDATASET
            CALL DAT_FIND(TLOC, NAME, LOC, STATUS)
            IF ( STATUS.NE.SAI__OK ) THEN
*         Unable to find 'name' in DEVDATASET
*         Flag MAG__LCERR to cause component to be created in
*         user's file.
               STATUS = MAG__LCERR
            ELSE
*         Specified device entry found - copy it to USRDEVDATA
               CALL DAT_COPY(LOC, ELOC, NAME, STATUS)
               CALL DAT_ANNUL(LOC, STATUS)
               IF ( STATUS.NE.SAI__OK ) THEN
                  STATUS = MAG__CRERR
                  CALL MAG1_ERNAM(NAME, STATUS)
               END IF
            END IF
         END IF
*    Force closure of DEVDATASET if open
         ISTAT = SAI__OK
         CALL HDS_CLOSE(TLOC, ISTAT)
         IF ( STATUS.EQ.SAI__OK ) STATUS = ISTAT

      END IF

*  If we couldn't get the component from the system-wide dataset,
*  create a suitable one in the user's file
      IF ( STATUS .EQ. MAG__LCERR ) THEN
         CALL ERR_ANNUL( STATUS )
         UNAME = NAME
         CALL CHR_LCASE( UNAME )
         CALL MAG1_CRTDS( ELOC, UNAME, NAME, STATUS )
      END IF

*  If we have either copied from DEVDATASET or created a new component
*  successfully, get a locator to the component
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL DAT_FIND(ELOC, NAME, LOC, STATUS)
         IF ( STATUS.EQ.SAI__OK ) CALL MAG1_CNTDS(LOC, STATUS)
                                               ! cancel entries
      END IF

      CALL ERR_RLSE

      END
