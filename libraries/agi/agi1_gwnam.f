************************************************************************

      SUBROUTINE AGI_1GWNAM ( INAME, PACKGE, AGINAM, STATUS )

*+
*  Name:
*     AGI_1GWNAM
*
*  Purpose:
*     Translate the given name into an AGI workstation name
*
*  Invocation:
*     CALL AGI_1GWNAM( INAME, PACKGE, AGINAM, STATUS )
*
*  Description:
*     This routine converts the workstation name given by INAME into
*     a name suitable for use by AGI. The mechanism of this routine
*     is not really important, it can be changed to suit a particular
*     system. This routine currently uses GNS.
*     The returned name should reflect the physical device being
*     addressed. For instance a device having overlay planes should
*     return the same name for the the base plane and the overlay
*     planes.
*     The naming system at present reflects the GKS way of identifying
*     workstations. The name is made up from the workstation type and
*     sequence number, but if the workstation type indicates an
*     overlay plane then the workstation type of the base plane is used.
*     The package argument specifies if a particular graphics package
*     should be searched. If a blank string is entered, all packages
*     are searched.
*
*  Arguments:
*     INAME = CHARACTER*(*) (Given)
*        Name of workstation to translate
*     PACKGE = CHARACTER*(*) (Given)
*        Name of package to search. Blank means search all
*     AGINAM = CHARACTER*(DAT__SZNAM) (Returned)
*        AGI name for the physical device
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Convert the package name to upper case.
*     See if the GKS part of GNS can convert the device name into an
*     AGI name.
*     If not then see if the IDI part of GNS can perform the conversion.
*     If both have failed then return an error.
*
*  Copyright:
*     Copyright (C) 1988, 1990, 1992 Science & Engineering Research Council.
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
*     NE: Nick Eaton (Durham University)
*
*  History:
*     Aug 1988 (NE):
*        Original version
*     Jun 1990 (NE):
*        Converted to use GNS. Added PACKGE argument.
*     Sep 1990 (NE):
*        Annul GNS errors
*     Jul 1992 (NE):
*        Ensure ERR_RLSE is called for all paths
*     Oct 1992 (NE):
*        Add device name to error reports
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_ERR'

*  Arguments Given :
      CHARACTER * ( * ) INAME
      CHARACTER * ( * ) PACKGE

*  Arguments Returned :
      CHARACTER * ( DAT__SZNAM ) AGINAM

*  Status :
      INTEGER STATUS

*  Local variables :
      CHARACTER LPACK * 64

      INTEGER ISTAT, LNAME
*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Set up a new error context
      CALL ERR_MARK

*   Copy the package argument to a local variable and convert to upper case
      LPACK = PACKGE
      CALL CHR_LDBLK( LPACK )
      CALL CHR_UCASE( LPACK )

*   See if the name is a valid GKS name.
      ISTAT = SAI__OK
      IF ( ( LPACK .EQ. 'GKS' ) .OR. ( LPACK .EQ. ' ' ) ) THEN
         CALL GNS_START( 'GKS', ISTAT )
         CALL GNS_IANG( INAME, AGINAM, ISTAT )

*   If the name is recognised then return
         IF ( ISTAT .EQ. SAI__OK ) THEN
            CALL ERR_RLSE
            GOTO 99
         ELSE
            CALL ERR_ANNUL( ISTAT )
         ENDIF
      ENDIF

*   See if the name is a valid IDI name.
      IF ( ( LPACK .EQ. 'IDI' ) .OR. ( LPACK .EQ. ' ' ) ) THEN
         CALL GNS_START( 'IDI', ISTAT )
         CALL GNS_IANI( INAME, AGINAM, ISTAT )

*   If the name is recognised then return
         IF ( ISTAT .EQ. SAI__OK ) THEN
            CALL ERR_RLSE
            GOTO 99
         ELSE
            CALL ERR_ANNUL( ISTAT )
         ENDIF
      ENDIF

*   If this has failed then there are no more packages to try
      CALL ERR_RLSE
      AGINAM = ' '
      STATUS = AGI__NAMNR
      LNAME = INDEX( INAME, ' ' ) - 1
      CALL MSG_SETC( 'NAME', INAME(:LNAME) )
      CALL ERR_REP( 'AGI_1GWNAM_NMNR',
     :              'Device name ^NAME not recognised', STATUS )

  99  CONTINUE

      END

