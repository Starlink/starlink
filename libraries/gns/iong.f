      SUBROUTINE GNS_IONG ( IWKID, NAME, LNAME, STATUS )

*+
*  Name:
*     GNS_IONG

*  Purpose:
*     Inquire overlay device name of GKS workstation

*  Invocation:
*     CALL GNS_IONG( IWKID, NAME, LNAME, STATUS )

*  Description:
*     The name of the overlay device for the GKS workstation specified
*     by the workstation identifier is returned. This can then be used
*     to open the device with GKS. If the device does not have an
*     overlay then an error is returned.
*
*     If the name is longer than the supplied character variable the
*     name is truncated but the length returned is the actual length of
*     the name.

*  Arguments:
*     IWKID = INTEGER (Given)
*        GKS workstation identifier
*     NAME = CHARACTER*(*) (Returned)
*        Overlay device name
*     LNAME = INTEGER (Returned)
*        Length of name
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Side Effects:
*     The GNS database may be opened.

*  Deficiencies:
*     At present the translation from base device to overlay device is
*     hardwired into this routine, but a fancier method using the
*     GKSDEVICES.TXT file could be invented if required. This would
*     resemble the AGITYPE qualifier.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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

*  History:
*     24-AUG-1992 (NE):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'GNS_ERR'
      INCLUDE 'GNS_PAR'
      INCLUDE 'gns.cmn'

*  Arguments Given:
      INTEGER IWKID

*  Arguments Returned:
      CHARACTER*(*) NAME
      INTEGER LNAME

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I, ICON, IERR, ITYPE, MATCH, OTYPE
*.

*   Check global status
      LNAME = 0
      IF ( STATUS .NE. 0 ) GOTO 99

*   Get workstation type and connection id
      CALL GQWKC( IWKID, IERR, ICON, ITYPE )
      IF ( IERR .NE. 0 ) THEN
         STATUS = GNS__INWKID
         CALL EMS_REP( 'GNS_IONG_INWKID',
     :                 'Invalid GKS workstation identifier', STATUS )
         GOTO 99
      ELSE

*   Hardwire the overlay device translations into this routine
         IF ( ITYPE .EQ. 3200 ) THEN
            OTYPE = 3201
         ELSEIF ( ITYPE .EQ. 3202 ) THEN
            OTYPE = 3203
         ELSEIF ( ITYPE .EQ. 3800 ) THEN
            OTYPE = 3805
         ELSEIF ( ITYPE .EQ. 3801 ) THEN
            OTYPE = 3806
         ELSEIF ( ITYPE .EQ. 3802 ) THEN
            OTYPE = 3807
         ELSEIF ( ITYPE .EQ. 3803 ) THEN
            OTYPE = 3808
         ELSE
            STATUS = GNS__NOVER
            CALL EMS_SETI( 'TYPE', ITYPE )
            CALL EMS_REP( 'GNS_IONG_NOVER',
     :                    'No overlay for device type ^TYPE', STATUS )
            GOTO 99
         ENDIF
      ENDIF

*   Ensure the GNS database is open
      CALL GNS_1INITG( STATUS )
      IF ( STATUS .NE. 0 ) GOTO 99

*   Look through the device names for a match with this type
      MATCH = 0
      DO 10 I = 1, NUMNAM
         IF ( OTYPE .EQ. ITYPES( I ) ) THEN
            NAME = NAMES( I )
            MATCH = MATCH + 1
         ENDIF
  10  CONTINUE

*   Check that there has been at least one match
      IF ( MATCH .LT. 1 ) THEN
         STATUS = GNS__NOVER
         CALL EMS_SETI( 'TYPE', ITYPE )
         CALL EMS_REP( 'GNS_IONG_NOVER',
     :                 'No overlay for device type ^TYPE', STATUS )
         GOTO 99

*   If there has been one or more matches return the last name found
      ELSE

*   Return the name length
         LNAME = INDEX( NAME, ' ' ) - 1
         IF ( LNAME .EQ. 0 ) LNAME = LEN( NAME )
      ENDIF

  99  CONTINUE
      END

