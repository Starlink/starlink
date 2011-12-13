      SUBROUTINE GNS_TNI ( NAME, TYPE, DEVICE, STATUS )

*+
*  Name:
*     GNS_TNI

*  Purpose:
*     Translate name to an IDI device specification

*  Invocation:
*     CALL GNS_TNI( NAME, TYPE, DEVICE, STATUS )

*  Description:
*     The workstation name is translated to an IDI workstation type, and
*     a physical device name.

*  Arguments:
*     NAME = CHARACTER*(*) (Given)
*        Workstation name
*     TYPE = CHARACTER*(*) (Returned)
*        IDI workstation type
*     DEVICE = CHARACTER*(*) (Returned)
*        Device name
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Notes:
*     IIDOPN requires a GNS workstation name as its name argument which
*     is translated by IIDOPN calling GNS_TNI; GNS_TNI is therefore not
*     normally called by applications programs.

*  Side Effects:
*     A GWM window with an overlay may be created.

*  Copyright:
*     Copyright (C) 1989, 1990, 1992 Science & Engineering Research Council.
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
*     DLT: D.L. Terrett (STARLINK)
*     NE: Nick Eaton (Durham University)

*  History:
*     18-JUL-1989 (DLT):
*        Original version.
*      9-JUL-1990 (NE):
*        Added error reporting
*      4-AUG-1992 (NE):
*        Added name to NAMNR error report
*     21-AUG-1992 (NE):
*        Create X-window with overlay if '+' in name
*      1-SEP-1992 (NE):
*        Updated prologue.
*        Extract device from name string.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'GNS_ERR'
      INCLUDE 'GNS_PAR'
      INCLUDE 'gns.cmn'

*  Arguments Given:
      CHARACTER*(*) NAME

*  Arguments Returned:
      CHARACTER*(*) TYPE
      CHARACTER*(*) DEVICE

*  Status:
      INTEGER STATUS

*  Local Constants:
*   Maximum number of translations allowed
      INTEGER MAXTRY
      PARAMETER (MAXTRY=5)

*  Local Variables:
      CHARACTER EQNAM*64
      CHARACTER*(GNS__SZNAM + GNS__SZDEV) CNAME, CNAME1
      CHARACTER*(GNS__SZDEV) CDEV
      INTEGER LNAME, I, MATCH, LDEV, LENEQ, NTRIES, NAMLEN
      LOGICAL PLUS
*.

*   Check status on entry
      IF (STATUS.EQ.0) THEN

*   Copy input device specification
      CNAME1 = NAME

*   See if there is a '+' special character in the name
*   If there is replace it with a space
      CALL GNS_1SPECH( '+', ' ', CNAME1, PLUS )

*   Extract the device from the name string
      CALL GNS_1DECSP( CNAME1, CNAME, LNAME, CDEV, LDEV )

   10 CONTINUE

*     Try logical name translation
         NTRIES = 0
         CALL GNS_1LOGTR(CNAME,EQNAM,LENEQ,STATUS)
         IF ( ( LENEQ .GT. 0 ) .AND. ( NTRIES .LT. MAXTRY ) )THEN
            CNAME = EQNAM(:LENEQ)
            LNAME = LENEQ
            NTRIES = NTRIES + 1
            GOTO 10
         ENDIF

*     Open GNS database
         CALL gns_1INITI(STATUS)
         IF (STATUS.EQ.0) THEN

*        Convert name to IDI equivalent
            MATCH = 0
            DO 30 I = 1,NUMNAI
               IF (CNAME(:LNAME).EQ.NAMESI(I)(:LNAME)) THEN
                  TYPE = TYPESI(I)
                  DEVICE = VMSNAI(I)

                  MATCH = MATCH + 1

*              If the names match exactly then abandon any futher
*              checking and ignore any matching abbreviations.
                  IF (CNAME(:LNAME).EQ.NAMESI(I)) GO TO 40
               END IF
   30       CONTINUE

*        If only one match was found then continue with device name
*        handling
            IF (MATCH.EQ.1) THEN
               GO TO 40

*        More than one match is an error
            ELSE IF (MATCH.GT.1) THEN
               STATUS = GNS__AMBNAM
               CALL EMS_REP( 'GNS_TNI_AMBNAM',
     :                       'Ambiguous workstation name', STATUS )
               GO TO 9000
            END IF

*     Unable to decode the name (preserve the existing status if has
*     already been set)
            IF (STATUS.EQ.0) THEN
               STATUS = GNS__NAMNR
               CALL EMS_SETC( 'NAME', CNAME(:LNAME) )
               CALL EMS_REP( 'GNS_TNI_NAMNR',
     :                      'Device name ^NAME not recognised', STATUS )
            ENDIF
            GO TO 9000
         END IF

  40     CONTINUE

*     If there is a given device name then use if
         IF ( LDEV .GT. 0 ) DEVICE = CDEV

*     If the device is an X-window and there has been a special character
         IF ( PLUS .AND. ( TYPE(:2) .EQ. 'XW' ) ) THEN

*     Create a GWM window with an overlay using the device name
            NAMLEN = INDEX( DEVICE, ' ' ) - 1
            CALL GNS_1GWMOV( DEVICE, NAMLEN, STATUS )
         ENDIF

      ENDIF
 9000 CONTINUE
      END

