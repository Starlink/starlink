      SUBROUTINE GNS_IETG ( IWKID, TXT, LTXT, STATUS )

*+
*  Name:
*     GNS_IETG

*  Purpose:
*     Inquire string to erase text screen

*  Invocation:
*     CALL GNS_IETG( IWKID, TXT, LTXT, STATUS )

*  Description:
*     A character string is returned that will clear the text screen if
*     written to the specified device (normally a terminal). The string
*     may contain control characters.
*
*     If no string is available a length of zero is returned.

*  Arguments:
*     IWKID = INTEGER (Given)
*        GKS workstation identifier
*     TXT = CHARACTER*(GNS__SZTXT) (Returned)
*        Text string
*     LTXT = INTEGER (Returned)
*        Length of text string
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Side Effects:
*     The GNS database may be opened.

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
*     DLT: D.L. Terrett (STARLINK)
*     NE: Nick Eaton (Durham University)

*  History:
*      7-JUN-1988 (DLT):
*        Original version.
*      9-JUL-1990 (NE):
*        Added error reporting
*      1-SEP-1992 (NE):
*        Updated prologue.
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
      CHARACTER*(*) TXT
      INTEGER LTXT

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER IERR, ICON, ITYPE
*.

      IF (STATUS.EQ.0) THEN

*     Convert the workstation id to a type
         CALL GQWKC(IWKID,IERR,ICON,ITYPE)
         IF (IERR.EQ.0) THEN

*        Make sure that the common block contains data for this type
            CALL GNS_1RDWST(ITYPE, STATUS)

            IF (STATUS.EQ.0) THEN
               LTXT = LERTXT
               TXT = ERTXT
            ELSE
               LTXT = 0
            END IF
         ELSE
            STATUS = GNS__INWKID
            CALL EMS_REP( 'GNS_IETG_INWKID',
     :                    'Invalid GKS workstation identifier', STATUS )
         END IF
      END IF
      END

