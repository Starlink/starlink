************************************************************************

      SUBROUTINE AGS_ASSOC ( PARAM, ACMODE, PNAME, PICID, NEWZON,
     :                       STATUS )

*+
*  Name:
*     AGS_ASSOC
*
*  Purpose:
*     Associate a device with AGI and SGS.
*
*  Invocation:
*     CALL AGS_ASSOC( PARAM, ACMODE, PNAME, PICID, NEWZON, STATUS )
*
*  Description:
*     This is a wrap-up routine to associate a device with the AGI
*     database via the ADAM parameter system and open SGS on it. An
*     SGS zone corresponding to the current picture in the database
*     is created. This routine calls AGI_ASSOC, AGI_BEGIN, AGS_ACTIV
*     and AGS_NZONE. Also if the name string is not blank then AGI_RCL
*     is called to recall the last picture of that name. This routine
*     should be matched by a closing call to AGS_DEASS.
*
*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        The name of the ADAM parameter for accessing device names
*     ACMODE = CHARACTER*(*) (Given)
*        Access mode for pictures. 'READ', 'WRITE' or 'UPDATE'.
*     PNAME = CHARACTER*(*) (Given)
*        Recall last picture of this name if not blank.
*     PICID = INTEGER (Returned)
*        Picture identifier for current picture on given device.
*     NEWZON = INTEGER (Returned)
*        The new SGS zone that matches the current picture.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Algorithm:
*     Check status on entry.
*     Open AGI through the parameter system.
*     Begin an AGI context.
*     If the given name is not blank then use it to recall the last picture.
*     Activate the SGS interface.
*     Return a new zone corresponding to the current database picture.
*
*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     10-MAR-1991 (MJC):
*        Original version.
*      6-JUL-1992 (NE):
*        Convert to AGI style.
*     29-JUL-1992 (NE):
*        Added PNAME argument
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Arguments Given :
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) ACMODE
      CHARACTER * ( * ) PNAME

*  Arguments Returned :
      INTEGER PICID
      INTEGER NEWZON

*  Status :
      INTEGER STATUS

*  Local variables :
      CHARACTER LNAME * 64
*.

*   Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Associate the parameter with a workstation and current picture.
      CALL AGI_ASSOC( PARAM, ACMODE, PICID, STATUS )

*   Start a new AGI context.
      CALL AGI_BEGIN

*   Copy given picture name to a local variable and remove leading blanks.
      LNAME = PNAME
      CALL CHR_LDBLK( LNAME )

*   If the given name is not blank then use it to recall the last picture.
*   The overwritten picture identifier will be tidied up by AGS_DEASS.
      IF ( LNAME .NE. ' ' ) THEN
         CALL AGI_RCL( LNAME, PICID, STATUS )
      ENDIF

*   Ensure that SGS is in "inherited status" mode
      CALL SGS_ISTAT(1, STATUS)

*   Activate SGS and get a zone.
      CALL AGS_ACTIV( STATUS )
      CALL AGS_NZONE( NEWZON, STATUS )

  99  CONTINUE

      END

