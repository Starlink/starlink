************************************************************************

      SUBROUTINE AGD_ASSOC ( PARAM, ACMODE, PNAME, MEMID, PICID, DISPID,
     :                       XSIZE, YSIZE, XOFF, YOFF, STATUS )

*+
*  Name:
*     AGD_ASSOC
*
*  Purpose:
*     Associate a device with AGI and IDI.
*
*  Invocation:
*     CALL AGD_ASSOC ( PARAM, ACMODE, PNAME, MEMID, PICID, DISPID,
*    :                 XSIZE, YSIZE, XOFF, YOFF, STATUS )
*
*  Description:
*     This is a wrap-up routine to associate a device with the AGI
*     database via the ADAM parameter system and open IDI on it.
*     The size and position of an IDI window corresponding to the
*     current picture in the database is returned. This routine calls
*     AGI_ASSOC, AGI_BEGIN, AGD_ACTIV and AGD_NWIND. Also if the name
*     string is not blank then AGI_RCL is called to recall the last
*     picture of that name. This routine should be matched by a closing
*     call to AGD_DEASS.
*
*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        The name of the ADAM parameter for accessing device names
*     ACMODE = CHARACTER*(*) (Given)
*        Access mode for pictures. 'READ', 'WRITE' or 'UPDATE'.
*     PNAME = CHARACTER*(*) (Given)
*        Recall last picture of this name if not blank.
*     MEMID = INTEGER (Given)
*        IDI Memory identifier.
*     PICID = INTEGER (Returned)
*        Picture identifier for current picture on given device.
*     DISPID = INTEGER (Returned)
*        IDI Display identifier.
*     XSIZE = INTEGER (Returned)
*        X size of window
*     YSIZE = INTEGER (Returned)
*        Y size of window
*     XOFF = INTEGER (Returned)
*        X offset of window from memory origin
*     YOFF = INTEGER (Returned)
*        Y offset of window from memory origin
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Algorithm:
*     Check status on entry.
*     Open AGI through the parameter system.
*     Begin an AGI context.
*     If the given name is not blank then use it to recall the last picture.
*     Activate the IDI interface.
*     Return a window corresponding to the current database picture.
*
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
*
*  History:
*      7-JUL-1992 (NE):
*        Original version.
*     29-JUL-1992 (NE):
*        Added PNAME argument.
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
      INTEGER MEMID

*  Arguments Returned :
      INTEGER PICID
      INTEGER DISPID
      INTEGER XSIZE
      INTEGER YSIZE
      INTEGER XOFF
      INTEGER YOFF

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
*   The overwritten picture identifier will be tidied up by AGD_DEASS.
      IF ( LNAME .NE. ' ' ) THEN
         CALL AGI_RCL( LNAME, PICID, STATUS )
      ENDIF

*   Activate IDI and obtain a window matching the current picture.
      CALL AGD_ACTIV( STATUS )
      CALL AGD_NWIND( MEMID, DISPID, XSIZE, YSIZE, XOFF, YOFF, STATUS )

  99  CONTINUE

      END

