      PROGRAM FAMSMASTER
*+
*  Name:
*      FAMSMASTER

*  Purpose:
*      Test of FAMS

*  Language:
*      Starlink Fortran 77

*  Description:
*      A test of fams - run in conjunction with famsslave
*        % famsslave &
*        % famsmaster

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*      AJC: Alan Chipperfield (Starlink)
*     {enter_new_authors_here}

*  History:
*      17-NOV-1994 (AJC):
*         Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADAM_DEFNS'
      INCLUDE 'MESSYS_LEN'
      INCLUDE 'MESSYS_PAR'

      INTEGER OUTMSG_STATUS
      INTEGER OUTMSG_FUNCTION
      INTEGER OUTMSG_CONTEXT
      INTEGER OUTMSG_LENGTH
      INTEGER INMSG_STATUS
      INTEGER INMSG_CONTEXT
      INTEGER INMSG_LENGTH
      INTEGER STATUS
      INTEGER PATH
      INTEGER MESSID
      INTEGER J
      CHARACTER*(MSG_NAME_LEN) SLAVE_NAME
      CHARACTER*32 OUTMSG_NAME
      CHARACTER*(MSG_VAL_LEN) OUTMSG_VALUE
      CHARACTER*32 INMSG_NAME
      CHARACTER*(MSG_VAL_LEN) INMSG_VALUE

      STATUS = 0
      SLAVE_NAME = 'slave'
      OUTMSG_STATUS = SAI__OK
      OUTMSG_FUNCTION = MESSYS__MESSAGE
      OUTMSG_CONTEXT = OBEY
      OUTMSG_LENGTH = 16

      OUTMSG_NAME = 'junk'
      OUTMSG_VALUE = 'master calling'

      CALL FAMS_INIT( 'master', STATUS )

      IF ( STATUS .NE. SAI__OK )
     :   PRINT *, 'master - bad status after FAMS_INIT'

      CALL FAMS_PATH ( SLAVE_NAME, PATH, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         PRINT *, 'master - bad status after FAMS_PATH'

      ELSE
         PRINT *, 'master - path set up ok'

      END IF

      DO J = 1, 1000
         CALL FAMS_SEND ( PATH, OUTMSG_FUNCTION, OUTMSG_STATUS,
     :    OUTMSG_CONTEXT, OUTMSG_NAME, OUTMSG_LENGTH, OUTMSG_VALUE,
     :    MESSID, STATUS )

         CALL FAMS_GETREPLY ( MESSYS__INFINITE, PATH, MESSID,
     :    INMSG_STATUS, INMSG_CONTEXT, INMSG_NAME, INMSG_LENGTH,
     :    INMSG_VALUE, STATUS )

         CALL FAMS_GETREPLY ( MESSYS__INFINITE, PATH, MESSID,
     :    INMSG_STATUS, INMSG_CONTEXT, INMSG_NAME, INMSG_LENGTH,
     :    INMSG_VALUE, STATUS )
      END DO

      IF ( STATUS .NE. SAI__OK ) THEN
         PRINT *, 'master: bad status = ', STATUS

      ELSE
         PRINT *, 'master: received - ', INMSG_VALUE(1:INMSG_LENGTH)

      END IF

      END
