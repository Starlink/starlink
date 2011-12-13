      PROGRAM FAMSSLAVE
*+
*  Name:
*     famsslave

*  Purpose:
*     Test of FAMS

*  Language:
*     Starlink Fortran 77

*  Description:
*     A test of fams - run in conjunction with famsmaster
*       % famsslave &
*       % famsmaster

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
      INCLUDE 'DTASK_ERR'

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
      CHARACTER*32 OUTMSG_NAME
      CHARACTER*(MSG_VAL_LEN) OUTMSG_VALUE
      CHARACTER*32 INMSG_NAME
      CHARACTER*(MSG_VAL_LEN) INMSG_VALUE

      STATUS = 0
      OUTMSG_STATUS = SAI__OK
      OUTMSG_FUNCTION = MESSYS__MESSAGE
      OUTMSG_CONTEXT = OBEY
      OUTMSG_LENGTH = 16

      OUTMSG_NAME = 'junk'
      OUTMSG_VALUE = 'slave replying'

      CALL FAMS_INIT ( 'slave', STATUS )
      IF ( STATUS .NE. 0 ) THEN
        PRINT *, 'slave: failed FAMS_INIT - ', STATUS

      ELSE
         DO J = 1, 1000
            CALL FAMS_RECEIVE ( MESSYS__INFINITE, INMSG_STATUS,
     :       INMSG_CONTEXT, INMSG_NAME, INMSG_LENGTH,
     :       INMSG_VALUE, PATH, MESSID, STATUS )

            OUTMSG_STATUS = DTASK__ACTSTART

            CALL FAMS_REPLY ( PATH, MESSID, OUTMSG_FUNCTION,
     :       OUTMSG_STATUS, OUTMSG_CONTEXT, OUTMSG_NAME, OUTMSG_LENGTH,
     :       OUTMSG_VALUE, STATUS )

            OUTMSG_STATUS = SAI__OK

            CALL FAMS_REPLY ( PATH, MESSID, OUTMSG_FUNCTION,
     :       OUTMSG_STATUS, OUTMSG_CONTEXT, OUTMSG_NAME, OUTMSG_LENGTH,
     :       OUTMSG_VALUE, STATUS )

         END DO

      END IF

      IF ( STATUS .NE. 0 ) THEN
         PRINT *, 'slave: bad status = ', STATUS
      ELSE
         PRINT *, 'slave: received - ', INMSG_VALUE(1:INMSG_LENGTH)
      END IF

      END
