************************************************************************

      SUBROUTINE AGI_1FNAME ( FNAME, LNAME, STATUS )

*+
*  Name:
*     AGI_1FNAME
*
*  Purpose:
*     Return the name of the database file
*
*  Invocation:
*     CALL AGI_1FNAME( FNAME, LNAME, STATUS )
*
*  Description:
*     Return the name of the database file. The file extension is
*     not included in the name. At present the name contains the
*     current node name obtained through a POSIX call, unless the
*     environment variable AGI_NODE is defined in which case the
*     equivalent string is inserted in place of the node name.
*     This routine can be changed by any supporter of AGI to return
*     a suitable file name.
*
*  Arguments:
*     FNAME = CHARACTER*(*) (Returned)
*        File name
*     LNAME = INTEGER (Returned)
*        Length of name string
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Get directory name from AGI_USER if defined otherwise from HOME
*     Get the node name from AGI_NODE if defined otherwise from system
*     Construct the full file name
*
*  Copyright:
*     Copyright (C) 1990, 1991, 1992 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (RAL)
*
*  History:
*     Jul 1990 (NE):
*         Original version
*     Dec 1991 (NE):
*         Get node name from POSIX
*     Mar 1992 (DLT):
*         Translate logical names (environment variables) explicitly.
*         Try HOME if AGI_USER will not translate.
*         Use first component of node name only.
*     Jul 1992 (NE):
*         Get node name from AGI_NODE if defined
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'

*  Arguments Returned :
      CHARACTER * ( * ) FNAME
      INTEGER LNAME

*  Status :
      INTEGER STATUS

*  Local variables :
      CHARACTER * 64 MSTR, NSTR, RSTR, SSTR, TSTR, VSTR

      INTEGER NLEN
*-

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Mark a new error context to trap errors from PSX
         CALL ERR_MARK

*   Try translating the default name
         CALL PSX_GETENV( 'AGI_USER', TSTR, STATUS )

*   If there is no translation then use HOME instead
         IF ( STATUS .NE. SAI__OK ) THEN
             CALL ERR_ANNUL( STATUS )
             CALL PSX_GETENV( 'HOME', TSTR, STATUS )
         ENDIF

         IF ( STATUS .EQ. SAI__OK ) THEN
             LNAME = INDEX( TSTR, ' ' ) - 1
         ELSE
             TSTR = ' '
             LNAME = 1
             CALL ERR_ANNUL( STATUS )
         ENDIF

*   See if the logical name AGI_NODE is defined
         CALL PSX_GETENV( 'AGI_NODE', NSTR, STATUS )

*   If this has failed then reset the node string and status
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            NSTR = ' '
         ENDIF

*   If the logical name is undefined then get the node name from the system
         IF ( NSTR .EQ. ' ' ) THEN
            CALL PSX_UNAME( SSTR, NSTR, RSTR, VSTR, MSTR, STATUS )

*   If this has failed give the node a default name
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
               NSTR = 'nonode'
            ENDIF
         ENDIF

*   Get the length of the node string (up to the first .)
         NLEN = INDEX( NSTR, '.' ) - 1
         IF ( NLEN .LE. 0 ) NLEN = INDEX( NSTR, ' ' ) - 1

*   Construct full file name
         FNAME = TSTR( 1 : LNAME ) // '/agi_' // NSTR( 1 : NLEN )
         LNAME = LNAME + NLEN + 5

*   End the error context
         CALL ERR_RLSE
      ENDIF

  99  CONTINUE

      END

