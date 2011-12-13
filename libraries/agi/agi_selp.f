************************************************************************

      SUBROUTINE AGI_SELP ( PICID, STATUS )

*+
*  Name:
*     AGI_SELP
*
*  Purpose:
*     Select the given picture as the current one
*
*  Invocation:
*     CALL AGI_SELP( PICID, STATUS )
*
*  Description:
*     Select the given picture as the current one.
*
*  Arguments:
*     PICID = INTEGER (Given)
*        Picture identifier
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Verify the picture identifier.
*     Save the identifier in the common block and in the database.
*     Release any previously deferred pictures.
*
*  Copyright:
*     Copyright (C) 1988, 1990 Science & Engineering Research Council.
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
*     Oct 1990 (NE):
*        Added deferred picture release
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'

*  Global variables :
      INCLUDE 'agi_locs'
      INCLUDE 'agi_pfree'

*  Arguments Given :
      INTEGER PICID

*  Status :
      INTEGER STATUS

*  Local variables :
      INTEGER I, LSTAT
*.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Verify the picture identifier
         IF ( ( PICID .GE. 1 ) .AND. ( PICID .LE. FRELEN ) ) THEN

*   Store the new ID in the COMMON block
            CURPID = PICID

*   Store the new ID in the database
            CALL AGI_1WPACT( CAGIWK( CURPID ), CPICNM( CURPID ), STATUS)

*   Release any deferred pictures
            IF ( CISDEP ) THEN
               CISDEP = .FALSE.

*   Check each picture in turn
               DO I = 1, FRELEN

*   Do not release the picture if it is still current
                  IF ( ( CDEPS( I ) .NE. 0 ) .AND.
     :                 ( I .NE. CURPID ) ) THEN
                     LSTAT = SAI__OK
                     CALL AGI_1FRETN( FRELEN, I, FRELIS, NEXFRE, LSTAT )
                     IF ( LSTAT .EQ. SAI__OK ) THEN
                        CGRAWK( I ) = ' '
                        CAGIWK( I ) = ' '
                        CPICNM( I ) = 0
                        PTNAME( I ) = ' '
                        PICACS( I ) = ' '
                        CIDIID( I ) = 0
                        CLEVEL( I ) = 0
                        CDEPS( I ) = 0
                        CNUMID = CNUMID - 1
                     ENDIF
                  ENDIF

*   Indicate if there is still a deferred picture
                  IF ( CDEPS( I ) .NE. 0 ) THEN
                     CISDEP = .TRUE.
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      ENDIF

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

*      print*, '+++++ AGI_SELP +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

