************************************************************************

      SUBROUTINE AGI_ILAB ( PICID, LABEL, STATUS )

*+
*  Name:
*     AGI_ILAB
*
*  Purpose:
*     Inquire label of a picture
*
*  Invocation:
*     CALL AGI_ILAB ( PICID, LABEL, STATUS )
*
*  Description:
*     Inquire the label of a picture referenced by the identifier. If
*     the picture identifier is negative then the current picture is
*     searched. If no label is associated with this picture then a
*     blank string is returned.
*
*  Arguments:
*     PICID = INTEGER (Given)
*        Picture identifier
*     LABEL = CHARACTER*(AGI__SZLAB) (Returned)
*        Label string
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Get details of the specified picture.
*     Get a locator to the label structure.
*     If the correct label element was found then return its contents
*     Otherwise return a blank string.
*
*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     December 1989 (NE):
*        Original version
*     November 1990 (NE):
*        Return blank string if not found
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'agi_nam'
      INCLUDE 'AGI_PAR'

*  Global variables :
      INCLUDE 'agi_locs'
      INCLUDE 'agi_pfree'

*  Arguments Given :
      INTEGER PICID

*  Arguments Returned :
      CHARACTER * ( * ) LABEL

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL FOUND

      INTEGER PICNUM, TOTNUM

      CHARACTER * ( DAT__SZLOC ) LABLOC, LSTLOC, WKSLOC
      CHARACTER * ( AGI__SZLAB ) LABEL1
      CHARACTER * ( DAT__SZNAM ) WKNAME
*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Obtain the workstation and picture number from the picture id
*   If PICID is less than 0 then use the current picture
      IF ( PICID .LT. 0 ) THEN
         WKNAME = CAGIWK( CURPID )
         PICNUM = CPICNM( CURPID )

*   Otherwise use the specified picture
      ELSEIF ( ( PICID .GT. 0 ) .AND. ( PICID .LE. FRELEN ) ) THEN
         WKNAME = CAGIWK( PICID )
         PICNUM = CPICNM( PICID )

*   Else the picture identifier is invalid so return blank string
      ELSE
         LABEL = ' '
         GOTO 99
      ENDIF

*   Get a locator to the label
      CALL AGI_1FDB( FOUND, STATUS )
      IF ( FOUND ) THEN
         WKSLOC = ' '
         CALL AGI_1FWORK( WKNAME, WKSLOC, FOUND, STATUS )
         IF ( FOUND ) THEN
            CALL DAT_THERE( WKSLOC, AGI__LANAM, FOUND, STATUS )
            IF ( FOUND ) THEN
               LSTLOC = ' '
               CALL DAT_FIND( WKSLOC, AGI__LANAM, LSTLOC, STATUS )
               CALL DAT_SIZE( LSTLOC, TOTNUM, STATUS )
               IF ( ( PICNUM .GE. 1 ) .AND.
     :              ( PICNUM .LE. TOTNUM ) ) THEN
                  FOUND = .TRUE.
                  LABLOC = ' '
                  CALL DAT_CELL( LSTLOC, 1, PICNUM, LABLOC, STATUS )
               ELSE
                  FOUND = .FALSE.
               ENDIF
               CALL DAT_ANNUL( LSTLOC, STATUS )
               LSTLOC = ' '
            ENDIF
            CALL DAT_ANNUL( WKSLOC, STATUS )
            WKSLOC = ' '
         ENDIF
      ENDIF

*   If the label was found then continue
      IF ( FOUND ) THEN

*   Read the contents of the cell
         CALL DAT_GET0C( LABLOC, LABEL1, STATUS )
         CALL DAT_ANNUL( LABLOC, STATUS )
         LABLOC = ' '

*   Copy the string to the output variable
         LABEL = LABEL1

*   Otherwise return a blank string
      ELSE
         LABEL = ' '
      ENDIF

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGI_ILAB +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

