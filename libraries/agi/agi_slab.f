************************************************************************

      SUBROUTINE AGI_SLAB ( PICID, LABEL, STATUS )

*+
*  Name:
*     AGI_SLAB
*
*  Purpose:
*     Store label in picture
*
*  Invocation:
*     CALL AGI_SLAB ( PICID, LABEL, STATUS )
*
*  Description:
*     Store a label in the picture referenced by the identifier. If the
*     picture identifier is negative then the current picture is used
*     to store the label. If a label already exists for the picture
*     then the old one is overwritten. If this label clashes with
*     another on the same device then the existing label will be
*     replaced with a blank string. For comparison purposes the label
*     string has leading blanks removed and is converted to upper case
*     before being processed, although it is stored as supplied. An
*     empty label string will delete any label stored for that picture.
*
*  Arguments:
*     PICID = INTEGER (Given)
*        Picture identifier
*     LABEL = CHARACTER*(AGI__SZLAB) (Given)
*        Label string
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Get details of the specified picture.
*     Get a locator to the label structure.
*     Remove leading blanks and convert the label string to upper case.
*     Delete any labels that match the given one.
*     Put the label in its correct place.
*
*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'agi_nam'
      INCLUDE 'AGI_PAR'
      INCLUDE 'AGI_ERR'

*  Global variables :
      INCLUDE 'agi_locs'
      INCLUDE 'agi_pfree'

*  Arguments Given :
      INTEGER PICID
      CHARACTER * ( * ) LABEL

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL FOUND

      INTEGER J, PICNUM, TOTNUM

      CHARACTER * ( DAT__SZLOC ) LABLOC, LSTLOC, WKSLOC
      CHARACTER * ( DAT__SZNAM ) WKNAME
      CHARACTER * ( AGI__SZLAB ) LABEL0, LABEL1, LABEL2
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

*   Else the picture identifier is invalid
      ELSE
         STATUS = AGI__IMPID
         CALL ERR_REP( 'AGI_SLAB_IMPID',
     :                 'Picture identifier is improper', STATUS )
         GOTO 99
      ENDIF

*   Get a locator to the label structure
      CALL AGI_1FDB( FOUND, STATUS )
      IF ( FOUND ) THEN
         WKSLOC = ' '
         CALL AGI_1FWORK( WKNAME, WKSLOC, FOUND, STATUS )
         IF ( FOUND ) THEN
            CALL DAT_THERE( WKSLOC, AGI__LANAM, FOUND, STATUS )
            IF ( FOUND ) THEN
               LSTLOC = ' '
               CALL DAT_FIND( WKSLOC, AGI__LANAM, LSTLOC, STATUS )
            ENDIF
            CALL DAT_ANNUL( WKSLOC, STATUS )
            WKSLOC = ' '
         ENDIF
      ENDIF

*   If the label structure was found then continue
      IF ( FOUND ) THEN

*   Copy the input label to a local variable and convert to upper
*   case and remove leading blanks
         LABEL0 = LABEL
         LABEL1 = LABEL0
         CALL CHR_LDBLK( LABEL1 )
         CALL CHR_UCASE( LABEL1 )

*   If the label is non-blank then check it against existing labels
         IF ( LABEL1 .NE. ' ' ) THEN

*   Find the number of labels in the structure
            CALL DAT_SIZE( LSTLOC, TOTNUM, STATUS )

*   Compare each label in turn including the intended one
            DO J = 1, TOTNUM
               LABLOC = ' '
               CALL DAT_CELL( LSTLOC, 1, J, LABLOC, STATUS )
               CALL DAT_GET0C( LABLOC, LABEL2, STATUS )

*   Remove leading blanks and change to uppercase
               CALL CHR_LDBLK( LABEL2 )
               CALL CHR_UCASE( LABEL2 )

*   Perform the comparison and delete any identical labels
               IF ( LABEL1 .EQ. LABEL2 ) THEN
                  CALL DAT_PUT0C( LABLOC, '               ', STATUS )
               ENDIF
               CALL DAT_ANNUL( LABLOC, STATUS )
               LABLOC = ' '
            ENDDO
         ENDIF

*   Store the given label in its hole
         LABLOC = ' '
         CALL DAT_CELL( LSTLOC, 1, PICNUM, LABLOC, STATUS )
         CALL DAT_PUT0C( LABLOC, LABEL0, STATUS )
         CALL DAT_ANNUL( LABLOC, STATUS )
         LABLOC = ' '
         CALL DAT_ANNUL( LSTLOC, STATUS )
         LSTLOC = ' '

*   Indicate that the database has been updated
         FLUSH = .TRUE.

*   Otherwise report an error
      ELSE
         STATUS = AGI__LABNF
         CALL ERR_REP( 'AGI_SLAB_LABNF', 'Label not found', STATUS )
         GOTO 99
      ENDIF

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGI_SLAB +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

