************************************************************************

      SUBROUTINE AGI_PDEL ( STATUS )

*+
*  Name:
*     AGI_PDEL
*
*  Purpose:
*     Delete all the pictures on the current device
*
*  Invocation:
*     CALL AGI_PDEL( STATUS )
*
*  Description:
*     Delete all the pictures (except the base picture) on the current
*     device. This routine will only execute if the current picture is
*     the base picture, otherwise no action is taken. All picture
*     identifiers associated with this device are released except for
*     the current one.
*
*  Arguments:
*     STATUS = INTEGER (Given and returned)
*        The global status.
*
*  Algorithm:
*     Check status on entry.
*     Get details of the current picture.
*     Get a locator to the current picture.
*     Read the contents of the current picture.
*     Check this picture is the base picture.
*     Delete all the elements of the workstation structure.
*     Clear the cache of pictures on this workstation.
*     Create a new base picture.
*     Get a picture identifier and store the details in the common block.
*
*  Copyright:
*     Copyright (C) 1988, 1989, 1990, 1992, 1993 Science & Engineering Research Council.
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
*        Original version.
*     Jul 1989 (NE):
*         Read database locator from common block
*     Dec 1989 (NE):
*         Added label structure and CIDIID
*     Jun 1990 (NE):
*         Added MEMID parameter
*     Aug 1990 (NE):
*         Added number of pictures
*     Oct 1990 (NE):
*         Release picture identifiers
*     Nov 1990 (NE):
*         Copy any parameter name
*     Mar 1992 (NE):
*         Use AGI__SZPAR to define parameter string length
*     Jan 1993 (NE):
*         Update the header block
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
      INCLUDE 'agi_cache'
      INCLUDE 'agi_locs'
      INCLUDE 'agi_pfree'

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL DELETE, FOUND, HFOUND, YESNO

      INTEGER CURPIC, I, IHEAD, J, MEMID, PICNUM

      REAL DEVICE( 4 ), NDC( 4 ), WORLD( 4 )

      CHARACTER * ( DAT__SZLOC ) HENLOC, PICLOC, PSTLOC, WKSLOC
      CHARACTER * ( AGI__CMAX ) COMENT
      CHARACTER * ( AGI__SZNAM ) PNAME
      CHARACTER * ( DAT__SZNAM ) WKNAME
      CHARACTER * ( AGI__SZPAR ) LPARAM
*.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Do some intialisations
         DELETE = .FALSE.
         LPARAM = ' '

*   Get the details of the current picture
         IF ( CURPID .GT. 0 ) THEN
            WKNAME = CAGIWK( CURPID )
            PICNUM = CPICNM( CURPID )
         ELSE
            STATUS = AGI__NOCUP
            CALL ERR_REP( 'AGI_PDEL_NOCUP',
     :                    'No current picture', STATUS )
            GOTO 99
         ENDIF

*   Find the locator to the current picture
         CALL AGI_1FDB( FOUND, STATUS )
         IF ( FOUND ) THEN
            WKSLOC = ' '
            CALL AGI_1FWORK( WKNAME, WKSLOC, FOUND, STATUS )
            IF ( FOUND ) THEN
               PSTLOC = ' '
               CALL AGI_1FPST( WKSLOC, PSTLOC, FOUND, STATUS )
               IF ( FOUND ) THEN
                  PICLOC = ' '
                  CALL AGI_1FPIC( PSTLOC, PICNUM, PICLOC, FOUND, STATUS)

*   Read the contents of this picture
                  IF ( FOUND ) THEN
                     CALL AGI_1RPARS( PICLOC, PNAME, COMENT, DEVICE,
     :                                NDC, WORLD, MEMID, YESNO, STATUS )
                     CALL DAT_ANNUL( PICLOC, STATUS )
                     PICLOC = ' '
                  ENDIF
                  CALL DAT_ANNUL( PSTLOC, STATUS )
                  PSTLOC = ' '
               ENDIF

*   Check if it is OK to carry on deleteing
               IF ( FOUND . AND. ( PNAME .EQ. 'BASE' ) ) THEN
                  DELETE = .TRUE.
               ELSE
                  DELETE = .FALSE.
               ENDIF

*   Recursively erase the picture structure and the pactive flag and
*   the label structure
*   Save the logical flag FOUND to indicate if the workstation is there
               IF ( DELETE ) THEN
                  CALL DAT_THERE( WKSLOC, AGI__ACNAM, YESNO, STATUS )
                  IF ( YESNO ) THEN
                     CALL DAT_ERASE( WKSLOC, AGI__ACNAM, STATUS )
                  ENDIF

                  CALL DAT_THERE( WKSLOC, AGI__PCNAM, YESNO, STATUS )
                  IF ( YESNO ) THEN
                     CALL DAT_ERASE( WKSLOC, AGI__PCNAM, STATUS )
                  ENDIF

                  CALL DAT_THERE( WKSLOC, AGI__LANAM, YESNO, STATUS )
                  IF ( YESNO ) THEN
                     CALL DAT_ERASE( WKSLOC, AGI__LANAM, STATUS )
                  ENDIF

                  CALL DAT_THERE( WKSLOC, AGI__IDNAM, YESNO, STATUS )
                  IF ( YESNO ) THEN
                     CALL DAT_ERASE( WKSLOC, AGI__IDNAM, STATUS )
                  ENDIF

*   Indicate that the database has been updated
                  FLUSH = .TRUE.

*   Clear the cache of pictures on this workstation
*   Note this does not change the fifo pointer
                  DO J = 0, NFIFO - 1
                     DO I = 0, FIFLEN - 1
                        IF ( CWKNAM( I, J ) .EQ. WKNAME ) THEN
                           FIFO( I, J ) = -1
                        ENDIF
                     ENDDO
                  ENDDO

*   Clear out the number of pictures on this workstation
                  IF ( CNUMPW .EQ. WKNAME ) THEN
                     CNUMPS = 0
                  ENDIF

*   Release all the picture identifiers that use this workstation.
                  DO I = 1, FRELEN
                     IF ( ( CAGIWK( I ) .EQ. WKNAME ) .AND.
     :                    ( I .NE. CURPID ) ) THEN

*   Remember any parameter association
                        IF ( PTNAME( I ) .NE. ' ' ) THEN
                           LPARAM = PTNAME( I )
                        ENDIF
                        CALL AGI_ANNUL( I, STATUS )
                     ENDIF
                  ENDDO

*   Increment the header block to show that the structure has been deleted
                  CALL DAT_THERE( WKSLOC, AGI__HENAM, HFOUND, STATUS )
                  IF ( HFOUND ) THEN
                     HENLOC = ' '
                     CALL DAT_FIND( WKSLOC, AGI__HENAM, HENLOC, STATUS )
                     CALL DAT_GET0I( HENLOC, IHEAD, STATUS )
                     IHEAD = IHEAD + 1
                     CALL DAT_PUT0I( HENLOC, IHEAD, STATUS )
                     CALL DAT_ANNUL( HENLOC, STATUS )
                  ENDIF
               ENDIF

               CALL DAT_ANNUL( WKSLOC, STATUS )
               WKSLOC = ' '

            ENDIF
         ENDIF

*   If the entries have been deleted then create a new base picture
         IF ( DELETE ) THEN
            CALL AGI_1PNEW( WKNAME, 'BASE', 'Base picture', DEVICE, NDC,
     :                      WORLD, MEMID, CURPIC, STATUS )

*   Update the details of the current picture
            CPICNM( CURPID ) = CURPIC
            PTNAME( CURPID ) = LPARAM
         ENDIF

      ENDIF

*   Flush HDS if database file has been updated.
*   This routine should flush HDS, otherwise if an error occurs elswhere
*   the database can be left with no pictures at all.
      IF ( FLUSH ) THEN
         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGI_PDEL +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

