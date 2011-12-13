************************************************************************

      SUBROUTINE AGI_1FETCU ( WKNAME, AGINAM, CURPIC, STATUS )

*+
*  Name:
*     AGI_1FETCU

*  Purpose:
*     Fetch the current picture from the database.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1FETCU( WKNAME, AGINAM, CURPIC, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Fetch the current picture from the database. If there is no
*     current picture then a base picture is created. The size of the
*     base picture is checked against the expected size of the display
*     and if they are different the database is cleared and a new base
*     picture is created

*  Algorithm:
*     Check status on entry.
*     Look in the database for the device, creating a new entry if needed.
*     Look to see if there is a current picture.
*     If the size of the base picture is different from that expected for
*     this device delete all pictures in the database and create a new
*     base picture.

*  Copyright:
*     Copyright (C) 1991, 1992, 1993 Science & Engineering Research Council.
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
*     Nick Eaton  ( DUVAD::NE )
*     {enter_new_authors_here}

*  History:
*     March 1991
*     March 1991  Check size of base picture with size of display
*     February 1992  Update current number of pictures in common block
*     Jnauary 1993  Clear out cache if header block has changed
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'agi_nam'
      INCLUDE 'AGI_PAR'


*  Global Variables:
      INCLUDE 'agi_locs'
      INCLUDE 'agi_cache'


*  Arguments Given:
*     Workstation name
      CHARACTER * ( * ) WKNAME

*     AGI name
      CHARACTER * ( * ) AGINAM


*  Arguments Returned:
*     Number of currently active picture
      INTEGER CURPIC


*  Status:
      INTEGER STATUS


*  Local Variables:
      LOGICAL FOUND, HFOUND, YESNO

      INTEGER BASMID, IHEAD, I, J, MEMID

      REAL BASDEV( 4 ), BASNDC( 4 ), BASWOR( 4 ), DEVICE( 4 ), NDC( 4 ),
     :     WORLD( 4 ), SFA

      CHARACTER * ( DAT__SZLOC ) PICLOC, PSTLOC, WKSLOC
      CHARACTER * ( DAT__SZNAM ) BASNAM
      CHARACTER * ( AGI__CMAX ) BASCOM

*.


*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Inquire the expected size of a base picture on this device
         CALL AGI_1DEFBA( WKNAME, AGINAM, DEVICE, NDC, WORLD, STATUS )

*   Look in the database for the workstation
*   If it is not there then create an entry of that name
         CALL AGI_1ODB( STATUS )
         WKSLOC = ' '
         CALL AGI_1OWORK( AGINAM, WKSLOC, STATUS )

*   Look to see if there is a current picture
         CALL AGI_1RPARI( WKSLOC, AGI__ACNAM, FOUND, CURPIC, STATUS )

*   Read the stored value of the header block
         IHEAD = -2
         CALL AGI_1RPARI( WKSLOC, AGI__HENAM, HFOUND, IHEAD, STATUS )

*   Initialise the volatile value of the header block if necessary
         IF ( IHEAD .LT. 0 ) CHEAD = -1
         IF ( CNUMPW .NE. AGINAM ) CHEAD = -1

*   Clear out the cache if the header blocks differ for this device
*   Note this does not change the fifo pointer
         IF ( CHEAD .NE. IHEAD ) THEN
            DO J = 0, NFIFO - 1
               DO I = 0, FIFLEN - 1
                  IF ( CWKNAM( I, J ) .EQ. AGINAM ) THEN
                     FIFO( I, J ) = -1
                  ENDIF
               ENDDO
            ENDDO
            CHEAD = IHEAD
         ENDIF

*   Look to see if there is a base picture
         CALL AGI_1FPST( WKSLOC, PSTLOC, FOUND, STATUS )
         IF ( FOUND ) THEN

*   Update current number of pictures in common block
            CALL DAT_SIZE( PSTLOC, CNUMPS, STATUS )
            CNUMPW = AGINAM

*   Base picture is the first picture in the database
            CALL AGI_1FPIC( PSTLOC, 1, PICLOC, FOUND, STATUS )
            IF ( FOUND ) THEN
               CALL AGI_1RPARS( PICLOC, BASNAM, BASCOM, BASDEV, BASNDC,
     :                          BASWOR, BASMID, FOUND, STATUS )
               CALL DAT_ANNUL( PICLOC, STATUS )
               PICLOC = ' '
            ENDIF
            CALL DAT_ANNUL( PSTLOC, STATUS )
            PSTLOC = ' '
         ENDIF

*   Check the size of the base picture with that expected for this
*   device allowing for a bit of error
         IF ( FOUND ) THEN
            SFA = BASDEV( 2 ) / 1000.0
            IF ( ( ABS( BASDEV( 2 ) - DEVICE( 2 ) ) .GT. SFA ) .OR.
     :           ( ABS( BASDEV( 4 ) - DEVICE( 4 ) ) .GT. SFA ) ) THEN

*   The sizes do not match so start again and inform the user
               FOUND = .FALSE.
               CALL MSG_OUT( 'AGI_SIZES',
     :                       'Display has changed size.',
     :                       STATUS )
               CALL MSG_OUT( 'AGI_ERASE',
     :                       'Deleting previous database entries.',
     :                       STATUS )

*   Recursively erase the structures
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

*   Update the header block to indicate the database has been cleared
               IF ( HFOUND ) THEN
                  IHEAD = IHEAD + 1
               ELSE
                  IHEAD = 0
               ENDIF
               CALL AGI_1WPARI( WKSLOC, AGI__HENAM, IHEAD, STATUS )

*   Indicate that the database has been updated
               FLUSH = .TRUE.
            ENDIF
         ENDIF

*   Annul the workstation locator.
         CALL DAT_ANNUL( WKSLOC, STATUS )
         WKSLOC = ' '

*   If there is no current picture then create a base picture
*   using a default memory identifier
         IF ( .NOT. FOUND ) THEN
            MEMID = 0
            CALL AGI_1PNEW( AGINAM, 'BASE', 'Base picture', DEVICE, NDC,
     :                      WORLD, MEMID, CURPIC, STATUS )
         ENDIF
      ENDIF

*   Flush HDS if database file has been updated.
*   This routine should flush HDS, otherwise if an error occurs elswhere
*   the database can be left with no pictures at all.
      IF ( FLUSH ) THEN
         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

*      print*, '+++++ AGI_1FETCU +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

