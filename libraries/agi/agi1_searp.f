************************************************************************

      SUBROUTINE AGI_1SEARP( WKNAME, PNAME, PSTART, DIRECN, PICNUM,
     :                       ONAME, COMENT, DEVICE, NDC, WORLD, MEMID,
     :                       STATUS )


*+
*  Name:
*     AGI_1SEARP

*  Purpose:
*     Search the picture array for a picture.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1SEARP( WKNAME, PNAME, PSTART, DIRECN, PICNUM, ONAME,

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This searches through the database looking for a picture of the
*     given name. The starting point and direction of the search are
*     defined by PSTART and DIRECN. If a picture of the correct name
*     is found then the picture number is returned. The contents of
*     the picture are also returned.

*  Algorithm:
*     Check status on entry.
*     Convert the name into uppercase and remove leading blanks.
*     Test for an empty name string.
*     Inquire the number of pictures on the workstation from the cache
*     or the database.
*     Examine PSTART to determine where to start the search.
*     Examine DIRECN to determine the direction of the search.
*     Do while picture counter is valid and have not found a picture.
*        First look in the cache for the required picture.
*        or get a locator to the picture specified by the counter.
*           read the parameters from the picture entry.
*        If the parameter name equals the given name or the name string
*           was empty then
*           Indicate that a picture has been found.
*        Increment or decrement the picture counter.
*     Enddo
*     If no picture has been found with the given name then
*        Signify an error.
*        Terminate the routine.
*     Else
*        Indicate the correct picture number.
*     Endif
*     Tidy up

*  Copyright:
*     Copyright (C) 1988, 1989, 1990, 1993 Science & Engineering Research Council.
*     Copyright (C) 2005, 2006 Particle Physics & Astronomy Research Council.
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
*     Peter W. Draper ( DUVAD::PWD )
*     David S. Berry (dsb@ast.man.ac,uk)
*     {enter_new_authors_here}

*  History:
*     Jul 1988
*     Jun 1989  Allow for search of cache
*     Jul 1989  Read database locator from common block
*     Nov 1989  Amended PNAME to remove leading blanks
*     Jun 1990  Added MEMID parameter
*     Aug 1990  Added number of pictures
*     Jan 1993  Initialise header block if necessary
*     Jul 2005  Stop nasty jump "GOTO 10" into IF/ELSE/ENDIF block.
*        Compilers rightly stop accepting this.
*     Feb 2006  Initialise FOUND to avoid valgrind complaining
*        Initialise TPNAME to blank to avoid valgrind complaining
*        and use CHR_LEN for robustness.
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
      INCLUDE 'AGI_PAR'
      INCLUDE 'AGI_ERR'


*  Arguments Given:
*     Name of workstation
      CHARACTER * ( * ) WKNAME

*     Name of picture to search for
      CHARACTER * ( * ) PNAME

*     Indicator of where to start the search
      CHARACTER PSTART

*     Indicator of direction of search
      CHARACTER DIRECN


*  Arguments Given and Returned:
*     Import - Number of picture to start search if relevant
*     Export - Number of picture with given name
      INTEGER PICNUM


*  Arguments Returned:
*     Name of picture found from search
      CHARACTER * ( * ) ONAME

*     Description of picture
      CHARACTER * ( * ) COMENT

*     Device coordinates of picture
      REAL DEVICE( 4 )

*     Normalised device coordinates of picture
      REAL NDC( 4 )

*     World coordinates of picture
      REAL WORLD( 4 )

*     Memory identifier
      INTEGER MEMID


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'agi_cache'


*  External References:
      INTEGER CHR_LEN


*  Local Variables:
      LOGICAL EMPTY, FOUND, GOTONE, PFOUND, WKSHUT, YESNO
      LOGICAL NASTY

      INTEGER CLEN, I, ID, II, J, JJ, K, POINT, TOTNUM

      CHARACTER * ( DAT__SZLOC ) PICLOC, PSTLOC, WKSLOC
      CHARACTER * ( AGI__SZNAM ) TPNAME
      CHARACTER * ( AGI__CMAX ) PVAL

*.


      IF ( STATUS .EQ. SAI__OK ) THEN

*   Indicate we have not yet found anything.
         FOUND = .FALSE.

*   Copy the name string to a local variable
*   Convert the name to uppercase and remove leading blanks
         TPNAME = ' '
         TPNAME = PNAME
         CALL CHR_LDBLK( TPNAME )
         CALL CHR_UCASE( TPNAME )

*   Find out the length of the modified name string
         CLEN = CHR_LEN( TPNAME )

         IF ( CLEN .LT. 1 ) THEN
            EMPTY = .TRUE.
         ELSE
            EMPTY = .FALSE.
         ENDIF

*   Indicate the database is shut
         WKSHUT = .TRUE.

*   Find out how many pictures are in the database entry
*   Look in the cache first
         WKSLOC = ' '
         PSTLOC = ' '
         IF ( ( CNUMPW .EQ. WKNAME ) .AND. ( CNUMPS .GT. 0 ) ) THEN
            TOTNUM = CNUMPS

*   First have to get locators to database and workstation
         ELSE
            CALL AGI_1FDB( FOUND, STATUS )
            IF ( FOUND ) THEN
               CALL AGI_1FWORK( WKNAME, WKSLOC, FOUND, STATUS )
               IF ( FOUND ) THEN
                  CALL AGI_1IPIC( WKSLOC, PSTLOC, TOTNUM, FOUND,
     :                            STATUS )
               ENDIF
            ENDIF

*   Set error flag if none of the structures was found
            IF ( .NOT. FOUND ) THEN
               STATUS = AGI__NOPIC
               CALL ERR_REP( 'AGI_1SEARP_NPIC',
     :                       'No pictures in workstation', STATUS )
               GOTO 99

*   Otherwise remember the number of pictures on this device
            ELSE
               IF ( CNUMPW .NE. WKNAME ) CHEAD = -1
               CNUMPW = WKNAME
               CNUMPS = TOTNUM
               WKSHUT = .FALSE.
            ENDIF
         ENDIF

*   Use pstart to indicate where to start search
*   F = start from first picture
*   L = start from last picture
*   P = start from picture specified by PICNUM
         IF ( ( PSTART .EQ. 'F' ) .OR. ( PSTART .EQ. 'f' ) ) THEN
            I = 1
         ELSEIF ( ( PSTART .EQ. 'L' ) .OR. ( PSTART .EQ. 'l' ) ) THEN
            I = TOTNUM
         ELSEIF ( ( PSTART .EQ. 'P' ) .OR. ( PSTART .EQ. 'p' ) ) THEN
            I = PICNUM
         ELSE
            STATUS = SAI__ERROR
         ENDIF

*   Use direcn to indicate direction of search
*   F = forwards, increasing picture number
*   B = backwards, decreasing picture number
         IF ( ( DIRECN .EQ. 'F' ) .OR. ( DIRECN .EQ. 'f' ) ) THEN
            ID = 1
         ELSEIF ( ( DIRECN .EQ. 'B' ) .OR. ( DIRECN .EQ. 'b' ) ) THEN
            ID = -1
         ELSE
            STATUS = SAI__ERROR
         ENDIF

*   Step through the pictures to find one of the correct name
*   Use flag gotone to indicate if the correct name has been found
         GOTONE = .FALSE.
         DO WHILE ( ( I .GE. 1 ) .AND. ( I .LE. TOTNUM ) .AND.
     :              ( .NOT. GOTONE ) )
            NASTY = .FALSE.

*   Look in the cache first
*   Obtain the FIFO number by hashing the picture number
            K = MOD( I, NFIFO )
            DO J = FIFLEN - 1, 0, -1

*   Start at the most recent entry and work backwards
               JJ = MOD( J + PFIFO( K ) + 1, FIFLEN )

*   If the cache entry matches the given picture then read the
*   parameters and jump out of the loop
               IF ( ( FIFO( JJ, K ) .EQ. I ) .AND.
     :              ( CWKNAM( JJ, K ) .EQ. WKNAME ) ) THEN
                  PVAL = CPNAME( JJ, K )
                  COMENT = CCOM( JJ, K )
                  MEMID = CMEMID( JJ, K )
                  DO II = 1, 4
                     DEVICE( II ) = CDEV( II, JJ, K )
                     NDC( II ) = CNDC( II, JJ, K )
                     WORLD( II ) = CWORLD( II, JJ, K )
                  ENDDO

*   Note this is a bit naughty since the jump is into the scope of an
*   IF...ENDIF block. PWD: changed to use NASTY variable to control
*   jump.
                  NASTY = .TRUE.
                  GOTO 10
               ENDIF
            ENDDO

*   Find locator to picture number i
            IF ( WKSHUT ) THEN
               CALL AGI_1FDB( FOUND, STATUS )
               IF ( FOUND ) THEN
                  WKSLOC = ' '
                  CALL AGI_1FWORK( WKNAME, WKSLOC, FOUND, STATUS )
                  IF ( FOUND ) THEN
                     PSTLOC = ' '
                     CALL AGI_1IPIC( WKSLOC, PSTLOC, TOTNUM, FOUND,
     :                               STATUS )
                  ENDIF
               ENDIF

*   Set error flag if none of the structures was found
               IF ( .NOT. FOUND ) THEN
                  STATUS = AGI__NOPIC
                  CALL ERR_REP( 'AGI_1SEARP_NPIC',
     :                          'No pictures in workstation', STATUS )
                  GOTO 99

*   Otherwise remember the number of pictures on this device
               ELSE
                  IF ( CNUMPW .NE. WKNAME ) CHEAD = -1
                  CNUMPW = WKNAME
                  CNUMPS = TOTNUM
                  WKSHUT = .FALSE.
               ENDIF
            ENDIF
            PICLOC = ' '
            CALL AGI_1FPIC( PSTLOC, I, PICLOC, FOUND, STATUS )
 10         CONTINUE
            IF ( FOUND .OR. NASTY ) THEN
               IF ( .NOT. NASTY ) THEN
*   Read the name parameter from the database
                  CALL AGI_1RPARS( PICLOC, PVAL, COMENT, DEVICE, NDC,
     :                        WORLD, MEMID, PFOUND, STATUS )
                  CALL DAT_ANNUL( PICLOC, STATUS )
                  PICLOC = ' '

*   Enter this picture into the cache
                  CALL AGI_1WCACH( WKNAME, I, PVAL, COMENT, DEVICE, NDC,
     :                        WORLD, MEMID, POINT, STATUS )
               END IF

*   If it matches the input then flag it otherwise annul the locator and go on
*   Only use a sub-string the length of the name string to test for match.
*   Note. FORTRAN will extend the shorter string if they are different lengths
*   Take the first picture if it is an empty name string.
C  Stop jump to this position from GOTO 10 above. Use NASTY value to achieve
C  same effect.
C  10           CONTINUE
               IF ( EMPTY .OR. ( PVAL .EQ. TPNAME ) ) THEN
                  GOTONE = .TRUE.
               ENDIF
            ENDIF

*   Increment / decrement the picture number
            I = I + ID
         ENDDO

*   If no match was found then flag an error, otherwise carry on
         IF ( .NOT. GOTONE ) THEN
            STATUS = AGI__NONAM
            CALL MSG_SETC( 'NAME', TPNAME(:CLEN) )
            CALL ERR_REP( 'AGI_1SEARP_NNAM',
     :                    'No picture with the name ^NAME', STATUS )
            GOTO 99
         ELSE

*   Output the name of the picture found with the search
            ONAME = PVAL

*   Return the picture number. It has to be altered to its correct value
            PICNUM = I - ID

         ENDIF

         CALL DAT_VALID( PSTLOC, YESNO, STATUS )
         IF ( YESNO ) THEN
            CALL DAT_ANNUL( PSTLOC, STATUS )
            PSTLOC = ' '
         ENDIF
         CALL DAT_VALID( WKSLOC, YESNO, STATUS )
         IF ( YESNO ) THEN
            CALL DAT_ANNUL( WKSLOC, STATUS )
            WKSLOC = ' '
         ENDIF
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGI_1SEARP +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

