      SUBROUTINE CON_WGRID( BUFFER, GRIDFL, TITLE, MAPX, MAPY, MAP,
     :                      STATUS )
*+
*  Name:
*     CON_WGRID

*  Purpose:
*     Writes a schematic of a map grid to a text file.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL CON_WGRID( BUFFER, GRIDFL, TITLE, MAPX, MAPY, MAP; STATUS )

*  Description:
*     Write a schematic of a map grid to a text file.  The schematic
*     shows an asterix at grid positions where a spectrum was
*     observed.

*  Arguments:
*     BUFFER  =  CHARACTER*(*) (Given )
*        A buffer to use to hold a single line for the output file.
*        It should be large enough to hold at least (10+MAPX) characters.
*     GRIDFL  =  CHARACTER*(*) (Given)
*        Name of the file to which the schematic of the map grid is
*        to be written.
*     TITLE  =  CHARACTER*(*) (Given)
*        Title of the map grid.
*     MAPX  =  INTEGER (Given)
*        Number of points along the X-axis of the grid of observed
*        positions.
*     MAPY  =  INTEGER (Given)
*        Number of points along the X-axis of the grid of observed
*        positions.
*     MAP(MAPX, MAPY)  =  INTEGER (Given)
*        Grid of observed positions.  The value of each grid point
*        either indicates the location of the spectrum at the point
*        or is a flag indicating that no observation was made.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Attempt to open the output file.
*     If ok then
*       Write the header lines.
*       For every row
*         Assemble the row for the current line.
*         Write the current line.
*       end for
*       Write the closing lines of the grid.
*       Close the output file.
*       Report any error.
*     else
*       Report error opening the file.
*     end if

*  Copyright:
*     Copyright (C) 1997-1998, 2003 Central Laboratory of the Research
*     Councils. Copyright (C) 2005 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*     AJC: A J Chipperfield (Starlink)
*     DSB: David S Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     9/7/97  (ACD):
*        Original version.
*     28/8/97 (ACD):
*        First stable version.
*      6/2/98 (AJC):
*        Insert missing comma
*     24/2/03 (DSB):
*        Modified to use a dynamic BUFFER rather than a fixed-length
*        BUFFER.
*     14/8/05 (TIMJ):
*        'NAME' is not a portable synonym, unlike 'FILE'.
*     2009 June 29 (MJC):
*        Used modern coding style.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard Starlink constants

*  Arguments Given:
      CHARACTER*(*) BUFFER
      CHARACTER*(*) GRIDFL
      CHARACTER*(*) TITLE
      INTEGER MAPX
      INTEGER MAPY
      INTEGER MAP( MAPX, MAPY )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BUFPOS             ! Current position in the output buffer
      INTEGER COUNT              ! Running count
      INTEGER CSPEC              ! Number of the current spectrum
      INTEGER LOOPX              ! X loop index in the map grid
      INTEGER LOOPY              ! Y loop index in the map grid
      INTEGER LSTAT              ! Local Fortran I/O status
      INTEGER WRUNIT             ! Fortran unit no. for writing the file

*  Local Data:
      CHARACTER*1 DIGIT( 9 )     ! Array of digits 1 to 9
      DATA DIGIT/ '1', '2', '3', '4', '5', '6', '7', '8', '9' /
      SAVE DIGIT

*.

*  Check the global inherited status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Attempt to open the output file and proceed if ok.
         CALL FIO_GUNIT( WRUNIT, STATUS )
         OPEN( UNIT=WRUNIT, FILE=GRIDFL, STATUS='UNKNOWN',
     :         IOSTAT=LSTAT )
         CALL FIO_SERR( LSTAT, STATUS )

         IF (STATUS .EQ. SAI__OK) THEN

*  Write the header lines.  First the title.
            WRITE( WRUNIT, 2000, IOSTAT=LSTAT ) TITLE
 2000       FORMAT(1X, 'Schematic map grid for ', A // )
            CALL FIO_SERR( LSTAT, STATUS )

*  Then the top of the grid.
            BUFFER = ' '

            BUFPOS = 4
            BUFFER( 4:4 ) = '+'

            DO LOOPX = 1, MAPX
               BUFPOS = BUFPOS + 1
               BUFFER( BUFPOS:BUFPOS ) = '-'
            END DO

            BUFPOS = BUFPOS + 1
            BUFFER( BUFPOS:BUFPOS ) = '+'

            WRITE( WRUNIT, 2001, IOSTAT=LSTAT ) BUFFER(1 : BUFPOS )
 2001       FORMAT(1X, A)
            CALL FIO_SERR( LSTAT, STATUS )

*  Assemble and write the row for the current line.
            DO LOOPY = MAPY, 1, -1
               BUFFER = ' '

               WRITE( BUFFER, 2002, IOSTAT=LSTAT ) LOOPY
 2002          FORMAT(I3)
               CALL FIO_SERR( LSTAT, STATUS )

               BUFFER( 4:4 ) = '|'
               BUFPOS = 4

               DO LOOPX = 1, MAPX
                  CSPEC = MAP( LOOPX, LOOPY )
                  BUFPOS = BUFPOS + 1

                  IF ( CSPEC .GE. 1 ) THEN
                     IF ( CSPEC .LE. 9 ) THEN
                        BUFFER( BUFPOS:BUFPOS ) = DIGIT( CSPEC )
                     ELSE
                        BUFFER( BUFPOS:BUFPOS ) = '*'
                     END IF
                  ELSE
                     BUFFER( BUFPOS:BUFPOS ) = ' '
                  END IF
               END DO

               BUFPOS = BUFPOS + 1
               BUFFER( BUFPOS:BUFPOS ) = '|'

               WRITE( WRUNIT, 2001, IOSTAT=LSTAT ) BUFFER( 1:BUFPOS )
               CALL FIO_SERR( LSTAT, STATUS )

            END DO

*  Write the closing lines of the grid.
            BUFFER = ' '

            BUFPOS = 4
            BUFFER( 4:4 ) = '+'

            DO LOOPX = 1, MAPX
               BUFPOS = BUFPOS + 1
               BUFFER( BUFPOS:BUFPOS ) = '-'
            END DO

            BUFPOS = BUFPOS + 1
            BUFFER( BUFPOS:BUFPOS ) = '+'

            WRITE( WRUNIT, 2001, IOSTAT=LSTAT ) BUFFER( 1:BUFPOS )
            CALL FIO_SERR( LSTAT, STATUS )

            BUFFER = ' '
            BUFPOS = 4
            COUNT = 0

            DO LOOPX = 1, MAPX
               COUNT = COUNT + 1

               IF ( COUNT .LT. 10 ) THEN
                  CALL CHR_PUTI( COUNT, BUFFER, BUFPOS )
               ELSE
                  COUNT = 0
                  CALL CHR_PUTC( ' ', BUFFER, BUFPOS )
               END IF
            END DO

            WRITE( WRUNIT, 2001, IOSTAT=LSTAT ) BUFFER( 1:BUFPOS )
            CALL FIO_SERR( LSTAT, STATUS )

*  Close the output file.
            CLOSE( UNIT=WRUNIT )
            CALL FIO_SERR( LSTAT, STATUS )

*  Report any error.
            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_REP( 'CON_WGRID_ERR', 'Failure writing '/
     :                       /'of map grid.', STATUS )
            END IF

         ELSE

*  Report error opening the file.
            CALL ERR_REP( 'CON_WGRID_OPN', 'Unable to open a file '/
     :                    /'for the map grid schematic.', STATUS )
            END IF

      END IF

      END
