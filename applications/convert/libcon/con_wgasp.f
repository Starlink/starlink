      SUBROUTINE CON_WGASP( FNAME, WIDTH, LENGTH, ARRAY, STATUS )
*+
*  Name:
*     CON_WGASP

*  Purpose:
*     Creates a GASP image file from an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_WGASP( FNAME, WIDTH, LENGTH, ARRAY, STATUS )

*  Description:
*     This routine creates a named GASP pixel file, and copies a signed
*     word two-dimensional array into it using direct access.  It also
*     creates the corresponding GASP header file.

*  Arguments:
*     FNAME = CHARACTER * ( * ) (Given)
*        The name of the GASP image to be written.  This should not have
*        any extension, as ".dat" and ".hdr" are assumed for the pixel
*        and header files respectively.
*     WIDTH = INTEGER (Given)
*        The number of columns in the input data array.
*     LENGTH = INTEGER (Given)
*        The number of rows in the input data array.
*     ARRAY( WIDTH, LENGTH )  = INTEGER*2 (Given)
*        The array containing the data to be written.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  References:
*     GASP documentation (MUD/66).

*  Keywords:
*     CONVERT, GASP

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     RAHM: Rhys Morris (STARLINK, University of Wales, Cardiff)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-JAN-1993 (RAHM):
*        Original version.
*     12-JUL-1993 (RAHM):
*        Tidied up code ready for release in CONVERT package.
*     1993 July 27 (MJC):
*        Renamed from WRITEGASP.  Removed the call to PSX_UNAME and used
*        RIO to open the GASP file.  No longer creates the header file.
*        Passed the signed-word array directly, so the PIXOUT argument
*        was removed.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) FNAME
      INTEGER WIDTH
      INTEGER LENGTH
      INTEGER*2 ARRAY( WIDTH, LENGTH )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of as character string
                                 ! excluding trailing blanks

*  Local Variables:
      CHARACTER * ( 132 ) DATFIL ! Name of GASP image file
      INTEGER EWIDTH             ! Even-numbered width
      INTEGER FD                 ! File descriptor
      CHARACTER * ( 132 ) HDRFIL ! Name of GASP header file
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter
      INTEGER IOVAL              ! IOSTAT Fortran i/o-error indicator
      INTEGER NAMLEN             ! Length of filename.
      REAL JUNK                  ! Junk data for the header
      INTEGER UNIT               ! File unit number

*.
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that the width of the image is an even number.
      EWIDTH = WIDTH - MOD( WIDTH, 2 )

*  Create the names of the data and header files by appending ".dat" and
*  ".hdr" respectively.
      NAMLEN = CHR_LEN( FNAME )
      CALL CHR_MOVE( FNAME, DATFIL )
      CALL CHR_MOVE( '.dat', DATFIL( NAMLEN+1: ) )

      CALL CHR_MOVE( FNAME, HDRFIL )
      CALL CHR_MOVE( '.hdr', HDRFIL( NAMLEN+1: ) )

*  Create the GASP header file.
*  ============================

*  Open the file.  The recordsize is 16 bytes.
      CALL RIO_OPEN( HDRFIL, 'WRITE', 'UNFORMATTED', 16, FD, STATUS )

*  Inquire the unit number of the header file.
      CALL FIO_UNIT( FD, UNIT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN

*  Close the header file.
         CALL RIO_CLOSE( FD, STATUS )
         GOTO 999
      END IF

*  Write the dimensions to the header file.  The other values are one
*  by definition.
      JUNK = 1.0
      WRITE( UNIT=UNIT, REC=1, IOSTAT=IOVAL ) REAL( EWIDTH ), JUNK,
     :                                        REAL( LENGTH ), JUNK

*  In case of an error reading the file, make a contextual error report
*  and abort.
      IF ( IOVAL .NE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_FIOER( 'MESSAGE', IOVAL )
         CALL MSG_SETC( 'HDRFIL', HDRFIL )
         CALL ERR_REP( 'CON_WGASP_FILEREAD',
     :      'Error writing the header file ^HDRFIL.  Error was: '/
     :      /'"^IOVAL".', STATUS )

*  Close the header file.
         CALL RIO_CLOSE( FD, STATUS )
         GOTO 999
      END IF

*  Close the header file.
      CALL RIO_CLOSE( FD, STATUS )

*  Open the GASP pixel file.
*  =========================

*  Open the file.
      CALL RIO_OPEN( DATFIL, 'WRITE', 'UNFORMATTED', 2 * EWIDTH, FD,
     :               STATUS )

*  Inquire the unit number of the header file.
      CALL FIO_UNIT( FD, UNIT, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN

*  Close the header file.
         CALL RIO_CLOSE( FD, STATUS )
         GOTO 999
      END IF

*  Loop for each record.
      DO 10 J = 1, LENGTH

*  Write the array to the GASP image file.
         WRITE( UNIT, REC=J, IOSTAT=IOVAL ) ( ARRAY( I, J ),
     :                                        I = 1, EWIDTH )

*  Make a contextual error report.
         IF ( IOVAL .NE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_FIOER( 'MESSAGE', IOVAL )
            CALL MSG_SETC( 'DATFIL', DATFIL )
            CALL MSG_SETI( 'REC', J )
            CALL ERR_REP( 'CON_WGASP_FILEWRITE',
     :        'Error writing the GASP image file ^DATFIL at record '/
     :        /'^REC.  Error was: "^IOVAL".', STATUS )
            CALL RIO_CLOSE( FD, STATUS )
            GOTO 999
         END IF

   10 CONTINUE

*  Close the data file.
      CALL RIO_CLOSE( FD, STATUS )

 999  CONTINUE

      END

