      SUBROUTINE CON_RGASP( FNAME, WIDTH, LENGTH, ARRAY, STATUS )
*+
*  Name:
*     CON_RGASP

*  Purpose:
*     Reads the contents of a GASP image and stores them in an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_RGASP( FNAME, WIDTH, LENGTH, ARRAY, STATUS )

*  Description:
*     This routine reads a named GASP file using direct access, and
*     stores the image data in a two-dimensional array.

*  Arguments:
*     FNAME = CHARACTER * ( * ) (Given)
*        The name of the image to to be read.  This should not have
*        any extension, as ".dat" is assumed.
*     WIDTH = INTEGER (Given)
*        The number of columns in the GASP data.
*     LENGTH = INTEGER (Given)
*        The number of rows in the GASP data.
*     ARRAY( WIDTH, LENGTH ) = INTEGER*2 (Returned)
*        The array to hold the GASP data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  References:
*     GASP documentation (MUD/66).

*  Keywords:
*     CONVERT, GASP

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     AJC: A J Chipperfield (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     18-JAN-1993 (RAHM):
*        Original version.
*     12-JUL-1993 (RAHM):
*        Tidies code up ready for release with CONVERT package.
*     1993 July 27 (MJC):
*        Renamed from READGASP.  Removed the call to PSX_UNAME and used
*        RIO to open the GASP file
*     21-NOV-1995 (AJC):
*        Correct token name MESSAGE to IOVAL.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER WIDTH
      INTEGER LENGTH

      CHARACTER FNAME * ( * )

*  Arguments Returned:
      INTEGER*2 ARRAY( WIDTH, LENGTH )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of as character string
                                 ! excluding trailing blanks

*  Local Variables:
      CHARACTER * ( 132 ) DATFIL ! Name of GASP image file
      INTEGER FD                 ! File descriptor
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter
      INTEGER IOVAL              ! IOSTAT Fortran i/o-error indicator
      INTEGER NAMLEN             ! Length of filename.
      INTEGER UNIT               ! File unit number

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct a filename for the data file.
      NAMLEN =  CHR_LEN( FNAME )
      CALL CHR_MOVE( FNAME, DATFIL )
      CALL CHR_MOVE( '.dat', DATFIL( NAMLEN+1: ) )

*  Try to open the GASP data file.  Since the file is opened for read
*  access, the recordsize should not need to be specified (set to zero),
*  however in practice...
      CALL RIO_OPEN( DATFIL, 'READ', 'UNFORMATTED', 2 * WIDTH, FD,
     :               STATUS )

*  Inquire the unit number of the data file.
      CALL FIO_UNIT( FD, UNIT, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Fill the array.
      DO 10 J = 1, LENGTH
         READ( UNIT, REC=J, IOSTAT=IOVAL ) ( ARRAY( I, J ),
     :                                       I = 1, WIDTH )

*  Make a contextual error report.
         IF ( IOVAL .NE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_FIOER( 'IOVAL', IOVAL )
            CALL MSG_SETC( 'DATFIL', DATFIL )
            CALL MSG_SETI( 'REC', J )
            CALL ERR_REP( 'CON_RGASP_FILEREAD',
     :        'Error reading the GASP image file ^DATFIL in record '/
     :        /'^REC.  Error was: "^IOVAL".', STATUS )
            CALL RIO_CLOSE( FD, STATUS )
            GOTO 999
         END IF
   10 CONTINUE

*  Close the data file.
      CALL RIO_CLOSE( FD, STATUS )

  999 CONTINUE

      END
