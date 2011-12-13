      SUBROUTINE COF_HALOG( FDL, EL, HEADER, FILE, CHDU, STATUS )
*+
*  Name:
*     COF_HALOG

*  Purpose:
*     Writes the FITS headers stored in an array to a logfile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_HALOG( FDL, EL, HEADER, FILE, CHDU, STATUS )

*  Description:
*     This writes the FITS headers stored in a character array
*     to a previously opened text file.  The logfile is not closed by
*     this routine.

*  Arguments:
*     FDL = INTEGER (Given)
*        The file descriptor for the logfile.
*     EL = INTEGER (Given)
*        The number of headers to write.
*     HEADER( EL ) = CHARACTER * ( * ) (Given)
*        The array of FITS headers.
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the FITS file or tape device to appear in the
*        heading of the log.
*     CHDU = INTEGER (Given)
*        The number of the current header and data unit of the FITS
*        file, which will appear in the heading of the log.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The log file must already be opened.
*
*     [optional_subroutine_items]...

*  Copyright:
*     Copyright (C) 1996, 1998 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 June 29 (MJC):
*        Original version.
*     1998 January 6 (MJC):
*        Allow for 0 RECLEN from INQUIRE
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FDL
      INTEGER EL
      CHARACTER * ( * ) HEADER( EL )
      CHARACTER * ( * ) FILE
      INTEGER CHDU

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a string less trailing
                                 ! blanks

*  Local Constants:
      INTEGER HEDLEN             ! FITS header length
      PARAMETER( HEDLEN = 80 )

      INTEGER MAXLEN             ! Maximum logfile record length
      PARAMETER( MAXLEN = 80 )

*  Local Variables:
      CHARACTER * ( MAXLEN ) DUMMY ! Output record
      INTEGER IHEAD              ! Loop counter for headers
      INTEGER LUNIT              ! Logical unit number of logfile
      INTEGER MXFILN             ! Maximum number of characters in the
                                 ! file name that can be displayed
      INTEGER NCF                ! Number of characters in the FITS file
                                 ! name
      INTEGER NCO                ! Number of characters in the output
      INTEGER RECLEN             ! Record length of logfile

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write blank line.
      CALL FIO_WRITE( FDL, ' ', STATUS )

*  Get the record length of the file.
      CALL FIO_UNIT( FDL, LUNIT, STATUS )
      INQUIRE( UNIT = LUNIT, RECL = RECLEN )

*  On some operating systems such as Solaris the record length is
*  returned as zero.  Supply a sensible default in this case.
      IF ( RECLEN .EQ. 0 ) RECLEN = 80

*  Find the maximum number of characters that can be output in the
*  caption.
      MXFILN = MIN( MAXLEN, RECLEN ) - 30

*  Get the length of the filename.
      NCF = CHR_LEN( FILE )

*  Add ellipsis where truncated.  Write caption to buffer, truncating
*  the filename.
      IF ( NCF .GT. MXFILN ) THEN
         WRITE( DUMMY, '(''** File: ...'',A,'' ('',I3,'' ) header '/
     :     /'cards'')' ) FILE( MAX( 1, NCF - MXFILN + 3 ):NCF ), CHDU

         NCO = MXFILN + 30

*  Write the caption using the full file name.
      ELSE
         WRITE( DUMMY, '(''** File: '',A,'' ('',I3,'' ) header '/
     :     /'cards'')' ) FILE( 1:NCF ), CHDU

         NCO = NCF + 30
      END IF

      CALL FIO_WRITE( FDL, DUMMY( :NCO ), STATUS )

*  Loop through the headers.
      DO IHEAD = 1, EL

*  Print the header to the logfile.
         CALL FIO_WRITE( FDL, HEADER( IHEAD )( :HEDLEN ), STATUS )

*  Write the header to the FITS extension.
      END DO

  999 CONTINUE

      END
