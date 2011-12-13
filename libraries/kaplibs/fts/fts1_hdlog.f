      SUBROUTINE FTS1_HDLOG( HEADER, FD, CFN, SUBFIL, NHEADS, HDNUM,
     :                       STATUS )
*+
*  Name:
*     FTS1_HDLOG

*  Purpose:
*     Outputs the FITS header cards to an ASCII file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_HDLOG( HEADER, FD, CFN, SUBFIL, NHEADS, HDNUM, STATUS )

*  Description:
*     This is a server routine for FITSIN.  It packages up the
*     operations required to write the FITS header cards (80-character)
*     into an ASCII file.  A heading is also written giving the
*     file and sub-file numbers.  The maximum record length of the
*     ASCII file supported by this routine is 132 characters.  The
*     minimum is 80 characters---an error is reported if it is smaller
*     than 80.

*     The file name appears in the caption before the headers.  It may
*     be truncated if it is longer than the file recordsize less 30
*     characters.  Truncation occurs at the start of the name and is
*     designated via an ellipsis.

*  Arguments:
*     HEADER( * ) = CHARACTER * 80 (Given)
*        The FITS headers in 80-character records.
*     FD = INTEGER (Given)
*        The file descriptor for the log file.
*     CFN = CHARACTER * ( * ) (Given)
*        The input filename or tape file number of the FITS file being
*        processed.
*     SUBFIL = INTEGER (Given)
*        The number of the sub-file/extension within the current FITS
*        file being processed.
*     NHEADS = INTEGER (Given)
*        The number of header sections in the sub-file.  This includes
*        dummy FITS header sections.
*     HDNUM( * ) = INTEGER (Given)
*        The number of header cards within each header in the sub-file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  The ASCII file must be opened.

*  Copyright:
*     Copyright (C) 1990, 1991, 1992 Science & Engineering Research
*                   Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     RDS: Richard D. Saxton (STARLINK, Leicester)
*     {enter_new_authors_here}

*  History:
*     1990 November 18 (MJC):
*        Original version.
*     1991 September 9 (MJC):
*        Corrected bug connected with ASCII record length.  Added
*        leading ellipsis if it is truncated.
*     1992 December (RDS):
*        Modified argument order and made one of the error checks VMS
*        only.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER
     :  FD,
     :  NHEADS,
     :  HDNUM( * ),
     :  SUBFIL

      CHARACTER * ( * )
     :  CFN,
     :  HEADER( * ) * 80

      CHARACTER
     :  MACHIN * ( 24 ),       ! Machine name
     :  NODE * ( 20 ),         ! Node name
     :  RELEAS * ( 10 ),       ! Release of operating system
     :  SYSNAM * ( 10 ),       ! Operating system
     :  VERSIO * ( 10 )        ! Sub-version of operating system

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER
     :  CHR_LEN                ! Number characters excluding trailing
                               ! blanks

*  Local Variables:
      INTEGER
     :  I, J, K,                 ! Loop counters
     :  LUNIT,                   ! Logical unit number
     :  MXFILN,                  ! Maximum number of characters output
     :  NCF,                     ! Number of characters in file name or
                                 ! number
     :  RECLEN                   ! Record length

      CHARACTER
     :  DUMMY * 132,             ! Buffer for output of message
     :  FNAME * 132              ! File name

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get the length of the filename.

      NCF = CHR_LEN( CFN )

*    Get the record length of the file.

      CALL FIO_UNIT( FD, LUNIT, STATUS )
      INQUIRE ( UNIT = LUNIT, RECL = RECLEN )

*    Obtain the operating system.

      CALL PSX_UNAME( SYSNAM, NODE, RELEAS, VERSIO, MACHIN, STATUS )
      CALL CHR_UCASE( SYSNAM )

*    Check that the headers can be written to the file (for VMS only).
*    This cannot be done on UNIX because the record length is zero.

      IF ( INDEX( SYSNAM, 'VMS') .NE. 0 .OR.
     :     INDEX( SYSNAM, 'RSX') .NE. 0 ) THEN
         IF ( RECLEN .LT. 80 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'CFN', CFN )
            CALL MSG_SETI( 'RL', RECLEN )
            CALL ERR_REP( 'FTS1_HDLOG_RECL',
     :        'File ^CFN has recordlength ^RL; it must be at least 80 '/
     :        /'to report the FITS headers (probable programming '/
     :        /'error).', STATUS )
           GOTO 999
         END IF

*    Define a record length of 80 on UNIX platforms.
      ELSE
         RECLEN = 80
      END IF

*    Caption.
*    ========

*    Write blank line.

      DUMMY = ' '
      CALL FIO_WRITE( FD, DUMMY( 1:1 ), STATUS )

*    Find the maximum number of characters that can be output
*    in the caption.

      MXFILN = MIN( 132, RECLEN ) - 30

*    Add ellipsis where truncated.  Write caption to buffer, truncating
*    the filename.

      IF ( NCF .GT. MXFILN ) THEN
         WRITE( DUMMY,
     :     '(''** File: ...'',A,'' ('',I3,'' ) header cards'')' )
     :     CFN( MAX(1, NCF-MXFILN+3):NCF ), SUBFIL
         NCF = MXFILN + 30

*    Write the caption using the full file name.

      ELSE
         WRITE( DUMMY,
     :     '(''** File: '',A,'' ('',I3,'' ) header cards'')' )
     :     CFN( 1:NCF ), SUBFIL
         NCF = NCF + 30
      END IF
      CALL FIO_WRITE( FD, DUMMY( :NCF ), STATUS )

*    and next the header cards.

      K = 0
      DO  J = 1, NHEADS
         DO  I = 1, HDNUM( J )
            K = K + 1
            CALL FIO_WRITE( FD, HEADER( K )( :80 ), STATUS )
         END DO
      END DO

*    Report the error context.

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL FIO_FNAME( FD, FNAME, STATUS )
         CALL MSG_SETC( 'LOGFILN', FNAME )
         CALL ERR_REP( 'FTS1_HDLOG_WRLOG',
     :     'Error writing header cards to the file ^LOGFILN.', STATUS )
      END IF

  999 CONTINUE

      END
