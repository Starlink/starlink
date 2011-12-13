      SUBROUTINE SST_STLAT( STATUS )
*+
*  Name:
*     SST_STLAT

*  Purpose:
*     Start a Latex document.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_STLAT( STATUS )

*  Description:
*     The routine writes the commands necessary to start a Latex
*     document to the output file. If a full Latex document is being
*     produced, then this routine should be called before processing any
*     input prologues.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councls.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     13-AUG-1990 (RFWS):
*        Original version.
*     14-AUG-1990 (RFWS):
*        Added missing call to FIO_SERR.
*     12-SEP-1990 (RFWS):
*        Added Latex command to produce a centred underscore.
*     28-SEP-1990 (RFWS):
*        Added calls to ERR_MARK and ERR_RLSE.
*     7-JUL-1996 (PDRAPER):
*        Fixed to use external file for document start information.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SST_PAR'          ! SST_ constants
      INCLUDE 'FIO_ERR'          ! FIO_ error codes

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      CHARACTER * ( SST__SZLIN ) LINE ! I/O buffer
      CHARACTER * ( 132 ) FILE   ! Full name of file to open
      INTEGER FD                 ! File descriptor
      INTEGER DEFNS              ! I/O unit for layout definitions
      INTEGER F                  ! First character position
      INTEGER L                  ! Last character position

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Now open the SST version of the template SUN file. This
*  includes the SST latex macros.
      CALL PSX_GETENV( 'SST_DIR', FILE, STATUS )
      CALL CHR_APPND( '/sst_preamble.tex', FILE, CHR_LEN( FILE ) )
      CALL FIO_OPEN( FILE, 'READ', 'list', 0, FD, STATUS )
      CALL FIO_UNIT( FD, DEFNS, STATUS )

*  If an error occurred, then construct a message and report it.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETI( 'UNIT', DEFNS )
         CALL ERR_REP( 'SST_STLAT_OPEN',
     :   'Error opening file $SST_DIR/sst_preamble.tex for'//
     :   ' reading on Fortran unit ^UNIT.', STATUS )
         GO TO 99
      END IF

*  Loop to read the file.
      CALL ERR_MARK
 2    CONTINUE                  ! Start of 'DO WHILE' loop
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL SST_GET( DEFNS, LINE, STATUS )
         CALL CHR_FANDL( LINE, F, L )

*  If the line is blank, then output a blank line. Otherwise, send the
*  input line to the output file, preserving its indentation.
         IF ( F .GT. L ) THEN
            CALL SST_PUT( 0, ' ', STATUS )
         ELSE
            CALL SST_PUT( F - 1, LINE( F : L ), STATUS )
         END IF
         GO TO 2
      END IF

*  Annul the end-of-file error and close the file.
      IF ( STATUS .EQ. FIO__EOF .OR.
     :     STATUS .EQ. FIO__ENDFL ) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE
      CALL FIO_CLOSE( FD, STATUS )
99    CONTINUE
      END
* @(#)sst_stlat.f   1.9   96/07/05 10:26:47   96/07/05 10:27:31
