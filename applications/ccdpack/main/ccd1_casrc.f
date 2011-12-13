      SUBROUTINE CCD1_CASRC( STATUS )
*+
*  Name:
*     CCD1_CASRC

*  Purpose:
*     Source routine for use by AST Channels for character array data.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_CASRC( STATUS )

*  Description:
*     This routine implements a source routine which has to be passed to
*     the AST Channel construction routines (AST_CHANNEL, AST_FITSCHAN)
*     in order to do input/output on AST objects to a file.  It does
*     the input from a character array whose characteristics are held
*     in a common block.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Usage:
*     CHAN = AST_CHANNEL( CCD1_CASRC, SINK, OPTIONS, STATUS )

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-JAN-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard HDS constants
      INCLUDE 'CCD1_PAR'         ! Local CCDPACK constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'CCD1_CACM'        ! CCD1_CALEN = INTEGER
                                 !    Length of elements in character array
                                 ! CCD1_CANUM = INTEGER
                                 !    Number of elements in character array
                                 ! CCD1_CAPOS = INTEGER
                                 !    Current position in array
                                 ! CCD1_CAPTR = INTEGER
                                 !    Pointer to character array
                                 ! CCD1_CALOC = CHARACTER * ( DAT__SZLOC )
                                 !    Locator for HDS component storing array

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CCD1__BLEN ) BUF ! Buffer for input
      CHARACTER * ( CCD1__BLEN ) NEX ! Next buffer for input
      CHARACTER * ( 2048 ) LINE  ! Buffer for an entire line
      INTEGER IAT                ! Write position in line buffer

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If there are no lines left, indicate this to the AST system.
      IF ( CCD1_CAPOS .GT. CCD1_CANUM ) THEN
         CALL AST_PUTLINE( ' ', -1, STATUS )

*  If there are lines left, read one.
      ELSE

*  Initialise position in buffer.
         IAT = 0

*  Get the line at the current position and increment the line counter.
         CALL CCD1_CA2C( %VAL( CNF_PVAL( CCD1_CAPTR ) ), CCD1_CAPOS,
     :                   BUF, STATUS, %VAL( CNF_CVAL( CCD1_CALEN ) ) )

*  This is the start of a do..while loop over continuation lines.
 1       CONTINUE
         CCD1_CAPOS = CCD1_CAPOS + 1

*  Copy it into the output buffer, excluding the first character which
*  is a flag.  If there is not enough space, signal an error.
         IF ( IAT + LEN( BUF ) .LT. LEN( LINE ) ) THEN
            CALL CHR_APPND( BUF( 2: ), LINE, IAT )
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_CASRC_OFLOW',
     :                    'CCD1_CASRC: Buffer overflow', STATUS )
         END IF

*  Take a look at the next line if there is one.
         IF ( CCD1_CAPOS .LT. CCD1_CANUM .AND. STATUS .EQ. SAI__OK )
     :      THEN
            CALL CCD1_CA2C( %VAL( CNF_PVAL( CCD1_CAPTR ) ), CCD1_CAPOS,
     :                      BUF, STATUS,
     :                      %VAL( CNF_CVAL( CCD1_CALEN ) ) )

*  If the next line is a continuation line loop round.
            IF ( BUF( 1:1 ) .EQ. '+' ) GO TO 1
         END IF

*  If all is well pass the line to the AST system.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL AST_PUTLINE( LINE, IAT, STATUS )
         END IF
      END IF

      END
* $Id$
