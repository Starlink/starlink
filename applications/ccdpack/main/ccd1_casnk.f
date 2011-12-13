      SUBROUTINE CCD1_CASNK( STATUS )
*+
*  Name:
*     CCD1_CASNK

*  Purpose:
*     Sink routine for use by AST Channels for character array data.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_CASNK( STATUS )

*  Description:
*     This routine implements a sink routine which has to be passed to
*     the AST Channel construction routines (AST_CHANNEL, AST_FITSCHAN)
*     in order to do input/output on AST objects to a file.  It does
*     output to a character array whose characteristics are held in
*     a common block.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Usage:
*     CHAN = AST_CHANNEL( SOURCE, CCD1_CASNK, OPTIONS, STATUS )

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
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters
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

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 2048 ) LINE  ! Buffer for an entire line
      CHARACTER * ( CCD1__BLEN ) BUF ! Buffer for one character element
      CHARACTER * ( 1 ) FLAG     ! Continuation flag character
      INTEGER I                  ! Start of text chunk in line buffer
      INTEGER J                  ! End of text chunk in line buffer
      INTEGER K                  ! Loop variable
      INTEGER NCHAR              ! Number of characters in line buffer

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the text to be written from the AST system.
      CALL AST_GETLINE( LINE, NCHAR, STATUS )

*  Check the length of the line.  If it is too long, report an error.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      IF ( NCHAR .GE. LEN( LINE ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_CASNK_OFLOW',
     :                 'CCD1_CASNK: Buffer overflow', STATUS )
         GO TO 99
      END IF

*  Remove any leading and trailing blanks (this removes any indentation).
      IF ( NCHAR .GT. 0 ) THEN
         CALL CHR_LDBLK( LINE( : NCHAR ) )
         NCHAR = CHR_LEN( LINE( : NCHAR ) )
      END IF

*  Set the flag character to a space (no continuation) for the first
*  chunk to output.
      IF ( NCHAR .GT. 0 ) THEN
         FLAG = ' '

*  Loop to write the text into the HDS array as a sequence of lines of
*  length one less than the number of characters in each array element
*  (the first character of each line is reserved for use as a flag
*  character).
         DO I = 1, NCHAR, CCD1_CALEN - 1

*  Before writing each line, check that the HDS array is large enough
*  to accomodate it. If not, unmap the array and increase its size.
*  Then re-map it and fill it with blanks to avoid problems with phantom
*  continuation lines.
            IF ( CCD1_CAPOS .GT. CCD1_CANUM ) THEN
               CALL DAT_UNMAP( CCD1_CALOC, STATUS )
               CCD1_CANUM = CCD1_CANUM * 2
               CALL DAT_ALTER( CCD1_CALOC, 1, CCD1_CANUM, STATUS )
               CALL DAT_MAPV( CCD1_CALOC, '_CHAR', 'UPDATE', CCD1_CAPTR,
     :                        CCD1_CANUM, STATUS )
               DO K = CCD1_CAPOS, CCD1_CANUM
                  CALL CCD1_C2CA( %VAL( CNF_PVAL( CCD1_CAPTR ) ),
     :                            K, ' ', STATUS,
     :                            %VAL( CNF_CVAL( CCD1_CALEN ) ) )
               END DO
            END IF

*  Find the last character to be included in the current line.
            IF ( STATUS .EQ. SAI__OK ) THEN
               J = MIN( I + CCD1_CALEN - 1, NCHAR )

*  Write the line, prefixed by the flag_character.
               CALL CCD1_C2CA( %VAL( CNF_PVAL( CCD1_CAPTR ) ),
     :                         CCD1_CAPOS,
     :                         FLAG // LINE( I : J ), STATUS,
     :                         %VAL( CNF_CVAL( CCD1_CALEN ) ) )

*  Increment the line number to be used next, and set the flag
*  appropriately for a continuation line.
               CCD1_CAPOS = CCD1_CAPOS + 1
               FLAG = '+'
            END IF

*  Exit the loop if there has been an error.
            IF ( STATUS .NE. SAI__OK ) GO TO 99
         END DO
      END IF

*  Error exit label.
 99   CONTINUE

      END
* $Id$
