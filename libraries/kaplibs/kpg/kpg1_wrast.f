      SUBROUTINE KPG1_WRAST( STATUS )
*+
*  Name:
*     KPG1_WRAST

*  Purpose:
*     Writes AST_ data as text to an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_WRAST

*  Description:
*     This is a service routine to be provided as a "sink" routine for
*     the AST_CHANNEL function. It takes data in the form of text (in
*     response to writing an AST_ object to a Channel) and delivers it
*     to an HDS object for storage.
*
*     This routine has only a STATUS argument, so it communicates with
*     other KPG routines via global variables stored in the KPG_AST
*     common blocks. These are described below under "Global Variables
*     used as Arguments".

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Global Variables used as Arguments:
*     ASTLC = CHARACTER * ( DAT__SZLOC ) (Given)
*        A locator for the HDS object which is to store the data. This
*        must be a one-dimensional _CHAR array, whose initial size and
*        character string length will be determined via this locator.
*        Write access to the object must be available via this locator,
*        but the locator itself is not altered by this routine.
*     ASTLN = INTEGER (Given and Returned)
*        This must initially be set to the value 1, to indicate that
*        data will be written starting at the first element of the HDS
*        array (note the routine will not operate correctly unless 1 is
*        the initial value - you cannot start writing at another point
*        in the array if you have previously written to a different
*        array). On exit it will be incremented by the number of
*        elements used to store data, so that it identifies the first
*        element to be used on the next invocation.
*     ASTPT = INTEGER (Given and Returned)
*        A pointer to the contents of the HDS object, initially mapped
*        in 'WRITE' mode. This pointer may be modified by the routine
*        (and re-mapped in 'UPDATE' mode) if it needs to extend the
*        size of the object to accommodate the data written.

*  Copyright:
*     Copyright (C) 1998, 2004 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-FEB-1998 (DSB):
*        Original version, based on NDF1_WRAST.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'KPG_PAR'          ! KPG_ constants
      INCLUDE 'AST_PAR'          ! AST_ public interface
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'KPG_AST'          ! KPG AST common blocks
*        ASTLC = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to HDS _CHAR array holding AST_ data.
*        ASTLN = INTEGER (Read and Write)
*           Next element to use in HDS _CHAR array holding AST_ data.
*        ASTPT = INTEGER (Read and Write)
*           Pointer to mapped HDS _CHAR array holding AST_ data.

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER SZTEXT             ! Size of text buffer
      PARAMETER ( SZTEXT = ( KPG__SZAST - 1 ) * ( KPG__MXACL + 1 ) )

*  Local Variables:
      CHARACTER * ( 1 ) FLAG     ! Flag character
      CHARACTER * ( SZTEXT + 1 ) TEXT ! Buffer for AST_ text
      INTEGER DIM( 1 )           ! Dimension size of HDS object
      INTEGER I1                 ! Index of first character to write
      INTEGER I2                 ! Index of last character to write
      INTEGER L                  ! Number of characters in AST_ text
      INTEGER LENGTH             ! Length of HDS object in characters
      INTEGER NDIM               ! Number of HDS object dimensions
      INTEGER STATUS             ! Local status variable

      SAVE DIM
      SAVE LENGTH
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Before writing the first line, obtain the initial number of elements
*  in the HDS _CHAR array being written to, and the length of each
*  element in characters.
      IF ( ASTLN .EQ. 1 ) THEN
         CALL DAT_SHAPE( ASTLC, 1, DIM, NDIM, STATUS )
         CALL DAT_CLEN( ASTLC, LENGTH, STATUS )
      END IF

*  Obtain the text to be written.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL AST_GETLINE( TEXT, L, STATUS )

*  If the text was obtained successfully, then check its length. If
*  this exceeds what can safely be handled, report an error.
         IF ( ( STATUS .EQ. SAI__OK ) .AND. ( L .GT. SZTEXT ) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'ACL', KPG__MXACL )
            CALL ERR_REP( 'KPG1_WRAST_XS',
     :           'Maximum number of continuation lines (^ACL) ' //
     :           'exceeded - output text is too long.',
     :           STATUS )
            CALL ERR_REP( 'KPG1_WRAST_XS2', TEXT( : 80 ), STATUS )
         END IF
      END IF

*  Remove any leading and trailing blanks (this removes any
*  indentation).
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( L .GT. 0 ) THEN
            CALL CHR_LDBLK( TEXT( : L ) )
            L = CHR_LEN( TEXT( : L ) )
         END IF

*  Initialise the flag character for continuation lines.
         IF ( L .GT. 0 ) THEN
            FLAG = ' '

*  Loop to write the text into the HDS array as a sequence of lines of
*  length one less than the number of characters in each array element
*  (the first character of each line is reserved for use as a flag
*  character).
            DO 1 I1 = 1, L, LENGTH - 1

*  Before writing each line, check that the HDS array is large enough
*  to accomodate it. If not, unmap the array and double its size. Then
*  re-map it.
               IF ( ASTLN .GT. DIM( 1 ) ) THEN
                  CALL DAT_UNMAP( ASTLC, STATUS )
                  DIM( 1 ) = 2 * DIM( 1 )
                  CALL DAT_ALTER( ASTLC, 1, DIM, STATUS )
                  CALL DAT_MAP( ASTLC, '_CHAR', 'UPDATE', 1, DIM,
     :                          ASTPT, STATUS )
               END IF

*  Find the last character to be included in the current line.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  I2 = MIN( I1 + LENGTH - 2, L )

*  Write the line, prefixed by the flag character.
                  CALL KPG1_AST2H( %VAL( CNF_PVAL( ASTPT ) ), ASTLN,
     :                             FLAG // TEXT( I1 : I2 ),
     :                             STATUS, %VAL( CNF_CVAL( LENGTH ) ) )

*  Increment the line number to be used next, and note that the next
*  line (if any) will be a continuation line.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     ASTLN = ASTLN + 1
                     FLAG = '+'
                  END IF
               END IF

*  Quit looping if an error occurs.
               IF ( STATUS .NE. SAI__OK ) GO TO 2
 1          CONTINUE
 2          CONTINUE
         END IF
      END IF

      END
