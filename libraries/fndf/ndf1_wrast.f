      SUBROUTINE NDF1_WRAST( STATUS )
*+
*  Name:
*     NDF1_WRAST

*  Purpose:
*     Write AST_ data as text to an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_WRAST

*  Description:
*     This is a service routine to be provided as a "sink" routine for
*     the AST_CHANNEL function. It takes data in the form of text (in
*     response to writing an AST_ object to a Channel) and delivers it
*     to an HDS object for storage.
*
*     This routine has only a STATUS argument, so it communicates with
*     other NDF_ routines via global variables stored in the DCB. These
*     are described below under "Global Variables used as Arguments".

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Global Variables used as Arguments:
*     DCB_ASTLC = CHARACTER * ( DAT__SZLOC ) (Given)
*        A locator for the HDS object which is to store the data. This
*        must be a 1-dimensional _CHAR array, whose initial size and
*        character string length will be determined via this locator.
*        Write access to the object must be available via this locator,
*        but the locator itself is not altered by this routine.
*     DCB_ASTLN = INTEGER (Given and Returned)
*        This must initially be set to the value 1, to indicate that
*        data will be written starting at the first element of the HDS
*        array (note the routine will not operate correctly unless 1 is
*        the initial value - you cannot start writing at another point
*        in the array if you have previously written to a different
*        array). On exit it will be incremented by the number of
*        elements used to store data, so that it identifies the first
*        element to be used on the next invocation.
*     DCB_ASTPT = INTEGER (Given and Returned)
*        A pointer to the contents of the HDS object, initially mapped
*        in 'WRITE' mode. This pointer may be modified by the routine
*        (and re-mapped in 'UPDATE' mode) if it needs to extend the
*        size of the object to accommodate the data written.

*  Copyright:
*     Copyright (C) 1997 Rutherford Appleton Laboratory

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     23-JUN-1997 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'AST_PAR'          ! AST_ public interface

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ASTLC = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator to HDS _CHAR array holding AST_ data.
*        DCB_ASTLN = INTEGER (Read and Write)
*           Next element to use in HDS _CHAR array holding AST_ data.
*        DCB_ASTPT = INTEGER (Read and Write)
*           Pointer to mapped HDS _CHAR array holding AST_ data.

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER SZTEXT             ! Size of text buffer
      PARAMETER ( SZTEXT = ( NDF__SZAST - 1 ) * ( NDF__MXACL + 1 ) )

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
      IF ( DCB_ASTLN .EQ. 1 ) THEN
         CALL DAT_SHAPE( DCB_ASTLC, 1, DIM, NDIM, STATUS )
         CALL DAT_CLEN( DCB_ASTLC, LENGTH, STATUS )
      END IF

*  Obtain the text to be written.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL AST_GETLINE( TEXT, L, STATUS )

*  If the text was obtained successfully, then check its length. If
*  this exceeds what can safely be handled, report an error.
         IF ( ( STATUS .EQ. SAI__OK ) .AND. ( L .GT. SZTEXT ) ) THEN
            STATUS = NDF__TRUNC
            CALL MSG_SETI( 'ACL', NDF__MXACL )
            CALL ERR_REP( 'NDF1_WRAST_XS',
     :           'Maximum number of continuation lines (^ACL) ' //
     :           'exceeded - output text is too long.',
     :           STATUS )
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
               IF ( DCB_ASTLN .GT. DIM( 1 ) ) THEN
                  CALL DAT_UNMAP( DCB_ASTLC, STATUS )
                  DIM( 1 ) = 2 * DIM( 1 )
                  CALL DAT_ALTER( DCB_ASTLC, 1, DIM, STATUS )
                  CALL DAT_MAP( DCB_ASTLC, '_CHAR', 'UPDATE', 1, DIM,
     :                          DCB_ASTPT, STATUS )
               END IF

*  Find the last character to be included in the current line.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  I2 = MIN( I1 + LENGTH - 2, L )

*  Write the line, prefixed by the flag character.
                  CALL NDF1_AST2H( %VAL( CNF_PVAL( DCB_ASTPT ) ),
     :                             DCB_ASTLN, FLAG // TEXT( I1 : I2 ),
     :                             STATUS, %VAL( CNF_CVAL( LENGTH ) ) )

*  Increment the line number to be used next, and note that the next
*  line (if any) will be a continuation line.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     DCB_ASTLN = DCB_ASTLN + 1
                     FLAG = '+'
                  END IF
               END IF

*  Quit looping if an error occurs.
               IF ( STATUS .NE. SAI__OK ) GO TO 2
 1          CONTINUE
 2          CONTINUE
         END IF
      END IF

*  Call error tracing routine.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_WRAST', STATUS )

      END
