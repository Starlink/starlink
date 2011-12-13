      SUBROUTINE TRN1_INVCM( COMM, STATUS )
*+
*  Name:
*     TRN1_INVCM

*  Purpose:
*     invert comment.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_INVCM( COMM, STATUS )

*  Description:
*     The routine "inverts" a comment string describing a transformation
*     module by interchanging any '-->' and '<--' symbols which appear
*     in it.

*  Copyright:
*     Copyright (C) 1988, 1992 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     25-APR-1988 (RFWS):
*        Original version.
*     13-FEB-1992 (RFWS):
*        Eliminated non-standard character string concatenations.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants


*  Arguments Given:
*     <declarations and descriptions for imported arguments>


*  Arguments Given and Returned:
      CHARACTER * ( * ) COMM    ! Comment to be inverted


*  Arguments Returned:
*     <declarations and descriptions for exported arguments>


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      INTEGER IMAP               ! Position of next arrow
      INTEGER IMAP1              ! Position of next '-->'
      INTEGER IMAP2              ! Position of next '<--'

*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Loop to search for the '-->' and '<--' symbol(s), until no more are
*   found.
      IMAP = 1
      DO WHILE ( ( IMAP + 2 ) .LE. LEN( COMM ) )


*   Find the next symbol.
        IMAP1 = INDEX( COMM( IMAP : ), '-->' )
        IF ( IMAP1 .EQ. 0 ) IMAP1 = LEN( COMM ) + 1
        IMAP2 = INDEX( COMM( IMAP : ), '<--' )
        IF ( IMAP2 .EQ. 0 ) IMAP2 = LEN( COMM ) + 1
        IMAP = IMAP - 1 + MIN( IMAP1, IMAP2 )


*   If a symbol was found, invert its direction.
        IF( ( IMAP + 2 ) .LE. LEN( COMM ) ) THEN
          IF( COMM( IMAP : IMAP + 2 ) .EQ. '-->' ) THEN
            COMM( IMAP : IMAP + 2 ) = '<--'
          ELSE IF( COMM( IMAP : IMAP + 2 ) .EQ. '<--' ) THEN
            COMM( IMAP : IMAP + 2 ) = '-->'
          ENDIF


*   Increment the pointer to the following character position.
          IMAP = IMAP + 3


*   End of "a symbol was found" condition.
        ENDIF


*   End of "loop to search for symbols" loop.
      ENDDO


*   Exit routine.
      END
