      SUBROUTINE KPS1_CHPIN( FD, COMENT, IGRPS, IGRPV, NEDIT, STATUS )
*+
*  Name:
*     KPS1_CHPIN

*  Purpose:
*     Extracts sections and values from a text file into groups.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CHPIN( FD, COMENT, IGRPS, IGRPV, NEDIT, STATUS )

*  Description:
*     This routine serves CHPIX.  It reads a text file to extract
*     pairs of strings per line that represent in order an NDF
*     section and a value to fill that section.  It skips over comment
*     and blank lines.  Comment lines are those beginning, with any of
*     the characters passed in the COMENT argument (normally ! and #). 
*     Here 'beginning' means the first non-blank character.

*  Arguments:
*     FD = INTEGER (Given)
*        Fortran file identifier.
*     COMENT = CHARACTER * ( * ) (Given)
*        A comma-separated list of comment characters.  If any line of
*        the file begins with one of these, the line is treated as a
*        comment line.
*     IGRPS = INTEGER (Given)
*        GRP identifier to an empty group used to store the NDF
*        sections.
*     IGRPV = INTEGER (Given)
*        GRP identifier to an empty group used to store the replacement
*        values.
*     NEDIT = INTEGER (Returned)
*        The number of NDF section/value pairs in the file.
*     STATUS = INTEGER (Given & Returned)
*        The global status.

*  Notes:
*     -  The file is rewound, but not closed on exit.
*     -  The groups are filled from the first member.
*     -  It does not validate the NDF section, but it checks that the
*     value is either 'Bad' or numeric.

*  Prior Requirements:
*     -  The Fortran text file must already be opened.
*     -  The groups must exist.

*  Copyright:
*     Copyright (C) 2018 Science & Technology Facilities Council/
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 3 of
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
*     {enter_new_authors_here}

*  History:
*     2018 September 19 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO__ error constants

*  Arguments Given:
      INTEGER FD
      CHARACTER * ( * ) COMENT
      INTEGER IGRPS
      INTEGER IGRPV

*  Arguments Returned:
      INTEGER NEDIT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_INSET          ! A string is a member of a given set?
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER NWORD              ! Number of words to extract from 
      PARAMETER ( NWORD = 2 )    ! text file
      
*  Local Variables:
      CHARACTER * 80 BUFFER      ! Text buffer for reading file
      CHARACTER * 80 CVALUE      ! Value string
      INTEGER CSTAT              ! CHR status
      DOUBLE PRECISION DVALUE    ! Numeric value
      INTEGER I1( NWORD )        ! Pointer to start of tokens
      INTEGER I2( NWORD )        ! Pointer to end of tokens
      INTEGER NCHAR              ! Number of characters read from file
      INTEGER NTOK               ! Number of tokens on line
      CHARACTER * 1 TOK( NWORD ) ! Buffer for parsing lines

*.

*  Initialise returned values.
      NEDIT = 0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new error context.
      CALL ERR_MARK

*  Determine the number of points in the list.  Loop round until an
*  error encountered (this will happen when the end of file is reached,
*  if not before).
   10 CONTINUE          ! Start of 'DO WHILE' loop
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Read a record of the text file.
         CALL FIO_READ( FD, BUFFER, NCHAR, STATUS )

*  Remove leading blanks.
         CALL CHR_LDBLK( BUFFER )
         NCHAR = CHR_LEN( BUFFER( :NCHAR ) )

*  Watch for an error.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Skip if the line a comment or a blank line.
            IF ( .NOT. ( CHR_INSET( COMENT, BUFFER( 1:1 ) ) .OR.
     :                   NCHAR .EQ. 0 ) ) THEN

*  It must therefore be a data line.
               CSTAT = 0
               CALL CHR_DCWRD( BUFFER( :NCHAR ), NWORD, NTOK, I1, I2,
     :                         TOK, CSTAT )

*  Extract and validate the value.  Can either be "Bad" or a
*  numeric value.
               CVALUE = BUFFER( I1( 2 ):I2( 2 ) )
               CALL CHR_UCASE( CVALUE )
               CALL CHR_CTOD( CVALUE, DVALUE, CSTAT )
               IF ( CVALUE .EQ. 'BAD' .OR. CSTAT .EQ. 0 ) THEN

*  Store the NDF section and value in their respective groups.
                  NEDIT = NEDIT + 1
                  CALL GRP_PUT1( IGRPV, CVALUE, NEDIT, STATUS )
                  CALL GRP_PUT1( IGRPS, BUFFER( I1( 1 ):I2( 1 ) ),
     :                           NEDIT, STATUS )
               END IF
            END IF
         END IF

*  End of 'DO WHILE' loop.
         GO TO 10
      END IF

*  If an end-of-file error has been reported, annul it.
      IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )

*  CLose the new error context.
      CALL ERR_RLSE

*  Rewind the file.
      CALL FIO_RWIND( FD, STATUS )

      END
