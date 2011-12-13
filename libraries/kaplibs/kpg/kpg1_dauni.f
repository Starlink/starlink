      SUBROUTINE KPG1_DAUNI( NDF, COMP, UNITS, NCU, STATUS )
*+
*  Name:
*     KPG1_DAUNI

*  Purpose:
*     Generates a string containing the units of an NDF's data or
*     variance component allowing for truncation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_DAUNI( NDF, COMP, UNITS, NCU, STATUS )

*  Description:
*     This routines generates a string containing the units of an
*     NDF's data or variance component.  If the length of the
*     variable to store the string is shorter than the units field,
*     the units string is truncated and an ellipsis is inserted.
*     For the variance the data units are enclosed in parentheses
*     and followed by '**2'.  Again if the the length of the units
*     string is too short, truncate the units within the parentheses
*     and insert an ellipsis.  A null string is returned if the UNITS
*     component does not exist within the NDF.

*  Arguments:
*     NDF = INTEGER (Given)
*        The NDF identifier
*     COMP = CHARACTER * ( * ) (Given)
*        The NDF component: 'DATA', 'VARIANCE', 'QUALITY' or 'ERROR'.
*        Any other component will result in a SAI__ERROR status being
*        returned immediately.
*     UNITS = CHARACTER * ( * ) (Returned)
*        The string containing the units for the component, possibly
*        truncated.  It is recommended that string should be at least
*        20 characters long.  If COMP = 'VARIANCE' the length must be
*        no less than 9 characters; for COMP = 'DATA' the minimum is 4
*        characters; for COMP = 'ERROR' the minimum is 5 characters;
*        otherwise the routine returns immediately with the SAI__ERROR
*        status.
*     NCU = INTEGER (Returned)
*        The length of the units string in characters ignoring trailing
*        blanks. Always returned equal to zero if COMP = 'QUALITY'.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 1998, 1999 Central Laboratory of the Research
*                   Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 February 23 (MJC):
*        Original version.
*     1992 April 14 (MJC):
*        Allow for blank units and for ERROR component.
*     22-SEP-1998 (DSB):
*        Bug fixed which caused UNITS to be addressed out of bounds if
*        the the UNITS NDF component is longer than the declared length of
*        the UNITS argument.
*     4-11-1999 (DSB)
*        Remove non-printable characters from the units string. Return
*        NCU = 0 without error if COMP = 'QUALITY'.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) COMP

*  Arguments Returned:
      CHARACTER * ( * ) UNITS
      INTEGER NCU

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a character string less
                                 ! trailing blanks

*  Local Constants:
      INTEGER LBUF               ! Declared length of BUFFER
      PARAMETER( LBUF = 132 )

*  Local Variables:
      CHARACTER * ( LBUF ) BUFFER ! Buffer for forming the concatenation
                                 ! of the parts of the output string
      CHARACTER * ( 8 ) CMPNAM   ! Uppercase copy of the component
      INTEGER CPOS               ! Character position of the last
                                 ! non-blank in a string
      INTEGER CUNITS             ! Number of characters in the units
      LOGICAL UNITSP             ! Units value is present?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the maximum number of characters there are to store the units
*  string.
      CUNITS = LEN( UNITS )

*  Check the component is valid.  Must copy the component to a local
*  variable before converting to uppercase and removal of leading
*  blanks for the comparison.
      CMPNAM = COMP
      CALL CHR_UCASE( CMPNAM )
      CALL CHR_LDBLK( CMPNAM )

      IF( CMPNAM( 1:2 ) .EQ. 'QU' ) THEN
         NCU = 0
         UNITS = ' '

         GO TO 999

      ELSE IF ( CMPNAM( 1:2 ) .NE. 'DA' .AND. CMPNAM( 1:2 ) .NE. 'VA'
     :          .AND. CMPNAM( 1:2 ) .NE. 'ER' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_DAUNI_COMP',
     :     'The component must be DATA or VARIANCE or ERROR to be '/
     :     /'able to generate the units string (programming error).',
     :     STATUS )

      ELSE IF ( ( CMPNAM( 1:2 ) .EQ. 'DA' .AND. CUNITS .LT. 4 ) .OR.
     :          ( CMPNAM( 1:2 ) .EQ. 'ER' .AND. CUNITS .LT. 5 ) .OR.
     :          ( CMPNAM( 1:2 ) .EQ. 'VA' .AND. CUNITS .LT. 9 ) ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP( 'KPG1_DAUNI_TOOSHORT',
     :     'The length allowed for the units component in ^NDF is too '/
     :     /'short (programming error).', STATUS )

      ELSE

*  Initialise the units.
         BUFFER = ' '
         UNITS = ' '
         NCU = 0

*  Obtain the units if present.
         CALL NDF_STATE( NDF, 'UNITS', UNITSP, STATUS )
         IF ( UNITSP ) THEN
            CALL NDF_CGET( NDF, 'UNITS', BUFFER, STATUS )

*  Clean it.
            CALL CHR_CLEAN( BUFFER )

*  Get the number of characters in the units.
            CALL NDF_CLEN( NDF, 'UNITS', NCU, STATUS )
            NCU = CHR_LEN( BUFFER( : MIN( NCU, LBUF ) ) )

*  Ignore units if it is a blank string.
            IF ( NCU .GT. 0 ) THEN

*  Copy the units string.
               UNITS = BUFFER( : MIN( LBUF, CUNITS ) )

*  Watch out for the cases where the units string cannot be fitted into
*  the key.  Insert an ellipsis if the text overflows.
               IF ( CMPNAM( 1:2 ) .EQ. 'DA' .OR.
     :              CMPNAM( 1:2 ) .EQ. 'ER' ) THEN
                  IF ( NCU .GT. CUNITS ) THEN
                     CPOS = CUNITS - 3
                     CALL CHR_APPND( '...', UNITS, CPOS )
                     NCU = CUNITS
                  END IF

*  Variance units are data units squared.  So modify the length of the
*  units string accordingly unless the units are truncated.  Insert an
*  ellipsis if the text overflows.  A buffer has to be used to form the
*  composite units string so as not to violate the Fortran standard.
               ELSE IF ( CMPNAM( 1:2 ) .EQ. 'VA' ) THEN
                  BUFFER = '('
                  CPOS = 1
                  IF ( NCU .GT. CUNITS - 5 ) THEN
                     CALL CHR_APPND( UNITS( 1:CUNITS-8 ), BUFFER,
     :                               CPOS )
                     CALL CHR_APPND( '...)**2', BUFFER, CPOS )
                     NCU = CUNITS
                  ELSE
                     CALL CHR_APPND( UNITS( 1:NCU ), BUFFER, CPOS )
                     CALL CHR_APPND( ')**2', BUFFER, CPOS )
                     UNITS = '('//UNITS( 1:NCU )//')**2'
                     NCU = NCU + 5
                  END IF
                  UNITS = BUFFER( :NCU )
               END IF
            END IF
         END IF
      END IF

 999  CONTINUE

      END
