      SUBROUTINE TRN1_CLVAR( NVAR, DEFIN, VAROUT, STATUS )
*+
*  Name:
*     TRN1_CLVAR

*  Purpose:
*     Clean (and check) variable names.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_CLVAR( NVAR, DEFIN, VAROUT, STATUS )

*  Description:
*     The routine extracts variable names from the left hand sides of a
*     set of transformation function definitions and cleans the
*     resulting strings by removing embedded blanks and converting to
*     upper case.  The variable names obtained are then validated to
*     check for correct syntax and no duplication.  STATUS is set and
*     an error is reported if anything is wrong.  Note this routine
*     does not check for string truncation on output.

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
*     12-MAY-1988 (RFWS):
*        Original version.
*     13-FEB-1992 (RFWS):
*        Improved the error reporting.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes

*  Arguments Given:
      INTEGER NVAR               ! Number of variables to process
      CHARACTER * ( * ) DEFIN( NVAR ) ! Input function definitions

*  Arguments Returned:
      CHARACTER * ( * ) VAROUT( NVAR ) ! Cleaned variable names

*  Status:
      INTEGER STATUS             ! Error status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*-

*  Local Variables:
      INTEGER I                  ! Loop counter for indexing arrays
      INTEGER J                  ! Loop counter for indexing arrays
      INTEGER LDEF               ! Length of a function definition string
      INTEGER LVAR               ! Length of a variable string
      LOGICAL OK                 ! Whether a variable name is valid

*.

*  Check status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Loop to process each variable until they are all done or an error is
*  detected.
      I = 0
      DO WHILE( ( I .LT. NVAR ) .AND. ( STATUS .EQ. SAI__OK ) )
        I = I + 1

*  Report an error if the function definition is blank.
        IF( DEFIN( I ) .EQ. ' ' ) THEN
          STATUS = TRN__MISVN    ! missing variable name
          CALL TRN1_ERROR( 'TRN1_CLVAR', ' ', STATUS )

*  If there is no error, find the length of the variable field in the
*  input function definition from the delimiting '=' sign, or the end
*  of the string if the '=' is absent.
        ELSE
          LDEF = CHR_LEN( DEFIN( I ) )
          LVAR = INDEX( DEFIN( I )( : LDEF ), '=' )
          IF( LVAR .NE. 0 ) THEN
            LVAR = LVAR - 1
            IF( LVAR .GE. 1 )LVAR = CHR_LEN( DEFIN( I )( : LVAR ) )
          ELSE
            LVAR = LDEF
          ENDIF

*  Report an error if the variable field is blank.
          IF( LVAR .LE. 0 ) THEN
            STATUS = TRN__MISVN
            CALL MSG_SETC( 'EXPRS', DEFIN( I ) )
            CALL ERR_REP( 'TRN1_CLVAR_LHS',
     :                    'Missing left hand side in the ' //
     :                    'expression ''^EXPRS'' (possible '//
     :                    'programming error).', STATUS )

*  If there is no error, extract the variable field and clean it.
          ELSE
            VAROUT( I ) = DEFIN( I )( : LVAR )
            CALL CHR_RMBLK( VAROUT( I ) )
            CALL CHR_UCASE( VAROUT( I ) )

*  Check the variable name is valid.  Report an error if it is not.
            CALL TRN1_ISNAM( VAROUT( I ), OK, LVAR )
            IF( .NOT. OK ) THEN
              STATUS = TRN__VARIN
              CALL MSG_SETC( 'VAR', VAROUT( I )( : LVAR ) )
              CALL ERR_REP( 'TRN1_CLVAR_BAD',
     :                      'Invalid variable name ''^VAR'' ' //
     :                      'specified  - bad syntax (possible ' //
     :                      'programming error).', STATUS )
            ENDIF
          ENDIF
        ENDIF
      ENDDO

*  If there is no error, loop to compare all the variable names with
*  each other to detect duplication .
      IF( STATUS .EQ. SAI__OK ) THEN
        DO I = 1, NVAR
          DO J = I + 1, NVAR

*  If a duplicate variable name is found, report an error and exit the
*  loops.
            IF( VAROUT( I ) .EQ. VAROUT( J ) ) THEN
              STATUS = TRN__DUVAR
              CALL MSG_SETC( 'VAR', VAROUT( I ) )
              CALL ERR_REP( 'TRN1_CLVAR_DUPE',
     :                      'Duplicate variable name ''^VAR'' ' //
     :                      'encountered (possible programming ' //
     :                      'error).', STATUS )
              GO TO 1
            ENDIF
          ENDDO
        ENDDO
    1   CONTINUE
      ENDIF

*  Exit routine.
      END
