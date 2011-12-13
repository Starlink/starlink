      SUBROUTINE TRN1_PSVAR( EXPRS, ISTART, NVAR, VAR, IVAR, IEND,
     :                       STATUS )
*+
*  Name:
*     TRN1_PSVAR

*  Purpose:
*     parse a variable name.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_PSVAR( EXPRS, ISTART, NVAR, VAR, IVAR, IEND,
*                      STATUS )

*  Description:
*     The routine parses an expression, looking for a variable name
*     starting at the character position ISTART in the expression EXPRS.
*     If the routine identifies the variable name successfully, IVAR
*     will be set to the number of the variable (in the list of
*     variables, VAR) and IEND will be set to the position of the final
*     variable name character in EXPRS.  If the characters encountered
*     do not constitute a valid name, the routine returns with IVAR and
*     IEND set to zero, but without setting STATUS.  However, if they
*     appear to be a name, but do not match any of the variable names
*     supplied in VAR, then STATUS is also set and an error is
*     reported.  Note that this routine is case sensitive and input
*     character arguments must be in upper case with no embedded blanks.

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
*     20-MAY-1988 (RFWS):
*        Original version.
*     13-FEB-1992 (RFWS):
*        Improved the error reporting.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      CHARACTER * ( * ) EXPRS   ! Input expression
      INTEGER ISTART            ! Initial character position of possible
                                ! variable name in EXPRS
      INTEGER NVAR              ! Number of valid variable names
      CHARACTER * ( * ) VAR( NVAR )
                                ! List of valid variable names


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      INTEGER IVAR              ! Identification number of the variable
      INTEGER IEND              ! Position of the final variable name
				! character in EXPRS


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      LOGICAL ISNAM             ! Whether characters appear to be a name
      LOGICAL ISVAR             ! Whether characters are a valid
                                ! variable name
      INTEGER NCH               ! Number of characters in the variable
                                ! name


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Initialise.
      IVAR = 0
      IEND = 0


*   Determine if the characters in the expression starting at position
*   ISTART constitute a valid name and find the number of characters in
*   the name.
      CALL TRN1_ISNAM( EXPRS( ISTART : ), ISNAM, NCH )
      IF( ( .NOT. ISNAM ) .AND. ( NCH .GT. 1 ) ) THEN
        NCH = NCH - 1
        ISNAM = .TRUE.
      ENDIF


*   If the expression contains a valid name, find the position of its
*   final character.
      IF( ISNAM ) THEN
        IEND = ISTART + NCH - 1


*   Loop to compare the name with the list of valid variable names
*   supplied.
        IVAR = 0
        ISVAR = .FALSE.
        DO WHILE ( ( IVAR .LT. NVAR ) .AND. ( .NOT. ISVAR ) )
          IVAR = IVAR + 1
          ISVAR = ( EXPRS( ISTART : IEND ) .EQ. VAR( IVAR ) )
        ENDDO


*   If it was not amongst them, report an error and reset the output
*   arguments.
        IF( .NOT. ISVAR ) THEN
          STATUS = TRN__VARUD
          CALL MSG_SETC( 'VAR', EXPRS( ISTART : IEND ) )
          CALL MSG_SETC( 'EXPRS', EXPRS( : IEND ) )
          CALL ERR_REP( 'TRN1_PSVAR_UNDEF',
     :                  'Undefined variable name ''^VAR'' used in ' //
     :                  'the expression ''^EXPRS''... (possible ' //
     :                  'programming error).', STATUS )
          IVAR = 0
          IEND = 0
        ENDIF


*   End of "the expression contains a valid name" condition.
      ENDIF


*   Exit routine.
      END
