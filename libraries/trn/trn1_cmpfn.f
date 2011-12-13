      SUBROUTINE TRN1_CMPFN( NEXPR, EXPR, NVAR, VAR, MXSYMB, GCODE,
     :                       MXCOD, MXCON, CODE, NCODE, CON, NCON,
     :                       STKSZ, IERR, WRK, STATUS )








*+
*  Name:
*     TRN1_CMPFN

*  Purpose:
*     compile transformation function definitions.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_CMPFN( NEXPR, EXPR, NVAR, VAR, MXSYMB, GCODE,
*                      MXCOD, MXCON, CODE, NCODE, CON, NCON,
*                      STKSZ, IERR, WRK, STATUS )

*  Description:
*     The routine compiles a set of transformation function definition
*     expressions.  It returns the total number of code elements and
*     constants produced by compiling each of the expressions in turn
*     and (optionally) fills arrays with the code and constant values.
*     These may subsequently be passed to the TRN1_EVTMx routines to
*     evaluate the transformation.  The code values produced are defined
*     in the include file TRN_CONST_STM.

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
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
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}

*  History:
*     1-MAR-1988:  Original version (DUVAD::RFWS)
*     13-MAY-1988:  Re-written to pass workspace (DUVAD::RFWS)
*     6-DEC-1988:  Added protection against invalid subscript for CON
*        array (DUVAD::RFWS)
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
      INTEGER NEXPR             ! Number of expressions to compile
      CHARACTER * ( * ) EXPR( NEXPR )
                                ! List of expressions to compile - must
                                ! be upper case with no embedded blanks
      INTEGER NVAR              ! Number of variables appearing in the
                                ! expressions
      CHARACTER * ( * ) VAR( NVAR )
                                ! List of the variable names - must be
                                ! upper case with no embedded blanks
      INTEGER MXSYMB            ! Size of the WRK array - determines the
                                ! maximum number of symbols allowed in
                                ! an expression
      LOGICAL GCODE             ! Whether to generate code and
                                ! constants or simply compile and check
      INTEGER MXCOD             ! Size of the CODE array
      INTEGER MXCON             ! Size of the CON array


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      INTEGER CODE( MXCOD )     ! Array of operation codes generated
                                ! (not used if GCODE = .FALSE.)
      INTEGER NCODE             ! Number of operation codes generated
      DOUBLE PRECISION CON( MXCON )
                                ! Array of numerical constants generated
                                ! (not used if GCODE = .FALSE.)
      INTEGER NCON              ! Number of constants generated
      INTEGER STKSZ             ! Stack size required to evaluate the
                                ! transformation
      INTEGER IERR              ! Number of any expression which failed
                                ! to compile
      INTEGER WRK( MXSYMB, 3 )  ! Workspace array


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      INTEGER IEXPR             ! Loop counter for expressions
      INTEGER NCODEN            ! Next element of the CODE array to use
      INTEGER NCONN             ! Next element of the CON array to use
      INTEGER NCODER            ! Number of CODE elements remaining
      INTEGER NCONR             ! Number of CON elements remaining
      INTEGER ICODE             ! Number of code elements produced by
                                ! an expression
      INTEGER ICON              ! Number of constants produced by an
                                ! expression
      INTEGER ISTK              ! Size of stack required to evaluate an
                                ! expression


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Initialise.
      NCODE = 0
      NCON = 0
      STKSZ = 0
      IERR = 0


*   Loop to compile each expression in turn.
      IEXPR = 0
      DO WHILE ( ( IEXPR .LT. NEXPR ) .AND. ( STATUS .EQ. SAI__OK ) )
        IEXPR = IEXPR + 1


*   If code and constants are being generated, set pointers to the next
*   array elements to use and calculate the number of elements
*   remaining.  Note that NCONN and NCONR require special attention to
*   avoid an invalid subscript or a zero-length array if the number of
*   constants generated by the final expression(s) is zero.
        IF( GCODE ) THEN
          NCODEN = NCODE + 1
          NCONN = MIN( NCON + 1, MXCON )
          NCODER = MXCOD - NCODE
          NCONR = MXCON - NCONN + 1


*   If no code or constants are being generated, always use the first
*   element of the CODE and CON arrays, as subsequent elements may not
*   exist.
        ELSE
          NCODEN = 1
          NCONN = 1
          NCODER = 1
          NCONR = 1
        ENDIF


*   Compile the expression, appending the code and constants generated
*   (if they are being generated) to those already produced.
        CALL TRN1_CMPEX( EXPR( IEXPR ), NVAR, VAR, MXSYMB, GCODE,
     :                   NCODER, NCONR, CODE( NCODEN ), ICODE,
     :                   CON( NCONN ), ICON, ISTK, WRK, STATUS )


*   If there is no error, increment the code and constant counters and
*   find the maximum evaluation stack size.
        IF( STATUS .EQ. SAI__OK ) THEN
          NCODE = NCODE + ICODE
          NCON = NCON + ICON
          STKSZ = MAX( STKSZ, ( IEXPR - 1 ) + ISTK + NVAR )
        ENDIF
      ENDDO


*   If there was an error, return the number of the expression which
*   produced it.
      IF( STATUS .NE. SAI__OK ) IERR = IEXPR


*   Exit routine.
      END
