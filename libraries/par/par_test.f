      SUBROUTINE PAR_TEST( STATUS )
*+
*  Name:
*     PAR_TEST

*  Purpose:
*     Tests a release of PAR has been made successfully.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PAR_TEST( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application calls a number of routines from the PAR library
*     to test the PAR has been installed correctly.  The values obtained
*     are used to form a message, incorporating the text and a number
*     derived from the numerical values.
*
*     The value is (A+C)/B(1) + B(2)/D + H * <I> + K

*  Usage:
*     PAR_TEST [A] [B] [C] [D] [E] [F] [G] [H] [I]

*  ADAM Parameters:
*     A = _INTEGER (Read)
*        An integer scalar, without constraints. [1]
*     B( 2 ) = _REAL (Read)
*        A real array, without constraints. [1.0,2.0]
*     C = _INTEGER (Read)
*        An integer scalar, constrained in the range 1 to 10. [4]
*     D = _DOUBLE (Read)
*        A double-precision scalar, constrained in the range -1.0D0 to
*        1.0D0. [0.5D0]
*     E = _LOGICAL (Read)
*        A logical scalar. [FALSE]
*     F = _CHAR (Read)
*        A character scalar, which can have one of the following values:
*        "CHR", "DAT" "ERR", "NDF", or "PAR".  ["NDF"]
*     G = _CHAR (Read)
*        A character scalar, which can have one of the following values:
*        "BAD", "GOOD" or a positive integer less than 1000. [!]
*     H = _INTEGER (Read)
*        An integer scalar constrained to be odd between 3 and 11. [5]
*     I() = _REAL (Read)
*        An array of reals, constrained to be between 10.0 and 100.
*        [20.0,40.0,75.0]
*     J = _REAL (Read)
*        A real output scalar value, derived from the input values and
*        the number of values in parameter I.
*     K = _INT64 (Read)
*        An integer scalar.
*     MSG_FILTER (Read)
*        The MSG filtering level. [NORM]
*     [parameter_spec]...

*  Examples:
*     PAR_TEST
*        The resulting message reads:
*         "Package NDF is 2.  Is this FALSE?  Calculation gives 234."
*     [routine_example]...

*  [optional_A_task_items]...
*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 October 9 (MJC):
*        Original version.
*     1999 September 17 (AJC):
*        Add MSG filter level
*        Set NULL TRUE on F and G
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! PAR public constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXVAL             ! Maximum number of values for
                                 ! parameter I
      PARAMETER ( MAXVAL = 5 )

*  Local Variables:
      INTEGER A                  ! Integer scalar
      INTEGER ACTVB              ! Number of B values obtained
      INTEGER ACTVI              ! Number of I values obtained
      REAL B( 2 )                ! Real array
      INTEGER C                  ! Integer scalar between 1 and 10.
      DOUBLE PRECISION D         ! Double-precision scalar between -1.D0
                                 ! and 1.D0
      LOGICAL E                  ! Logical scalar
      CHARACTER * 3 F            ! Character scalar chosen from a menu
      CHARACTER * ( 4 ) G        ! Character scalar chosen from a menu,
                                 ! but also may be a positive integer
                                 ! less than 1000
      INTEGER H                  ! Integer scalar constrained to be odd,
                                 ! and between 3 and 11
      REAL I( MAXVAL )           ! Vector of reals between 10 and 100.
      INTEGER IVALUE             ! Loop counter for I values
      REAL J                     ! Result of the calculation
      INTEGER*8 K                ! Value of parameter K

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get MSG filter level
      CALL MSG_IFGET( STATUS )

*  Obtain an integer scalar.
      CALL PAR_GET0I( 'A', A, STATUS )

*  Obtain an integer scalar.
      CALL PAR_GET1R( 'B', 2, B, ACTVB, STATUS )

*  Obtain an integer scalar between 1 and 10.
      CALL PAR_GDR0I( 'C', 4, 1, 10, .FALSE., C, STATUS )

*  Obtain an integer*8 scalar between -10 and 10.
      CALL PAR_GDR0K( 'K', 4, -10, 10, .FALSE., K, STATUS )

*  Obtain an double-precision scalar between -1 and 1.
      CALL PAR_GDR0D( 'D', 0.5D0, -1D0, 1.D0, .FALSE., D, STATUS )

*  Obtain an logical scalar, which defaults to .FALSE. when a null is
*  entered.
      CALL PAR_GTD0L( 'E', .FALSE., .TRUE., E, STATUS )

*  Obtain a character value from a menu.
      CALL PAR_CHOIC( 'F', 'NDF', 'CHR,DAT,ERR,NDF,PAR', .TRUE., F,
     :                STATUS )

*  Obtain a character value from a menu.
      CALL PAR_MIX0I( 'G', '2', 1, 999, 'BAD,GOOD', .TRUE., G, STATUS )

*  Obtain an odd integer scalar between 3 and 11.
      CALL PAR_GODD( 'H', 5, 3, 11, .FALSE., H, STATUS )

*  Obtain up to MAXVAL real values in a vector.
      CALL PAR_PROMT( 'I', 'Give up to 5 values for I', STATUS )
      CALL PAR_GDRVR( 'I', MAXVAL, 10.0, 100., I, ACTVI, STATUS )

*  Perform the calculation and report if nothing has gone wrong.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Evaluate J.
         J = 0.0
         DO IVALUE = 1, ACTVI
            J = J + I( IVALUE )
         END DO
         J = REAL( A + C ) / B( 1 ) + B( 2 ) / REAL( D ) + REAL( H ) *
     :       J / REAL( ACTVI ) + K

*  Create some message tokens.
         CALL MSG_SETL( 'E', E )
         CALL MSG_SETC( 'F', F )
         CALL MSG_SETC( 'G', G )
         CALL MSG_SETR( 'J', J )

*  Write the message.
         CALL MSG_OUT( 'TEST_RESULT',
     :     'Package ^F is ^G.  Is this ^E?  Calculation gives ^J.',
     :     STATUS )

*  Write the output parameter.
         CALL PAR_PUT0R( 'J', J, STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PAR_TEST_ERR',
     :     'PAR_TEST: Unable to complete the test of the installation '/
     :     /'of the PAR library.', STATUS )
      END IF

      END
