      INTEGER*8 FUNCTION VAL_ABSK( BAD, ARG, STATUS )
*+
*  Name:
*     VAL_ABSK

*  Purpose:
*     Evaluate the INTEGER*8 Fortran ABS function.

*  Language:
*     Starlink Fortran

*  Invocation:
*     RESULT = VAL_ABSK( BAD, ARG, STATUS )

*  Description:
*     The routine evaluates the Fortran ABS function for a single
*     argument of type INTEGER*8.  If a numerical error occurs, the
*     value VAL__BADK is returned and a STATUS value is set.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether the argument value (ARG) may be "bad".
*     ARG = INTEGER*8 (Given)
*        The INTEGER*8 argument of the Fortran ABS function.
*     STATUS = INTEGER (Given & Returned)
*        This should be set to SAI__OK on entry, otherwise the routine
*        returns immediately with the result VAL__BADK.  A STATUS
*        value will be set by this routine if a numerical error occurs.

*  Returned Value:
*     VAL_ABSK = INTEGER*8
*        Returns the evaluated Fortran ABS function result as an
*        INTEGER*8 value.  The value VAL__BADK will be returned under
*        error conditions.

*  Copyright:
*     Copyright (C) 1987, 1991 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     BKM: Brian McIlwrath (STARRLINK)
*     MJC: Malcolm J. Currie (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     15-AUG-1987 (RFWS):
*        Original version.
*     28-OCT-1991 (RFWS):
*        Added LIB$REVERT call.
*     7-NOV-1991 (RFWS):
*        Changed to use NUM_TRAP.
*     27-SEP-1995 (BKM):
*        Changed LIB$ESTABLISH and LIB$REVERT calls to NUM_HANDL and NUM_REVRT
*     2012 May 10 (MJC):
*        Adapted from VAL_ABSI.
*     22-FEB-2022 (DSB):
*        Changed error handling to use NUM_TEST
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRM_ public constants
      INCLUDE 'PRM_CONST'        ! PRM_ private constants
      INCLUDE 'PRM_ERR'          ! PRM_ public constants

*  Arguments Given:
      LOGICAL BAD                ! Bad data flag
      INTEGER*8 ARG              ! Function argument

*  Status:
      INTEGER STATUS             ! Error status

*  External References:
      LOGICAL NUM_TEST           ! Error testing routine

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! Declare NUM_ conversion functions

      INCLUDE 'NUM_DEC_K'      ! Declare NUM_ arithmetic functions

      INCLUDE 'NUM_DEF_CVT'      ! Define NUM_ conversion functions

      INCLUDE 'NUM_DEF_K'      ! Define NUM_ arithmetic functions

*.

*  Check status.  Return the function result VAL__BADK if not OK.
      IF( STATUS .NE. SAI__OK ) THEN
         VAL_ABSK = VAL__BADK

*  If the bad data flag is set, check if the argument given is bad.
*  Return VAL__BADK if it is.
      ELSE IF( BAD .AND. ( ARG .EQ. VAL__BADK ) ) THEN
         VAL_ABSK = VAL__BADK

*  Check if the argument value is acceptable.  If not, return the
*  result VAL__BADK and set a STATUS value.
      ELSE IF( .NOT. ( BAD .OR. NUM_NEK( ARG, NUM__MINK ) ) ) THEN
         VAL_ABSK = VAL__BADK
         STATUS = PRM__INTOF

*  If the argument value is acceptable...
      ELSE

*  Evaluate the function.
         VAL_ABSK = NUM_ABSK( ARG )

*  If an error handler is established, check if the numerical error
*  flag is set.  If so, return the result VAL__BADK and set STATUS to
*  PRM__FPERR.
         IF( .FALSE. ) THEN
            IF( NUM_TEST() ) THEN
               VAL_ABSK = VAL__BADK
               STATUS = PRM__FPERR
            ENDIF

         ENDIF
      ENDIF

*  Exit routine.
      END
