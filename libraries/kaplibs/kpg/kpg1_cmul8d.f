      SUBROUTINE KPG1_CMUL8D( BAD, EL, A, CONST, B, NBAD, STATUS )
*+
*  Name:
*     KPG1_CMUL8D

*  Purpose:
*     Multiplies each element of a vectorised double precision array by
*     a constant.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_CMUL8D( BAD, EL, A, CONST, B, NBAD, STATUS )

*  Description:
*     The routine multiplies each element of a vectorised double
*     precision array by a constant to produce a new double precision
*     array. Bad value checking is performed if required.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether to check for bad values in the input array.
*     EL = INTEGER*8 (Given)
*        Number of array elements to process.
*     A( EL ) = DOUBLE PRECISION (Given)
*        Input array.
*     CONST = DOUBLE PRECISION (Given)
*        Constant by which each array element is to be multiplied.
*     B( EL ) = DOUBLE PRECISION (Returned)
*        Output array.
*     NBAD = INTEGER*8 (Returned)
*        Number of bad values in the output array B.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine is intended for processing double-precision data
*     only. There is a related generic routine for processing other data
*     types.
*     -  This routine will handle numerical errors (i.e. overflow) by
*     assigning the appropriate "bad" value to affected output array
*     elements.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
*     Copyright (C) 1996, 2004 Central Laboratory of the Research
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-APR-1990 (RFWS):
*        Original version.
*     11-MAR-1991 (RFWS):
*        Changed to return the total number of bad pixels in the output
*        array.
*     11-MAR-1991 (RFWS):
*        Derived a separate version for handling double precision
*        arrays.
*     1996 May 20 (MJC):
*        Replaced LIB$ESTABLISH and LIB$REVERT calls.
*     2004 Oct 1 (TIMJ):
*        No longer use NUM_ERROR directly
*     20-FEB-2020 (DSB):
*        Support huge arrays.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Global Variables:

*  Arguments Given:
      LOGICAL BAD
      INTEGER*8 EL
      DOUBLE PRECISION A( EL )
      DOUBLE PRECISION CONST

*  Arguments Returned:
      DOUBLE PRECISION B( EL )
      INTEGER*8 NBAD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL NUM_TRAP
      INTEGER NUM_TRAP           ! Numerical error handler
      EXTERNAL NUM_WASOK
      LOGICAL NUM_WASOK          ! Was numeric operation ok?

*  Local Variables:
      INTEGER*8 I                ! Loop counter for array elements

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Establish a numerical error handler and initialise the error flag
*  and error count.
      CALL NUM_HANDL( NUM_TRAP )
      CALL NUM_CLEARERR()
      NBAD = 0

*  No bad values present:
*  =====================
      IF ( .NOT. BAD ) THEN

*  Multiply the array by the constant.
         DO 1 I = 1, EL
            B( I ) = CONST * A( I )

*  Check for numerical errors (i.e. overflow). If present, then assign
*  a bad value to the output array element and count the error. Reset
*  the numerical error flag.
            IF ( .NOT. NUM_WASOK() ) THEN
               B( I ) = VAL__BADD
               NBAD = NBAD + 1
               CALL NUM_CLEARERR()
            END IF
 1       CONTINUE

*  Bad values present:
*  ==================
      ELSE

*  If the input array element is bad, then so is the output element.
         DO 2 I = 1, EL
            IF ( A( I ) .EQ. VAL__BADD ) THEN
               B( I ) = VAL__BADD
               NBAD = NBAD + 1

*  Otherwise, multiply by the constant, again checking for numerical
*  errors.
            ELSE
               B( I ) = CONST * A( I )
               IF ( .NOT. NUM_WASOK() ) THEN
                  B( I ) = VAL__BADD
                  NBAD = NBAD + 1
                  CALL NUM_CLEARERR()
               END IF
            END IF
 2       CONTINUE
      END IF

*  Remove the numerical error handler.
      CALL NUM_REVRT

      END
