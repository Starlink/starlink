      SUBROUTINE KPS1_ERRCL( LIMIT, TYPE, EL, DIN, VIN, DOUT, VOUT, BAD,
     :                       NBAD, STATUS )
*+
*  Name:
*     KPS1_ERRCL

*  Purpose:
*     Rejects DATA and VARIANCE values with large errors.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_ERRCL( LIMIT, TYPE, EL, DIN, VIN, DOUT, VOUT, BAD, NBAD,
*                      STATUS )

*  Description:
*     This routine produces copies of the input DATA and VARIANCE
*     arrays in which pixels with errors greater than a specified limit
*     are set invalid.  The error limit may be specified as the maximum
*     acceptable standard deviation (or variance), or the minimum
*     acceptable signal-to-noise ratio.

*  Arguments:
*     LIMIT = DOUBLE PRECISION (Given)
*        The error limit.
*     TYPE = CHARACTER * ( * ) (Given)
*        The type of limit supplied: 'SIGMA', 'VARIANCE' or 'SNR'.
*     EL = INTEGER (Given)
*        Number of elements in each array.
*     DIN( EL ) = DOUBLE PRECISION (Given)
*        Input DATA array.
*     VIN( EL ) = DOUBLE PRECISION (Given)
*        Input VARIANCE array.
*     DOUT( EL ) = DOUBLE PRECISION (Returned)
*        Output DATA array.
*     VOUT( EL ) = DOUBLE PRECISION (Returned)
*        Output VARIANCE array.
*     BAD = LOGICAL (Returned)
*        It is .TRUE. if there are any bad values in the returned arrays.
*     NBAD = INTEGER (Returned)
*        Number of pixels rejected by this routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie ({affiliation})
*     {enter_new_authors_here}

*  History:
*     16-SEP-1993 (DSB):
*        Original version.
*     1995 April 11 (MJC):
*        Fixed bug in SNR comparison, where the variance and not the
*        standard deviation was used.  Corrected typo's and other
*        tidying.  Used a modern-style variable declaration.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      DOUBLE PRECISION LIMIT
      CHARACTER * ( * ) TYPE
      INTEGER EL
      DOUBLE PRECISION DIN( EL )
      DOUBLE PRECISION VIN( EL )

*  Arguments Returned:
      DOUBLE PRECISION DOUT( EL )
      DOUBLE PRECISION VOUT( EL )
      LOGICAL BAD
      INTEGER NBAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION DATVAL    ! Data value
      INTEGER I                  ! Element count
      DOUBLE PRECISION VARVAL    ! Variance value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned bad-pixel flag.
      BAD = .FALSE.

*  Initialise the number of pixels set bad by this routine.
      NBAD = 0

*  Process the arrays according to the limit specified.

*  Handle standard-deviation limits.
*  =================================
      IF ( TYPE .EQ. 'SIGMA' ) THEN

*  Loop round each element of the arrays.
         DO I = 1, EL

*  Get the input values.
            VARVAL = VIN( I )
            DATVAL = DIN( I )

*  If neither are invalid, proceed.
            IF ( VARVAL .NE. VAL__BADD .AND.
     :           DATVAL .NE. VAL__BADD ) THEN

*  Compare the variance with the limit, and store bad values in the
*  output arrays if it is too high.
               IF ( SQRT( MAX( 0.0D0, VARVAL ) ) .GT. LIMIT ) THEN
                  DOUT( I ) = VAL__BADD
                  VOUT( I ) = VAL__BADD
                  BAD = .TRUE.
                  NBAD = NBAD + 1

*  Otherwise, store the input values in the output arrays.
               ELSE
                  DOUT( I ) = DATVAL
                  VOUT( I ) = VARVAL

               END IF

*  If one of the input values were bad, put both bad in the output
*  arrays and set the returned bad pixel flag.
            ELSE
               DOUT( I ) = VAL__BADD
               VOUT( I ) = VAL__BADD
               BAD = .TRUE.
            END IF

         END DO

*  Handle VARIANCE limits.
*  =======================
      ELSE IF ( TYPE .EQ. 'VARIANCE' ) THEN

*  Loop round each element of the arrays.
         DO I = 1, EL

*  Get the input values.
            VARVAL = VIN( I )
            DATVAL = DIN( I )

*  If neither are invalid, proceed.
            IF ( VARVAL .NE. VAL__BADD .AND.
     :           DATVAL .NE. VAL__BADD ) THEN

*  Compare the variance with the limit, and store bad values in the
*  output arrays if it is too high.
               IF ( VARVAL .GT. LIMIT ) THEN
                  DOUT( I ) = VAL__BADD
                  VOUT( I ) = VAL__BADD
                  BAD = .TRUE.
                  NBAD = NBAD + 1

*  Otherwise, store the input values in the output arrays.
               ELSE
                  DOUT( I ) = DATVAL
                  VOUT( I ) = VARVAL

               END IF

*  If one of the input values were bad, put both bad in the output
*  arrays and set the returned bad pixel flag.
            ELSE
               DOUT( I ) = VAL__BADD
               VOUT( I ) = VAL__BADD
               BAD = .TRUE.
            END IF

         END DO

*  Handle signal-to-noise limits.
*  ==============================
      ELSE IF ( TYPE .EQ. 'SNR' ) THEN

*  Loop round each element of the arrays.
         DO I = 1, EL

*  Get the input values.
            VARVAL = VIN( I )
            DATVAL = DIN( I )

*  If neither are invalid, proceed.
            IF ( VARVAL .NE. VAL__BADD .AND.
     :           DATVAL .NE. VAL__BADD ) THEN

*  Compare the SNR with the limit, and store bad values in the
*  output arrays if it is too low.
               IF ( ABS( DATVAL ) .LT. ABS( SQRT( MAX( 0.0D0, VARVAL ) )
     :              * LIMIT ) ) THEN
                  DOUT( I ) = VAL__BADD
                  VOUT( I ) = VAL__BADD
                  BAD = .TRUE.
                  NBAD = NBAD + 1

*  Otherwise, store the input values in the output arrays.
               ELSE
                  DOUT( I ) = DATVAL
                  VOUT( I ) = VARVAL

               END IF

*  If one of the input values were bad, put both bad in the output
*  arrays and set the returned bad pixel flag.
            ELSE
               DOUT( I ) = VAL__BADD
               VOUT( I ) = VAL__BADD
               BAD = .TRUE.
            END IF

         END DO

*  Report an error if the limit type is unknown.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', TYPE )
         CALL ERR_REP( 'KPS1_ERRCL_ERR1', 'KPS1_ERRCL: Unknown limit'/
     :                 /' type ''^TYPE'' (programming error).', STATUS )
      END IF

      END
