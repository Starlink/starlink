      SUBROUTINE CCG1_UMD1D( STACK, NPIX, NLINES, VARS, MINPIX,
     :                         RESULT, RESVAR, NCON, STATUS )
*+
*  Name:
*     CCG1_UMD1D

*  Purpose:
*     Combines data lines using an unweighted mean.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCG1_UMD1D( STACK, NPIX, NLINES, VARS, MINPIX,
*                        RESULT, RESVAR, NCON, STATUS )

*  Description:
*     This routine accepts an array consisting of a series of
*     (vectorised) lines of data. The data values in the lines are then
*     combined to form an unweighted mean. The output means
*     are returned in the array RESULT. The output variances are
*     estimated from the VARS values and are returned in RESVAR.

*  Notes
*     - this routine performs its work in double precision, but the
*     output is returned in single precision, therefore it should only
*     be used with data whose outputs are in the range of single
*     precision values. It accepts the data in any of the non-complex
*     formats as supported by PRIMDAT, the variances must be given in
*     double precision.

*  Arguments:
*     STACK( NPIX, NLINES ) = DOUBLE PRECISION (Given)
*        The array of lines which are to be combined into a single line.
*     NPIX = INTEGER (Given)
*        The number of pixels in a line of data.
*     NLINES = INTEGER (Given)
*        The number of lines of data in the stack.
*     VARS( NPIX, NLINES ) = DOUBLE PRECISION (Given)
*        The data variances.
*     MINPIX = INTEGER (Given)
*        The minimum number of pixels required to contribute to an
*        output pixel.
*     RESULT( NPIX ) = DOUBLE PRECISION (Returned)
*        The output line of data.
*     RESVAR( NPIX ) = DOUBLE PRECISION (Returned)
*        The output variances.
*     NCON( NLINES ) = DOUBLE PRECISION (Given and Returned)
*        The actual number of contributing pixels from each input line
*        to the output line.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     BRADC: Brad Cavanagh (JAC)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-2001 (DSB):
*        Original version.
*     11-OCT-2004 (BRADC):
*        No longer use NUM_CMN.
*     2005 December 27 (MJC):
*        Remove unused variables.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      INTEGER NPIX
      INTEGER NLINES
      INTEGER MINPIX
      DOUBLE PRECISION STACK( NPIX, NLINES )
      DOUBLE PRECISION VARS( NPIX, NLINES )

*  Arguments Given and Returned:
      DOUBLE PRECISION NCON( NLINES )

*  Arguments Returned:
      DOUBLE PRECISION RESULT( NPIX )
      DOUBLE PRECISION RESVAR( NPIX )

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables:


*  External References:
      EXTERNAL NUM_WASOK
      LOGICAL NUM_WASOK          ! Was numeric operation ok?
      EXTERNAL NUM_TRAP
      INTEGER NUM_TRAP           ! Numerical error handler

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      INTEGER NGOOD              ! Number of good pixels
      DOUBLE PRECISION SUM1      ! Sum of weights
      DOUBLE PRECISION SUM2      ! Sum of weighted values

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEF_CVT'      ! Define functions...

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the numeric error and set error flag value.
      CALL NUM_HANDL( NUM_TRAP )

      DO 1 I = 1, NPIX

*  Loop over for all possible output pixels.
         SUM1 = 0.0D0
         SUM2 = 0.0D0
         CALL NUM_CLEARERR()
         NGOOD = 0

*  Loop over all possible contributing pixels forming weighted mean
*  sums.
         DO 5 J = 1, NLINES
            IF( STACK( I, J ) .NE. VAL__BADD .AND.
     :           VARS( I, J ) .NE. VAL__BADD ) THEN

*  Conversion increment good value counter.
               NGOOD = NGOOD + 1

*  Sum variances.
               SUM1 = SUM1 + NUM_DTOD( VARS( I, J ) )

*  Sum data values
               SUM2 = SUM2 + NUM_DTOD( STACK( I, J ) )

*  Update the contribution buffer - all values contribute when forming
*  mean.
               NCON( J ) = NCON( J ) + 1.0D0
            END IF
 5       CONTINUE

*  If there are sufficient good pixels output the result.
         IF ( NGOOD .GE. MINPIX ) THEN
            RESULT( I ) = SUM2 / NGOOD 
            RESVAR( I ) = SUM1 / (NGOOD*NGOOD)

*  Trap numeric errors.
            IF ( .NOT. NUM_WASOK() ) THEN
               RESULT( I ) = VAL__BADD
               RESVAR( I ) = VAL__BADD
            END IF
         ELSE

*  Not enough contributing pixels, set output invalid.
            RESULT( I ) = VAL__BADD
            RESVAR( I ) = VAL__BADD
         END IF

 1    CONTINUE

*  Remove the numerical error handler.
      CALL NUM_REVRT

      END
