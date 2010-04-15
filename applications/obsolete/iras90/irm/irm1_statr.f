      SUBROUTINE IRM1_STATR( BAD, EL, DATA, NCLIP, CLIP, NGOOD,
     :                       IMIN, DMIN, IMAX, DMAX, SUM, MEAN,
     :                       STDEV, NGOODC, IMINC, DMINC, IMAXC,
     :                       DMAXC, SUMC, MEANC, STDEVC, STATUS )
*+
*  Name:
*     IRM1_STATR

*  Purpose:
*     Compute simple statistics for an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM1_STATR( BAD, EL, DATA, NCLIP, CLIP, NGOOD, IMIN, DMIN,
*                      IMAX, DMAX, SUM, MEAN, STDEV, NGOODC, IMINC,
*                      DMINC, IMAXC, DMAXC, SUMC, MEANC, STDEVC,
*                      STATUS )

*  Description:
*     This routine computes simple statistics for an array, namely: the
*     number of valid pixels, the minimum and maximum pixel values (and
*     their positions), the pixel sum, the mean, and the standard
*     deviation. Iterative K-sigma clipping may also be optionally
*     applied.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether checks for bad pixels should be performed on the array
*        being analysed.
*     EL = INTEGER (Given)
*        Number of pixels in the array.
*     DATA( EL ) = ? (Given)
*        Array to be analysed.
*     NCLIP = INTEGER (Given)
*        Number of K-sigma clipping iterations to apply (may be zero).
*     CLIP( NCLIP ) = REAL (Given)
*        Array of clipping limits for successive iterations, expressed
*        as standard deviations.
*     NGOOD = INTEGER (Returned)
*        Number of valid pixels in the array before clipping.
*     IMIN = INTEGER (Returned)
*        Index where the pixel with the lowest value was (first) found
*        before clipping.
*     DMIN = DOUBLE PRECISION (Returned)
*        Minimum pixel value in the array before clipping.
*     IMAX = INTEGER (Returned)
*        Index where the pixel with the highest value was (first) found
*        before clipping.
*     DMAX = DOUBLE PRECISION (Returned)
*        Maximum pixel value in the array before clipping.
*     SUM = DOUBLE PRECISION (Returned)
*        Sum of the valid pixels before clipping.
*     MEAN = DOUBLE PRECISION (Returned)
*        Mean of the valid pixels before clipping.
*     STDEV = DOUBLE PRECISION (Returned)
*        Standard deviation of the valid pixels before clipping.
*     NGOODC = INTEGER (Returned)
*        Number of valid pixels in the array after clipping.
*     IMINC = INTEGER (Returned)
*        Index where the pixel with the lowest value was (first) found
*        after clipping.
*     DMINC = DOUBLE PRECISION (Returned)
*        Minimum pixel value in the array after clipping.
*     IMAXC = INTEGER (Returned)
*        Index where the pixel with the highest value was (first) found
*        after clipping.
*     DMAXC = DOUBLE PRECISION (Returned)
*        Maximum pixel value in the array after clipping.
*     SUMC = DOUBLE PRECISION (Returned)
*        Sum of the valid pixels after clipping.
*     MEANC = DOUBLE PRECISION (Returned)
*        Mean of the valid pixels after clipping.
*     STDEVC = DOUBLE PRECISION (Returned)
*        Standard deviation of the valid pixels after clipping.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  There is a routine for each of the standard numeric types.
*     Replace "x" in the routine name by D, R, I, W, UW, B or UB as
*     appropriate. The data type of the array being analysed must match
*     the particular routine used.
*     -  If no clipping is performed (i.e. if NCLIP = 0) then the values
*     of arguments which return results after clipping will be the same
*     as for those returning results before clipping.
*     -  If NGOOD or NGOODC is zero, then the values of all the derived
*     statistics will be undefined and will be set to the "bad" value
*     appropriate to their data type (except for the pixel sum, which
*     will be zero).

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-MAR-1992 (DSB):
*        Original version copied from KAPPA routine KPG_$STATx.GEN
*        by RFWS and MJC.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      LOGICAL BAD
      INTEGER EL
      REAL DATA( EL )
      INTEGER NCLIP
      REAL CLIP( * )

*  Arguments Returned:
      INTEGER NGOOD
      INTEGER IMIN
      DOUBLE PRECISION DMIN
      INTEGER IMAX
      DOUBLE PRECISION DMAX
      DOUBLE PRECISION SUM
      DOUBLE PRECISION MEAN
      DOUBLE PRECISION STDEV
      INTEGER NGOODC
      INTEGER IMINC
      DOUBLE PRECISION DMINC
      INTEGER IMAXC
      DOUBLE PRECISION DMAXC
      DOUBLE PRECISION SUMC
      DOUBLE PRECISION MEANC
      DOUBLE PRECISION STDEVC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION LLIM      ! Lower clipping limit
      DOUBLE PRECISION SUMSQ     ! Sum of pixels squared
      DOUBLE PRECISION ULIM      ! Upper clipping limit
      DOUBLE PRECISION VALUE     ! Double precision array value
      INTEGER I                  ! Loop counter for array pixels
      INTEGER ICLIP              ! Loop counter for clipping iterations
      INTEGER NCLP               ! Number of clipping iterations

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion functions
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop through each clipping iteration, plus an initial iteration
*  where no clipping is applied.
      NCLP = MAX( 0, NCLIP )
      DO 3 ICLIP = 0, NCLP

*  Initialise for forming statistics.
         NGOODC = 0
         SUMC = 0.0D0
         SUMSQ = 0.0D0
         DMINC = NUM__MAXD
         DMAXC = NUM__MIND

*  If no clipping is being applied, then loop through the array,
*  selecting all the pixels which are not bad.
         IF ( ICLIP .EQ. 0 ) THEN
            DO 1 I = 1, EL
               IF ( ( .NOT. BAD ) .OR.
     :              ( DATA( I ) .NE. VAL__BADR ) ) THEN

*  Convert the array values to double precision, count them, and form
*  sums for the statistics.
                  VALUE = NUM_RTOD( DATA( I ) )
                  NGOODC = NGOODC + 1
                  SUMC = SUMC + VALUE
                  SUMSQ = SUMSQ + ( VALUE * VALUE )

*  Note the minimum and maximum pixel values and where they occur.
                  IF ( VALUE .LT. DMINC ) THEN
                     DMINC = VALUE
                     IMINC = I
                  END IF
                  IF ( VALUE .GT. DMAXC ) THEN
                     DMAXC = VALUE
                     IMAXC = I
                  END IF
               END IF
 1          CONTINUE

*  If clipping is being applied, then calculate the clipping limits.
         ELSE
            LLIM = MEANC - STDEVC * DBLE( CLIP( ICLIP ) )
            ULIM = MEANC + STDEVC * DBLE( CLIP( ICLIP ) )

*  Loop (as above), selecting all the pixels which are not bad.
            DO 2 I = 1, EL
               IF ( ( .NOT. BAD ) .OR.
     :              ( DATA( I ) .NE. VAL__BADR ) ) THEN

*  Convert the array values to double precision, but this time select
*  only those which lie within the clipping limits.
                  VALUE = NUM_RTOD( DATA( I ) )
                  IF ( ( VALUE .LE. ULIM ) .AND.
     :                 ( VALUE .GE. LLIM ) ) THEN

*  Count them and form sums for the statistics.
                     NGOODC = NGOODC + 1
                     SUMC = SUMC + VALUE
                     SUMSQ = SUMSQ + ( VALUE * VALUE )

*  Note the minimum and maximum pixel values and where they occur.
                     IF ( VALUE .LT. DMINC ) THEN
                        DMINC = VALUE
                        IMINC = I
                     END IF
                     IF ( VALUE .GT. DMAXC ) THEN
                        DMAXC = VALUE
                        IMAXC = I
                     END IF
                  END IF
               END IF
 2          CONTINUE
         END IF

*  If there were no valid pixels, then use null result values.
         IF ( NGOODC .EQ. 0 ) THEN
            IMINC = VAL__BADI
            DMINC = VAL__BADD
            IMAXC = VAL__BADI
            DMAXC = VAL__BADD
            SUMC = 0.0D0
            MEANC = VAL__BADD
            STDEVC = VAL__BADD

*  Otherwise, calculate the pixel mean.
         ELSE
            MEANC = SUMC / DBLE( NGOODC )

*  Before calculating the standard deviation, check for (a) only one
*  contributing pixel, (b) all pixels having the same value and (c)
*  rounding errors producing a negative variance value. In all these
*  cases, calculate a standard deviation value of zero.
            STDEVC = SUMSQ - ( MEANC * MEANC * DBLE( NGOODC ) )
            IF ( ( NGOODC .EQ. 1 ) .OR.
     :           ( DMINC .EQ. DMAXC ) .OR.
     :           ( STDEVC .LT. 0.0D0 ) ) THEN
               STDEVC = 0.0D0

*  Otherwise, calculate the standard deviation normally.
            ELSE
               STDEVC = SQRT( STDEVC / DBLE( NGOODC - 1 ) )
            END IF
         END IF

*  If no clipping was applied on this iteration, then set values for the
*  un-clipped return arguments.
         IF ( ICLIP .EQ. 0 ) THEN
            NGOOD = NGOODC
            IMIN = IMINC
            DMIN = DMINC
            IMAX = IMAXC
            DMAX = DMAXC
            SUM = SUMC
            MEAN = MEANC
            STDEV = STDEVC
         END IF

*  Quit performing clipping iterations if there are no valid pixels
*  left.
         IF ( NGOODC .EQ. 0 ) GO TO 4
 3    CONTINUE
 4    CONTINUE

      END
