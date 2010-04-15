      SUBROUTINE POL1_HIST( EL, DATA, FRAC, NBIN, EXZERO, HIST,
     :                      DMIN, DMAX, INIT, VALUE, STATUS )
*+
*  Name:
*     POL1_HIST

*  Purpose:
*     Form a histogram of the supplied data values, and return requested
*     percentiles.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_HIST( EL, DATA, FRAC, NBIN, EXZERO, HIST, DMIN, DMAX, INIT,
*                     VALUE, STATUS )

*  Description:
*     This routine forms a normalised histogram of the supplied (good) data
*     values, and returns an estimate of a specified percentile. The returned
*     histogram can be used in subsequent calls to this routine, thus
*     removing the need to recalculate the histogram for each percentile.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of data values.
*     DATA( EL ) = REAL (Given)
*        The data values. Bad values are ignored. Ignored if INIT is
*        supplied equal to .FALSE.
*     FRAC = REAL (Given)
*        The fraction of the supplied data values which are to lie below
*        the returned VALUE. In the range 0.0 to 1.0.
*     NBIN = INTEGER (Given)
*        The number of bins in the histogram array.
*     EXZERO = LOGICAL (Given)
*        If .TRUE. all zero values are excluded from the histogram.
*     HIST( NBIN ) = REAL (Given and Returned)
*        The histogram array. It is assumed to contain a valid histogram on
*        entry if INIT is supplied .FALSE. The sum of all histogram values
*        should be 1.0.
*     DMIN = REAL (Given and Returned)
*        If DMIN is less than DMAX on entry, then it gives the lowest
*        data value to be used in the histogram and is unchanged on exit.
*        Otherwise, the supplied value is ignored, and the value to use is
*        found by searching the supplied DATA array. and this value is
*        returned on exit.
*     DMAX = REAL (Given and Returned)
*        If DMAX is greater than DMIN on entry, then it gives the highest
*        data value to be used in the histogram and is unchanged on exit.
*        Otherwise, the supplied value is ignored, and the value to use is
*        found by searching the supplied DATA array. and this value is
*        returned on exit.
*     INIT = LOGICAL (Given and Returned)
*        Should the histogram array be recalculated? If so, the histogram
*        is calculated and stored in HIST. If not, the histogram supplied
*        in HIST is used. INIT is always returned .FALSE. on exit.
*     VALUE = REAL (Returned)
*        The data value below which the specified fraction (see FRAC) of
*        the histogram values lie.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-FEB-1999 (DSB):
*        Original version.
*     11-AUG-2000 (DSB):
*        Added EXZERO.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER EL
      REAL DATA( EL )
      REAL FRAC
      INTEGER NBIN
      LOGICAL EXZERO

*  Arguments Given and Returned:
      REAL HIST( NBIN )
      LOGICAL INIT
      REAL DMAX
      REAL DMIN

*  Arguments Returned:
      REAL VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Data element index
      INTEGER IBIN               ! Histogram element index
      INTEGER NTOT               ! Total no. of points in histogram
      REAL ACCUM                 ! Accumulated histogram value
      REAL D                     ! Data value
      REAL DELTA                 ! Size of each histogram bin
      REAL LFRAC                 ! Validated FRAC value
      REAL PACCUM                ! Previous accumulated histogram value
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the minimum and maximum data value to be used were not supplied,
*  find the minimum and maximum data values in the supplied DATA array
*  and use these values.
      IF( DMAX .LE. DMIN ) THEN

*  Initialise the limits.
         DMIN = VAL__MAXR
         DMAX = VAL__MINR

*  Check every good data value.
         DO I = 1, EL
            D = DATA( I )
            IF( D .NE. VAL__BADR .AND. (
     :          D .NE. 0.0 .OR. .NOT. EXZERO ) ) THEN

*  Update the limits.
               DMIN = MIN( DMIN, D )
               DMAX = MAX( DMAX, D )

            END IF
         END DO

      END IF

*  If no good data was found, return VAL__BADR.
      IF( DMAX .LT. DMIN ) THEN
         VALUE = VAL__BADR

*  If all data values were equal, return that value
      ELSE IF( DMAX .EQ. DMIN ) THEN
         VALUE = DMIN

*  Otherwise...
      ELSE

*  Calculate the size of each histogram bin.
         DELTA = ( DMAX - DMIN )/REAL( NBIN )

*  If no histogram was supplied, form one now.
         IF( INIT ) THEN

*  Initialise the histogram to hold zero in every bin.
            DO IBIN = 1, NBIN
               HIST( IBIN ) = 0
            END DO

*  Initialise the number of data values included in the histogram.
            NTOT = 0

*  Bin each good data value.
            DO I = 1, EL
               D = DATA( I )
               IF( D .NE. VAL__BADR .AND. (
     :             D .NE. 0.0 .OR. .NOT. EXZERO ) ) THEN
                  IBIN = INT( ( D - DMIN )/DELTA ) + 1
                  IF( IBIN .GE. 1 .AND. IBIN .LE. NBIN ) THEN
                     HIST( IBIN ) = HIST( IBIN ) + 1.0
                     NTOT = NTOT + 1
                  END IF
               END IF
            END DO

*  Normalise the histogram values to a total sum of 1.0.
            DO IBIN = 1, NBIN
               HIST( IBIN ) = HIST( IBIN )/REAL( NTOT )
            END DO

*  Indicate we now have a histogram.
            INIT = .FALSE.

         END IF

*  Initialise the accumulated histogram value.
         ACCUM = 0.0

*  Ensure FRAC is between 0 and 1.
         LFRAC = MAX( 0.0, MIN( 1.0, FRAC ) )

*  Loop through the histogram.
         DO IBIN = 1, NBIN

*  Save the previous accumulated value.
            PACCUM = ACCUM

*  Add on this histogram value to the accumulated histogram value.
            ACCUM = ACCUM + HIST( IBIN )

*  Break out of the loop when we have reached or exceeded the target value.
            IF( ACCUM .GE. LFRAC ) GO TO 10

         END DO

 10      CONTINUE

*  If the target value was not reached, return the maximum data value.
         IF( ACCUM .LT. LFRAC ) THEN
            VALUE = DMAX

*  Otherwise, do linear interpolation to get the required value.
         ELSE IF( ACCUM .GT. PACCUM ) THEN
            VALUE = DMIN + DELTA*( IBIN - 1 +
     :                     ( LFRAC - PACCUM )/( ACCUM - PACCUM ) )

         ELSE IF( ACCUM .GT. PACCUM ) THEN
            VALUE = DMIN + DELTA*IBIN
         END IF

      END IF

      END
