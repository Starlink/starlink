      SUBROUTINE POL1_SNGVR( DIM1, DIM2, SQRES, EXDATA, WGT, DMAX, DMIN,
     :                       VAR, WORK1, WORK2, NOISE, STATUS )
*+
*  Name:
*     POL1_SNGVR

*  Purpose:
*     Calculate variance estimates for the supplied data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SNGVR( DIM1, DIM2, SQRES, EXDATA, WGT, DMAX, DMIN,
*                      VAR, WORK1, WORK2, NOISE, STATUS )

*  Description:
*     This routine returns estimates of the variance associated with each
*     intensity value.
*
*     The supplied squared residuals are smoothed using a weighted 7x7
*     pixel mean box filter. These mean squared residual values are used
*     as the variance estimates.

*  Arguments:
*     DIM1 = INTEGER (Given)
*        The first dimension of the data array.
*     DIM2 = INTEGER (Given)
*        The first dimension of the data array.
*     SQRES( DIM1, DIM2 ) = REAL (Given)
*        The squared residual associated with each data value. The
*        residual is the difference between the data value read from the
*        input NDF and the data value estimated from the current Stokes
*     EXDATA( DIM1, DIM2 ) = REAL (Given)
*        The expected data values implied by the current smoothed Stokes
*        vectors.
*     WGT( DIM1, DIM2 ) = REAL (Given)
*        The weights to assign to each value in SQRES.
*     DMAX = REAL (Given)
*        The highest value in EXDATA.
*     DMIN = REAL (Given)
*        The lowest value in EXDATA.
*     VAR( DIM1, DIM2 ) = REAL (Returned)
*        The variance estimate associated with each EXDATA value.
*     WORK1( DIM1 ) = DOUBLE PRECISION (Returned)
*        A work array.
*     WORK2( DIM1 ) = DOUBLE PRECISION (Returned)
*        A work array.
*     NOISE = REAL (Returned)
*        An estimate of the standard deviation of the noise in the
*        background regions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAR-1999 (DSB):
*        Original version.
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
      INTEGER DIM1
      INTEGER DIM2
      REAL SQRES( DIM1, DIM2 )
      REAL EXDATA( DIM1, DIM2 )
      REAL WGT( DIM1, DIM2 )
      REAL DMAX
      REAL DMIN

*  Arguments Returned:
      REAL VAR( DIM1, DIM2 )
      DOUBLE PRECISION WORK1( DIM1 )
      DOUBLE PRECISION WORK2( DIM1 )
      REAL NOISE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NBIN               ! Histogram size
      PARAMETER ( NBIN = 1000 )

*  Local Variables:
      INTEGER I                  ! Loop count
      INTEGER J                  ! Index of largest weight found so far
      REAL DELTA                 ! Bin size
      REAL WBIN( NBIN )          ! Weight for each bin
      REAL WMAX                  ! Largest weight found so far
      REAL WTOT                  ! Total weight in histogram
      REAL YBIN( NBIN )          ! Mean squared residual for each bin

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Smooth the squared residuals to get the variance estimates.
      CALL POL1_BLKWR( DIM1, DIM2, SQRES, WGT, 3, 3, VAL__EPSR,
     :                 VAR, WORK1, WORK2, STATUS )

*  Form a histogram of variance against expected data value.
      CALL POL1_HIST2( DIM1*DIM2, EXDATA, VAR, WGT, NBIN, DMIN, DMAX,
     :                 YBIN, WBIN, DELTA, WTOT, STATUS )

*  Find the histogram bin with the largest weight.
      J = 1
      WMAX = 0.0
      DO I = 1, NBIN
         IF( WBIN( I ) .GT. WMAX ) THEN
            WMAX = WBIN( I )
            J = I
         END IF
      END DO

*  Return the standard deviation in this bin as an estimate of the
*  background noise.
      NOISE = SQRT( MAX( 0.0, YBIN( J ) ) )

      END
