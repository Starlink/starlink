      SUBROUTINE FLA_OPEXT( DIM1, DIM2, INARR, INVAR, WEIGHT,
     :                      NFIBRE, OUTARR, OUTVAR, STATUS )
*+
*  Name:
*     FLA_OPEXT

*  Purpose:
*     Performs an optimal extraction and reconfiguration for a FLAIR
*     frame.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FLA_OPEXT( DIM1, DIM2, INARR, INVAR, WEIGHT, NFIBRE,
*                     OUTARR, OUTVAR, STATUS )

*  Description:
*     This routine extracts FLAIR spectra from two-dimensional images
*     and stores them in an output array configured with increasing
*     wavelength along the x axis, and a spectrum per line.  It performs
*     an optimal extraction using Horne's algorithm (1986, PASP 98,
*     609) and an average profile of the relative response of the fibres
*     in each column.  The spectra are delineated by bad values in the
*     profile array.

*  Arguments:
*     DIM1 = INTEGER (Given)
*        The first dimension of the input data array and length of the
*        weight array.
*     DIM2 = INTEGER (Given)
*        The second dimension of the input data array.
*     INARR( DIM1, DIM2 ) = REAL (Given)
*        The data array whose spectra are to be extracted optimally and
*        reconfigured.  Dispersion is along the y-axis and wavelength
*        decreases as y increases.
*     INVAR( DIM1, DIM2 ) = REAL (Given)
*        The variance array associated with the input data array.
*     WEIGHT( DIM1 ) = REAL (Given)
*        The profile or weights to be used during an optimal extraction.
*        Bad values must separate the individual spectra.
*     NFIBRE = INTEGER (Given)
*        The number of fibres to extract, and the second dimension of
*        the output arrays.
*     OUTARR( DIM2, NFIBRE ) = REAL (Returned)
*        The array of extracted spectra.
*     OUTVAR( DIM2, NFIBRE ) = REAL (Returned)
*        The variance associated with the array of extracted spectra.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     ACD: A C Davenhall (Edinburgh)
*     {enter_new_authors_here}

*  History:
*     1993 April 6 (MJC):
*        Original version.
*     1993 May 13 (MJC):
*        No longer flips in x direction.
*     1998 October 26 (ACD)
*        Replaced explict directory specification for INCLUDE files
*        with the standard upper-case softlinks.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_PAR'
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*  Arguments Given:
      INTEGER DIM1
      INTEGER DIM2
      INTEGER NFIBRE
      REAL INARR( DIM1, DIM2 )
      REAL INVAR( DIM1, DIM2 )
      REAL WEIGHT( DIM1 )

*  Arguments Returned:
      REAL OUTARR( DIM2, NFIBRE )
      REAL OUTVAR( DIM2, NFIBRE )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXFIB             ! Maximum number of fibres
      PARAMETER ( MAXFIB = 92 )

*  Local Variables:
      INTEGER COLMAX             ! Maximum column for current extraction
      INTEGER COLMIN             ! Minimum column for current extraction
      INTEGER FIBRE              ! Loop counter for the fibres
      INTEGER I                  ! Loop counter
      INTEGER IOUT               ! Index to output array first dimension
      INTEGER J                  ! Loop counter
      LOGICAL LOOP               ! Loop while true
      REAL SUM                   ! Summation counter
      REAL SUMW                  ! Summation of the weights

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the last column to be included in an extraction.
      IF ( WEIGHT( 1 ) .EQ. VAL__BADR ) THEN
         COLMAX = 1
      ELSE
         COLMAX = 0
      END IF

*  Loop for each fibre.
      DO FIBRE = 1, MIN( NFIBRE, MAXFIB )

*  Find the limits of this spectrum by only include the good profile
*  points between the bad. So skip over the bad-pixel delimiter to set
*  the lower limit, and then search subsequent profile values until the
*  next bad value is found.
         COLMIN = COLMAX + 2
         LOOP = .TRUE.
         COLMAX = COLMIN + 1
         DO WHILE ( LOOP .AND. COLMAX .LE. DIM1 )
            IF ( WEIGHT( COLMAX ) .EQ. VAL__BADR ) THEN
               LOOP = .FALSE.
            ELSE
               COLMAX = COLMAX + 1
            END IF
         END DO
         COLMAX = COLMAX - 1

*  Not that this reversal of normal accessing is intended, and so
*  sufficient memory resources are required to prevent very inefficient
*  processing.
         DO J = 1, DIM2
            SUM = 0.0
            SUMW = 0.0
            DO I = COLMIN, COLMAX

*  Do not include pixels with bad data or variance in the summations.
               IF ( INARR( I, J ) .NE. VAL__BADR .AND.
     :              INVAR( I, J ) .NE. VAL__BADR ) THEN

*  Sum the contributions to the weighted spectrum.
                  SUM = WEIGHT( I ) * INARR( I, J ) / INVAR( I, J ) +
     :                  SUM
                  SUMW = WEIGHT( I ) * WEIGHT( I ) / INVAR( I, J ) +
     :                  SUMW
               END IF
            END DO

*  Derive the index of the output elements.  Note that the spectrum is
*  oriented along the x-axis (dispersion was along the y axis in the
*  input array), and it is NOT flipped to have wavelength increasing
*  with pixel index.
            IOUT = J

*  Set the output value to be bad when the sum of the wieghts is zero.
            IF ( SUMW .LT. VAL__SMLR ) THEN
               OUTARR( IOUT, FIBRE ) = VAL__BADR
               OUTVAR( IOUT, FIBRE ) = VAL__BADR

*  Proceed when the sum of weights is not zero.
            ELSE
               OUTARR( IOUT, FIBRE ) = SUM / SUMW
               OUTVAR( IOUT, FIBRE ) = 1.0 / SUMW
            END IF
         END DO
      END DO

      END
