      SUBROUTINE KPG1_HSTAI( BAD, EL, DATA, NUMBIN, NUMPER, PERCNT,
     :                         NGOOD, IMIN, DMIN, IMAX, DMAX, SUM, MEAN,
     :                         MEDIAN, MODE, PERVAL, HIST, STATUS )
*+
*  Name:
*     KPG1_HSTAx
 
*  Purpose:
*     Compute simple ordered statistics for an array via an histogram.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_HSTAX( BAD, EL, DATA, NUMBIN, NUMPER, PERCNT, NGOOD,
*                      IMIN, DMIN, IMAX, DMAX, SUM, MEAN, MEDIAN, MODE,
*                      PERVAL, HIST, STATUS )
 
*  Description:
*     This routine computes simple ordered statistics for an array,
*     namely: the number of valid pixels, the minimum and maximum pixel
*     values (and their positions), the pixel sum, the mean, the mode,
*     the median, and selected percentiles.  For efficiency reasons
*     the routine computes an histogram, rather than completely
*     sorting the data.  The accuracy of the statistics therefore
*     depends inversely on the number of bins.
 
*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether checks for bad pixels should be performed on the array
*        being analysed.
*     EL = INTEGER (Given)
*        Number of pixels in the array.
*     DATA( EL ) = ? (Given)
*        Array to be analysed.
*     NUMBIN = INTEGER (Given)
*        Number of histogram bins.  This should be moderately large,
*        say at least 1000.
*     NUMPER = INTEGER (Given)
*        Number of percentiles to evaluate.
*     PERCNT( NUMPER ) = REAL (Given and Returned)
*        The array of percentiles to evaluate.  They must in the range
*        0.0 to 100.0.  If there are none to calculate, set NUMPER to 1
*        and pass the bad value in PERCNT( 1 ).  On exit these are
*        placed in ascending order.
*     NGOOD = INTEGER (Returned)
*        Number of valid pixels in the array.
*     IMIN = INTEGER (Returned)
*        Index where the pixel with the lowest value was (first) found.
*     DMIN = DOUBLE PRECISION (Returned)
*        Minimum pixel value in the array.
*     IMAX = INTEGER (Returned)
*        Index where the pixel with the highest value was (first) found.
*     DMAX = DOUBLE PRECISION (Returned)
*        Maximum pixel value in the array.
*     SUM = DOUBLE PRECISION (Returned)
*        Sum of the valid pixels.
*     MEAN = DOUBLE PRECISION (Returned)
*        Mean of the valid pixels.
*     MEDIAN = DOUBLE PRECISION (Returned)
*        Median of the valid pixels.
*     MODE = DOUBLE PRECISION (Returned)
*        Mode of the valid pixels.
*     PERVAL( NUMPER ) = DOUBLE PRECISION (Returned)
*        Percentile values of the valid pixels.  These correspond to the
*        ordered fractions returned in PERCNT.
*     HIST( NUMBIN ) = INTEGER (Returned)
*        The histogram of pixel values between their minimum and
*        maximum.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for each of the standard numeric types.
*     Replace "x" in the routine name by D, R, I, W, UW, B or UB as
*     appropriate. The data type of the array being analysed must match
*     the particular routine used.
*     -  If NGOOD is zero, then the values of all the derived
*     statistics will be undefined and will be set to the "bad" value
*     appropriate to their data type (except for the pixel sum, which
*     will be zero).
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1991 November 13 (MJC):
*        Original version
*     1994 September 27 (MJC):
*        Used modern subroutine prefixes, standardised comment
*        alignment,and sorted the variables.
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
      INTEGER NUMBIN
      INTEGER NUMPER
      REAL PERCNT( NUMPER )
      INTEGER DATA( EL )
 
*  Arguments Returned:
      INTEGER NGOOD
      INTEGER IMIN
      DOUBLE PRECISION DMIN
      INTEGER IMAX
      DOUBLE PRECISION DMAX
      DOUBLE PRECISION SUM
      DOUBLE PRECISION MEAN
      DOUBLE PRECISION MEDIAN
      DOUBLE PRECISION MODE
      DOUBLE PRECISION PERVAL( NUMPER )
      INTEGER HIST( NUMBIN )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      LOGICAL BAD2               ! There are bad pixels in the array or
                                 ! not
      INTEGER I                  ! Loop counter
      INTEGER NINVAL             ! Number of bad values in the array
      INTEGER MAXMUM             ! Maximum data value
      INTEGER MINMUM             ! Minimum data value
 
*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ definitions for type conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM_ declarations for type
                                 ! conversions
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
* Initialise the statistics to the bad values, particularly in case
* there are no good values within the array.
      SUM = 0.0D0
      MEAN = VAL__BADD
      MEDIAN = VAL__BADD
      MODE = VAL__BADD
      DMAX = VAL__BADD
      DMIN = VAL__BADD
      IMAX = VAL__BADI
      IMIN = VAL__BADI
      DO I = 1, NUMPER
         PERVAL( I ) = VAL__BADD
      END DO
 
*  Obtain the maximum and minimum values to define the bounds of the
*  histogram.  Convert to the standard double precision data type
*  needed later and for ease of display.
      CALL KPG1_MXMNI( BAD, EL, DATA, NINVAL, MAXMUM, MINMUM,
     :                   IMAX, IMIN, STATUS )
      DMAX = NUM_ITOD( MAXMUM )
      DMIN = NUM_ITOD( MINMUM )
 
*  Derive the number of good pixels.
      NGOOD = EL - NINVAL
      IF ( NGOOD .GT. 0 ) THEN
 
*  The number of bad pixels has been counted so it might be possible to
*  save future processing.
         BAD2 = NINVAL .NE. 0
 
*  Generate the histogram between those bounds.
         CALL KPG1_GHSTI( BAD2, EL, DATA, NUMBIN, MAXMUM, MINMUM,
     :                      HIST, STATUS )
 
*  Calculate the sum, mean, median and mode from the histogram.
         CALL KPG1_HSSTP( NUMBIN, HIST, DMAX, DMIN, SUM, MEAN, MEDIAN,
     :                    MODE, STATUS )
 
*  If there are percentiles to evaluate call a routine to do it,
*  temporarily converting to fractions, as required by the routine.
*  Otherwise return a flagged value.
         IF ( NUMPER .EQ. 1 .AND. PERCNT( 1 ) .EQ. VAL__BADR ) THEN
            PERVAL( 1 ) = VAL__BADD
         ELSE
            DO  I = 1, NUMPER
               PERCNT( I ) = PERCNT( I ) * 0.01
            END DO
 
            CALL KPG1_HSTFD( NUMBIN, HIST, DMAX, DMIN, NUMPER, PERCNT,
     :                       PERVAL, STATUS )
 
            DO  I = 1, NUMPER
               PERCNT( I ) = PERCNT( I ) * 100.0
            END DO
         END IF
      END IF
 
      END
