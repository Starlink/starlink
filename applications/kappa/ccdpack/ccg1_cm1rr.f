      SUBROUTINE CCG1_CM1RR( STACK, NPIX, NLINES, VARS, COORDS, WIDTHS,
     :                       IMETH, MINPIX, NITER, NSIGMA, ALPHA, RMIN,
     :                       RMAX, RESULT, RESVAR, COIND, WRK1, WRK2,
     :                       PP, COVEC, NMAT, NCON, POINT, USED, 
     :                       STATUS )
*+
*  Name:
*     CCG1_CM1RR

*  Purpose:
*     To combine a stack of array lines into one line, using a variety
*     of methods. REAL version.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCG1_CM1RR( STACK, NPIX, NLINES, VARS, COORDS, WIDTHS, IMETH,
*                      MINPIX, NITER, NSIGMA, ALPHA, RMIN, RMAX, RESULT,
*                      RESVAR, COIND, WRK1, WRK2, PP, COVEC, NMAT, NCON,
*                      POINT, USED, STATUS )

*  Description:
*     The routine works along each line of the input stack of lines,
*     combining the data.  This variant uses a complete variance array
*     propagates them.  All work is done in single precision when
*     possible and double precision when not.  Note that the output
*     arrays are in the processing precision.  The array NCON holds the
*     actual numbers of pixels which were used in deriving the output
*     value plus any values already present in the array; thus a
*     cumulative sum of contributing pixel numbers may be kept.

*  Arguments:
*     STACK( NPIX, NLINES ) = REAL (Given)
*        The array of lines which are to be combined into a single line.
*     NPIX = INTEGER (Given)
*        The number of pixels in a line of data.
*     NLINES = INTEGER (Given)
*        The number of lines of data in the stack.
*     VARS( NPIX, NLINES ) = REAL (Given)
*        The data variances.
*     COORDS( NPIX, NLINES ) = REAL (Given)
*        The co-ordinates along the collapse axis for each pixel.
*        It is accessed only for IMETH = 22, 23, 33, 34.
*     WIDTHS( NPIX, NLINES ) = REAL (Given)
*        The widths along the collapse axis for each pixel.  It is
*        accessed only for IMETH = 21.
*     IMETH = INTEGER (Given)
*        The method to use in combining the lines.  It has a code of 1
*        to 300 which represent the following statistics.
*        1  = Mean
*        2  = Weighted mean
*        3  = Median
*        4  = Trimmed mean
*        5  = Mode
*        6  = Sigma clipped mean
*        7  = Threshold exclusion mean
*        8  = Minmax mean
*        9  = Broadened median
*        10 = Sigma clipped median
*        11 = Fast median
*        12 = Sum
*        13 = Standard deviation about the mean
*        21 = Integrated value (sum of pixel co-ordinate width times value)
*        22 = Intensity-weighted co-ordinate
*        23 = Intensity-weighted dispersion of the co-ordinate.
*        24 = Root mean square
*        25 = Absolute mean deviation
*        31 = Maximum
*        32 = Minimum
*        33 = Co-ordinate of maximum
*        34 = Co-ordinate of minimum
*        300 = Median, but estimating variance from mean variance.
*     MINPIX = INTEGER (Given)
*        The minimum number of pixels required to contribute to an
*        output pixel.
*     NITER = INTEGER (Given)
*        The maximum number of iterations (IMETH = 5).
*     NSIGMA = REAL (Given)
*        The number of sigmas to clip the data at (IMETH = 5 and 6).
*     ALPHA = REAL (Given)
*        The fraction of data values to remove from data (IMETH = 4).
*     RMIN = REAL (Given)
*        The minimum allowed data value (IMETH = 7).
*     RMAX = REAL (Given)
*        The maximum allowed data value (IMETH = 7).
*     RESULT( NPIX ) = REAL (Returned)
*        The output line of data.
*     RESVAR( NPIX ) = REAL (Returned)
*        The output variances.
*     COIND( NPIX ) = INTEGER (Given and Returned)
*        Workspace to hold co-ordinate indices.
*     WRK1( NLINES ) = REAL (Given and Returned)
*        Workspace for calculations.
*     WRK2( NLINES ) = REAL (Given and Returned)
*        Workspace for calculations.
*     PP( NLINES ) = DOUBLE PRECISION (Given and Returned)
*        Workspace for order statistics calculations.
*     COVEC( NMAT, NLINES ) = DOUBLE PRECISION (Given and Returned)
*        Workspace for storing ordered statistics variance-covariance
*        matrix.  Used for IMETHs 3 to 11.
*     NCON( NLINES ) = DOUBLE PRECISION (Given and Returned)
*        The actual number of contributing pixels from each input line
*        to the output line.
*     POINT( NLINES ) = INTEGER (Given and Returned)
*        Workspace to hold pointers to the original positions of the
*        data before extraction and conversion in to the WRK1 array.
*     USED( NLINES ) = LOGICAL (Given and Returned)
*        Workspace used to indicate which values have been used in
*        estimating a resultant value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - Various of the options are simply variations on a theme. The
*     Broadened median is just a trimmed mean with a variable trimming
*     fraction. The Mode is an iteratively carried out version of the
*     sigma clipping (or more precisely the reverse). The minmax and
*     threshold mean are also just trimmed means, but require their own
*     mechanisms.
*
*     - The 'propagation' of the input variances assumes that the input
*     data are fairly represented by a normal distribution. This fact
*     is used together with the 'order statistics' of a normal
*     population to form a new variance estimate. The order statistics
*     are not independent so have non-zero covariances (off diagonal
*     components of the variance-covariance matrix). All 'trimmed
*     means' of any description use the order of the values to
*     estimate which values are corrupt. This applies to all the
*     methods supported here except the mean which rejects no data. The
*     variance used to represent the input normal population is the
*     reciprocal of the sum of the reciprocal variances. We have no
*     other estimate of this value except from the population itself.
*
*     - NMAT = NLINES * ( NLINES + 1 )/ 2 at least.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-MAY-1992 (PDRAPER):
*        Original version. Revamping of routine structure to increase
*        efficiency and keep in line with new routines.
*     30-JAN-1998 (PDRAPER):
*        Added sigma clipped median.
*     16-NOV-1998 (PDRAPER):
*        Added fast median.
*     9-SEP-2002 (DSB):
*        Added unweighted mean method.
*     1-NOV-2002 (DSB):
*        Added workspace argument to CCD1_ORVAR.
*     27-AUG-2003 (DSB):
*        Added IMETH 300 to provide a means of estimating the output
*        variances for large samples, for which the memory requirements
*        for the COVEC array would be too large.
*     2005 December 22 (MJC):
*        Added several translations for new estimators and moments 
*        (IMETH=12 upwards except 300).
*     2005 December 24 (MJC):
*        Added mean absolute deviation.
*     2005 December 27 (MJC):
*        Added standard deviation.
*     2005 December 28 (MJC):
*        Add root mean square.
*     2005 December 29 (MJC):
*        Add summation method.
*     2006 January 2 (MJC):
*        Add COORDS argument.
*     2006 January 5 (MJC):
*        Add COIND argument.
*     2006 January 6 (MJC):
*        Add WIDTHS argument and calls for IMETH = 21, 22, 23.
*     2006 January 26 (MJC):
*        Made COORDS and WIDTHS per pixel.  Change calls to
*        reflect new APIs for CCG1_IWC1 and CCG1_IWD1.
*     2006 January 27 (MJC):
*        Use CCG1_I2WC instead of KPG1_VASV to cope with the
*        two-dimensional co-ordinate array.  Derive the widths for
*        IMETH = 21 from the co-ordinates.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NPIX
      INTEGER NLINES
      INTEGER IMETH
      INTEGER MINPIX
      INTEGER NMAT
      REAL STACK( NPIX, NLINES )
      REAL VARS( NPIX, NLINES )
      REAL COORDS( NPIX, NLINES )
      REAL WIDTHS( NPIX, NLINES )
      DOUBLE PRECISION PP( NLINES )
      INTEGER NITER
      REAL NSIGMA
      REAL ALPHA
      REAL RMIN
      REAL RMAX

*  Arguments Given and Returned:
      INTEGER COIND( NPIX )
      REAL WRK1( NLINES )
      REAL WRK2( NLINES )
      DOUBLE PRECISION COVEC( NMAT, NLINES )
      DOUBLE PRECISION NCON( NLINES )
      INTEGER POINT( NLINES )
      LOGICAL USED( NLINES )

*  Arguments Returned:
      REAL RESULT( NPIX )
      REAL RESVAR( NPIX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IPW1               ! Work space pointer
      INTEGER NBAD               ! Number of bad values

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If we're doing a trimmed mean derive the variance-covariance matrix
*  for the order statistics of a normal popl with up to NLINE members.
*  This also sets up the scale factor for converting mean variances to
*  median variances.
      IF ( IMETH .GE. 3 .AND. IMETH .LE. 11 ) THEN
          CALL PSX_CALLOC( NLINES*NLINES, '_DOUBLE', IPW1, STATUS )
          CALL CCD1_ORVAR( NLINES, NMAT, PP, COVEC, 
     :                     %VAL( CNF_PVAL( IPW1 ) ),
     :                     STATUS )
          CALL PSX_FREE( IPW1, STATUS )
      END IF

*  Now branch for each method.
      IF ( IMETH .EQ. 1 ) THEN

*  Forming the unweighted mean.
         CALL CCG1_UMR1R( STACK, NPIX, NLINES, VARS, MINPIX,
     :                    RESULT, RESVAR, NCON, STATUS )

      ELSE IF ( IMETH .EQ. 2 ) THEN

*  Forming the weighted mean.
         CALL CCG1_MER1R( STACK, NPIX, NLINES, VARS, MINPIX,
     :                      RESULT, RESVAR, NCON, STATUS )

      ELSE IF ( IMETH .EQ. 3 .OR. IMETH .EQ. 300 ) THEN

*  Forming the weighted median.
         CALL CCG1_MDR1R( ( IMETH .EQ. 3 ), STACK, NPIX, NLINES, VARS, 
     :                    MINPIX, COVEC, NMAT, RESULT, RESVAR, WRK1, 
     :                    WRK2, NCON, POINT, USED, STATUS )

      ELSE IF ( IMETH .EQ. 4 ) THEN

*  Forming trimmed mean.
          CALL CCG1_TMR1R( ALPHA, STACK, NPIX, NLINES, VARS, MINPIX,
     :                       COVEC, NMAT, RESULT, RESVAR, WRK1, NCON,
     :                       POINT, USED, STATUS )

      ELSE IF ( IMETH .EQ. 5 ) THEN

*  Forming the mode.
         CALL CCG1_MOR1R( NSIGMA, NITER, STACK, NPIX, NLINES,
     :                      VARS, MINPIX, COVEC, NMAT, RESULT, RESVAR,
     :                      WRK1, WRK2, NCON, POINT, USED, STATUS )

      ELSE IF ( IMETH .EQ. 6 ) THEN

*  Forming sigma clipped mean.
         CALL CCG1_SCR1R( NSIGMA, STACK, NPIX, NLINES, VARS, MINPIX,
     :                      COVEC, NMAT, RESULT, RESVAR, WRK1, WRK2,
     :                      NCON, POINT, USED, STATUS )

      ELSE IF ( IMETH .EQ. 7 ) THEN

*  Forming threshold trimmed mean.
          CALL CCG1_TCR1R( RMIN, RMAX, STACK, NPIX, NLINES, VARS,
     :                       MINPIX, COVEC, NMAT, RESULT, RESVAR,
     :                       WRK1, WRK2, NCON, POINT, USED, STATUS )

      ELSE IF ( IMETH .EQ. 8 ) THEN

*  Forming Min-Max exclusion mean.
        CALL CCG1_MMR1R( STACK, NPIX, NLINES, VARS, MINPIX, COVEC,
     :                     NMAT, RESULT, RESVAR, WRK1, NCON, POINT,
     :                     USED, STATUS )

      ELSE IF ( IMETH .EQ. 9 ) THEN

*  Forming broadened median,
         CALL CCG1_BMR1R( STACK, NPIX, NLINES, VARS, MINPIX, COVEC,
     :                      NMAT, RESULT, RESVAR, WRK1, NCON, POINT,
     :                      USED, STATUS )

      ELSE IF ( IMETH .EQ. 10 ) THEN

*  Forming sigma clipped median.
         CALL CCG1_SMR1R( NSIGMA, STACK, NPIX, NLINES, VARS, MINPIX,
     :                      COVEC, NMAT, RESULT, RESVAR, WRK1, WRK2,
     :                      NCON, POINT, USED, STATUS )

      ELSE IF ( IMETH .EQ. 11 ) THEN

*  Forming fast median (no weights).
         CALL CCG1_FMR1R( STACK, NPIX, NLINES, VARS, MINPIX, COVEC, 
     :                      NMAT, RESULT, RESVAR, WRK1, WRK2, NCON,
     :                      POINT, USED, STATUS )

      ELSE IF ( IMETH .EQ. 12 ) THEN

*  Forming sum.
         CALL CCG1_SUM1R( NPIX, NLINES, STACK, VARS, MINPIX,
     :                    RESULT, RESVAR, NCON, STATUS )

      ELSE IF ( IMETH .EQ. 13 ) THEN

*  Forming standard deviation.
         CALL CCG1_SD1R( NPIX, NLINES, STACK, MINPIX,
     :                   RESULT, RESVAR, NCON, STATUS )

      ELSE IF ( IMETH .EQ. 21 ) THEN

*  Create the widths from the co-ordinates assuming that there are
*  gaps.
         CALL CCG1_WCWIR( NPIX, NLINES, COORDS, WIDTHS, STATUS )

*  Forming integrated value.
         CALL CCG1_FLX1R( NPIX, NLINES, STACK, VARS, WIDTHS, MINPIX,
     :                    RESULT, RESVAR, NCON, STATUS )

      ELSE IF ( IMETH .EQ. 22 ) THEN

*  Forming intensity-weighted co-ordinate dispersion.
         CALL CCG1_IWC1R( NPIX, NLINES, STACK, VARS, COORDS,
     :                    MINPIX, RESULT, RESVAR, NCON, STATUS )

      ELSE IF ( IMETH .EQ. 23 ) THEN

*  Forming intensity-weighted co-ordinate dispersion.
         CALL CCG1_IWD1R( NPIX, NLINES, STACK, VARS, COORDS, MINPIX,
     :                    RESULT, RESVAR, NCON, STATUS )

      ELSE IF ( IMETH .EQ. 24 ) THEN

*  Forming mean absolute deviation.
         CALL CCG1_RMS1R( NPIX, NLINES, STACK, MINPIX, RESULT, RESVAR,
     :                    NCON, STATUS )

      ELSE IF ( IMETH .EQ. 25 ) THEN

*  Forming mean absolute deviation.
         CALL CCG1_AD1R( NPIX, NLINES, STACK, MINPIX, RESULT, RESVAR,
     :                   NCON, STATUS )

      ELSE IF ( IMETH .EQ. 31 ) THEN

*  Forming array of maxima.
         CALL CCG1_MXR1R( .TRUE., NPIX, NLINES, STACK, VARS, RESULT,
     :                    RESVAR, COIND, WRK1, STATUS )

      ELSE IF ( IMETH .EQ. 32 ) THEN

*  Forming array of minima.
         CALL CCG1_MNR1R( .TRUE., NPIX, NLINES, STACK, VARS, RESULT,
     :                    RESVAR, COIND, WRK1, STATUS )

      ELSE IF ( IMETH .EQ. 33 ) THEN

*  Forming array of maxima and corresponding indices.
         CALL CCG1_MXR1R( .TRUE., NPIX, NLINES, STACK, VARS, RESULT,
     :                    RESVAR, COIND, WRK1, STATUS )

*  Convert the pixel indices of the maxima into co-ordinates stored in
*  the RESULT array.
         CALL CCG1_I2WCR( NPIX, NLINES, COIND, COORDS, RESULT, NBAD,
     :                    STATUS )

      ELSE IF ( IMETH .EQ. 34 ) THEN

*  Forming array of minima and corresponding indices.
         CALL CCG1_MNR1R( .TRUE., NPIX, NLINES, STACK, VARS, RESULT,
     :                    RESVAR, COIND, WRK1, STATUS )

*  Convert the pixel indices of the minima into co-ordinates stored in
*  the RESULT array.
         CALL CCG1_I2WCR( NPIX, NLINES, COIND, COORDS, RESULT, NBAD,
     :                    STATUS )

      ELSE

*  Invalid method report error
         STATUS = SAI__ERROR
         CALL ERR_REP( 'BAD_METH',
     :                 'Bad method specified for image combination'//
     :                 ' ( invalid or not implemented )', STATUS )
      END IF

      END
* $Id$
