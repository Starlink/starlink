      SUBROUTINE SURFIT( STATUS )
*+
*  Name:
*     SURFIT

*  Purpose:
*     Fits a polynomial or bi-cubic spline surface to two-dimensional
*     data array.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SURFIT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The background of a two-dimensional data array in the supplied NDF
*     structure is estimated by condensing the array into equally sized
*     rectangular bins, fitting a spline or polynomial surface to the
*     bin values, and finally evaluating the surface for each pixel in
*     the data array.
*
*     There is a selection of estimators by which representative
*     values for each bin are determined.  There are several options to
*     make the fit more accurate.  Values beyond upper and lower
*     thresholds may be excluded from the binning.  Bad pixels are also
*     excluded, so prior masking may help to find the background more
*     rapidly.  Kappa-sigma clipping of the fitted bins is available
*     so that the fit is not biased by anomalous bins, such as those
*     entirely within an extended object.  If a given bin contains more
*     than a prescribed fraction of bad pixels, it is excluded from the
*     fit.
*
*     The data array representing the background is evaluated at each
*     pixel by one of two methods.  It is written to the output NDF
*     structure.
*
*     The raw binned data, the weights, the fitted binned data and the
*     residuals to the fit may be written to a logfile.  This also
*     keeps a record of the input parameters and the rms error of the
*     fit.

*  Usage:
*     surfit in out [fittype] [estimator] [bindim] [evaluate]

*  ADAM Parameters:
*     BINDIM() = _INTEGER (Read)
*        The x-y dimensions of a bin used to estimate the local
*        background.  If you supply only one value, it is used for
*        both dimensions.  The minimum value is 2.  The maximum may be
*        constrained by the number of polynomial terms, such that in
*        each direction there are at least as many bins as terms.  If a
*        null (!) value is supplied, the value used is such that 32 bins
*        are created along each axis.  [!]
*     CLIP() = _REAL (Read)
*        Array of limits for progressive clipping of pixel values
*        during the binning process in units of standard deviation.  A
*        null value means only unclipped statistics are computed and
*        presented.  Between one and five values may be supplied.  [2,3]
*     ESTIMATOR = LITERAL (Read)
*        The estimator for the bin.  It must be one of the following
*        values: "Mean" for the mean value, "Ksigma" for the mean with
*        kappa-sigma clipping; "Mode" for the mode, and "Median" for
*        the median.  "Mode" is only available when there are at least
*        twelve pixels in a bin.  If a null (!) value is supplied,
*        "Median" is used if there are fewer than 6 values in a bin, and
*        "Mode" is used otherwise.  [!]
*     EVALUATE = LITERAL (Read)
*        The method by which the resulting data array is to be
*        evaluated from the surface-fit.  It must be either
*        "Interpolate" where the values at the corners of the bins are
*        derived first, and then the pixel values are found by linear
*        interpolation within those bins; or "All" where the
*        surface-fit is evaluated for every pixel.  The latter is
*        slower, but can produce more-accurate results, unless the
*        surface is well behaved.  The default is the current value,
*        which is initially set to "Interpolate".  []
*     FITCLIP() = _REAL (Read)
*        Array of limits for progressive clipping of the binned array
*        in units of the rms deviation of the fit.  A null value (!)
*        means no clipping of the binned array will take place.
*        Between 1 and 5 values may be supplied.  The default is the
*        current value, which is ! initially.  []
*     FITTYPE = LITERAL (Read)
*        The type of fit.  It must be either "Polynomial" for a
*        Chebyshev polynomial or "Spline" for a bi-cubic spline.  The
*        default is the current value, which initially is "Spline". []
*     GENVAR = _LOGICAL (Read)
*        If TRUE, a constant variance array is created in the output NDF
*        assigned to the mean square surface-fit error.  [FALSE]
*     LOGFILE = FILENAME (Read)
*        Name of the file to log the binned array and errors before and
*        after fitting.  If null, there will be no logging. [!]
*     IN = NDF (Read)
*        NDF containing the two-dimensional data array to be fitted.
*     KNOTS( 2 ) = _INTEGER (Read)
*        The number of interior knots used for the bi-cubic-spline fit
*        along the x and y axes.  These knots are equally spaced within
*        the image.  Both values must be in the range 0 to 11.  If you
*        supply a single value, it applies to both axes.  Thus 1
*        creates one interior knot, [5,4] gives 5 along the x axis and
*        4 along the y direction.  Increasing this parameter values
*        increases the flexibility of the surface.  Normally, 4 is a
*        reasonable value.  The upper limit of acceptable values will
*        be reduced along each axis when its binned array dimension is
*        fewer than 29.  KNOTS is only accessed when FITTYPE="Spline".
*        The default is the current value, which is 4 initially. []
*     ORDER( 2 ) = _INTEGER (Read)
*        The orders of the fits along the x and y directions.  Both
*        values must be in the range 0 to 14.  If you supply a single
*        single value, it applies to both axes.  Thus 0 gives a
*        constant, [3,1] gives a cubic along the x direction and a
*        linear fit along the y.  Increasing this parameter values
*        increases the flexibility of the surface.  The upper limit of
*        acceptable values will be reduced along each axis when its
*        binned array dimension is fewer than 29.  ORDER is only
*        accessed when FITTYPE="Polynomial".  The default is the current
*        value, which is 4 initially. []
*     OUT = NDF (Write)
*        NDF to contain the fitted two-dimensional data array.
*     RMS = _REAL (Write)
*        An output parameter in which is stored the RMS deviation of the
*        fit from the original data (per pixel).
*     THRHI = _REAL (Read)
*        Upper threshold above which values will be excluded from the
*        analysis to derive representative values for the bins.  If it
*        is null (!) there will be no upper threshold.  [!]
*     THRLO = _REAL (Read)
*        Lower threshold below which values will be excluded from the
*        analysis to derive representative values for the bins.  If it
*        is null (!) there will be no lower threshold.  [!]
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the NDF supplied for parameter IN to be used
*        instead.  [!]
*     WLIM = _REAL (Read)
*        The minimum fraction of good pixels in a bin that permits the
*        bin to be included in the fit.  Here good pixels are ones that
*        participated in the calculation of the bin's representative
*        value. So they exclude both bad pixels and ones rejected
*        during estimation (e.g. ones beyond the thresholds or were
*        clipped).  [!]

*  Notes:
*     A polynomial surface fit is stored in a SURFACEFIT extension,
*     component FIT of type POLYNOMIAL, variant CHEBYSHEV or BSPLINE.
*
*     For further details of the CHEBYSHEV variant see SGP/38.  The
*     CHEBYSHEV variant includes the fitting variance for each
*     coefficient.
*
*     The BSPLINE variant structure is provisional.  It contain the
*     spline coefficients in the two-dimensional DATA_ARRAY component,
*     the knots in XKNOTS and YKNOTS arrays, and a scaling factor to
*     restore the original values after spline evaluation recorded in
*     component SCALE.  All of these components have type _REAL.
*
*     Also stored in the SURFACEFIT extension is the r.m.s. deviation
*     of data values compared with the fit (component RMS); and the
*     co-ordinate system component COSYS, set to "GRID".

*  Examples:
*     surfit comaB comaB_bg
*        This calculates the surface fit to the two-dimensional NDF
*        called comaB using the current defaults.  The evaluated fit is
*        stored in the NDF called comaB_bg.
*     surfit comaB comaB_bg poly median order=5 bindim=[24,30]
*        As above except that 5th-order polynomial fit is chosen,
*        the median is used to derive the representative value for each
*        bin, and the binning size is 24 pixels along the first axis,
*        and 32 pixels along the second.
*     surfit comaB comaB_bg fitclip=[2,3] logfile=comaB_fit.lis
*        As the first example except that the binned array is clipped at
*        2 then 3 standard deviations to remove outliers before the
*        final fit is computed.  The text file comaB_fit.lis records a
*        log of the surface fit.
*     surfit comaB comaB_bg estimator=ksigma clip=[2,2,3]
*        As the first example except that the representative value of
*        each bin is the mean after clipping twice at 2 then once at
*        3 standard deviations.
*     surfit in=irasorion out=sback evaluate=all fittype=s knots=7
*        This calculates the surface fit to the two-dimensional NDF
*        called irasorion.  The fit is evaluated at every pixel and the
*        resulting array stored in the NDF called sback.  A spline with
*        seven knots along each axis is used to fit the surface.

*  Related Applications:
*     KAPPA: ARDMASK, FITSURFACE, MAKESURFACE, REGIONMASK.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     LABEL, TITLE, UNITS, WCS and HISTORY components of the input NDF.
*     Any input VARIANCE is ignored.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using single- or double-precision floating point for
*     FITTYPE = "Spline" or "Polynomial" respectively.  The output NDF's
*     DATA and VARIANCE components have type _REAL (single-precision).

*  Copyright:
*     Copyright (C) 1996, 1998, 2000, 2004 Central Laboratory of the
*     Research Councils.
*     Copyright (C) 2007-2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK).
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1996 October 23 (MJC):
*        Original NDF version.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     3-FEB-2000 (DSB):
*        Replace AIF_FLNAM calls with NDF_MSG.
*     12-APR-2000 (DSB):
*        Added parameter RMS.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2007 June 28 (MJC):
*        Now writes a SURFACEFIT structure for polynomial coefficients,
*        like application FITSURFACE.
*     2007 July 3 (MJC):
*        Use singular value decomposition to solve the normal
*        equations and to provide the variances of the polynomial
*        coefficients, stored in the SURFACEFIT extension.  Writes a
*        SURFACEFIT structure for bi-cubic spline fitting.
*     2007 December 20 (MJC):
*        Add GENVAR parameter and evaluate pixel-by-pixel variance.
*     2008 January 5 (MJC):
*        Write a constant variance array using the mean square residual
*        of the fit when parameter GENVAR is TRUE.
*     2009 December 19 (MJC):
*        Fix two bugs: one in the calculation of required workspace
*        for splines when the number of knots are different along the
*        two axes; and the other arising from the spline fitting is
*        single-precision in PDA when finiding the maximum residual.
*     2010 August 27 (MJC):
*        Use renamed KPG_LD2AR calls (previously LD2AR).
*     2010 August 29 (MJC):
*        Fixed bug logging spline-fit binned values by introducing
*        calls to the new KPG_LR2Ax that takes single-precision binned
*        data.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'PRM_PAR'          ! Magic-value definitions
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of string ignoring trailing
                                 ! blanks

*  Local Constants:
      INTEGER MXCLIP             ! Maximum number of clips of the data
      PARAMETER ( MXCLIP = 5 )

      INTEGER MXPAR              !Maximum number of parameters which
                                 ! can be handled in each direction.
                                 ! Should be the same as in KPS1_SUPF.
      PARAMETER ( MXPAR = 15 )

      INTEGER MXKNOT             ! Maximum number of interior knots that
                                 ! can be handled in each direction.
      PARAMETER ( MXKNOT = 11 )

      INTEGER MCHOEF             ! Maximum number of Chebyshev
                                 ! polynomial coefficients
      PARAMETER ( MCHOEF = MXPAR * MXPAR )

      INTEGER MINBIN             ! Minimum number of elements along each
                                 ! axis to form a bin
      PARAMETER ( MINBIN = 2 )

      INTEGER MXBIN              ! Maximum number of bins along each
                                 ! axis
      PARAMETER ( MXBIN = 60 )

      INTEGER MBCOEF             ! Maximum number of B-spline
                                 ! coefficients
      PARAMETER ( MBCOEF = ( MXKNOT + 4 ) ** 2 )

      INTEGER MTKNOT             ! Maximum total number of knots that
                                 ! can be handled in each direction.
      PARAMETER ( MTKNOT = MXKNOT + 8 )


      INTEGER MXWORK             ! Maximum number of work arrays
      PARAMETER ( MXWORK = 15 )  ! obtained

      INTEGER NDIM               ! Dimensionality of arrays
      PARAMETER ( NDIM = 2 )     ! two-dimensional only

      INTEGER OPTBIN             ! Default number of bins
      PARAMETER ( OPTBIN = 32 )

      CHARACTER * ( 4 ) FILNAM   ! Junk file name for listing subroutine
      PARAMETER ( FILNAM = 'JUNK' )

*  Local Variables:
      INTEGER ACTVAL             ! Number of parameter values supplied
      LOGICAL ALL                ! All pixels are evaluated without
                                 ! interpolation
      LOGICAL AUTO               ! Automatic clipping of fitted binned
                                 ! data
      LOGICAL BAD                ! Input NDF may contain bad pixels
      INTEGER BINMAX( NDIM )     ! Maximum number of pixels in a bin
                                 ! along each axis
      INTEGER BINMIN( NDIM )     ! Minimum number of pixels in a bin
                                 ! along each axis
      REAL BINNED( MXBIN, MXBIN ) ! Binned data or weights used for
                                 ! logging
      INTEGER BINPTR             ! Mnemonic pointer to workspace for a
                                 ! bin of pixels
      INTEGER BINSIZ             ! The number of pixels in a bin
      CHARACTER * ( 132 ) BUFFER ! Buffer for writing to the logfile
      DOUBLE PRECISION CHCOEF( MCHOEF ) ! Chebyshev coefficients of fit
      REAL CLIP( MXCLIP )        ! Clipping sigmas during binning
      REAL CLIPF( MXCLIP )       ! Clipping sigmas after binning and
                                 ! fitting
      REAL COEFF( MBCOEF )       ! B-spline coefficients of the fit
      INTEGER CPTR               ! Pointer to mapped covariance matrix
      LOGICAL CREVAR             ! Create variance array?
      CHARACTER * ( 100 ) DATNAM ! Name of input and output IMAGEs
      INTEGER DIMS( NDIM )       ! Dimensions of both data arrays
      DOUBLE PRECISION DRMS      ! RMS difference of the input and
                                 ! output data arrays
      REAL DSCALE                ! Scale factor applied before logging
                                 ! the fit and residuals, and must be
                                 ! re-applied in reciprocal
      REAL DUMMY( 1 )            ! Dummy thresholding array
      DOUBLE PRECISION DXMAX     ! Upper x position limit of the fit
      DOUBLE PRECISION DXMIN     ! Lower x position limit of the fit
      DOUBLE PRECISION DYMAX     ! Upper y position limit of the fit
      DOUBLE PRECISION DYMIN     ! Lower y position limit of the fit
      INTEGER DBD( NDIM )        ! Default number of pixels in a bin
                                 ! along each axis
      INTEGER EL                 ! Number of elements in mapped array
      CHARACTER * ( 6 ) ESTIMA   ! The type of estimator
      CHARACTER * ( 11 ) EVMETH  ! Evaluation method
      INTEGER FDL                ! File description of logfile
      LOGICAL FIRST              ! Weights are to squared in polynomial
                                 ! fit?
      LOGICAL FITCLP             ! There is to be clipping of the binned
                                 ! data?
      INTEGER FITPTR             ! Mnemonic pointer to the fitted binned
                                 ! data
      CHARACTER * ( 10 ) FITTYP  ! The type of fitting function
      INTEGER GPTR               ! Mnemonic pointer to workspace for
                                 ! sorting positions
      LOGICAL HITHRS             ! There is an upper threshold set?
      INTEGER I                  ! Loop counter
      INTEGER IBIN( NDIM )       ! Number of elements in a bin
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Implementation type
      INTEGER IX                 ! Number of columns in a bin
      INTEGER IY                 ! Number of lines in a bin
      INTEGER LBND( NDIM )       ! Lower bound of data array
      INTEGER LINPTR             ! Mnemonic pointer to workspace for a
                                 ! line of the data array plus x-y
                                 ! positions
      LOGICAL LOGFIL             ! A log file is being written?
      LOGICAL LOTHRS             ! There is a lower threshold set?
      INTEGER M                  ! Loop counter
      INTEGER MAXBIN             ! Total number of bins
      INTEGER MAXKNO( NDIM )     ! Maximum KNOT values
      DOUBLE PRECISION MAXMUM    ! Maximum value
      INTEGER MAXORD( NDIM )     ! Maximum ORDER values
      INTEGER MAXPOS             ! Index of maximum array value
      INTEGER MINKNO( NDIM )     ! Minimum KNOT values
      DOUBLE PRECISION MINMUM    ! Minimum value
      INTEGER MINORD( NDIM )     ! Minimum ORDER values
      INTEGER MINPOS             ! Index of minimum array value
      INTEGER MPCOEF             ! Maximum number of polynomial
                                 ! coefficients for chosen NXPAR and
                                 ! NYPAR
      INTEGER MPTR               ! Pointer to SVD V matrix
      INTEGER NBIN               ! No. of bins with defined statistics
      INTEGER*8 NBIN8            ! No. of bins with defined statistics
      INTEGER NC                 ! Character column counter
      INTEGER NCI                ! Character column counter of image
                                 ! names
      INTEGER NCLIP              ! Number of clips of the data
      INTEGER NCLIPF             ! Number of clips of the fitted binned
                                 ! data
      INTEGER NCOEF              ! Number of coefficients
      INTEGER NDFI               ! Identifier for input NDF
      INTEGER NDFO               ! Identifier for output NDF
      INTEGER NELM               ! Number of bins with defined
                                 ! statistics (a copy before clipping)
      INTEGER NEQPTR             ! Mnemonic pointer to normal-equation
                                 ! workspace
      INTEGER NINVAL             ! Number of bad values
      INTEGER*8 NIWS             ! Storage space size for spline fit
      INTEGER NKNOT( NDIM )      ! Number of knots in each direction
      INTEGER*8 NLWS             ! Storage space size to allow for A
                                 ! rank-deficiency system
      INTEGER NPOINT             ! Storage space size for spline eval
      INTEGER NWORK              ! Number of d.p. work arrays required
                                 ! to store information about each bin
      INTEGER*8 NWS              ! Size of spline-fitting routine's
                                 ! workspace
      INTEGER NX, NY             ! Number of bins in the x and y dirns
      INTEGER NXKNOT             ! Number of knots in x direction
      INTEGER NXPAR              ! Number of fitting parameters in x
                                 ! direction
      INTEGER NYKNOT             ! Number of knots in y direction
      INTEGER NYPAR              ! Number of fitting parameters in y
                                 ! direction
      INTEGER ORDER( NDIM )      ! Polynomial order in each direction
      INTEGER PANPTR             ! Mnemonic pointer to panel-ordering
                                 ! workspace
      INTEGER PNTRI( 1 )         ! Pointer to input data array
      INTEGER PNTRO( 2 )         !    "     " output data & variance
      INTEGER RESPTR             ! Mnemonic pointer to the residuals to
                                 ! fitted binned data
      REAL RMAX                  ! Maximum value for spline
      REAL RMIN                  ! Minimum value for spline
      REAL RMS                   ! RMS difference of the input and
                                 ! output data arrays
      REAL RMSF                  ! RMS difference of the fitted and raw
                                 ! binned data
      DOUBLE PRECISION RSMAX     ! Maximum residual
      INTEGER S1                 ! Used to eval size of spline workspace
      INTEGER S2                 ! Used to eval size of spline workspace
      INTEGER SDIM( NDF__MXDIM ) ! Significant NDF dimensions
      REAL SCALE                 ! Scale factor applied before fitting
                                 ! to improve the fit and must be
                                 ! re-applied in reciprocal
      INTEGER SEVPTR             ! Mnemonic pointer to spline-evaluation
                                 ! workspace
      INTEGER SIWPTR             ! Mnemonic pointer to spline workspace
      INTEGER SLWPTR             ! Mnemonic pointer to spline workspace
      INTEGER SWPTR              ! Mnemonic pointer to spline workspace
      INTEGER TBNPTR             ! Mnemonic pointer to workspace for a
                                 ! bin of pixels after thresholding
      REAL THRLO                 ! Lower threshold
      REAL THRHI                 ! Upper threshold
      INTEGER UBND( NDIM )       ! Upper bound of data array
      DOUBLE PRECISION VARIAN( MCHOEF ) ! Variance of Chebyshev coeffs.
      INTEGER WOBT               ! Number of work arrays successfully
                                 ! created and mapped
      REAL WLIMIT                ! Minimum fraction of good pixels in a
                                 ! valid bin
      INTEGER WPNTR( MXWORK )    ! Pointers to workspace
      INTEGER WPTR               ! Mnemonic pointer to weights
      REAL XKNOT( MTKNOT )       ! Spline knots in x direction
      REAL XMAX                  ! Upper x position limit of the fit
      REAL XMIN                  ! Lower x position limit of the fit
      REAL XN                    ! The number of bins in the x direction
      INTEGER XPTR               ! Mnemonic pointer to x positions
      REAL YKNOT( MTKNOT )       ! Spline knots in y direction
      REAL YMAX                  ! Upper y position limit of the fit
      REAL YMIN                  ! Lower y position limit of the fit
      REAL YN                    ! The number of bins in the y direction
      INTEGER YPTR               ! Mnemonic pointer to y positions
      INTEGER ZPTR               ! Mnemonic pointer to binned data

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the NDFs.
*  ================

*  Begin the NDF context.
      CALL NDF_BEGIN

*  Get the NDF containing the input data.  There must be only two
*  significant dimensions.
      CALL KPG1_GTNDF( 'IN', NDIM, .TRUE., 'READ', NDFI, SDIM, LBND,
     :                 UBND, STATUS )

*  Evaluate the dimensions.
      DIMS( 1 ) = UBND( 1 ) - LBND( 1 ) + 1
      DIMS( 2 ) = UBND( 2 ) - LBND( 2 ) + 1

*  Find out if the NDF contains magic bad values.
      CALL NDF_BAD( NDFI, 'Data', .FALSE., BAD, STATUS )

*  Create the surface-fit NDF, propagating the WCS, AXIS, UNITS, LABEL,
*  TITLE, HISTORY and extensions from the input NDF.
      CALL LPG_PROP( NDFI, 'WCS,Axis,Units', 'OUT', NDFO, STATUS )

*  Obtain a new title for the output NDF.
      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Open the log file.
*  ==================

*  Attempt to obtain and open a log file to list the binned data.  A
*  null value, meaning no logfile is required, is handled invisibly.
      LOGFIL = .FALSE.
      CALL ERR_MARK
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 132, FDL, STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         LOGFIL = .TRUE.
      END IF
      CALL ERR_RLSE
      IF ( STATUS .NE. SAI__OK ) GOTO 999
      IF ( LOGFIL ) CALL MSG_OUTIF( MSG__NORM, 'LOG',
     :  'Logging to $LOGFILE.', STATUS )

*  Obtain parameters that definite the fitting and bin size.
*  =========================================================

*  Obtain the type of fit.
      CALL PAR_CHOIC( 'FITTYPE', 'Spline', 'Spline,Polynomial', .TRUE.,
     :                 FITTYP, STATUS )

*  Constrain the number of interior knots given the number of pixels.
*  This is not perfect, as we really need to know the number of pixels
*  in the bins rather than the minimum number within in a bin.
*  However, the number of knots is needed to constrain the number of
*  pixels...
      IF ( FITTYP( 1:3 ) .EQ. 'SPL' ) THEN
         MINKNO( 1 ) = 0
         MINKNO( 2 ) = 0
         MAXKNO( 1 ) = MIN( ( DIMS( 1 ) + 1 ) / MINBIN + 1, MXPAR - 1,
     :                 MXKNOT )
         MAXKNO( 2 ) = MIN( ( DIMS( 2 ) + 1 ) / MINBIN + 1, MXPAR - 1,
     :                 MXKNOT )

*  Obtain the number of interior knots in the x and y directions.  A
*  single value supplied applies to both axes.
         CALL PAR_GRMVI( 'KNOTS', NDIM, MINKNO, MAXKNO, NKNOT, ACTVAL,
     :                   STATUS )
         IF ( ACTVAL .EQ. 1 ) NKNOT( 2 ) = NKNOT( 1 )

*  Define the number of terms.
         NXKNOT = NKNOT( 1 )
         NYKNOT = NKNOT( 2 )

      ELSE

*  Constrain the number of terms given the number of pixels.  This is
*  not perfect, as we really need to know the number of pixels in the
*  bins rather than the minimum number within in a bin.  However, the
*  number of terms is needed to constrain the number of pixels...
         MINORD( 1 ) = 0
         MINORD( 2 ) = 0
         MAXORD( 1 ) = MIN( MXPAR, ( DIMS( 1 ) + 1 ) / MINBIN ) - 1
         MAXORD( 2 ) = MIN( MXPAR, ( DIMS( 2 ) + 1 ) / MINBIN ) - 1

*  Obtain the fit orders in the x and y directions.  A single value
*  supplied applies to both axes.
         CALL PAR_GRMVI( 'ORDER', NDIM, MINORD, MAXORD, ORDER, ACTVAL,
     :                   STATUS )
         IF ( ACTVAL .EQ. 1 ) ORDER( 2 ) = ORDER( 1 )

*  Define the number of terms.
         NXPAR = ORDER( 1 ) + 1
         NYPAR = ORDER( 2 ) + 1

      END IF

*  Suggest default bin dimensions.
      DBD( 1 ) = MAX( MINBIN, ( DIMS( 1 ) - 1 ) / OPTBIN + 1 )
      DBD( 2 ) = MAX( MINBIN, ( DIMS( 2 ) - 1 ) / OPTBIN + 1 )
      CALL PAR_DEF1I( 'BINDIM', NDIM, DBD, STATUS )

*  Find the maximum bin dimensions.  The upper limit is constrained
*  such that the maximum number of bins in a given axis is not
*  exceeded, and there are enough bins to perform the fit for the order
*  selected.  The factor of 4 for the spline comes from the fact that
*  there needs to be at least the order plus 1 bins along each
*  dimension.  The order is 3 for a bi-cubic.
      IF ( FITTYP( 1:3 ) .EQ. 'SPL' ) THEN
         BINMAX( 1 ) = MAX( ( DIMS( 1 ) - 1 ) / MXBIN + 1,
     :                 DIMS( 1 ) / 4, DBD( 1 ) )
         BINMAX( 2 ) = MAX( ( DIMS( 2 ) - 1 ) / MXBIN + 1,
     :                 DIMS( 2 ) / 4, DBD( 2 ) )

      ELSE IF ( FITTYP( 1:3 ) .EQ. 'POL' ) THEN
         BINMAX( 1 ) = MAX( ( DIMS( 1 ) - 1 ) / MXBIN + 1,
     :                 DIMS( 1 ) / ( ORDER( 1 ) + 1 ), DBD( 1 ) )
         BINMAX( 2 ) = MAX( ( DIMS( 2 ) - 1 ) / MXBIN + 1,
     :                 DIMS( 2 ) / ( ORDER( 2 ) + 1 ), DBD( 2 ) )
      END IF

*  Obtain the dimensions of rectangles into which the data are to be
*  binned.  A single value supplied applies to both axes.
      BINMIN( 1 ) = MINBIN
      BINMIN( 2 ) = MINBIN

      IF( STATUS .NE. SAI__OK ) GO TO 999

      CALL PAR_GRMVI( 'BINDIM', NDIM, BINMIN, BINMAX, IBIN, ACTVAL,
     :                STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         IBIN( 1 ) = DBD( 1 )
         IBIN( 2 ) = DBD( 2 )
         ACTVAL = 2
      END IF

      IF ( ACTVAL .EQ. 1 ) IBIN( 2 ) = IBIN( 1 )
      IX = IBIN( 1 )
      IY = IBIN( 2 )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Calculate the number of bins in the x and y directions.
      XN = REAL( DIMS( 1 ) ) / REAL( IX )
      YN = REAL( DIMS( 2 ) ) / REAL( IY )
      NX = IFIX( XN )
      NY = IFIX( YN )
      BINSIZ = IX * IY

*  Allow for the less-than-half sized bins at the periphery.
      IF ( REAL( NX ) .LT. XN ) NX = NX + 1
      IF ( REAL( NY ) .LT. YN ) NY = NY + 1

*  Validate the number of bins.
*  ============================

*  A spline fit usually needs more points to derive a stable fit.  The
*  chicken-and-egg situation alluded to before makes it impossible to
*  determine whether a reasonable fit is obtainable.  Here we check for
*  either type of fit as a safety valve.
      IF ( FITTYP( 1:3 ) .EQ. 'POL' .AND.
     :     ( NX .LT. NXPAR .OR. NY .LT. NYPAR ) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NX', NX )
         CALL MSG_SETI( 'NY', NY )
         CALL MSG_SETI( 'NXT', NXPAR )
         CALL MSG_SETI( 'NYT', NYPAR )
         CALL ERR_REP( 'SURFIT_TOOFEW1',
     :     'SURFIT: There are too few bins (^NX by ^NY) for a '/
     :     /'^NXT-by-^NYT term fit.  Try with more bins or less terms.',
     :     STATUS )
         GOTO 999

      ELSE IF ( FITTYP( 1:3 ) .EQ. 'SPL' .AND.
     :          ( NX .LT. 4 .OR. NY .LT. 4 ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SURFIT_TOOFEW2',
     :     'SURFIT: There are too few bins (^NX by ^NY) for a '/
     :     /'bi-cubic-spline fit.  Try with more bins.', STATUS )
         GOTO 999

      END IF

*  Obtain more parameters.
*  =======================
*
*  Get the estimator type.  The mode-finding algorithm needs at least
*  six points to derive the mode.
      IF ( BINSIZ .GE. 6 ) THEN
         CALL PAR_CHOIC( 'ESTIMATOR', 'Mode', 'Mode,Mean,Ksigma,Median',
     :                   .TRUE., ESTIMA, STATUS )
      ELSE
         CALL PAR_CHOIC( 'ESTIMATOR', 'Median', 'Median,Ksigma,Mean',
     :                   .TRUE., ESTIMA, STATUS )
      END IF

*  Kappa-sigma clipping requires an additional parameter: the clipping
*  limits.  A null value indicates no clipping is to be performed.
      NCLIP = 0
      IF ( ESTIMA .EQ. 'KSIGMA' ) THEN
         CALL PAR_GDRVR( 'CLIP', MXCLIP, 0.1, VAL__MAXR,
     :                   CLIP, NCLIP, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            NCLIP = 0

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            GOTO 999

         END IF
      END IF

*  Get the minimum permitted fraction of non-bad pixels in a bin, yet
*  allow the bin to be included in the fit.
      CALL PAR_GDR0R( 'WLIM', 0.3, 0.1, 1.0, .FALSE., WLIMIT, STATUS )

*  Null means no lower threshold.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         WLIMIT = 0.3
      END IF

*  Are thresholds to be set to concentrate the estimator around the
*  background rather than some dominant objects?

*  Get the low threshold value.
      LOTHRS = .TRUE.
      CALL PAR_GDR0R( 'THRLO', 0.0, VAL__MINR, VAL__MAXR,
     :                .FALSE., THRLO, STATUS )

*  Null means no lower threshold.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         THRLO = VAL__BADR
         LOTHRS = .FALSE.
      END IF

*  Get the high threshold value.
      HITHRS = .TRUE.
      CALL PAR_GDR0R( 'THRHI', VAL__MAXR, THRLO + VAL__EPSR,
     :                VAL__MAXR, .FALSE., THRHI, STATUS )

*  Null means no upper threshold.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         THRHI = VAL__BADR
         HITHRS = .FALSE.
      END IF

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the mode of operation. Interactive mode not available yet.
      AUTO = .TRUE.
*     CALL PAR_GTD0L( 'AUTO', .TRUE., .TRUE., AUTO, STATUS )

*  Obtain the values of Kappa-sigma clipping of the fitted bins, i.e.
*  controls the rejection of outlier bins compared with the overall
*  fit.
      FITCLP = .FALSE.
      IF ( AUTO ) THEN
         NCLIPF = 0
         CALL PAR_GDRVR( 'FITCLIP', MXCLIP, 0.1, VAL__MAXR,
     :                   CLIPF, NCLIPF, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            NCLIPF = 0

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            GOTO 999

*  No problem, so store the input parameters in the log file.
         ELSE IF ( NCLIPF .GT. 0 ) THEN
            FITCLP = .TRUE.
         END IF
      END IF

*  Get the method of final evaluation.
      CALL PAR_CHOIC( 'EVALUATE', 'Interpolate', 'Interpolate,All',
     :                .TRUE., EVMETH, STATUS )

*  Determine whether or not a variance array is to be created.
      CALL PAR_GET0L( 'GENVAR', CREVAR, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Write information to the log file.
*  ==================================
      IF ( LOGFIL ) THEN

*  Get the name of the input data array arrays and store it in the log
*  file.
         NC = 0
         BUFFER = ' '

         CALL NDF_MSG( 'NDF', NDFI )
         CALL MSG_LOAD( ' ', '^NDF', DATNAM, NCI, STATUS )

         CALL CHR_PUTC( 'Input IMAGE is ', BUFFER, NC )
         CALL CHR_PUTC( DATNAM( :NCI ), BUFFER, NC )
         CALL CHR_PUTC( '.', BUFFER, NC )
         CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )

*  Get the name of the output data array arrays and store it in the log
*  file.
         NC = 0
         BUFFER = ' '

         CALL NDF_MSG( 'NDF', NDFO )
         CALL MSG_LOAD( ' ', '^NDF', DATNAM, NCI, STATUS )

         CALL CHR_PUTC( 'Output IMAGE is ', BUFFER, NC )
         CALL CHR_PUTC( DATNAM( :NCI ), BUFFER, NC )
         CALL CHR_PUTC( '.', BUFFER, NC )
         CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )

*  Record the type of fit in the log file.
         NC = 0
         BUFFER = ' '
         CALL CHR_PUTC( 'Fit type is ', BUFFER, NC )
         NCI = CHR_LEN( FITTYP )
         CALL CHR_PUTC( FITTYP( :NCI ), BUFFER, NC )
         CALL CHR_PUTC( '.', BUFFER, NC )
         CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )

*  Record the number of fitting parameters or knots in the log file.
         NC = 0
         BUFFER = ' '
         IF ( FITTYP( 1:3 ) .EQ. 'POL' ) THEN
            CALL CHR_PUTC( 'The number of fitting parameters is ',
     :                     BUFFER, NC )
            CALL CHR_PUTI( NXPAR, BUFFER, NC )
            CALL CHR_PUTC( ' in x and ', BUFFER, NC )
            CALL CHR_PUTI( NYPAR, BUFFER, NC )
         ELSE
            CALL CHR_PUTC( 'The number of interior knots is ',
     :                     BUFFER, NC )
            CALL CHR_PUTI( NXKNOT, BUFFER, NC )
            CALL CHR_PUTC( ' in x and ', BUFFER, NC )
            CALL CHR_PUTI( NYKNOT, BUFFER, NC )
         END IF
         CALL CHR_PUTC( ' in y.', BUFFER, NC )
         CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )

*  Record the bin sizes in the log file.
         NC = 0
         BUFFER = ' '
         CALL CHR_PUTC( 'The number of pixels per bin is ', BUFFER, NC )
         CALL CHR_PUTI( IX, BUFFER, NC )
         CALL CHR_PUTC( ' in x and ', BUFFER, NC )
         CALL CHR_PUTI( IY, BUFFER, NC )
         CALL CHR_PUTC( ' in y.', BUFFER, NC )
         CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )

*  Record the estimator in the log file.
         NC = 0
         BUFFER = ' '
         CALL CHR_PUTC( 'Estimator is ', BUFFER, NC )
         NCI = CHR_LEN( ESTIMA )
         CALL CHR_PUTC( ESTIMA( :NCI ), BUFFER, NC )
         CALL CHR_PUTC( '.', BUFFER, NC )
         CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )

*  Store the clipping values in the log file.
         IF ( ESTIMA .EQ. 'KSIGMA' .AND. NCLIP .GT. 0 ) THEN
            NC = 0
            BUFFER = ' '
            CALL CHR_PUTC( 'Clipping at ', BUFFER, NC )
            DO I = 1, NCLIP
               CALL CHR_PUTR( CLIP( I ), BUFFER, NC )
               IF ( I .NE. NCLIP ) CALL CHR_PUTC( ', ', BUFFER, NC )
            END DO
            CALL CHR_PUTC( ' standard deviations.', BUFFER, NC )
            CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )
         END IF

*  Record in the logfile the minimum permitted fraction of non-bad
*  pixels in a bin.
         NC = 0
         BUFFER = ' '
         CALL CHR_PUTC( 'The minimum fraction of non-bad '/
     :                  /'pixels in a bin is ', BUFFER, NC )
         CALL CHR_PUTR( WLIMIT, BUFFER, NC )
         CALL CHR_PUTC( '.', BUFFER, NC )
         CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )

*  Some more logging, first the lower threshold...
         IF ( LOTHRS ) THEN
            NC = 0
            BUFFER = ' '
            CALL CHR_PUTC( 'Pixel values below ', BUFFER, NC )
            CALL CHR_PUTR( THRLO, BUFFER, NC )
            CALL CHR_PUTC( ' are excluded from the binning.', BUFFER,
     :                     NC )
            CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )
         END IF

*  ...now the upper threshold.
         IF ( HITHRS ) THEN
            NC = 0
            BUFFER = ' '
            CALL CHR_PUTC( 'Pixel values above ', BUFFER, NC )
            CALL CHR_PUTR( THRHI, BUFFER, NC )
            CALL CHR_PUTC( ' are excluded from the binning.', BUFFER,
     :                     NC )
            CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )
         END IF

*  Record clipping of the bins to the fit.
         IF ( FITCLP ) THEN
            NC = 0
            BUFFER = ' '
            CALL CHR_PUTC( 'Clipping of the bins at ', BUFFER, NC )
            DO I = 1, NCLIPF
               CALL CHR_PUTR( CLIPF( I ), BUFFER, NC )
               IF ( I .NE. NCLIPF ) CALL CHR_PUTC( ', ', BUFFER, NC )
            END DO
            CALL CHR_PUTC( ' times the rms error of the fit.', BUFFER,
     :                     NC )
            CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )
         END IF

*  Record the method of evaluation.
         IF ( EVMETH( 1:3 ) .EQ. 'ALL' ) THEN
            BUFFER = 'All pixels are evaluated.'
            NC = 25
         ELSE
            BUFFER = 'Pixels are evaluated by bi-linear interpolation.'
            NC = 48
         END IF
         CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )
         CALL FIO_WRITE( FDL, ' ', STATUS )
      END IF

*  Calculate dimensions and set data type of work arrays.
*  ======================================================
      ALL = .FALSE.
      IF ( EVMETH( 1:3 ) .EQ. 'ALL' ) ALL = .TRUE.

*  Calculate the total number of bins and the storage requirement for
*  the polynomial-fitting routine.
      IF ( FITTYP( 1:3 ) .EQ. 'POL' ) THEN
         MAXBIN = NX * NY
         NWORK = 6

*  Calculate the number of free fitting parameters.
         MPCOEF = ( MIN( NXPAR, NYPAR ) *
     :            ( MIN( NXPAR, NYPAR ) + 1 ) ) / 2 +
     :            ABS( NXPAR - NYPAR )

*  The PDA library routine for solving the normal equations is only
*  available in double precision (even though the Chebyshev evaluations
*  are generic), so set the processing type to this data type.  This is
*  sensible for reliable results too.
         ITYPE = '_DOUBLE'
      ELSE

*  Calculate the total number of bins and the storage requirement for
*  the spline-fitting routine.
         MAXBIN = NX * NY + 2
         IF ( ALL ) NPOINT = 4 * ( DIMS( 1 ) + 1 )

         NWORK = 7

*  Find the dimensions of the workspace arrays for the spline-fitting
*  function.  Note that these sizes assume a bi-cubic spline.  These
*  formulae are specified by PDA_SURFIT (u is NXPAR, v is NYPAR, b1 is
*  S1, b2 is S2).
         NXPAR = NXKNOT + 4
         NYPAR = NYKNOT + 4
         IF ( NYKNOT .GT. NXKNOT ) THEN
            S1 = 3 * NYPAR + 4
            S2 = S1 + NYPAR - 3
         ELSE
            S1 = 3 * NXPAR + 4
            S2 = S1 + NXPAR - 3
         END IF
         NWS = NXPAR * NYPAR * ( 2 + S1 + S2 ) + 2 * ( NXPAR + NYPAR +
     :         4 * MAXBIN + 5 * ( MAX( NXKNOT, NYKNOT ) + 8 ) - 6 ) +
     :         S2 + 1
         NIWS = MAXBIN + ( NXKNOT + 1 ) * ( NYKNOT + 1 )
         NLWS = NXPAR * NYPAR * ( S2 + 1 ) + S2

*  The PDA library routine for spline surface fitting is only available
*  in single precision, so set the processing type to this data type.
         ITYPE = '_REAL'
      END IF

*  Map lots of work arrays.
*  ========================
*
*  This is a messy section...
*
*  Create and map temporary workspace. First for the bin x-y centroid,
*  value, weight, and for sorting in the spline fit.
      WOBT = 0
      DO I = 1, NWORK
         CALL PSX_CALLOC( MAXBIN, ITYPE, WPNTR( I ), STATUS )
         IF ( STATUS .EQ. SAI__OK ) WOBT = WOBT + 1
      END DO

*  Give the pointers some more-memorable names.  Later on there are many
*  subroutine calls with several pointers passed, and so it is quite
*  easier to mix them up via array indices.  X for x position, Y for y
*  position, Z for the data, W for the weights, G for general workspace
*  RES for the residuals, and FIT for storing the fitted data.  The
*  existing array names are used because they are easier for the
*  creation and annulling the workspace.
      XPTR = WPNTR( 1 )
      YPTR = WPNTR( 2 )
      ZPTR = WPNTR( 3 )
      WPTR = WPNTR( 4 )
      IF ( FITTYP( 1:3 ) .EQ. 'SPL' ) GPTR = WPNTR( 5 )
      RESPTR = WPNTR( NWORK - 1 )
      FITPTR = WPNTR( NWORK )

*  Next get space for the bin with and without thresholding.  Again
*  choose some more-memorable pointers: BIN for a bin of pixels, TBN
*  for the thresholded bin.
      CALL PSX_CALLOC( BINSIZ, '_REAL', WPNTR( WOBT + 1 ), STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         WOBT = WOBT + 1
         BINPTR = WPNTR( WOBT )
      END IF

      IF ( LOTHRS .OR. HITHRS ) THEN
         CALL PSX_CALLOC( IX * IY, '_REAL', WPNTR( WOBT + 1 ), STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Increment the number of work arrays obtained.
            WOBT = WOBT + 1
            TBNPTR = WPNTR( WOBT )
         END IF
      ELSE

*  No thresholds, but there needs to be a dummy argument for the
*  binning subroutine.
         DUMMY( 1 ) = 0.0
      END IF

*  Some space is also needed to hold the normal-equation coefficients.
*  Use a mnemonic pointer name.
      IF ( FITTYP( 1:3 ) .EQ. 'POL' ) THEN
         CALL PSX_CALLOC( MPCOEF * MPCOEF, ITYPE, WPNTR( WOBT + 1 ),
     :                    STATUS )

*  Increment the number of work arrays obtained.
         IF ( STATUS .EQ. SAI__OK ) THEN
            WOBT = WOBT + 1
            NEQPTR = WPNTR( WOBT )
         END IF
      END IF

*  Some space is also needed to manage the knots and panels, and for
*  workspace in a spline fit.
      IF ( FITTYP( 1:3 ) .EQ. 'SPL' ) THEN
         IF ( ALL ) THEN
            CALL PSX_CALLOC( NPOINT, ITYPE, WPNTR( WOBT + 1 ),
     :                       STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN

*  Increment the number of work arrays obtained and use a mnemonic
*  pointer.
               WOBT = WOBT + 1
               PANPTR = WPNTR( WOBT )
            END IF

            CALL PSX_CALLOC( DIMS( 1 ) + 1, '_INTEGER',
     :                       WPNTR( WOBT + 1 ), STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN

*  Increment the number of work arrays obtained and use a mnemonic
*  pointer.
               WOBT = WOBT + 1
               SEVPTR = WPNTR( WOBT )
            END IF
         END IF

*  Get workspace for spline fitting.
         CALL PSX_CALLOC8( NWS, '_REAL', WPNTR( WOBT + 1 ), STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*  Increment the number of work arrays obtained and use a mnemonic
*  pointer.
            WOBT = WOBT + 1
            SWPTR = WPNTR( WOBT )
         END IF

*  Get more workspace for spline fitting.
         CALL PSX_CALLOC8( NIWS, '_INTEGER', WPNTR( WOBT + 1 ), STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*  Increment the number of work arrays obtained and use a mnemonic
*  pointer.
            WOBT = WOBT + 1
            SIWPTR = WPNTR( WOBT )
         END IF

*  Get workspace to allow for rank-deficient system in spline fitting.
         CALL PSX_CALLOC8( NLWS, '_REAL', WPNTR( WOBT + 1 ), STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*  Increment the number of work arrays obtained and use a mnemonic
*  pointer.
            WOBT = WOBT + 1
            SLWPTR = WPNTR( WOBT )
         END IF
      END IF

*  Should all points be evaluated from the fitting function instead of
*  from a coarse grid with interpolation, there would be great
*  inefficiency calling two routines per pixel.  This workspace is to
*  enable the calls to be made for a line at a time.  It stores x
*  positions and values.  Again choose a more-memorable name for the
*  pointer.
      IF ( ALL ) THEN
         CALL PSX_CALLOC( DIMS( 1 ) * 2, ITYPE, WPNTR( WOBT + 1 ),
     :                    STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*  Increment the number of work arrays obtained.
            WOBT = WOBT + 1
            LINPTR = WPNTR( WOBT )
         END IF
      END IF

*  Check for an error during the laborious effort to obtain the
*  workspace before accessing the pointers.  This is the end of the
*  workspace section.
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Map the input and output arrays.
*  ================================

*  Map the input data array.  The binning routines take a _REAL array
*  as input but generate _REAL or _DOUBLE vectors.
      CALL KPG1_MAP( NDFI, 'Data', '_REAL', 'READ', PNTRI, EL, STATUS )

*  Map the data array (and possibly the variance) of the new NDF for
*  write access.
      IF ( CREVAR ) THEN
         CALL KPG1_MAP( NDFO, 'Data,Variance', '_REAL',
     :                  'WRITE/BAD', PNTRO, EL, STATUS )
      ELSE
         CALL KPG1_MAP( NDFO, 'Data', '_REAL',
     :                  'WRITE/BAD', PNTRO, EL, STATUS )
      END IF

*  Bin the data.
*  =============

*  Bin the data array, using the chosen estimator and thresholding.
*  As the two fitting methods use routines that use different
*  data types, store the bins' value, co-ordinates, and weight in the
*  implementation type.
      IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         IF ( LOTHRS .OR. HITHRS ) THEN
            CALL KPS1_SUBID( DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       IX, IY, ESTIMA, NCLIP, CLIP, THRLO, THRHI,
     :                       WLIMIT, MAXBIN, %VAL( CNF_PVAL( BINPTR ) ),
     :                       %VAL( CNF_PVAL( TBNPTR ) ),
     :                       %VAL( CNF_PVAL( XPTR ) ),
     :                       %VAL( CNF_PVAL( YPTR ) ),
     :                       %VAL( CNF_PVAL( ZPTR ) ),
     :                       %VAL( CNF_PVAL( WPTR ) ), NBIN, STATUS )
         ELSE
            CALL KPS1_SUBID( DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       IX, IY, ESTIMA, NCLIP, CLIP, THRLO, THRHI,
     :                       WLIMIT, MAXBIN, %VAL( CNF_PVAL( BINPTR ) ),
     :                       DUMMY,
     :                       %VAL( CNF_PVAL( XPTR ) ),
     :                       %VAL( CNF_PVAL( YPTR ) ),
     :                       %VAL( CNF_PVAL( ZPTR ) ),
     :                       %VAL( CNF_PVAL( WPTR ) ), NBIN, STATUS )
         END IF

*  Spline is single precision.
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN

         IF ( LOTHRS .OR. HITHRS ) THEN
            CALL KPS1_SUBIR( DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       IX, IY, ESTIMA, NCLIP, CLIP, THRLO, THRHI,
     :                       WLIMIT, MAXBIN, %VAL( CNF_PVAL( BINPTR ) ),
     :                       %VAL( CNF_PVAL( TBNPTR ) ),
     :                       %VAL( CNF_PVAL( XPTR ) ),
     :                       %VAL( CNF_PVAL( YPTR ) ),
     :                       %VAL( CNF_PVAL( ZPTR ) ),
     :                       %VAL( CNF_PVAL( WPTR ) ), NBIN, STATUS )
         ELSE
            CALL KPS1_SUBIR( DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       IX, IY, ESTIMA, NCLIP, CLIP, THRLO, THRHI,
     :                       WLIMIT, MAXBIN, %VAL( CNF_PVAL( BINPTR ) ),
     :                       DUMMY,
     :                       %VAL( CNF_PVAL( XPTR ) ),
     :                       %VAL( CNF_PVAL( YPTR ) ),
     :                       %VAL( CNF_PVAL( ZPTR ) ),
     :                       %VAL( CNF_PVAL( WPTR ) ), NBIN, STATUS )
         END IF

      END IF

*  Might have accidently removed all the data.
      IF ( NBIN .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SURFIT_NODAT',
     :     'SURFIT: No data to fit.', STATUS )
      END IF

      CALL MSG_OUTIF( MSG__NORM, 'BINNED', 'Data have been binned.',
     :                STATUS )

*  Write the binned data and errors to the log file.
*  =================================================

*  Present the binned data and the errors in the log file.
      IF ( LOGFIL .AND. STATUS .EQ. SAI__OK ) THEN
         CALL FIO_WRITE( FDL, ' ', STATUS )
         CALL FIO_WRITE( FDL, 'The binned data are:', STATUS )
         CALL FIO_WRITE( FDL, ' ', STATUS )

*  Convert the x-y list of binned values into a full array, using the
*  appropriate routine for the bins.
         IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG_LD2AR( MXBIN, NY, REAL( IX ), REAL( IY ),
     :                      NBIN, %VAL( CNF_PVAL( XPTR ) ),
     :                      %VAL( CNF_PVAL( YPTR ) ),
     :                      %VAL( CNF_PVAL( ZPTR ) ), BINNED, STATUS )
         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG_LR2AR( MXBIN, NY, REAL( IX ), REAL( IY ),
     :                      NBIN, %VAL( CNF_PVAL( XPTR ) ),
     :                      %VAL( CNF_PVAL( YPTR ) ),
     :                      %VAL( CNF_PVAL( ZPTR ) ), BINNED, STATUS )
         END IF

*  Write it out to the log file, already opened, hence the junk name.
         CALL LISTSB( BINNED, MXBIN, MXBIN, 1, 1, NX, NY,
     :                .FALSE., FDL, FILNAM, STATUS )

*  Now for the errors.
         CALL FIO_WRITE( FDL, ' ', STATUS )
         CALL FIO_WRITE( FDL, 'The errors of the binned data are:',
     :                   STATUS )
         CALL FIO_WRITE( FDL, ' ', STATUS )

*  Convert the x-y list of binned values into a full array, using the
*  appropriate routine for the bins.
         IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG_LD2AR( MXBIN, NY, REAL( IX ), REAL( IY ),
     :                      NBIN, %VAL( CNF_PVAL( XPTR ) ),
     :                      %VAL( CNF_PVAL( YPTR ) ),
     :                      %VAL( CNF_PVAL( WPTR ) ), BINNED, STATUS )
         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG_LR2AR( MXBIN, NY, REAL( IX ), REAL( IY ),
     :                      NBIN, %VAL( CNF_PVAL( XPTR ) ),
     :                      %VAL( CNF_PVAL( YPTR ) ),
     :                      %VAL( CNF_PVAL( WPTR ) ), BINNED, STATUS )
         END IF

*  Write it out to the log file, already opened, hence the junk name.
         CALL LISTSB( BINNED, MXBIN, MXBIN, 1, 1, NX, NY,
     :                .FALSE., FDL, FILNAM, STATUS )
      END IF

*  Fit Chebyshev polynomial.
*  =========================
      IF ( FITTYP( 1:3 ) .EQ. 'POL' .AND. STATUS .EQ. SAI__OK ) THEN

*  Set up image boundaries.
         DXMIN = 0.5D0
         DXMAX = DBLE( DIMS( 1 ) ) + 0.5D0
         DYMIN = 0.5D0
         DYMAX = DBLE( DIMS( 2 ) ) + 0.5D0

*  Map work arrays (MPCOEF x MPCOEF) in size to hold the covariance
*  matrix, and a work array used by routine KPS1_FSPF2 below.
         CALL PSX_CALLOC( MPCOEF * MPCOEF, '_DOUBLE', CPTR, STATUS )
         CALL PSX_CALLOC( MPCOEF * MPCOEF, '_DOUBLE', MPTR, STATUS )

*  Indicates whether or not the weights have to be squared to give the
*  correct values for the Chebyshev least-squares fit.
         FIRST = .TRUE.

         DO M = 0, NCLIPF

*  Fit a polynomial surface to the binned array.  Use this routine in
*  preference to KPS1_SUPF because it computes the variance of the
*  coefficients.
            CALL KPS1_FSPF2( DXMIN, DXMAX, DYMIN, DYMAX, NXPAR, NYPAR,
     :                       FIRST, NBIN, MPCOEF, MAXBIN,
     :                       %VAL( CNF_PVAL( XPTR ) ),
     :                       %VAL( CNF_PVAL( YPTR ) ),
     :                       %VAL( CNF_PVAL( ZPTR ) ),
     :                       %VAL( CNF_PVAL( WPTR ) ),
     :                       %VAL( CNF_PVAL( NEQPTR ) ),
     :                       %VAL( CNF_PVAL( MPTR ) ),
     :                       %VAL( CNF_PVAL( CPTR ) ),
     :                       CHCOEF, VARIAN, NCOEF, STATUS )

            FIRST = .FALSE.

*  Evaluate and log the the rms error of the fit.
*  ==============================================

*  Evaluate the surface at each bin and obtain the RMS error and the
*  residuals of the fit.
            CALL KPS1_FSPE2( NBIN, %VAL( CNF_PVAL( XPTR ) ),
     :                       %VAL( CNF_PVAL( YPTR ) ),
     :                       %VAL( CNF_PVAL( ZPTR ) ),
     :                       DXMIN, DXMAX, DYMIN, DYMAX,
     :                       NXPAR, NYPAR, MCHOEF, CHCOEF,
     :                       %VAL( CNF_PVAL( FITPTR ) ),
     :                       %VAL( CNF_PVAL( RESPTR ) ), DRMS, STATUS )

*  Determine the maximum absolute residual.
            CALL KPG1_MXMND( BAD, NBIN, %VAL( CNF_PVAL( RESPTR ) ),
     :                       NINVAL, MAXMUM, MINMUM, MAXPOS, MINPOS,
     :                       STATUS )
            RSMAX = MAX( ABS( MAXMUM ), ABS( MINMUM ) )
            RMS = SNGL( DRMS )

*  Report the latest rms error.  It should be decreasing each cycle.
            NC = 0
            BUFFER = ' '
            CALL CHR_PUTC( 'The rms error of the binned data to '/
     :                     /'the fit is ', BUFFER, NC )
            CALL CHR_PUTR( RMS, BUFFER, NC )
            CALL CHR_PUTC( ' from ', BUFFER, NC )
            CALL CHR_PUTI( NBIN, BUFFER, NC )
            CALL CHR_PUTC( ' bins.', BUFFER, NC )
            CALL MSG_OUTIF( MSG__NORM, 'BINRMS', BUFFER, STATUS )
            IF ( LOGFIL ) CALL FIO_WRITE( FDL, BUFFER(:NC ), STATUS )

*  Perform clipping of the fitted bins.
*  ====================================

*  Determine when to clip the fit.  Note there is no clipping for the
*  evaluation cycle.
            IF ( FITCLP .AND. M .LT. NCLIPF .AND.
     :           STATUS .EQ. SAI__OK ) THEN

*  Remove deviant bins from the fit by deriving the residuals to the
*  fit.
               NELM = NBIN
               CALL KPS1_SUCLD( NELM, %VAL( CNF_PVAL( FITPTR ) ),
     :                          RMS, CLIPF( M+1 ),
     :                          %VAL( CNF_PVAL( XPTR ) ),
     :                          %VAL( CNF_PVAL( YPTR ) ),
     :                          %VAL( CNF_PVAL( ZPTR ) ),
     :                          %VAL( CNF_PVAL( WPTR ) ), NBIN,
     :                          STATUS )

*  Report the progress of the clipping.
               CALL MSG_SETR( 'CLIP', CLIPF( M + 1 ) )
               CALL MSG_OUTIF( MSG__NORM, 'CLIPRMS', 'Binned data '/
     :            /'clipped where the absolute residuals to the fit '/
     :            /'exceeded ^CLIP times the rms error.', STATUS )

*  Might have accidently removed all the data.
               IF ( NBIN .LT. 1 ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'SURFIT_NODAT',
     :              'SURFIT: No data to fit.', STATUS )
               END IF
            END IF
         END DO

*  Fill the output data array.
*  ===========================

*  Clipping (if any) has now completed.  Evaluate the fit for the
*  original pixels, by one of two methods.
         IF ( ALL .AND. STATUS .EQ. SAI__OK ) THEN

*  Evaluate the polynomial for each pixel a line at time.
            CALL KPS1_SUPEV( DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       DXMIN, DXMAX, DYMIN, DYMAX, NXPAR, NYPAR,
     :                       MCHOEF, CHCOEF,
     :                       %VAL( CNF_PVAL( LINPTR ) ),
     :                       %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       RMSF, STATUS )
         ELSE

*  Evaluate the polynomial at the bin corners, and obtain the pixel
*  values by bi-linear interpolation.
            CALL KPS1_SUPEI( DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       IX, IY, DXMIN, DXMAX, DYMIN, DYMAX, NXPAR,
     :                       NYPAR, MCHOEF, CHCOEF,
     :                       %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       RMSF, STATUS )
         END IF

*  If the fit has been successful, write the results to an extension
*  named SURFACEFIT.  The coefficients will be stored in a structure
*  within this called FIT of type POLYNOMIAL (see SGP/38 for a
*  description of the contents of a POLYNOMIAL structure).
         CALL KPS1_FSWPE( NDFO, DXMIN, DXMAX, DYMIN, DYMAX, NXPAR,
     :                    NYPAR, MCHOEF, CHCOEF, VARIAN, SNGL( RSMAX ),
     :                    RMS, 'GRID', STATUS )

*  Fit a bi-cubic spline.
*  ======================
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise knots.
         DO I = 1, MTKNOT
            XKNOT( I ) = 0.0
            YKNOT( I ) = 0.0
         END DO

*  Set up image boundaries used to find the centres of the bottom-left
*  and top-right corners of the image, so that an additional pixel can
*  be placed there to constrain the fit and prevent undesirable edge
*  effects.
         XMIN = 1.0
         XMAX = REAL( DIMS( 1 ) )
         YMIN = 1.0
         YMAX = REAL( DIMS( 2 ) )
         FIRST = .TRUE.
         NBIN8 = NBIN

         DO M = 0, NCLIPF

*  Fit the bi-cubic-spline surface to the binned array.
            CALL KPS1_SUSF( NXKNOT, NYKNOT, XMIN, XMAX, YMIN, YMAX,
     :                      MTKNOT, FIRST, NWS, NLWS, NIWS, MAXBIN,
     :                      %VAL( CNF_PVAL( XPTR ) ),
     :                      %VAL( CNF_PVAL( YPTR ) ),
     :                      %VAL( CNF_PVAL( ZPTR ) ),
     :                      %VAL( CNF_PVAL( WPTR ) ),
     :                      %VAL( CNF_PVAL( GPTR ) ), NBIN8,
     :                      XKNOT, YKNOT, %VAL( CNF_PVAL( SWPTR ) ),
     :                      %VAL( CNF_PVAL( SLWPTR ) ),
     :                      %VAL( CNF_PVAL( SIWPTR ) ),
     :                      COEFF, NCOEF, SCALE, STATUS )

            FIRST = .FALSE.

*  The two special additional clamping bins are excluded. They will be
*  included again at the end of the list by SPL2D.
            NELM = NBIN - 2

*  Only apply reciprocal scaling if this is the last evaluation of the
*  bins, i.e. no more clipping is to follow.
            IF ( FITCLP .AND. M .LT. NCLIPF ) THEN
               DSCALE = -1.
            ELSE
               DSCALE = SCALE
            END IF

*  Determine and report the rms error of the fit.
*  ==============================================

*  Evaluate the surface at each bin and obtain the rms error of the
*  fit. -1 prevent re-scaling because clipping below needs to work with
*  the scaled fit (if there has been any scaling).
            CALL KPS1_SUSEB( %VAL( CNF_PVAL( XPTR ) ),
     :                       %VAL( CNF_PVAL( YPTR ) ),
     :                       %VAL( CNF_PVAL( ZPTR ) ),
     :                       NELM, NXKNOT, NYKNOT, XKNOT, YKNOT, NCOEF,
     :                       COEFF, DSCALE, %VAL( CNF_PVAL( FITPTR ) ),
     :                       %VAL( CNF_PVAL( RESPTR ) ), RMS, STATUS )

*  Determine the maximum absolute residual.
            CALL KPG1_MXMNR( BAD, NELM, %VAL( CNF_PVAL( RESPTR ) ),
     :                       NINVAL, RMAX, RMIN, MAXPOS, MINPOS,
     :                       STATUS )
            RSMAX = DBLE( MAX( ABS( RMAX ), ABS( RMIN ) ) )

*  Report the latest rms error. It should be decreasing each cycle.
*  Note the RMS is already scaled correctly at this point during the
*  last clipping cycle or there has been no scaling.
            NC = 0
            BUFFER = ' '
            CALL CHR_PUTC( 'The rms error of the binned data to '/
     :                     /'the fit is ', BUFFER, NC )
            IF ( SCALE .GT. 0.0 .AND. DSCALE .LT. 0.0 ) THEN
               CALL CHR_PUTR( RMS / SCALE, BUFFER, NC )
            ELSE
               CALL CHR_PUTR( RMS, BUFFER, NC )
            END IF
            CALL CHR_PUTC( ' from ', BUFFER, NC )
            CALL CHR_PUTI( NELM, BUFFER, NC )
            CALL CHR_PUTC( ' bins.', BUFFER, NC )
            CALL MSG_OUTIF( MSG__NORM, 'BINRMS', BUFFER, STATUS )
            IF ( LOGFIL ) CALL FIO_WRITE( FDL, BUFFER(:NC ), STATUS )

*  Perform clipping of the fitted bins.
*  ====================================

*  Determine when to clip the fit.  Note there is no clipping for the
*  evaluation cycle.
            IF ( FITCLP .AND. M .LT. NCLIPF .AND.
     :           STATUS .EQ. SAI__OK ) THEN

*  Remove deviant bins from the fit by deriving the residuals to the
*  fit.
               CALL KPS1_SUCLR( NELM, %VAL( CNF_PVAL( FITPTR ) ),
     :                          RMS, CLIPF( M+1 ),
     :                          %VAL( CNF_PVAL( XPTR ) ),
     :                          %VAL( CNF_PVAL( YPTR ) ),
     :                          %VAL( CNF_PVAL( ZPTR ) ),
     :                          %VAL( CNF_PVAL( WPTR ) ), NBIN,
     :                          STATUS )

*  Report the progress of the clipping.
               CALL MSG_SETR( 'CLIP', CLIPF( M + 1 ) )
               CALL MSG_OUTIF( MSG__NORM, 'CLIPRMS', 'Binned data '/
     :           /'clipped where the absolute residuals to the fit '/
     :           /'exceeded ^CLIP times the rms error.', STATUS )

*  Might have accidently removed all the data.
               IF ( NBIN .LT. 1 ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'SURFIT_NODAT',
     :              'SURFIT: No data to fit.', STATUS )
               END IF
            END IF
         END DO

*  Fill the output data array.
*  ===========================

*  Clipping (if any) has now completed.  Evaluate the fit for the
*  original pixels, by one of two methods.
         IF ( ALL .AND. STATUS .EQ. SAI__OK ) THEN

*  Evaluate the polynomial for each pixel a line at time.
            CALL KPS1_SUSEV( DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       NXKNOT, NYKNOT, XKNOT, YKNOT, NCOEF, COEFF,
     :                       SCALE, NPOINT, %VAL( CNF_PVAL( PANPTR ) ),
     :                       %VAL( CNF_PVAL( LINPTR ) ),
     :                       %VAL( CNF_PVAL( SEVPTR ) ),
     :                       %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       RMSF, STATUS )
         ELSE

*  Evaluate the polynomial at the bin corners, and obtain the pixel
*  values by bi-linear interpolation.
            CALL KPS1_SUSEI( DIMS( 1 ), DIMS( 2 ),
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       IX, IY, NXKNOT, NYKNOT, XKNOT, YKNOT,
     :                       NCOEF, COEFF, SCALE,
     :                       %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       RMSF, STATUS )
         END IF

*  The two special additional clamping bins are excluded from any
*  logging.
         NBIN = NBIN - 2

*  If the fit has been successful, write the results to an extension
*  named SURFACEFIT.  The coefficients will be stored in a structure
*  within this called FIT of type POLYNOMIAL (see SGP/38 for a
*  description of the contents of a POLYNOMIAL structure).
         CALL KPS1_FSWSE( NDFO, NXKNOT, NYKNOT, XKNOT, YKNOT, MCHOEF,
     :                    COEFF, SCALE, SNGL( RSMAX ), RMS, 'GRID',
     :                    STATUS )

      END IF

*  Assign the RMS residual squared to the variance.
      IF ( CREVAR ) THEN
         CALL KPG1_FILLR( RMS * RMS, EL, %VAL( CNF_PVAL( PNTRO( 2 ) ) ),
     :                    STATUS )
      END IF

*  Report the fitted data and residuals to the log file.
*  =====================================================

*  Present the fitted binned data and the residuals in the log file.
      IF ( LOGFIL .AND. STATUS .EQ. SAI__OK ) THEN
         CALL FIO_WRITE( FDL, ' ', STATUS )
         CALL FIO_WRITE( FDL, 'The fitted binned data are:', STATUS )
         CALL FIO_WRITE( FDL, ' ', STATUS )

*  Convert the x-y list of binned values into a full array, using the
*  appropriate routine for the bins.
         IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG_LD2AR( MXBIN, NY, REAL( IX ), REAL( IY ), NBIN,
     :                      %VAL( CNF_PVAL( XPTR ) ),
     :                      %VAL( CNF_PVAL( YPTR ) ),
     :                      %VAL( CNF_PVAL( FITPTR ) ),
     :                      BINNED, STATUS )
         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG_LR2AR( MXBIN, NY, REAL( IX ), REAL( IY ), NBIN,
     :                      %VAL( CNF_PVAL( XPTR ) ),
     :                      %VAL( CNF_PVAL( YPTR ) ),
     :                      %VAL( CNF_PVAL( FITPTR ) ),
     :                      BINNED, STATUS )
         END IF

*  Write it out to the log file, already opened, hence the junk name.
         CALL LISTSB( BINNED, MXBIN, MXBIN, 1, 1, NX, NY,
     :                .FALSE., FDL, FILNAM, STATUS )

*  Now for the residuals.
         CALL FIO_WRITE( FDL, ' ', STATUS )
         CALL FIO_WRITE( FDL, 'The residuals of the binned data '/
     :                   /'are:', STATUS )
         CALL FIO_WRITE( FDL, ' ', STATUS )

*  Convert the x-y list of binned values into a full array, using the
*  appropriate routine for the bins.
         IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG_LD2AR( MXBIN, NY, REAL( IX ), REAL( IY ), NBIN,
     :                      %VAL( CNF_PVAL( XPTR ) ),
     :                      %VAL( CNF_PVAL( YPTR ) ),
     :                      %VAL( CNF_PVAL( RESPTR ) ),
     :                      BINNED, STATUS )
         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG_LR2AR( MXBIN, NY, REAL( IX ), REAL( IY ), NBIN,
     :                      %VAL( CNF_PVAL( XPTR ) ),
     :                      %VAL( CNF_PVAL( YPTR ) ),
     :                      %VAL( CNF_PVAL( RESPTR ) ),
     :                      BINNED, STATUS )
         END IF

*  Write it out to the log file, already opened, hence the junk name.
         CALL LISTSB( BINNED, MXBIN, MXBIN, 1, 1, NX, NY,
     :                .FALSE., FDL, FILNAM, STATUS )
      END IF

*  Report the rms error of fit.
      NC = 0
      BUFFER = ' '
      IF ( LOGFIL ) CALL FIO_WRITE( FDL, ' ', STATUS )
      CALL CHR_PUTC( 'RMS deviation of the fit from the original is ',
     :               BUFFER, NC )
      CALL CHR_PUTR( RMSF, BUFFER, NC )
      CALL CHR_PUTC( ' per pixel.', BUFFER, NC )
      CALL MSG_OUTIF( MSG__NORM, 'RMSFIT', BUFFER, STATUS )
      IF ( LOGFIL ) CALL FIO_WRITE( FDL, BUFFER( :NC ), STATUS )

*  Save it in an output parameter.
      CALL PAR_PUT0R( 'RMS', RMSF, STATUS )

*  Closedown sequence.
*  ===================

*  Tidy up the workspace.
  980 CONTINUE
      DO I = 1, WOBT
         CALL PSX_FREE( WPNTR( I ), STATUS )
      END DO

  999 CONTINUE

*  End of logging.
      IF ( LOGFIL ) CALL FIO_ANNUL( FDL, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

      END
