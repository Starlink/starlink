      SUBROUTINE FITSURFACE( STATUS )
*+
*  Name:
*     FITSURFACE

*  Purpose:
*     Fits a polynomial surface to two-dimensional data array.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FITSURFACE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This task fits a surface to a two-dimensional data array stored
*     array within an NDF data structure.  At present it only
*     permits a fit with a polynomial, and the coefficients of that
*     surface are stored in a POLYNOMIAL structure (SGP/38) as an
*     extension to that NDF.
*
*     Unlike SURFIT, neither does it bin the data nor does it reject
*     outliers.

*  Usage:
*     fitsurface ndf [fittype] { nxpar nypar
*                              { [knots]
*                           fittype

*  ADAM Parameters:
*     COSYS = LITERAL (Read)
*        The co-ordinate system to be used.  This can be either "World"
*        or "Data".  If COSYS = "World" the co-ordinates used to fits
*        the surface are pixel co-ordinates.  If COSYS = "Data" the
*        data co-ordinates used are used in the fit, provided there are
*        axis centres present in the NDF.  COSYS="World" is
*        recommended.  [Current co-ordinate system]
*     FITTYPE = LITERAL (Read)
*        The type of fit.  It must be either "Polynomial" for a
*        polynomial or "Spline" for a bi-cubic spline.  ["Polynomial"]
*     KNOTS( 2 ) = _INTEGER (Read)
*        The number of interior knots used for the bi-cubic-spline fit
*        along the x and y axes.  These knots are equally spaced within
*        the image.  Both values must be in the range 0 to 11.  If you
*        supply a single value, it applies to both axes.  Thus 1
*        creates one interior knot, [5,4] gives five along the x axis
*        and four along the y direction.  Increasing this parameter
*        values increases the flexibility of the surface.  Normally, 4
*        is a reasonable value.  The upper limit of acceptable values
*        will be reduced along each axis when its binned array dimension
*        is fewer than 29.  KNOTS is only accessed when FITTYPE="Spline".
*        The default is the current value, which is 4 initially. []
*     NDF  = NDF (Update)
*        The NDF containing the two-dimensional data array to be fitted.
*     NXPAR = _INTEGER (Read)
*        The number of fitting parameters to be used in the x
*        direction.  It must be in the range 1 to 15 for a polynomial
*        fit.  Thus 1 gives a constant, 2 a linear fit, 3 a quadratic etc.
*        Increasing this parameter increases the flexibility of the
*        surface in the x direction.  The upper limit of acceptable values
*        will be reduced for arrays with an x dimension less than 29.
*        NXPAR is only accessed when FITTYPE="Polynomial".
*     NYPAR = _INTEGER (Read)
*        The number of fitting parameters to be used in the y
*        direction.  It must be in the range 1 to 15 for a polynomial
*        fit.  Thus 1 gives a constant, 2 a linear fit, 3 a quadratic
*        etc.  Increasing this parameter increases the flexibility of the
*        surface in the y direction.  The upper limit of acceptable values
*        will be reduced for arrays with a y dimension less than 29.
*        NYPAR is only accessed when FITTYPE="Polynomial".
*     OVERWRITE = _LOGICAL (Read)
*        OVERWRITE=TRUE, allows an NDF extension containing an existing
*        surface fit to be overwritten.  OVERWRITE=FALSE protects an
*        existing surface-fit extension, and should one exist, an error
*        condition will result and the task terminated.  [TRUE]
*     VARIANCE = _LOGICAL (Read)
*        A flag indicating whether any variance array present in the
*        NDF is used to define the weights for the fit.  If VARIANCE
*        is TRUE and the NDF contains a variance array this will be
*        used to define the weights, otherwise all the weights will be
*        set equal.  [TRUE]
*     XMAX = _DOUBLE (Read)
*        The maximum x value to be used in the fit.  This must be
*        greater than or equal to the x co-ordinate of the right-hand
*        pixel in the data array.  Normally this parameter is
*        automatically set to the maximum x co-ordinate found in the
*        data, but this mechanism can be overridden by specifying XMAX
*        on the command line.  The parameter is provided to allow the
*        fit limits to be fine tuned for special purposes.  It should
*        not normally be altered.  If a null (!) value is supplied, the
*        value used is the maximum x co-ordinate of the fitted data. [!]
*     XMIN = _DOUBLE (Read)
*        The minimum x value to be used in the fit.  This must be
*        smaller than or equal to the x co-ordinate of the left-hand
*        pixel in the data array.  Normally this parameter is
*        automatically set to the minimum x co-ordinate found in the
*        data, but this mechanism can be overridden by specifying XMIN
*        on the command line.  The parameter is provided to allow the
*        fit limits to be fine tuned for special purposes.  It should
*        not normally be altered.  If a null (!) value is supplied, the
*        value used is the minimum x co-ordinate of the fitted data. [!]
*     YMAX = _DOUBLE (Read)
*        The maximum y value to be used in the fit.  This must be
*        greater than or equal to the y co-ordinate of the top pixel in
*        the data array.  Normally this parameter is automatically set
*        to the maximum y co-ordinate found in the data, but this
*        mechanism can be overridden by specifying YMAX on the command
*        line.  The parameter is provided to allow the fit limits to be
*        fine tuned for special purposes.  It should not normally be
*        altered.  If a null (!) value is supplied, the value used is
*        the maximum y co-ordinate of the fitted data. [!]
*     YMIN = _DOUBLE (Read)
*        The minimum y value to be used in the fit.  This must be
*        smaller than or equal to the y co-ordinate of the bottom pixel
*        in the data array.  Normally this parameter is automatically
*        set to the minimum y co-ordinate found in the data, but this
*        mechanism can be overridden by specifying YMIN on the command
*        line.  The parameter is provided to allow the fit limits to be
*        fine tuned for special purposes.  It should not normally be
*        altered.  If a null (!) value is supplied, the value used is
*        the minimum y co-ordinate of the fitted data. [!]

*  Examples:
*     fitsurface virgo nxpar=4 nypar=4 novariance
*        This fits a bi-cubic polynomial surface to the data array
*        in the NDF called virgo.  All the data values are given
*        equal weight.  The coefficients of the fitted surface are
*        stored in an extension of virgo.
*     fitsurface virgo nxpar=4 nypar=4
*        As the first example except the data variance, if present,
*        is used to weight the data values.
*     fitsurface virgo fittype=spl
*        As the previous example except a B-spline fit is made using
*        four interior knots along both axes.
*     fitsurface virgo fittype=spl knots=[10,7]
*        As the previous example except now there are ten interior knots
*        along the x axis and seven along the y axis.
*     fitsurface mkn231 nxpar=6 nypar=2 cosys=d xmin=-10.0 xmax=8.5
*        This fits a polynomial surface to the data array in the NDF
*        called mkn231.  A fifth order is used along the x direction,
*        but only a linear fit along the y direction.  The fit is made
*        between x data co-ordinates -10.0 to 8.5.  The variance
*        weights the data values.  The coefficients of the fitted
*        surface are stored in an extension of mkn231.

*  Notes:
*     A polynomial surface fit is stored in a SURFACEFIT extension,
*     component FIT of type POLYNOMIAL, variant CHEBYSHEV or BSPLINE.
*     This is read by MAKESURFACE to create a NDF of the fitted surface.
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
*     Also stored in the SURFACEFIT extension are the r.m.s. deviation
*     to the fit (component RMS), the maximum absolute deviation
*     (component RSMAX), and the co-ordinate system (component COSYS)
*     translated to AST Domain names AXIS (for parameter COSYS="Data")
*     and PIXEL ("World").

*  Related Applications:
*     KAPPA: MAKESURFACE, SURFIT.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, and HISTORY components of an NDF data structure.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using double-precision floating point.

*  Implementation Deficiencies:
*     There is no logfile.  Clipping outliers is not yet supported.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995-1997, 2003-2004 Central Laboratory of the
*     Research Councils.
*     Copyright (C) 2007, 2009 Science & Technology Facilities Council.
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
*     SMB: Steven M. Beard (ROE)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     20-Apr-1993 (SMB):
*        Original version, based on the KAPPA function SURFIT written
*        by Malcolm Currie.
*     22-Apr-1993 (SMB):
*        Modified to use PLYPUT2D.
*     23-Apr-1993 (SMB):
*        DAT_PAR included (commented out) so the routine can work in a
*        UNIX environment.
*     06-May-1993 (SMB):
*        DAT_PAR does not need to be commented out.
*     02-Jun-1993 (SMB):
*        Modified to report some goodness of fit information.
*     08-Nov-1993 (SMB):
*        Modified to allow the x and y extrema returned by ARXYZW to be
*        overridden by specifying XMIN, XMAX, YMIN, YMAX parameters.
*     07-Dec-1993 (SMB):
*        Comments tidied up.
*     1995 August 2 (MJC):
*        Used a modern prologue and completed it.  Renamed many of the
*        routines and called existing subroutines rather than use SMB's
*        new ones.  Added COSYS parameter, and stored its value in the
*        SURFACEFIT extension.  Obtain axis centres in double
*        precision.  Insisted on two significant dimensions in the NDF.
*        Used PSX to get workspace to improve efficiency.
*     1996 October 10 (MJC):
*        Remove one work array no longer needed for NAG-free
*        subroutines.
*     1997 May 10 (MJC):
*        Computes (via SVD) and records the variances of the polynomial
*        coefficients.
*     27-AUG-2003 (DSB):
*        Check that the values obtained for XMAX, XMIN, YMAX and YMIN
*        are usable.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2007 June 28 (MJC):
*        Translate COSYS values to AXIS and PIXEL to bring it more into
*        modern AST parlance.  Use new KPS1_FSWPE routine to create
*        SURFACEFIT extension.
*     2007 July 3 (MJC):
*        Added bi-cubic spline fitting and parameter KNOTS.
*     2009 December 19 (MJC):
*        Fix bug in the calculation of required workspace for splines
*        when the number of knots are different along the two axes.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PRM_PAR'          ! Magic-value definitions (VAL__BADx)
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global Status

*  Local Constants:
      INTEGER MXPAR              ! Maximum number of parameters which
                                 ! can be handled in each direction.
                                 ! Should be the same as in PLY2D.
      PARAMETER ( MXPAR = 15 )

      INTEGER MCHOEF             ! Maximum number of Chebyshev
                                 ! polynomial coefficients
      PARAMETER ( MCHOEF = MXPAR * MXPAR )

      INTEGER MXKNOT             ! Maximum number of interior knots that
                                 ! can be handled in each direction.
      PARAMETER ( MXKNOT = 11 )

      INTEGER MBCOEF             ! Maximum number of B-spline
                                 ! coefficients
      PARAMETER ( MBCOEF = ( MXKNOT + 4 ) ** 2 )

      INTEGER MTKNOT             ! Maximum total number of knots that
                                 ! can be handled in each direction.
      PARAMETER ( MTKNOT = MXKNOT + 8 )

      INTEGER NDIM               ! Maximum number of dimensions---only
      PARAMETER ( NDIM = 2 )     ! two-dimensional arrays can be handled

*  Local Variables:
      INTEGER ACTVAL             ! Number of parameter values supplied
      INTEGER APTR               ! Pointer to workspace
      INTEGER AXPTR              ! Pointer to mapped 1st (x) axis array
      INTEGER AYPTR              ! Pointer to mapped 2nd (y) axis array
      LOGICAL BAD                ! Bad pixels may be present?
      DOUBLE PRECISION CHCOEF( MCHOEF ) ! Chebyshev coefficients of fit
      REAL COEFF( MBCOEF )       ! B-spline coefficients of the fit
      CHARACTER * ( 5 ) COSYS    ! Co-ordinate system
      INTEGER CPTR               ! Pointer to mapped covariance matrix
      INTEGER DPTR               ! Pointer to mapped data array
      DOUBLE PRECISION DRMS      ! R.M.S. deviation of fit
      INTEGER DSIZE              ! Number of elements in data array
      DOUBLE PRECISION DXMAX     ! Upper x position limit of the fit
      DOUBLE PRECISION DXMIN     ! Lower x position limit of the fit
      DOUBLE PRECISION DYMAX     ! Upper y position limit of the fit
      DOUBLE PRECISION DYMIN     ! Lower y position limit of the fit
      INTEGER EL                 ! General "number of elements" variable
      CHARACTER * ( 16 ) FITYPE  ! Type of fit ('POLYNOMIAL'|'SPLINE')
      INTEGER FTPTR              ! Pointer to mapped fit array
      INTEGER GPTR               ! Pointer: sort-positions workspace
      INTEGER I                  ! Loop counter
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Data type for processing
      INTEGER LBND( NDIM )       ! Lower bound of data array
      INTEGER MAXKNO( NDIM )     ! Maximum KNOT values
      DOUBLE PRECISION MAXMUM    ! Maximum value
      INTEGER MAXORD( NDIM )     ! Maximum ORDER values
      INTEGER MAXPOS             ! Index of maximum array value
      INTEGER MINKNO( NDIM )     ! Minimum KNOT values
      DOUBLE PRECISION MINMUM    ! Minimum value
      INTEGER MINORD( NDIM )     ! Minimum NXPAR/NYPAR values
      INTEGER MINPOS             ! Index of minimum array value
      INTEGER MPCOEF             ! Maximum number of polynomial
                                 ! coefficients for chosen NXPAR and
                                 ! NYPAR
      INTEGER MPTR               ! Pointer to SVD V matrix
      INTEGER NCOEF              ! Number of coefficients
      INTEGER NDFI               ! Identifier for NDF
      LOGICAL NDFVAR             ! NDF contains a variance array?
      INTEGER NGOOD              ! Number of good pixels
      INTEGER*8 NGOOD8           ! Number of good pixels
      INTEGER NINVAL             ! Number of bad values
      INTEGER*8 NIWS             ! Storage space size for spline fit
      INTEGER NKNOT( NDIM )      ! Number of knots in each direction
      INTEGER*8 NLWS             ! Storage space size to allow for A
                                 ! rank-deficiency system
      INTEGER*8 NWS              ! Size of spline-fitting routine's
                                 ! workspace
      INTEGER NXKNOT             ! Number of knots in x direction
      INTEGER NXPAR              ! Number of fitting parameters in x
                                 ! direction
      INTEGER NYKNOT             ! Number of knots in y direction
      INTEGER NYPAR              ! Number of fitting parameters in y
                                 ! direction
      LOGICAL OVERWR             ! Allow surface fits to be overwritten?
      REAL RMAX                  ! Maximum value (single precision)
      REAL RMIN                  ! Minimum value (single precision)
      REAL RMS                   ! R.M.S. deviation of fit
      DOUBLE PRECISION RSMAX     ! Maximum residual
      INTEGER RSPTR              ! Pointer to mapped residuals array
      INTEGER S1                 ! Used to eval size of spline workspace
      INTEGER S2                 ! Used to eval size of spline workspace
      REAL SCALE                 ! Scale factor applied before fitting
                                 ! to improve the fit and must be
                                 ! re-applied in reciprocal
      INTEGER SDIM( NDF__MXDIM ) ! Significant NDF dimensions
      INTEGER SIWPTR             ! Pointer: spline-fitting workspace
      INTEGER SLWPTR             ! Pointer: rank-deficient workspace
      INTEGER SWPTR              ! Pointer: spline-fitting workspace
      LOGICAL THERE              ! SURFACEFIT extension is present?
      INTEGER UBND( NDIM )       ! Upper bound of data array
      LOGICAL USEVAR             ! Allow weights to be derived from the
                                 ! NDF's variance array (if present)
      DOUBLE PRECISION VARIAN( MCHOEF ) ! Variance of Chebyshev coeffs.
      LOGICAL VARWTS             ! Weights are to be derived from
                                 ! variance?
      INTEGER VPTR               ! Pointer to mapped variance array
      INTEGER VSIZE              ! Number of elements in variance array
      INTEGER WPTR               ! Pointer to mapped weight array
      INTEGER XDIM               ! First (x) dimension of data array
      REAL XKNOT( MTKNOT )       ! Spline knots in x direction
      REAL XMAX                  ! Upper x position limit of the fit
      REAL XMIN                  ! Lower x position limit of the fit
      INTEGER XPTR               ! Pointer to mapped x co-ordinate array
      INTEGER YDIM               ! Second (y) dimension of data array
      REAL YKNOT( MTKNOT )       ! Spline knots in y direction
      REAL YMAX                  ! Upper y position limit of the fit
      REAL YMIN                  ! Lower y position limit of the fit
      INTEGER YPTR               ! Pointer to mapped y co-ordinate array
      INTEGER ZPTR               ! Pointer to mapped data value array

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the NDF to fit.
*  ======================

*  Begin the NDF context.
      CALL NDF_BEGIN

*  Get the NDF containing the input data.  There must be only two
*  significant dimensions.
      CALL KPG1_GTNDF( 'NDF', NDIM, .TRUE., 'UPDATE', NDFI, SDIM,
     :                 LBND, UBND, STATUS )

*  Evaluate the dimensions.
      XDIM = UBND( 1 ) - LBND( 1 ) + 1
      YDIM = UBND( 2 ) - LBND( 2 ) + 1

*  Find out whether variances are to be used to define the weights, if
*  the NDF contains any.
      CALL PAR_GET0L( 'VARIANCE', USEVAR, STATUS )

*  Find out if the NDF contains a variance array and/or magic bad
*  values.
      CALL NDF_STATE( NDFI, 'Variance', NDFVAR, STATUS )
      CALL NDF_BAD( NDFI, 'Data,Variance', .FALSE., BAD, STATUS )

*  Weights will be derived from variances only if allowed by USEVAR and
*  if the NDF contains a variance array.
      VARWTS = ( USEVAR .AND. NDFVAR )

*  Get parameters.
*  ===============

*  Obtain the type of the fit ('POLYNOMIAL' or 'SPLINE').
      CALL PAR_CHOIC( 'FITTYPE', 'Polynomial', 'Polynomial,Spline',
     :                .TRUE., FITYPE, STATUS )

*  Constrain the number of interior knots given the number of pixels.
*  This is not perfect, as we really need to know the number of pixels
*  in the bins rather than the minimum number within in a bin.
*  However, the number of knots is needed to constrain the number of
*  pixels...
      IF ( FITYPE( 1:3 ) .EQ. 'SPL' ) THEN
         MINKNO( 1 ) = 0
         MINKNO( 2 ) = 0
         MAXKNO( 1 ) = MIN( XDIM + 1, MXPAR )
         MAXKNO( 2 ) = MIN( YDIM + 1, MXPAR )

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
         MINORD( 1 ) = 1
         MINORD( 2 ) = 1
         MAXORD( 1 ) = MIN( MXPAR, XDIM + 1 )
         MAXORD( 2 ) = MIN( MXPAR, YDIM + 1 )

*  Obtain the number fitting parameters required in the x and y
*  directions and constrain these to be within the above limits.
         CALL PAR_GDR0I( 'NXPAR', 4, MINORD( 1 ), MAXORD( 1 ), .FALSE.,
     :                   NXPAR, STATUS )
         CALL PAR_GDR0I( 'NYPAR', 4, MINORD( 2 ), MAXORD( 2 ), .FALSE.,
     :                   NYPAR, STATUS )

      END IF

*  Find out if an existing surface fit extension can be overwritten.
      CALL PAR_GET0L( 'OVERWRITE', OVERWR, STATUS )

*  If a SURFACEFIT extension exists, then either delete it if OVERWRITE
*  is allowed, or report an error if it is not.
      CALL NDF_XSTAT( NDFI, 'SURFACEFIT', THERE, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

         IF ( THERE ) THEN

            IF ( OVERWR ) THEN
               CALL NDF_XDEL( NDFI, 'SURFACEFIT', STATUS )
               CALL MSG_OUT( ' ', 'Existing SURFACEFIT extension '/
     :           /'deleted.', STATUS )

            ELSE
               STATUS = SAI__ERROR
               CALL NDF_MSG( 'NDF', NDFI )
               CALL ERR_REP( ' ', 'FITSURFACE : SURFACEFIT '/
     :            /'extension already exists in ^NDF.  Specify '/
     :            /'OVERWRITE on the command line to overwrite the '/
     :            /'extension.', STATUS )
            END IF
         END IF
      END IF

*  Obtain the type of co-ordinates to use to fits the surface.
      CALL PAR_CHOIC( 'COSYS', 'World', 'Data,World', .FALSE., COSYS,
     :                STATUS )

*  Check that all the parameters have been obtained and the NDF
*  accessed successfully.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Access the data and co-ordinates.
*  =================================

*  Obtain the processing type.  It is unfortunately controlled by the
*  demands of the fitting rouitnes, not the data.
      IF ( FITYPE( 1:3 ) .EQ. 'POL' ) THEN

*  The PDA library routine for solving the normal equations is only
*  available in double precision (even though the Chebyshev evaluations
*  are generic), so set the processing type to this data type.  This is
*  sensible for reliable results too.
         ITYPE = '_DOUBLE'
      ELSE

*  The PDA library routine for spline surface fitting is only available
*  in single precision, so set the processing type to this data type.
         ITYPE = '_REAL'
      END IF

*  Map the data array of the NDF.
      CALL KPG1_MAP( NDFI, 'Data', ITYPE, 'READ', DPTR, DSIZE, STATUS )

*  Map the variance array if it exists.  An access mode of 'READ/ZERO'
*  ensures that a pointer to a zeroed array is provided if there is no
*  variance array in the NDF.
      CALL KPG1_MAP( NDFI, 'Variance', ITYPE, 'READ/ZERO', VPTR, VSIZE,
     :               STATUS )

*  Map the x and y axes of the NDF (assuming these correspond to the
*  first and second dimensions of the NDF). (N.B. Any variances in the
*  axis co-ordinates are ignored by this routine).
      IF ( COSYS .EQ. 'DATA' ) THEN
         CALL NDF_AMAP( NDFI, 'CENTRE', 1, '_DOUBLE', 'READ',
     :                  AXPTR, EL, STATUS )
         CALL NDF_AMAP( NDFI, 'CENTRE', 2, '_DOUBLE', 'READ',
     :                  AYPTR, EL, STATUS )
      ELSE

*  Get some workspace the length of the two axes.
         CALL PSX_CALLOC( XDIM, '_DOUBLE', AXPTR, STATUS )
         CALL PSX_CALLOC( YDIM, '_DOUBLE', AYPTR, STATUS )

*  Fill the work arrays with pixel co-ordinates.
         CALL KPG1_SSAZD( XDIM, 1.0D0, DBLE( LBND( 1 ) ) - 0.5D0,
     :                    %VAL( CNF_PVAL( AXPTR ) ) , STATUS )
         CALL KPG1_SSAZD( YDIM, 1.0D0, DBLE( LBND( 2 ) ) - 0.5D0,
     :                    %VAL( CNF_PVAL( AYPTR ) ) , STATUS )
      END IF

*  Calculate dimensions of work arrays.
*  ====================================

*  Calculate the storage requirement for the polynomial-fitting routine.
      IF ( FITYPE( 1:3 ) .EQ. 'POL' ) THEN

*  Calculate the number of free fitting parameters.
         MPCOEF = ( MIN( NXPAR, NYPAR ) *
     :            ( MIN( NXPAR, NYPAR ) + 1 ) ) / 2 +
     :            ABS( NXPAR - NYPAR )

      ELSE

*  Calculate the total number of bins and the storage requirement for
*  the spline-fitting routine.  We add a couple of clamps.
         DSIZE = DSIZE + 2

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

         NWS = NXPAR * NYPAR * ( 2 + S1 + S2 ) +
     :         2 * ( NXPAR + NYPAR + 4 * DSIZE +
     :         5 * ( MAX( NXKNOT, NYKNOT ) + 8 ) - 6 ) + S2 + 1
         NIWS = DSIZE + ( NXKNOT + 1 ) * ( NYKNOT + 1 )
         NLWS = NXPAR * NYPAR * ( S2 + 1 ) + S2

      END IF

*  Obtain workspace.
*  =================

*  Map some workspace to hold the x and y co-ordinates, z values and
*  weights.  The maximum number of values which may be required is
*  DSIZE, though the presence of bad values may mean that not all this
*  workspace is needed.
      CALL PSX_CALLOC( DSIZE, ITYPE, XPTR, STATUS )
      CALL PSX_CALLOC( DSIZE, ITYPE, YPTR, STATUS )
      CALL PSX_CALLOC( DSIZE, ITYPE, ZPTR, STATUS )
      CALL PSX_CALLOC( DSIZE, ITYPE, WPTR, STATUS )

*  Map some double-precision workspace to hold the fit and the
*  residuals.
      CALL PSX_CALLOC( DSIZE, ITYPE, FTPTR, STATUS )
      CALL PSX_CALLOC( DSIZE, ITYPE, RSPTR, STATUS )

*  Map some double precision workspace to be used by the surface
*  fitting routines.
      IF ( FITYPE( 1:3 ) .EQ. 'POL' ) THEN

*  Polynomial fit.
*  ---------------

*  Map work arrays (MPCOEF x MPCOEF) in size to hold the normal
*  equation coefficients, the covariance matrix, and a work array used
*  by routine KPS1_FSPF2 below.
         CALL PSX_CALLOC( MPCOEF * MPCOEF, '_DOUBLE', APTR, STATUS )
         CALL PSX_CALLOC( MPCOEF * MPCOEF, '_DOUBLE', CPTR, STATUS )
         CALL PSX_CALLOC( MPCOEF * MPCOEF, '_DOUBLE', MPTR, STATUS )

*  Spline fit.
*  -----------
      ELSE IF ( FITYPE( 1:3 ) .EQ. 'SPL' ) THEN

*  Get some general workspace.
         CALL PSX_CALLOC( DSIZE, ITYPE, GPTR, STATUS )

*  Get workspace for spline evaluation and fitting.
         CALL PSX_CALLOC8( NWS, '_REAL', SWPTR, STATUS )
         CALL PSX_CALLOC8( NIWS, '_INTEGER', SIWPTR, STATUS )

*  Get workspace to allow for rank-deficient system in spline fitting.
         CALL PSX_CALLOC8( NLWS, '_REAL', SLWPTR, STATUS )

      END IF

*  Check everything has been mapped and all the workspace obtained
*  successfully.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'Error while mapping NDF and '/
     :                 /'mapping workspace.', STATUS )
         GOTO 998
      END IF

*  Turn data into vectors.
*  =======================

*  Convert the information contained in the data and axes arrays into a
*  list of co-ordinates, values and weights.
      IF ( ITYPE .EQ. '_REAL' ) THEN

*  The routine KPG1_XYZWR returns double-precision co-ordinates, values,
*  and weights.  Rather than grab a second. potentially large, tranch of
*  workspace and copying to it from the double-precision arrays, we use
*  a modified version of KPG1_XYZWR.  KPS1_XYZWR (notice it is in
*  KAPSUB) has output arrays that have the same data type as the routine
*  type.  Ideally the spline fitting would use double precision too
*  and we could then use the ITYPE for the data mapping not the fitting
*  method.
         CALL KPS1_XYZWR( XDIM, YDIM, %VAL( CNF_PVAL( DPTR ) ),
     :                    %VAL( CNF_PVAL( AXPTR ) ),
     :                    %VAL( CNF_PVAL( AYPTR ) ), BAD, VARWTS,
     :                    %VAL( CNF_PVAL( VPTR ) ), DSIZE,
     :                    %VAL( CNF_PVAL( XPTR ) ),
     :                    %VAL( CNF_PVAL( YPTR ) ),
     :                    %VAL( CNF_PVAL( ZPTR ) ),
     :                    %VAL( CNF_PVAL( WPTR ) ),
     :                    NGOOD, DXMIN, DXMAX, DYMIN, DYMAX, STATUS )

      ELSE
         CALL KPG1_XYZWD( XDIM, YDIM, %VAL( CNF_PVAL( DPTR ) ),
     :                    %VAL( CNF_PVAL( AXPTR ) ),
     :                    %VAL( CNF_PVAL( AYPTR ) ), BAD, VARWTS,
     :                    %VAL( CNF_PVAL( VPTR ) ), DSIZE,
     :                    %VAL( CNF_PVAL( XPTR ) ),
     :                    %VAL( CNF_PVAL( YPTR ) ),
     :                    %VAL( CNF_PVAL( ZPTR ) ),
     :                    %VAL( CNF_PVAL( WPTR ) ),
     :                    NGOOD, DXMIN, DXMAX, DYMIN, DYMAX, STATUS )
      END IF

*  Allow the x and y extrema returned by KPG1_XYZWx to be overridden by
*  parameters. (These parameters will normally have a VPATH of DEFAULT,
*  and DEFAULT = !, so the calculated value will be used unless
*  otherwise specified).
      CALL PAR_GDR0D( 'XMIN', DXMIN, VAL__MIND, XMIN, .TRUE., DXMIN,
     :                STATUS )
      CALL PAR_GDR0D( 'XMAX', DXMAX, XMAX, VAL__MAXD, .TRUE., DXMAX,
     :                STATUS )
      CALL PAR_GDR0D( 'YMIN', DYMIN, VAL__MIND, YMIN, .TRUE., DYMIN,
     :                STATUS )
      CALL PAR_GDR0D( 'YMAX', DYMAX, YMAX, VAL__MAXD, .TRUE., DYMAX,
     :                STATUS )

*  Polynomial fit
*  ==============

*  Check which type of surface fit is required.
      IF ( FITYPE( 1:3 ) .EQ. 'POL' ) THEN

*  Fit a polynomial surface to the data.
         CALL KPS1_FSPF2( DXMIN, DXMAX, DYMIN, DYMAX, NXPAR, NYPAR,
     :                    .FALSE., NGOOD, MPCOEF, DSIZE,
     :                    %VAL( CNF_PVAL( XPTR ) ),
     :                    %VAL( CNF_PVAL( YPTR ) ),
     :                    %VAL( CNF_PVAL( ZPTR ) ),
     :                    %VAL( CNF_PVAL( WPTR ) ),
     :                    %VAL( CNF_PVAL( APTR ) ),
     :                    %VAL( CNF_PVAL( MPTR ) ),
     :                    %VAL( CNF_PVAL( CPTR ) ),
     :                    CHCOEF, VARIAN, NCOEF, STATUS )

*  Evaluate the surface at each pixel and obtain the RMS error and the
*  residuals of the fit.
         CALL KPS1_FSPE2( NGOOD, %VAL( CNF_PVAL( XPTR ) ),
     :                    %VAL( CNF_PVAL( YPTR ) ),
     :                    %VAL( CNF_PVAL( ZPTR ) ), DXMIN, DXMAX, DYMIN,
     :                    DYMAX, NXPAR, NYPAR, MCHOEF, CHCOEF,
     :                    %VAL( CNF_PVAL( FTPTR ) ),
     :                    %VAL( CNF_PVAL( RSPTR ) ), DRMS, STATUS )
         RMS = SNGL( DRMS )

*  Determine the maximum absolute residual.
         CALL KPG1_MXMND( BAD, NGOOD, %VAL( CNF_PVAL( RSPTR ) ), NINVAL,
     :                    MAXMUM, MINMUM, MAXPOS, MINPOS, STATUS )
         RSMAX = MAX( ABS( MAXMUM ), ABS( MINMUM ) )

*  Convert the normalised variances to true variances.
         DO I = 1, NXPAR * NYPAR
            VARIAN( I ) = VARIAN( I ) * DRMS * DRMS
         END DO

*  Spline fitting.
*  ===============
      ELSE IF ( FITYPE( 1:3 ) .EQ. 'SPL' ) THEN

*  Initialise knots.
         DO I = 1, MTKNOT
            XKNOT( I ) = 0.0
            YKNOT( I ) = 0.0
         END DO

*  Spline fitting is limited to single-precision.
         XMIN = SNGL( DXMIN )
         XMAX = SNGL( DXMAX )
         YMIN = SNGL( DYMIN )
         YMAX = SNGL( DYMAX )

*  Fit the bi-cubic-spline surface to the binned array.
         NGOOD8 = NGOOD
         CALL KPS1_SUSF( NXKNOT, NYKNOT, XMIN, XMAX, YMIN, YMAX,
     :                   MTKNOT, .TRUE., NWS, NLWS, NIWS, DSIZE,
     :                   %VAL( CNF_PVAL( XPTR ) ),
     :                   %VAL( CNF_PVAL( YPTR ) ),
     :                   %VAL( CNF_PVAL( ZPTR ) ),
     :                   %VAL( CNF_PVAL( WPTR ) ),
     :                   %VAL( CNF_PVAL( GPTR ) ), NGOOD8,
     :                   XKNOT, YKNOT, %VAL( CNF_PVAL( SWPTR ) ),
     :                   %VAL( CNF_PVAL( SLWPTR ) ),
     :                   %VAL( CNF_PVAL( SIWPTR ) ),
     :                   COEFF, NCOEF, SCALE, STATUS )

*  Evaluate the surface at each pixel and obtain the RMS and the
*  residuals of the fit.  The two additional clamping bins are excluded
*  from the evaluation.
         NGOOD = NGOOD - 2

*  Evaluate the surface at each bin and obtain the rms error of the
*  fit. -1 prevent re-scaling because clipping below needs to work with
*  the scaled fit (if there has been any scaling).
         CALL KPS1_SUSEB( %VAL( CNF_PVAL( XPTR ) ),
     :                    %VAL( CNF_PVAL( YPTR ) ),
     :                    %VAL( CNF_PVAL( ZPTR ) ),
     :                    NGOOD, NXKNOT, NYKNOT, XKNOT, YKNOT, NCOEF,
     :                    COEFF, SCALE, %VAL( CNF_PVAL( FTPTR ) ),
     :                    %VAL( CNF_PVAL( RSPTR ) ), RMS, STATUS )

*  Determine the maximum absolute residual.
         CALL KPG1_MXMNR( BAD, NGOOD, %VAL( CNF_PVAL( RSPTR ) ), NINVAL,
     :                    RMAX, RMIN, MAXPOS, MINPOS, STATUS )
         RSMAX = DBLE( MAX( ABS( RMAX ), ABS( RMIN ) ) )

      ELSE
         STATUS = SAI__ERROR
          CALL MSG_SETC( 'FITTYPE', FITYPE )
          CALL ERR_REP( ' ', 'FITSURFACE: Unknown fit '/
     :                  /'type, ^FITTYPE.', STATUS )
      END IF

*  Report the goodness of the fit.
*  ===============================

*  Report the RMS error of the fit and the maximum residual.
      CALL MSG_SETR( 'RSMAX', SNGL( RSMAX ) )
      CALL MSG_OUTIF( MSG__NORM, ' ', 'The maximum residual of the '/
     :                /'data from the fit is ^RSMAX.', STATUS )
      CALL MSG_SETR( 'RMS', RMS )
      CALL MSG_OUTIF( MSG__NORM, ' ', 'The r.m.s. error is ^RMS.',
     :                STATUS )

*  Store the fit.
*  ==============
      IF ( FITYPE( 1:3 ) .EQ. 'POL' ) THEN

*  If the fit has been successful, write the results to an extension
*  named SURFACEFIT.  The coefficients will be stored in a structure
*  within this called FIT of type POLYNOMIAL (see SGP/38 for a
*  description of the contents of a POLYNOMIAL structure).
         CALL KPS1_FSWPE( NDFI, DXMIN, DXMAX, DYMIN, DYMAX, NXPAR,
     :                    NYPAR, MCHOEF, CHCOEF, VARIAN, SNGL( RSMAX ),
     :                    RMS, COSYS, STATUS )

      ELSE IF ( FITYPE( 1:3 ) .EQ. 'SPL' ) THEN

*  If the fit has been successful, write the results to an extension
*  named SURFACEFIT.  The coefficients will be stored in a structure
*  within this called FIT of type POLYNOMIAL (see SGP/38 for a
*  description of the contents of a POLYNOMIAL structure).
         CALL KPS1_FSWSE( NDFI, NXKNOT, NYKNOT, XKNOT, YKNOT, NCOEF,
     :                    COEFF, SCALE, SNGL( RSMAX ), RMS, 'GRID',
     :                    STATUS )
      END IF

  998 CONTINUE

*  Tidy resources.
*  ===============

*  Tidy up the workspace.
      IF ( FITYPE( 1:3 ) .EQ. 'POL' ) THEN
         CALL PSX_FREE( MPTR, STATUS )
         CALL PSX_FREE( CPTR, STATUS )
         CALL PSX_FREE( APTR, STATUS )

      ELSE IF ( FITYPE( 1:3 ) .EQ. 'SPL' ) THEN
         CALL PSX_FREE( SLWPTR, STATUS )
         CALL PSX_FREE( SIWPTR, STATUS )
         CALL PSX_FREE( SWPTR, STATUS )
         CALL PSX_FREE( GPTR, STATUS )
      END IF

      IF ( COSYS .EQ. 'WORLD' ) THEN
         CALL PSX_FREE( AYPTR, STATUS )
         CALL PSX_FREE( AXPTR, STATUS )
      END IF

      CALL PSX_FREE( WPTR, STATUS )
      CALL PSX_FREE( ZPTR, STATUS )
      CALL PSX_FREE( YPTR, STATUS )
      CALL PSX_FREE( XPTR, STATUS )

  999 CONTINUE

*  Close the NDF context, regardless of the status.
      CALL NDF_END( STATUS )

      END
