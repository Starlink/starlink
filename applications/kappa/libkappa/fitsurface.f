      SUBROUTINE FITSURFACE( STATUS )
*+
*  Name:
*     FITSURFACE

*  Purpose:
*     Fits a polynomial surface to 2-dimensional data array.

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
*     This task fits a surface to a 2-dimensional data array stored
*     array within an NDF data structure.  At present it only
*     permits a fit with a polynomial, and the coefficients of that
*     surface are stored in a POLYNOMIAL structure (SGP/38) as an
*     extension to that NDF.
*
*     Unlike SURFIT, neither does it bin the data nor does it reject
*     outliers.

*  Usage:
*     fitsurface ndf [fittype] nxpar nypar

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
*        polynomial or "Spline" for a bi-cubic spline. ["Polynomial"]
*     NDF  = NDF (Update)
*        The NDF containing the 2-dimensional data array to be fitted.
*     NXPAR = _INTEGER (Read)
*        The number of fitting parameters to be used in the x
*        direction.  It must be in the range 1 to 15 for a polynomial
*        fit, and 4 to 15 for a bi-cubic-spline fit.  Thus 1 gives a
*        constant, 2 a linear fit, 3 a quadratic etc.  Increasing this
*        parameter increases the flexibility of the surface in the x
*        direction.  The upper limit of acceptable values will be
*        reduced for arrays with an x dimension less than 29.
*     NYPAR = _INTEGER (Read)
*        The number of fitting parameters to be used in the y
*        direction.  It must be in the range 1 to 15 for a polynomial
*        fit, and 4 to 15 for a bi-cubic-spline fit.  Thus 1 gives a
*        constant, 2 a linear fit, 3 a quadratic etc.  Increasing this
*        parameter increases the flexibility of the surface in the y
*        direction.  The upper limit of acceptable values will be
*        reduced for arrays with a y dimension less than 29.
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
*        not normally be altered. [Maximum x co-ordinate of the fitted
*        data]
*     XMIN = _DOUBLE (Read)
*        The minimum x value to be used in the fit.  This must be
*        smaller than or equal to the x co-ordinate of the left-hand
*        pixel in the data array.  Normally this parameter is
*        automatically set to the minimum x co-ordinate found in the
*        data, but this mechanism can be overridden by specifying XMIN
*        on the command line.  The parameter is provided to allow the
*        fit limits to be fine tuned for special purposes.  It should
*        not normally be altered.  [Minimum x co-ordinate of the fitted
*        data]
*     YMAX = _DOUBLE (Read)
*        The maximum y value to be used in the fit.  This must be
*        greater than or equal to the y co-ordinate of the top pixel in
*        the data array.  Normally this parameter is automatically set
*        to the maximum y co-ordinate found in the data, but this
*        mechanism can be overridden by specifying YMAX on the command
*        line.  The parameter is provided to allow the fit limits to be
*        fine tuned for special purposes.  It should not normally be
*        altered. [Maximum y co-ordinate of the fitted data]
*     YMIN = _DOUBLE (Read)
*        The minimum y value to be used in the fit.  This must be
*        smaller than or equal to the y co-ordinate of the bottom pixel
*        in the data array.  Normally this parameter is automatically
*        set to the minimum y co-ordinate found in the data, but this
*        mechanism can be overridden by specifying YMIN on the command
*        line.  The parameter is provided to allow the fit limits to be
*        fine tuned for special purposes.  It should not normally be
*        altered. [Minimum y co-ordinate of the fitted data]

*  Examples:
*     fitsurface virgo nxpar=4 nypar=4 novariance
*        This fits a bi-cubic polynomial surface to the data array
*        in the NDF called virgo.  All the data values are given
*        equal weight.  The coefficients of the fitted surface are
*        stored in an extension of virgo.
*     fitsurface virgo nxpar=4 nypar=4
*        As the first example except the data variance, if present,
*        is used to weight the data values.
*     fitsurface mkn231 nxpar=6 nypar=2 cosys=d xmin=-10.0 xmax=8.5
*        This fits a polynomial surface to the data array in the NDF
*        called mkn231.  A fifth order is used along the x direction,
*        but only a linear fit along the y direction.  The fit is made
*        between x data co-ordinates -10.0 to 8.5.  The variance
*        weights the data values.  The coefficients of the fitted
*        surface are stored in an extension of mkn231.

*  Notes:
*     The polynomial surface fit is stored in SURFACEFIT extension,
*     component FIT of type POLYNOMIAL, variant CHEBYSHEV.  This is
*     read by MAKESURFACE to create a NDF of the fitted surface.  Also
*     stored in the SURFACEFIT extension are the r.m.s. deviation to the
*     fit (component RMS), the maximum absolute deviation (component
*     RSMAX), and the co-ordinate system (component COSYS).

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, and HISTORY components of an NDF data structure.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using double-precision floating point.

*  Related Applications:
*     KAPPA: MAKESURFACE, SURFIT.

*  Implementation Deficiencies:
*     A spline-fitting option is not yet available.  One could be
*     implemented using the subroutines called by SURFIT, and a
*     skeleton of the code is included below, commented out.  A
*     standard SPLINE data structure would need to be designed.
*     There is no logfile.  Clipping outliers is not yet supported.

*  Authors:
*     SMB: Steven M. Beard (ROE)
*     MJC: Malcolm J. Currie (STARLINK)
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
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PRM_PAR'          ! Magic-value definitions (VAL__BADx)

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

      INTEGER NDIM               ! Maximum number of dimensions.
      PARAMETER ( NDIM = 2 )     ! Only 2-dimensional arrays can be handled.

*  Local Variables:
      INTEGER APTR               ! Pointer to workspace
      INTEGER AXPTR              ! Pointer to mapped 1st (x) axis array
      INTEGER AYPTR              ! Pointer to mapped 2nd (y) axis array
      LOGICAL BAD                ! Bad pixels may be present?
      DOUBLE PRECISION CHCOEF( MCHOEF ) ! Chebyshev coefficients of fit
      CHARACTER * ( 5 ) COSYS    ! Co-ordinate system
      INTEGER CPTR               ! Pointer to mapped covariance matrix
      INTEGER DPTR               ! Pointer to mapped data array
      DOUBLE PRECISION DRMS      ! R.M.S. deviation of fit
      INTEGER DSIZE              ! Number of elements in data array
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Dummy data type
      INTEGER EL                 ! General "number of elements" variable
      CHARACTER * ( 16 ) FITYPE  ! Type of fit ('POLYNOMIAL'|'SPLINE')
      CHARACTER * ( DAT__SZLOC ) FLOC ! Locator to FIT polynomial
                                 ! structure
      INTEGER FTPTR              ! Pointer to mapped fit array
      INTEGER I                  ! Loop counter
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Data type for processing
      INTEGER LBND( NDIM )       ! Lower bound of data array
      DOUBLE PRECISION MAXMUM    ! Maximum value
      INTEGER MAXPOS             ! Index of maximum array value
      INTEGER MAXTMX             ! Maximum allowed value of NXPAR
      INTEGER MAXTMY             ! Maximum allowed value of NYPAR
      DOUBLE PRECISION MINMUM    ! Minimum value
      INTEGER MINPOS             ! Index of minimum array value
      INTEGER MINTRM             ! Minimum allowed value of NXPAR|NYPAR
      INTEGER MPCOEF             ! Maximum number of polynomial
                                 ! coefficients for chosen NXPAR and
                                 ! NYPAR
      INTEGER MPTR               ! Pointer to SVD V matrix
      INTEGER NCOEF              ! Number of coefficients
      INTEGER NDFI               ! Identifier for NDF
      LOGICAL NDFVAR             ! NDF contains a variance array?
      INTEGER NGOOD              ! Number of good pixels
      INTEGER NINVAL             ! Number of bad values
      INTEGER NXPAR              ! Number of fitting parameters in x
                                 ! direction
      INTEGER NYPAR              ! Number of fitting parameters in y
                                 ! direction
      INTEGER ONXPAR             ! Old value of NXPAR
      INTEGER ONYPAR             ! Old value of NYPAR
      LOGICAL OVERWR             ! Allow surface fits to be overwritten?
      REAL RMS                   ! R.M.S. deviation of fit
      DOUBLE PRECISION RSMAX     ! Maximum residual
      INTEGER RSPTR              ! Pointer to mapped residuals array
      INTEGER SDIM( NDF__MXDIM ) ! Significant NDF dimensions
      LOGICAL THERE              ! SURFACEFIT extension is present?
      INTEGER UBND( NDIM )       ! Upper bound of data array
      LOGICAL USEVAR             ! Allow weights to be derived from the
                                 ! NDF's variance array (if present)
      DOUBLE PRECISION VARIAN( MCHOEF ) ! Variance of Chebyshev coeffs.
      LOGICAL VARWTS             ! Weights are to be derived from
                                 ! variance?
      INTEGER VPTR               ! Pointer to mapped variance array
      INTEGER VSIZE              ! Number of elements in variance array
      DOUBLE PRECISION WORK( MXPAR, MXPAR ) ! Workspace for flipped
                                 ! polynomial coefficients
      INTEGER WPTR               ! Pointer to mapped weight array
      INTEGER XDIM               ! First (x) dimension of data array
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator to SURFACEFIT extension
      DOUBLE PRECISION XMAX      ! Upper x position limit of the fit
      DOUBLE PRECISION XMIN      ! Lower x position limit of the fit
      INTEGER XPTR               ! Pointer to mapped x co-ordinate array
      INTEGER YDIM               ! Second (y) dimension of data array
      DOUBLE PRECISION YMAX      ! Upper y position limit of the fit
      DOUBLE PRECISION YMIN      ! Lower y position limit of the fit
      INTEGER YPTR               ! Pointer to mapped y co-ordinate array
      INTEGER ZPTR               ! Pointer to mapped data value array

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

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

*  Obtain the type of the fit ('POLYNOMIAL' or 'SPLINE').  (For the
*  time being only 'POLYNOMIAL' is implemented).
      CALL PAR_CHOIC( 'FITTYPE', 'Polynomial', 'Polynomial,Spline',
     :                .TRUE., FITYPE, STATUS )

*  Determine the constraints on the number of fitting parameters from
*  the size of the data array and the type of fit.
      IF ( FITYPE( 1:3 ) .EQ. 'SPL' ) THEN
         MINTRM = 4
         MAXTMX = MIN( XDIM + 1, MXPAR )
         MAXTMY = MIN( YDIM + 1, MXPAR )
      ELSE
         MINTRM = 1
         MAXTMX = MIN( XDIM, MXPAR )
         MAXTMY = MIN( YDIM, MXPAR )
      END IF

*  Obtain the number fitting parameters required in the x and y
*  directions and constrain these to be within the above limits.
      CALL PAR_GET0I( 'NXPAR', NXPAR, STATUS )
      CALL PAR_GET0I( 'NYPAR', NYPAR, STATUS )

      IF ( ( NXPAR .LT. MINTRM ) .OR. ( NXPAR .GT. MAXTMX ) ) THEN
         ONXPAR = NXPAR
         NXPAR = MIN( MAX( NXPAR, MINTRM ), MAXTMX )
         CALL MSG_SETI( 'ONXPAR', ONXPAR )
         CALL MSG_SETI( 'NXPAR', NXPAR )
         CALL MSG_OUT( ' ', '*** WARNING: Number of fitting '/
     :     /'parameters in x had to be changed from ^ONXPAR to '/
     :     /'^NXPAR.', STATUS )
      END IF

      IF ( ( NYPAR .LT. MINTRM ) .OR. ( NYPAR .GT. MAXTMY ) ) THEN
         ONYPAR = NYPAR
         NYPAR = MIN( MAX( NYPAR, MINTRM ), MAXTMY )
         CALL MSG_SETI( 'ONYPAR', ONYPAR )
         CALL MSG_SETI( 'NYPAR', NYPAR )
         CALL MSG_OUT( ' ', '*** WARNING: Number of fitting '/
     :     /'parameters in y had to be changed from ^ONYPAR to '/
     :     /'^NYPAR.', STATUS )
      END IF
!
!      CALL MSG_SETI( 'NXPAR', NXPAR )
!      CALL MSG_SETI( 'NYPAR', NYPAR )
!      CALL MSG_OUT( ' ', 'Using ^NXPAR fitting parameters in x '/
!     :  /'and ^NYPAR in Y.', STATUS )

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
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Find the processing data type.
         CALL NDF_MTYPE( '_REAL,_DOUBLE', NDFI, NDFI, 'Data,Variance',
     :                   ITYPE, DTYPE, STATUS )

*  Map the data array of the NDF.
         CALL KPG1_MAP( NDFI, 'Data', ITYPE, 'READ', DPTR, DSIZE,
     :                 STATUS )

*  Map the variance array if it exists.  An access mode of 'READ/ZERO'
*  ensures that a pointer to a zeroed array is provided if there is no
*  variance array in the NDF.
         CALL KPG1_MAP( NDFI, 'Variance', ITYPE, 'READ/ZERO', VPTR,
     :                 VSIZE, STATUS )

*  Map the x and y axes of the NDF (assuming these correspond to the
*  first and second dimensions of the NDF). (N.B. Any variances in the
*  axis co-ordinates are ignored by this routine).
         IF ( COSYS .EQ. 'DATA' ) THEN
            CALL NDF_AMAP( NDFI, 'CENTRE', 1, '_DOUBLE', 'READ',
     :                     AXPTR, EL, STATUS )
            CALL NDF_AMAP( NDFI, 'CENTRE', 2, '_DOUBLE', 'READ',
     :                     AYPTR, EL, STATUS )
         ELSE

*  Get some workspace the length of the two axes.
            CALL PSX_CALLOC( XDIM, '_DOUBLE', AXPTR, STATUS )
            CALL PSX_CALLOC( YDIM, '_DOUBLE', AYPTR, STATUS )

*  Fill the work arrays with pixel co-ordinates.
            CALL KPG1_SSAZD( XDIM, 1.0D0, DBLE( LBND( 1 ) ) - 0.5D0,
     :                       %VAL( AXPTR ) , STATUS )
            CALL KPG1_SSAZD( YDIM, 1.0D0, DBLE( LBND( 2 ) ) - 0.5D0,
     :                       %VAL( AYPTR ) , STATUS )
         END IF

*  Map some DOUBLE PRECISION workspace to hold the x and y
*  co-ordinates, z values and weights.  The maximum number of values
*  which may be required is DSIZE, though the presence of bad values
*  may mean that not all this workspace is needed.
         CALL PSX_CALLOC( DSIZE, '_DOUBLE', XPTR, STATUS )
         CALL PSX_CALLOC( DSIZE, '_DOUBLE', YPTR, STATUS )
         CALL PSX_CALLOC( DSIZE, '_DOUBLE', ZPTR, STATUS )
         CALL PSX_CALLOC( DSIZE, '_DOUBLE', WPTR, STATUS )

*  Map some double-precision workspace to hold the fit and the
*  residuals.
         CALL PSX_CALLOC( DSIZE, '_DOUBLE', FTPTR, STATUS )
         CALL PSX_CALLOC( DSIZE, '_DOUBLE', RSPTR, STATUS )

*  Map some double precision workspace to be used by the surface
*  fitting routines.
         IF ( FITYPE( 1:3 ) .EQ. 'POL' ) THEN

*  Polynomial fit.

*  Calculate the number of free fitting parameters.
            MPCOEF = ( MIN( NXPAR, NYPAR ) *
     :               ( MIN( NXPAR, NYPAR ) + 1 ) ) / 2
     :               + ABS( NXPAR - NYPAR )

*  Map work arrays (MPCOEF x MPCOEF) in size to hold the normal
*  equation coefficients, the covariance matrix, and a work array used
*  by routine KPS1_FSPF2 below.
            CALL PSX_CALLOC( MPCOEF * MPCOEF, '_DOUBLE', APTR, STATUS )
            CALL PSX_CALLOC( MPCOEF * MPCOEF, '_DOUBLE', CPTR, STATUS )
            CALL PSX_CALLOC( MPCOEF * MPCOEF, '_DOUBLE', MPTR, STATUS )
          
         ELSE IF ( FITYPE(1:3) .EQ. 'SPL' ) THEN

*  Spline fit. Not implemented yet, so do nothing.
*            EVENTUALLY, OBTAIN WORKSPACE FOR SPLINE FITTING HERE.

         END IF

*  Check everything has been mapped and all the workspace obtained
*  successfully.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Convert the information contained in the data and axes arrays into a
*  list of co-ordinates, values and weights.
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPG1_XYZWR( XDIM, YDIM, %VAL( DPTR ), %VAL( AXPTR ),
     :                          %VAL( AYPTR ), BAD, VARWTS,
     :                          %VAL( VPTR ), DSIZE, %VAL( XPTR ),
     :                          %VAL( YPTR ), %VAL( ZPTR ),
     :                          %VAL( WPTR ), NGOOD, XMIN, XMAX, YMIN,
     :                          YMAX, STATUS )

            ELSE
               CALL KPG1_XYZWD( XDIM, YDIM, %VAL( DPTR ), %VAL( AXPTR ),
     :                          %VAL( AYPTR ), BAD, VARWTS,
     :                          %VAL( VPTR ), DSIZE, %VAL( XPTR ),
     :                          %VAL( YPTR ), %VAL( ZPTR ),
     :                          %VAL( WPTR ), NGOOD, XMIN, XMAX, YMIN,
     :                          YMAX, STATUS )
            END IF

*  Allow the x and y extrema returned by KPG1_XYZWx to be overridden by
*  parameters. (These parameters will normally have a VPATH of DYNAMIC,
*  so the calculated value will be used unless otherwise specified).
            CALL PAR_DEF0D( 'XMIN', XMIN, STATUS )
            CALL PAR_DEF0D( 'XMAX', XMAX, STATUS )
            CALL PAR_DEF0D( 'YMIN', YMIN, STATUS )
            CALL PAR_DEF0D( 'YMAX', YMAX, STATUS )

            CALL PAR_GET0D( 'XMIN', XMIN, STATUS )
            CALL PAR_GET0D( 'XMAX', XMAX, STATUS )
            CALL PAR_GET0D( 'YMIN', YMIN, STATUS )
            CALL PAR_GET0D( 'YMAX', YMAX, STATUS )

*  Report if something has gone wrong while obtaining the parameters.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( ' ', 'Error while obtaining %XMIN, '/
     :           /'%XMAX, %YMIN and %YMAX parameters.', STATUS )
            END IF

*  Check which type of surface fit is required.
            IF ( FITYPE( 1:3 ) .EQ. 'POL' ) THEN

*  Fit a polynomial surface to the data.
               CALL KPS1_FSPF2( XMIN, XMAX, YMIN, YMAX, NXPAR, NYPAR,
     :                         .FALSE., NGOOD, MPCOEF, DSIZE,
     :                         %VAL( XPTR ), %VAL( YPTR ), %VAL( ZPTR ),
     :                         %VAL( WPTR ), %VAL( APTR ), %VAL( MPTR ),
     :                         %VAL( CPTR ), CHCOEF, VARIAN, NCOEF,
     :                         STATUS )

*  Evaluate the surface at each bin and obtain the RMS error and the
*  residuals of the fit.
               CALL KPS1_FSPE2( NGOOD, %VAL( XPTR ), %VAL( YPTR ),
     :                          %VAL( ZPTR ), XMIN, XMAX, YMIN, YMAX,
     :                          NXPAR, NYPAR, MCHOEF, CHCOEF, NCOEF,
     :                          %VAL( FTPTR ), %VAL( RSPTR ), DRMS,
     :                          STATUS )

*  Determine the maximum absolute residual.
               CALL KPG1_MXMND( BAD, NGOOD, %VAL( RSPTR ), NINVAL,
     :                          MAXMUM, MINMUM, MAXPOS, MINPOS, STATUS )
               RSMAX = MAX( ABS( MAXMUM ), ABS( MINMUM ) )
               RMS = SNGL( DRMS )

*  Report the RMS error of the fit and the maximum residual.
               CALL MSG_SETR( 'RSMAX', SNGL( RSMAX ) )
               CALL MSG_SETR( 'RMS', RMS )
               CALL MSG_OUTIF( MSG__NORM, ' ', 'The maximum residual '/
     :           /'of the binned data from the fit is ^RSMAX, and '/
     :           /'the r.m.s. error is ^RMS.', STATUS )

*  Convert the normalised variances to true variances.
               DO I = 1, NXPAR * NYPAR
                  VARIAN( I ) = VARIAN( I ) * DRMS * DRMS
               END DO

*  If the fit has been successful, write the results to an extension
*  named SURFACEFIT.  The coefficients will be stored in a structure
*  within this called FIT of type POLYNOMIAL (see SGP/38 for a
*  description of the contents of a POLYNOMIAL structure).
               CALL NDF_XNEW( NDFI, 'SURFACEFIT', 'EXT',
     :                        0, 0, XLOC, STATUS )

               CALL DAT_NEW( XLOC, 'FIT', 'POLYNOMIAL', 0, 0, STATUS )
               CALL DAT_FIND( XLOC, 'FIT', FLOC, STATUS )

               CALL KPG1_PL2PU( FLOC, 'CHEBYSHEV', NXPAR, NYPAR, .TRUE.,
     :                          XMIN, XMAX, YMIN, YMAX, CHCOEF, VARIAN,
     :                          WORK, STATUS )

*  In addition to the coefficients, write the RMS error, the maximum
*  residual to the SURFACEFIT extension, and the co-ordinate system.
               CALL NDF_XPT0R( RMS, NDFI, 'SURFACEFIT', 'RMS', STATUS )
               CALL NDF_XPT0R( SNGL( RSMAX ), NDFI, 'SURFACEFIT',
     :                         'RESIDMAX', STATUS )
               CALL NDF_XPT0C( COSYS, NDFI, 'SURFACEFIT', 'COSYS', 
     :                         STATUS )

            ELSE IF ( FITYPE( 1:3 ) .EQ. 'SPL' ) THEN
               CALL MSG_OUT( ' ', 'Spline fitting not '/
     :                       /'implemented yet.', STATUS )

*  EVENTUALLY FIT A SPLINE AND WRITE THE COEFFICIENTS TO THE SURFACEFIT
*  EXTENSION.  THE COMMENTED OUT CODE SHOWS HOW THIS WILL BE DONE.

!*  Fit a spline surface to the data.
!               CALL SPL2D( NXKNOT, NYKNOT, XMIN, XMAX, YMIN, YMAX,
!     :              .FALSE., NPOINT, NWS, DSIZE, %VAL(XPTR), %VAL(YPTR),
!     :              %VAL(ZPTR), %VAL(GPTR), NGOOD, %VAL(PANPTR), XKNOT,
!     :              YKNOT, %VAL(SWPTR), COEFF, NCOEF, SCALE, STATUS )
!
!*  Evaluate the surface at each bin and obtain the RMS
!*  and the residuals of the fit. The two additional
!*  clamping bins are excluded from the evaluation, hence
!*  NELM being 2 less than NGOOD.
!                NELM = NGOOD - 2
!               CALL SPL2EB( %VAL(XPTR), %VAL(YPTR), %VAL(ZPTR),
!     :              NELM, NXKNOT, NYKNOT, XKNOT, YKNOT, COEFF, NCOEF,
!     :              SCALE, NPOINT, %VAL(PANPTR), %VAL(FTPTR), %VAL(RSPTR),
!     :              RMS, STATUS )
!
!*  Determine the maximum residual.
!               CALL AMAX1D( NELM, %VAL(RSPTR), BAD, DRSMAX, STATUS )
!               RSMAX = REAL( DRSMAX )
!
!*  Report the RMS error of the fit and the maximum residual.
!               CALL MSG_SETR( 'RSMAX', RSMAX )
!               CALL MSG_OUT( ' ', 'The maximum residual of the '/
!     :              /'binned data from the fit is ^RSMAX,', STATUS )
!               CALL MSG_SETR( 'RMS', RMS )
!               CALL MSG_OUT( ' ', 'and the r.m.s. error is ^RMS.',
!     :                       STATUS )
!
!*  If the fit has been successful, write the results to an
!*  extension named SURFACEFIT. The coefficients will be stored
!*  in a structure within this called FIT of type SPLINE
!*  (not yet defined in SGP/38).
!               CALL NDF_XNEW( NDFI, 'SURFACEFIT', 'EXT',
!     :                        0, 0, XLOC, STATUS )
!
!               CALL DAT_NEW( XLOC, 'FIT', 'SPLINE', 0, 0, STATUS )
!               CALL DAT_FIND( XLOC, 'FIT', FLOC, STATUS )
!
!               CALL SPLPUT2D( FLOC, 'B-SPLINE', NXPAR, NYPAR,
!     :              NXKNOT, NYKNOT, XKNOT, YKNOT, COEFF, NCOEF,
!     :              etc... etc..., STATUS )
!
!*  In addition to the coefficients, write the RMS error, the maximum
!*  residual to the SURFACEFIT extension, and the co-ordinate system.
!               CALL NDF_XPT0R( RMS, NDFI, 'SURFACEFIT', 'RMS', STATUS )
!               CALL NDF_XPT0R( RSMAX, NDFI, 'SURFACEFIT',
!     :                         'RESIDMAX', STATUS )
!               CALL NDF_XPT0C( COSYS, NDFI, 'SURFACEFIT', 'COSYS', 
!     :                         STATUS )

            ELSE
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'FITTYPE', FITYPE )
               CALL ERR_REP( ' ', 'FITSURFACE: Unknown fit '/
     :           /'type, ^FITTYPE.', STATUS )
            END IF

         ELSE
            CALL ERR_REP( ' ', 'Error while mapping NDF and '/
     :                    /'mapping workspace.', STATUS )
         END IF

*  Tidy up the workspace.
         CALL PSX_FREE( XPTR, STATUS )
         CALL PSX_FREE( YPTR, STATUS )
         CALL PSX_FREE( ZPTR, STATUS )
         CALL PSX_FREE( WPTR, STATUS )
         CALL PSX_FREE( APTR, STATUS )

         IF ( COSYS .EQ. 'WORLD' ) THEN
            CALL PSX_FREE( AXPTR, STATUS )
            CALL PSX_FREE( AYPTR, STATUS )
         END IF

      END IF

*  Close the NDF context, regardless of the status.
      CALL NDF_END( STATUS )

      END
