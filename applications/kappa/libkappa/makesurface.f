      SUBROUTINE MAKESURFACE( STATUS )
*+
*  Name:
*     MAKESURFACE

*  Purpose:
*     Creates a 2-dimensional NDF from the coefficients of a polynomial
*     surface

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MAKESURFACE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The coefficients describing a 2-dimensional polynomial surface
*     are read from a SURFACEFIT extension in an NDF (written by
*     FITSURFACE), and are used to create a 2-dimensional surface of
*     specified size and extent.  The surface is written to a new NDF.

*     The size and extent of the surface may be obtained from a template
*     NDF or given explicitly.

*     Elements in the new NDF outside the defined range of the
*     polynomial will be set to bad values.

*  Usage:
*     makesurface in out [like] type=? lbound=? ubound=? xlimit=?
*        ylimit=?

*  ADAM Parameters:
*     IN  = NDF (Read)
*        The NDF containing the SURFACEFIT extension.
*     LBOUND( 2 ) = _INTEGER (Read)
*        Lower bounds of new NDF (if LIKE=!).  The suggested defaults
*        are the lower bounds of the IN NDF.
*     LIKE = NDF (Read)
*        An optional template NDF which, if specified, will be used to
*        define the labels, size, shape, data type and axis range of
*        the new NDF.  If a null response (!) is given, the label,
*        units, axis labels, and axis units are taken from the IN NDF.
*        The task prompts for the data type and bounds, using those of
*        the IN NDF as defaults, and the axis ranges. [!]
*     OUT = NDF (Write)
*        The new NDF to contain the surface fit.
*     TITLE = LITERAL (Read)
*        A title for the new NDF.  If a null response (!) is given,
*        the title will be propagated either from LIKE, or from IN
*        if LIKE=!. [!]
*     TYPE = LITERAL (Read)
*        Data type for the new NDF (if LIKE=!).  It must be one of
*        the following: "_DOUBLE", "_REAL", "_INTEGER", "_WORD",
*        "_BYTE", "_UBYTE".  The suggested default is the data type of
*        the data array in the IN NDF.
*     UBOUND( 2 ) = _INTEGER (Read)
*        Upper bounds of new NDF (if LIKE=!).  The suggested defaults
*        are the upper bounds of the IN NDF.
*     VARIANCE = _LOGICAL (Read)
*        If TRUE, a variance array is created in the output NDF
*        provided the SURFACEFIT.FIT structure contains variance
*        information.
*     XLIMIT( 2 ) = _DOUBLE (Read)
*        Co-ordinates of the left then right edges of the x axis (if
*        LIKE=!).  The suggested defaults are respectively the minimum
*        and maximum x co-ordinates of the IN NDF.
*     YLIMIT( 2 ) = _DOUBLE (Read)
*        Co-ordinates of the bottom then top edges of the y axis (if
*        LIKE=!).  The suggested defaults are respectively the minimum
*        and maximum y co-ordinates of the IN NDF.

*  Examples:
*     makesurface flatin flatout \
*        This generates a 2-dimensional image in the NDF called flatout
*        using the surface fit stored in the 2-dimensional NDF flatin.
*        The created image has the same data type, bounds, and
*        co-ordinate limits as the data array of flatin.
*     makesurface flatin flatout type=_wo lbound=[1,1] ubound=[320,512]
*        As the previous example, except that the data array in flatout
*        has data type _WORD, and the bounds of flatout are 1:320,
*        1:512.
*     makesurface flatin flatout like=flatin
*        This has the same effect as the first example, except it has
*        an advantage.  If the current co-ordinate system is "Data" and
*        either or both of the axes are inverted (values decrease with
*        increasing pixel index), the output image will be correctly
*        oriented.
*     makesurface flatin flatout template title="Surface fit"
*        This generates a 2-dimensional image in the NDF called flatout
*        using the surface fit stored in the 2-dimensional NDF flatin.
*        The created image inherits the attributes of the NDF called
*        template.  The title of flatout is "Surface fit".

*  Notes:
*     -  The polynomial surface fit is stored in SURFACEFIT extension,
*     component FIT of type POLYNOMIAL, variant CHEBYSHEV.  This
*     extension is created by FITSURFACE.    Also read from the
*     SURFACEFIT extension is the co-ordinate system (component COSYS).
*     -  When LIKE=!, COSYS="Data" and the original NDF had an axis that
*     decreased with increasing pixel index, you may want to flip the
*     co-ordinate limits (via parameters XLIMIT or YLIMIT) to match
*     the original sense of the axis, otherwise the created surface will
*     be flipped with respect to the image from which it was fitted.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of an NDF
*     data structure and propagates all extensions.  However, neither
*     QUALITY nor a SURFACEFIT extension is propagated when LIKE is not
*     null.
*     -  All non-complex numeric data types can be handled.  Processing
*     is performed in single- or double-precision floating point, as
*     appropriate.

*  Related Applications:
*     KAPPA: FITSURFACE, SURFIT.

*  Implementation Deficiencies:
*     The ability to generate a spline surface is not yet available.
*     One could be implemented using the subroutines called by SURFIT,
*     but standard SPLINE data structure would need to be designed
*     first.

*  Authors:
*     SMB: Steven M. Beard (ROE)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-May-1993 (SMB):
*        Original version.
*     03-Jun-1993 (SMB):
*        Modified to read the new data structure produced by FITSURFACE.
*     07-Dec-1993 (SMB):
*        Comments tidied up.
*     1995 August 8 (MJC):
*        Used a modern prologue and completed it.  Corrected the
*        description of LIKE and some data types.  Merged XMIN and XMAX
*        into parameter XLIMIT, and YMIN and YMAX into YLIMIT.  Renamed
*        many of the routines and called existing subroutines rather
*        than use SMB's new ones.  Read COSYS component from the
*        FITSURFACE.  Restructured for clarity and efficiency.  Removed
*        PAR_GETs for labels and units when LIKE=!.  Insisted on two
*        significant dimensions in IN and LIKE.
*     1997 May 10 (MJC):
*        Added support for creating a variance array.  Obtained
*        workspace by PSX for efficiency.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     23-JUN-1998 (DSB):
*        Used KPG1_MAP instead of NDF_MAP, so that NaN and Inf values
*        are converted to Starlink BAD values before being used.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PAR_ERR'          ! PAR__ error codes
      INCLUDE 'PRM_PAR'          ! VAL__ definitions

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXDIM             ! Maximum number of dimensions.
      PARAMETER ( MAXDIM = 2 )   ! Only 2-dimensional arrays can be
                                 ! handled

      INTEGER MXPAR              ! Maximum number of fit parameters
      PARAMETER ( MXPAR = 15 )

      INTEGER MCHOEF             ! Maximum number of Chebyshev
                                 ! polynomial coefficients
      PARAMETER ( MCHOEF = MXPAR * MXPAR )

*  Local Variables:
      DOUBLE PRECISION CHCOEF( MCHOEF ) ! Chebyshev polynomial coeffs
      CHARACTER * ( 5 ) COSYS    ! Co-ordinate system
      LOGICAL CREVAR             ! Create variance array?
      INTEGER DPTR( 2 )          ! Pointer to data and variance arrays
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Destination data type
      INTEGER EL                 ! Number of elements
      CHARACTER * ( DAT__SZLOC ) FLOC ! Locator to FIT structure
                                 ! within SURFACEFIT
      CHARACTER * ( DAT__SZTYP ) FTYPE ! Type of FIT structure
      INTEGER I                  ! Loop counter
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Implementation data type
      CHARACTER * ( DAT__SZTYP ) INTYPE ! Data type of NDF containing coefficients
      CHARACTER * ( 40 ) LABEL   ! Data label of NDF
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER LIKEID             ! ID of template NDF
      LOGICAL LIMITS             ! PXMIN, PXMAX, PYMIN and PYMAX are
                                 ! valid?
      INTEGER NCOEF              ! Number of Chebyshev coefficients used
      INTEGER NDFI               ! ID of NDF containing coefficients
      INTEGER NDFO               ! ID of new NDF to contain surface
      INTEGER NDFW               ! ID of temporary workspace NDF
      INTEGER NDIM               ! Number of dimensions
      INTEGER NXPAR              ! Number of fitting parameters in x
                                 ! direction
      INTEGER NYPAR              ! Number of fitting parameters in y
                                 ! direction
      LOGICAL OBWORK             ! Workspace obtained?
      DOUBLE PRECISION PXMAX     ! Maximum x for which fitted surface
                                 ! is valid
      DOUBLE PRECISION PXMIN     ! Minimum x for which fitted surface
                                 ! is valid
      DOUBLE PRECISION PYMAX     ! Maximum y for which fitted surface
                                 ! is valid
      DOUBLE PRECISION PYMIN     ! Minimum y for which fitted surface
                                 ! is valid
      DOUBLE PRECISION ROUND( 2 )! Rounding needed to compare
                                 ! co-ordinate limits as SUBPAR loses
                                 ! accuracy
      INTEGER SDIM( MAXDIM )     ! Indices of significant dimensions
      LOGICAL STATE              ! NDF component is defined?
      CHARACTER * ( 80 ) TITLE   ! Title of NDF
      CHARACTER * ( DAT__SZTYP ) TYPE ! Data type of new NDF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF
      CHARACTER * ( 40 ) UNITS   ! Data units of NDF
      DOUBLE PRECISION VARIAN( MCHOEF ) ! Chebyshev coeff's variances
      CHARACTER * ( DAT__SZTYP ) VARNT ! Variant of FIT structure
      LOGICAL VARPRE             ! Chebyshev coeff's variance present?
      DOUBLE PRECISION WORK( MXPAR, MXPAR ) ! Work array used when
                                 ! reading coefficients
      INTEGER WPTR               ! Pointer to temporary workspace
      INTEGER XDIM               ! First (x) dimension of data array
      CHARACTER * ( 40 ) XLABEL  ! X-axis label of NDF
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator to SURFACEFIT extension
      DOUBLE PRECISION XLIMIT( MAXDIM ) ! Co-ordinate range in x for
                                 ! NDF to be created
      DOUBLE PRECISION XMAX      ! Maximum x for NDF to be created
      DOUBLE PRECISION XMIN      ! Minimum x for NDF to be created
      INTEGER XPTR               ! Pointer to x-axis array
      LOGICAL XROUND( 2 )        ! Supplied x limits out of bounds?
      CHARACTER * ( 40 ) XUNITS  ! X-axis units of NDF
      INTEGER YDIM               ! Second (y) dimension of data array
      CHARACTER * ( 40 ) YLABEL  ! Y-axis label of NDF
      DOUBLE PRECISION YLIMIT( MAXDIM ) ! Co-ordinate range in y for
                                 ! NDF to be created
      DOUBLE PRECISION YMAX      ! Maximum y for NDF to be created
      DOUBLE PRECISION YMIN      ! Minimum y for NDF to be created
      INTEGER YPTR               ! Pointer to y-axis array
      LOGICAL YROUND( 2 )        ! Supplied y limits out of bounds
      CHARACTER * ( 40 ) YUNITS  ! Y-axis units of NDF

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Specifiy the rounding for fuzzy limits.
      ROUND( 1 ) = 1.0D0 - VAL__EPSD * 1.0D6
      ROUND( 2 ) = 1.0D0 + VAL__EPSD * 1.0D6

*  Begin the NDF context.
      CALL NDF_BEGIN

*  Open the NDF containing the POLYNOMIAL (or SPLINE) structure.  There
*  must be only two significant dimensions.  Obtain the bounds of the
*  NDF (to be used as dynamic default values later).
      CALL KPG1_GTNDF( 'IN', MAXDIM, .TRUE., 'READ', NDFI, SDIM,
     :                 LBND, UBND, STATUS )

*  The NDF should have an extension called SURFACEFIT containing a
*  structure called FIT, which should contain the polynomial or spline
*  coefficients.  Find a locator to this structure and determine its
*  type.  An error will be generated if the structure does not exist or
*  cannot be accessed.
      CALL NDF_XLOC( NDFI, 'SURFACEFIT', 'READ', XLOC, STATUS )
      CALL DAT_FIND( XLOC, 'FIT', FLOC, STATUS )
      CALL DAT_TYPE( FLOC, FTYPE, STATUS )

      OBWORK = .FALSE.

*  Initialise the Chebyshev coefficients.
      DO I = 1, MCHOEF
         CHCOEF( I ) = 0.0D0
      END DO

*  Check everything has worked so far.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the type of the FIT structure is recognised.  It should
*  either be POLYNOMIAL or SPLINE.
         IF ( FTYPE .EQ. 'POLYNOMIAL' ) THEN

*  Read the polynomial structure to obtain the coefficients and
*  variances, and the x-y limits, and check this has worked.
            CALL KPG1_PL2GE( FLOC, MXPAR, VARNT, NXPAR, NYPAR, LIMITS,
     :                       PXMIN, PXMAX, PYMIN, PYMAX, CHCOEF,
     :                       VARPRE, VARIAN, WORK, STATUS )

*  Determine whether or not a variance array is to be created.  It isn't
*  when the polynomial structure does not contain a VARIANCE component.
            IF ( VARPRE ) THEN
               CALL PAR_GET0L( 'VARIANCE', CREVAR, STATUS )
            ELSE
               CREVAR = .FALSE.
            END IF

*  Obtain the co-ordinate system.
            CALL NDF_XGT0C( NDFI, 'SURFACEFIT', 'COSYS', COSYS, STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN

*  Determine the number of non-zero coefficients actually used.
               NCOEF = 0
               DO I = 1, MCHOEF

                  IF ( ABS( CHCOEF( I ) ) .GT. VAL__SMLD ) THEN
                     NCOEF = NCOEF + 1
                  END IF
               END DO

*  Check that the type of variant can be dealt with.
               IF ( VARNT .EQ. 'CHEBYSHEV' ) THEN

*  Attempt to open a template NDF, which the output NDF will be based
*  on.  This may be the same as the input NDF.  If a NULL response is
*  given, the attributes will be obtained from parameters.  The
*  template NDF must only have two significant dimensions.  (Note that
*  a mark is set in the error stack, which is later released with
*  ERR_RLSE.  This allows errors encountered after the mark to be
*  annulled without affecting earlier ones.
                  CALL ERR_MARK
                  CALL KPG1_GTNDF( 'LIKE', MAXDIM, .TRUE., 'READ',
     :                             LIKEID, SDIM, LBND, UBND, STATUS )

                  IF ( STATUS .EQ. SAI__OK ) THEN

*  Remove the mark set in the error stack above.
                     CALL ERR_RLSE

*  For ease of use just have bounds with elements one and two.
                     LBND( 1 ) = LBND( SDIM( 1 ) )
                     LBND( 2 ) = LBND( SDIM( 2 ) )
                     UBND( 1 ) = UBND( SDIM( 1 ) )
                     UBND( 2 ) = UBND( SDIM( 2 ) )

*  Create a new NDF using the supplied template, propagating the data
*  and axis arrays, the title, label and history but NOT the SURFACEFIT
*  extension.
                     CALL NDF_PROP( LIKEID, 'DATA,UNITS,AXIS,TITLE,'/
     :                              /'LABEL,HISTORY,WCS,'/
     :                              /'NOEXTENSION(SURFACEFIT)',
     :                              'OUT', NDFO, STATUS )

*  Obtain the bounds of the NDF just created.
                     CALL NDF_BOUND( NDFO, MAXDIM, LBND, UBND,
     :                               NDIM, STATUS )

*  For ease of use just have bounds with elements one and two.
                     LBND( 1 ) = LBND( SDIM( 1 ) )
                     LBND( 2 ) = LBND( SDIM( 2 ) )
                     UBND( 1 ) = UBND( SDIM( 1 ) )
                     UBND( 2 ) = UBND( SDIM( 2 ) )

*  Evaluate the dimensions.
                     XDIM = UBND( 1 ) - LBND( 1 ) + 1
                     YDIM = UBND( 2 ) - LBND( 2 ) + 1

*  Is the surface is defined in terms of data co-ordinates?
                     IF ( COSYS .EQ. 'DATA' ) THEN

*  Map the axis arrays of the new NDF for read access.
                        CALL NDF_AMAP( NDFO, 'CENTRE', SDIM( 1 ),
     :                                 '_DOUBLE', 'READ', XPTR, EL,
     :                                 STATUS )
                        CALL NDF_AMAP( NDFO, 'CENTRE', SDIM( 2 ),
     :                                 '_DOUBLE', 'READ', YPTR, EL,
     :                                 STATUS )

*  Determine the bounds of the axes of the template NDF.
                        CALL KPG1_AXBND( XDIM, %VAL( XPTR ), XMIN, XMAX,
     :                                   STATUS )
                        CALL KPG1_AXBND( YDIM, %val( YPTR ), YMIN, YMAX,
     :                                   STATUS )

                     ELSE

*  Get some workspace the length of the two axes.
                        CALL PSX_CALLOC( XDIM, '_DOUBLE', XPTR, STATUS )
                        CALL PSX_CALLOC( YDIM, '_DOUBLE', YPTR, STATUS )

*  Fill the work arrays with pixel co-ordinates.
                        CALL KPG1_SSAZD( XDIM, 1.0D0,
     :                                   DBLE( LBND( 1 ) ) - 0.5D0,
     :                                   %VAL( XPTR ) , STATUS )
                        CALL KPG1_SSAZD( YDIM, 1.0D0,
     :                                   DBLE( LBND( 2 ) ) - 0.5D0,
     :                                   %VAL( YPTR ) , STATUS )

*  Record that there is workspace to free.
                        OBWORK = .TRUE.

*  Obtain the limits of the axes.
                        XMIN = DBLE( LBND( 1 ) ) - 0.5D0
                        YMIN = DBLE( LBND( 2 ) ) - 0.5D0
                        XMAX = DBLE( UBND( 1 ) ) - 0.5D0
                        YMAX = DBLE( UBND( 2 ) ) - 0.5D0
                     END IF

*  LIKE=!
                  ELSE IF ( STATUS .EQ. PAR__NULL ) THEN

*  Annul the PAR__NULL error. (Note that ERR_ANNUL will only annul
*  errors back to the last ERR_MARK).
                     CALL ERR_ANNUL( STATUS )
                     CALL ERR_RLSE

*  Obtain the data type of the output NDF, using the type of the input
*  NDF as a dynamic default.  Only allow the primitive numeric types.
                     CALL NDF_TYPE( NDFI, 'DATA', INTYPE, STATUS )
                     CALL PAR_CHOIC( 'TYPE', INTYPE, '_DOUBLE,_REAL,'/
     :                               /'_INTEGER,_WORD,_BYTE,_UBYTE',
     :                               .TRUE., TYPE, STATUS )

*  Obtain the required type and bounds for the new NDF, using the
*  attributes of the input NDF as a default.
                     CALL PAR_DEF1I( 'LBOUND', MAXDIM, LBND, STATUS )
                     CALL PAR_GET1I( 'LBOUND', MAXDIM, LBND, NDIM,
     :                               STATUS )

                     CALL PAR_DEF1I( 'UBOUND', MAXDIM, UBND, STATUS )
                     CALL PAR_GET1I( 'UBOUND', MAXDIM, UBND, NDIM,
     :                               STATUS )

                     XDIM = UBND( 1 ) - LBND( 1 ) + 1
                     YDIM = UBND( 2 ) - LBND( 2 ) + 1

*  Create a new NDF of the right type and size from scratch.
                     CALL NDF_CREAT( 'OUT', TYPE, MAXDIM, LBND, UBND,
     :                               NDFO, STATUS )

*  Initialise the axis system.
                     CALL NDF_ACRE( NDFO, STATUS )

*  Obtain the title of the output NDF, using the title of the input
*  NDF as the default, where there is one, if the user supplies a null
*  response.
                     CALL NDF_STATE( NDFI, 'TITLE', STATE, STATUS )
                     IF ( STATE ) THEN
                        CALL NDF_CGET( NDFI, 'TITLE', TITLE, STATUS )
                        CALL NDF_CPUT( TITLE, NDFO, 'TITLE', STATUS )
                     END IF
                     CALL NDF_CINP( 'TITLE', NDFO, 'TITLE', STATUS )

*  Propagate the label of the input NDF, if there is one present, to
*  the output NDF.
                     CALL NDF_STATE( NDFI, 'LABEL', STATE, STATUS )
                     IF ( STATE ) THEN
                        CALL NDF_CGET( NDFI, 'LABEL', LABEL, STATUS )
                        CALL NDF_CPUT( LABEL, NDFO, 'LABEL', STATUS )
                     END IF

*  Propagate the label of the input NDF, if there is one present, to
*  the output NDF.
                     CALL NDF_STATE( NDFI, 'UNITS', STATE, STATUS )
                     IF ( STATE ) THEN
                        CALL NDF_CGET( NDFI, 'UNITS', UNITS, STATUS )
                        CALL NDF_CPUT( UNITS, NDFO, 'UNITS', STATUS )
                     END IF

*  Propagate the x-axis label of the input NDF, if there is one
*  present, to the output NDF.
                     CALL NDF_ASTAT( NDFI, 'LABEL', SDIM( 1 ), STATE,
     :                               STATUS )
                     IF ( STATE ) THEN
                        CALL NDF_ACGET( NDFI, 'LABEL', SDIM( 1 ),
     :                                  XLABEL, STATUS )
                        CALL NDF_ACPUT( XLABEL, NDFO, 'LABEL',
     :                                  SDIM( 1 ), STATUS )
                     END IF

*  Propagate the y-axis label of the input NDF, if there is one
*  present, to the output NDF.
                     CALL NDF_ASTAT( NDFI, 'LABEL', SDIM( 2 ), STATE,
     :                               STATUS )
                     IF ( STATE ) THEN
                        CALL NDF_ACGET( NDFI, 'LABEL', SDIM( 2 ),
     :                                  YLABEL, STATUS )
                        CALL NDF_ACPUT( YLABEL, NDFO, 'LABEL',
     :                                  SDIM( 2 ), STATUS )
                     END IF

*  Propagate the x-axis units of the input NDF, if there is one
*  present, to the output NDF.
                     CALL NDF_ASTAT( NDFI, 'UNITS', SDIM( 1 ), STATE,
     :                               STATUS )
                     IF ( STATE ) THEN
                        CALL NDF_ACGET( NDFI, 'UNITS', SDIM( 1 ),
     :                                  XUNITS, STATUS )
                        CALL NDF_ACPUT( XUNITS, NDFO, 'UNITS',
     :                                  SDIM( 1 ), STATUS )
                     END IF

*  Propagate the y-axis label of the input NDF, if there is one
*  present, to the output NDF.
                     CALL NDF_ASTAT( NDFI, 'UNITS', SDIM( 2 ), STATE,
     :                               STATUS )
                     IF ( STATE ) THEN
                        CALL NDF_ACGET( NDFI, 'UNITS', SDIM( 2 ),
     :                                  YUNITS, STATUS )
                        CALL NDF_ACPUT( YUNITS, NDFO, 'UNITS',
     :                                  SDIM( 2 ), STATUS )
                     END IF

*  Obtain the required co-ordinate range for this polynomial, using the
*  full range of the polynomial as a default.  The NDF will be filled
*  with bad values outside the range of the polynomial.
                     XLIMIT( 1 ) = PXMIN
                     XLIMIT( 2 ) = PXMAX
                     CALL PAR_DEF1D( 'XLIMIT', MAXDIM, XLIMIT, STATUS )
                     CALL PAR_EXACD( 'XLIMIT', MAXDIM, XLIMIT, STATUS )
                     XMIN = XLIMIT( 1 )
                     XMAX = XLIMIT( 2 )

                     YLIMIT( 1 ) = PYMIN
                     YLIMIT( 2 ) = PYMAX
                     CALL PAR_DEF1D( 'YLIMIT', MAXDIM, YLIMIT, STATUS )
                     CALL PAR_EXACD( 'YLIMIT', MAXDIM, YLIMIT, STATUS )
                     YMIN = YLIMIT( 1 )
                     YMAX = YLIMIT( 2 )

*  Map the axis-centre arrays of the new NDF for write access.
                     CALL NDF_AMAP( NDFO, 'CENTRE', 1, '_DOUBLE',
     :                              'UPDATE', XPTR, EL, STATUS )
                     CALL NDF_AMAP( NDFO, 'CENTRE', 2, '_DOUBLE',
     :                              'UPDATE', YPTR, EL, STATUS )

*  Fill the axis arrays with appropriate values.
                     IF ( XDIM .EQ. 1 )  THEN
                        CALL KPG1_SSAZD( XDIM, 0.0D0, XMIN,
     :                                   %VAL( XPTR ), STATUS )
                     ELSE
                        CALL KPG1_SSAZD( XDIM, ( XMAX - XMIN ) /
     :                                   DBLE( XDIM - 1 ), XMIN,
     :                                   %VAL( XPTR ), STATUS )
                     END IF

                     IF ( YDIM .EQ. 1 )  THEN
                        CALL KPG1_SSAZD( YDIM, 0.0D0, YMIN,
     :                                   %VAL( YPTR ), STATUS )
                     ELSE
                        CALL KPG1_SSAZD( YDIM, ( YMAX - YMIN ) /
     :                                   DBLE( YDIM - 1 ), YMIN,
     :                                   %VAL( YPTR ), STATUS )
                     END IF
                  END IF

*  Specify the rounding for fuzzy co-ordinate limits.
                  ROUND( 1 ) = 1.0D0 - VAL__EPSD * 1.0D3
                  ROUND( 2 ) = 1.0D0 + VAL__EPSD * 1.0D3

*  In order to test for pixels outside the range, we have to allow for
*  fuzzy edges due to truncation of significant figures inside the
*  parmerer system.  The sense of the fuzziness depends on the sense of
*  the axis (shaving off some for an increasing co-ordinate with pixel
*  index at lower limit and vice versa).
                  IF ( XMIN .LT. XMAX ) THEN
                     XROUND( 1 ) = ABS( XMIN ) .LT. ABS( PXMIN ) *
     :                             ROUND( 1 )
                     XROUND( 2 ) = ABS( XMAX ) .GT. ABS( PXMAX ) *
     :                             ROUND( 2 )
                  ELSE
                     XROUND( 1 ) = ABS( XMIN ) .GT. ABS( PXMIN ) *
     :                             ROUND( 2 )
                     XROUND( 2 ) = ABS( XMAX ) .LT. ABS( PXMAX ) *
     :                             ROUND( 1 )
                  END IF

                  IF ( YMIN .LT. YMAX ) THEN
                     YROUND( 1 ) = ABS( YMIN ) .LT. ABS( PYMIN ) *
     :                             ROUND( 1 )
                     YROUND( 2 ) = ABS( YMAX ) .GT. ABS( PYMAX ) *
     :                             ROUND( 2 )
                  ELSE
                     YROUND( 1 ) = ABS( YMIN ) .GT. ABS( PYMIN ) *
     :                             ROUND( 2 )
                     YROUND( 2 ) = ABS( YMAX ) .LT. ABS( PYMAX ) *
     :                             ROUND( 1 )
                  END IF

*  Any pixels in the NDF outside the valid range of the polynomial will
*  be set to bad values.  Report if this
*  is going to happen.
                  IF ( XROUND( 1 ) .OR. XROUND( 2 ) .OR.
     :                 YROUND( 1 ) .OR. YROUND( 2 ) ) THEN

                     CALL MSG_OUT( ' ', 'N.B:  Some points lie '/
     :                 /'outside the valid range of the surface '/
     :                 /'fit', STATUS )
                     CALL MSG_OUT( ' ', '      and will be set '/
     :                 /'to bad values.', STATUS )
                  END IF

*  Determine the processing type of the output array.
                  CALL NDF_MTYPE( '_REAL,_DOUBLE', NDFO, NDFO, 'Data',
     :                            ITYPE, DTYPE, STATUS )

*  Map the data array (and possibly the variance) of the new NDF for
*  write access.
                  IF ( CREVAR ) THEN
                     CALL KPG1_MAP( NDFO, 'Data,Variance', ITYPE,
     :                             'WRITE/BAD', DPTR, EL, STATUS )
                  ELSE
                     CALL KPG1_MAP( NDFO, 'Data', ITYPE,
     :                             'WRITE/BAD', DPTR, EL, STATUS )
                  END IF

*  Map some workspace for the evaluation of the polynomial.
                  CALL PSX_CALLOC( XDIM, '_DOUBLE', WPTR, STATUS )

*  Check everything has worked so far.  (This test guards against
*  KPS1_FSPAx being called with junk for the mapped data pointers).
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  Calculate the polynomial at each pixel, calling the routine of the
*  appropriate data type.
                     IF ( ITYPE .EQ. '_REAL' ) THEN
                        CALL KPS1_MSPAR( XDIM, YDIM, %VAL( XPTR ),
     :                                   PXMIN, PXMAX, %VAL( YPTR ),
     :                                   PYMIN, PYMAX, NXPAR, NYPAR,
     :                                   MCHOEF, CHCOEF, %VAL( WPTR ),
     :                                   %VAL( DPTR( 1 ) ), STATUS )

                     ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                        CALL KPS1_MSPAD( XDIM, YDIM, %VAL( XPTR ),
     :                                   PXMIN, PXMAX, %VAL( YPTR ),
     :                                   PYMIN, PYMAX, NXPAR, NYPAR,
     :                                   MCHOEF, CHCOEF, %VAL( WPTR ),
     :                                   %VAL( DPTR( 1 ) ), STATUS )
                     END IF

*  Create the variance array.
                     IF ( CREVAR ) THEN

                        IF ( ITYPE .EQ. '_REAL' ) THEN
                           CALL KPS1_MSPAR( XDIM, YDIM, %VAL( XPTR ),
     :                                      PXMIN, PXMAX, %VAL( YPTR ),
     :                                      PYMIN, PYMAX, NXPAR, NYPAR,
     :                                      MCHOEF, VARIAN,
     :                                      %VAL( WPTR ),
     :                                      %VAL( DPTR( 2 ) ), STATUS )

                        ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                           CALL KPS1_MSPAD( XDIM, YDIM, %VAL( XPTR ),
     :                                      PXMIN, PXMAX, %VAL( YPTR ),
     :                                      PYMIN, PYMAX, NXPAR, NYPAR,
     :                                      MCHOEF, VARIAN,
     :                                      %VAL( WPTR ),
     :                                      %VAL( DPTR( 2 ) ), STATUS )
                        END IF

                     END IF
                  END IF

               ELSE
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'VARTN', VARNT )
                  CALL ERR_REP( ' ', 'Polynomials of variant '/
     :              /'^VARNT cannot be dealt with at present.', STATUS )
               END IF

*  Tidy up the workspace.
               CALL PSX_FREE( WPTR, STATUS )
               IF ( OBWORK ) THEN
                  CALL PSX_FREE( XPTR, STATUS )
                  CALL PSX_FREE( YPTR, STATUS )
               END IF

            END IF

         ELSE IF ( FTYPE .EQ. 'SPLINE' ) THEN
            CALL MSG_OUT( ' ', 'Sorry, SPLINE surfaces have not '/
     :                    /'been implemented yet.', STATUS )

         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FTYPE', FTYPE )
            CALL ERR_REP( ' ', 'Unknown surface type, ^FTYPE.', STATUS )
         END IF

      ELSE
         CALL ERR_REP( ' ', 'Error while accessing input NDF.', STATUS )
      END IF

*  Close the NDF context, regardless of the status.
      CALL NDF_END( STATUS )

      END
