      SUBROUTINE MAKESURFACE( STATUS )
*+
*  Name:
*     MAKESURFACE

*  Purpose:
*     Creates a two-dimensional NDF from the coefficients of a
*     polynomial surface

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
*     The coefficients describing a two-dimensional polynomial surface
*     are read from a SURFACEFIT extension in an NDF (written by
*     FITSURFACE), and are used to create a two-dimensional surface of
*     specified size and extent.  The surface is written to a new NDF.
*
*     The size and extent of the surface may be obtained from a template
*     NDF or given explicitly.
*
*     Elements in the new NDF outside the defined range of the
*     polynomial or spline will be set to bad values.

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
*        the IN NDF as defaults, and the axis ranges.  [!]
*     OUT = NDF (Write)
*        The new NDF to contain the surface fit.
*     TITLE = LITERAL (Read)
*        A title for the new NDF.  If a null response (!) is given,
*        the title will be propagated either from LIKE, or from IN
*        if LIKE=!.  [!]
*     TYPE = LITERAL (Read)
*        Data type for the new NDF (if LIKE=!).  It must be one of
*        the following: "_DOUBLE", "_REAL", "_INTEGER", "_WORD",
*        "_BYTE", "_UBYTE".  The suggested default is the data type of
*        the data array in the IN NDF.
*     UBOUND( 2 ) = _INTEGER (Read)
*        Upper bounds of new NDF (if LIKE=!).  The suggested defaults
*        are the upper bounds of the IN NDF.
*     VARIANCE = _LOGICAL (Read)
*        If TRUE, a uniform variance array equated to the mean squared
*        residual of the fit is created in the output NDF, provided the
*        SURFACEFIT structure  contains the RMS component.  [FALSE]
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
*        This generates a two-dimensional image in the NDF called
*        flatout using the surface fit stored in the two-dimensional NDF
*        flatin.  The created image has the same data type, bounds, and
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
*        This generates a two-dimensional image in the NDF called
*        flatout using the surface fit stored in the two-dimensional NDF
*        flatin.  The created image inherits the attributes of the NDF
*        called template.  The title of flatout is "Surface fit".

*  Notes:
*     -  The polynomial surface fit is stored in SURFACEFIT extension,
*     component FIT of type POLYNOMIAL, variant CHEBYSHEV or BSPLINE.
*     This extension is created by FITSURFACE.  Also read from the
*     SURFACEFIT extension is the co-ordinate system (component COSYS),
*     and the fit RMS (component RMS).
*     -  When LIKE=!, COSYS="Data" or "Axis" and the original NDF had an
*     axis that decreased with increasing pixel index, you may want to
*     flip the co-ordinate limits (via parameters XLIMIT or YLIMIT) to
*     match the original sense of the axis, otherwise the created
*     surface will be flipped with respect to the image from which it
*     was fitted.

*  Related Applications:
*     KAPPA: FITSURFACE, SURFIT.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS, and HISTORY components of an
*     NDF data structure and propagates all extensions.  However,
*     neither QUALITY nor a SURFACEFIT extension is propagated when
*     LIKE is not null.
*     -  All non-complex numeric data types can be handled.  Processing
*     is performed in single- or double-precision floating point, as
*     appropriate.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 1997-1998, 2004 Central Laboratory of the
*     Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2007-2010 Science and Technology Facilities Council.
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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
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
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 April 12 (MJC):
*        Remove unused variable.
*     2007 June 28 (MJC):
*        Allow for COSYS to be AXIS, PIXEL, or GRID.
*     2007 July 6 (MJC):
*        Added support and restructured for BSPLINE variant.
*     2008 January 3 (MJC):
*        Denormalise the fit variances before evaluation.
*     2008 January 5 (MJC):
*        Write a constant variance array using the SURFACEFIT.RMS
*        component when parameter VARIANCE is TRUE.
*     2008 June 17 (MJC):
*        Trim trailing blanks from output NDF character components.
*     2009 December 19 (MJC):
*        Instead of a fixed work array use a dynamic workspace whose
*        type depends on the fit type.
*     2010 April 28 (MJC):
*        Permit output VARIANCE for the spline fit now that component
*        SURFACEFIT.RMS is used.  This feature had been disabled because
*        at one time variance was taken from the SURFACEFIT.FIT.VARIANCE
*        component which, however, was not present in the BSPLINE
*        variant.
*     4-OCT-2019 (DSB):
*        Cast EL to INTEGER*8 when calling KPG1_AXBN<x>. At some point the
*        whole of this function (and the whole of KAPPA!) should be
*        changed to use 8-byte dimensions and counters.
*     9-JAN-2020 (DSB):
*        KPG1_AXBN<x> now uses INTEGER*4 so remove the above cast.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PAR_ERR'          ! PAR__ error codes
      INCLUDE 'PRM_PAR'          ! VAL__ definitions
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER MAXDIM             ! Maximum number of dimensions---only
      PARAMETER ( MAXDIM = 2 )   ! two-dimensional arrays can be handled

      INTEGER MXPAR              ! Maximum number of fit parameters
      PARAMETER ( MXPAR = 15 )

      INTEGER MCHOEF             ! Maximum number of Chebyshev
                                 ! polynomial coefficients
      PARAMETER ( MCHOEF = MXPAR * MXPAR )

      INTEGER MXKNOT             ! Maximum number of interior knots that
                                 ! can be handled in each direction.
      PARAMETER ( MXKNOT = MXPAR - 4 )

      INTEGER MTKNOT             ! Maximum total number of knots that
                                 ! can be handled in each direction.
      PARAMETER ( MTKNOT = MXKNOT + 8 )

      INTEGER MBCOEF             ! Maximum number of B-spline
                                 ! coefficients
      PARAMETER ( MBCOEF = MXPAR * MXPAR )

*  Local Variables:
      CHARACTER * ( NDF__SZTYP ) ATYPE ! Algorithm data type
      DOUBLE PRECISION CHCOEF( MCHOEF ) ! Chebyshev polynomial coeffs
      REAL COEFF( MBCOEF )       ! B-spline coefficients of the fit
      CHARACTER * ( 5 ) COSYS    ! Co-ordinate system
      LOGICAL CREVAR             ! Create variance array?
      INTEGER DPTR( 2 )          ! Pointer to data and variance arrays
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Destination data type
      DOUBLE PRECISION DXMAX     ! Maximum x for NDF to be created
      DOUBLE PRECISION DXMIN     ! Minimum x for NDF to be created
      DOUBLE PRECISION DYMAX     ! Maximum y for NDF to be created
      DOUBLE PRECISION DYMIN     ! Minimum y for NDF to be created
      INTEGER EL                 ! Number of elements
      CHARACTER * ( DAT__SZLOC ) FLOC ! Locator to FIT structure
                                 ! within SURFACEFIT
      CHARACTER * ( DAT__SZTYP ) FTYPE ! Type of FIT structure
      INTEGER I                  ! Loop counter
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Implementation data type
      CHARACTER * ( DAT__SZTYP ) INTYPE ! Data type of NDF containing
                                 ! coefficients
      CHARACTER * ( 40 ) LABEL   ! Data label of NDF
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER LIKEID             ! ID of template NDF
      LOGICAL LIMITS             ! PXMIN, PXMAX, PYMIN and PYMAX are
                                 ! valid?
      INTEGER NC                 ! Used length of string
      INTEGER NCOEF              ! Number of Chebyshev coefficients used
      INTEGER NDFI               ! ID of NDF containing coefficients
      INTEGER NDFO               ! ID of new NDF to contain surface
      INTEGER NDIM               ! Number of dimensions
      INTEGER NXKNOT             ! Number of knots in x direction
      INTEGER NXPAR              ! Number of fitting parameters in x
                                 ! direction
      INTEGER NYKNOT             ! Number of knots in y direction
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
      REAL RMS                   ! Root-mean squared of fitted surface
      DOUBLE PRECISION ROUND( 2 )! Rounding needed to compare
                                 ! co-ordinate limits as SUBPAR loses
                                 ! accuracy
      REAL SCALE                 ! Scale factor applied before fitting
                                 ! to improve the fit and must be
                                 ! re-applied in reciprocal
      INTEGER SDIM( MAXDIM )     ! Indices of significant dimensions
      LOGICAL STATE              ! NDF component is defined?
      CHARACTER * ( 80 ) TITLE   ! Title of NDF
      CHARACTER * ( DAT__SZTYP ) TYPE ! Data type of new NDF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF
      CHARACTER * ( 40 ) UNITS   ! Data units of NDF
      LOGICAL VALID              ! HDS locator is valid?
      DOUBLE PRECISION VARIAN( MCHOEF ) ! Chebyshev coeff's variances
      CHARACTER * ( DAT__SZTYP ) VARNT ! Variant of FIT structure
      LOGICAL VARPRE             ! Chebyshev coeff's variance present?
      INTEGER WRKPTR             ! Pointer to workspace reading coeffs.
      INTEGER WPTR               ! Pointer to temporary workspace
      INTEGER XDIM               ! First (x) dimension of data array
      CHARACTER * ( 40 ) XLABEL  ! X-axis label of NDF
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator to SURFACEFIT extension
      REAL XKNOT( MTKNOT )       ! Spline knots in x direction
      DOUBLE PRECISION XLIMIT( MAXDIM ) ! Co-ordinate range in x for
                                 ! NDF to be created
      REAL XMAX                  ! Maximum x for NDF to be created
      REAL XMIN                  ! Minimum x for NDF to be created
      INTEGER XPTR               ! Pointer to x-axis array
      LOGICAL XROUND( 2 )        ! Supplied x limits out of bounds?
      CHARACTER * ( 40 ) XUNITS  ! X-axis units of NDF
      INTEGER YDIM               ! Second (y) dimension of data array
      CHARACTER * ( 40 ) YLABEL  ! Y-axis label of NDF
      REAL YKNOT( MTKNOT )       ! Spline knots in y direction
      DOUBLE PRECISION YLIMIT( MAXDIM ) ! Co-ordinate range in y for
                                 ! NDF to be created
      REAL YMAX                  ! Maximum y for NDF to be created
      REAL YMIN                  ! Minimum y for NDF to be created
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
      CALL CMP_GET0C( FLOC, 'VARIANT', VARNT, STATUS )
      CALL DAT_TYPE( FLOC, FTYPE, STATUS )

*  Obtain the co-ordinate system.
      CALL NDF_XGT0C( NDFI, 'SURFACEFIT', 'COSYS', COSYS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Determine whether or not a variance array is to be created.  It isn't
*  when the SURFACEFIT structure does not contain an RMS component.
      CALL ERR_MARK
      CALL NDF_XGT0R( NDFI, 'SURFACEFIT', 'RMS', RMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         CREVAR = .FALSE.
      ELSE
         CALL PAR_GET0L( 'VARIANCE', CREVAR, STATUS )
      END IF
      CALL ERR_RLSE

      OBWORK = .FALSE.

*  Initialise the Chebyshev coefficients.
      DO I = 1, MCHOEF
         CHCOEF( I ) = 0.0D0
      END DO

*  Check everything has worked so far.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Check that the type of the FIT structure is recognised.  It should
*  either be POLYNOMIAL or SPLINE.
*      IF ( FTYPE .EQ. 'POLYNOMIAL' ) THEN

*  Check that the type of variant can be dealt with.
      IF ( VARNT .EQ. 'CHEBYSHEV' ) THEN
         ATYPE = '_DOUBLE'

*  PDA spline code works in single precision.
      ELSE IF ( VARNT .EQ. 'BSPLINE' ) THEN
         ATYPE = '_REAL'
      END IF

*  Obtain workspace of the appropriatte data type used to obtain
*  the fit coefficients.
      CALL PSX_CALLOC( MXPAR * MXPAR, ATYPE, WRKPTR, STATUS )

*  Extract the fit parameters from the POLYNOMIAL-type structure.
      IF ( VARNT .EQ. 'CHEBYSHEV' ) THEN

*  Read the polynomial structure to obtain the coefficients and
*  variances, and the x-y limits, and check this has worked.
         CALL KPG1_PL2GE( FLOC, MXPAR, VARNT, NXPAR, NYPAR, LIMITS,
     :                    PXMIN, PXMAX, PYMIN, PYMAX, CHCOEF,
     :                    VARPRE, VARIAN, %VAL( CNF_PVAL( WRKPTR ) ),
     :                    STATUS )

         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Determine the number of non-zero coefficients actually used.
         NCOEF = 0
         DO I = 1, MCHOEF
            IF ( ABS( CHCOEF( I ) ) .GT. VAL__SMLD ) THEN
               NCOEF = NCOEF + 1
            END IF
         END DO


*  Check that the type of variant can be dealt with.
      ELSE IF ( VARNT .EQ. 'BSPLINE' ) THEN
         CALL KPS1_BS2GE( FLOC, MXPAR, NXKNOT, NYKNOT, XKNOT, YKNOT,
     :                    SCALE, COEFF, %VAL( CNF_PVAL( WRKPTR ) ),
     :                    STATUS )

*  Set the fit limits from the exterior knots.
         PXMIN = DBLE( XKNOT( 1 ) )
         PXMAX = DBLE( XKNOT( NXKNOT + 8 ) )
         PYMIN = DBLE( YKNOT( 1 ) )
         PYMAX = DBLE( YKNOT( NYKNOT + 8 ) )

      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'VARTN', VARNT )
         CALL ERR_REP( ' ', 'Polynomials of variant ^VARNT cannot be '/
     :                 /'dealt with at present.', STATUS )
      END IF

*  Tidy workspace used to obtain the fit coefficients.
      CALL PSX_FREE( WRKPTR, STATUS )

*  Attempt to open a template NDF, which the output NDF will be based
*  on.  This may be the same as the input NDF.  If a NULL response is
*  given, the attributes will be obtained from parameters.  The
*  template NDF must only have two significant dimensions.  (Note that
*  a mark is set in the error stack, which is later released with
*  ERR_RLSE.  This allows errors encountered after the mark to be
*  annulled without affecting earlier ones.
      CALL ERR_MARK
      CALL KPG1_GTNDF( 'LIKE', MAXDIM, .TRUE., 'READ', LIKEID, SDIM,
     :                 LBND, UBND, STATUS )

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
         CALL LPG_PROP( LIKEID, 'DATA,UNITS,AXIS,TITLE,LABEL,HISTORY,'/
     :                  /'WCS,NOEXTENSION(SURFACEFIT)', 'OUT', NDFO,
     :                  STATUS )

*  Obtain the bounds of the NDF just created.
         CALL NDF_BOUND( NDFO, MAXDIM, LBND, UBND, NDIM, STATUS )

*  For ease of use just have bounds with elements one and two.
         LBND( 1 ) = LBND( SDIM( 1 ) )
         LBND( 2 ) = LBND( SDIM( 2 ) )
         UBND( 1 ) = UBND( SDIM( 1 ) )
         UBND( 2 ) = UBND( SDIM( 2 ) )

*  Evaluate the dimensions.
         XDIM = UBND( 1 ) - LBND( 1 ) + 1
         YDIM = UBND( 2 ) - LBND( 2 ) + 1

*  DATA or AXIS
*  ------------
*  Is the surface is defined in terms of data co-ordinates (now called
*  AXIS)?
         IF ( COSYS .EQ. 'DATA' .OR. COSYS .EQ. 'AXIS' ) THEN

*  Map the axis arrays of the new NDF for read access.
            CALL NDF_AMAP( NDFO, 'CENTRE', SDIM( 1 ), ATYPE, 'READ',
     :                     XPTR, EL, STATUS )
            CALL NDF_AMAP( NDFO, 'CENTRE', SDIM( 2 ), ATYPE, 'READ',
     :                     YPTR, EL, STATUS )

*  Determine the bounds of the axes of the template NDF.
            IF ( ATYPE .EQ. '_REAL' ) THEN
               CALL KPG1_AXBNR( XDIM, %VAL( CNF_PVAL( XPTR ) ),
     :                          XMIN, XMAX, STATUS )
               CALL KPG1_AXBNR( YDIM, %VAL( CNF_PVAL( YPTR ) ),
     :                          YMIN, YMAX, STATUS )

            ELSE
               CALL KPG1_AXBND( XDIM, %VAL( CNF_PVAL( XPTR ) ),
     :                          DXMIN, DXMAX, STATUS )
               CALL KPG1_AXBND( YDIM, %VAL( CNF_PVAL( YPTR ) ),
     :                          DYMIN, DYMAX, STATUS )
            END IF

*  WORLD or PIXEL
*  --------------
         ELSE

*  Get some workspace the length of the two axes.
            CALL PSX_CALLOC( XDIM, ATYPE, XPTR, STATUS )
            CALL PSX_CALLOC( YDIM, ATYPE, YPTR, STATUS )

            IF ( COSYS .EQ. 'WORLD' .OR. COSYS .EQ. 'PIXEL' ) THEN
               IF ( ATYPE .EQ. '_REAL' ) THEN

*  Fill the work arrays with pixel co-ordinates of the required type.
                  CALL KPG1_SSAZR( XDIM, 1.0D0,
     :                             DBLE( LBND( 1 ) ) - 0.5D0,
     :                             %VAL( CNF_PVAL( XPTR ) ), STATUS )
                  CALL KPG1_SSAZR( YDIM, 1.0D0,
     :                             DBLE( LBND( 2 ) ) - 0.5D0,
     :                             %VAL( CNF_PVAL( YPTR ) ), STATUS )

               ELSE
                  CALL KPG1_SSAZD( XDIM, 1.0D0,
     :                             DBLE( LBND( 1 ) ) - 0.5D0,
     :                             %VAL( CNF_PVAL( XPTR ) ), STATUS )
                  CALL KPG1_SSAZD( YDIM, 1.0D0,
     :                             DBLE( LBND( 2 ) ) - 0.5D0,
     :                             %VAL( CNF_PVAL( YPTR ) ), STATUS )
               END IF

*  GRID
*  ----
            ELSE
               IF ( ATYPE .EQ. '_REAL' ) THEN
                  CALL KPG1_SSAZR( XDIM, 1.0D0, DBLE( LBND( 1 ) ),
     :                             %VAL( CNF_PVAL( XPTR ) ),  STATUS )
                  CALL KPG1_SSAZR( YDIM, 1.0D0, DBLE( LBND( 2 ) ),
     :                             %VAL( CNF_PVAL( YPTR ) ), STATUS )

               ELSE
                  CALL KPG1_SSAZD( XDIM, 1.0D0, DBLE( LBND( 1 ) ),
     :                             %VAL( CNF_PVAL( XPTR ) ),  STATUS )
                  CALL KPG1_SSAZD( YDIM, 1.0D0, DBLE( LBND( 2 ) ),
     :                             %VAL( CNF_PVAL( YPTR ) ), STATUS )
               END IF
            END IF


*  Record that there is workspace to free.
            OBWORK = .TRUE.

*  Obtain the limits of the axes.
            IF ( ATYPE .EQ. '_DOUBLE' ) THEN
               DXMIN = DBLE( LBND( 1 ) ) - 0.5D0
               DYMIN = DBLE( LBND( 2 ) ) - 0.5D0
               DXMAX = DBLE( UBND( 1 ) ) - 0.5D0
               DYMAX = DBLE( UBND( 2 ) ) - 0.5D0

            ELSE
               XMIN = REAL( LBND( 1 ) ) - 0.5
               YMIN = REAL( LBND( 2 ) ) - 0.5
               XMAX = REAL( UBND( 1 ) ) - 0.5
               YMAX = REAL( UBND( 2 ) ) - 0.5
            END IF
         END IF

*  LIKE=!
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN

*  Annul the PAR__NULL error.  (Note that ERR_ANNUL will only annul
*  errors back to the last ERR_MARK).
         CALL ERR_ANNUL( STATUS )
         CALL ERR_RLSE

*  Obtain the data type of the output NDF, using the type of the input
*  NDF as a dynamic default.  Only allow the primitive numeric types.
         CALL NDF_TYPE( NDFI, 'DATA', INTYPE, STATUS )
         CALL PAR_CHOIC( 'TYPE', INTYPE, '_DOUBLE,_REAL,_INTEGER,'/
     :                   /'_WORD,_BYTE,_UBYTE', .TRUE., TYPE, STATUS )

*  Obtain the required type and bounds for the new NDF, using the
*  attributes of the input NDF as a default.
         CALL PAR_DEF1I( 'LBOUND', MAXDIM, LBND, STATUS )
         CALL PAR_GET1I( 'LBOUND', MAXDIM, LBND, NDIM, STATUS )

         CALL PAR_DEF1I( 'UBOUND', MAXDIM, UBND, STATUS )
         CALL PAR_GET1I( 'UBOUND', MAXDIM, UBND, NDIM, STATUS )

         XDIM = UBND( 1 ) - LBND( 1 ) + 1
         YDIM = UBND( 2 ) - LBND( 2 ) + 1

*  Create a new NDF of the right type and size from scratch.
         CALL LPG_CREAT( 'OUT', TYPE, MAXDIM, LBND, UBND, NDFO, STATUS )

*  Initialise the axis system.
         CALL NDF_ACRE( NDFO, STATUS )

*  Obtain the title of the output NDF, using the title of the input
*  NDF as the default, where there is one, if the user supplies a null
*  response.  Note that NDF_CPUT does not truncate trailing blanks.
         CALL NDF_STATE( NDFI, 'TITLE', STATE, STATUS )
         IF ( STATE ) THEN
            CALL NDF_CGET( NDFI, 'TITLE', TITLE, STATUS )
            NC = CHR_LEN( TITLE )
            CALL NDF_CPUT( TITLE( :NC ), NDFO, 'TITLE', STATUS )
         END IF
         CALL NDF_CINP( 'TITLE', NDFO, 'TITLE', STATUS )

*  Propagate the label of the input NDF, if there is one present, to
*  the output NDF.
         CALL NDF_STATE( NDFI, 'LABEL', STATE, STATUS )
         IF ( STATE ) THEN
            CALL NDF_CGET( NDFI, 'LABEL', LABEL, STATUS )
            NC = CHR_LEN( LABEL )
            CALL NDF_CPUT( LABEL( :NC ), NDFO, 'LABEL', STATUS )
         END IF

*  Propagate the label of the input NDF, if there is one present, to
*  the output NDF.
         CALL NDF_STATE( NDFI, 'UNITS', STATE, STATUS )
         IF ( STATE ) THEN
            CALL NDF_CGET( NDFI, 'UNITS', UNITS, STATUS )
            NC = CHR_LEN( UNITS )
            CALL NDF_CPUT( UNITS( :NC ), NDFO, 'UNITS', STATUS )
         END IF

*  Propagate the x-axis label of the input NDF, if there is one
*  present, to the output NDF.  Note that NDF_ACPUT does not truncate
*  trailing blanks.
         CALL NDF_ASTAT( NDFI, 'LABEL', SDIM( 1 ), STATE, STATUS )
         IF ( STATE ) THEN
            CALL NDF_ACGET( NDFI, 'LABEL', SDIM( 1 ), XLABEL, STATUS )
            NC = CHR_LEN( XLABEL )
            CALL NDF_ACPUT( XLABEL( :NC ), NDFO, 'LABEL', SDIM( 1 ),
     :                      STATUS )
         END IF

*  Propagate the y-axis label of the input NDF, if there is one
*  present, to the output NDF.
         CALL NDF_ASTAT( NDFI, 'LABEL', SDIM( 2 ), STATE, STATUS )
         IF ( STATE ) THEN
            CALL NDF_ACGET( NDFI, 'LABEL', SDIM( 2 ), YLABEL, STATUS )
            NC = CHR_LEN( YLABEL )
            CALL NDF_ACPUT( YLABEL( :NC ), NDFO, 'LABEL', SDIM( 2 ),
     :                      STATUS )
         END IF

*  Propagate the x-axis units of the input NDF, if there is one
*  present, to the output NDF.
         CALL NDF_ASTAT( NDFI, 'UNITS', SDIM( 1 ), STATE, STATUS )
         IF ( STATE ) THEN
            CALL NDF_ACGET( NDFI, 'UNITS', SDIM( 1 ), XUNITS, STATUS )
            NC = CHR_LEN( XUNITS )
            CALL NDF_ACPUT( XUNITS( :NC ), NDFO, 'UNITS', SDIM( 1 ),
     :                      STATUS )
         END IF

*  Propagate the y-axis label of the input NDF, if there is one
*  present, to the output NDF.
         CALL NDF_ASTAT( NDFI, 'UNITS', SDIM( 2 ), STATE, STATUS )
         IF ( STATE ) THEN
            CALL NDF_ACGET( NDFI, 'UNITS', SDIM( 2 ), YUNITS, STATUS )
            NC = CHR_LEN( YUNITS )
            CALL NDF_ACPUT( YUNITS( :NC ), NDFO, 'UNITS', SDIM( 2 ),
     :                      STATUS )
         END IF

*  Obtain the required co-ordinate range for this polynomial, using the
*  full range of the polynomial as a default, or for a spline the
*  defaults cover the extreme knots that are just exterior to the
*  original array's bounds.  If this is not accurate enough the SPLINE
*  variant would need modification to record the limits. The NDF will be
*  filled with bad values outside the range of the polynomial.
         XLIMIT( 1 ) = PXMIN
         XLIMIT( 2 ) = PXMAX
         YLIMIT( 1 ) = PYMIN
         YLIMIT( 2 ) = PYMAX

         CALL PAR_DEF1D( 'XLIMIT', MAXDIM, XLIMIT, STATUS )
         CALL PAR_EXACD( 'XLIMIT', MAXDIM, XLIMIT, STATUS )
         DXMIN = XLIMIT( 1 )
         DXMAX = XLIMIT( 2 )

         CALL PAR_DEF1D( 'YLIMIT', MAXDIM, YLIMIT, STATUS )
         CALL PAR_EXACD( 'YLIMIT', MAXDIM, YLIMIT, STATUS )
         DYMIN = YLIMIT( 1 )
         DYMAX = YLIMIT( 2 )

*  Map the axis-centre arrays of the new NDF for write access.
         CALL NDF_AMAP( NDFO, 'CENTRE', 1, ATYPE, 'UPDATE', XPTR,
     :                  EL, STATUS )
         CALL NDF_AMAP( NDFO, 'CENTRE', 2, ATYPE, 'UPDATE', YPTR,
     :                  EL, STATUS )

*  Fill the axis arrays with appropriate values of the appropriate
*  data type.  Note the the scale and offset arguments to KPG1_SSAZx
*  are double precision not the generic type.
         IF ( ATYPE .EQ. '_REAL' ) THEN
            IF ( XDIM .EQ. 1 )  THEN
               CALL KPG1_SSAZR( XDIM, 0.0D0, DXMIN,
     :                          %VAL( CNF_PVAL( XPTR ) ), STATUS )
            ELSE
               CALL KPG1_SSAZR( XDIM,
     :                          ( DXMAX - DXMIN ) / DBLE( XDIM - 1 ),
     :                          DXMIN, %VAL( CNF_PVAL( XPTR ) ),
     :                          STATUS )
            END IF

            IF ( YDIM .EQ. 1 )  THEN
               CALL KPG1_SSAZR( YDIM, 0.0D0, DYMIN,
     :                          %VAL( CNF_PVAL( YPTR ) ), STATUS )
            ELSE
               CALL KPG1_SSAZR( YDIM,
     :                          ( DYMAX - DYMIN ) / DBLE( YDIM - 1 ),
     :                          DYMIN, %VAL( CNF_PVAL( YPTR ) ),
     :                          STATUS )
            END IF

* We need single-precision versions of the limits from the parameter
* double-precision values.
            XMIN = REAL( DXMIN )
            XMAX = REAL( DXMAX )
            YMIN = REAL( DYMIN )
            YMAX = REAL( DYMAX )

        ELSE
            IF ( XDIM .EQ. 1 )  THEN
               CALL KPG1_SSAZD( XDIM, 0.0D0, DXMIN,
     :                          %VAL( CNF_PVAL( XPTR ) ), STATUS )
            ELSE
               CALL KPG1_SSAZD( XDIM,
     :                          ( DXMAX - DXMIN ) / DBLE( XDIM - 1 ),
     :                          DXMIN, %VAL( CNF_PVAL( XPTR ) ),
     :                          STATUS )
            END IF

            IF ( YDIM .EQ. 1 )  THEN
               CALL KPG1_SSAZD( YDIM, 0.0D0, DYMIN,
     :                          %VAL( CNF_PVAL( YPTR ) ), STATUS )
            ELSE
               CALL KPG1_SSAZD( YDIM,
     :                          ( DYMAX - DYMIN ) / DBLE( YDIM - 1 ),
     :                          DYMIN, %VAL( CNF_PVAL( YPTR ) ),
     :                          STATUS )
            END IF
         END IF
      END IF

*  We need the double-precision bounds for the rounding check.
      IF ( ATYPE .EQ. '_REAL' ) THEN
         DXMIN = DBLE( XMIN )
         DXMAX = DBLE( XMAX )
         DYMIN = DBLE( YMIN )
         DYMAX = DBLE( YMAX )

*  Specify the rounding for fuzzy co-ordinate limits.
         ROUND( 1 ) = 1.0D0 - DBLE( VAL__EPSR ) * 1.0D3
         ROUND( 2 ) = 1.0D0 + DBLE( VAL__EPSR ) * 1.0D3

      ELSE
         ROUND( 1 ) = 1.0D0 - VAL__EPSD * 1.0D3
         ROUND( 2 ) = 1.0D0 + VAL__EPSD * 1.0D3
      END IF

*  In order to test for pixels outside the range, we have to allow for
*  fuzzy edges due to truncation of significant figures inside the
*  parmerer system.  The sense of the fuzziness depends on the sense of
*  the axis (shaving off some for an increasing co-ordinate with pixel
*  index at lower limit and vice versa).
      IF ( DXMIN .LT. DXMAX ) THEN
         XROUND( 1 ) = ABS( DXMIN ) .LT. ABS( PXMIN ) * ROUND( 1 )
         XROUND( 2 ) = ABS( DXMAX ) .GT. ABS( PXMAX ) * ROUND( 2 )
      ELSE
         XROUND( 1 ) = ABS( DXMIN ) .GT. ABS( PXMIN ) * ROUND( 2 )
         XROUND( 2 ) = ABS( DXMAX ) .LT. ABS( PXMAX ) * ROUND( 1 )
      END IF

      IF ( DYMIN .LT. DYMAX ) THEN
         YROUND( 1 ) = ABS( DYMIN ) .LT. ABS( PYMIN ) * ROUND( 1 )
         YROUND( 2 ) = ABS( DYMAX ) .GT. ABS( PYMAX ) * ROUND( 2 )
      ELSE
         YROUND( 1 ) = ABS( DYMIN ) .GT. ABS( PYMIN ) * ROUND( 2 )
         YROUND( 2 ) = ABS( DYMAX ) .LT. ABS( PYMAX ) * ROUND( 1 )
      END IF

*  Any pixels in the NDF outside the valid range of the polynomial will
*  be set to bad values.  Report if this is going to happen.
      IF ( XROUND( 1 ) .OR. XROUND( 2 ) .OR.
     :     YROUND( 1 ) .OR. YROUND( 2 ) ) THEN

         CALL MSG_OUT( ' ', 'N.B:  Some points lie outside of the '/
     :                 /'valid range of the surface fit', STATUS )
         CALL MSG_OUT( ' ', '      and will be set to bad values.',
     :                 STATUS )
      END IF

*  Determine the processing type of the output array.
      CALL NDF_MTYPE( '_REAL,_DOUBLE', NDFO, NDFO, 'Data', ITYPE,
     :                DTYPE, STATUS )

*  Map the data array (and possibly the variance) of the new NDF for
*  write access.
      IF ( CREVAR ) THEN
         CALL KPG1_MAP( NDFO, 'Data,Variance', ITYPE,
     :                  'WRITE/BAD', DPTR, EL, STATUS )
      ELSE
         CALL KPG1_MAP( NDFO, 'Data', ITYPE,
     :                  'WRITE/BAD', DPTR, EL, STATUS )
      END IF

*  Map some workspace for the evaluation of the polynomial.
      CALL PSX_CALLOC( XDIM, ATYPE, WPTR, STATUS )

*  Check everything has worked so far.  (This test guards against
*  KPS1_MSPAx and KPS1_MSSAx being called with junk for the mapped
*  data pointers).
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Polynomial evaluation
*  =====================
      IF ( VARNT .EQ. 'CHEBYSHEV' ) THEN

*  Calculate the polynomial at each pixel, calling the routine of the
*  appropriate data type.
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_MSPAR( XDIM, YDIM, %VAL( CNF_PVAL( XPTR ) ),
     :                       PXMIN, PXMAX, %VAL( CNF_PVAL( YPTR ) ),
     :                       PYMIN, PYMAX, NXPAR, NYPAR, MCHOEF,
     :                       CHCOEF, %VAL( CNF_PVAL( WPTR ) ),
     :                       %VAL( CNF_PVAL( DPTR( 1 ) ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_MSPAD( XDIM, YDIM, %VAL( CNF_PVAL( XPTR ) ),
     :                       PXMIN, PXMAX, %VAL( CNF_PVAL( YPTR ) ),
     :                       PYMIN, PYMAX, NXPAR, NYPAR, MCHOEF,
     :                       CHCOEF, %VAL( CNF_PVAL( WPTR ) ),
     :                       %VAL( CNF_PVAL( DPTR( 1 ) ) ), STATUS )
         END IF


*  B-spline evaluation
*  ===================
      ELSE IF ( VARNT .EQ. 'BSPLINE' ) THEN

*  Calculate the spline at each pixel, calling the routine of the
*  appropriate data type.
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_MSSAR( XDIM, YDIM, %VAL( CNF_PVAL( XPTR ) ),
     :                       XMIN, XMAX, %VAL( CNF_PVAL( YPTR ) ),
     :                       YMIN, YMAX, NXKNOT, NYKNOT, XKNOT,
     :                       YKNOT, SCALE, NCOEF, COEFF,
     :                       %VAL( CNF_PVAL( WPTR ) ),
     :                       %VAL( CNF_PVAL( DPTR( 1 ) ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_MSSAD( XDIM, YDIM, %VAL( CNF_PVAL( XPTR ) ),
     :                       XMIN, XMAX, %VAL( CNF_PVAL( YPTR ) ),
     :                       YMIN, YMAX, NXKNOT, NYKNOT, XKNOT,
     :                       YKNOT, SCALE, NCOEF, COEFF,
     :                       %VAL( CNF_PVAL( WPTR ) ),
     :                       %VAL( CNF_PVAL( DPTR( 1 ) ) ), STATUS )
         END IF
      END IF

*  Create the cinstant variance array.
*  ===================================
         IF ( CREVAR ) THEN
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPG1_FILLR( RMS * RMS, EL,
     :                          %VAL( CNF_PVAL( DPTR( 2 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPG1_FILLD( DBLE( RMS * RMS ), EL,
     :                          %VAL( CNF_PVAL( DPTR( 2 ) ) ), STATUS )
            END IF
         END IF

*  Tidy up the workspace.
      CALL PSX_FREE( WPTR, STATUS )
      IF ( OBWORK ) THEN
         CALL PSX_FREE( XPTR, STATUS )
         CALL PSX_FREE( YPTR, STATUS )
      END IF

  999 CONTINUE

      CALL DAT_VALID( XLOC, VALID, STATUS )
      IF ( VALID ) CALL DAT_ANNUL( XLOC, STATUS )

      CALL DAT_VALID( FLOC, VALID, STATUS )
      IF ( VALID ) CALL DAT_ANNUL( FLOC, STATUS )

*  Close the NDF context, regardless of the status.
      CALL NDF_END( STATUS )

      END
