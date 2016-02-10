      SUBROUTINE ALIGN2D( STATUS )
*+
*  Name:
*     ALIGN2D

*  Purpose:
*     Aligns a pair of 2-dimensional NDFs by minimising the residuals between them.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ALIGN2D( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application attempts to align a 2-dimensional input NDF with a
*     2-dimensional reference NDF in pixel coordinates, using an affine
*     transformation of the form:
*
*        Xin = C1 + C2*Xref + C3*Yref
*
*        Yin = C4 + C5*Xref + C6*Yref
*
*     where (Xin,Yin) are pixel coordinates in the input NDF, and
*     (Xref,Yref) are pixel coordinates in the reference NDF. The
*     co-efficient values are determined by doing a least squares fit
*     that minimises the sum of the squared residuals between the
*     reference NDF and the transformed input NDF. If variance
*     information is present in either NDF, it is used to weight the
*     residuals within the fit, so that noisy data values have less
*     effect on the fit. The best fit co-efficients are displayed on
*     the screen and written to an output parameter. Optionally, the
*     transformation may be used to transform the input NDF to create
*     an output NDF (see Parameter OUT). It is possible to restrict the
*     transformation in order to prevent shear, rotation, scaling, etc.
*     (see Parameter FORM).

*  Usage:
*     align2d in ref out

*  ADAM Parameters:
*     CONSERVE = _LOGICAL (Read)
*        If set TRUE, then the output pixel values will be scaled in
*        such a way as to preserve the total data value in a feature on
*        the sky.  The scaling factor is the ratio of the output pixel
*        size to the input pixel size.  This option can only be used if
*        the Mapping is successfully approximated by one or more linear
*        transformations.  Thus an error will be reported if it used
*        when the ACC parameter is set to zero (which stops the use of
*        linear approximations), or if the Mapping is too non-linear to
*        be approximated by a piece-wise linear transformation.  The
*        ratio of output to input pixel size is evaluated once for each
*        panel of the piece-wise linear approximation to the Mapping,
*        and is assumed to be constant for all output pixels in the
*        panel.  This parameter is ignored if the NORM parameter is set
*        FALSE.  [TRUE]
*     FORM = _INTEGER (Read)
*        The form of the affine transformation to use:
*
*        - 0: Full unrestricted 6 coefficient fit
*        - 1: Shift, rotation and a common X/Y scale but no shear.
*        - 2: Shift and rotation but no scale or shear.
*        - 3: Shift but not rotation, scale or shear.
*                                                                   [0]
*     IN = NDF (Read)
*        NDF to be transformed.
*     METHOD = LITERAL (Read)
*        The method to use when sampling the input pixel values (if
*        resampling), or dividing an input pixel value between a group
*        of neighbouring output pixels (if rebinning). For details of
*        these schemes, see the descriptions of routines AST_RESAMPLEx
*        and AST_REBINSEQx in SUN/210. METHOD can take the following
*        values.
*
*        - "Linear" -- When resampling, the output pixel values are
*        calculated by bi-linear interpolation among the four nearest
*        pixels values in the input NDF.  When rebinning, the input
*        pixel value is divided bi-linearly between the four nearest
*        output pixels.  Produces smoother output NDFs than the
*        nearest-neighbour scheme, but is marginally slower.
*
*        - "Nearest" -- When resampling, the output pixel values are
*        assigned the value of the single nearest input pixel.  When
*        rebinning, the input pixel value is assigned completely to the
*        single nearest output pixel.
*
*        - "Sinc" -- Uses the sinc(pi*x) kernel, where x is the pixel
*        offset from the interpolation point (resampling) or transformed
*        input pixel centre (rebinning), and sinc(z)=sin(z)/z.  Use of
*        this scheme is not recommended.
*
*        - "SincSinc" -- Uses the sinc(pi*x)sinc(k*pi*x) kernel. A
*        valuable general-purpose scheme, intermediate in its visual
*        effect on NDFs between the bi-linear and nearest-neighbour
*        schemes.
*
*        - "SincCos" -- Uses the sinc(pi*x)cos(k*pi*x) kernel.  Gives
*        similar results to the "Sincsinc" scheme.
*
*        - "SincGauss" -- Uses the sinc(pi*x)exp(-k*x*x) kernel.  Good
*        results can be obtained by matching the FWHM of the
*        envelope function to the point-spread function of the
*        input data (see Parameter PARAMS).
*
*        - "Somb" -- Uses the somb(pi*x) kernel, where x is the pixel
*        offset from the interpolation point (resampling) or transformed
*        input pixel centre (rebinning), and somb(z)=2*J1(z)/z (J1 is
*        the first-order Bessel function of the first kind.  This scheme
*        is similar to the "Sinc" scheme.
*
*        - "SombCos" -- Uses the somb(pi*x)cos(k*pi*x) kernel.  This
*        scheme is similar to the "SincCos" scheme.
*
*        - "Gauss" -- Uses the exp(-k*x*x) kernel. The FWHM of the
*        Gaussian is given by Parameter PARAMS(2), and the point at
*        which to truncate the Gaussian to zero is given by Parameter
*        PARAMS(1).
*
*        - "BlockAve"  -- Block averaging over all pixels in the
*        surrounding N-dimensional cube. This option is only available
*        when resampling (i.e. if REBIN is set to FALSE).
*
*        All methods propagate variances from input to output, but the
*        variance estimates produced by interpolation schemes other than
*        nearest neighbour need to be treated with care since the
*        spatial smoothing produced by these methods introduces
*        correlations in the variance estimates. Also, the degree of
*        smoothing produced varies across the NDF.  This is because a
*        sample taken at a pixel centre will have no contributions from
*        the neighbouring pixels, whereas a sample taken at the corner
*        of a pixel will have equal contributions from all four
*        neighbouring pixels, resulting in greater smoothing and lower
*        noise.  This effect can produce complex Moire patterns in the
*        output variance estimates, resulting from the interference of
*        the spatial frequencies in the sample positions and in the
*        pixel-centre positions.  For these reasons, if you want to use
*        the output variances, you are generally safer using
*        nearest-neighbour interpolation.  The initial default is
*        "Nearest".  [current value]
*     NORM = _LOGICAL (Read)
*        In general, each output pixel contains contributions from
*        multiple input pixel values, and the number of input pixels
*        contributing to each output pixel will vary from pixel to
*        pixel.  If NORM is set TRUE (the default), then each output
*        value is normalised by dividing it by the number of
*        contributing input pixels, resulting in each output value being
*        the weighted mean of the contributing input values.  However,
*        if NORM is set FALSE, this normalisation is not applied.  See
*        also Parameter CONSERVE.  [TRUE]
*     OUT = NDF (Writed)
*        An optional output NDF to contain a copy of IN aligned with OUT.
*        No output is created if null (!) is supplied.
*     PARAMS( 2 ) = _DOUBLE (Read)
*        An optional array which consists of additional parameters
*        required by the Sinc, SincSinc, SincCos, SincGauss, Somb,
*        SombCos, and Gauss methods.
*
*        PARAMS( 1 ) is required by all the above schemes.
*        It is used to specify how many pixels are to contribute to the
*        interpolated result on either side of the interpolation or
*        binning point in each dimension. Typically, a value of 2 is
*        appropriate and the minimum allowed value is 1 (i.e. one pixel
*        on each side). A value of zero or fewer indicates that a
*        suitable number of pixels should be calculated automatically.
*        [0]
*
*        PARAMS( 2 ) is required only by the SombCos, Gauss, SincSinc,
*        SincCos, and SincGauss schemes.  For the SombCos, SincSinc, and
*        SincCos schemes, it specifies the number of pixels at which the
*        envelope of the function goes to zero.  The minimum value is
*        1.0, and the run-time default value is 2.0.  For the Gauss and
*        SincGauss scheme, it specifies the full-width at half-maximum
*        (FWHM) of the Gaussian envelope measured in output pixels.
*        The minimum value is 0.1, and the run-time default is 1.0.  On
*        astronomical images and spectra, good results are often
*        obtained by approximately matching the FWHM of the envelope
*        function, given by PARAMS(2), to the point-spread function of
*        the input data.  []
*     REBIN = _LOGICAL (Read)
*        Determines the algorithm used to calculate the output pixel
*        values.  If a TRUE value is given, a rebinning algorithm is
*        used.  Otherwise, a resampling algorithm is used. See the
*        "Choice of Algorithm" below.  [current value]
*     REF = NDF (Read)
*        NDF to be used as a refernece.
*     TOL = _DOUBLE (Read)
*        The maximum tolerable geometrical distortion which may be
*        introduced as a result of approximating non-linear Mappings
*        by a set of piece-wise linear transforms.  Both
*        algorithms approximate non-linear co-ordinate transformations
*        in order to improve performance, and this parameter controls
*        how inaccurate the resulting approximation is allowed to be,
*        as a displacement in pixels of the input NDF.  A value of
*        zero will ensure that no such approximation is done, at the
*        expense of increasing execution time. [0.05]
*     TR( 6 ) = _DOUBLE (Write)
*        An output parameter to which are written the coefficients of the
*        fit.
*     WLIM = _REAL (Read)
*        This parameter is only used if REBIN is set TRUE. It specifies
*        the  minimum number of good pixels which must contribute to an
*        output pixel for the output pixel to be valid.  Note,
*        fractional values are allowed. A null (!) value causes a very
*        small positive value to be used resulting in output pixels
*        being set bad only if they receive no significant contribution
*        from any input pixel.  [!]

*  Examples:
*     aligned my_data orionA my_corrected form=2
*        Aligns the two-dimensional NDF called my_data with the
*        two-dimensional NDF called orionA, putting the aligned image in
*        a new NDF called my_corrected. The transformation is restricted
*        to a shift of origin and a rotation.

*  Related Applications:
*     KAPPA: WCSALIGN.

*  Implementation Status:
*     -  This routine correctly processes the DATA, VARIANCE, WCS,
*     LABEL, TITLE, and UNITS components of the NDF.
*     -  All non-complex numeric data types can be handled.

*  Implementation Deficiencies:
*     -  The least squares minimisation starts from an initial guess
*     which is A unit transformation between IN and REF. If the actual
*     transformation is very different, then it would be faster and more
*     accurate to create a better first guess using an FFT approach such
*     as phase correlation. See "Robust image registration using log-polar
*     transform" by George Wolberg and Siavash Zokai
*     (http://www-cs.engr.ccny.cuny.edu/~wolberg/pub/icip00.pdf)

*  Copyright:
*     Copyright (C) 2016 East Asian Observatory.
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
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     2-FEB-2016 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'NDF_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'AST_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER KPG1_FLOOR
      INTEGER KPG1_CEIL

*  Local Variables:
      CHARACTER DTYPE * ( NDF__SZFTP )
      CHARACTER ITYPE * ( NDF__SZTYP )
      CHARACTER METHOD * ( 16 )
      DOUBLE PRECISION C( 6 )
      DOUBLE PRECISION DLBNDI( 2 )
      DOUBLE PRECISION DUBNDI( 2 )
      DOUBLE PRECISION LPO
      DOUBLE PRECISION MATRIX( 4 )
      DOUBLE PRECISION PARAMS( 4 )
      DOUBLE PRECISION PT1I( 2 )
      DOUBLE PRECISION PT1O( 2 )
      DOUBLE PRECISION PT2I( 2 )
      DOUBLE PRECISION PT2O( 2 )
      DOUBLE PRECISION SHIFT( 2 )
      DOUBLE PRECISION TOL
      DOUBLE PRECISION UPO
      DOUBLE PRECISION XL( 2 )
      DOUBLE PRECISION XU( 2 )
      INTEGER DIMS( 2 )
      INTEGER EL
      INTEGER ELI
      INTEGER ELO
      INTEGER FLAGS
      INTEGER FORM
      INTEGER I
      INTEGER INDF1
      INTEGER INDF1A
      INTEGER INDF2
      INTEGER INDF3
      INTEGER INTERP
      INTEGER IPDATI
      INTEGER IPDATO
      INTEGER IPIN
      INTEGER IPQUAI
      INTEGER IPQUAO
      INTEGER IPREF
      INTEGER IPVARI
      INTEGER IPVARO
      INTEGER IPVIN
      INTEGER IPVREF
      INTEGER IPW
      INTEGER IWCS2
      INTEGER IWCS3
      INTEGER LBND( 2 )
      INTEGER LBNDI( 2 )
      INTEGER LBNDO( 2 )
      INTEGER MAPHI
      INTEGER MAPHIO
      INTEGER MAPHO
      INTEGER MAPIO
      INTEGER MM
      INTEGER NBAD
      INTEGER NDIM
      INTEGER NDIMI
      INTEGER NPARAM
      INTEGER SM
      INTEGER UBND( 2 )
      INTEGER UBNDI( 2 )
      INTEGER UBNDO( 2 )
      INTEGER*8 NUSED
      LOGICAL BAD
      LOGICAL CONSRV
      LOGICAL HASQUA
      LOGICAL MORE
      LOGICAL NORM
      LOGICAL REBIN
      LOGICAL VIN
      LOGICAL VREF
      REAL WLIM
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain identifiers for the two input NDFs.
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )
      CALL LPG_ASSOC( 'REF', 'READ', INDF2, STATUS )

*  Get the WCS FrameSet from the reference NDF.
      CALL NDF_GTWCS( INDF2, IWCS2, STATUS )

*  See if an output NDF is required. Propagate the shape and other
*  properties from IN.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL LPG_PROP( INDF1, 'UNIT', 'OUT', INDF3, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            INDF3 = NDF__NOID
         END IF
      END IF

*  Trim the input pixel-index bounds to match. First clone the IN
*  identifier in case we need it when crting an output NDF.
      CALL NDF_CLONE( INDF1, INDF1A, STATUS )
      CALL NDF_MBND( 'TRIM', INDF1, INDF2, STATUS )

*  See what form of transformation is to be used.
      CALL PAR_GDR0I( 'FORM', 0, 0, 3, .FALSE., FORM, STATUS )

*  Get the  dimensions of the arrays.
      CALL NDF_DIM( INDF1, 2, DIMS, NDIM, STATUS )

*  Map the input and reference data arrays.
      CALL NDF_MAP( INDF1, 'Data', '_DOUBLE', 'READ', IPIN, EL,
     :              STATUS )
      CALL NDF_MAP( INDF2, 'Data', '_DOUBLE', 'READ', IPREF, EL,
     :              STATUS )

*  If present, map the variance arrays.
      CALL NDF_STATE( INDF1, 'Variance', VIN, STATUS )
      IF( VIN ) THEN
         CALL NDF_MAP( INDF1, 'Variance', '_DOUBLE', 'READ', IPVIN,
     :                 EL, STATUS )
      ELSE
         IPVIN = IPIN
      END IF

      CALL NDF_STATE( INDF2, 'Variance', VREF, STATUS )
      IF( VREF ) THEN
         CALL NDF_MAP( INDF2, 'Variance', '_DOUBLE', 'READ', IPVREF,
     :                 EL, STATUS )
      ELSE
         IPVREF = IPREF
      END IF

*  Calculate the alignment transformation.
      CALL KPG1_ALIGN( DIMS( 1 ), DIMS( 2 ), IPIN, IPREF, VIN, VREF,
     :                 IPVIN, IPVREF, FORM, C, STATUS )

*  Get the pixel index bounds of the NDF sections used in the minimisation.
      CALL NDF_BOUND( INDF1, 2, LBND, UBND, NDIM, STATUS )

*  Modify the transformation so that it refers to PIXEL coordinates rather
*  than GRID coordinates (i.e. takes account of the pixel origin of the
*  NDF section).
      C( 1 ) = C( 1 ) + LBND(1) - 1.5 + C( 2 )*( 1.5 - LBND(1) )
     :         + C( 3 )*( 1.5 - LBND(2) )
      C( 4 ) = C( 4 ) + LBND(2) - 1.5 + C( 5 )*( 1.5 - LBND(1) )
     :         + C( 6 )*( 1.5 - LBND(2) )

*  Display them.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', 'Transformation co-efficients:', STATUS )
      CALL MSG_SETR( 'C1', REAL( C( 1 ) ) )
      CALL MSG_SETR( 'C2', REAL( C( 2 ) ) )
      CALL MSG_SETR( 'C3', REAL( C( 3 ) ) )
      CALL MSG_OUT( ' ', '   Xin = (^C1) + (^C2)*Xref + (^C3)*Yref',
     :              STATUS )
      CALL MSG_SETR( 'C4', REAL( C( 4 ) ) )
      CALL MSG_SETR( 'C5', REAL( C( 5 ) ) )
      CALL MSG_SETR( 'C6', REAL( C( 6 ) ) )
      CALL MSG_OUT( ' ', '   Yin = (^C4) + (^C5)*Xref + (^C6)*Yref',
     :              STATUS )
      CALL MSG_BLANK( STATUS )

*  Write them to an output parameter.
      CALL PAR_PUT1D( 'TR', 6, C, STATUS )

*  Annul the NDF section identifiers since they are no longer needed.
      CALL NDF_ANNUL( INDF1, STATUS )
      CALL NDF_ANNUL( INDF2, STATUS )

*  If an output NDF is required. create it.
      IF( INDF3 .NE. NDF__NOID ) THEN

*  Get the pixel index bounds of the original input NDF (not the trimmed
*  section).
         CALL NDF_BOUND( INDF1A, 2, LBNDI, UBNDI, NDIMI, STATUS )

*  Initialise the resampling routine control flags to indicate BAD pixel
*  may be present.
         FLAGS = AST__USEBAD

*  Get the algorithm to use.
         CALL PAR_GET0L( 'REBIN', REBIN, STATUS )

*  Get the method for calculating the output array value from the
*  input values.
         MORE = .TRUE.
         DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )
            CALL PAR_CHOIC( 'METHOD', 'SincSinc', 'Nearest,Linear,'//
     :                      'Sinc,Gauss,SincSinc,SincCos,SincGauss,'//
     :                      'BlockAve,Somb,SombCos', .TRUE., METHOD,
     :                      STATUS )

            IF( REBIN .AND. METHOD( 1 : 2 ) .EQ. 'BL' ) THEN
               CALL MSG_OUT( ' ', 'Method "BlockAve" cannot be used '//
     :                       'because REBIN is set true.', STATUS )
               CALL MSG_OUT( ' ', 'Please supply a new value for '//
     :                       'Parameter METHOD.', STATUS )
               CALL PAR_CANCL( 'METHOD', STATUS )

            ELSE
               MORE = .FALSE.
            END IF
         END DO

         IF ( STATUS .NE. SAI__OK ) GO TO 999

         IF ( METHOD .EQ. 'NEAREST' ) THEN
            CALL MSG_SETC( 'M', 'Nearest Neighbour' )
            INTERP = AST__NEAREST
            NPARAM = 0
         ELSE IF ( METHOD .EQ. 'LINEAR' ) THEN
            CALL MSG_SETC( 'M', 'Linear' )
            INTERP = AST__LINEAR
            NPARAM = 0
         ELSE IF( METHOD .EQ. 'GAUSS' ) THEN
            CALL MSG_SETC( 'M', 'Gaussian' )
            INTERP = AST__GAUSS
            NPARAM = 2
         ELSE IF ( METHOD .EQ. 'SINC' ) THEN
            CALL MSG_SETC( 'M', 'Sinc' )
            INTERP = AST__SINC
            NPARAM = 1
         ELSE IF ( METHOD .EQ. 'SINCSINC' ) THEN
            CALL MSG_SETC( 'M', 'SincSinc' )
            INTERP = AST__SINCSINC
            NPARAM = 2
         ELSE IF ( METHOD .EQ. 'SINCCOS' ) THEN
            CALL MSG_SETC( 'M', 'SincCos' )
            INTERP = AST__SINCCOS
            NPARAM = 2
         ELSE IF ( METHOD .EQ. 'SINCGAUSS' ) THEN
            CALL MSG_SETC( 'M', 'SincGauss' )
            INTERP = AST__SINCGAUSS
            NPARAM = 2
         ELSE IF ( METHOD .EQ. 'BLOCKAVE' ) THEN
            CALL MSG_SETC( 'M', 'BlockAve' )
            INTERP = AST__BLOCKAVE
            NPARAM = 1
         ELSE IF ( METHOD .EQ. 'SOMB' ) THEN
            CALL MSG_SETC( 'M', 'Somb' )
            INTERP = AST__SOMB
            NPARAM = 1
         ELSE IF ( METHOD .EQ. 'SOMBCOS' ) THEN
            CALL MSG_SETC( 'M', 'SombCos' )
            INTERP = AST__SOMBCOS
            NPARAM = 2
         END IF

         IF( REBIN ) THEN
            CALL MSG_OUT( ' ', '  Creating output NDF using ^M '//
     :                    'binning.', STATUS )
         ELSE
            CALL MSG_OUT( ' ', '  Creating output NDF using ^M '//
     :                    'interpolation.', STATUS )
         END IF
         CALL MSG_BLANK( STATUS )

*  Get an additional parameter vector if required.
         IF ( NPARAM .GT. 0 ) THEN
            CALL PAR_EXACD( 'PARAMS', NPARAM, PARAMS, STATUS )
         END IF

*  Get the tolerance for Mapping linear approximation.
         CALL PAR_GET0D( 'TOL', TOL, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the minimum acceptable output weight
         IF( STATUS .EQ. SAI__OK .AND. REBIN ) THEN
            CALL PAR_GET0R( 'WLIM', WLIM, STATUS )
            IF( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               WLIM = 1.0E-10
            END IF
         END IF

*  Create a Mapping from reference (i.e. output) pixel coordinates to
*  input pixel coordinates.
         MATRIX( 1 ) = C( 2 )
         MATRIX( 2 ) = C( 3 )
         MATRIX( 3 ) = C( 5 )
         MATRIX( 4 ) = C( 6 )
         MM = AST_MATRIXMAP( 2, 2, 0, MATRIX, ' ', STATUS )

         SHIFT( 1 ) = C( 1 )
         SHIFT( 2 ) = C( 4 )
         SM = AST_SHIFTMAP( 2, SHIFT, ' ', STATUS )
         MAPIO = AST_CMPMAP( MM, SM, 1, ' ', STATUS )

*  Invert it to get the input->reference Mapping.
         CALL AST_INVERT( MAPIO, STATUS )

*  Work out the bounds of an array which would contain the resampled
*  copy of the whole input array.
         DO I = 1, 2
            DLBNDI( I ) = DBLE( LBNDI( I ) - 1 )
            DUBNDI( I ) = DBLE( UBNDI( I ) )
         END DO
         DO I = 1, 2
            CALL AST_MAPBOX( MAPIO, DLBNDI, DUBNDI, .TRUE., I, LPO, UPO,
     :                       XL, XU, STATUS )
            LBNDO( I ) = KPG1_FLOOR( REAL( LPO ) ) + 1
            UBNDO( I ) = KPG1_CEIL( REAL( UPO ) )
         END DO

*  Set the shape of the output NDF.
         CALL NDF_SBND( 2, LBNDO, UBNDO, INDF3, STATUS )

*  Determine a data type which can be used for operations on the
*  DATA and possibly VARIANCE components of the NDF.
         IF( REBIN ) THEN
            CALL NDF_MTYPN( '_INTEGER,_REAL,_DOUBLE', 1, INDF1A,
     :                      'DATA,VARIANCE', ITYPE, DTYPE, STATUS )
         ELSE
            CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,'//
     :                      '_INT64, _REAL,_DOUBLE', 1, INDF1A,
     :                      'DATA,VARIANCE', ITYPE, DTYPE, STATUS )
         END IF

*  Set the Data and possibly VARIANCE component data types.
         CALL NDF_STYPE( ITYPE, INDF3, 'DATA', STATUS )
         IF ( VIN ) THEN
            CALL NDF_STYPE( ITYPE, INDF3, 'VARIANCE', STATUS )
         END IF

*  Remove any WCS FrameSet in the output NDF, and then get the default
*  WCS FrameSet containing just GRID, PIXEL and AXIS.
         CALL NDF_RESET( INDF3, 'WCS', STATUS )
         CALL NDF_GTWCS( INDF3, IWCS3, STATUS )

*  Merge the output and reference WCS FrameSets, aligning them in PIXEL
*  coords.
         CALL KPG1_ASMRG( IWCS3, IWCS2, 'PIXEL', .TRUE., 0, STATUS )

*  Store the merged FrameSet back in the output NDF. All this means that
*  the output NDF inherits the WCS of the reference NDF.
         CALL NDF_PTWCS( IWCS3, INDF3, STATUS )

*  Map the Data array of the input and output NDFs.
         CALL NDF_MAP( INDF1A, 'DATA', ITYPE, 'READ', IPDATI, ELI,
     :                 STATUS )
         CALL NDF_MAP( INDF3, 'DATA', ITYPE, 'WRITE', IPDATO, ELO,
     :                 STATUS )

*  Map the VARIANCE component of the input and output NDFs if we are
*  processing variances.
         IF ( VIN ) THEN
            CALL NDF_MAP( INDF1A, 'VARIANCE', ITYPE, 'READ', IPVARI,
     :                    ELI, STATUS )
            CALL NDF_MAP( INDF3, 'VARIANCE', ITYPE, 'WRITE', IPVARO,
     :                    ELO, STATUS )

*  Record the fact that variances should be processed.
            FLAGS = FLAGS + AST__USEVAR
         END IF

*  See if the normalisation of the output values is to be skipped.
*  Only applies to rebinning.
         IF( REBIN ) THEN
            CALL PAR_GET0L( 'NORM', NORM, STATUS )
            IF( .NOT. NORM ) FLAGS = FLAGS + AST__NONORM
         ELSE
            NORM = .TRUE.
         END IF

*  If not, see if total flux is to be preserved.
         IF( NORM ) THEN
            CONSRV = REBIN
            CALL PAR_DEF0L( 'CONSERVE', CONSRV, STATUS )
            CALL PAR_GET0L( 'CONSERVE', CONSRV, STATUS )
            IF( CONSRV ) FLAGS = FLAGS + AST__CONSERVEFLUX
         ELSE
            CONSRV = .FALSE.
         END IF

*  Since AST_RESAMPLE<X> requires the centre of pixels to be represented
*  by integers (the LBND and UBND arrays) it is necessary to add a
*  half-pixel shift on to both ends of the Mapping prior to executing
*  the resample.  First construct a Mapping which transforms minus a
*  half pixel in every input dimension.
         DO I = 1, 2
            PT1I( I ) = 0D0
            PT2I( I ) = 1D0
            PT1O( I ) = PT1I( I ) - 0.5D0
            PT2O( I ) = PT2I( I ) - 0.5D0
         END DO
         MAPHI = AST_WINMAP( 2, PT1I, PT2I, PT1O, PT2O, ' ', STATUS )

*  Then one which transforms plus a half-pixel in every output
*  dimension.
         DO I = 1, 2
            PT1I( I ) = 0D0
            PT2I( I ) = 1D0
            PT1O( I ) = PT1I( I ) + 0.5D0
            PT2O( I ) = PT2I( I ) + 0.5D0
         END DO
         MAPHO = AST_WINMAP( 2, PT1I, PT2I, PT1O, PT2O, ' ', STATUS )

*  Combine these to get a Mapping which does what we want it to,
*  correcting for the half pixel at either end.
         MAPHIO = AST_CMPMAP( MAPHI, MAPIO, .TRUE., ' ', STATUS )
         MAPHIO = AST_CMPMAP( MAPHIO, MAPHO, .TRUE., ' ', STATUS )
         MAPHIO = AST_SIMPLIFY( MAPHIO, STATUS )

*  Perform the resampling according to data type.
         IF( .NOT. REBIN ) THEN
            IF ( ITYPE .EQ. '_BYTE' ) THEN
               NBAD = AST_RESAMPLEB( MAPHIO, 2, LBNDI, UBNDI,
     :                            %VAL( CNF_PVAL( IPDATI )),
     :                            %VAL( CNF_PVAL( IPVARI )), INTERP,
     :                            AST_NULL, PARAMS, FLAGS, TOL, 100,
     :                            VAL__BADB, 2, LBNDO, UBNDO, LBNDO,
     :                            UBNDO, %VAL( CNF_PVAL( IPDATO )),
     :                            %VAL( CNF_PVAL( IPVARO )),
     :                            STATUS )

            ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
               NBAD = AST_RESAMPLEUB( MAPHIO, 2, LBNDI, UBNDI,
     :                            %VAL( CNF_PVAL( IPDATI )),
     :                            %VAL( CNF_PVAL( IPVARI )), INTERP,
     :                            AST_NULL, PARAMS, FLAGS, TOL, 100,
     :                            VAL__BADUB, 2, LBNDO, UBNDO, LBNDO,
     :                            UBNDO, %VAL( CNF_PVAL( IPDATO )),
     :                            %VAL( CNF_PVAL( IPVARO )),
     :                            STATUS )

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
               NBAD = AST_RESAMPLEW( MAPHIO, 2, LBNDI, UBNDI,
     :                            %VAL( CNF_PVAL( IPDATI )),
     :                            %VAL( CNF_PVAL( IPVARI )), INTERP,
     :                            AST_NULL, PARAMS, FLAGS, TOL, 100,
     :                            VAL__BADW, 2, LBNDO, UBNDO, LBNDO,
     :                            UBNDO, %VAL( CNF_PVAL( IPDATO )),
     :                            %VAL( CNF_PVAL( IPVARO )),
     :                            STATUS )

            ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
               NBAD = AST_RESAMPLEUW( MAPHIO, 2, LBNDI, UBNDI,
     :                            %VAL( CNF_PVAL( IPDATI )),
     :                            %VAL( CNF_PVAL( IPVARI )), INTERP,
     :                            AST_NULL, PARAMS, FLAGS, TOL, 100,
     :                            VAL__BADUW, 2, LBNDO, UBNDO, LBNDO,
     :                            UBNDO, %VAL( CNF_PVAL( IPDATO )),
     :                            %VAL( CNF_PVAL( IPVARO )),
     :                            STATUS )

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
               NBAD = AST_RESAMPLEI( MAPHIO, 2, LBNDI, UBNDI,
     :                            %VAL( CNF_PVAL( IPDATI )),
     :                            %VAL( CNF_PVAL( IPVARI )), INTERP,
     :                            AST_NULL, PARAMS, FLAGS, TOL, 100,
     :                            VAL__BADI, 2, LBNDO, UBNDO, LBNDO,
     :                            UBNDO, %VAL( CNF_PVAL( IPDATO )),
     :                            %VAL( CNF_PVAL( IPVARO )),
     :                            STATUS )

            ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
               NBAD = AST_RESAMPLEK( MAPHIO, 2, LBNDI, UBNDI,
     :                            %VAL( CNF_PVAL( IPDATI )),
     :                            %VAL( CNF_PVAL( IPVARI )), INTERP,
     :                            AST_NULL, PARAMS, FLAGS, TOL, 100,
     :                            VAL__BADK, 2, LBNDO, UBNDO, LBNDO,
     :                            UBNDO, %VAL( CNF_PVAL( IPDATO )),
     :                            %VAL( CNF_PVAL( IPVARO )),
     :                            STATUS )

            ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
               NBAD = AST_RESAMPLER( MAPHIO, 2, LBNDI, UBNDI,
     :                            %VAL( CNF_PVAL( IPDATI )),
     :                            %VAL( CNF_PVAL( IPVARI )), INTERP,
     :                            AST_NULL, PARAMS, FLAGS, TOL, 100,
     :                            VAL__BADR, 2, LBNDO, UBNDO, LBNDO,
     :                            UBNDO, %VAL( CNF_PVAL( IPDATO )),
     :                            %VAL( CNF_PVAL( IPVARO )),
     :                            STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               NBAD = AST_RESAMPLED( MAPHIO, 2, LBNDI, UBNDI,
     :                            %VAL( CNF_PVAL( IPDATI )),
     :                            %VAL( CNF_PVAL( IPVARI )), INTERP,
     :                            AST_NULL, PARAMS, FLAGS, TOL, 100,
     :                            VAL__BADD, 2, LBNDO, UBNDO, LBNDO,
     :                            UBNDO, %VAL( CNF_PVAL( IPDATO )),
     :                            %VAL( CNF_PVAL( IPVARO )),
     :                            STATUS )
            END IF


         ELSE

            FLAGS = FLAGS + AST__REBININIT + AST__REBINEND
            CALL PSX_CALLOC( ELO, '_DOUBLE', IPW, STATUS )

            IF ( ITYPE .EQ. '_INTEGER' ) THEN
               CALL AST_REBINSEQI( MAPHIO, DBLE(WLIM), 2, LBNDI, UBNDI,
     :                             %VAL( CNF_PVAL( IPDATI )),
     :                             %VAL( CNF_PVAL( IPVARI )), INTERP,
     :                             PARAMS, FLAGS, TOL, 100,
     :                             VAL__BADI, 2, LBNDO, UBNDO, LBNDI,
     :                             UBNDI, %VAL( CNF_PVAL( IPDATO )),
     :                             %VAL( CNF_PVAL( IPVARO )),
     :                             %VAL( CNF_PVAL( IPW )), NUSED,
     :                             STATUS )

            ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL AST_REBINSEQR( MAPHIO, DBLE(WLIM), 2, LBNDI, UBNDI,
     :                             %VAL( CNF_PVAL( IPDATI )),
     :                             %VAL( CNF_PVAL( IPVARI )), INTERP,
     :                             PARAMS, FLAGS, TOL, 100,
     :                             VAL__BADR, 2, LBNDO, UBNDO, LBNDI,
     :                             UBNDI, %VAL( CNF_PVAL( IPDATO )),
     :                             %VAL( CNF_PVAL( IPVARO )),
     :                             %VAL( CNF_PVAL( IPW )), NUSED,
     :                             STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL AST_REBINSEQD( MAPHIO, DBLE(WLIM), 2, LBNDI, UBNDI,
     :                             %VAL( CNF_PVAL( IPDATI )),
     :                             %VAL( CNF_PVAL( IPVARI )), INTERP,
     :                             PARAMS, FLAGS, TOL, 100,
     :                             VAL__BADD, 2, LBNDO, UBNDO, LBNDI,
     :                             UBNDI, %VAL( CNF_PVAL( IPDATO )),
     :                             %VAL( CNF_PVAL( IPVARO )),
     :                             %VAL( CNF_PVAL( IPW )), NUSED,
     :                             STATUS )
            END IF

            CALL PSX_FREE( IPW, STATUS )
            NBAD = 1

         END IF

*  We can set the bad pixels flag according to the bad pixel count
*  returned from AST_RESAMPLE<X>.
         BAD = NBAD .GT. 0
         CALL NDF_SBAD( BAD, INDF3, 'DATA', STATUS )
         IF ( VIN ) THEN
            CALL NDF_SBAD( BAD, INDF3, 'VARIANCE', STATUS )
         END IF

*  If using Nearest Neighbour interpolation, resample any QUALITY array.
         CALL NDF_STATE( INDF1A, 'QUALITY', HASQUA, STATUS )
         IF( INTERP .EQ. AST__NEAREST .AND. HASQUA ) THEN

*  Map the QUALITY array of the input and output NDFs. Note, QUALITY
*  arrays should always be mapped as _UBYTE.
            CALL NDF_MAP( INDF1A, 'QUALITY', '_UBYTE', 'READ', IPQUAI,
     :                    ELI, STATUS )
            CALL NDF_MAP( INDF3, 'QUALITY', '_UBYTE', 'WRITE', IPQUAO,
     :                    ELO, STATUS )

*  Do the resampling.
            NBAD = AST_RESAMPLEUB( MAPHIO, 2, LBNDI, UBNDI,
     :                             %VAL( CNF_PVAL( IPQUAI )),
     :                             %VAL( CNF_PVAL( IPQUAI )), INTERP,
     :                             AST_NULL, PARAMS, 0, TOL, 100,
     :                             VAL__BADUB, 2, LBNDO, UBNDO, LBNDO,
     :                             UBNDO, %VAL( CNF_PVAL( IPQUAO )),
     :                             %VAL( CNF_PVAL( IPQUAO )),
     :                             STATUS )

         END IF

      END IF

 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ALIGN2D_ERR',
     :   'ALIGN2D: Error aligning two 2-dimensional NDFs.', STATUS )
      END IF

      END
