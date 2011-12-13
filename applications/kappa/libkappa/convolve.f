      SUBROUTINE CONVOLVE( STATUS )
*+
*  Name:
*     CONVOLVE

*  Purpose:
*     Convolves a pair of NDFs where the smoothing NDF is one- or
*     two-dimensional

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CONVOLVE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application smooths an NDF using a Point-Spread Function
*     given by a second NDF.  The output NDF is normalised to the same
*     mean data value as the input NDF (if parameter NORM is set to
*     TRUE), and is the same size as the input NDF.

*     The NDF being smoothed may have up to three dimensions.  If it
*     has three significant dimensions, then the filter must be
*     two-dimensional, and it is applied in turn to each plane in the
*     cube and the result written to the corresponding plane in the
*     output cube.  The orientation of the smoothing plane can be
*     specified using the AXES parameter.

*  Usage:
*     convolve in psf out xcentre ycentre

*  ADAM Parameters:
*     AXES(2) = _INTEGER (Read)
*        This parameter is only accessed if the NDF has exactly three
*        significant pixel axes.  It should be set to the indices of the
*        NDF pixel axes which span the plane in which smoothing is to
*        be applied.  All pixel planes parallel to the specified plane
*        will be smoothed independently of each other.  The dynamic
*        default is the indices of the first two significant axes in
*        the NDF. []
*     IN = NDF (Read)
*        The input NDF containing the array to be smoothed.
*     NORM = _LOGICAL (Read)
*        Determines how the output NDF is normalised to take account of
*        the total data sum in the PSF, and of the presence of bad pixels
*        in the input NDF. If TRUE, bad pixels are excluded from the data
*        sum for each output pixel, and the associated weight for the output
*        pixel is reduced appropriately. The supplied PSF is normalised
*        to a total data sum of unity so that the output NDF has same
*        normalisation as the input NDF. If NORM is FALSE, bad pixels are
*        replaced by the mean value and then included in the convolution as
*        normal. The normalisation of the supplied PSF is left unchanged,
*        and so determines the normalisation of the output NDF. [TRUE]
*     OUT = NDF (Write)
*        The output NDF which is to contain the smoothed array.
*     PSF = NDF (Read)
*        An NDF holding the Point-Spread Function (PSF) with which the
*        input array is to be smoothed.  An error is reported if the
*        PSF contains any bad pixels.  The PSF can be centred anywhere
*        within the array (see parameters XCENTRE and YCENTRE).  A
*        constant background is removed from the PSF before use.  This
*        background level is equal to the minimum of the absolute value
*        in the four corner pixel values.  The PSF is assumed to be
*        zero beyond the bounds of the supplied NDF.  It should have
*        the same number of dimensions as the NDF being smoothed, unless
*        the input NDF has three significant dimensions, whereupon the
*        PSF must be two-dimensional. It will be normalised to a total
*        data sum of unity if NORM is TRUE.
*     TITLE = LITERAL (Read)
*        A title for the output NDF.  A null (!) value means using the
*        title of the input NDF. [!]
*     WLIM = _REAL (Read)
*        If the input array contains bad pixels, and NORM is TRUE, then
*        this parameter may be used to determine the number of good pixels
*        that must be present within the smoothing box before a valid output
*        pixel is generated.  It can be used, for example, to prevent
*        output pixels from being generated in regions where there are
*        relatively few good pixels to contribute to the smoothed
*        result.
*
*        By default, a null (!) value is used for WLIM, which causes
*        the pattern of bad pixels to be propagated from the input
*        array to the output array unchanged.  In this case, smoothed
*        output values are only calculated for those pixels which are
*        not bad in the input array.
*
*        If a numerical value is given for WLIM, then it specifies the
*        minimum total weight associated with the good pixels in the
*        smoothing box required to generate a good output pixel
*        (weights for each pixel are defined by the normalised PSF).
*        If this specified minimum weight is not present, then a bad
*        output pixel will result, otherwise a smoothed output value
*        will be calculated.  The value of this parameter should lie
*        between 0.0 and 1.0.  A value of 0.0 will result in a good
*        output pixel being created even if only one good input pixel
*        contributes to it.  A value of 1.0 will result in a good output
*        pixel being created only if all the input pixels which
*        contribute to it are good. See also NORM. [!]
*     XCENTRE = _INTEGER (Read)
*        The x pixel index (column number) of the centre of the PSF
*        within the supplied PSF array.  The suggested default is the
*        centre of the PSF array.  (This is how the PSF command would
*        generate the array.)
*     YCENTRE = _INTEGER (Read)
*        The y pixel index (line number) of the centre of the PSF
*        within the supplied PSF array.  The suggested default is the
*        centre of the PSF array.  (This is how the PSF command would
*        generate the array.)

*  Examples:
*     convolve ccdframe iraspsf ccdlores 50 50
*        The image in the NDF called ccdframe is convolved using the
*        PSF in NDF iraspsf to create the smoothed image ccdlores.  The
*        centre of the PSF image in iraspsf is at pixel indices
*        (50,50).  Any bad pixels in the input image are propagated to
*        the output.
*     convolve ccdframe iraspsf ccdlores 50 50 wlim=1.0
*        As above, but good output values are only created for pixels
*        which have no contributions from bad input pixels.
*     convolve ccdframe iraspsf ccdlores \
*        As in the first example except the centre of the PSF is located
*        at the centre of the PSF array.

*  Notes:
*     -  The algorithm used is based on the multiplication of the
*     Fourier transforms of the input array and PSF array.
*     -  A PSF can be created using the PSF command or MATHS if the
*     PSF is an analytic function.

*  Related Applications:
*     KAPPA: BLOCK, FFCLEAN, GAUSMOOTH, MATHS, MEDIAN, PSF; Figaro:
*     ICONV3, ISMOOTH, IXSMOOTH, MEDFILT.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of the
*     input NDF and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using double-precision floating point.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2009-2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RAHM: Rhys Morris (STARLINK, University of Wales, Cardiff)
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     12-MAY-1992 (RAHM):
*        Original version.
*     10-JAN-1995 (DSB):
*        Major changes for inclusion in KAPPA.
*     1995 March 17 (MJC):
*        Improvements to the documentation including remarks about
*        suggested defaults for XCENTRE and YCENTRE, and a related
*        example, and a list of related applications.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 June 22 (MJC):
*        Added support for smoothing all two-dimensional planes in a
*        three-dimensional cube.
*     22-APR-2009 (DSB):
*        Continue to convolve remaining slices if a slice of a cube
*        contains or produces no good data.
*     26-MAY-2010 (DSB):
*        Added parameter NORM.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'PAR_ERR'          ! PAR__ error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Dimensionality required
      PARAMETER ( NDIM = 2 )

      REAL PSFFRA                ! Threshold for finding PSF dimensions
      PARAMETER ( PSFFRA = 0.01 )

*  Local Variables:
      CHARACTER * ( 13 ) COMP    ! List of components to process
      INTEGER DIMI( 2 )          ! Dimensions of supplied input NDF
      INTEGER DIMP( 2 )          ! Dimensions of supplied PSF NDF
      INTEGER EL                 ! No. of elements in mapped array
      INTEGER INDFI              ! Identifier for input array NDF
      INTEGER INDFIB             ! Section of input NDF to be smoothed
      INTEGER INDFO              ! Identifier for output array NDF
      INTEGER INDFOB             ! Section of output NDF to be filled
      INTEGER INDFP              ! Identifier for PSF NDF
      INTEGER IPI( 2 )           ! Pointer to mapped input arrays
      INTEGER IPO( 2 )           ! Pointer to mapped output arrays
      INTEGER IPP                ! Pointer to mapped PSF DATA array
      INTEGER IPW1               ! Pointer to mapped work array
      INTEGER IPW2               ! Pointer to mapped work array
      INTEGER IPW3               ! Pointer to mapped work array
      INTEGER IPW4               ! Pointer to mapped work array
      INTEGER IPW5               ! Pointer to mapped work array
      INTEGER ISTAT              ! Status for an individual slice
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF pixel axes
      INTEGER NLIN               ! Second dimension for internal arrays
      INTEGER NPIX               ! First dimension for internal arrays
      INTEGER PAXHI              ! Upper pixel bound of perp. axis
      INTEGER PAXLO              ! Lower pixel bound of perp. axis
      INTEGER PAXVAL             ! Current pixel value on perp. axis
      INTEGER PERPAX             ! Indx of axis perp. to smoothing plane
      INTEGER PSFXSZ             ! Width of PSF on 1st axis
      INTEGER PSFYSZ             ! Width of PSF on 2nd axis
      INTEGER SDIMI( 2 )         ! Indices of used axes for i/p
      INTEGER SDIMP( 2 )         ! Indices of used axes for psf
      INTEGER SLBNDP( 2 )        ! Low bounds of used axes of psf
      INTEGER SUBNDP( 2 )        ! High bounds of used axes of psf
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF pixel axes
      INTEGER W1DIM( 2 )         ! Dimensions of work array 1
      INTEGER XCEN               ! X index of PSF centre pixel
      INTEGER YCEN               ! Y index of PSF centre pixel
      LOGICAL BAD                ! Are there bad pixels in the array?
      LOGICAL BADDAT             ! Bad values stored in o/p data array?
      LOGICAL BADOUT             ! Bad pixels in output array?
      LOGICAL INBAD              ! Are all input data bad?
      LOGICAL NORM               ! Normalise returned array?
      LOGICAL OUTBAD             ! Are all output data bad?
      LOGICAL VAR                ! Does input NDF have a VARIANCE array?
      REAL WLIM                  ! Fraction of good i/p pixels required

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the input NDF and obtain the significant dimensions to smooth.
*  =====================================================================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF, its significant axes up to the maximum two
*  dimensions for processing, and the NDF's bounds.  If the NDF
*  possesses three significant dimensions, obtain an iteration axis
*  through parameter AXES, so that planes along that axis can be
*  processed in sequence.
      CALL KPG1_GNDFP( 'IN', 'AXES', NDIM, 'READ', INDFI, SDIMI, LBND,
     :                 UBND, PERPAX, STATUS )

*  Form the dimensions on the axes to be used of the input array.
      DIMI( 1 ) = UBND( SDIMI( 1 ) ) - LBND( SDIMI( 1 ) ) + 1
      DIMI( 2 ) = UBND( SDIMI( 2 ) ) - LBND( SDIMI( 2 ) ) + 1

*  See if the output array should be normalised.
      CALL PAR_GET0L( 'NORM', NORM, STATUS )

*  Determine which arrays to process.
*  ==================================

*  See if the input array has associated variance values.
      CALL NDF_STATE( INDFI, 'VAR', VAR, STATUS )
      IF ( VAR ) THEN
         COMP = 'Data,Variance'
      ELSE
         COMP = 'Data'
      END IF

*  Obtain the PSF and its significant dimensions.
*  ==============================================

*  Get the NDF containing the PSF, ensuring that it has no more than
*  two significant dimensions, unless the input NDF has three
*  significant dimensions, whereupon the PSF must be two-dimensional.
*  Also find the axes to use and their bounds.
      IF ( PERPAX .GT. 0 ) THEN
         CALL KPG1_GTNDF( 'PSF', NDIM, .TRUE., 'READ', INDFP, SDIMP,
     :                    SLBNDP, SUBNDP, STATUS )
      ELSE
         CALL KPG1_GTNDF( 'PSF', NDIM, .FALSE., 'READ', INDFP, SDIMP,
     :                    SLBNDP, SUBNDP, STATUS )
      END IF

*  Form the dimensions on the significant axes of the PSF.
      DIMP( 1 ) = SUBNDP( 1 ) - SLBNDP( 1 ) + 1
      DIMP( 2 ) = SUBNDP( 2 ) - SLBNDP( 2 ) + 1

*  Check and parameter for bad pixels.
*  ===================================

*  See if there are any bad pixels in the PSF.  If so, report an error
*  and abort.
      CALL NDF_BAD( INDFP, 'DATA', .TRUE., BAD, STATUS )
      IF ( BAD .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CONVOLVE_ERR1', 'The PSF contains bad or '/
     :     /'missing pixel values.', STATUS )
         GO TO 999
      END IF

*  Obtain the minimum fraction of good pixels which should be used to
*  calculate an output pixel value.  Test if a null value is specified
*  and set WLIM negative, annulling the error.
      CALL PAR_GDR0R( 'WLIM', 0.5, 0.0, 1.0, .FALSE., WLIM, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         WLIM = -1.0
         CALL ERR_ANNUL( STATUS )

*  If WLIM is exactly 1.0 reduce it slightly (rounding errors, etc. can
*  mean that a pixel weight may be less than 1 even if all input pixels
*  are good).
      ELSE
         WLIM = MIN( WLIM, 0.9999 )

      END IF

*  Obtain the PSF width and centre.
*  ================================

*  Map the PSF 'DATA' array.
      CALL KPG1_MAP( INDFP, 'DATA', '_DOUBLE', 'READ', IPP, EL, STATUS )

*  Get workspace for use by the routine which finds the PSF size.
      W1DIM( 1 ) = MAX( DIMP( 1 ), DIMP( 2 ) )
      W1DIM( 2 ) = 2
      CALL PSX_CALLOC( W1DIM( 1 ) * W1DIM( 2 ), '_DOUBLE', IPW1,
     :                 STATUS )

*  Get the approximate width of the PSF along both array axes.
*  Internal arrays will be padded with a blank margin of this size to
*  reduce edge effects caused by wrap-around in the convolution.
      CALL KPG1_PSFSD( %VAL( CNF_PVAL( IPP ) ), DIMP( 1 ), DIMP( 2 ),
     :                 %VAL( CNF_PVAL( IPW1 ) ),
     :                 W1DIM( 1 ), W1DIM( 2 ), PSFFRA, 1, PSFXSZ,
     :                 PSFYSZ, STATUS )

*  Add this margin on to the input-array dimensions to get the
*  dimensions of the internal arrays used within the convolution
*  process, and then check that this array size can be handled by the
*  FFT routines.  If not, increase the array size until it can.
      CALL FTSIZE( DIMI( 1 ) + 2 * PSFXSZ, NPIX, STATUS )
      CALL FTSIZE( DIMI( 2 ) + 2 * PSFYSZ, NLIN, STATUS )

*  Get the co-ordinates of the centre of the PSF within the supplied
*  NDF.  The default is the centre of the NDF.
      CALL PAR_GDR0I( 'XCENTRE', ( SLBNDP( 1 ) + SUBNDP( 1 ) ) / 2,
     :                SLBNDP( 1 ), SUBNDP( 1 ), .FALSE., XCEN, STATUS )
      CALL PAR_GDR0I( 'YCENTRE', ( SLBNDP( 2 ) + SUBNDP( 2 ) ) / 2,
     :                SLBNDP( 2 ), SUBNDP( 2 ), .FALSE., YCEN, STATUS )


*  Create the output NDF and workspace.
*  ====================================

*  Propagate the output NDF from the input array NDF, copying WCS,
*  UNITS, AXIS, and QUALITY components (the default components HISTORY,
*  TITLE, LABEL, and all extensions are also copied).
      CALL LPG_PROP( INDFI, 'WCS,UNITS,AXIS,QUALITY', 'OUT', INDFO,
     :               STATUS )

*  Get the required workspace.
      CALL PSX_CALLOC( NPIX * NLIN, '_DOUBLE', IPW2, STATUS )
      CALL PSX_CALLOC( NPIX * NLIN, '_DOUBLE', IPW3, STATUS )

      IF( NORM ) THEN
         CALL PSX_CALLOC( NPIX * NLIN, '_DOUBLE', IPW4, STATUS )
      ELSE
         IPW4 = IPW2
      END IF

      CALL PSX_CALLOC( 3 * MAX( NPIX, NLIN ) + 15, '_DOUBLE', IPW5,
     :                 STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Loop around planes.
*  ===================

*  Initialise flags.
      BADDAT = .FALSE.
      INBAD = .TRUE.
      OUTBAD = .TRUE.

*  Loop round every slice to be smoothed.
      PAXLO = LBND( PERPAX )
      PAXHI = UBND( PERPAX )
      DO PAXVAL = PAXLO, PAXHI

*  Get identifiers for the required slices of the input and output NDF.
         LBND( PERPAX ) = PAXVAL
         UBND( PERPAX ) = PAXVAL
         CALL NDF_SECT( INDFI, NDF__MXDIM, LBND, UBND, INDFIB, STATUS )
         CALL NDF_SECT( INDFO, NDF__MXDIM, LBND, UBND, INDFOB, STATUS )

*  Map these input and output arrays.
         CALL KPG1_MAP( INDFIB, COMP, '_DOUBLE', 'READ', IPI, EL,
     :                  STATUS )
         CALL KPG1_MAP( INDFOB, COMP, '_DOUBLE', 'WRITE', IPO, EL,
     :                  STATUS )

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Call a lower level routine to do the work.
         CALL KPS1_CNVLV( VAR, DIMI( 1 ), DIMI( 2 ),
     :                    %VAL( CNF_PVAL( IPI( 1 ) ) ),
     :                    %VAL( CNF_PVAL( IPI( 2 ) ) ),
     :                    DIMP( 1 ), DIMP( 2 ),
     :                    %VAL( CNF_PVAL( IPP ) ),
     :                    XCEN - SLBNDP( 1 ) + 1,
     :                    YCEN - SLBNDP( 2 ) + 1, NPIX, NLIN, WLIM,
     :                    NORM,
     :                    %VAL( CNF_PVAL( IPO( 1 ) ) ),
     :                    %VAL( CNF_PVAL( IPO( 2 ) ) ), BADOUT, ISTAT,
     :                    %VAL( CNF_PVAL( IPW2 ) ),
     :                    %VAL( CNF_PVAL( IPW3 ) ),
     :                    %VAL( CNF_PVAL( IPW4 ) ),
     :                    %VAL( CNF_PVAL( IPW5 ) ), STATUS )

*  Update the bad-data flag.
         IF( BADOUT ) BADDAT = .TRUE.

*  Update the "all input data bad" flag.
         IF( ISTAT .NE. 1 ) INBAD = .FALSE.

*  Update the "all output data bad" flag.
         IF( ISTAT .EQ. 0 ) OUTBAD = .FALSE.

*  Free the section identifiers.
         CALL NDF_ANNUL( INDFIB, STATUS )
         CALL NDF_ANNUL( INDFOB, STATUS )

      END DO

*  Report an error and abort if all input pixels are bad.
      IF( INBAD .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CONVOLVE_ERR3', 'No good input pixels '//
     :                 'found.', STATUS )
      END IF

*  Report an error and abort if all the output pixel values are bad.
      IF( OUTBAD .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CONVOLVE_ERR4', 'All the output data values '//
     :                 'are bad (is the value of parameter WLIM too '//
     :                 'high?)', STATUS )
      END IF

*  Set the bad-pixel flags for the output NDF components.
      CALL NDF_SBAD( BADDAT, INDFO, 'DATA', STATUS )
      IF ( VAR ) CALL NDF_SBAD( BADDAT, INDFO, 'VAR', STATUS )

*  Obtain a new title for the output NDF, with the default value
*  being the input array title.
      CALL KPG1_CCPRO( 'TITLE', 'Title', INDFI, INDFO, STATUS )

*  Jump to here if an error has occurred.
 999  CONTINUE

*  Release work space.
      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPW2, STATUS )
      CALL PSX_FREE( IPW3, STATUS )
      IF( NORM ) CALL PSX_FREE( IPW4, STATUS )
      CALL PSX_FREE( IPW5, STATUS )

*  If an error occurred, delete the output NDF.
      IF ( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDFO, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CONVOLVE_ERR2', 'CONVOLVE: Unable to '/
     :                 /'convolve two NDFs.', STATUS )
      END IF

      END
