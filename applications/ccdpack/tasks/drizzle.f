      SUBROUTINE DRIZZLE( STATUS )
*+
*  Name:
*     DRIZZLE

*  Purpose:
*     Resamples and mosaics using the drizzling algorithm.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DRIZZLE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine transforms a set of NDFs from their pixel into
*     their Current coordinate system.  The resulting NDFs are
*     combined together onto a single output grid, which can therefore
*     form a mosaic of the input images.  Normalisation of the images
*     can optionally be carried out so that in overlapping regions the
*     scaling and zero point values of the images are consistent with
*     each other.
*
*     The algorithm used for combining the images on the output grid
*     is Variable-Pixel Linear Reconstruction, or so-called 'drizzling'.
*     The user is allowed to shrink the input pixels to a smaller size
*     (drops) so that each pixel of the input image only affects pixels
*     in the output image under the corresponding drop.

*  Usage:
*     drizzle in out

*  ADAM Parameters:
*     CORRECT = LITERAL (Read)
*        Name of the sequential file containing the SCALE and ZERO point
*        corrections for the list of input NDFs given by the IN parameter
*        [!]
*     GENVAR = _LOGICAL (Read)
*        If GENVAR is set to TRUE and some of the input images supplied
*        contain statistical error (variance) information, then variance
*        information will also be calculated for the output image.
*        [TRUE]
*     IN = LITERAL (Read)
*        A list of the names of the input NDFs which are to be combined
*        into a mosaic. The NDF names should be separated by commas
*        and may include wildcards. The input NDFs are accessed only
*        for reading.
*     LISTIN = _LOGICAL (Read)
*        If a TRUE value is given for this parameter (the default),
*        then the names of all the NDFs supplied as input will be
*        listed (and will be recorded in the logfile if this is
*        enabled).  Otherwise, this listing will be omitted.
*        [TRUE]
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter, then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP,
*        then the value specified there will be used. Otherwise, the
*        default is "CCDPACK.LOG".
*        [CCDPACK.LOG]
*     LOGTO = LITERAL (Read)
*        Every CCDPACK application has the ability to log its output
*        for future reference as well as for display on the terminal.
*        This parameter controls this process, and may be set to any
*        unique abbreviation of the following:
*           -  TERMINAL  -- Send output to the terminal only
*           -  LOGFILE   -- Send output to the logfile only (see the
*                           LOGFILE parameter)
*           -  BOTH      -- Send output to both the terminal and the
*                           logfile
*           -  NEITHER   -- Produce no output at all
*
*        If the logging system has been initialised using CCDSETUP,
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     MAPVAR = _LOGICAL (Read)
*        The value of this parameter specifies whether statistical
*        error (variance) information contained in the input NDFs
*        should be used to weight the input image pixels as they
*        are drizzled on to the output NDF (see the discussion of the
*        drizzling algorithm). If MAPVAR is set to .TRUE. then the
*        ratio of the inverse variance of the input pixel and the
*        the mean inverse variance of the reference frame (or first
*        input NDF if no reference frame is provided) will be used to
*        weight each pixel as it drizzled onto the output image.
*
*        If weighting of the input pixels by the mean inverse variance
*        of the entire input image (rather than the pixels own variance)
*        is required MAPVAR should be set to .FALSE. and USEVAR should
*        be set to .TRUE. (this is the default condition).
*        [FALSE]
*     MULTI = _DOUBLE (Read)
*        The linear scaling between the size of the input and output
*        pixels, i.e. for a MULTI of 2.0 then each side of the input
*        pixel is twice that of the sub-sampling output pixel. For large
*        values of MULTI, PIXFRAC must also be larger (e.g. for a MULTI
*        of 4.0 a PIXFRAC of 0.7 is unacceptably small for simgle image
*        drizzling, however for a MULTI of 3.0 a PIXFRAC of 0.7 produces
*        acceptable output images).
*        [1.5]
*     OUT = NDF (Write)
*        Name of the NDF to contain the output mosaic.
*     PIXFRAC = _DOUBLE (Read)
*        The linear "drop" size, this being the ratio of the linear
*        size of the drizzled drop to that of the input pixel. Interlacing
*        is equivalent to setting PIXFRAC=0.0, while shift-and-add is
*        equivalent to setting PIXFRAC=1.0. For low values of PIXFRAC the
*        MULTI parameter must also be set correspondingly low.
*        [0.9]
*     PRESERVE = _LOGICAL (Read)
*        If a TRUE value is given for this parameter (the default),
*        then the data type of the output mosaic NDF will be derived
*        from that of the input NDF with the highest precision, so that
*        the input data type will be "preserved" in the output NDF.
*        Alternatively, if a FALSE value is given, then the output NDF
*        will be given an appropriate floating point data type.
*
*        When using integer input data, the former option is useful for
*        minimising the storage space required for large mosaics, while
*        the latter typically permits a wider output dynamic range when
*        necessary. A wide dynamic range is particularly important if a
*        large range of scale factor corrections are being applied (as
*        when combining images with a wide range of exposure times).
*
*        If a global value has been set up for this parameter using
*        CCDSETUP, then that value will be used.
*        [TRUE]
*     REF = NDF (Read)
*        If the input NDFs being drizzled onto the output NDF are being
*        weighted by the inverse of their mean variance (see the USEVAR
*        parameter) then by default the first NDF frame in the input
*        list (IN) will be used as a reference frame. However, if an NDF
*        is given via the REF parameter (so as to over-ride its default
*        null value), then the weighting will instead be relative to
*        the "reference NDF" supplied via this parameter.
*
*        If scale-factor, zero-point corrections (see the SCALE and
*        ZERO parameters respectively) have not been specified via a
*        sequential file listing (see the CORRECT parameter) then if
*        an NDF is given via the REF parameter the program will
*        attempt to normalise the input NDFs to the "reference NDF"
*        supplied.
*
*        This provides a means of retaining the calibration of a set of
*        data, even when corrections are being applied, by nominating a
*        reference NDF which is to remain unchanged. It also allows the
*        output mosaic to be normalised to any externally-calibrated
*        NDF with which it overlaps, and hence allows a calibration to
*        be transferred from one set of data to another.
*
*        If the NDF supplied via the REF parameter is one of those
*        supplied as input via the IN parameter, then this serves to
*        identify which of the input NDFs should be used as a
*        reference, to which the others will be adjusted. In this case,
*        the scale-factor, zero-point corrections and/or weightings
*        applied to the nominated input NDF will be set to one, zero
*        and one respectively, and the corrections for the others will
*        be adjusted accordingly.
*
*        Alternatively, if the reference NDF does not appear as one of
*        the input NDFs, then it will be included as an additional set
*        of data in the inter-comparisons made between overlapping NDFs
*        and will be used to normalise the corrections obtained (so
*        that the output mosaic is normalised to it). However, it will
*        not itself contribute to the output mosaic in this case.
*        [!]
*     SCALE = _LOGICAL (Read)
*        This parameter specifies whether DRIZZLE should attempt to
*        adjust the input data values by applying scale-factor (i.e.
*        multiplicative) corrections before combining them into a
*        mosaic. This would be appropriate, for instance, if a series
*        of images had been obtained with differing exposure times; to
*        combine them without correction would yield a mosaic with
*        discontinuities at the image edges where the data values
*        differ.
*
*        If SCALE is set to TRUE, then DRIZZLE will ask the user for a
*        sequential file containing the corrections for each image (see
*        the CORRECT parameter). If none is supplied the program will
*        attempt to find its own corrections.
*
*        DRIZZLE will inter-compare the NDFs supplied as input and will
*        estimate the relative scale-factor between selected pairs of
*        input data arrays where they overlap.  From this information,
*        a global set of multiplicative corrections will be derived which
*        make the input data as mutually consistent as possible. These
*        corrections will be applied to the input data before drizzling
*        them onto the output frame.
*
*        Calculation of scale-factor corrections may also be combined
*        with the use of zero-point corrections (see the ZERO
*        parameter). By default, no scale-factor corrections are
*        applied.
*        [FALSE]
*     TITLE = LITERAL (Read)
*        Title for the output mosaic NDF.
*        [Output from DRIZZLE]
*     USEVAR = _LOGICAL (Read)
*        The value of this parameter specifies whether statistical
*        error (variance) information contained in the input NDFs
*        should be used to weight the input image pixels as they
*        are drizzled on to the output NDF (see the discussion of the
*        drizzling algorithm). If USEVAR is set to TRUE then the
*        ratio of the mean inverse variance of the input image and
*        the mean inverse variance of the reference frame (or first
*        input NDF if no reference frame is provided) will be used as
*        a weighting for the image.
*
*        If weighting of the input image by the inverse variance map
*        (rather than the mean) then the MAPVAR parameter whould be used.
*        [TRUE]
*     ZERO = _LOGICAL (Read)
*        This parameter specifies whether DRIZZLE should attempt to
*        adjust the input data values by applying zero-point (i.e.
*        additive) corrections before combining them into a mosaic.
*        This would be appropriate, for instance, if a series of images
*        had been obtained with differing background (sky) values; to
*        combine them without correction would yield a mosaic with
*        discontinuities at the image edges where the data values
*        differ.
*
*        If ZERO is set to TRUE, then DRIZZLE will ask the user for a
*        sequential file containing the corrections for each image (see
*        the CORRECT parameter). If none is supplied the program will
*        attempt to calculate its own corrections.
*
*        DRIZZLE will inter-compare the NDFs supplied as input and will
*        estimate the relative zero-point difference between selected
*        pairs of input data arrays where they overlap.  From this
*        information, a global set of additive corrections will be
*        derived which make the input data as mutually consistent as
*        possible. These corrections will be applied to the input data
*        before drizzling them onto the output frame.
*
*        Calculation of zero-point corrections may also be combined
*        with the use of scale-factor corrections (see the SCALE
*        parameter). By default, no zero-point corrections are applied.
*        [FALSE]
*     {enter_further_parameters_here}

*  Examples:
*     drizzle * out pixfrac=0.7
*        Drizzles a set of NDFs matching the wild-card "*" into a
*        mosaic called "out". The drop size of the input pixel is set
*        to 0.7, i.e.  it is scaled to 70% of its orginal size before
*        being drizzled onto the output grid.
*
*     drizzle in=img* out=combined scale=true zero=true ref=! multi=4.0
*        Drizzles a set of NDFs matching the wild-card "img*" into a
*        mosaic called "combined". Both scaling and zero-point
*        corrections are enabled (the program will request a
*        correction file), however no reference image has been
*        supplied (the program will use the first NDF supplied in the
*        input list). The multiplicative scaling factor between input
*        and output images is set to 4, i.e. the input pixel is 4
*        times larger than the output pixel and contains 16 output
*        pixels.
*     {enter_further_examples_here}

*  Implementation Status:
*     -  All non-complex numeric data types are supported.
*     -  Bad pixels are supported.
*     -  The algorithm is restricted to handling 2D NDFs only.

*  Notes:
*     The file containing scale and zero-point corrections (see the
*     CORRECT parameter) must contain one line per frame having the
*     following information
*
*              INDEX SCALE ZERO
*
*     Where the fields have the following meaning:
*
*        INDEX = the index number of the frame, this must be the
*                same as its order number in the input list (see
*                the IN parameter)
*        SCALE = the multiplicative scaling factor for the NDF
*        ZERO  = the zero-point correction for the NDF
*
*     Comment lines may be added, but must be prefixed with a "#"
*     character.

*  Algorithms Used:
*     Taken from Fruchter et al., "A package for the reduction of dithered
*     undersampled images", in Casertano et al. (eds), HST Calibration
*     Workshop, STSCI, 1997, pp. 518-528
*
*     "The drizzle algorithm is conceptually straightforward. Pixels in
*      the original input images are mapped into pixels in the
*      subsampled output image, taking into account shifts and
*      rotations between the images and the optical distortion of the
*      camera. However, in order to avoid convolving the image with
*      the larger pixel `footprint' of the camera, we allow the user
*      to shrink the pixel before it is averaged into the output
*      image.
*
*      The new shrunken pixels, or `drops', rain down upon the
*      subsampled output. In the case of the Hubble Deep Field (HDF),
*      the drops used had linear dimensions one-half that of the input
*      pixel -- slightly larger than the dimensions of the output
*      subsampled pixels. The value of an input pixel is averaged into
*      the output pixel with a weight proportional to the area of
*      overlap between the `drop' and the output pixel. Note that, if
*      the drop size if sufficently small, not all output pixels have
*      data added to them from each input image. One must therefore
*      choose a drop size that is small enough to avoid degrading the
*      image, but large enough so that after all images are `dripped'
*      the coverage is fairly uniform.
*
*      The drop pize if controlled by a user-adjustable parameter
*      called PIXFRAC, which is simply the ratio of the linear size of
*      the drop to the input pixel (before any adjustment due to
*      geometric distortion of the camera). Thus interlacing is
*      equivalent to setting PIXFRAC=0.0, while shift-and-add is
*      equivalent to PIXFRAC=1.0.
*
*      When a drop with value i_{xy} and a user-defined weight w_{xy}
*      is added to an image with pixel value I_{xy}, weight W_{xy}, and
*      fractional pixel overlap 0 < a_{xy} < 1, the resulting value
*      the image I'_{xy} and weight W'_{xy} is
*
*                       W'_{xy} = a_{xy}w_[xy} + W_{xy}
*
*                I'_{xy} = a_{xy}i_{xy}w_{xy} + I_{xy}W_{xy}
*                          ---------------------------------
*                                      W'_{xy}
*
*      This algorithm has a number of advantages over standard linear
*      reconstruction methods presently used. Since the area of the pixels
*      scales with the Jacobian of the geometric distortion, drizzle
*      preserves both surface and absolute photometry. Therefore flux can
*      be measured using an aperture whose size is independent of position
*      on the chip. As the method anticipates that a given output pixel may
*      receive no information from a given input pixel, missing data (due
*      for instance to cosmic rays or detector defects) do not cause a
*      substantial problem, so long as there are enough dithered images to
*      fill in the gaps caused by these zero-weight input pixels. Finally
*      the linear weighting scheme is statistically optimum when inverse
*      variance maps are used as weights."

*  Pitfalls:
*     The format of the file containing scale and zero-point corrections
*     must be correct or the A-task will abort operations.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council
*     Copyright (C) 1998-1999 Central Laboratory of the Research Councils

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
*     AA: Alasdair Allan (STARLINK, Keele University)
*     MBT: Mark Taylor (STARLINK, IoA)
*     {enter_new_authors_here}

*  History:
*     7-APR-1999 (AA):
*        Original version, based on TRANNDF and MAKEMOS
*     10-JUL-1999 (AA):
*        Heavily revised version
*     23-JUL-1999 (AA):
*        Debugged version?
*     06-AUG-1999 (AA):
*        Debugged version?
*     13-AUG-1999 (AA):
*        Bare bones of algorithm completed, sort of works correctly
*     31-AUG-1999 (AA):
*        Debugged version, drizzling now works correctly
*     01-SEP-1999 (AA):
*        Added SCALE and ZERO point corrections to code
*     03-SEP-1999 (AA):
*        Propagation of WCS component from reference NDF
*     06-SEP-1999 (AA):
*        Changed MULTI from _INTEGER to _DOUBLE and propgated changes
*     06-SEP-1999 (AA):
*        Moved the drizzling algorithm from CCD1_DODIZ to CCG1_ODIZx
*     07-SEP-1999 (AA):
*        Added weighting by inverse variance map to code (MAPVAR parameter)
*     07-SEP-1999 (AA):
*        Renamed some KPG1_* routines and propagated changes
*     23-SEP-1999 (MBT):
*        Cosmetic changes, replaced some calls with standard CCDPACK ones,
*        initialised output array with BAD value.
*     25-OCT-1999 (AA):
*        Added propagation of (incorrect!) variances through drizzling
*        Propagated input data type through drizzling routine
*        Changed to use temporary workspace for weight arrays
*     26-OCT-1999 (AA):
*        Added preserve keyword and associated changes
*        Changed to use CCD1_MKTMP to create temporary workspace
*     05-NOV-1999 (AA):
*        Minor cosmetic changes to output
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     10-MAY-2005 (MBT):
*        Added GENVAR parameter.
*     26-MAY-2005 (MBT):
*        Output NDF is now created by propagation from the first input
*        NDF rather than created from scratch.  This means that any
*        extensions are propagated.  Also removed the DRIZZLE entry
*        in the CCDPACK extension, which doesn't seem to be for anything.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'CCD1_PAR'         ! General CCDPACK constants
      INCLUDE 'CCD1_MOSPR'       ! Constants specific to MAKEMOS & DRIZZLE
      INCLUDE 'PAR_ERR'          ! PAR_ error codes
      INCLUDE 'AST_PAR'          ! AST parameters
      INCLUDE 'CNF_PAR'          ! CNF functions

*  Global Variables:
      INCLUDE 'CCD1_MOSCM'       ! Global variables for MAKEMOS & DRIZZLE
*        CCD1_DDIFS( CCD1__MXCMP ) = DOUBLE PRECISION (Write)
*           Array of standard errors associated with the observed scale
*           factor differences.
*        CCD1_DDIFZ( CCD1__MXCMP ) = DOUBLE PRECISION (Write)
*           Array of standard errors associated with the observed zero
*           point differences.
*        CCD1_DIFS( CCD1__MXCMP + 1 ) = DOUBLE PRECISION (Write)
*           Array of observed scale factor differences.
*        CCD1_DIFZ( CCD1__MXCMP + 1 ) = DOUBLE PRECISION (Write)
*           Array of observed zero point differences.
*        CCD1_IPAIR( 2, CCD1__MXCMP ) = INTEGER (Write)
*           Array of pairs of indices identifying which two data arrays
*           contribute to which observed scale factor and zero point
*           difference.
*        CCD1_IREF = INTEGER (Write)
*           Index of the "reference data array" to which optimised
*           scale factor and zero point corrections should be
*           normalised (set to zero if there is no reference data
*           array).

*  Status:
      INTEGER STATUS                 ! Global status

*  External References:

*  Local Variables:
      CHARACTER * ( 6 ) ACMODE              ! NDF access mode
      CHARACTER * ( DAT__SZTYP ) ITYPE      ! NDF array implementation type
      CHARACTER * ( DAT__SZTYP ) OTYPE      ! NDF array implementation type
      CHARACTER * ( DAT__SZLOC ) DLOC       ! CCDPACK extension locator
      CHARACTER * ( 255 ) NAME           ! NDF file name (without file type).

      INTEGER WDREF
      INTEGER WVREF
      INTEGER CNREF

      DOUBLE PRECISION ASTART( NDF__MXDIM ) ! Start co-ord of each axis
      DOUBLE PRECISION AEND( NDF__MXDIM )   ! End co-ord of each axis
      DOUBLE PRECISION DDLBND( NDF__MXDIM ) ! Co-ord lower bound of input NDF
      DOUBLE PRECISION DDUBND( NDF__MXDIM ) ! Co-ord upper bounds of output NDF
      DOUBLE PRECISION DDXL( NDF__MXDIM ) ! Co-ord of input pnt gives lower bnd
      DOUBLE PRECISION DDXU( NDF__MXDIM ) ! Co-ord of input pnt gives upper bnd
      DOUBLE PRECISION DSCALE( CCD1__MXNDF + 1 ) ! Scale factor error
      DOUBLE PRECISION DZERO( CCD1__MXNDF + 1 )  ! Zero point error
      DOUBLE PRECISION MEANV                ! Mean variance for input NDF
      DOUBLE PRECISION MEAND                ! Mean count for the input NDF
      DOUBLE PRECISION MDREF                ! Mean count for reference NDF
      DOUBLE PRECISION PIXFRAC              ! Fractional size of pixel "drop"
      DOUBLE PRECISION SCALE( CCD1__MXNDF + 1 )  ! Scale factor correction
      DOUBLE PRECISION WEIGHT( CCD1__MXNDF )     ! Weight for each image
      DOUBLE PRECISION ZERO( CCD1__MXNDF + 1 )   ! Zero point correction
      DOUBLE PRECISION VARFAC                    ! Variance scaling factor
      DOUBLE PRECISION MULTI        ! Number of output pixels to input pixels

      INTEGER CFRAME( CCD1__MXNDF + 1 )   ! Index value of the current AST Frame
      INTEGER DIMSIZ                      ! Output NDF dimension size
      INTEGER EL                          ! Size of vectorised output array
      INTEGER FDIN                        ! Pointer to INFILE file descriptor
      INTEGER FRCUR( CCD1__MXNDF + 1 )    ! Pointer to the Current AST Frame
      INTEGER FRM                    ! AST pointer to frame under consideration
      INTEGER IDIM                   ! Loop counter for NDF dimensions
      INTEGER INGRP                  ! ID for group of input NDFs
      INTEGER IWCS                   ! Pointer to the input NDF WCS component
      INTEGER I, K                   ! General loop counter
      INTEGER IDAT                   ! Pointer to the Input NDF Data component
      INTEGER IDUM                   ! Dummy integer
      INTEGER IREF                   ! Loop counter for reference NDFs
      INTEGER INSIZ                  ! Input NDF dimension size
      INTEGER IDIMS( NDF__MXDIM )    ! Dimensions of the input NDF
      INTEGER ILBND( NDF__MXDIM )    ! Input NDF lower bounds
      INTEGER IUBND( NDF__MXDIM )    ! Input NDF upper bounds
      INTEGER LBNDX( NDF__MXDIM )    ! Minimum (overall) lower bound
      INTEGER IVAR                   ! Pointer to the Input NDF Variances
      INTEGER MAPC                   ! Current mapping to use
      INTEGER MAPN ( CCD1__MXNDF + 1 )   ! Compound mapping of MAPC and MAPZ
      INTEGER MAPZ                   ! New multipicative (ZOOM) map
      INTEGER NDF( CCD1__MXNDF + 1 ) ! Array of input NDF identifiers
      INTEGER NDFX                   ! NDF identifier for temporary section
      INTEGER NDIMI                  ! Number of dimensions in input NDF
      INTEGER NDIMX                  ! Maximum (overall) no. of dimensions
      INTEGER NFRM                   ! Number of frames in frameset
      INTEGER NIN                    ! Number of input NDFs
      INTEGER NNDF                   ! Number of NDFs supplied
      INTEGER NDFREF                 ! Reference NDF identifier
      INTEGER OUTNDF                 ! Pointer to the output NDF
      INTEGER NPXOUT                 ! Number of pixels in output NDF
      INTEGER NPXIN( CCD1__MXNDF )   ! Number of pixels in input NDF
      INTEGER NVAR                   ! Number of input variance arrays
      INTEGER NVIN                   ! Number of input variables
      INTEGER NVOUT                  ! Number of output variables
      INTEGER OLBND( NDF__MXDIM , CCD1__MXNDF ) ! Output NDF lower bounds
      INTEGER OUBND( NDF__MXDIM, CCD1__MXNDF )  ! Output NDF upper bounds
      INTEGER OCNT                   ! Pointer to the Output NDF Counts
      INTEGER ODAT                   ! Pointer to the Output NDF Data component
      INTEGER ODIM ( NDF__MXDIM )    ! Ouptut extension dimension sizes
      INTEGER OVAR                   ! Pointer to the Output NDF Variances
      INTEGER OWCS              ! Pointer to the WCS extension of the output NDF
      INTEGER OWHT                   ! Pointer to the Output NDF Weights
      INTEGER PFRAME                 ! Index of the PIXEL frame
      INTEGER PLACE                  ! NDF place holder
      INTEGER UBNDX( NDF__MXDIM )    ! Maximum (overall) upper bound
      INTEGER VALPIX                 ! Number of valid pixels in array (dummy)
      INTEGER VWHT                   ! Pointer to the variance weights

      LOGICAL ADJUST                 ! Apply scale/zero corrections?
      LOGICAL ISECT                  ! NDFs intersect? (not used)
      LOGICAL GENVAR                 ! Write output variance array?
      LOGICAL GETS                   ! Make scale factor corrections?
      LOGICAL GETZ                   ! Make zero point corrections?
      LOGICAL GETV                   ! Use inverse mean variances as weights?
      LOGICAL GETM                   ! Use inverse variance maps as weights?
      LOGICAL GVAR                   ! Update output variance for frame?
      LOGICAL LISTIN                 ! Display input NDFs?
      LOGICAL INOPN                  ! Success open of sequential file?
      LOGICAL SAME                   ! NDFs are the same?
      LOGICAL SWCS                   ! WCS component present if .TRUE.
      LOGICAL VAR                    ! Variance array present?
      LOGICAL PREVD                  ! Preserve input data type?

*  Internal references:
      INCLUDE 'NUM_DEC_CVT'      ! Conversion declarations
      INCLUDE 'NUM_DEF_CVT'      ! Conversion definitions
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
*  ==========
*  Start up the logging system.
      CALL CCD1_START( 'DRIZZLE', STATUS )

*  Determine if scale factor and/or zero point corrections are to be
*  made.
      CALL PAR_GET0L( 'SCALE', GETS, STATUS )
      CALL PAR_GET0L( 'ZERO', GETZ, STATUS )

*  Are we using the image variances as weights?
      CALL PAR_GET0L( 'USEVAR', GETV, STATUS )

*  Are we using variance maps?
      CALL PAR_GET0L( 'MAPVAR', GETM, STATUS )
      IF( GETM ) THEN
         GETV = .TRUE.
      ENDIF

*  Are we preserving the input data type?
      CALL PAR_GET0L( 'PRESERVE', PREVD, STATUS )

*  Get the scaling size for drizzling (ie linear scaling of output
*  to input pixels), this defaults to 2.0.
       CALL PAR_GET0D( 'MULTI', MULTI, STATUS )

*  Get the PIXFRAC (or drop) size
       CALL PAR_GET0D( 'PIXFRAC', PIXFRAC, STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Start a new AST context
      CALL AST_BEGIN( STATUS )

*  If corrections are required, then see whether the input NDFs are to
*  be modified and set an appropriate access mode string.
      ACMODE = 'READ'

*  Obtain the input NDFs.
*  =====================
*  Obtain an NDF group specifying the input NDFs.
      NIN = 0
      CALL CCD1_NDFIN( 'IN', INGRP, NIN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Report progress so far
*  ======================

      CALL CCD1_MSG( ' ',  ' ', STATUS )
      CALL CCD1_MSG( ' ', '    Input parameters:', STATUS )
      CALL CCD1_MSG( ' ', '    -----------------', STATUS )
      CALL MSG_SETD( 'REP_PIXFRAC', PIXFRAC )
      CALL CCD1_MSG( ' ', '    Fractional drop: ^REP_PIXFRAC',
     :               STATUS )
      CALL MSG_SETD( 'REP_MULTI', MULTI )
      CALL CCD1_MSG( ' ', '    Scaling (output/input): ^REP_MULTI',
     :               STATUS )
      CALL MSG_SETL( 'REP_PREVD', PREVD )
      CALL CCD1_MSG( ' ', '    Preserve input data type: ^REP_PREVD',
     :               STATUS )

*  Loop to obtain an identifier for each NDF and get the bounds for
*  the mosaiced image + determine presence of variance component
      NVAR = 0
      DO 3 I = 1, NIN

*  Initialise scale factor and zero point corrections.
         SCALE( I ) = 1.0D0
         DSCALE( I ) = 0.0D0
         ZERO( I ) = 0.0D0
         DZERO( I ) = 0.0D0

*  Get group identifier
         CALL NDG_NDFAS( INGRP, I, ACMODE, NDF( I ), STATUS )

*  Test to see whether the NDF contains variance information and count
*  the number which do.
         CALL NDF_STATE( NDF( I ), 'Variance', VAR, STATUS )
         IF ( VAR ) THEN
            NVAR = NVAR + 1
         ENDIF

*  Map the current NDF to find its actual LBND() and UBND()
*  ========================================================

*  First check that there is an existing WCS FrameSet
         CALL NDF_STATE( NDF( I ), 'WCS', SWCS, STATUS )
         IF( .NOT. SWCS ) THEN
             STATUS = SAI__ERROR
             CALL NDF_MSG( 'NDFNAME', NDF( I ) )
             CALL ERR_REP( 'DRIZZLE_NOAST', '  NDF ^NDFNAME '//
     :'does not have a WCS extension.',
     :                       STATUS )
             GOTO 940
         ENDIF

*  Get pointers to the WCS component
         CALL CCD1_GTWCS( NDF( I ), IWCS, STATUS )

*  Validate the transformation
         IF ( IWCS .EQ. AST__NULL ) THEN
             STATUS = SAI__ERROR
             CALL NDF_MSG( 'NDFNAME', NDF( I ) )
             CALL ERR_REP( 'DRIZZLE_NOAST', '  NDF ^NDFNAME '//
     :'does not have a valid WCS extension.',STATUS )
             GO TO 940
         ELSE IF ( AST_GETC( IWCS, 'Class', STATUS ) .NE. 'FrameSet' )
     :      THEN
             STATUS = SAI__ERROR
             CALL NDF_MSG( 'NDFNAME', NDF( I ) )
             CALL ERR_REP( 'DRIZZLE_FRAME', '  NDF ^NDFNAME '//
     :'does not have a WCS extension with class FrameSet.', STATUS)
             GO TO 940
         ENDIF

*  We have a valid WCS component, get pointer to the current FrameSet
         FRCUR( I ) = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Lets find out which frame contains the PIXEL domain (its going to be
*  frame 2, but we may as well do it properly) in the output WCS frameset.
         NFRM = AST_GETI( IWCS, 'Nframe', STATUS )
         DO K = 1, NFRM
            FRM = AST_GETFRAME( IWCS, K, STATUS )
            IF( AST_GETC( FRM, 'Domain', STATUS )
     :          .EQ. 'PIXEL' ) PFRAME = K
         END DO

*  Get mapping between PIXEL and Current frames.
         MAPC = AST_GETMAPPING( IWCS, PFRAME, AST__CURRENT, STATUS )
         MAPC = AST_SIMPLIFY( MAPC, STATUS )

*  Get the index of the current frame for future use
         CFRAME( I ) = AST_GETI( IWCS, 'Current', STATUS )

*  Obtain the number of input and output co-ordinates for the Mapping
         NVIN = AST_GETI( MAPC, 'Nin', STATUS )
         NVOUT = AST_GETI( MAPC, 'Nout', STATUS )

*  Get the properties of the NDF.
*  ==============================
*  Dimensions.
         CALL NDF_DIM( NDF( I ), NDF__MXDIM, IDIMS, NDIMI, STATUS )

*  Bounds.
         CALL NDF_BOUND( NDF( I ), NDF__MXDIM, ILBND, IUBND,
     :                   NDIMI, STATUS )

         IF ( STATUS .NE. SAI__OK ) GO TO 940

*  Find the coordinate bounds of the output NDF.
*  ==============================================

*  We need to pad the lower bounds because of the way the drizzling
*  alogrithm works, see ccd1_dodiz.f for details
         DO IDIM = 1, NVIN
            ASTART( IDIM ) = DBLE( ILBND( IDIM ) - 1 ) - PIXFRAC * 0.5D0
            AEND( IDIM ) = DBLE( IUBND( IDIM ) ) + PIXFRAC * 0.5D0
         END DO

*  Change the current mapping MAP to include the a multiplicative
*  ZOOMMAP of MULTI. We'll have to do the PIXFRAC twiddles later by hand
*  as this depends on where we are around the pixel.
        MAPZ = AST_ZOOMMAP( NVIN, MULTI, ' ', STATUS)
        MAPN( I ) = AST_CMPMAP( MAPC, MAPZ, .TRUE.,  ' ', STATUS )

*  Find the bounding box of the transformed co-ordinates
         DO IDIM = 1, NVIN
            CALL AST_MAPBOX( MAPN( I ), ASTART, AEND, .TRUE., IDIM,
     :                       DDLBND( IDIM ), DDUBND( IDIM ), DDXL, DDXU,
     :                       STATUS )
         END DO

*  Set the bounds of the output NDF.
*  =================================

*  Find the dimensions and bounds of the output array. The program will
*  autosize these bounds using the MULTI factor from the parameter system
         DO IDIM = 1, NVOUT
            OLBND( IDIM, I ) = NUM_DTOI( DDLBND( IDIM ) - 0.5D0 )
            OUBND( IDIM, I ) = NUM_DTOI( DDUBND( IDIM ) )
         END DO

*  Accumulate the minimum lower bound value, the maximum upper bound
*  value and the maximum number of dimensions. These determine the
*  shape of the output mosaic.
         NDIMX = NVOUT
         DO 2 IDIM = 1, NDF__MXDIM
            IF ( I .EQ. 1 ) THEN
               LBNDX( IDIM ) = OLBND( IDIM, I )
               UBNDX( IDIM ) = OUBND( IDIM, I )
            ELSE
               LBNDX( IDIM ) = MIN( LBNDX( IDIM ), OLBND( IDIM, I ) )
               UBNDX( IDIM ) = MAX( UBNDX( IDIM ), OUBND( IDIM, I ) )
            END IF
2        CONTINUE

         IF ( STATUS .NE. SAI__OK ) GO TO 940

* We use the mean variance as a pixel weighting for the image during
* drizzling, lets calculate this now for each image and stuff the value
* into an array for later use.
         IF ( VAR .AND. GETV) THEN

* Get number of pixels for the current input image
            NPXIN( I ) = 1
            DO IDIM = 1, NDIMI
               INSIZ = IUBND( IDIM ) - ILBND( IDIM ) + 1
               NPXIN( I ) = NPXIN( I ) * INSIZ
            END DO

* Map the variance component of the image
            CALL NDF_TYPE( NDF( I ), 'Variance', ITYPE, STATUS )
            CALL NDF_MAP( NDF( I ), 'Variance', ITYPE, 'READ',
     :                    IVAR, NPXIN( I ), STATUS )

* Calculate the mean variance for the image
            IF ( ITYPE .EQ. '_BYTE' ) THEN
               CALL CCG1_MEANB( .TRUE., %VAL( CNF_PVAL( IVAR ) ),
     :                          NPXIN( I ),
     :                          MEANV, VALPIX, STATUS )

*  Get the mean variance from a double-precision array.
            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL CCG1_MEAND( .TRUE., %VAL( CNF_PVAL( IVAR ) ),
     :                          NPXIN( I ),
     :                          MEANV, VALPIX, STATUS )

*  Get the mean variance from an integer array.
            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
               CALL CCG1_MEANI( .TRUE., %VAL( CNF_PVAL( IVAR ) ),
     :                          NPXIN( I ),
     :                          MEANV, VALPIX, STATUS )

*  Get the mean variance from an integer*8 array.
            ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
               CALL CCG1_MEANK( .TRUE., %VAL( CNF_PVAL( IVAR ) ),
     :                          NPXIN( I ),
     :                          MEANV, VALPIX, STATUS )

*  Get the mean variance from a single-precision array.
            ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL CCG1_MEANR( .TRUE., %VAL( CNF_PVAL( IVAR ) ),
     :                          NPXIN( I ),
     :                          MEANV, VALPIX, STATUS )

*  Get the mean variance from an unsigned-byte array.
            ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
               CALL CCG1_MEANUB( .TRUE., %VAL( CNF_PVAL( IVAR ) ),
     :                           NPXIN( I ),
     :                           MEANV, VALPIX, STATUS )

*  Get the mean variance from an unsigned-word array.
            ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
               CALL CCG1_MEANUW( .TRUE., %VAL( CNF_PVAL( IVAR ) ),
     :                           NPXIN( I ),
     :                           MEANV, VALPIX, STATUS )

*  Get the mean variance from a word array.
            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
               CALL CCG1_MEANW( .TRUE., %VAL( CNF_PVAL( IVAR ) ),
     :                          NPXIN( I ),
     :                          MEANV, VALPIX, STATUS )
            END IF

*  Calculate weight from mean variance.
            WEIGHT( I ) = 1.0D0 / MEANV
            CALL NDF_UNMAP( NDF( I ), 'Variance', STATUS )
         ELSE
            WEIGHT( I ) = 1.0D0
         ENDIF
3     CONTINUE

*  Count the number of NDFs to be considered so far.
      NNDF = NIN

*  If not all the images have a variance array then we need to set the
*  weights for the entire input stack to 1.0 otherwise wierd things will
*  happen
      IF ( NVAR .NE. NNDF ) THEN
         DO I = 1, NIN
            WEIGHT( I ) = 1.0D0
         END DO
      ENDIF

*  Work out if we will be generating an output variance array.
      IF ( NVAR .GT. 0 ) THEN
         CALL PAR_GET0L( 'GENVAR', GENVAR, STATUS )
      ELSE
         GENVAR = .FALSE.
      END IF

      IF ( STATUS .NE. SAI__OK ) GO TO 940

*  Obtain an optional reference NDF.
*  ================================
*  If correction of scale factors or zero points is required, then mark
*  the error stack and attempt to obtain a reference NDF to be used to
*  normalise the corrections.
      IF ( GETS .OR. GETZ .OR. GETV ) THEN
         CALL ERR_MARK
         CALL CCD1_NDFAC( 'REF', 'READ', 1, 1, IDUM, NDFREF, STATUS )

*  If a null reference NDF is specified, then annul the error and set
*  CCD1_IREF to zero to indicate that there is no reference NDF.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            CCD1_IREF = 0

*  Otherwise, compare the reference NDF with each of the input NDFs in
*  turn to see if they are the same. If so, set CCD1_IREF to identify
*  the reference NDF and annul the separate identifier just obtained.
         ELSE
            DO 4 IREF = 1, NIN
               CALL NDF_SAME( NDF( IREF ), NDFREF, SAME, ISECT, STATUS )
               IF ( ( STATUS .EQ. SAI__OK ) .AND. SAME ) THEN
                  CALL NDF_ANNUL( NDFREF, STATUS )
                  GO TO 5
               END IF
 4          CONTINUE
 5          CONTINUE
            CCD1_IREF = IREF

*  If the reference NDF is not amongst the input NDFs, then add it to
*  the end of the list of NDFs.
            IF ( CCD1_IREF .GT. NIN ) THEN
               NNDF = NNDF + 1
               NDF( NNDF ) = NDFREF

*  Check that there is an existing WCS FrameSet
               CALL NDF_STATE( NDF( CCD1_IREF ), 'WCS', SWCS, STATUS )
               IF( .NOT. SWCS ) THEN
                  STATUS = SAI__ERROR
                  CALL NDF_MSG( 'NDFNAME', NDF( I ) )
                  CALL ERR_REP( 'DRIZZLE_NOAST', '  NDF ^NDFNAME '//
     :'does not have a WCS extension.',STATUS )
                  GOTO 940
               ENDIF

*  Get the index of the current frame for future use
               CALL CCD1_GTWCS( NDF( CCD1_IREF ), IWCS, STATUS )
               CFRAME( CCD1_IREF ) = AST_GETI( IWCS, 'Current', STATUS )

*  Check if the reference NDF has a variance component, if so map
*  this component and work out the mean variance.
               CALL NDF_STATE( NDF( CCD1_IREF ),
     :                         'Variance', VAR, STATUS )

               IF( VAR .AND. GETV ) THEN

                  CALL NDF_TYPE( NDF( CCD1_IREF ), 'Variance', ITYPE,
     :                           STATUS )
                  CALL NDF_MAP( NDF( CCD1_IREF ), 'Variance', ITYPE,
     :                         'READ', IVAR, NPXIN( I ), STATUS )

*  Calculate the mean variance for the image

*  Get the mean variance from a byte array.
                  IF ( ITYPE .EQ. '_BYTE' ) THEN
                     CALL CCG1_MEANB( .TRUE., %VAL( CNF_PVAL( IVAR ) ),
     :                                NPXIN( I ),
     :                                MEANV, VALPIX, STATUS )

*  Get the mean variance from a double-precision array.
                  ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
                     CALL CCG1_MEANB( .TRUE., %VAL( CNF_PVAL( IVAR ) ),
     :                                NPXIN( I ),
     :                                MEANV, VALPIX, STATUS )

*  Get the mean variance from an integer array.
                  ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
                     CALL CCG1_MEANI( .TRUE., %VAL( CNF_PVAL( IVAR ) ),
     :                                NPXIN( I ),
     :                                MEANV, VALPIX, STATUS )

*  Get the mean variance from an integer*8 array.
                  ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
                     CALL CCG1_MEANK( .TRUE., %VAL( CNF_PVAL( IVAR ) ),
     :                                NPXIN( I ),
     :                                MEANV, VALPIX, STATUS )

*  Get the mean variance from a single-precision array.
                  ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
                     CALL CCG1_MEANR( .TRUE., %VAL( CNF_PVAL( IVAR ) ),
     :                                NPXIN( I ),
     :                                MEANV, VALPIX, STATUS )

*  Get the mean variance from an unsigned-byte array.
                  ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
                     CALL CCG1_MEANUB( .TRUE., %VAL( CNF_PVAL( IVAR ) ),
     :                                 NPXIN( I ),
     :                                 MEANV, VALPIX, STATUS )

*  Get the mean variance from an unsigned-word array.
                  ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
                     CALL CCG1_MEANUW( .TRUE., %VAL( CNF_PVAL( IVAR ) ),
     :                                 NPXIN( I ),
     :                                 MEANV, VALPIX, STATUS )

*  Get the mean variance from a word array.
                  ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
                     CALL CCG1_MEANW( .TRUE., %VAL( CNF_PVAL( IVAR ) ),
     :                                NPXIN( I ),
     :                                MEANV, VALPIX, STATUS )

                  END IF
                  VARFAC = MEANV
                  CALL NDF_UNMAP( NDF( I ), 'Variance', STATUS )
               ENDIF
            ELSE IF( GETV ) THEN

*  The reference image is part of the input image stack, so use this
*  as the variance factor if we're using image variances as weights
               CALL NDF_STATE( NDF( CCD1_IREF ), 'Variance',
     :                         VAR, STATUS )
               IF( VAR ) THEN
                  VARFAC = 1.0D0 / WEIGHT( CCD1_IREF )
               ENDIF
            END IF
         END IF

*  Release the error stack and check status.
         CALL ERR_RLSE
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 940

*  If we're using variances as weights then scale all the images such
*  either the reference NDF or the 1st image in the stack has weight
*  of 1.0
      IF ( VAR .AND. GETV ) THEN
         DO  IREF = 1, NIN
            IF( ( CCD1_IREF .EQ. 0 ) .AND. ( IREF .EQ. 1 ) ) THEN
               VARFAC = 1.0D0 / WEIGHT( IREF )
            ENDIF
            WEIGHT( IREF ) = WEIGHT( IREF ) * VARFAC
         END DO
      ENDIF

*  Report progress so far
*  ======================

      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Display statistics derived from the input NDFs.
*  ==============================================
*  See whether the names of the input NDFs are to be displayed.
      CALL PAR_GET0L( 'LISTIN', LISTIN, STATUS )

*  If so, display their names.
      IF ( LISTIN ) THEN
         CALL CCD1_MSG( ' ', '    Input NDFs:', STATUS )
         CALL CCD1_MSG( ' ', '    ----------', STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )
         DO 6 I = 1, NNDF
            CALL MSG_SETI( 'I', I )
            CALL NDF_MSG( 'NDF', NDF( I ) )
            IF ( I .EQ. CCD1_IREF )  THEN
               IF ( NNDF .GT. NIN ) THEN
                  CALL MSG_SETC( 'NDF', ' (additional reference NDF)' )
               ELSE
                  CALL MSG_SETC( 'NDF', ' (reference NDF)' )
               END IF
            END IF
            CALL CCD1_MSG( ' ', '    ^I: ^NDF', STATUS )
 6       CONTINUE
      END IF

*  Format and display the pixel index bounds of the output mosaic.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      DO 7 IDIM = 1, NDIMX
         IF ( IDIM .NE. 1 ) CALL MSG_SETC( 'BOUNDS', ',' )
         CALL MSG_SETI( 'BOUNDS', LBNDX( IDIM ) )
         CALL MSG_SETC( 'BOUNDS', ':' )
         CALL MSG_SETI( 'BOUNDS', UBNDX( IDIM ) )
 7    CONTINUE
      CALL CCD1_MSG( ' ',
     :   '    Pixel bounds of output mosaic: (^BOUNDS)', STATUS )

*  Also format its dimension sizes, calculating the total number of
*  output pixels in the process.
      NPXOUT = 1
      DO 8 IDIM = 1, NDIMX
         IF ( IDIM .NE. 1 ) CALL MSG_SETC( 'DIMS', 'x' )
         DIMSIZ = UBNDX( IDIM ) - LBNDX( IDIM ) + 1
         CALL MSG_SETI( 'DIMS', DIMSIZ )
         NPXOUT = NPXOUT * DIMSIZ
 8    CONTINUE

*  Display the dimension sizes and total number of output pixels.
      CALL CCD1_MSG( ' ',
     :   '    Dimension size(s) of output mosaic: ^DIMS', STATUS )
      CALL MSG_SETI( 'NPXOUT', NPXOUT )
      CALL CCD1_MSG( ' ',
     :   '    Number of output pixels:            ^NPXOUT', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

      IF ( STATUS .NE. SAI__OK ) GO TO 940

*  Work out the scale factors
*  ==========================
*
*  We have a problem in that, unlike MAKEMOS, at no point during the
*  code do we have images in real pixel co-ordinate space, ie we haven't
*  done the transform using WCS AST FrameSet. We won't do this transform
*  right up until the last minute when we drizzle individual pixels onto
*  output image.
*
*  In other words, in order to calculate SCALE and ZERO values in the
*  same manner as MAKEMOS we have to call a semi-cut down version of
*  TRANNDF, create a bunch of transformed NDFs, calculate overlaps
*  (and hence SCALE and ZERO points) then delete all the NDFs keeping
*  our arrays of SCALE and ZERO.
*
*  This is why there is an option to provide the SCALE & ZERO point
*  values via a file. If the user flags SCALE and ZERO as TRUE the code
*  will look for a file containing scale and zero-point information
*  (CORRECT). If it can't finds one, then it will do it the hard way.

*  So do we want to adjust the NDFs, or not?
      ADJUST = ( ( GETS .OR. GETZ ) .AND. ( NNDF .GT. 1 ) )

      IF( ADJUST ) THEN

*  Yup we do, so look for a file containing SCALE and ZERO point
*  corrections of format (frame no., scale factor, zero point correction)
*
*  e.g.
*
*        # comment line
*        1 1.0000  0.0000
*        2 1.0172 -8.5135
*        3 0.99909 0.87089
*        4 0.97846 7.0652
*
*  Frames don't have to be in order, but weird things will happen if
*  there isn't an entry for every frame.
         CALL CCD1_ASFIO( 'CORRECT', 'READ', 'LIST', 0, FDIN, INOPN,
     :                    STATUS )
         IF( INOPN ) THEN

*  Looks like we found the file, good, the user is doing it the
*  easy way, get SCALE() and ZERO() from the file.
            CALL CCD1_GSZFF( FDIN, NIN, SCALE, ZERO, STATUS )

         ELSE

*  The user want the code to calcuate the scale and zero-point values
*  for them. This may take some time...

*  Call CCD1_TRAN (a knock off of TRANNDF) to do the transformation of
*  all the input images
            CALL CCD1_TRAN( NDF, NNDF, NIN, INGRP, CFRAME,
     :                      FRCUR, NVAR, GETS, GETZ, SCALE,
     :                      ZERO, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 940
            CALL CCD1_MSG( ' ',  ' ', STATUS )
         ENDIF

      ENDIF

*  Create the Output NDF
*  =====================

*  Get the data type for the input NDF
      IF ( CCD1_IREF .NE. 0 ) THEN
         CALL NDF_TYPE( NDF( CCD1_IREF ), 'Data', ITYPE, STATUS )
      ELSE
         CALL NDF_TYPE( NDF( 1 ), 'Data', ITYPE, STATUS )
      ENDIF

*  Get the data type for the output NDF.
      IF ( PREVD ) THEN
         OTYPE = ITYPE
      ELSE IF( ITYPE .EQ. '_DOUBLE' .OR.
     :    ITYPE .EQ. '_INTEGER' ) THEN
          OTYPE = '_DOUBLE'
      ELSE
         OTYPE = '_REAL'
      ENDIF

*  Create the output NDF by propagation from an appropriately-sized
*  section of one of the input ones.  This ensures that extensions
*  are propagated correctly.
      CALL NDF_SECT( NDF( 1 ), NDIMX, LBNDX, UBNDX, NDFX, STATUS )
      CALL NDF_PROP( NDFX, 'Units', 'OUT', OUTNDF, STATUS )
      CALL NDF_ANNUL( NDFX, STATUS )
      CALL NDF_STYPE( OTYPE, OUTNDF, 'Data', STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 940

*  Map the Data array to force it into existence.
      CALL NDF_MAP( OUTNDF, 'Data', OTYPE, 'WRITE/BAD',
     :              ODAT, NPXOUT, STATUS )

*  If we want to propagate the variance information, we need an
*  output variance array. It should be noted, that the propagated
*  variances are STATISTICALLY INCORRECT (error are correlated).
      IF ( GENVAR ) THEN
         CALL NDF_STYPE( OTYPE, OUTNDF, 'Variance', STATUS )
         CALL NDF_MAP( OUTNDF, 'Variance', OTYPE, 'WRITE/BAD',
     :                 OVAR, NPXOUT, STATUS )
      END IF

      IF ( STATUS .NE. SAI__OK ) GOTO 940

*  Setup stuff for workspace creation
      EL = 0
      DO IDIM = 1, NDIMX
         DIMSIZ = UBNDX( IDIM ) - LBNDX( IDIM ) + 1
         ODIM( IDIM ) = DIMSIZ
         IF( IDIM .EQ. 1 ) THEN
            EL = ODIM( IDIM )
         ELSE
            EL = EL*ODIM( IDIM )
         END IF
      END DO

*  Create a Weight array and map it.
      CALL CCD1_MKTMP( EL, '_DOUBLE', WDREF, STATUS )
      CALL CCD1_MPTMP( WDREF, 'WRITE', OWHT, STATUS )

*  Create a Weight array for the variances and map it if required.
      IF ( GENVAR ) THEN
         CALL CCD1_MKTMP( EL, '_DOUBLE', WVREF, STATUS )
         CALL CCD1_MPTMP( WVREF, 'WRITE', VWHT, STATUS )
      END IF

*  Create a Count array and map it, we'll use this to count the
*  number of input pixels we've drizzled onto the output image.
      CALL CCD1_MKTMP( EL, '_INTEGER', CNREF, STATUS )
      CALL CCD1_MPTMP( CNREF, 'WRITE', OCNT, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 940

*  Add a title to the new NDF.
      CALL NDF_CINP( 'TITLE', OUTNDF, 'TITLE', STATUS )

*  Propagate the WCS component to the output NDF
*  =============================================
*
*  Propagate the WCS component of either the first NDF or the reference
*  NDF if we have one, we might have an RA and DEC calibration or something
*  else we don't want to loose.

*  Get default WCS component (just GRID, PIXEL, AXIS) for the output NDF.
      CALL CCD1_GTWCS( OUTNDF, OWCS, STATUS )

*  Remove GRID, PIXEL, AXIS frames from original frameset since they're
*  out of date now.  This will leave us with just the non-automatic
*  frames.
      IF( CCD1_IREF .NE. 0 ) THEN
         CALL CCD1_GTWCS( NDF( CCD1_IREF ), IWCS, STATUS )
         CALL AST_SETI( IWCS, 'Current', CFRAME( CCD1_IREF ), STATUS )
      ELSE
         CALL CCD1_GTWCS( NDF( 1 ), IWCS, STATUS )
         CALL AST_SETI( IWCS, 'Current', CFRAME( 1 ), STATUS )
      ENDIF
      CALL CCD1_DMPRG( IWCS, 'PIXEL', .FALSE., 0, STATUS )
      CALL CCD1_DMPRG( IWCS, 'AXIS', .FALSE., 0, STATUS )
      CALL CCD1_DMPRG( IWCS, 'GRID', .FALSE., 0, STATUS )

*  Lets find out which frame contains the PIXEL domain (its going to be
*  frame 2, but we may as well do it properly) in the output WCS frameset.
      NFRM = AST_GETI( OWCS, 'Nframe', STATUS )
      DO K = 1, NFRM
         FRM = AST_GETFRAME( OWCS, K, STATUS )
         IF( AST_GETC( FRM, 'Domain', STATUS )
     :       .EQ. 'PIXEL' ) PFRAME = K
      END DO

*  And add the remaining bits of IWCS to OWCS, scale by a factor
*  of 1.0D0/MULTI to map the input WCS component onto the output.
      CALL AST_ADDFRAME( OWCS, PFRAME,
     :                   AST_ZOOMMAP( NDIMI, 1.0D0/MULTI,
     :                   ' ', STATUS ), IWCS, STATUS )

*  Write it out.
      CALL NDF_PTWCS( OWCS, OUTNDF, STATUS )

      IF ( STATUS .NE. SAI__OK ) GO TO 940

*  Drizzle images onto the output NDF
*  ==================================

      DO I = 1, NIN

*  Write out name of this NDF. And which loop this is.
         CALL CCD1_MSG( ' ',  ' ', STATUS )
         CALL NDF_MSG( 'CURRENT_NDF', NDF(I) )
         CALL CCD1_MSG( ' ', '    +++ Processing NDF: ^CURRENT_NDF',
     :                  STATUS )
         CALL MSG_SETI( 'CURRENT_NUM', I )
         CALL MSG_SETI( 'MAX_NUM', NIN )
         CALL CCD1_MSG( ' ', '    (Number ^CURRENT_NUM of ^MAX_NUM)',
     :                  STATUS )
         CALL CCD1_MSG( ' ',  ' ', STATUS )

*  Get the data type for the input NDF
         CALL NDF_TYPE( NDF( I ), 'Data', ITYPE, STATUS )

*  Test to see whether the NDF contains variance information and count
*  the number which do.
         CALL NDF_STATE( NDF( I ), 'Variance', VAR, STATUS )

         IF ( VAR ) THEN
            CALL CCD1_MSG( ' ',
     :                     '    NDF variance component: present',
     :                     STATUS )
            CALL MSG_SETL( 'GENVAR', GENVAR )
            CALL CCD1_MSG( ' ',
     :                     '    Propagating variances: ^GENVAR',
     :                     STATUS )
            IF( GETV ) THEN
               CALL MSG_SETL( 'REP_USEVAR', GETV )
               CALL CCD1_MSG( ' ', '    Using variances as '//
     :'weights: ^REP_USEVAR', STATUS )
            ENDIF
         ELSE
            CALL CCD1_MSG( ' ',
     :                     '    NDF variance component: absent',
     :                     STATUS )
         ENDIF

*  Report the extent of the input NDF
         DO IDIM = 1, NVIN
            IF ( IDIM .NE. 1 ) CALL MSG_SETC( 'INBOUND', ',' )

            CALL MSG_SETI( 'INBOUND', ILBND( IDIM ) )
            CALL MSG_SETC( 'INBOUND', ':' )
            CALL MSG_SETI( 'INBOUND', IUBND( IDIM ) )
         ENDDO
         CALL CCD1_MSG( ' ',
     :   '    Pixel bounds of input image: (^INBOUND)', STATUS )

*  The subroutine will drop the input images onto OUTNDF directly
*  one at a time, weights are accumulated in the WEIGHT array
*  located in the CCDPACK_EXT NDF extension
         CALL NDF_DIM( NDF(I), NDF__MXDIM, IDIMS, NDIMI, STATUS )

*  Determine whether the output variance array is to be updated for
*  this frame.
         GVAR = VAR .AND. GENVAR

*  Byte array
         IF ( OTYPE .EQ. '_BYTE' ) THEN
            CALL CCG1_DODIZB( NDF(I), WEIGHT(I), NPXIN(I),
     :                        ITYPE, %VAL(CNF_PVAL(ODAT)),
     :                        %VAL(CNF_PVAL(OWHT)),
     :                        %VAL(CNF_PVAL(OCNT)),
     :                        %VAL(CNF_PVAL(OVAR)),
     :                        MAPN(I),  IDIMS(1), IDIMS(2), NDIMI,
     :                        ODIM(1), ODIM(2), NVOUT, ILBND, LBNDX,
     :                        PIXFRAC, GETV, GETS, GETZ, GETM,
     :                        SCALE(I), ZERO(I), VARFAC, GVAR,
     :                        %VAL(CNF_PVAL(VWHT)), STATUS )

*  Double-precision array
         ELSE IF ( OTYPE .EQ. '_DOUBLE' ) THEN
            CALL CCG1_DODIZD( NDF(I), WEIGHT(I), NPXIN(I),
     :                        ITYPE, %VAL(CNF_PVAL(ODAT)),
     :                        %VAL(CNF_PVAL(OWHT)),
     :                        %VAL(CNF_PVAL(OCNT)),
     :                        %VAL(CNF_PVAL(OVAR)),
     :                        MAPN(I),  IDIMS(1), IDIMS(2), NDIMI,
     :                        ODIM(1), ODIM(2), NVOUT, ILBND, LBNDX,
     :                        PIXFRAC, GETV, GETS, GETZ, GETM,
     :                        SCALE(I), ZERO(I), VARFAC, GVAR,
     :                        %VAL(CNF_PVAL(VWHT)), STATUS )

*  Integer array
         ELSE IF ( OTYPE .EQ. '_INTEGER' ) THEN
            CALL CCG1_DODIZI( NDF(I), WEIGHT(I), NPXIN(I),
     :                        ITYPE, %VAL(CNF_PVAL(ODAT)),
     :                        %VAL(CNF_PVAL(OWHT)),
     :                        %VAL(CNF_PVAL(OCNT)),
     :                        %VAL(CNF_PVAL(OVAR)),
     :                        MAPN(I),  IDIMS(1), IDIMS(2), NDIMI,
     :                        ODIM(1), ODIM(2), NVOUT, ILBND, LBNDX,
     :                        PIXFRAC, GETV, GETS, GETZ, GETM,
     :                        SCALE(I), ZERO(I), VARFAC, GVAR,
     :                        %VAL(CNF_PVAL(VWHT)), STATUS )

*  Integer array
         ELSE IF ( OTYPE .EQ. '_INT64' ) THEN
            CALL CCG1_DODIZK( NDF(I), WEIGHT(I), NPXIN(I),
     :                        ITYPE, %VAL(CNF_PVAL(ODAT)),
     :                        %VAL(CNF_PVAL(OWHT)),
     :                        %VAL(CNF_PVAL(OCNT)),
     :                        %VAL(CNF_PVAL(OVAR)),
     :                        MAPN(I),  IDIMS(1), IDIMS(2), NDIMI,
     :                        ODIM(1), ODIM(2), NVOUT, ILBND, LBNDX,
     :                        PIXFRAC, GETV, GETS, GETZ, GETM,
     :                        SCALE(I), ZERO(I), VARFAC, GVAR,
     :                        %VAL(CNF_PVAL(VWHT)), STATUS )

*  Single-precision array
         ELSE IF ( OTYPE .EQ. '_REAL' ) THEN
            CALL CCG1_DODIZR( NDF(I), WEIGHT(I), NPXIN(I),
     :                        ITYPE, %VAL(CNF_PVAL(ODAT)),
     :                        %VAL(CNF_PVAL(OWHT)),
     :                        %VAL(CNF_PVAL(OCNT)),
     :                        %VAL(CNF_PVAL(OVAR)),
     :                        MAPN(I),  IDIMS(1), IDIMS(2), NDIMI,
     :                        ODIM(1), ODIM(2), NVOUT, ILBND, LBNDX,
     :                        PIXFRAC, GETV, GETS, GETZ, GETM,
     :                        SCALE(I), ZERO(I), VARFAC, GVAR,
     :                        %VAL(CNF_PVAL(VWHT)), STATUS )

*  Unsigned-byte array
         ELSE IF ( OTYPE .EQ. '_UBYTE' ) THEN
            CALL CCG1_DODIZUB( NDF(I), WEIGHT(I), NPXIN(I),
     :                        ITYPE, %VAL(CNF_PVAL(ODAT)),
     :                        %VAL(CNF_PVAL(OWHT)),
     :                        %VAL(CNF_PVAL(OCNT)),
     :                        %VAL(CNF_PVAL(OVAR)),
     :                        MAPN(I),  IDIMS(1), IDIMS(2), NDIMI,
     :                        ODIM(1), ODIM(2), NVOUT, ILBND, LBNDX,
     :                        PIXFRAC, GETV, GETS, GETZ, GETM,
     :                        SCALE(I), ZERO(I), VARFAC, GVAR,
     :                        %VAL(CNF_PVAL(VWHT)), STATUS )


*  Unsigned-word array
         ELSE IF ( OTYPE .EQ. '_UWORD' ) THEN
            CALL CCG1_DODIZUW( NDF(I), WEIGHT(I), NPXIN(I),
     :                        ITYPE, %VAL(CNF_PVAL(ODAT)),
     :                        %VAL(CNF_PVAL(OWHT)),
     :                        %VAL(CNF_PVAL(OCNT)),
     :                        %VAL(CNF_PVAL(OVAR)),
     :                        MAPN(I),  IDIMS(1), IDIMS(2), NDIMI,
     :                        ODIM(1), ODIM(2), NVOUT, ILBND, LBNDX,
     :                        PIXFRAC, GETV, GETS, GETZ, GETM,
     :                        SCALE(I), ZERO(I), VARFAC, GVAR,
     :                        %VAL(CNF_PVAL(VWHT)), STATUS )

*  Word array
         ELSE IF ( OTYPE .EQ. '_WORD' ) THEN
            CALL CCG1_DODIZW( NDF(I), WEIGHT(I), NPXIN(I),
     :                        ITYPE, %VAL(CNF_PVAL(ODAT)),
     :                        %VAL(CNF_PVAL(OWHT)),
     :                        %VAL(CNF_PVAL(OCNT)),
     :                        %VAL(CNF_PVAL(OVAR)),
     :                        MAPN(I),  IDIMS(1), IDIMS(2), NDIMI,
     :                        ODIM(1), ODIM(2), NVOUT, ILBND, LBNDX,
     :                        PIXFRAC, GETV, GETS, GETZ, GETM,
     :                        SCALE(I), ZERO(I), VARFAC, GVAR,
     :                        %VAL(CNF_PVAL(VWHT)), STATUS )
         END IF

         IF ( STATUS .NE. SAI__OK ) GOTO 940

      END DO

*  Display information about the output mosaic.
*  ===========================================

      CALL CCD1_MSG( ' ',  ' ', STATUS )
      CALL CCD1_MSG( ' ', '    Generated output mosaic:', STATUS )
      CALL CCD1_MSG( ' ', '    -----------------------', STATUS )
      CALL CCD1_MSG( ' ',  ' ', STATUS )
      CALL NDF_MSG( 'NDF', OUTNDF )
      CALL CCD1_MSG( ' ',
     :               '    Written to NDF structure: ^NDF', STATUS )

*  Format and display the pixel index bounds of the output mosaic.
      DO  IDIM = 1, NDIMX
         IF ( IDIM .NE. 1 ) CALL MSG_SETC( 'BOUNDS', ',' )
         CALL MSG_SETI( 'BOUNDS', LBNDX( IDIM ) )
         CALL MSG_SETC( 'BOUNDS', ':' )
         CALL MSG_SETI( 'BOUNDS', UBNDX( IDIM ) )
      END DO
      CALL CCD1_MSG( ' ',
     :   '    Pixel bounds of output mosaic: (^BOUNDS)', STATUS )
      CALL MSG_SETC( 'INPUT_TYPE', ITYPE )
      CALL CCD1_MSG( ' ',
     :   '    Input frame(s) data type: ^INPUT_TYPE', STATUS )
      CALL MSG_SETC( 'OUTPUT_TYPE', OTYPE )
      CALL CCD1_MSG( ' ',
     :   '    Output mosaic data type: ^OUTPUT_TYPE', STATUS )
      CALL MSG_SETL( 'IN_WHT', GETV )
      CALL CCD1_MSG( ' ', '    Weighted by inverse variance: '
     :             //' ^IN_WHT', STATUS )
      CALL MSG_SETL( 'IN_MAP', GETM )
      CALL CCD1_MSG( ' ', '    Weighted variance maps: '
     :             //' ^IN_MAP', STATUS )
      CALL MSG_SETL( 'GENVAR', GENVAR )
      CALL CCD1_MSG( ' ', '    Wrote output variance: '
     :             //' ^GENVAR', STATUS )
      CALL MSG_SETL( 'IN_ZERO', GETZ )
      CALL CCD1_MSG( ' ', '    Carried out zero-point correction: '
     :             //' ^IN_ZERO', STATUS )
      CALL MSG_SETL( 'IN_SCALE', GETS )
      CALL CCD1_MSG( ' ', '    Carried out scaling: '
     :             //' ^IN_SCALE', STATUS )

*  Clean up - Arrive here if an error occurs
*  =========================================

940   CONTINUE

      CALL CCD1_MFREE( -1, STATUS )
      CALL CCD1_FRTMP( -1, STATUS )

960   CONTINUE

*  Tidy up any remaining AST FrameSets
      CALL AST_ANNUL( MAPC, STATUS )
      CALL AST_ANNUL( MAPZ, STATUS )

      DO I = 1, NIN
         CALL AST_ANNUL( FRCUR( I ), STATUS )
         CALL AST_ANNUL( MAPN( I ), STATUS )
      END DO

      CALL AST_ANNUL( IWCS, STATUS )

      CALL AST_END( STATUS )

999   CONTINUE

*  Release NDFs and close container files
      CALL NDF_ANNUL( OUTNDF, STATUS )
      DO I = 1, NNDF
         CALL NDF_UNMAP( NDF( I ), '*', STATUS )
         CALL NDF_ANNUL( NDF( I ), STATUS )
      END DO

      CALL NDF_END( STATUS )

*  Annul the input NDF group.
      CALL CCD1_GRDEL( INGRP, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'DRIZZLE_ERR',
     :   'DRIZZLE: Error drizzling a mosaic from input NDFs.',
     :      STATUS )
      END IF

*  Close the logging system.
      CALL CCD1_END( STATUS )

*  Time at the bar please...
      END

* $Id$
