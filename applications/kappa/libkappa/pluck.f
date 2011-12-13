      SUBROUTINE PLUCK ( STATUS )
*+
*  Name:
*     PLUCK

*  Purpose:
*     Plucks slices from an NDF at arbitrary positions.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PLUCK( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application's function is to extract data at scientifically
*     relevant points such as the spatial location of a source or
*     wavelength of a spectral feature, rather than at data sampling
*     points (for which NDFCOPY is appropriate).  This is achieved by
*     the extraction of interpolated slices from an NDF.  A slice is
*     located at a supplied set of co-ordinates in the current WCS
*     Frame for some but not all axes, and it possesses one fewer
*     significant dimension per supplied co-ordinate.  The slices run
*     parallel to pixel axes of the NDF.

*     The interpolation uses one of a selection of resampling methods
*     to effect the non-integer shifts along the fixed axes, applied to
*     each output element along the retained axes (see the METHOD,
*     PARAMS, and TOL parameters).
*
*     Three routes are available for obtaining the fixed positions,
*     selected using parameter MODE:
*
*     - from the parameter system (see parameter POS);
*
*     - from a specified positions list (see parameter INCAT); or
*
*     - from a simple text file containing a list of co-ordinates (see
*     parameter COIN).
*
*     In the first mode the application loops, asking for new extraction
*     co-ordinates until it is told to quit or encounters an error.
*     However there is no looping if the position has been supplied on
*     the command line.
*
*     Each extracted dataset is written to a new NDF, which however, may
*     reside in a single container file (see the CONTAINER parameter).

*  Usage:
*     pluck in axes out method [mode] { pos
*                                     { coin=?
*                                     { incat=?
*                                    mode

*  ADAM Parameters:
*     AXES( ) = _INTEGER (Read)
*        The WCS axis or axes to remain in the output NDF.   The slice
*        will therefore contain an array comprising all the elements
*        along these axes.  The maximum number of axes is one fewer
*        than the number of WCS axes in the NDF.
*
*        Each axis can be specified using one of the following options.
*
*        - Its integer index within the current Frame of the input
*        NDF (in the range 1 to the number of axes in the current
*        Frame).
*        - Its symbol string such as "RA" or "VRAD".
*        - A generic option where "SPEC" requests the spectral axis,
*        "TIME" selects the time axis, "SKYLON" and "SKYLAT" picks the
*        sky longitude and latitude axes respectively.  Only those axis
*        domains present are available as options.
*
*        A list of acceptable values is displayed if an illegal value is
*        supplied.  If the axes of the current Frame are not parallel to
*        the NDF pixel axes, then the pixel axis which is most nearly
*        parallel to the specified current Frame axis will be used.
*     COIN = FILENAME (Read)
*        Name of a text file containing the co-ordinates of slices to
*        be plucked.  It is only accessed if parameter MODE is given the
*        value "File".  Each line should contain the formatted axis
*        values for a single position, in the current Frame of the NDF.
*        Axis values can be separated by spaces, tabs or commas.  The
*        file may contain comment lines with the first character # or !.
*     CONTAINER = _LOGICAL (Read)
*        If TRUE, each slice extracted is written as an NDF component of
*        the HDS container file specified by the OUT parameter.  The nth
*        component will be named PLUCK_n.  If set FALSE, each extraction
*        is written to a separate file.  On-the-fly format conversion to
*        foreign formats is not possible when CONTAINER=TRUE. [FALSE]
*     DESCRIBE = _LOGICAL (Read)
*        If TRUE, a detailed description of the co-ordinate Frame in
*        which the fixed co-ordinates are to be supplied is displayed
*        before the positions themselves.  It is ignored if
*        MODE="Catalogue".  [current value]
*     INCAT = FILENAME (Read)
*        A catalogue containing a positions list giving the co-ordinates
*        of the fixed positions, such as produced by applications
*        CURSOR, LISTMAKE, etc.  It is only accessed if parameter MODE
*        is given the value "Catalogue".  The catalogue should have a
*        WCS Frame common with the NDF, so that the NDF and catalogue
*        FrameSets can be aligned.
*     MODE = LITERAL (Read)
*        The mode in which the initial co-ordinates are to be obtained.
*        The supplied string can be one of the following values.
*
*        - "Interface" -- positions are obtained using parameter POS.
*
*        - "Catalogue" -- positions are obtained from a positions list
*                         using parameter INCAT.
*
*        - "File"      -- positions are obtained from a text file using
*                         parameter COIN.
*
*        [current value]
*     IN = NDF (Read)
*        The NDF structure containing the data to be extracted.  It
*        must have at least two dimensions.
*     METHOD = LITERAL (Read)
*        The method to use when sampling the input pixel values.  For
*        details of these schemes, see the descriptions of routine
*        AST_RESAMPLEx in SUN/210. METHOD can take the following values.
*
*        - "Linear" -- When resampling, the output pixel values are
*        calculated by linear interpolation in the input NDF among the
*        two nearest pixel values along each axis chosen by AXES.  This
*        method produces smoother output NDFs than the
*        nearest-neighbour scheme, but is marginally slower.
*
*        - "Sinc" -- Uses the sinc(pi*x) kernel, where x is the pixel
*        offset from the interpolation point, and sinc(z)=sin(z)/z.  Use
*        of this scheme is not recommended.
*
*        - "SincSinc" -- Uses the sinc(pi*x)sinc(k*pi*x) kernel. A
*        valuable general-purpose scheme, intermediate in its visual
*        effect on NDFs between the linear option and using the
*        nearest neighbour.
*
*        - "SincCos" -- Uses the sinc(pi*x)cos(k*pi*x) kernel.  Gives
*        similar results to the "SincSinc" scheme.
*
*        - "SincGauss" -- Uses the sinc(pi*x)exp(-k*x*x) kernel.  Good
*        results can be obtained by matching the FWHM of the
*        envelope function to the point-spread function of the
*        input data (see parameter PARAMS).
*
*        - "Somb" -- Uses the somb(pi*x) kernel, where x is the pixel
*        offset from the interpolation point, and somb(z)=2*J1(z)/z (J1
*        is the first-order Bessel function of the first kind.  This
*        scheme is similar to the "Sinc" scheme.
*
*        - "SombCos" -- Uses the somb(pi*x)cos(k*pi*x) kernel.  This
*        scheme is similar to the "SincCos" scheme.
*
*        - "BlockAve"  -- Block averaging over all pixels in the
*        surrounding N-dimensional cube.
*
*        All methods propagate variances from input to output, but the
*        variance estimates produced by interpolation schemes need to be
*        treated with care since the spatial smoothing produced by these
*        methods introduces correlations in the variance estimates.  The
*        initial default is "SincSinc".  [current value]
*     OUT = NDF (Write)
*        The name for the output NDF, or the name of the single
*        container file if CONTAINER=TRUE.
*     PARAMS( 2 ) = _DOUBLE (Read)
*        An optional array which consists of additional parameters
*        required by the Sinc, SincSinc, SincCos, SincGauss, Somb,
*        SombCos, and Gauss methods.
*
*        PARAMS(1) is required by all the above schemes.  It is used to
*        specify how many pixels are to contribute to the interpolated
*        result on either side of the interpolation in each dimension.
*        Typically, a value of 2 is appropriate and the minimum allowed
*        value is 1 (i.e. one pixel on each side).  A value of zero or
*        fewer indicates that a suitable number of pixels should be
*        calculated automatically.  [0]
*
*        PARAMS(2) is required only by the SombCos, Gauss, SincSinc,
*        SincCos, and SincGauss schemes.  For the SombCos, SincSinc, and
*        SincCos schemes, it specifies the number of pixels at which the
*        envelope of the function goes to zero.  The minimum value is
*        1.0, and the run-time default value is 2.0.  For the Gauss and
*        SincGauss scheme, it specifies the full-width at half-maximum
*        (FWHM) of the Gaussian envelope.  The minimum value is 0.1, and
*        the run-time default is 1.0.  On astronomical images and
*        spectra, good results are often obtained by approximately
*        matching the FWHM of the envelope function, given by PARAMS(2),
*        to the point-spread function of the input data.  []
*     POS( ) = LITERAL (Read)
*        An the co-ordinates of the next slice to be extracted, in the
*        current co-ordinate Frame of the NDF (supplying a colon ":"
*        will display details of the current co-ordinate Frame).  The
*        position should be supplied as a list of formatted axis values
*        separated by spaces or commas.  POS is only accessed if
*        parameter MODE is given the value "Interface".  If the
*        co-ordinates are supplied on the command line only one
*        slice will be extracted; otherwise the application will ask
*        for further positions which may be terminated by supplying the
*        null value (!).
*     TITLE = LITERAL (Read)
*        A Title for every output NDF structure.  A null value (!)
*        propagates the title from the input NDF to all output NDFs.
*        [!]
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

*  Examples:
*     pluck omc1 pos="5:35:13.7,-5:22:13.6" axes=FREQ
*           method=sincgauss params=[3,5] out=omc1_trap
*        The NDF omc1 is a spectral-imaging cube with
*        (Right ascension, declination, frequency) World Co-ordinate
*        axes.  This example extracts a spectrum at RA=5h35m13.7s,
*        Dec=-5d22m13.6 using the SincGauss interpolation method.
*        Three pixels either side of the point are used to interpolate,
*        the full-width half-maximum of the Gaussian is five pixels.
*        The resultant spectrum called omc1_trap, is still a cube, but
*        its spatial dimensions each only have one element.
*     pluck omc1 mode=cat incat=a axes=FREQ container out=omc1_spectra
*        This example reads the fixed positions from the
*        positions list in file a.FIT.  The selected spectra are stored
*        in an HDS container file called omc1_spectra.sdf.
*     pluck omc1 mode=cat incat=a axes=SPEC container out=omc1_spectra
*        As the previous example, plucking spectra, this time by
*        selecting the generic spectral axis.
*     pluck omc1 pos=3.45732E11 axes="RA,Dec" method=lin out=peakplane
*        This example extracts a plane from omc1 at frequency
*        3.45732E11 Hz using linear interpolation and stores it in NDF
*        peakplane.

*  Notes:
*     -  In Interface or File modes all positions should be supplied in
*     the current co-ordinate Frame of the NDF.  A description of the
*     co-ordinate Frame being used is given if parameter DESCRIBE is set
*     to a TRUE value.  Application WCSFRAME can be used to change the
*     current co-ordinate Frame of the NDF before running this
*     application if required.
*     -  The output NDF has the same dimensionality as the input NDF,
*     although the axes with fixed co-ordinates (those not specified
*     by the AXES parameter) are degenerate, having bounds of 1:1.
*     The retention of these insignificant axes enables the
*     co-ordinates of where the slice originated to be recorded.
*     Such fixed co-ordinates may be examined with say NDFTRACE.
*     NDFCOPY may be used to trim the degenerate axes if their presence
*     prevents some old non-KAPPA tasks from operating.
*     -  In Catalogue or File modes the table file need only contain
*     columns supplying the fixed positions.  In this case the
*     co-ordinates along the retained axes are deemed to be independent,
*     that is they do not affect the shifts required of the other axes.
*     In practice this assumption only affects File mode, as catalogues
*     made with CURSOR or LISTMAKE will contain WCS information.
*
*     In Interface mode representaive co-ordinates along retained axes
*     are the midpoints of the bounds of an array that would contain the
*     resampled copy of the whole input array.

*  Related Applications:
*     KAPPA: NDFCOPY, REGRID.

*  Implementation Status:
*     -  The LABEL, UNITS, and HISTORY components, and all extensions
*     are propagated.  TITLE is controlled by the TITLE parameter.
*     DATA, VARIANCE, AXIS, and WCS are propagated after appropriate
*     modification.  The QUALITY component is not propagated.
*     -  The processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     -  The minimum number of dimensions in the input NDF is two.
*     -  Processing a group of input NDFs is not supported unless
*     CONTAINER = TRUE or when only one output NDF is created per
*     input file.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 July 13 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT  NONE           ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! SSE global definitions
      INCLUDE 'AST_PAR'        ! AST constants and functions
      INCLUDE 'NDF_PAR'        ! NDF definitions
      INCLUDE 'PAR_PAR'        ! PAR constants
      INCLUDE 'PAR_ERR'        ! Parameter-system errors
      INCLUDE 'CNF_PAR'        ! For CNF_PVAL function

*  Status:
      INTEGER  STATUS

*  Local Variables:
      INTEGER AXES( NDF__MXDIM ) ! Axes to retain
      LOGICAL CAT                ! Catalogue mode was selected
      INTEGER CFRM               ! Pointer to the Current Frame of NDF
      LOGICAL CONTNR             ! Output NDFs to be written as
                                 ! components of an HDS container file?
      LOGICAL DESC               ! Describe the current Frame?
      INTEGER DIMS( NDF__MXDIM ) ! Dimensions of NDF
      LOGICAL FILE               ! File mode was selected
      INTEGER I                  ! Loop counter
      INTEGER IGRID              ! Index of GRID Frame in IWCS
      LOGICAL INTERF             ! Interface mode was selected
      INTEGER INTERP             ! Interpolation method identifier
      INTEGER IPID               ! Pointer to array of pos'n identifiers
      INTEGER IPIN               ! Pointer to array of supplied positions
      INTEGER IPW1               ! Pointer to work space
      INTEGER IWCS               ! WCS FrameSet from input NDF
      INTEGER IWCSG              ! FrameSet read from input catalogue
      INTEGER J                  ! Loop counter
      INTEGER MAP1               ! Mapping from GRID Frame to Current
                                 ! Frame
      INTEGER MAP2               ! Mapping from supplied Frame to
                                 ! Current Frame
      INTEGER MAP3               ! Mapping from supplied Frame to GRID
                                 ! Frame
      CHARACTER*16 METHOD        ! Name of resampling scheme
      CHARACTER*10 MODE          ! Mode for getting co-ordinates
      INTEGER NAXC               ! No. of axes in current NDF Frame
      INTEGER NAXREM             ! Number of axes remaining
      INTEGER NAXIN              ! No. of axes in supplied Frame
      INTEGER NDFI               ! Input NDF identifier
      INTEGER NDIM               ! Actual number of dimensions of NDF
      INTEGER NDIMS              ! No. of significant dimensions of NDF
      INTEGER NPARAM             ! Number of interpolation parameters
      INTEGER NPOS               ! No. of supplied positions
      INTEGER OTOMAP             ! One-to-one mapping
      CHARACTER*( PAR__SZNAM ) PARAMS( 3 ) ! Parameter names for s/r
      LOGICAL QUIET              ! Suppress screen output?
      LOGICAL RETAX( NDF__MXDIM ) ! Retained axes?
      DOUBLE PRECISION RSPARS( 2 ) ! Interpolation parameters
      INTEGER PAXIS( NDF__MXDIM - 1 ) ! Pixel axes to retain
      DOUBLE PRECISION PXHIGH    ! GRID upper bound of retained axis
      DOUBLE PRECISION PXLOW     ! GRID loweer bound of retained axis
      INTEGER SDIM( NDF__MXDIM ) ! Significant dimensions of the NDF
      INTEGER SLBND( NDF__MXDIM )! Significant lower bounds of the image
      INTEGER SUBND( NDF__MXDIM )! Significant upper bounds of the image
      CHARACTER*80 TITLE         ! Title from the catalogue
      DOUBLE PRECISION TOL       ! Tolerance for linear transform
                                 ! approximation

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      FILE = .FALSE.
      NPOS = 0

*  Initialise pointers for valgrind.
      IPW1 = 0
      IPID = 0

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Open the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Abort if an error occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Mode for accessing co-ordinates
*  ===============================

*  Find where the initial guess positions are to be obtained from.
      CALL PAR_CHOIC( 'MODE', 'Interface', 'Interface,Catalogue,'//
     :                'File', .TRUE., MODE, STATUS )

*  Set convenience flags for the various values of MODE.
      CAT = MODE .EQ. 'CATALOGUE'
      FILE = MODE .EQ. 'FILE'
      INTERF = MODE .EQ. 'INTERFACE'

*  Abort if an error occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  No initialization needed for "File" mode.  We cannot read the
*  contents of a file yet, because we do not yet have an NDF and so do
*  not know how many columns the file must contain.
      IF ( FILE ) THEN

*  In "Catalogue" mode, open a positions list catalogue and read its
*  contents.  A pointer to a FrameSet is returned, together with
*  pointers to positions and identifiers, and a title.  The positions
*  are returned in the Base Frame of this FrameSet.
      ELSE IF ( CAT ) THEN
         IWCSG = AST__NULL
         CALL KPG1_RDLST( 'INCAT', .FALSE., IWCSG, NPOS, NAXIN, IPIN,
     :                    IPID, TITLE, ' ', STATUS )

*  No initialisation needed in "Interface" mode.
      ELSE IF ( INTERF ) THEN

      END IF

      QUIET = .TRUE.

*  WCS information
*  ===============

*  We need to know how many significant axes there are (i.e. pixel axes
*  spanning more than a single pixel), so count them.  We ignore
*  insignificant axes since the user will probably not be interested in
*  them (and the interpolating routines cannot handle axes spanning only
*  a single pixel).
      CALL NDF_DIM( NDFI, NDF__MXDIM, DIMS, NDIM, STATUS )
      NDIMS = 0
      DO I = 1, NDIM
         IF ( DIMS( I ) .GT. 1 ) NDIMS = NDIMS + 1
      END DO

*  Now get the WCS FrameSet from the NDF.
      CALL KPG1_ASGET( NDFI, NDIMS, .TRUE., .FALSE., .FALSE., SDIM,
     :                 SLBND, SUBND, IWCS, STATUS )

*  Get a pointer to the Current Frame in the NDF.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Save the number of Current Frame axes.
      NAXC = AST_GETI( CFRM, 'NAXES', STATUS )

*  Get the Mapping from GRID to Current Frame in the NDF.  First find
*  the index of the GRID Frame, and then get the Mapping.
      CALL KPG1_ASFFR( IWCS, 'GRID', IGRID, STATUS )
      MAP1 = AST_SIMPLIFY( AST_GETMAPPING( IWCS, IGRID, AST__CURRENT,
     :                                     STATUS ), STATUS )

*  In catalogue mode, the positions are supplied in the Base Frame of
*  the FrameSet stored in the catalogue.  Merge this FrameSet with the
*  FrameSet read from the NDF aligning them in some suitable Frame.
      IF ( CAT ) THEN
         CALL KPG1_ASMRG( IWCSG, IWCS, ' ', QUIET, 0, STATUS )

*  Get the Mapping.
         MAP2 = AST_SIMPLIFY( AST_GETMAPPING( IWCSG, AST__BASE,
     :                                        AST__CURRENT, STATUS ),
     :                        STATUS )

*  In the other modes, the positions are supplied in the Current Frame.
      ELSE
         MAP2 = AST_UNITMAP( NAXC, ' ', STATUS )
      END IF

*  Save the number of axes in the Frame in which the positions are
*  supplied.
      NAXIN = AST_GETI( MAP2, 'NIN', STATUS )

*  We need the Mapping from the Frame in which the positions are
*  supplied, to the GRID Frame of the NDF.  We get this Mapping by
*  concatenating the Mapping from input Frame to Current Frame, with
*  the Mapping from Current Frame to GRID Frame (obtained by
*  temporarily inverting the Mapping from GRID to Current Frame).
      CALL AST_INVERT( MAP1, STATUS )
      MAP3 = AST_SIMPLIFY( AST_CMPMAP( MAP2, MAP1, .TRUE., ' ',
     :                                 STATUS ), STATUS )
      CALL AST_INVERT( MAP1, STATUS )

*  See if a description of the NDFs current Frame is required.
      IF ( .NOT. CAT ) THEN
         CALL PAR_GET0L( 'DESCRIBE', DESC, STATUS )

*  If so, give a detailed description of the Frame in which positions
*  will be reported if required.
         IF ( DESC ) THEN
            CALL KPG1_DSFRM( CFRM, 'Positions should be supplied in '/
     :                       /'the following co-ordinate Frame:',
     :                       AST__BAD, AST__BAD, .TRUE., STATUS )
         END IF
      END IF

*  If we are in "File" mode, obtain the file and read the positions,
*  interpreting them as positions within the Current Frame of the NDF.
*  A pointer to memory holding the positions is returned.  Store a safe
*  value for the IPID pointer.  Identifiers are generated automatically
*  in File mode instead of being read from the file, and so we do not
*  have a pointer to an array of identifiers at this point.
      IF ( FILE ) THEN
         CALL KPG1_ASFIL( 'COIN', ' ', CFRM, NPOS, IPIN, ' ', STATUS )
         IPID = IPIN
      END IF

*  See if a description of the NDFs current Frame is required.
      CALL PAR_GET0L( 'CONTAINER', CONTNR, STATUS )

*  Select the axes to retain.
*  ==========================

*  Obtain the indices of the WCS axes to be retained in the output NDF.
*  At least one axis must be removed.
      DO I = 1, NAXC - 1
         AXES( 1 ) = 0
      END DO
      CALL KPG1_GTAXM( 'AXES', CFRM, NAXC - 1, AXES, NAXREM, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Find the corresponding pixel axes.
      DO I = 1, NAXREM
         CALL KPG1_ASAPA( NDFI, CFRM, MAP1, AXES( I ), AST__BAD,
     :                    AST__BAD, PAXIS( I ), PXLOW, PXHIGH, OTOMAP,
     :                    STATUS )
      END DO

*  Set flags indicating the retained axes.
      DO I = 1, NAXC
         RETAX( I ) = .FALSE.
         DO J = 1, NAXREM
            RETAX( I ) = RETAX( I ) .OR. I .EQ. PAXIS( J )
         END DO
      END DO

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get the qualifications to the transformation.
*  =============================================

*  Get the method for calculating the output array value from the
*  input values.
      CALL PAR_CHOIC( 'METHOD', 'SincSinc', 'Linear,Sinc,SincSinc,'//
     :                'SincCos,SincGauss,BlockAve,Somb,SombCos', .TRUE.,
     :                METHOD, STATUS )

      IF ( STATUS .NE. SAI__OK ) GO TO 999
      IF ( METHOD .EQ. 'LINEAR' ) THEN
         CALL MSG_SETC( 'M', 'Linear' )
         INTERP = AST__LINEAR
         NPARAM = 0

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

*  Get an additional parameter vector if required.
      IF ( NPARAM .GT. 0 ) THEN
         DO I = 1, 2
            RSPARS( I ) = AST__BAD
         END DO
         CALL PAR_EXACD( 'PARAMS', NPARAM, RSPARS, STATUS )
      END IF

*  Get the tolerance for Mapping linear approximation.
      CALL PAR_GET0D( 'TOL', TOL, STATUS )

      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Process all the supplied positions together as a single batch in
*  non-interactive modes.
      IF ( CAT .OR. FILE ) THEN

*  Allocate work arrays.
         CALL PSX_CALLOC( NPOS * NDIMS, '_DOUBLE', IPW1, STATUS )

*  Pluck all slices at the co-ordinates given in the catalogue or
*  file.
         PARAMS( 1 ) = 'OUT'
         PARAMS( 2 ) = 'TITLE'
         CALL KPS1_PLFIL( NDFI, IWCS, MAP3, NPOS, NAXIN,
     :                    %VAL( CNF_PVAL( IPIN ) ), CAT,
     :                    %VAL( CNF_PVAL( IPID ) ), PARAMS, CONTNR,
     :                    NAXC, RETAX, INTERP, RSPARS, TOL, NDIMS,
     :                    %VAL( CNF_PVAL( IPW1 ) ), STATUS )

*  Free the work arrays.
         CALL PSX_FREE( IPW1, STATUS )

*  Specify the the co-ordinates of each slice to pluck interactively.
*  Also accesses OUT and TITLE parameters.
      ELSE
         PARAMS( 1 ) = 'POS'
         PARAMS( 2 ) = 'OUT'
         PARAMS( 3 ) = 'TITLE'
         CALL KPS1_PLINT( NDFI, IWCS, MAP3, PARAMS, CONTNR, NAXC,
     :                    RETAX, INTERP, RSPARS, TOL, STATUS )

      END IF

*  Tidy up.
*  ========
 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Release the dynamic arrays holding the input positions and
*  identifiers in catalogue mode.
      IF ( CAT ) THEN
         CALL PSX_FREE( IPID, STATUS )
         CALL PSX_FREE( IPIN, STATUS )

*  Release the dynamic arrays holding the input positions and
*  identifiers in file mode.
      ELSE IF ( FILE ) THEN
         CALL PSX_FREE( IPIN, STATUS )

      END IF

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a contextual error message if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PLUCK_ERR', 'PLUCK: Failed to extract '//
     :                 'the slice.', STATUS )
      END IF

      END
