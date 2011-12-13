       SUBROUTINE FFCLEAN( STATUS )
*+
*  Name:
*     FFCLEAN

*  Purpose:
*     Removes defects from a substantially flat one-, two- or
*     three-dimensional NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FFCLEAN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application cleans a one- or two-dimensional NDF by removing
*     defects smaller than a specified size.  In addition,
*     three-dimensional NDFs can be cleaned by processing each row or
*     plane within it using the one- or two-dimensional algorithm (see
*     parameter AXES).
*
*     The defects are flagged with the bad value.  The defects are found
*     by looking for pixels that deviate from the data's smoothed
*     version by more than an arbitrary number of standard deviations
*     from the local mean, and that lie within a specified range of
*     values.  Therefore, the spectrum or image must be substantially
*     flat.  The data variances provide the local-noise estimate for the
*     threshold, but if these are not available a variance for the
*     whole of the spectrum or image is derived from the mean squared
*     deviations of the original and smoothed versions.  The smoothed
*     version of the image is obtained by block averaging over a
*     rectangular box.  An iterative process progressively removes the
*     outliers from the image.

*  Usage:
*     ffclean in out clip box [thresh] [wlim]

*  ADAM Parameters:
*     AXES( 2 ) = _INTEGER (Read)
*        The indices of up to two axes that span the rows or planes
*        that are to be cleaned.  If only one value is supplied, then
*        the NDF is processed as a set of one-dimensional spectra
*        parallel to the specified pixel axis.  If two values are
*        supplied, then the NDF is processed as a set of two-dimensional
*        images spanned by the given axes.  Thus, a two-dimensional NDF
*        can be processed either as a single two-dimensional image or as
*        a set of one-dimensional spectra.  Likewise, a
*        three-dimensional NDF can be processed either as a set of
*        two-dimensional images or a set of one-dimensional spectra.  By
*        default, a two-dimensional NDF is processed as a single
*        two-dimensional image, and a three-dimensional NDF is
*        processed as a set of one-dimensional spectra (the spectral
*        axis is chosen by examining the WCS component---pixel-axis 1 is
*        used if the current WCS frame does not contain a spectral
*        axis).  []
*     BOX( 2 ) = _INTEGER (Read)
*        The x and y sizes (in pixels) of the rectangular box to be
*        applied to smooth the image.  If only a single value is given,
*        then it will be duplicated so that a square filter is used
*        except where the image is one-dimensional for which the box
*        size along the insignificant dimension is set to 1.  The values
*        given will be rounded up to positive odd integers if
*        necessary.
*     CLIP( ) = _REAL (Read)
*        The number of standard deviations for the rejection threshold
*        of each iteration.  Pixels that deviate from their counterpart
*        in the smoothed image by more than CLIP times the noise are
*        made bad.  The number of values given specifies the number of
*        iterations.  Values should lie in the range 0.5--100.  Up to
*        one hundred values may be given.  [3.0, 3.0, 3.0]
*     GENVAR = _LOGICAL (Read)
*        If TRUE, the noise level implied by the deviations from the
*        local mean over the supplied box size are stored in the output
*        VARIANCE component.  This noise level has a constant value over
*        the whole NDF (or over each section of the NDF if the NDF is
*        being processed in sections---see Parameter AXES).  This
*        constant noise level is also displayed on the screen if
*        the current message-reporting level is at least NORMAL.  If
*        GENVAR is FALSE, then the output variances will be copied from
*        the input variances (if the input NDF has no variances, then
*        the output NDF will not have any variances either).  [FALSE]
*     IN = NDF (Read)
*        The one- or two-dimensional NDF containing the input image to
*        be cleaned.
*     OUT = NDF (Write)
*        The NDF to contain the cleaned image.
*     SIGMA = _DOUBLE (Write)
*        The estimation of the RMS noise per pixel of the output image.
*        If the NDF is processed in sections (see parameter AXES), then
*        the value stored in this output parameter refers to the final
*        section processed.
*     THRESH( 2 ) = _DOUBLE (Read)
*        The range between which data values must lie if cleaning is to
*        occur.  Thus it is possible to clean the background without
*        removing the cores of images by a judicious choice of these
*        thresholds.  If null, !, is given, then there is no limit on
*        the data range.  [!]
*     TITLE = LITERAL (Read)
*        The title of the output NDF.  A null (!) value means using the
*        title of the input NDF.  [!]
*     WLIM = _REAL (Read)
*        If the input image contains bad pixels, then this parameter
*        may be used to determine the number of good pixels which must
*        be present within the smoothing box before a valid output
*        pixel is generated.  It can be used, for example, to prevent
*        output pixels from being generated in regions where there are
*        relatively few good pixels to contribute to the smoothed
*        result.
*
*        By default, a null (!) value is used for WLIM, which causes
*        the pattern of bad pixels to be propagated from the input
*        image to the output image unchanged.  In this case, smoothed
*        output values are only calculated for those pixels which are
*        not bad in the input image.
*
*        If a numerical value is given for WLIM, then it specifies the
*        minimum fraction of good pixels which must be present in the
*        smoothing box in order to generate a good output pixel.  If
*        this specified minimum fraction of good input pixels is not
*        present, then a bad output pixel will result, otherwise a
*        smoothed output value will be calculated.  The value of this
*        parameter should lie between 0.0 and 1.0 (the actual number
*        used will be rounded up if necessary to correspond to at least
*        1 pixel).  [!]

*  Examples:
*     ffclean dirty clean \
*       The NDF called dirty is filtered such that pixels that deviate
*       by more than three standard deviations from the smoothed
*       version of dirty are rejected.  Three iterations are performed.
*       Each pixel in the smoothed image is the average of the
*       neighbouring nine pixels.  The filtered NDF is called clean.
*     ffclean out=clean in=dirty thresh=[-100,200]
*       As above except only those pixels whose values lie between -100
*       and 200 can be cleaned.
*     ffclean poxy dazed [2.5,2.8] [5,5]
*       The two-dimensional NDF called poxy is filtered such that pixels
*       that deviate by more than 2.5 then 2.8 standard deviations from
*       the smoothed version of poxy are rejected.  The smoothing is an
*       average of a 5-by-5-pixel neighbourhood.  The filtered NDF is
*       called dazed.

*  Notes:
*     -  There are different facts reported, their verbosity depending
*     on the current message-reporting level set by environment variable
*     MSG_FILTER.  When the filtering level is at least as verbose as
*     NORMAL, the application will report the intermediate results after
*     each iteration during processing.  In addition, it will report the
*     section of the input NDF currently being processed (but only if
*     the NDF is being processed in sections---see Parameter AXES).

*  Related Applications:
*     KAPPA: CHPIX, FILLBAD, GLITCH, MEDIAN, MSTATS, ZAPLIN;
*     Figaro: BCLEAN, COSREJ, CLEAN, ISEDIT, MEDFILT, MEDSKY, TIPPEX.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of an
*     NDF data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using single- or double-precision floating point as
*     appropriate.

*  Copyright:
*     Copyright (C) 1981, 1990-1992 Science & Engineering Research
*     Council. Copyright (C) 1995, 2004 Central Laboratory of the
*     Research Councils.
*     Copyright (C) 2008, 2009 Science & Technology Facilities Council.
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
*     RFWS: R. F. Warren-Smith (STARLINK)
*     WG: Wei Gong  (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David Berry (JAC, UCLan)
*     PWD: Peter W. Draper (University of Durham)
*     {enter_new_authors_here}

*  History:
*     1981 July (RFWS):
*        Original INTERIM version.
*     5-OCT-1990 (WG):
*        Converted to the KAPPA/ADAM version.
*     1991 June 30 (MJC):
*        Made generic; completed and expanded the prologue, renaming
*        some parameters for consistency with other applications;
*        introduced WLIM parameter for fraction; corrected error
*        reporting; and added thresholds.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 21 (MJC):
*        Made to handle significant dimensions for user-defined
*        sections.
*     1992 December 17 (MJC):
*        Sets the bad-pixel flag in the output NDF.
*     1995 April 5 (MJC):
*        Renamed from CLEANER to avoid name clash with Figaro.  Made to
*        work on one-dimensional arrays.  Used lowercase examples and
*        usage.  Added Related Applications and additional commentary.
*        Changed the default of TITLE to null.  Used PSX to obtain
*        workspace.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     4-FEB-2008 (DSB):
*        Added parameters AXES and GENVAR.
*     2009 July 22 (MJC):
*        Remove ILEVEL parameter and use the current reporting level
*        instead (set by the global MSG_FILTER environment variable).
*     2009 November 11 (PWD):
*        Fix initialisation of OFF variable to be correct for double
*        precision data. Previously this only worked for real data.
*     24-JUN-2011 (DSB):
*        Ignore the input Variance component if it is full of bad values.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! PRM public constants
      INCLUDE 'PAR_ERR'          ! Parameter-system error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'MSG_PAR'          ! Message-system constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constant:
      INTEGER MXCLIP             ! Max. number of clips, and also
                                 ! the max. number of iteration of
                                 ! rejection algorithm can perform
      PARAMETER ( MXCLIP = 100 )

      INTEGER NDIM               ! Max. dimension of images the routine
                                 ! can process
      PARAMETER ( NDIM = 2 )

*  Local Variables:
      INTEGER BOX( NDIM )        ! Size of smoothing box
      INTEGER DIM( NDF__MXDIM )  ! Size of the image in each dimension
      INTEGER EL                 ! No. of elements copied
      INTEGER I                  ! Loop index
      INTEGER IBOX( 2 )          ! Smoothing box half-size
      INTEGER IL1                ! Grid index on first looping axis
      INTEGER IL2                ! Grid index on second looping axis
      INTEGER IPVIN              ! Pointer to input variance array
      INTEGER IPDIN              ! Pointer to input data array
      INTEGER IPVOUT             ! Pointer to output variance array
      INTEGER IPDOUT             ! Pointer to output data array
      INTEGER ITMP               ! Temporary storage
      INTEGER IWCS               ! WCS FrameSet
      INTEGER J                  ! Loop index
      INTEGER LBND( NDF__MXDIM ) ! Lower pixel index bounds of NDF
      INTEGER LBNDG( 3 )         ! Lower grid index bounds of NDF
      INTEGER LAXES( NDIM )      ! Indices of looping significant axes
      INTEGER LDIM( 2 )          ! Sizes of looping axes
      INTEGER LNAX               ! No. of looping axes
      INTEGER LNEL               ! No. of spectra/planes
      INTEGER NBADV              ! No. of bad input variance values
      INTEGER NCLIP              ! Number of gammas given by users, and
                                 ! also the number of rejecting
                                 ! iteration will be performed
      INTEGER NDFI               ! NDF Identifier for input image
      INTEGER NDFO               ! NDF Identifier for output image
      INTEGER NDIMS              ! Actual Number of the dimension of
                                 ! the image
      INTEGER NEL                ! Number of mapped elements
      INTEGER NGOOD              ! Number of valid pixels after
                                 ! rejection
      INTEGER NLIM               ! Minimum good pixel limit
      INTEGER NSDIM              ! Significant number of the dimension
                                 ! of the image
      INTEGER NVAL               ! Number of values obtained
      INTEGER NVAR               ! Number of elements of variance
                                 ! component of the image
      INTEGER NWS( 0:NDIM-1 )    ! Number of elements of work spaces
      INTEGER OFF                ! Vector offset into NDF arrays
      INTEGER PAXES( NDIM )      ! Indices of processed significant axes
      INTEGER PDIM( 2 )          ! Axis sizes in processed spectrum/plane
      INTEGER PLBNDG( 3 )        ! Lower grid bounds of spectrum/plane
      INTEGER PNAX               ! No. of processing axes
      INTEGER PNEL               ! No. of elements in a spectrum/plane
      INTEGER PNTIN( 2 )         ! Pointer to the mapped data and
                                 ! Variance of the input image
      INTEGER PNTINW             ! Pointer to work space
      INTEGER PNTOUT( 2 )        ! Pointer to the mapped data and
                                 ! variance of the output image
      INTEGER PNTAS( NDIM-1 )    ! Pointer to work spaces AS
      INTEGER PNTNS( NDIM-1 )    ! Pointer to work spaces NS
      INTEGER PUBNDG( 3 )        ! Upper grid bounds of spectrum/plane
      INTEGER SAXES( 3 )         ! Indices of significant NDF axes
      INTEGER SPAX               ! Indices of spectral axis
      INTEGER SPFRM              ! Spectral Frame
      INTEGER UBND( NDF__MXDIM ) ! Upper pixel index bounds of NDF
      INTEGER UBNDG( 3 )         ! Upper grid index bounds of NDF

      REAL CLIP( MXCLIP )        ! Number of standard deviation for
                                 ! rejection threshold for each
                                 ! iteration
      REAL THRDEF( 2 )           ! Suggested default thresholds for
                                 ! cleaning a single-precision array
      REAL THRESH( 2 )           ! Thresholds for cleaning a
                                 ! single-precision array
      REAL WLIM                  ! Fraction of good pixels required

      REAL RSIGMA                ! RMS noise per pixel in the output
                                 ! image

      DOUBLE PRECISION DTHDEF( 2 ) ! Suggested default Thresholds for
                                 ! cleaning a d.p. array
      DOUBLE PRECISION DTHRES( 2 ) ! Thresholds for cleaning a d.p.
                                 ! array
      DOUBLE PRECISION SIGMA     ! RMS noise per pixel in the output
                                 ! image

      CHARACTER * ( 13 ) COMPI   ! The array component(s) of input NDF
      CHARACTER * ( 13 ) COMPO   ! The array component(s) of output NDF
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Numeric type for output arrays
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Numeric type for processing

      LOGICAL CONTIG             ! Are spectra/planes contiguous?
      LOGICAL GENVAR             ! Create output variances?
      LOGICAL SAMBAD             ! Propagate bad pixels to same place?
      LOGICAL VAR                ! Variance array present?

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the input NDF.
*  =====================

*  Begin NDF and AST contexts.
      CALL AST_BEGIN( STATUS )
      CALL NDF_BEGIN

*  Initialise the upper and lower GRID index bounds.
      LBNDG( 1 ) = 1
      LBNDG( 2 ) = 1
      LBNDG( 3 ) = 1

      UBNDG( 1 ) = 1
      UBNDG( 2 ) = 1
      UBNDG( 3 ) = 1

*  Get identifier for the NDF containing the input image.
      CALL LPG_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Get the bounds of all pixel axes in the NDF.
      CALL NDF_BOUND( NDFI, NDF__MXDIM, LBND, UBND, NDIMS, STATUS )

*  Find and count the NDF pixel axes that have a length of more than
*  1 pixel. Also update the upper grid index bounds.
      NSDIM = 0
      DO I = 1, NDIMS
         DIM( I ) = UBND( I ) - LBND( I ) + 1

         IF ( DIM( I ) .GT. 1 ) THEN
            NSDIM = NSDIM + 1
            SAXES( NSDIM ) = I
            UBNDG( NSDIM ) = DIM( I )
         END IF

      END DO

*  If the data has only 1 significant pixel axis, then there can be only
*  1 processing axis, and no looping will be needed.
      IF ( NSDIM .EQ. 1 ) THEN
         PNAX = 1
         PAXES( 1 ) = 1
         LNAX = 0

*  If the data has two significant pixel axes, then we can process it as
*  a single two-dimensional plane or as a set of one-dimensional
*  spectra.  Ask the user what to do.  The default is to process as a
*  single plane.
      ELSE IF ( NSDIM .EQ. 2 ) THEN
         PAXES( 1 ) = 1
         PAXES( 2 ) = 2
         CALL PAR_DEF1I( 'AXES', 2, PAXES, STATUS )
         CALL PAR_GDRVI( 'AXES', 2, 1, 2, PAXES, PNAX, STATUS )

*  Determine the looping axes implied by the users choice of processing
*  axes.  Also ensure the processing axes are in increasing order.
         IF ( PNAX .EQ. 1 ) THEN
            LNAX = 1
            LAXES( 1 ) = 3 - PAXES( 1 )
         ELSE
            LNAX = 0
            IF ( PAXES( 2 ) .LT. PAXES( 1 ) ) THEN
               ITMP = PAXES( 2 )
               PAXES( 2 ) = PAXES( 1 )
               PAXES( 1 ) = ITMP
            END IF
         END IF

*  If the data has three significant pixel axes, then we can process it
*  as a a set of two-dimensional planes or as a set of one-dimensional
*  spectra.  Ask the user what to do.  The default is to process as a
*  set of one-dimensional spectra along the spectral axis (or along
*  pixel-axis 1 if there is no spectral axis).
      ELSE IF ( NSDIM .EQ. 3 ) THEN

*  Get the WCS FrameSet from the input NDF, and then search the current
*  WCS Frame for a SpecFrame.
         CALL KPG1_GTWCS( NDFI, IWCS, STATUS )
         CALL ATL_FSPEC( IWCS, SPAX, SPFRM, STATUS )
         IF ( SPAX .EQ. 0 ) SPAX = 1

*  Set up the default for parameter AXES and then get a new value.
         PAXES( 1 ) = SPAX
         CALL PAR_DEF1I( 'AXES', 1, PAXES, STATUS )
         CALL PAR_GDRVI( 'AXES', 2, 1, 3, PAXES, PNAX, STATUS )

*  Determine the looping axes implied by the users choice of processing
*  axes.
         IF ( PNAX .EQ. 1 ) THEN
            LNAX = 2
            IF ( PAXES( 1 ) .EQ. 1 ) THEN
               LAXES( 1 ) = 2
               LAXES( 2 ) = 3
            ELSE IF ( PAXES( 1 ) .EQ. 2 ) THEN
               LAXES( 1 ) = 1
               LAXES( 2 ) = 3
            ELSE
               LAXES( 1 ) = 1
               LAXES( 2 ) = 2
            END IF

         ELSE
            LNAX = 1
            LAXES( 1 ) = 6 - PAXES( 1 ) - PAXES( 2 )

            IF ( PAXES( 2 ) .LT. PAXES( 1 ) ) THEN
               ITMP = PAXES( 2 )
               PAXES( 2 ) = PAXES( 1 )
               PAXES( 1 ) = ITMP
            END IF

         END IF

*  Report an error if the number of significant pixel axes cannot be
*  handled by this application.
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         PNAX = 0
         LNAX = 0
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', NDFI )
         CALL MSG_SETI( 'ND', NSDIM )
         CALL ERR_REP( 'KAPPA_FFCLEAN_ND', 'The NDF ^NDF has ^ND '//
     :                 'significant dimensions. This application '//
     :                 'can only handled from 1 to 3.', STATUS )
      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Find the length of the axes.
      DO I = 1, PNAX
         PDIM( I ) = DIM( SAXES( PAXES( I ) ) )
      END DO

      DO I = 1, LNAX
         LDIM( I ) = DIM( SAXES( LAXES( I ) ) )
      END DO

*  Pad the arrays with insignficant axes.
      IF ( LNAX .EQ. 0 ) LDIM( 1 ) = 1
      IF ( LNAX .LE. 1 ) LDIM( 2 ) = 1
      IF ( PNAX .EQ. 1 ) PDIM( 2 ) = 1

*  Check the processing axes are distinct.
      IF ( PNAX .EQ. 2 .AND. STATUS .EQ. SAI__OK ) THEN
         IF ( PAXES( 1 ) .EQ. PAXES( 2 ) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'ND', NSDIM )
            CALL ERR_REP( 'KAPPA_FFCLEAN_DS', 'The two axis indices '//
     :                    'supplied for parameter AXES are the same.',
     :                    STATUS )
         END IF
      END IF

*  Get the number of elements in one processing plane or spectrum.
      PNEL = PDIM( 1 )
      IF ( PNAX .EQ. 2 ) PNEL = PNEL * PDIM( 2 )

*  Get the number of planes or spectra to be processed.
      LNEL = LDIM( 1 )
      IF ( LNAX .EQ. 2 ) LNEL = LNEL * LDIM( 2 )

*  See if output variances are to be created from the deviations from
*  the local mean.
      CALL PAR_GET0L( 'GENVAR', GENVAR, STATUS )

*  If not, check the state of variance component of the NDF.
      IF ( .NOT. GENVAR ) THEN
         CALL NDF_STATE( NDFI, 'Variance', VAR, STATUS )

*  Set the NDF component string according to the state of variance
*  component.
         IF ( VAR ) THEN

*  Set the component string for the case when variance component exists.
            COMPI = 'Data,Variance'
            COMPO = 'Data,Variance'
         ELSE

*  Set the component string for the case when variance component does
*  not exist.
            COMPI = 'Data'
            COMPO = 'Data'
         END IF

*  If we are creating output variances from the local spread of values,
*  then the input variance array is not needed, but the output variance
*  array is needed.
      ELSE
         VAR = .FALSE.
         COMPI = 'Data'
         COMPO = 'Data,Variance'
      END IF

*  Determine the numeric type to be used for processing the input
*  arrays.  This application supports single- and double-precision
*  floating point processing.
      CALL NDF_MTYPE( '_REAL,_DOUBLE', NDFI, NDFI, COMPI, ITYPE, DTYPE,
     :                STATUS )

*  Create the output NDF and map arrays.
*  =====================================

*  Create a new output NDF to contain the cleaned image, which
*  inheriting all the attributes of the input NDF.  Set an appropriate
*  numeric type for the output arrays.
      CALL LPG_PROP( NDFI, 'WCS,Axis,Units,Quality', 'OUT', NDFO,
     :               STATUS )
      CALL NDF_STYPE( DTYPE, NDFO, COMPO, STATUS )

*  Map the input and output data arrays.
      CALL KPG1_MAP( NDFI, COMPI, ITYPE, 'READ', PNTIN, NEL, STATUS )
      CALL KPG1_MAP( NDFO, COMPO, ITYPE, 'WRITE', PNTOUT, NEL, STATUS )

*  Exit if an error occurred.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  If a variance array exists, count the bad values in it.
      IF( VAR ) THEN
         IF( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_NBADD( NEL, %VAL( CNF_PVAL( PNTIN( 2 ) ) ), NBADV,
     :                       STATUS )
         ELSE
            CALL KPG1_NBADR( NEL, %VAL( CNF_PVAL( PNTIN( 2 ) ) ), NBADV,
     :                       STATUS )
         END IF

*  If all values are bad, warn the user and pretend no input variance
*  array is available.
         IF( NBADV .EQ. NEL ) THEN
            CALL NDF_UNMAP( NDFI, 'VARIANCE', STATUS )
            IF( .NOT. GENVAR ) CALL NDF_UNMAP( NDFO, 'VARIANCE',
     :                                         STATUS )
            VAR = .FALSE.
            CALL MSG_OUT( ' ', 'All input variances are bad, and so '//
     :                    'will not be used.', STATUS )
         END IF

      END IF


*  Create a dummy variance if it is not present.
*  =============================================

*  Set the number of elements of variance component.
      IF ( VAR ) THEN

*  When variance component exists, it has the same number of element as
*  the processing plane or spectrum.
         NVAR = PNEL
      ELSE

*  Otherwise assume it has 1 element.
         NVAR = 1
      END IF

*  Create and map work space for the smoothing.
*  ============================================

*  Get the required work space for to hold the cleaned iterations to
*  be input to local-mean routine.
      CALL PSX_CALLOC( PNEL, ITYPE, PNTINW, STATUS )

*  Initial the element number of work spaces.
      DO 10 I = 0, NDIM-1
         NWS( I ) = 1
   10 CONTINUE

*  Get the required work space for local-mean algorithm.
      DO 20 I = 1, NDIM-1

*  Calculate the element number of ith work spaces.
         NWS( I ) = NWS( I-1 ) * PDIM( I )

*  Get the required work spaces for smoothing I+1 dimensional image.
         CALL PSX_CALLOC( PDIM( I ), ITYPE, PNTAS( I ), STATUS )
         CALL PSX_CALLOC( PDIM( I ), '_INTEGER', PNTNS( I ), STATUS )

   20 CONTINUE

*  Check if error happens, report to user and exit.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'KAPPA_FFCLEAN_WSP',
     :     'FFCLEAN: Insufficient memory space available to obtain '/
     :     /'workspace.', STATUS )
         GOTO 99
      END IF

*  Obtain parameters to control the filtering.
*  ===========================================

*  Obtain the smoothing box sizes, duplicating the value if only a
*  single value is given.  Give an appropriate dynamic default.  Each
*  box size must be a positive odd number, so derive IBOX so that BOX =
*  2*IBOX+1 is rounded up if necessary.  Also allow for one-dimensional
*  data, setting the default box size to 1 element.
      BOX( 1 ) = 3
      BOX( 2 ) = 3
      IF ( PDIM( 1 ) .EQ. 1 .OR. PDIM( 2 ) .EQ. 1 ) THEN
         CALL PAR_DEF1I( 'BOX', 1, BOX, STATUS )
      ELSE
         CALL PAR_DEF1I( 'BOX', NDIM, BOX, STATUS )
      END IF

      CALL PAR_GDRVI( 'BOX', NDIM, 1, MAX( PDIM( 1 ), PDIM( 2 ) ), BOX,
     :                NVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

      IF ( NVAL .LT. NDIM ) THEN
         BOX( 2 ) = BOX( 1 )
      END IF

      DO I = 1, NDIM
         IF ( PDIM( I ) .EQ. 1 ) THEN
            IBOX( I ) = 0
            BOX( I ) = 1
         ELSE
            IBOX( I ) = MAX( BOX( I ), 1 ) / 2
         END IF
      END DO

*  Get number of standard deviation for rejection threshold for each
*  iteration.
      CALL PAR_GET1R( 'CLIP', MXCLIP, CLIP, NCLIP, STATUS )

*  Obtain the minimum fraction of good pixels which should be used to
*  calculate an output pixel value.  Test if a null value is specified
*  and set SAMBAD appropriately, annulling the error.
      CALL ERR_MARK
      SAMBAD = .FALSE.
      CALL PAR_GDR0R( 'WLIM', 0.5, 0.0, 1.0, .FALSE., WLIM, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         SAMBAD = .TRUE.
         CALL ERR_ANNUL( STATUS )

*  Derive the minimum number of pixels, using at least one.
      ELSE
         NLIM = MAX( 1, NINT( REAL( ( 2 * IBOX( 1 ) + 1 ) *
     :                              ( 2 * IBOX( 2 ) + 1 ) ) * WLIM ) )
      END IF
      CALL ERR_RLSE

*  Obtain the range of thresholds in the appropriate type.  If a null
*  value is returned the full range of the implementation type are
*  used.  Set the suggested default arguments to bad values in order to
*  have no dynamic defaults.
      CALL ERR_MARK
      IF ( ITYPE .EQ. '_DOUBLE' )THEN
         DTHDEF( 1 ) = VAL__BADD
         DTHDEF( 2 ) = VAL__BADD
         DTHRES( 1 ) = VAL__MIND
         DTHRES( 2 ) = VAL__MAXD
         CALL PAR_GDR1D( 'THRESH', 2, DTHDEF, VAL__MIND, VAL__MAXD,
     :                   .FALSE., DTHRES, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            DTHRES( 1 ) = VAL__MIND
            DTHRES( 2 ) = VAL__MAXD
         END IF
      ELSE
         THRDEF( 1 ) = VAL__BADR
         THRDEF( 2 ) = VAL__BADR
         THRESH( 1 ) = VAL__MINR
         THRESH( 2 ) = VAL__MAXR
         CALL PAR_GDR1R( 'THRESH', 2, THRDEF, VAL__MINR, VAL__MAXR,
     :                   .FALSE., THRESH, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            THRESH( 1 ) = VAL__MINR
            THRESH( 2 ) = VAL__MAXR
         END IF
      END IF
      CALL ERR_RLSE


*  Loop round all spectra or planes being processed.
*  =================================================

*  If each processed spectrum or plane forms a contiguous block of
*  pixels in the NDF, then we read input data directly from the input
*  NDF and write output data directly to the output NDF.
      IF ( ( PAXES( 1 ) .EQ. 1 ) .AND. ( PNAX .EQ. 1 .OR.
     :                                   PAXES( 2 ) .EQ. 2 ) ) THEN
         CONTIG = .TRUE.
         IPDIN = PNTIN( 1 )
         IPVIN = PNTIN( 2 )
         IPDOUT = PNTOUT( 1 )
         IPVOUT = PNTOUT( 2 )

*  Otherwise, we copy each spectrum or plane from the input NDF to
*  workspace, process it, and then copy it to the output NDF.  Allocate
*  the necessary workspace.
      ELSE
         CONTIG = .FALSE.
         CALL PSX_CALLOC( PNEL, ITYPE, IPDIN, STATUS )
         CALL PSX_CALLOC( PNEL, ITYPE, IPDOUT, STATUS )
         IF ( VAR ) THEN
            CALL PSX_CALLOC( PNEL, ITYPE, IPVIN, STATUS )
         ELSE
            IPVIN = IPDIN
         END IF

         IF ( VAR .OR. GENVAR ) THEN
            CALL PSX_CALLOC( PNEL, ITYPE, IPVOUT, STATUS )
         ELSE
            IPVOUT = IPDOUT
         END IF

      END IF

*  Initialise the grid index bounds on the processing axes of the next
*  spectrum or plane to be processed.
      DO I = 1, PNAX
         PLBNDG( PAXES( I ) ) = 1
         PUBNDG( PAXES( I ) ) = PDIM( I )
      END DO

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each spectrum or plane, and complete the grid index bounds
*  of the current spectrum or plane.
      IF ( ITYPE .EQ. '_REAL' ) THEN
         OFF = -PNEL * VAL__NBR
      ELSE
         OFF = -PNEL * VAL__NBD
      END IF
      DO IL2 = 1, LDIM( 2 )

         IF ( LNAX .EQ. 2 ) THEN
            PLBNDG( LAXES( 2 ) ) = IL2
            PUBNDG( LAXES( 2 ) ) = IL2
         END IF

         DO IL1 = 1, LDIM( 1 )

            IF ( LNAX .GT. 0 ) THEN
               PLBNDG( LAXES( 1 ) ) = IL1
               PUBNDG( LAXES( 1 ) ) = IL1
            END IF

*  If required, report the section being processed.
            IF ( LNEL .GT. 1 ) THEN

               J = 1
               DO I = 1, NDIMS
                  IF ( DIM( I ) .EQ. 1 ) THEN
                     CALL MSG_SETI( 'SEC', LBND( I ) )
                     CALL MSG_SETC( 'SEC', ':' )
                     CALL MSG_SETI( 'SEC', UBND( I ) )
                  ELSE
                     CALL MSG_SETI( 'SEC', PLBNDG( J ) + LBND( I ) - 1 )
                     CALL MSG_SETC( 'SEC', ':' )
                     CALL MSG_SETI( 'SEC', PUBNDG( J ) + LBND( I ) - 1 )
                     J = J + 1
                  END IF

                  IF ( I .NE. NDIMS ) CALL MSG_SETC( 'SEC', ',' )
               END DO

               CALL MSG_BLANK( STATUS )
               CALL MSG_OUTIF( MSG__NORM, ' ',
     :                         '   Processing section (^SEC)...',
     :                         STATUS )

            END IF

*  If required, copy the current spectrum or plane from the input NDF to
*  the work arrays.
            IF ( .NOT. CONTIG ) THEN
               OFF = 0

               IF ( ITYPE .EQ. '_REAL' ) THEN

                  CALL KPG1_CPNDR( NSDIM, LBNDG, UBNDG,
     :                             %VAL( CNF_PVAL( PNTIN( 1 ) ) ),
     :                             PLBNDG, PUBNDG,
     :                             %VAL( CNF_PVAL( IPDIN ) ),
     :                             EL, STATUS )

                  IF ( VAR ) THEN
                     CALL KPG1_CPNDR( NSDIM, LBNDG, UBNDG,
     :                                %VAL( CNF_PVAL( PNTIN( 2 ) ) ),
     :                                PLBNDG, PUBNDG,
     :                                %VAL( CNF_PVAL( IPVIN ) ),
     :                                EL, STATUS )
                  END IF

               ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

                  CALL KPG1_CPNDD( NSDIM, LBNDG, UBNDG,
     :                             %VAL( CNF_PVAL( PNTIN( 1 ) ) ),
     :                             PLBNDG, PUBNDG,
     :                             %VAL( CNF_PVAL( IPDIN ) ),
     :                             EL, STATUS )

                  IF ( VAR ) THEN
                     CALL KPG1_CPNDD( NSDIM, LBNDG, UBNDG,
     :                                %VAL( CNF_PVAL( PNTIN( 2 ) ) ),
     :                                PLBNDG, PUBNDG,
     :                                %VAL( CNF_PVAL( IPVIN ) ),
     :                                EL, STATUS )
                  END IF

               END IF

*  For contiguous access, increment the offset into the input and output
*  NDFs.
            ELSE
               IF ( ITYPE .EQ. '_REAL' ) THEN
                  OFF = OFF + PNEL * VAL__NBR
               ELSE
                  OFF = OFF + PNEL * VAL__NBD
               END IF
            END IF

*  Perform the filtering.
*  ======================

*  Reject pixels deviating from their local mean by more than the
*  threshold calling the routine of the appropriate data type.
            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPS1_CFF2R( PDIM( 1 ), PDIM( 2 ),
     :                          %VAL( CNF_PVAL( IPDIN ) + OFF ), VAR,
     :                          NVAR, %VAL( CNF_PVAL( IPVIN ) + OFF ),
     :                          BOX, NCLIP, CLIP, THRESH, SAMBAD, NLIM,
     :                          %VAL( CNF_PVAL( PNTINW ) ),
     :                          %VAL( CNF_PVAL( IPDOUT ) + OFF ),
     :                          %VAL( CNF_PVAL( IPVOUT ) + OFF ), NGOOD,
     :                          SIGMA, %VAL( CNF_PVAL( PNTAS( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTNS( 1 ) ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPS1_CFF2D( PDIM( 1 ), PDIM( 2 ),
     :                          %VAL( CNF_PVAL( IPDIN ) + OFF ), VAR,
     :                          NVAR, %VAL( CNF_PVAL( IPVIN ) + OFF ),
     :                          BOX, NCLIP, CLIP, DTHRES, SAMBAD, NLIM,
     :                          %VAL( CNF_PVAL( PNTINW ) ),
     :                          %VAL( CNF_PVAL( IPDOUT ) + OFF ),
     :                          %VAL( CNF_PVAL( IPVOUT ) + OFF ), NGOOD,
     :                          SIGMA, %VAL( CNF_PVAL( PNTAS( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTNS( 1 ) ) ), STATUS )

            END IF

*  If we are processing multiple spectra or pleanes, and an error
*  occurred with this one, annull the error and fill the output spectrum
*  or plane with bad values.
            IF ( STATUS .NE. SAI__OK .AND. LNEL .GT. 1 ) THEN
               CALL ERR_ANNUL( STATUS )
               IF ( ITYPE .EQ. '_REAL' ) THEN
                  CALL KPG1_FILLR( VAL__BADR, PNEL,
     :                             %VAL( CNF_PVAL( IPDOUT ) + OFF ),
     :                             STATUS )
                  IF ( VAR ) THEN
                     CALL KPG1_FILLR( VAL__BADR, PNEL,
     :                                %VAL( CNF_PVAL( IPVOUT ) + OFF ),
     :                                STATUS )
                  END IF

               ELSE
                  CALL KPG1_FILLD( VAL__BADD, PNEL,
     :                             %VAL( CNF_PVAL( IPDOUT ) + OFF ),
     :                             STATUS )
                  IF ( VAR ) THEN
                     CALL KPG1_FILLD( VAL__BADD, PNEL,
     :                                %VAL( CNF_PVAL( IPVOUT ) + OFF ),
     :                                STATUS )
                  END IF

               END IF

               SIGMA = VAL__BADD

            END IF

*  If we are generating output variances from the local spread of
*  values, fill the output variance array with the sigma value returned
*  by the filtering algorithm.
            IF ( GENVAR ) THEN
               IF ( ITYPE .EQ. '_REAL' ) THEN

                  IF ( SIGMA .EQ. VAL__BADD ) THEN
                     RSIGMA = VAL__BADR
                  ELSE
                     RSIGMA = REAL( SIGMA * SIGMA )
                  END IF

                  CALL KPG1_FILLR( RSIGMA, PNEL,
     :                             %VAL( CNF_PVAL( IPVOUT ) + OFF ),
     :                             STATUS )

               ELSE
                  CALL KPG1_FILLD( SIGMA * SIGMA, PNEL,
     :                             %VAL( CNF_PVAL( IPVOUT ) + OFF ),
     :                             STATUS )

               END IF
            END IF

*  If required, paste the current spectrum or plane into the output NDF.
            IF ( .NOT. CONTIG ) THEN
               IF ( ITYPE .EQ. '_REAL' ) THEN

                  EL = -1
                  CALL KPG1_PTNDR( NSDIM, PLBNDG, PUBNDG,
     :                             %VAL( CNF_PVAL( IPDOUT ) ),
     :                             LBNDG, UBNDG, .FALSE.,
     :                             %VAL( CNF_PVAL( PNTOUT( 1 ) ) ),
     :                             EL, STATUS )

                  IF ( VAR .OR. GENVAR ) THEN
                     EL = -1
                     CALL KPG1_PTNDR( NSDIM, PLBNDG, PUBNDG,
     :                                %VAL( CNF_PVAL( IPVOUT ) ),
     :                                LBNDG, UBNDG, .FALSE.,
     :                                %VAL( CNF_PVAL( PNTOUT( 2 ) ) ),
     :                                EL, STATUS )
                  END IF

               ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

                  EL = -1
                  CALL KPG1_PTNDD( NSDIM, PLBNDG, PUBNDG,
     :                             %VAL( CNF_PVAL( IPDOUT ) ),
     :                             LBNDG, UBNDG, .FALSE.,
     :                             %VAL( CNF_PVAL( PNTOUT( 1 ) ) ),
     :                             EL, STATUS )

                  IF ( VAR .OR. GENVAR ) THEN
                     EL = -1
                     CALL KPG1_PTNDD( NSDIM, PLBNDG, PUBNDG,
     :                                %VAL( CNF_PVAL( IPVOUT ) ),
     :                                LBNDG, UBNDG, .FALSE.,
     :                                %VAL( CNF_PVAL( PNTOUT( 2 ) ) ),
     :                                EL, STATUS )
                  END IF

               END IF
            END IF


         END DO
      END DO

*  Release the work space.
      IF ( .NOT. CONTIG ) THEN
         CALL PSX_FREE( IPDIN, STATUS )
         CALL PSX_FREE( IPDOUT, STATUS )

         IF ( VAR ) CALL PSX_FREE( IPVIN, STATUS )
         IF ( VAR .OR. GENVAR ) CALL PSX_FREE( IPVOUT, STATUS )

      END IF

*  Output the RMS noise per pixel of the cleaned image to
*  the environment.
      CALL PAR_PUT0D( 'SIGMA', SIGMA, STATUS )

*  Get the new title for the output image and insert it into the output
*  NDF.  The input NDF's title was already propagated by the LPG_PROP
*  call and so a null value will leave it unaltered.
      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )

*  If valid pixels in output image equals total number pixels, set
*  bad-pixel flag .false., otherwise .true..
      IF ( NGOOD .EQ. NEL ) THEN
         CALL NDF_SBAD( .FALSE., NDFO, 'Data', STATUS )
         IF ( VAR ) CALL NDF_SBAD( .FALSE., NDFO, 'Variance', STATUS )
      ELSE
         CALL NDF_SBAD( .TRUE., NDFO, 'Data', STATUS )
         IF ( VAR ) CALL NDF_SBAD( .TRUE., NDFO, 'Variance', STATUS )
      END IF

   99 CONTINUE

*  Release the work spaces.
      CALL PSX_FREE( PNTINW, STATUS )
      DO 30 I = 1, NDIM-1
        CALL PSX_FREE( PNTAS( I ), STATUS )
        CALL PSX_FREE( PNTNS( I ), STATUS )
   30 CONTINUE

  999 CONTINUE

* End the NDF and AST contexts.
      CALL NDF_END( STATUS )
      CALL AST_END( STATUS )

*  End the routine.

      END
