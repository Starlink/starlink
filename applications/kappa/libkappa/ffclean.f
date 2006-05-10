       SUBROUTINE FFCLEAN( STATUS )
*+
*  Name:
*     FFCLEAN

*  Purpose:
*     Removes defects from a substantially flat 1- or 2-dimensional NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FFCLEAN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application cleans a 1- or 2-dimensional NDF by removing
*     defects smaller than a specified size.  The defects are flagged
*     with the bad value.  The defects are found by looking for pixels
*     that deviate from the image's smoothed version by more than an
*     arbitrary number of standard deviations from the local mean,
*     and that lie within a specified range of values.  Therefore, the
*     image must be substantially flat.  The data variances provide the
*     local-noise estimate for the threshold, but if these are not
*     available a variance for the whole of the image is derived from
*     the mean squared deviations of the original and smoothed images.
*     The smoothed version of the image is obtained by block averaging
*     over a rectangular box.  An iterative process progressively
*     removes the outliers from the image.

*  Usage:
*     ffclean in out clip box [thresh] [wlim] [ilevel]

*  ADAM Parameters:
*     BOX( 2 ) = _INTEGER (Read)
*        The x and y sizes (in pixels) of the rectangular box to be
*        applied to smooth the image.  If only a single value is given,
*        then it will be duplicated so that a square filter is used
*        except where the image is 1-dimensional for which the box size
*        along the insignificant dimension is set to 1.  The values
*        given will be rounded up to positive odd integers if
*        necessary.
*     CLIP( ) = _REAL (Read)
*        The number of standard deviations for the rejection threshold
*        of each iteration.  Pixels that deviate from their counterpart
*        in the smoothed image by more than CLIP times the noise are
*        made bad.  The number of values given specifies the number of
*        iterations.  Values should lie in the range 0.5--100.  Up to
*        one hundred values may be given.  [3.0, 3.0, 3.0]
*     ILEVEL = _INTEGER (Read)
*        The interactive level of the routine.  When it is greater or
*        equal to two, the application will report the intermediate
*        results after each iteration during processing.  It should lie
*        between 1 and 3. [2]
*     IN = NDF (Read)
*        The 1- or 2-dimensional NDF containing the input image to be
*        cleaned.
*     OUT = NDF (Write)
*        The NDF to contain the cleaned image.
*     SIGMA = _DOUBLE (Write)
*        The estimation of the RMS noise per pixel of the output image.
*     THRESH( 2 ) = _DOUBLE (Read)
*        The range between which data values must lie if cleaning is to
*        occur.  Thus it is possible to clean the background without
*        removing the cores of images by a judicious choice of these
*        thresholds.  If null, !, is given, then there is no limit on
*        the data range. [!]
*     TITLE = LITERAL (Read)
*        The title of the output NDF.  A null (!) value means using the
*        title of the input NDF. [!]
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
*        1 pixel). [!]

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
*       The 2-dimensional NDF called poxy is filtered such that pixels
*       that deviate by more than 2.5 then 2.8 standard deviations from
*       the smoothed version of poxy are rejected.  The smoothing is an
*       average of a 5-by-5-pixel neighbourhood.  The filtered NDF is
*       called dazed.

*  Related Applications:
*     KAPPA: CHPIX, FILLBAD, GLITCH, MEDIAN, MSTATS, ZAPLIN;
*     Figaro: BCLEAN, COSREJ, CLEAN, ISEDIT, MEDFILT, MEDSKY, TIPPEX.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of an NDF
*     data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using single- or double-precision floating point as
*     appropriate.

*  Copyright:
*     Copyright (C) 1981, 1990-1992 Science & Engineering Research
*     Council. Copyright (C) 1995, 2004 Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R. F. Warren-Smith (STARLINK)
*     WG: Wei Gong  (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
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
*        work on 1-dimensional arrays.  Used lowercase examples and
*        usage.  Added Related Applications and additional commentary.
*        Changed the default of TITLE to null.  Used PSX to obtain
*        workspace.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
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
      INTEGER I                  ! Loop index
      INTEGER IBOX( 2 )          ! Smoothing box half-size
      INTEGER ILEVEL             ! Interaction level
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
      INTEGER NVAL               ! Number of values obtained
      INTEGER NVAR               ! Number of elements of variance
                                 ! component of the image
      INTEGER NWS( 0:NDIM-1 )    ! Number of elements of work spaces
      INTEGER PNTIN( 2 )         ! Pointer to the mapped data and
                                 ! Variance of the input image
      INTEGER PNTINW             ! Pointer to work space
      INTEGER PNTOUT( 2 )        ! Pointer to the mapped data and
                                 ! variance of the output image
      INTEGER PNTAS( NDIM-1 )    ! Pointer to work spaces AS
      INTEGER PNTNS( NDIM-1 )    ! Pointer to work spaces NS
      INTEGER SDIM( NDF__MXDIM ) ! Significant NDF dimensions

      REAL CLIP( MXCLIP )        ! Number of standard deviation for
                                 ! rejection threshold for each
                                 ! iteration
      REAL THRDEF( 2 )           ! Suggested default thresholds for
                                 ! cleaning a single-precision array
      REAL THRESH( 2 )           ! Thresholds for cleaning a
                                 ! single-precision array
      REAL WLIM                  ! Fraction of good pixels required

      DOUBLE PRECISION DTHDEF( 2 ) ! Suggested default Thresholds for
                                 ! cleaning a d.p. array
      DOUBLE PRECISION DTHRES( 2 ) ! Thresholds for cleaning a d.p.
                                 ! array
      DOUBLE PRECISION SIGMA     ! RMS noise per pixel in the output
                                 ! image

      CHARACTER * ( 13 ) COMP    ! The array component(s) of NDF
      CHARACTER * ( NDF__SZFTP ) DTYPE ! Numeric type for output arrays
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Numeric type for processing

      LOGICAL SAMBAD             ! Propagate bad pixels to same place?
      LOGICAL VAR                ! Variance array present?

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the input NDF.
*  =====================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get identifier for the NDF containing the input image.
      CALL LPG_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Find whether or not there are no more than two significant
*  dimensions and which ones they are.
      CALL KPG1_SDIMP( NDFI, NDIM, SDIM, STATUS )

*  Exit if an error occurred.  This is needed because the significant
*  dimensions are used as array indices.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Determine its dimensions (note that only two significant dimensions
*  can be accommodated).  Then ignore non-significant dimensions.
      CALL NDF_DIM( NDFI, SDIM( NDIM ), DIM, NDIMS, STATUS )
      DIM( 1 ) = DIM( SDIM( 1 ) )
      DIM( 2 ) = DIM( SDIM( 2 ) )

*  Check the state of variance component of the NDF.      
      CALL NDF_STATE( NDFI, 'Variance', VAR, STATUS )

*  Set the NDF component string according to the state of variance
*  component.
      IF ( VAR ) THEN

*  Set the component string for the case when variance component exists.
         COMP = 'Data,Variance'
      ELSE

*  Set the component string for the case when variance component does
*  not exist.
         COMP = 'Data'
      END IF

*  Determine the numeric type to be used for processing the input
*  arrays.  This application supports single- and double-precision
*  floating point processing.
      CALL NDF_MTYPE( '_REAL,_DOUBLE', NDFI, NDFI, COMP, ITYPE, DTYPE,
     :                STATUS )

*  Create the output NDF and map arrays.
*  =====================================

*  Create a new output NDF to contain the cleaned image, which
*  inheriting all the attributes of the input NDF.  Set an appropriate
*  numeric type for the output arrays.
      CALL LPG_PROP( NDFI, 'WCS,Axis,Units,Quality', 'OUT', NDFO, 
     :               STATUS )
      CALL NDF_STYPE( DTYPE, NDFO, COMP, STATUS )

*  Map the input and output data arrays.
      CALL KPG1_MAP( NDFI, COMP, ITYPE, 'READ', PNTIN, NEL, STATUS )
      CALL KPG1_MAP( NDFO, COMP, ITYPE, 'WRITE', PNTOUT, NEL, STATUS )

*  Exit if an error occurred.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Create a dummy variance if it is not present.
*  =============================================

*  Set the number of elements of variance component.
      IF ( VAR ) THEN

*  When variance component exists, it has the same number of element as 
*  the data array.
         NVAR=NEL
      ELSE

*  Otherwise assume it has 1 element.
         NVAR=1
      END IF

*  Create and map work space for the smoothing.
*  ============================================

*  Get the required work space for to hold the cleaned iterations to
*  be input to local-mean routine.
      CALL PSX_CALLOC( NEL, ITYPE, PNTINW, STATUS )

*  Initial the element number of work spaces.
      DO 10 I = 0, NDIM-1
         NWS( I ) = 1
   10 CONTINUE

*  Get the required work space for local-mean algorithm.
      DO 20 I = 1, NDIM-1

*  Calculate the element number of ith work spaces.
         NWS( I ) = NWS( I-1 ) * DIM( I )

*  Get the required work spaces for smoothing I+1 dimensional image. 
         CALL PSX_CALLOC( DIM( I ), ITYPE, PNTAS( I ), STATUS )
         CALL PSX_CALLOC( DIM( I ), '_INTEGER', PNTNS( I ), STATUS )

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
      IF ( DIM( 1 ) .EQ. 1 .OR. DIM( 2 ) .EQ. 1 ) THEN
         CALL PAR_DEF1I( 'BOX', 1, BOX, STATUS )
      ELSE
         CALL PAR_DEF1I( 'BOX', NDIM, BOX, STATUS )
      END IF

      CALL PAR_GDRVI( 'BOX', NDIM, 1, MAX( DIM( 1 ), DIM( 2 ) ), BOX,
     :                NVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

      IF ( NVAL .LT. NDIM ) THEN
         BOX( 2 ) = BOX( 1 )
      END IF

      DO I = 1, NDIM
         IF ( DIM( I ) .EQ. 1 ) THEN
            IBOX( I ) = 0
            BOX( I ) = 1
         ELSE
            IBOX( I ) = MAX( BOX( I ), 1 ) / 2
         END IF
      END DO

*  Get number of standard deviation for rejection threshold for each
*  iteration.
      CALL PAR_GET1R( 'CLIP', MXCLIP, CLIP, NCLIP, STATUS )

*  Get interaction level for rejection algorithm.
      CALL PAR_GDR0I( 'ILEVEL', 2, 1, 3, .TRUE., ILEVEL, STATUS )

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
         CALL PAR_GDR1R( 'THRESH', 2, THRDEF, VAL__MINR, VAL__MAXR,
     :                   .FALSE., THRESH, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            THRESH( 1 ) = VAL__MINR
            THRESH( 2 ) = VAL__MAXR
         END IF
      END IF
      CALL ERR_RLSE

*  Perform the filtering.
*  ======================

*  Reject pixels deviating from their local mean by more than the
*  threshold calling the routine of the appropriate data type.
      IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPS1_CFF2R( DIM( 1 ), DIM( 2 ), 
     :                    %VAL( CNF_PVAL( PNTIN( 1 ) ) ), VAR,
     :                    NVAR, %VAL( CNF_PVAL( PNTIN( 2 ) ) ), 
     :                    BOX, NCLIP, CLIP,
     :                    THRESH, ILEVEL, SAMBAD, NLIM, 
     :                    %VAL( CNF_PVAL( PNTINW ) ),
     :                    %VAL( CNF_PVAL( PNTOUT( 1 ) ) ), 
     :                    %VAL( CNF_PVAL( PNTOUT( 2 ) ) ),
     :                    NGOOD, SIGMA, %VAL( CNF_PVAL( PNTAS( 1 ) ) ),
     :                    %VAL( CNF_PVAL( PNTNS( 1 ) ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPS1_CFF2D( DIM( 1 ), DIM( 2 ), 
     :                    %VAL( CNF_PVAL( PNTIN( 1 ) ) ), VAR,
     :                    NVAR, %VAL( CNF_PVAL( PNTIN( 2 ) ) ), 
     :                    BOX, NCLIP, CLIP,
     :                    DTHRES, ILEVEL, SAMBAD, NLIM, 
     :                    %VAL( CNF_PVAL( PNTINW ) ),
     :                    %VAL( CNF_PVAL( PNTOUT( 1 ) ) ), 
     :                    %VAL( CNF_PVAL( PNTOUT( 2 ) ) ),
     :                    NGOOD, SIGMA, %VAL( CNF_PVAL( PNTAS( 1 ) ) ),
     :                    %VAL( CNF_PVAL( PNTNS( 1 ) ) ), STATUS )

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

* End the NDF context.
      CALL NDF_END( STATUS )

*  End the routine.
      
      END
