       SUBROUTINE FILLBAD( STATUS )
*+
*  Name:
*     FILLBAD

*  Purpose:
*     Removes regions of bad values from an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FILLBAD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application replaces bad values in a NDF with a smooth
*     function which matches the surrounding data.  It can fill
*     arbitrarily shaped regions of bad values within n-dimensional
*     arrays.
*
*     It forms a smooth replacement function for the regions of bad
*     values by forming successive approximations to a solution of
*     Laplace's equation, with the surrounding valid data providing the
*     boundary conditions.

*  Usage:
*     fillbad in out [niter] [size]

*  ADAM Parameters:
*     BLOCK = _INTEGER (Read)
*        The maximum number of pixels along either dimension when the
*        array is divided into blocks for processing.  It is ignored
*        unless MEMORY=TRUE.  This must be at least 256.  [512]
*     CNGMAX = _DOUBLE (Write)
*        The maximum absolute change in output values which occurred in
*        the final iteration.
*     CNGRMS = _DOUBLE (Write)
*        The root-mean-squared change in output values which occurred
*        in the last iteration.
*     IN = NDF (Read)
*        The NDF containing the input image with bad values.
*     MEMORY = _LOGICAL (Read)
*        If this is FALSE, the whole array is processed at the same
*        time.  If it is TRUE, the array is divided into chunks whose
*        maximum dimension along an axis is given by parameter BLOCK.
*        [FALSE]
*     NITER = INTEGER (Given)
*        The number of iterations of the relaxation algorithm.  This
*        value cannot be less than two, since this is the minimum
*        number required to ensure that all bad values are assigned a
*        replacement value.  The more iterations used, the finer the
*        detail in the replacement function and the closer it will
*        match the surrounding good data.  [2]
*     OUT = NDF (Write)
*        The NDF to contain the image free of bad values.
*     SIZE( ) = _REAL (Read)
*        The initial scale lengths in pixels to be used in the first
*        iteration, along each axis.  If fewer values are supplied than
*        pixel axes in the NDF, the last value given is repeated for
*        the remaining axes.  The size 0 means no fitting across a
*        dimension.  For instance, [0,0,5] would be appropriate if the
*        spectra along the third dimension of a cube are independent,
*        and the replacement values are to be derived only within each
*        spectrum.
*
*        For maximum efficiency, a scale length should normally have a
*        value about half the `size' of the largest invalid region to
*        be replaced.  (See the Notes section for more details.) [5.0]
*     TITLE = LITERAL (Read)
*        The title of the output NDF.  A null (!) value means using the
*        title of the input NDF. [!]
*     VARIANCE = _LOGICAL (Read)
*        If VARIANCE is TRUE, variance information is to be propagated;
*        any bad values therein are filled.  Also the variance is used
*        to weight the calculation of the replacement data values.  If
*        VARIANCE is FALSE, there will be no variance processing thus
*        requiring two less arrays in memory.  This parameter is only
*        accessed if the input NDF contains a VARIANCE component.
*        [TRUE]

*  Examples:
*     fillbad aa bb
*       The NDF called aa has its bad pixels replaced by good values
*       derived from the surrounding good pixel values using two
*       iterations of a relaxation algorithm.  The initial scale length
*       is 5 pixels.  The resultant NDF is called bb.
*     fillbad aa bb 6 20 title="Cleaned image"
*       As above except the initial scale length is 20 pixels, 5
*       iterations will be performed, and the output title is "Cleaned
*       image" instead of the title of NDF aa.
*     fillbad aa bb memory novariance
*       As in the first example except that processing is performed
*       with blocks up to 512 by 512 pixels to reduce the memory
*       requirements, and no variance information will be used or
*       propagated.
*     fillbad in=speccube out=speccube_fill size=[0,0,128] iter=5
*       Suppose NDF speccube is a spectral imaging cube with the
*       spectral axis third.  This example replaces the bad pixels by
*       valid values derived from the surrounding good pixel values
*       within each spectrm, using an initial scale length of 128
*       channels, iterating five times.  The filled NDF is called
*       speccube_fill.
*     fillbad in=speccube out=speccube_fill size=[5,5,128] iter=5
*       As the previous example, but now the relaxation occurs along
*       the spatial axes too, initially with a scale length of five
*       pixels.

*  Notes:
*     -  The algorithm is based on the relaxation method of repeatedly
*     replacing each bad pixel with the mean of its two nearest
*     neighbours along each pixel axis.  Such a method converges to the
*     required solution, but information about the good regions only
*     propagates at a rate of about one pixel per iteration into the
*     bad regions, resulting in slow convergence if large areas are to
*     be filled.
*
*     This application speeds convergence to an acceptable function by
*     forming the replacement mean from all the pixels in the same
*     axis (such as a row or a column), using a weight which decreases
*     exponentially with distance and goes to zero after the first good
*     pixel is encountered in any direction.  If there is variance
*     information, this is included in the weighting so as to give more
*     weight to surrounding values with lower variance.  The scale
*     length of the exponential weight is initially set large, to allow
*     rapid propagation of an approximate `smooth' solution into the
*     bad regions---an initially acceptable solution is thus rapidly
*     obtained (often in the first one or two iterations).  The scale
*     length is subsequently reduced by a factor of 2 whenever the
*     maximum absolute change occurring in an iteration has decreased
*     by a factor of 4 since the current scale length was first used.
*     In this way, later iterations introduce progressively finer
*     detail into the solution.  Since this fine detail occurs
*     predominantly close to the `crinkly' edges of the bad regions,
*     the slower propagation of the solution in the later iterations is
*     then less important.
*
*     When there is variance processing the output variance is
*     reassigned if either the input variance or data value was bad.
*     Where the input value is good but its associated variance is bad,
*     the calculation proceeds as if the data value were bad, except
*     that only the variance is substituted in the output.  The new
*     variance is approximated as twice the inverse of the sum of the
*     weights.
*     -  The price of the above efficiency means that considerable
*     workspace is required, typically two or three times the size of
*     the input image, but even larger for the one and two-byte integer
*     types.  If memory is at a premium, there is an option to process
*     in blocks (cf. parameter MEMORY).  However, this may not give as
*     good results as processing the array in full, especially when the
*     bad-pixel regions span blocks.
*     -  The value of the parameter SIZE is not critical and the
*     default value will normally prove effective.  It primarily
*     affects the efficiency of the algorithm on various size scales.
*     If the smoothing scale is set to a large value, large scale
*     variations in the replacement function are rapidly found, while
*     smaller scale variations may require many iterations.
*     Conversely, a small value will rapidly produce the small scale
*     variations but not the larger scale ones.  The aim is to select
*     an initial value SIZE such that during the course of a few
*     iterations, the range of size scales in the replacement function
*     are all used.  In practice this means that the value of SIZE
*     should be about half the size of the largest scale variations
*     expected.  Unless the valid pixels are very sparse, this is
*     usually determined by the `size' of the largest invalid region to
*     be replaced.
*     -  An error results if the input NDF has no bad values to replace.
*     -  The progress of the iterations is reported at the normal
*     reporting level.  The format of the output is slightly different
*     if the scale lengths vary with pixel axis; an extra axis column
*     is included.

*  Related Applications:
*     KAPPA: CHPIX, GLITCH, MEDIAN, ZAPLIN; Figaro: BCLEAN,
*     COSREJ, CLEAN, ISEDIT, MEDFILT, MEDSKY, REMBAD, TIPPEX.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of an NDF
*     data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.  The output bad-pixel flag is set to indicate no bad
*     values in the data and variance arrays.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using single- or double-precision floating point as
*     appropriate.

*  Timing:
*     The time taken increases in proportion to the value of NITER.
*     Adjusting the SIZE parameter to correspond to the largest regions
*     of bad values will reduce the processing time.  See the Notes
*     section.

*  Copyright:
*     Copyright (C) 1995, 1998-1999, 2001, 2004 Central Laboratory of
*     the Research Councils. All Rights Reserved.

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
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1995 April 16 (MJC):
*        Original version.  Some of the documentation was derived from
*        R.F. Warren-Smith's EDRS manual.
*     21-MAY-1998 (DSB):
*        Check that input variances are not all bad or zero, and ignore
*        them if they are.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     4-MAY-1999 (TDCA):
*        Badbits mask set to zero.
*     23-MARCH-2001 (DSB):
*        Reset the smoothing size before processing each block.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2007 October 11 (MJC):
*        Made to work with n-dimensional NDFs.  Added final contextual
*        error message.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER DEFSIZ             ! Default maximum dimension when
                                 ! blocking
      PARAMETER ( DEFSIZ = 512 )

      BYTE BADBIT                ! The value the bad-bits mask of the output
      PARAMETER ( BADBIT = 0 )   ! NDF is set to after processing.


*  Local Variables:
      LOGICAL BAD                ! Bad pixels present?
      INTEGER BLDIM( NDF__MXDIM ) ! Size of an block in each dimension
      INTEGER BLKSIZ             ! Maximum dimension when blocking
                                 ! per block
      LOGICAL BLOCK              ! Process in blocks?
      DOUBLE PRECISION CNGMAX    ! Maximum change of value at last iteration
      DOUBLE PRECISION CNGRMS    ! RMS change at last iteration per block
      DOUBLE PRECISION CNGSUM    ! Some of weighted square changes at last
                                 ! iteration
      CHARACTER*13 COMP          ! The array component(s) of NDF
      INTEGER DIM( NDF__MXDIM )  ! Size of the image in each dimension
      DOUBLE PRECISION DMNV      ! Min variance value
      DOUBLE PRECISION DMXV      ! Max variance value
      CHARACTER*( NDF__SZFTP ) DTYPE ! Numeric type for output arrays
      INTEGER EL                 ! Number of elements in mapped array
      INTEGER I                  ! Loop index
      INTEGER II                 ! Loop index
      INTEGER IDIM               ! Actual number of dimensions of image
      CHARACTER*( NDF__SZTYP ) ITYPE ! Numeric type for processing
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of block
      DOUBLE PRECISION MAXCNG    ! Maximum change of value at last iteration
      INTEGER MNVP               ! Index of min variance value
      INTEGER MXVP               ! Index of max variance value
      INTEGER NBAD               ! Number of bad values replaced in a block
      INTEGER NBLOCK             ! Number of blocks the array is divided into
                                 ! for processing
      INTEGER NDFBI              ! NDF identifier for input image block
      INTEGER NDFBO              ! NDF identifier for output image block
      INTEGER NDFI               ! NDF identifier for input image
      INTEGER NDFO               ! NDF identifier for output image
      INTEGER NITER              ! Number of iterations
      INTEGER NSIZE              ! Number of scale lengths supplied
      INTEGER NVBAD              ! Number of bad variance values
      INTEGER PNTRI( 2 )         ! Pointer to the mapped data and variance of
                                 ! the input image
      INTEGER PNTRO( 2 )         ! Pointer to the mapped data and variance of
                                 ! the output image
      INTEGER PNTW1              ! Pointer to work space
      INTEGER PNTW2              ! Pointer to work space
      INTEGER PNTW3              ! Pointer to work space
      INTEGER PNTW4              ! Pointer to work space
      LOGICAL QUAL               ! Quality array present in output NDF ?
      REAL RMNV                  ! Min variance value
      DOUBLE PRECISION RMSCNG    ! RMS change at last iteration
      REAL RMXV                  ! Max variance value
      INTEGER SDIM( NDF__MXDIM ) ! Significant NDF dimensions
      REAL SIZE( NDF__MXDIM )    ! Scale lengths
      REAL SIZEF( NDF__MXDIM )   ! Final scale lengths
      INTEGER SLBND( NDF__MXDIM ) ! Lower bounds of significant dimensions
      INTEGER SUBND( NDF__MXDIM ) ! Upper bounds of significant dimensions
      INTEGER TOTBAD             ! Total number of bad values replaced
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of block
      LOGICAL VAR                ! Variance processing?
      INTEGER WKSIZE             ! Size of smaller work arrays

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See whether or not chunking is required.
*  ========================================

*  See if memory is at a premium.
      CALL PAR_GET0L( 'MEMORY', BLOCK, STATUS )
      IF ( BLOCK ) CALL PAR_GDR0I( 'BLOCK', DEFSIZ, 256, VAL__MAXI,
     :  .TRUE., BLKSIZ, STATUS )

*  Obtain the input NDF.
*  =====================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get identifier for the NDF containing the input image.  Find whether
*  or not there are no more than two significant dimensions and which
*  ones they are.  Determine its dimensions (note that only two
*  significant dimensions can be accommodated).  Then ignore
*  non-significant dimensions.
      CALL KPG1_GTNDF( 'IN', NDF__MXDIM, .FALSE., 'READ', NDFI, SDIM,
     :                 SLBND, SUBND, STATUS )

*  Find the number of dimensions.
      CALL NDF_DIM( NDFI, NDF__MXDIM, BLDIM, IDIM, STATUS )
      DO I = 1, IDIM
         DIM( I ) = SUBND( I ) - SLBND( I ) + 1
      END DO

*  Determine the size of the smaller work arrays.  It's for sums
*  along each dimension.
      WKSIZE = 1
      IF ( IDIM .GT. 1 ) THEN
         DO I = 1, IDIM - 1
            WKSIZE = WKSIZE * DIM( I )
         END DO
      END IF

*  Exit if an error occurred.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Check that there are bad pixels.
*  ================================
      CALL NDF_BAD( NDFI, 'Data', .TRUE., BAD, STATUS )
      IF ( .NOT. BAD ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', NDFI )
         CALL ERR_REP( 'FILLBAD_NOBAD', 'There are no bad values to '/
     :     /'replace in the data array of NDF ^NDF.', STATUS )
         GOTO 999
      END IF

*  Allow for variance information.
*  ===============================

*  Check the state of variance component of the NDF.
      CALL NDF_STATE( NDFI, 'Variance', VAR, STATUS )

*  See if output NDF is to have a VARIANCE component, provided the
*  input NDF has a variance array.
      IF ( VAR ) CALL PAR_GET0L( 'VARIANCE', VAR, STATUS )

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
*  floating-point processing.
      CALL NDF_MTYPE( '_REAL,_DOUBLE', NDFI, NDFI, COMP, ITYPE, DTYPE,
     :                STATUS )

*  Obtain the parameter values.
*  ============================

*  See if memory is at a premium.  If it is the image is divided into
*  blocks that are processed separately.
      CALL PAR_GET0L( 'MEMORY', BLOCK, STATUS )

*  Obtain the number of iterations.
      CALL PAR_GDR0I( 'NITER', 2, 2, 100, .FALSE., NITER, STATUS )

*  Obtain the initial smoothing sizes along each axis.  Pad missing
*  values with the last value.  Thus a single value suppled will apply
*  to all dimensions.
      CALL PAR_DEF1R( 'SIZE', 1, 5.0, STATUS )
      CALL PAR_GDRVR( 'SIZE', IDIM, 0.0, 1.0E6, SIZE, NSIZE, STATUS )
      IF ( NSIZE .LT. IDIM ) THEN
         DO I = NSIZE + 1, IDIM
            SIZE( I ) = SIZE( NSIZE )
         END DO
      END IF

*  Exit if an error occurred.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Create the output NDF and find the number of blocks.
*  ====================================================

*  Create a new output NDF to contain the cleaned image, which
*  inheriting all the attributes of the input NDF.
      CALL LPG_PROP( NDFI, 'WCS,Axis,Units,Quality', 'OUT', NDFO,
     :               STATUS )

*  Set an appropriate numeric type for the output arrays.
      CALL NDF_STYPE( DTYPE, NDFO, COMP, STATUS )

*  Get the new title for the output image and insert it into the output
*  NDF.  The input NDF's title was already propagated by the LPG_PROP
*  call and so a null value will leave it unaltered.
      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )

*  Set up the block sizes for each dimension.  BLDIM already contains
*  the full dimensions, so it only needs to be altered for blocking in
*  the significant dimensions.
      IF ( BLOCK ) THEN
         DO I = 1, IDIM
            IF ( DIM( I ) .GT. 1 ) BLDIM( I ) = BLKSIZ
         END DO
      END IF

*  Find the number of blocks required.
      CALL NDF_NBLOC( NDFI, IDIM, BLDIM, NBLOCK, STATUS )

*  Main loop.
*  ==========

*  Initialise change summation.
      CNGSUM = 0.0D0

*  Loop through the blocks, creating a section to refer to each one.
      DO I = 1, NBLOCK
         CALL NDF_BLOCK( NDFI, IDIM, BLDIM, I, NDFBI, STATUS )
         CALL NDF_BLOCK( NDFO, IDIM, BLDIM, I, NDFBO, STATUS )

*  Obtain the significant dimensions of the block.
         CALL NDF_BOUND( NDFBI, NDF__MXDIM, LBND, UBND, IDIM, STATUS )
         DO II = 1, IDIM
            DIM( II ) = UBND( SDIM( II ) ) - LBND( SDIM( II ) ) + 1
         END DO

*  If blocking, tell the user which block is being done.
         IF( BLOCK ) THEN
            CALL MSG_BLANKIF( MSG__NORM, STATUS )

            DO II = 1, IDIM
               CALL MSG_SETI( 'SEC', LBND( II ) )
               CALL MSG_SETC( 'SEC', ':' )
               CALL MSG_SETI( 'SEC', UBND( II ) )
               IF( II .NE. IDIM ) CALL MSG_SETC( 'SEC', ',' )
            END DO

            CALL MSG_SETI( 'I', I )
            CALL MSG_SETI( 'N', NBLOCK )

            CALL MSG_OUTIF( MSG__NORM, ' ', '      Block ^I of ^N '//
     :                      '(^SEC)', STATUS )

         END IF

*  Map the input arrays.
         CALL KPG1_MAP( NDFBI, COMP, ITYPE, 'READ', PNTRI, EL, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  If variances are available in the input, check that they are not all
*  bad or zero. If so, issue a warning and do not produce output variances.
         IF( VAR ) THEN

            IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPG1_MXMNR( .TRUE., EL,
     :                          %VAL( CNF_PVAL( PNTRI( 2 ) ) ), NVBAD,
     :                          RMXV, RMNV, MXVP, MNVP, STATUS )

               IF( RMXV .NE. VAL__BADR ) THEN
                  DMXV = DBLE( RMXV )
               ELSE
                  DMXV = VAL__BADD
               END IF

            ELSE
               CALL KPG1_MXMND( .TRUE., EL,
     :                          %VAL( CNF_PVAL( PNTRI( 2 ) ) ), NVBAD,
     :                          DMXV, DMNV, MXVP, MNVP, STATUS )
            END IF

*  If an error occurred above (eg if all variance values are bad), annull
*  the error and ignore the variances.
            IF( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
               VAR = .FALSE.

*  Otherwise, check the maximum variance value is greater than zero.
            ELSE IF( DMXV .EQ. VAL__BADD .OR. DMXV .EQ. 0.0 ) THEN
               VAR = .FALSE.
            END IF

*  If we are ignoring variances, warn the user.
            IF( .NOT. VAR ) THEN
               CALL MSG_BLANK( STATUS )
               CALL MSG_OUT( ' ','FILLBAD: Warning - all input '//
     :                    'variances are zero or bad and will not be '//
     :                    'used. No output variances will be created.',
     :                    STATUS )
               CALL MSG_BLANK( STATUS )
               COMP = 'Data'

            END IF
         END IF

*  Map the output arrays.
         CALL KPG1_MAP( NDFBO, COMP, ITYPE, 'WRITE', PNTRO, EL, STATUS )

*  Create and map work space for the filtering.
*  ============================================

*  Get the required work space for to hold the sums and weights.
         CALL PSX_CALLOC( EL, ITYPE, PNTW1, STATUS )
         CALL PSX_CALLOC( EL, ITYPE, PNTW2, STATUS )
         CALL PSX_CALLOC( WKSIZE, ITYPE, PNTW3, STATUS )
         CALL PSX_CALLOC( WKSIZE, ITYPE, PNTW4, STATUS )

*  Perform the filtering.
*  ======================

*  Reset the smoothing size for this block.  The scale length is
*  modified in the infill routines below.
         DO II = 1, IDIM
            SIZEF( II ) = SIZE( II )
         END DO

*  Reject pixels deviating from their local mean by more than the
*  threshold calling the routine of the appropriate data type.
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_BAFNR( IDIM, DIM, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       VAR, %VAL( CNF_PVAL( PNTRI( 2 ) ) ),
     :                       NITER, SIZEF, CNGMAX, CNGRMS, NBAD,
     :                       %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       %VAL( CNF_PVAL( PNTRO( 2 ) ) ),
     :                       %VAL( CNF_PVAL( PNTW1 ) ),
     :                       %VAL( CNF_PVAL( PNTW2 ) ),
     :                       %VAL( CNF_PVAL( PNTW3 ) ),
     :                       %VAL( CNF_PVAL( PNTW4 ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_BAFND( IDIM, DIM, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       VAR, %VAL( CNF_PVAL( PNTRI( 2 ) ) ),
     :                       NITER, SIZEF, CNGMAX, CNGRMS, NBAD,
     :                       %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       %VAL( CNF_PVAL( PNTRO( 2 ) ) ),
     :                       %VAL( CNF_PVAL( PNTW1 ) ),
     :                       %VAL( CNF_PVAL( PNTW2 ) ),
     :                       %VAL( CNF_PVAL( PNTW3 ) ),
     :                       %VAL( CNF_PVAL( PNTW4 ) ), STATUS )

         END IF

*  Release the work spaces.
         CALL PSX_FREE( PNTW1, STATUS )
         CALL PSX_FREE( PNTW2, STATUS )
         CALL PSX_FREE( PNTW3, STATUS )
         CALL PSX_FREE( PNTW4, STATUS )

*  Annul the section identifiers.
         CALL NDF_ANNUL( NDFBI, STATUS )
         CALL NDF_ANNUL( NDFBO, STATUS )

*  Find the global maximum change, and sum the weighted mean squared
*  difference at the last iteration and number of bad values replaced.
         MAXCNG = MAX( MAXCNG, CNGMAX )
         TOTBAD =TOTBAD + NBAD
         CNGRMS = CNGSUM + CNGRMS * CNGRMS * NBAD
      END DO

*  Find the approximate global rms change at the last iteration.
      RMSCNG = SQRT( CNGSUM / DBLE( TOTBAD ) )

*  Output the RMS change at the last iteration.
      CALL PAR_PUT0D( 'CNGRMS', RMSCNG, STATUS )

*  Output the maximum change at the last iteration.
      CALL PAR_PUT0D( 'CNGMAX', MAXCNG, STATUS )

*  Set the bad-pixel flags to indicate that there are no longer any bad
*  pixels.
      CALL NDF_SBAD( .FALSE., NDFO, 'Data', STATUS )
      IF ( VAR ) CALL NDF_SBAD( .FALSE., NDFO, 'Variance', STATUS )

*  If QUALITY array present in NDFO, set badbits mask to zero.
      CALL NDF_STATE( NDFO, 'QUAL', QUAL, STATUS )
      IF ( QUAL ) THEN
         CALL NDF_SBB( BADBIT, NDFO, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 999
      END IF

  999 CONTINUE

* End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report contextual information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FILLBAD_ERR',
     :     'FILLBAD: Error filling bad-pixel regions of an NDF '/
     :     /'using a relaxation algorithm.', STATUS )
      END IF

*  End the routine.

      END
