      SUBROUTINE MEDIAN( STATUS )
*+
*  Name:
*     MEDIAN

*  Purpose:
*     Smooths a 2-dimensional data array using a weighted median filter.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL MEDIAN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This task filters the 2-dimensional data array in the input NDF
*     structure with a Weighted Median Filter (WMF) in a 3-by-3-pixel
*     kernel to create a new NDF.  There are a number of predefined
*     weighting functions and parameters that permit other symmetric
*     weighting functions.  See Parameter MODE and the topic
*     "User-defined Weighting Functions".
*
*     A threshold for replacement of a value by the median can be set.
*     If the absolute value of the difference between the actual value
*     and the median is less than the threshold, the replacement will
*     not occur.  The array boundary is dealt by either pixel
*     replication or a reflection about the edge pixels of the array.
*
*     The WMF can be repeated iteratively a specified number of times,
*     or it can be left to iterate continuously until convergence is
*     achieved and no further changes are made to the data.  In the
*     latter case a damping algorithm is used if the number of
*     iterations exceeds some critical value, which prevents the result
*     oscillating between two solutions (which can sometimes happen).
*     When damping is switched on data values are replaced not by the
*     median value, but by a value midway between the original and the
*     median.
*
*     Bad pixels are not included in the calculation of the median.
*     There is a defined threshold which specifies minimum-allowable
*     median position as a fraction of the median position when there
*     are no bad pixels.  For neighbourhoods with too many bad pixels,
*     and so the median position is too small, the resulting output
*     pixel is bad.

*  Usage:
*     median in out [mode] [diff] [bound] [numit] corner side centre

*  ADAM Parameters:
*     BOUND = LITERAL (Read)
*        Determines the type of padding required at the array edges
*        before the filtering starts.  The alternatives are described
*        below.
*
*          "Replication" - The values at the edge of the data array
*                          are replicated into the padded area.  For
*                          example, with STEP=2 one corner of the
*                          original and padded arrays would appear
*                          as follows:
*                                                        1 1 1 1 1 1 1
*                                                        1 1 1 1 1 1 1
*               corner of     1 1 1 1 1   corresponding  1 1 1 1 1 1 1
*               original      1 2 2 2 2   corner of      1 1 1 2 2 2 2
*               array:        1 2 3 3 3   padded array:  1 1 1 2 3 3 3
*                             1 2 3 4 4                  1 1 1 2 3 4 4
*                             1 2 3 4 5                  1 1 1 2 3 4 5
*
*          "Reflection"  - The values near the edge of the data array
*                          are reflected about the array's edge pixels.
*                          For example, with STEP=2 one corner of the
*                          original and padded arrays would appear as
*                          follows:
*                                                        3 2 1 2 3 3 3
*                                                        2 2 1 2 2 2 2
*                  corner of  1 1 1 1 1   corresponding  1 1 1 1 1 1 1
*                  original   1 2 2 2 2   corner of      2 2 1 2 2 2 2
*                  array:     1 2 3 3 3   padded array:  3 2 1 2 3 3 3
*                             1 2 3 4 4                  3 2 1 2 3 4 4
*                             1 2 3 4 5                  3 2 1 2 3 4 5
*
*         ["Replication"]
*     CENTRE = _INTEGER (Read)
*        Central value for weighting function, required if MODE = -1.
*        It must be an odd value in the range 1 to 21. [1]
*     CORNER = _INTEGER (Read)
*        Corner value for weighting function, required if MODE = -1.
*        It must be in the range 0 to 10. [1]
*     DIFF  = _DOUBLE (Read)
*        Replacement of a value by the median occurs if the absolute
*        difference of the value and the median is greater than DIFF.
*        [0.0]
*     IN = NDF (Read)
*        NDF structure containing the 2-dimensional data array to be
*        filtered.
*     ITERATE = LITERAL (Read)
*        Determines the type of iteration used.  The alternatives are
*        described below.
*
*          "Specified"   - You specify the number of iterations
*                          at each step size in the Parameter NUMIT.
*
*          "Continuous"  - The filter iterates continuously until
*                          convergence is achieved and the array is no
*                          longer changed by the filter.  A damping
*                          algorithm comes into play after MAXIT
*                          iterations, and the filter will give up
*                          altogether after MAXIT * 1.5 iterations
*                          (rounded up to the next highest integer).
*
*        "Continuous" mode is recommended only for images which are
*        substantially smooth to start with (such as a sky background
*        frame from a measuring machine).  Complex images may take many
*        iterations, and a great deal of time, to converge.
*        ["Specified"]
*     MAXIT = _INTEGER (Read)
*        The maximum number of iterations of the filter before the
*        damping algorithm comes into play, when ITERATE =
*        "Continuous".  It must lie in the range 1 to 30.  [10]
*     MEDTHR = _REAL (Read)
*        Minimum-allowable actual median position as a fraction of the
*        median position when there are no bad pixels, for the
*        computation of the median at a given pixel. [0.8]
*     MODE = _INTEGER (Read)
*        Determines type of weighting used, -1 allows you to define the
*        weighting, and 0 to 7 the predefined filters.  The predefined
*        modes have the following weighting functions:
*
*          0:  1 1 1   1:  0 1 0   2:  1 0 1   3:  1 1 1   4:  0 1 0
*              1 1 1       1 1 1       0 1 0       1 3 1       1 3 1
*              1 1 1       0 1 0       1 0 1       1 1 1       0 1 0
*
*          5:  1 0 1   6:  1 2 1   7:  1 3 1
*              0 3 0       2 3 2       3 3 3
*              1 0 1       1 2 1       1 3 1
*
*        [0]
*     NUMIT = _INTEGER (Read)
*        The specified number of iterations of the filter, when ITERATE
*        = "Specified".  [1]
*     OUT = NDF (Write)
*        NDF structure to contain the 2-dimensional data array after
*        filtering.
*     SIDE = _INTEGER (Read)
*        Side value for weighting function, required if MODE = -1.
*        It must be in the range 0 to 10. [1]
*     STEP() = _INTEGER (Read)
*        The spacings between the median filter elements to be used.
*        The data may be filtered at one particular spacing by
*        specifying a single value, such as STEP=4, or may be filtered
*        at a whole series of spacings in turn by specifying a list of
*        values, such as STEP=[4,3,2,1].  There is a limit of 32 values.
*        [1]
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the NDF supplied for Parameter IN to be used
*        instead. [!]

*  Examples:
*     median a100 a100med
*        This applies an equally weighted median filter to the NDF
*        called a100 and writes the result to the NDF a100med.  It uses
*        the default settings, which are a single step size of one
*        pixel, and a difference threshold of 0.0.  The task pads the
*        array by replication to deals with the edge pixels, and runs
*        the filter once only.
*     median a100 a100med bound=ref
*        As in the previous example except that it uses reflection
*        rather than replication when padding the array.
*     median abc sabc mode=3 step=4 diff=1.0 numit=2
*        This applies a median filter to the NDF called abc with a
*        1 1 1
*        1 3 1  weighting mask (MODE=3), a step size of 4 pixels
*        1 1 1
*        (STEP=4) and a difference threshold of 1.0 (DIFF=1.0).  It
*        runs the filter twice (NUMIT=2) and writes the result to
*        the NDF called sabc.
*     median abc sabc mode=3 step=[4,3,2,1] diff=1.0 numit=2
*         This applies a median filter as in the previous example,
*         only this time run the filter at step sizes of 4, 3, 2,
*         and 1 pixels, in that order (STEP=[4,3,2,1]).  It runs the
*         filter twice at each step size (NUMIT=2).  Note that the
*         filter will be run a total of EIGHT times (number of step
*         sizes times the number of iterations).
*     median in=spotty step=[4,3,2,1] iterate=cont maxit=6 out=clean
*         This applies a median filter to the NDF called spotty with
*         the default settings for the mode and difference threshold.
*         It runs the filter at step sizes of 4, 3, 2, and 1 pixels,
*         operating continuously at each step size until the result
*         converges (ITERATE=CONT).  Damping will begin after 6
*         iterations (MAXIT=6), and the filtering will stop regardless
*         after 10 iterations (1 + INT(1.5 * MAXIT)).  Note that the
*         filter will run an indeterminate number of times, up to a
*         maximum of 40 (number of step sizes * maximum number of
*         iterations), and may take a long time.  The resultant data
*         array are written to the NDF called clean.

*  User-defined Weighting Functions:
*     Parameters CORNER, SIDE, and CENTRE allow other symmetric
*     functions in addition to those offered by MODE=0 to 7.  A step
*     size has to be specified too; this determines the spacing of the
*     elements of the weighting function. The data can be filtered at
*     one step size only, or using a whole series of step sizes in
*     sequence.  The weighting function has the form:
*
*               %CORNER  .   %SIDE   .  %CORNER
*                  .           .           .
*                %SIDE   .  %CENTRE  .   %SIDE
*                  .           .           .
*               %CORNER  .   %SIDE   .  %CORNER
*
*     The . indicates that the weights are separated by the
*     stepsize-minus-one zeros.

*  Related Applications:
*     KAPPA: BLOCK, CONVOLVE, FFCLEAN, GAUSMOOTH; Figaro: ICONV3,
*     ISMOOTH, IXSMOOTH, MEDFILT.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, LABEL, TITLE,
*     UNITS, WCS and HISTORY components of an NDF data structure and
*     propagates all extensions.  VARIANCE is not used to weight the
*     median filter and is not propagated.  QUALITY is also lost.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.

*  Copyright:
*     Copyright (C) 1983-1984, 1986-1989, 1991-1993 Science &
*     Engineering Research Council. Copyright (C) 1995, 1998, 2004
*     Central Laboratory of the Research Councils.
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     DB: Dave Baines (ROE)
*     MJC: Malcolm J. Currie (STARLINK)
*     SMB: Steven Beard (ROE)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     20/10/1983 (DB):
*       Original version.
*     17/02/1984 (DB):
*        Modified to use TITLE component.
*     1986 August 7 (MJC):
*        Standardised prologue formatting. Made 10th argument of MEDWTS
*        the 12th.  Added status check on entry.
*     1986 August 29 (MJC):
*        Replaced KERGEN routines by their nearest AIF_ equivalent.
*        Added arguments section to the prologue, added extra parameter
*        MEDTHR and tidied.
*     1987 October 15 (MJC):
*        Reordered tidying and extra status checks.
*     1988 March 8 (MJC):
*        Substituted AIF_ANTMP to annul workspace.
*     1988 March 17 (MJC):
*        Referred to `array' rather than `image'.
*     1988 June 8 (MJC):
*        More reporting of error context.
*     1989 June 13 (MJC):
*        Allow for processing primitive NDFs.
*     1989 August  8 (MJC):
*        Passed array dimensions as separate variables to COPY2D,
*        MEDREF, MEDREP and MEDWTS.
*     1989 December 21 (MJC):
*        Workspace managed by AIF_TEMP.
*     1991 October 25 (MJC):
*        Propagates AXIS, UNITS, LABEL and HISTORY.
*     1992 February 25 (MJC):
*        Limited processing of simple NDFs.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1993 July 20 (SMB):
*        SKYMEDIAN cloned from MEDIAN.  KPG1 calls replaced by NDF.
*        COSMOS sky-background-filtering algorithm used.  Instead of
*        having a specified number of iterations this algorithm will
*        iterate until no further changes are made to the data, with a
*        damping algorithm coming into play after a certain number of
*        iterations.  Parameter NUMIT replaced by MAXIT.  1.0E38
*        replaced by VAL__MAXR.
*     1993 July 21 (SMB):
*        SKYMEDIAN combined with MEDIAN to create a more general median
*        filtering program which can replace MEDIAN (as suggested by
*        Malcolm Currie).  ITERATION parameter introduced. Parameter
*        NUMIT reinstated.  Parameter STEP converted from a scalar to a
*        vector, which will allow the filter to run at a whole series
*        of step sizes.
*     1995 July 27 (MJC):
*        Made to operate on all numeric data types.  Used a modern-style
*        prologue with additional topics.  Modern-style coding.  Made
*        messages conditional.  Removed a couple of unnecessary calls.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     16-SEP-1998 (DSB):
*        Correction to component list passed to LPG_PROP; ">WCS" replaced
*        by ",WCS".
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     7-SEP-2006 (DSB):
*        Initialise NSTEP before getting Parameter STEP, in order to avoid
*        seg faults in the event of an earlier error.
*     2012 May 8 (MJC):
*        Add _INT64 support.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! no implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
      INCLUDE 'PAR_ERR'          ! PAR__ error constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External References:
      LOGICAL CHR_SIMLR

*  Local Constants:
      INTEGER DCENT              ! Default CENTRE value
      PARAMETER ( DCENT = 3 )

      INTEGER DCORN              ! Default CORNER value
      PARAMETER ( DCORN = 1 )

      INTEGER DMAXIT             ! Default MAXIT value
      PARAMETER ( DMAXIT = 10 )

      INTEGER DMODE              ! Default MODE value
      PARAMETER ( DMODE = 3 )

      INTEGER DNUMIT             ! Default NUMIT value
      PARAMETER ( DNUMIT = 1 )

      INTEGER DSIDE              ! Default SIDE value
      PARAMETER ( DSIDE = 1 )

      INTEGER NDIM               ! Dimensionality of input/output arrays
      PARAMETER ( NDIM = 2 )

      INTEGER MAXSTP             ! Maximum number of step sizes
      PARAMETER ( MAXSTP = 32 )

      INTEGER SAMSIZ             ! Maximum sample size for sorting
      PARAMETER ( SAMSIZ = 9 )

      DOUBLE PRECISION DDIFF     ! Default DIFF value
      PARAMETER ( DDIFF = 0.0D0 )

      REAL DMEDT                 ! Default MEDTHR value
      PARAMETER ( DMEDT = 0.8 )

*  Local Variables:
      INTEGER BIGSTP             ! The largest step size given.
      CHARACTER * ( 10 ) BOUND   ! Determines what to do at array boundary
      INTEGER CENTRE             ! Central value of wmf weighting function
      LOGICAL CHANGE             ! Whether the data array has changed
                                 ! or not during an iteration?
      INTEGER CORNER             ! Corner value of wmf weighting function
      LOGICAL DAMP               ! Damping should be applied?
      DOUBLE PRECISION DIFF      ! If abs( value - median ) > diff then
                                 ! replacement occurs
      INTEGER DIMS( NDIM )       ! Dimensions of NDF
      INTEGER EL                 ! Number of elements
      LOGICAL FINIS              ! Finish the iteration loop?
      INTEGER IS                 ! Step array index
      INTEGER ITERAT             ! Iteration counter
      CHARACTER * ( 10 ) ITERATE ! Determines the iteration mode
      INTEGER LBND( NDIM )       ! Lower bounds of NDF
      INTEGER LIMIT              ! Absolute maximum number of iterations
      INTEGER MAXIT              ! Maximum number of iterations before damping
      INTEGER MEDPOS             ! Position of median in the sample with
                                 ! no bad pixels present
      REAL MEDTHR                ! Minimum-allowable fraction of MEDPOS
                                 ! for median will be computed at a
                                 ! pixel
      INTEGER MODE               ! Type of weighted median filter
      INTEGER MXSTEP             ! Maximum allowed stepsize
      INTEGER NDFI               ! Identifier for input NDF
      INTEGER NDFO               ! Identifier for output NDF
      INTEGER NSTEP              ! The number of different step sizes to use.
      INTEGER NUMIT              ! Specified number of iterations of the filter
      INTEGER NUMSAM             ! Number of elements in the sample
      INTEGER PNTRO              ! Pointer to output DATA_ARRAY
      INTEGER SAMINF( SAMSIZ, 3 ) ! Weights, offsets for sample elements
                                 ! (in MEDWTS)
      DOUBLE PRECISION SAMPLE( SAMSIZ ) ! Holds sample for sorting
      INTEGER SAMWT( SAMSIZ )    ! stores weights during sorting
      INTEGER SDIM( NDIM )       ! Indices of significant dimensions
      INTEGER SIDE               ! Side value of wmf weighting function
      INTEGER ST                 ! Current separation of weighted median filter
                                 ! elements
      INTEGER STEP( MAXSTP )     ! List of separations of weighted median filter
                                 ! elements
      CHARACTER * ( NDF__SZTYP ) TYPE ! Array component numeric type
      INTEGER UBND( NDIM )       ! Upper bounds of NDF
      INTEGER WDIMS( NDIM )      ! Dimensions of workspace array
      INTEGER WNDF               ! Identifier for temporary NDF
      INTEGER WPLACE             ! Placeholder for temporary NDF
      INTEGER WPNTR              ! Pointer to workspace

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the input NDF.
*  =====================

*  Begin the NDF context.
      CALL NDF_BEGIN

*  Get the NDF containing the input data.  There must be only two
*  significant dimensions.
      CALL KPG1_GTNDF( 'IN', NDIM, .TRUE., 'READ', NDFI, SDIM,
     :                 LBND, UBND, STATUS )

*  Obtain the size of the input data-array component.
      DIMS( 1 ) = UBND( SDIM( 1 ) ) - LBND( SDIM( 1 ) ) + 1
      DIMS( 2 ) = UBND( SDIM( 2 ) ) - LBND( SDIM( 2 ) ) + 1

*  Obtain the implementation type for the processing.
      CALL NDF_TYPE( NDFI, 'Data', TYPE, STATUS )

*  Obtain parameter values.
*  ========================

*  Obtain the value for the filter mode.  It must be in the range -1 to
*  7.  Suggest the default DMODE.
      CALL PAR_GDR0I( 'MODE', DMODE, -1, 7, .TRUE., MODE, STATUS )

*  If MODE is -1 then a user-defined weighting function will be input.
      IF ( MODE .EQ. -1 ) THEN

*  Get the weighting values.
         CALL PAR_GDR0I( 'CORNER', DCORN, 0, 10, .TRUE., CORNER,
     :                   STATUS )
         CALL PAR_GDR0I( 'SIDE', DSIDE, 0, 10, .FALSE., SIDE, STATUS )

*  The value for the centre CENTRE must be an odd number.
         CALL PAR_GODD( 'CENTRE', DCENT, 1, 21, .TRUE., CENTRE,
     :                        STATUS )
      END IF

*  Calculate the maximum-possible step value.
      MXSTEP = ( MIN( DIMS( 1 ), DIMS( 2 ) ) - 2 ) / 2

*  There must be between 1 and MAXSTP step values, and each one must be
*  in the range 1 to MXSTEP.
      NSTEP = 0
      CALL PAR_GDRVI( 'STEP', MAXSTP, 1, MAXSTP, STEP, NSTEP, STATUS )

*  Determine the largest step size specified.
      BIGSTP = STEP( 1 )
      DO IS = 2, NSTEP
         BIGSTP = MAX( BIGSTP, STEP(IS) )
      END DO

*  Value for Parameter DIFF must be in the range 0.0 to a huge
*  number.
      CALL PAR_GDR0D( 'DIFF', DDIFF, 0.0D0, VAL__MAXD, .TRUE., DIFF,
     :                STATUS )

*  Value for %MEDTHR must be in the range 0.5 to 1.0.
      CALL PAR_GDR0R( 'MEDTHR', DMEDT, 0.5, 1.0, .TRUE., MEDTHR,
     :                STATUS )

*  Find out what to do at the array boundary.
      CALL PAR_CHOIC( 'BOUND', 'Replicate', 'Replicate,Reflect',
     :                .TRUE., BOUND, STATUS )

*  Find out how the filter is to be iterated.
      CALL PAR_CHOIC( 'ITERATE', 'Specified', 'Specified,Continuous',
     :                .TRUE., ITERATE, STATUS )

*  Obtain either %MAXIT or %NUMIT according to %ITERATE, setting the
*  unused parameter to 0.  The mode can be tested later on by checking
*  which parameter is non-zero.
      IF ( CHR_SIMLR( 'CO', ITERATE( 1:2 ) ) ) THEN

*  Continuous iteration until convergence, using a damping algorithm
*  after %MAXIT iterations.  Value for %MAXIT must be in the range 1 to
*  30.
         CALL PAR_GDR0I( 'MAXIT', DMAXIT, 1, 30, .TRUE., MAXIT, STATUS )
         NUMIT = 0
      ELSE

*  Iteration a specified number of times, %NUMIT.  The value for %NUMIT
*  must be in the range 1 to 50.
         CALL PAR_GDR0I( 'NUMIT', DNUMIT, 1, 50, .TRUE., NUMIT, STATUS )
         MAXIT = 0
      END IF

*  Create the output NDF.
*  ======================

*  Create output NDF structure, propagating the DATA, AXIS, UNITS, WCS,
*  LABEL, TITLE, HISTORY and extensions from the input NDF.
      CALL LPG_PROP( NDFI, 'Data,Axis,Units,WCS', 'OUT', NDFO, STATUS )

*  Obtain a new title for the output NDF.
      CALL NDF_CINP( 'TITLE', NDFO, 'Title', STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Map the data arrays.
*  ====================

*  Map the data-array component in the output NDF
      CALL KPG1_MAP( NDFO, 'DATA', TYPE, 'UPDATE', PNTRO, EL, STATUS )

*  Calculate dimensions of workspace array required, from the largest
*  step size to be used.  This is needed for the padding.
      LBND( 1 ) = 1
      LBND( 2 ) = 1

      WDIMS( 1 ) = DIMS( 1 ) + ( 2 * BIGSTP )
      WDIMS( 2 ) = DIMS( 2 ) + ( 2 * BIGSTP )

*  Create and map the workspace array.
      CALL NDF_TEMP( WPLACE, STATUS )
      CALL NDF_NEW( TYPE, NDIM, LBND, WDIMS, WPLACE, WNDF, STATUS )
      CALL KPG1_MAP( WNDF, 'DATA', TYPE, 'WRITE', WPNTR, EL, STATUS )

*  Check for errors.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Filter the data at each specified step size in turn.
         DO IS = 1, NSTEP

            ST = STEP( IS )

*  Inform the user.
            CALL MSG_SETI( 'ST', ST )
            CALL MSG_OUTIF( MSG__NORM, 'MSG_STEP',
     :                     'Filtering at a step size of ^ST.', STATUS )

*  Set up the required weighting function for the filter.
            CALL KPS1_MDSET( MODE, ST, SAMSIZ, CENTRE, CORNER, SIDE,
     :                       MEDPOS, NUMSAM, SAMINF, STATUS )

*  Perform the weighted median filtering as many iterations as
*  necessary.
            ITERAT = 1
            LIMIT = 1 + INT( MAXIT * 1.5 )
            FINIS = .FALSE.

            DO WHILE ( ( .NOT.FINIS ) .AND. ( STATUS .EQ. SAI__OK ) )

*  Transfer the output array into the workspace.
               IF ( CHR_SIMLR( 'REF', BOUND( 1:3 ) ) ) THEN

*  Pad edges by reflection.  Call routine appropriate for the data
*  type.
                  IF ( TYPE .EQ. '_REAL' ) THEN
                     CALL KPS1_MDRFR( ST, DIMS( 1 ), DIMS( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                WDIMS( 1 ),
     :                                WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
                     CALL KPS1_MDRFB( ST, DIMS( 1 ), DIMS( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                WDIMS( 1 ),
     :                                WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                     CALL KPS1_MDRFD( ST, DIMS( 1 ), DIMS( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                WDIMS( 1 ),
     :                                WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                     CALL KPS1_MDRFI( ST, DIMS( 1 ), DIMS( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                WDIMS( 1 ),
     :                                WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_INT64' ) THEN
                     CALL KPS1_MDRFK( ST, DIMS( 1 ), DIMS( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                WDIMS( 1 ),
     :                                WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                     CALL KPS1_MDRFUB( ST, DIMS( 1 ), DIMS( 2 ),
     :                                 %VAL( CNF_PVAL( PNTRO ) ),
     :                                 WDIMS( 1 ),
     :                                 WDIMS( 2 ),
     :                                 %VAL( CNF_PVAL( WPNTR ) ),
     :                                 STATUS )

                  ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                     CALL KPS1_MDRFUW( ST, DIMS( 1 ), DIMS( 2 ),
     :                                 %VAL( CNF_PVAL( PNTRO ) ),
     :                                 WDIMS( 1 ),
     :                                 WDIMS( 2 ),
     :                                 %VAL( CNF_PVAL( WPNTR ) ),
     :                                 STATUS )

                  ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                     CALL KPS1_MDRFW( ST, DIMS( 1 ), DIMS( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                WDIMS( 1 ),
     :                                WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                STATUS )

                  END IF
               ELSE

*  Pad edges by replication.  Call routine appropriate for the data
*  type.
                  IF ( TYPE .EQ. '_REAL' ) THEN
                     CALL KPS1_MDRPR( ST, DIMS( 1 ), DIMS( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                WDIMS( 1 ),
     :                                WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
                     CALL KPS1_MDRPB( ST, DIMS( 1 ), DIMS( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                WDIMS( 1 ),
     :                                WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                     CALL KPS1_MDRPD( ST, DIMS( 1 ), DIMS( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                WDIMS( 1 ),
     :                                WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                     CALL KPS1_MDRPI( ST, DIMS( 1 ), DIMS( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                WDIMS( 1 ),
     :                                WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_INT64' ) THEN
                     CALL KPS1_MDRPK( ST, DIMS( 1 ), DIMS( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                WDIMS( 1 ),
     :                                WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                STATUS )

                  ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                     CALL KPS1_MDRPUB( ST, DIMS( 1 ), DIMS( 2 ),
     :                                 %VAL( CNF_PVAL( PNTRO ) ),
     :                                 WDIMS( 1 ),
     :                                 WDIMS( 2 ),
     :                                 %VAL( CNF_PVAL( WPNTR ) ),
     :                                 STATUS )

                  ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                     CALL KPS1_MDRPUW( ST, DIMS( 1 ), DIMS( 2 ),
     :                                 %VAL( CNF_PVAL( PNTRO ) ),
     :                                 WDIMS( 1 ),
     :                                 WDIMS( 2 ),
     :                                 %VAL( CNF_PVAL( WPNTR ) ),
     :                                 STATUS )

                  ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                     CALL KPS1_MDRPW( ST, DIMS( 1 ), DIMS( 2 ),
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                WDIMS( 1 ),
     :                                WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                STATUS )

                  END IF
               END IF

               IF ( STATUS .EQ. SAI__OK ) THEN

*  Switch on damping if operating in continuous mode and the number of
*  iterations has exceeded MAXIT.
                  IF ( ( MAXIT .GT. 0 ) .AND.
     :                 ( ITERAT .GT. MAXIT ) ) THEN
                     DAMP = .TRUE.
                  ELSE
                     DAMP = .FALSE.
                  END IF

*  Perform the 2-dimensional median filtering.
                  IF ( TYPE .EQ. '_REAL' ) THEN
                     CALL KPS1_MDWTR( DIFF, ST, DAMP, NUMSAM, MEDPOS,
     :                                MEDTHR, SAMSIZ, SAMINF,
     :                                WDIMS( 1 ), WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                DIMS( 1 ),
     :                                DIMS( 2 ), SAMPLE, SAMWT,
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                CHANGE, STATUS )

                  ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
                     CALL KPS1_MDWTB( DIFF, ST, DAMP, NUMSAM, MEDPOS,
     :                                MEDTHR, SAMSIZ, SAMINF,
     :                                WDIMS( 1 ), WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                DIMS( 1 ),
     :                                DIMS( 2 ), SAMPLE, SAMWT,
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                CHANGE, STATUS )

                  ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                     CALL KPS1_MDWTD( DIFF, ST, DAMP, NUMSAM, MEDPOS,
     :                                MEDTHR, SAMSIZ, SAMINF,
     :                                WDIMS( 1 ), WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                DIMS( 1 ),
     :                                DIMS( 2 ), SAMPLE, SAMWT,
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                CHANGE, STATUS )

                  ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                     CALL KPS1_MDWTI( DIFF, ST, DAMP, NUMSAM, MEDPOS,
     :                                MEDTHR, SAMSIZ, SAMINF,
     :                                WDIMS( 1 ), WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                DIMS( 1 ),
     :                                DIMS( 2 ), SAMPLE, SAMWT,
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                CHANGE, STATUS )

                  ELSE IF ( TYPE .EQ. '_INT64' ) THEN
                     CALL KPS1_MDWTK( DIFF, ST, DAMP, NUMSAM, MEDPOS,
     :                                MEDTHR, SAMSIZ, SAMINF,
     :                                WDIMS( 1 ), WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                DIMS( 1 ),
     :                                DIMS( 2 ), SAMPLE, SAMWT,
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                CHANGE, STATUS )

                  ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
                     CALL KPS1_MDWTUB( DIFF, ST, DAMP, NUMSAM, MEDPOS,
     :                                 MEDTHR, SAMSIZ, SAMINF,
     :                                 WDIMS( 1 ), WDIMS( 2 ),
     :                                 %VAL( CNF_PVAL( WPNTR ) ),
     :                                 DIMS( 1 ),
     :                                 DIMS( 2 ), SAMPLE, SAMWT,
     :                                 %VAL( CNF_PVAL( PNTRO ) ),
     :                                 CHANGE, STATUS )

                  ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
                     CALL KPS1_MDWTUW( DIFF, ST, DAMP, NUMSAM, MEDPOS,
     :                                 MEDTHR, SAMSIZ, SAMINF,
     :                                 WDIMS( 1 ), WDIMS( 2 ),
     :                                 %VAL( CNF_PVAL( WPNTR ) ),
     :                                 DIMS( 1 ),
     :                                 DIMS( 2 ), SAMPLE, SAMWT,
     :                                 %VAL( CNF_PVAL( PNTRO ) ),
     :                                 CHANGE, STATUS )

                  ELSE IF ( TYPE .EQ. '_WORD' ) THEN
                     CALL KPS1_MDWTW( DIFF, ST, DAMP, NUMSAM, MEDPOS,
     :                                MEDTHR, SAMSIZ, SAMINF,
     :                                WDIMS( 1 ), WDIMS( 2 ),
     :                                %VAL( CNF_PVAL( WPNTR ) ),
     :                                DIMS( 1 ),
     :                                DIMS( 2 ), SAMPLE, SAMWT,
     :                                %VAL( CNF_PVAL( PNTRO ) ),
     :                                CHANGE, STATUS )

                  END IF

*  Let the user know what is going on.  the message depends on what
*  iteration mode is being used.
                  IF ( MAXIT .GT. 0 ) THEN

*  In continuous mode tell the user whether the data has changed and
*  whether damping has been switched on.  Give a warning if the
*  iteration limit has been reached.
                     CALL MSG_SETI( 'ITERAT', ITERAT )
                     IF ( CHANGE ) THEN
                        CALL MSG_SETC( 'CMESS', '(data changed)' )
                     ELSE
                        CALL MSG_SETC( 'CMESS', '(no change)' )
                     END IF
                     IF ( DAMP ) THEN
                        CALL MSG_SETC( 'DMESS', '(damping used)' )
                     ELSE
                        CALL MSG_SETC( 'DMESS', '(no damping)' )
                     END IF
                     CALL MSG_OUTIF( MSG__NORM, 'MSG_ITERAT_CONT',
     :                               '   Iteration ^ITERAT completed '/
     :                               /'^CMESS ^DMESS.', STATUS )

                     IF ( ITERAT .GE. LIMIT ) THEN
                        CALL MSG_OUT( 'MSG_ITERAT_LIMIT',
     :                                '   WARNING: Iteration limit '/
     :                                /'reached without convergence.  '/
     :                                /'Filtering terminated.',
     :                                STATUS )
                     END IF
                  ELSE

*  In specified mode tell the user whether the data have changed.
                     CALL MSG_SETI( 'ITERAT', ITERAT )
                     IF ( CHANGE ) THEN
                        CALL MSG_SETC( 'CMESS', '(data changed)' )
                     ELSE
                        CALL MSG_SETC( 'CMESS', '(no change)' )
                     END IF
                     CALL MSG_OUTIF( MSG__NORM, 'MSG_ITERAT_SPEC',
     :                             '   Iteration ^ITERAT completed '/
     :                             /'^CMESS.', STATUS )
                  END IF

*  End of no-error-returned-by-MEDREP/F check
               END IF

*  Increment the iteration count.
               ITERAT = ITERAT + 1

*  Check whether the loop should be terminated or not.  In continuous
*  mode the filtering is complete when convergence has been achieved or
*  the number of iterations is greater than LIMIT.  In specified mode
*  the filtering is complete when the number of iterations has exceeded
*  the specified limit.
               IF ( MAXIT .GT. 0 ) THEN
                  FINIS = ( .NOT.CHANGE ) .OR. ( ITERAT .GT. LIMIT )
               ELSE
                  FINIS = ITERAT .GT. NUMIT
               END IF

*  End of iteration loop.
            END DO

*  End of step size loop
         END DO

*  End of if-no-error-after-getting-input-structure check
      END IF

  999 CONTINUE

*  End the NDF context. This will also unmap everything.
      CALL NDF_END( STATUS )

      END
