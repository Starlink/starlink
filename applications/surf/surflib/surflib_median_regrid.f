      SUBROUTINE SURFLIB_MEDIAN_REGRID( N_FILES, N_PTS,
     :     OUT_PIXEL, NX, NY, ICEN, JCEN, BOL_RA_PTR,
     :     BOL_DEC_PTR, DATA_PTR,  OUT_DATA, OUT_VAR, OUT_QUAL,
     :     STATUS )
*+
*  Name:
*     SURFLIB_MEDIAN_REGRID

*  Purpose:
*     Generate image with each pixel the median of all pixels in bin

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SURFLIB_MEDIAN_REGRID( N_FILES, N_PTS,
*    :     OUT_PIXEL, NX, NY, ICEN, JCEN, BOL_RA_PTR,
*    :     BOL_DEC_PTR, DATA_PTR,  OUT_DATA, OUT_VAR, OUT_QUAL,
*    :     STATUS )

*  Description:
*     This is done in two stages:
*
*     1) Find the size of the output grid from the maximum extent of
*        the input data.
*     2) Loop through data. Find I,J coordinate of each point in the
*        output grid.
*     3) Find out maximum number of points for an I,J position.
*     4) Put data onto grid in array (I,J,N) [REALS].
*        We also need to store positions of these data.
*        We can either do it by storing the file number, bolometer and
*        position (time) index OR we can just store some index in a merged
*        data array that goes from 1..TOT_PTS.
*         First method is easy but memory hungry. Second method is
*         more efficient but does need some reconstruction to work
*         out where the point was in the original data.
*       Use the second method.
*
*     Once the data is gridded, it is first displayed and then
*     despiked. Currently despiking is done on a simple sigma clipping
*     basis for each bin.


*  Arguments:
*     N_FILES = INTEGER (Given)
*       Number of data sets (ie files)
*     N_PTS ( N_FILES ) = INTEGER (Given)
*       Total number of points in each map
*     N_POS( N_FILES ) = INTEGER (Given)
*       Number of positions per set (Y positions)
*     N_BOLS( N_FILES ) = INTEGER (Given)
*       Number of bolometers per set (X positions)
*     BOL_RA_PTR( N_FILES ) = INTEGER (Given)
*       Array of pointers to position information (X coords)
*       Note that each data set has positions for N_POS * N_BOLS
*     BOL_RA_PTR( N_FILES ) = INTEGER (Given)
*       Array of pointers to position information (Y coords)
*     DATA_PTR( N_FILES ) = INTEGER (Given)
*       Pointers to actual data arrays
*     QUALITY_PTR( N_FILES ) = INTEGER (Given)
*       Pointer to quality arrays
*     NX = INTEGER (Returned)
*       Number of points in grid (X)
*     NY = INTEGER (Returned)
*       Number of points in grid (Y)
*     ICEN = INTEGER (Returned)
*       Reference pixel (X)
*     JCEN = INTEGER (Returned)
*       Reference pixel (Y)
*     NSPIKES ( N_FILES ) = INTEGER (Returned)
*       Number of spikes detected (and removed) in each file
*     BADBIT ( N_FILES ) = BYTE (Given)
*       Bad bit mask for identifying bad pixels from quality
*     STATUS = INTEGER (Given & Returned)
*       Global Status

*  ADAM Parameters:
*     DEVICE = DEVICE (Given)
*       Device to display plot.
*     DMODE = CHAR (Given)
*       Display mode for plot. Allowed values are:
*         SPIRAL  - A Spiral outwards from the reference pixel
*         XLINEAR - unfold each X strip in turn for each Y
*         YLINEAR - unfold each Y strip in turn for each X
*         DIAG1   - diagonal strips starting at position (1,1)
*         DIAG2   - diagonal strips starting at positions (nx,1)
*       (see SURFLIB_CALC_GRIDIJ subroutine)
*       Also used when pixel smoothing is required to smooth out the
*       variations between bins.
*     NSIGMA = REAL (Given)
*       Number of sigma used in clipping
*     SMODE = CHAR (Given)
*       Mode used for smoothing the clipping envelope.
*       Available modes are:
*         NONE  - No smoothing (this is not dependent on the DMODE)
*         HANN  - Hanning smoothing
*       All modes except 'NONE' depend on the unwrapping mode (given
*       by parameter DMODE) since this determines which pixels are adjacent
*       to a given bin.
*     XRANGE = INTEGER (Given)
*       X Range of plot

*  Notes:
*     For SMODE=NONE, DMODE is only requested if a plot is required.

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 1995-2002 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     Original version: Timj, 1997 Oct 20
*     $Log$
*     Revision 1.8  2005/03/23 03:48:48  timj
*     No longer use wavelength + diameter for determining resolution element. Use
*     the pixel size.
*
*     Revision 1.7  2004/09/01 01:02:03  timj
*     use CNF_PVAL
*
*     Revision 1.6  2002/09/14 03:58:13  timj
*     Update copyright
*
*     Revision 1.5  2002/09/11 00:00:33  timj
*     Initialize the weights array
*
*     Revision 1.4  2000/08/22 00:14:18  timj
*     Some MALLOC variables were not being initialised
*
*     Revision 1.3  1999/08/06 02:29:06  timj
*     Tweak headers for use with PROLAT.
*
*     Revision 1.2  1999/08/03 19:32:50  timj
*     Add copyright message to header.
*
*     Revision 1.1  1998/05/20 22:52:04  timj
*     Initial revision
*
*     Revision 1.2  1998/05/12 20:56:48  timj
*     Free the memory allocated for STATS
*
*     Revision 1.1  1997/11/12 00:13:37  timj
*     Initial revision
*


*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'                          ! Bad values
      INCLUDE 'PAR_ERR'                          ! PAR__NULL
      INCLUDE 'MSG_PAR'                          ! MSG__NORM
      INCLUDE 'CNF_PAR'                          ! For CNF_PVAL function


*  Arguments Given:
      REAL    OUT_PIXEL                     ! Pixel size in radians
      INTEGER N_FILES
      INTEGER N_PTS ( N_FILES )
      INTEGER BOL_DEC_PTR ( N_FILES )
      INTEGER BOL_RA_PTR ( N_FILES )
      INTEGER DATA_PTR ( N_FILES )
      INTEGER NX
      INTEGER NY
      INTEGER ICEN
      INTEGER JCEN

*  Arguments Returned:
      REAL    OUT_DATA ( NX, NY )
      REAL    OUT_VAR ( NX, NY )
      BYTE    OUT_QUAL ( NX, NY )

*  Status:
      INTEGER STATUS                        ! Global status

*  Local Constants:

*  Local Variables:
      BYTE    BADBIT                        ! Bad bits mask
      INTEGER BIN_PTR                       ! Binned data
      INTEGER BIN_PTR_END                   ! End of BIN_PTR
      INTEGER BIN_POS_PTR                   ! Position of binned data
      INTEGER BIN_POS_END                   ! End of BIN_POS
      BYTE    BTEMP                         ! Scratch byte
      INTEGER GRID_END                      ! End of scratch array
      INTEGER GRID_PTR                      ! Scratch space
      INTEGER I                             ! Loop counter
      INTEGER IERR                          ! For VEC_
      INTEGER IJPOS_PTR                     ! Ptr to array containing I,J's
      INTEGER IJPOS_END                     ! End of IJPOS_PTR
      INTEGER IMAX                          ! I pos of hist max
      INTEGER IPOS_END                      ! End of ipos_ptr
      INTEGER IPOS_PTR                      ! I positions in lookup table
      INTEGER ITEMP                         ! Temp integer
      INTEGER J                             ! Loop variable
      INTEGER JPOS_END                      ! End of jpos_ptr
      INTEGER JPOS_PTR                      ! J positions in lookup table
      INTEGER JMAX                          ! J pos of hist max
      INTEGER NERR                          ! For VEC_
      INTEGER NMAX                          ! Max entries per cell
      REAL    NSIGMA                        ! Despiking level
      INTEGER OFFSET                        ! Offset in data array

      INTEGER PNT_END                       ! End of PNT_PTR
      INTEGER PNT_PTR                       ! Work space for gridded points
      INTEGER QUALITY_END                   ! end Scratch quality
      INTEGER QUALITY_PTR                   ! Scratch quality
      INTEGER REGRID1_END                   ! End of REGRID1 Scratch array
      INTEGER REGRID1_PTR                   ! Scratc space for regrid1
      REAL    SCALE                         ! Scale size for bad pixel finding
      INTEGER SCRATCH_END                   ! Scratch space
      INTEGER SCRATCH_PTR                   ! Scratch space
      CHARACTER * (10) SMODE                ! Smoothing mode
      INTEGER STATS_END                     ! End of STATS_PTR
      INTEGER STATS_PTR                     ! Bin statistics
      INTEGER TOT_PTS                       ! Total number of points to despike
      INTEGER TOTAL_WEIGHT_END              ! End of total weight
      INTEGER TOTAL_WEIGHT_PTR              ! Pointer to total weight
      CHARACTER * (10) UMODE                ! Pixel unwrapping mode

*  Local data
*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Initialise pointers
      BIN_PTR = 0
      BIN_PTR_END = 0
      BIN_POS_PTR = 0
      BIN_POS_END = 0
      GRID_PTR = 0
      GRID_END = 0
      PNT_END = 0
      PNT_PTR = 0
      SCRATCH_END = 0
      SCRATCH_PTR = 0
      IJPOS_PTR = 0
      IJPOS_END = 0
      TOTAL_WEIGHT_PTR = 0
      TOTAL_WEIGHT_END = 0
      REGRID1_PTR = 0
      REGRID1_END = 0

*     First need to find out how many data points we are dealing with

      TOT_PTS = 0

      DO I = 1, N_FILES

         TOT_PTS = TOT_PTS + N_PTS(I)

      END DO

*     GENERATE OUTPUT QUALITY MASK
*     Need to calculate quality mask for the data set. This is based
*     on the calculation of which output pixels have data close enough
*     to them to actually have some measure of the flux at that point.
*     This is identical to that calculated by the other rebinning techniques.

      CALL SCULIB_MALLOC (NX * NY * VAL__NBR, TOTAL_WEIGHT_PTR,
     :     TOTAL_WEIGHT_END, STATUS)
      CALL SCULIB_MALLOC (NX * NY * VAL__NBI, REGRID1_PTR,
     :     REGRID1_END, STATUS)

*     Fill with zeroes
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_CFILLI(NX * NY, 0,
     :                      %VAL(CNF_PVAL(TOTAL_WEIGHT_PTR)))
      END IF

*     Generate a mask for all the pixels that do not have valid
*     data points. We choose a scalesize that will cover an entire
*     pixel -- the distance from the centre of the pixel to the corner
      SCALE = OUT_PIXEL / SQRT( 2.0 )
      DO I = 1, N_FILES

         CALL SCULIB_WTFN_REGRID_1 ( 1.0,
     :        %VAL(CNF_PVAL(DATA_PTR(I))),
     :        %VAL(CNF_PVAL(BOL_RA_PTR(I))),
     :        %VAL(CNF_PVAL(BOL_DEC_PTR(I))), N_PTS(I), DBLE(OUT_PIXEL),
     :        NX, NY, ICEN, JCEN, 1, SCALE,
     :        %VAL(CNF_PVAL(TOTAL_WEIGHT_PTR)),
     :        %VAL(CNF_PVAL(REGRID1_PTR)), STATUS)

      END DO

*     Free the scratch array
      CALL SCULIB_FREE ('REGRID1', REGRID1_PTR, REGRID1_END, STATUS)

*     Now need to generate a quality mask based on the histogram
      DO J = 1, NY
         DO I = 1, NX

            OFFSET = (J-1)*NX + I - 1

*     Retrieve current value in histogram
            CALL VEC_ITOI(.FALSE., 1,
     :           %VAL(CNF_PVAL(TOTAL_WEIGHT_PTR)+(OFFSET*VAL__NBI)),
     :           ITEMP, IERR, NERR, STATUS)

*     If there is a point there set quality to 0 else set to 1
            IF (ITEMP .EQ. 0) THEN
               BTEMP = 1
            ELSE
               BTEMP = 0
            END IF

*     Copy byte to quality array
            OUT_QUAL(I,J) = BTEMP

         END DO
      END DO

*     Free the total weight
      CALL SCULIB_FREE ('TOTAL WEIGHT', TOTAL_WEIGHT_PTR,
     :     TOTAL_WEIGHT_END, STATUS)


*     END OF FILLING OUTPUT QUALITY ARRAY






*     Need to allocate some memory for the quality array
*     since this is not supplied automatically when we are running
*     as REBIN
*     Find max amount of points required

      ITEMP = 0
      DO I = 1, N_FILES
         ITEMP = MAX(ITEMP, N_PTS(I))
      END DO

*     Get the memory
      QUALITY_PTR = 0
      QUALITY_END = 0
      CALL SCULIB_MALLOC(ITEMP * VAL__NBUB, QUALITY_PTR, QUALITY_END,
     :     STATUS)

*     Fill with zeroes
      IF (STATUS .EQ. SAI__OK) THEN
         BTEMP = 0
         CALL SCULIB_CFILLB(ITEMP, BTEMP, %VAL(CNF_PVAL(QUALITY_PTR)))
      END IF

*     The first run through simply stores an I,J for each of the TOT_PTS
*     So need to allocate some memory. Just need two locations per point.
*     A 2 x TOT_PTS integer array

      CALL SCULIB_MALLOC (TOT_PTS * 2 * VAL__NBI,
     :     IJPOS_PTR, IJPOS_END, STATUS)

*     Allocate some memory for the histogram

      CALL SCULIB_MALLOC(NX * NY * VAL__NBI, GRID_PTR, GRID_END,
     :     STATUS)

*     Fill with zeroes
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_CFILLI(NX * NY, 0, %VAL(CNF_PVAL(GRID_PTR)))
      END IF

*     Now loop through each map in turn and fill this array
*     Note that we put all the data in this array (ie each
*     input file is in this array). The index is 1..TOT_PTS where
*     the division is every N_PTS(I)
*     Whilst we are at it, construct a histogram.
*     Need to do this via a subroutine since I only have access to
*     pointers here.

      OFFSET = 0
      BADBIT = 0

      DO I = 1, N_FILES


*     Fill the array with data. (1 file at a time)

         CALL SURFLIB_CALC_IJPOS(N_PTS(I), DBLE(OUT_PIXEL), ICEN, JCEN,
     :        %VAL(CNF_PVAL(BOL_RA_PTR(I))),
     :        %VAL(CNF_PVAL(BOL_DEC_PTR(I))),
     :        %VAL(CNF_PVAL(IJPOS_PTR)+ (2 * OFFSET * VAL__NBR)),
     :        STATUS)

*     At the same time we can be adding the returned data into
*     a histogram (since we have already calculated the offsets
*     in the returned data). NMAX tells us the maximum number of
*     entries for any cell.
*     Note that in order that the histogram
*     is correct for our given data set we must pass in the quality
*     and data array so that bad pixels are not included in the
*     calculation. We could do it without the data values (and probably
*     should) but I am just trying to do it properly...

         IF (STATUS .EQ. SAI__OK) THEN
            CALL SURFLIB_HISTOGRAM_GRID( N_PTS(I), NX, NY, .TRUE.,
     :           %VAL(CNF_PVAL(DATA_PTR(I))),
     :           %VAL(CNF_PVAL(QUALITY_PTR)), BADBIT,
     :           %VAL(CNF_PVAL(IJPOS_PTR) + (2 * OFFSET * VAL__NBI)),
     :           %VAL(CNF_PVAL(GRID_PTR)), IMAX, JMAX, NMAX, STATUS)

*     Dont worry if index values were out of range since we can rebin
*     a subset
            IF (STATUS .EQ. SAI__WARN) CALL ERR_ANNUL(STATUS)
         END IF

*     Calculate the offset in the position array based on file number
*     for the next time round the loop
         OFFSET = OFFSET + N_PTS(I)

      END DO


*     Now we know where each pixel is. We need to put this data onto
*     a grid. From the histogram we know how big the array needs to
*     be that contains each data point.

*     Get some memory for the output grid
*     Need two arrays
*      1) Real array containing the data values (NX, NY, NMAX)
*      2) Integer array containing the positions for each value (NX,NY,NMAX)

      CALL SCULIB_MALLOC(NX * NY * NMAX * VAL__NBR, BIN_PTR,
     :     BIN_PTR_END, STATUS)

      CALL SCULIB_MALLOC(NX * NY * NMAX * VAL__NBI, BIN_POS_PTR,
     :     BIN_POS_END, STATUS)

*     We are going to use the histogram scratch space to keep track
*     of the current highest member used in BIN_PTR (etc).
*     Two options for doing this:
*       1. Reset to zero and increment each time a data point is entered.
*       2. Leave as is and decrement each time a data point is entered.
*     Not much difference so I will go for the increment option.


*     Initialise the work arrays

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_CFILLI(NX * NY, 0, %VAL(CNF_PVAL(GRID_PTR)))
         CALL SCULIB_CFILLR(NX * NY * NMAX, VAL__BADR,
     :                      %VAL(CNF_PVAL(BIN_PTR)))
         CALL SCULIB_CFILLI(NX * NY * NMAX, VAL__BADI,
     :        %VAL(CNF_PVAL(BIN_POS_PTR)))
      END IF

*     Now we need to copy the data into BIN_PTR and the positions
*     into BIN_POS_PTR.

      OFFSET = 0

      IF (STATUS .EQ. SAI__OK) THEN
         DO I = 1, N_FILES

            CALL SURFLIB_FILL_GRID(N_PTS(I), NX, NY, NMAX, OFFSET,
     :           %VAL(CNF_PVAL(DATA_PTR(I))),
     :           %VAL(CNF_PVAL(QUALITY_PTR)), BADBIT,
     :           %VAL(CNF_PVAL(IJPOS_PTR) + (2 * OFFSET * VAL__NBI)),
     :           %VAL(CNF_PVAL(GRID_PTR)), %VAL(CNF_PVAL(BIN_PTR)),
     :           %VAL(CNF_PVAL(BIN_POS_PTR)),
     :           STATUS)

*     If a warning was raised simply ignore it since that means we
*     are using a grid that is too small (which is okay)
            IF (STATUS .EQ. SAI__WARN) CALL ERR_ANNUL(STATUS)


*     Calculate the offset in the position array based on file number
*     for the next time round the loop
            OFFSET = OFFSET + N_PTS(I)

         END DO
      END IF

*     Free quality
      CALL SCULIB_FREE('QUAL', QUALITY_PTR, QUALITY_END, STATUS)


*     Free the scratch memory used for the histogram and counting the
*     current position in the array.

      CALL SCULIB_FREE ('GRID_PTR', GRID_PTR, GRID_END, STATUS)

*     Some scratch space for storing the numbers (size nmax)
*     in each bin

      CALL SCULIB_MALLOC(NMAX * VAL__NBR, PNT_PTR, PNT_END, STATUS)
      CALL SCULIB_MALLOC(NMAX * VAL__NBR, SCRATCH_PTR, SCRATCH_END,
     :     STATUS)


*     Calculate the grid positions related to a given pixel index
*     Need to do this since some people want complicated spiral unwrapping
*     and it takes too long to calculate all this on the fly.

*     Allocate some memory

      IPOS_PTR = 0
      JPOS_PTR = 0
      IPOS_END = 0
      JPOS_END = 0

      CALL SCULIB_MALLOC(NX * NY * VAL__NBI, IPOS_PTR, IPOS_END,
     :     STATUS)
      CALL SCULIB_MALLOC(NX * NY * VAL__NBI, JPOS_PTR, JPOS_END,
     :     STATUS)

*     ...and calculate the new grid look up table

      UMODE = 'XLINEAR'
      CALL SURFLIB_CALC_GRIDIJ(UMODE, NX, NY, ICEN, JCEN,
     :     %VAL(CNF_PVAL(IPOS_PTR)), %VAL(CNF_PVAL(JPOS_PTR)), STATUS)


*     Calculate the statistics of each bin and store in an array.
*     Since this is generally useful for the plotting and the
*     despiking itself.
*     Have three measurements:
*         Median and The mean + nsigma and the mean - nsigma
*     Note that SURFLIB_PLOT_GRID still needs to work out the
*     positions itself since it can not deal with bad pixels.

*     Memory for the statistics
*     Since it is all related, just create one array to store
*     all the stats.

      STATS_PTR = 0
      STATS_END = 0
      CALL SCULIB_MALLOC(3 * NX * NY * VAL__NBR, STATS_PTR,STATS_END,
     :     STATUS)

*     Calculate stats using the specified smoothing mode

      SMODE = 'NONE'
      NSIGMA = 3.0  ! Cant use less than this because of iterative clipping
      CALL SURFLIB_STATS_GRID(SMODE, NX, NY, NMAX, NSIGMA,
     :     %VAL(CNF_PVAL(IPOS_PTR)),
     :     %VAL(CNF_PVAL(JPOS_PTR)), %VAL(CNF_PVAL(BIN_PTR)),
     :     %VAL(CNF_PVAL(PNT_PTR)),
     :     %VAL(CNF_PVAL(STATS_PTR)), STATUS)

*     Free memory
      CALL SCULIB_FREE('PNT_PTR', PNT_PTR, PNT_END, STATUS)
      CALL SCULIB_FREE('SCRATCH', SCRATCH_PTR, SCRATCH_END, STATUS)

*     Free positions
      CALL SCULIB_FREE('IPOS_PTR', IPOS_PTR, IPOS_END, STATUS)
      CALL SCULIB_FREE('JPOS_PTR', JPOS_PTR, JPOS_END, STATUS)


*     Free BIN_PTR
      CALL SCULIB_FREE('BIN_PTR', BIN_PTR, BIN_PTR_END, STATUS)
      CALL SCULIB_FREE('BIN_POS', BIN_POS_PTR, BIN_POS_END, STATUS)


*     Copy these stats to the output image
      CALL VEC_RTOR(.FALSE., NX*NY, %VAL(CNF_PVAL(STATS_PTR)),
     :     OUT_DATA, IERR, NERR, STATUS)

*     Variaance
*     STATS(2,) and STATS(3,) contain mean +/- nsigma
*     therefore STATS(3,) - STATS(2,) = SIGMA when NSIGMA = 0.5
*     In this case use NSIGMA=3 since this is the iterative clipping
*     level used by STATS_GRID (if I use 0.5 the stdev always comes
*     out as zero)
*     Take difference and then square to get variance

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_SUBARE(NX*NY,
     :                      %VAL(CNF_PVAL(STATS_PTR)+(NX*NY*VAL__NBR)),
     :        %VAL(CNF_PVAL(STATS_PTR)+(2*NX*NY*VAL__NBR)), OUT_VAR,
     :        ITEMP, ITEMP, ITEMP, ITEMP, ITEMP, ITEMP,
     :        .FALSE., .TRUE., .FALSE.)
      END IF

*     Loop over OUT_VAR
*     Divide by 2.0*NSIGMA and then
*     Square the stdev to get variance
*
      DO J = 1, NY
         DO I = 1, NX
            IF (OUT_VAR(I,J) .NE. VAL__BADR) THEN
               OUT_VAR(I,J) = OUT_VAR(I,J) / (2.0 *NSIGMA)
               OUT_VAR(I,J) = OUT_VAR(I,J) ** 2
            END IF
         END DO
      END DO

      CALL SCULIB_FREE('STATS_PTR', STATS_PTR, STATS_END, STATUS)
      CALL SCULIB_FREE ('IJPOS_PTR', IJPOS_PTR, IJPOS_END, STATUS)

      END
