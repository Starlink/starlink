      SUBROUTINE SURF_GRID_DESPIKE( N_FILES, N_PTS, N_POS, N_BOLS,
     :     BITNUM, WAVELENGTH, DIAMETER, BOL_RA_PTR, BOL_DEC_PTR, 
     :     DATA_PTR, QUALITY_PTR, NX, NY, ICEN, JCEN, NSPIKES,
     :     BADBIT, STATUS )
*+
*  Name:
*     SURF_GRID_DESPIKE

*  Purpose:
*     Despike data by sorting into a given grid position

*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL SURF_GRID_DESPIKE ( N_FILES, N_PTS, N_POS, N_BOLS, WAVELENGTH, 
*    :     DIAMETER, BOL_RA_PTR, BOL_DEC_PTR, DATA_PTR, QUALITY_PTR,
*    :     NX, NY, ICEN, JCEN, NSPIKES,
*    :     BADBIT, STATUS ) 

*  Description:
*     For each data point this routine places it into a bin in the 
*     output grid depending on the position of the data point on the sky.
*     The position in the input data array is stored.
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
*     BITNUM = INTEGER (Given)
*       Bit number to be affected by this routine
*     WAVELENGTH = REAL (Given)
*       Wavelength of data (microns)
*     DIAMETER = REAL (Given)
*       Diameter of telescope (metres)
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

*  History:
*     Original version: Timj, 1997 Oct 20
*     $Log$
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
 
*  Arguments Given:
      INTEGER N_FILES
      INTEGER BITNUM
      BYTE    BADBIT ( N_FILES )
      INTEGER N_POS ( N_FILES )
      INTEGER N_PTS ( N_FILES )
      INTEGER N_BOLS ( N_FILES )
      INTEGER BOL_DEC_PTR ( N_FILES )
      INTEGER BOL_RA_PTR ( N_FILES )
      INTEGER DATA_PTR ( N_FILES )
      INTEGER QUALITY_PTR ( N_FILES )
      REAL    WAVELENGTH
      REAL    DIAMETER
      INTEGER NX
      INTEGER NY
      INTEGER ICEN
      INTEGER JCEN
      
*  Arguments Returned:
      INTEGER NSPIKES ( N_FILES )

*  Status:
      INTEGER STATUS                        ! Global status

*  Local Constants:
 
*  Local Variables:
      INTEGER BIN_PTR                       ! Binned data
      INTEGER BIN_PTR_END                   ! End of BIN_PTR
      INTEGER BIN_POS_PTR                   ! Position of binned data
      INTEGER BIN_POS_END                   ! End of BIN_POS
      INTEGER GRID_END                      ! End of scratch array
      INTEGER GRID_PTR                      ! Scratch space
      INTEGER I                             ! Loop counter
      INTEGER IJPOS_PTR                     ! Ptr to array containing I,J's
      INTEGER IJPOS_END                     ! End of IJPOS_PTR
      INTEGER IMAX                          ! I pos of hist max
      INTEGER IPOS_END                      ! End of ipos_ptr
      INTEGER IPOS_PTR                      ! I positions in lookup table
      INTEGER JPOS_END                      ! End of jpos_ptr
      INTEGER JPOS_PTR                      ! J positions in lookup table
      INTEGER JMAX                          ! J pos of hist max
      INTEGER NMAX                          ! Max entries per cell
      REAL    NSIGMA                        ! Despiking level
      INTEGER OFFSET                        ! Offset in data array
      REAL    OUT_PIXEL                     ! Pixel size in radians
      LOGICAL PLOT                          ! Plot points?
      INTEGER PNT_END                       ! End of PNT_PTR
      INTEGER PNT_PTR                       ! Work space for gridded points
      INTEGER SCRATCH_END                   ! Scratch space
      INTEGER SCRATCH_PTR                   ! Scratch space
      CHARACTER * (10) SMODE                ! Smoothing mode
      INTEGER STATS_END                     ! End of STATS_PTR
      INTEGER STATS_PTR                     ! Bin statistics
      INTEGER TOT_PTS                       ! Total number of points to despike
      CHARACTER * (10) UMODE                ! Pixel unwrapping mode
      INTEGER UNIT                          ! Unit id for display device

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

*     Default unpacking mode
      UMODE = 'SPIRAL'

*     First need to find out how many data points we are dealing with

      TOT_PTS = 0

      DO I = 1, N_FILES

         TOT_PTS = TOT_PTS + N_PTS(I)

      END DO

*     Choose a pixel size based on wavelength
*     and dish diameter.
*     Try for quarter beam size first.

      OUT_PIXEL = (WAVELENGTH * 1.0E-6 / DIAMETER) / 4.0

*     Now find out how big an output grid is needed

      CALL SURFLIB_CALC_OUTPUT_GRID(N_FILES, N_PTS, OUT_PIXEL,
     :     BOL_RA_PTR, BOL_DEC_PTR, NX, NY, ICEN, JCEN, STATUS)

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
         CALL SCULIB_CFILLI(NX * NY, 0, %VAL(GRID_PTR))
      END IF



*     Now loop through each map in turn and fill this array
*     Note that we put all the data in this array (ie each
*     input file is in this array). The index is 1..TOT_PTS where
*     the division is every N_PTS(I)
*     Whilst we are at it, construct a histogram.
*     Need to do this via a subroutine since I only have access to
*     pointers here.

      OFFSET = 0

      DO I = 1, N_FILES

*     Fill the array with data. (1 file at a time)

         CALL SURFLIB_CALC_IJPOS(N_PTS(I), DBLE(OUT_PIXEL), ICEN, JCEN,
     :        %VAL(BOL_RA_PTR(I)), %VAL(BOL_DEC_PTR(I)), 
     :        %VAL(IJPOS_PTR + (2 * OFFSET * VAL__NBR)),
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

         CALL SURFLIB_HISTOGRAM_GRID( N_PTS(I), NX, NY, .TRUE.,
     :        %VAL(DATA_PTR(I)), %VAL(QUALITY_PTR(I)), BADBIT(I),
     :        %VAL(IJPOS_PTR + (2 * OFFSET * VAL__NBI)),
     :        %VAL(GRID_PTR), IMAX, JMAX, NMAX, STATUS)


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
     :  BIN_POS_END, STATUS)

*     We are going to use the histogram scratch space to keep track
*     of the current highest member used in BIN_PTR (etc).
*     Two options for doing this:
*       1. Reset to zero and increment each time a data point is entered.
*       2. Leave as is and decrement each time a data point is entered.
*     Not much difference so I will go for the increment option.


*     Initialise the work arrays

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_CFILLI(NX * NY, 0, %VAL(GRID_PTR))
         CALL SCULIB_CFILLR(NX * NY * NMAX, VAL__BADR, %VAL(BIN_PTR))
         CALL SCULIB_CFILLI(NX * NY * NMAX, VAL__BADI, 
     :        %VAL(BIN_POS_PTR))
      END IF

*     Now we need to copy the data into BIN_PTR and the positions
*     into BIN_POS_PTR. 

      OFFSET = 0

      DO I = 1, N_FILES

         CALL SURFLIB_FILL_GRID(N_PTS(I), NX, NY, NMAX, OFFSET,
     :        %VAL(DATA_PTR(I)), %VAL(QUALITY_PTR(I)), BADBIT(I),
     :        %VAL(IJPOS_PTR + (2 * OFFSET * VAL__NBI)),
     :        %VAL(GRID_PTR), %VAL(BIN_PTR), %VAL(BIN_POS_PTR),
     :        STATUS)

*     Calculate the offset in the position array based on file number
*     for the next time round the loop
         OFFSET = OFFSET + N_PTS(I)

      END DO


*     Free the scratch memory used for the histogram and counting the
*     current position in the array.

      CALL SCULIB_FREE ('GRID_PTR', GRID_PTR, GRID_END, STATUS)

*     Some scratch space for storing the numbers (size nmax)
*     in each bin

      CALL SCULIB_MALLOC(NMAX * VAL__NBR, PNT_PTR, PNT_END, STATUS)
      CALL SCULIB_MALLOC(NMAX * VAL__NBR, SCRATCH_PTR, SCRATCH_END, 
     :     STATUS)


*     Ask for the NSIGMA clipping level

      CALL PAR_GET0R('NSIGMA', NSIGMA, STATUS)

*     Ask for the smoothing mode. Ask for it here since 
*       1. We need to know it anyway
*       2. We only need to know DMODE if SMODE is not equal to NONE
*          or if we are plotting.

      CALL PAR_CHOIC('SMODE', 'NONE', 'NONE,HANN', 
     :        .TRUE., SMODE, STATUS)


*     Ask for the plotting device
*     Do it here so that we get more control

*     Obtain Zone on a graphics workstation

      IF (STATUS .EQ. SAI__OK) THEN

         PLOT = .FALSE.  ! Start by assuming no plot

         CALL PGP_ASSOC( 'DEVICE', 'WRITE', 1, 1, UNIT, STATUS )

         IF (STATUS .EQ. PAR__NULL) THEN

*     Assume that a null means that people dont want the plot but do
*     want the despike
            CALL ERR_ANNUL(STATUS)

         ELSE

            PLOT = .TRUE.

         END IF

      END IF

*     Since we are plotting then we can ask about the display mode
     
*     At this point we also need to find out the type of unwrapping
*     to use.

*     Find out the display mode if we are smoothing or if we are plotting

      IF ((SMODE .NE. 'NONE') .OR. PLOT) THEN

         CALL PAR_CHOIC('DMODE', 'SPIRAL', 
     :        'SPIRAL,XLINEAR,YLINEAR,DIAG1,DIAG2',
     :        .TRUE., UMODE, STATUS)

      END IF
         
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

      CALL SURFLIB_CALC_GRIDIJ(UMODE, NX, NY, ICEN, JCEN,
     :     %VAL(IPOS_PTR), %VAL(JPOS_PTR), STATUS)


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
      CALL SCULIB_MALLOC(3 * NX * NY * VAL__NBR, STATS_PTR, STATS_END, 
     :     STATUS)

*     Calculate stats using the specified smoothing mode

      CALL SURFLIB_STATS_GRID(SMODE, NX, NY, NMAX, NSIGMA, 
     :     %VAL(IPOS_PTR),
     :     %VAL(JPOS_PTR), %VAL(BIN_PTR), %VAL(PNT_PTR),
     :     %VAL(STATS_PTR), STATUS)


*     Do this because:
*       1. More efficient to calculate stats once rather than for each plot
*       2. Allows smoothing of the ranges (although not sure this
*          is a good thing for bright sources).

******* PLOTTING **************************************

      IF (PLOT) THEN

*     Plot the data
         CALL SURFLIB_PLOT_GRID(UNIT, NX, NY, NMAX, NSIGMA, 
     :        %VAL(IPOS_PTR), %VAL(JPOS_PTR), %VAL(BIN_PTR), 
     :        %VAL(STATS_PTR), %VAL(PNT_PTR), %VAL(SCRATCH_PTR), 
     :        STATUS)
         
*     Finish the plotting excursion
*     Close down PGPLOT
         CALL PGP_DEACT( STATUS )

      END IF

****** CLIPPING *******************

*     Find the stats and clip the data.
*     Note that I do realise that SURFLIB_PLOT_GRID already calculates
*     the mean etc, but doesn't really help me here since it is
*     difficult to relate back to actual quality.

      CALL SURFLIB_CLIP_GRID(N_FILES, N_PTS, NX, NY, NMAX,
     :     N_BOLS, BITNUM, DATA_PTR, QUALITY_PTR, %VAL(BIN_PTR),
     :     %VAL(BIN_POS_PTR), %VAL(STATS_PTR), %VAL(IPOS_PTR),
     :     %VAL(JPOS_PTR), NSPIKES, STATUS)
         
*     Free memory
      CALL SCULIB_FREE('PNT_PTR', PNT_PTR, PNT_END, STATUS)
      CALL SCULIB_FREE('SCRATCH', SCRATCH_PTR, SCRATCH_END, STATUS)

*     Free positions
      CALL SCULIB_FREE('IPOS_PTR', IPOS_PTR, IPOS_END, STATUS)
      CALL SCULIB_FREE('JPOS_PTR', JPOS_PTR, JPOS_END, STATUS)


*     Free BIN_PTR 
      CALL SCULIB_FREE('BIN_PTR', BIN_PTR, BIN_PTR_END, STATUS)
      CALL SCULIB_FREE('BIN_POS', BIN_POS_PTR, BIN_POS_END, STATUS)

*     Free Statistics
      CALL SCULIB_FREE('STATS_PTR', STATS_PTR, STATS_END, STATUS)

      END
