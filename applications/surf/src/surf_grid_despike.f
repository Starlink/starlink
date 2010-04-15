      SUBROUTINE SURF_GRID_DESPIKE( N_FILES, N_PTS, N_POS, N_BOLS,
     :     BITNUM, NYQUIST, BOL_RA_PTR, BOL_DEC_PTR,
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
*     CALL SURF_GRID_DESPIKE ( N_FILES, N_PTS, N_POS, N_BOLS, NYQUIST,
*    :     BOL_RA_PTR, BOL_DEC_PTR, DATA_PTR, QUALITY_PTR,
*    :     NX, NY, ICEN, JCEN, NSPIKES,
*    :     BADBIT, STATUS )

*  Description:
*     For each data point this routine places it into a bin in the
*     output grid depending on the position of the data point on the sky.
*     The position in the input data array is stored.
*     This is done in two stages:
*
*     - Find the size of the output grid from the maximum extent of
*        the input data.
*     - Loop through data. Find I,J coordinate of each point in the
*        output grid.
*     - Find out maximum number of points for an I,J position.
*     - Put data onto grid in array (I,J,N) [REALS].
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
*     NYQUIST = DOUBLE PRECISION (Given)
*       Nyquist sampling (radians)
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
*     CLEAR = LITERAL (Read)
*       Controls whether the plot device should be cleared before use.
*       Default is TRUE.
*     DEVICE = DEVICE (Read)
*       Device to display plot.
*     DMODE = CHAR (Read)
*       Display mode for plot. Allowed values are:
*         SPIRAL  - A Spiral outwards from the reference pixel
*         XLINEAR - unfold each X strip in turn for each Y
*         YLINEAR - unfold each Y strip in turn for each X
*         DIAG1   - diagonal strips starting at position (1,1)
*         DIAG2   - diagonal strips starting at positions (nx,1)
*       (see SURFLIB_CALC_GRIDIJ subroutine)
*       Also used when pixel smoothing is required to smooth out the
*       variations between bins.
*     NSIGMA = REAL (Read)
*       Number of sigma used in clipping
*     SMODE = CHAR (Read)
*       Mode used for smoothing the clipping envelope.
*       Available modes are:
*         NONE  - No smoothing (this is not dependent on the DMODE)
*         HANN  - Hanning smoothing
*       All modes except 'NONE' depend on the unwrapping mode (given
*       by parameter DMODE) since this determines which pixels are adjacent
*       to a given bin.
*     STYLE = LITERAL (Read)
*       Plot style to use. See the KAPPA manual for more information on styles.
*     XRANGE = INTEGER (Read)
*       X Range of plot

*  Notes:
*     For SMODE=NONE, DMODE is only requested if a plot is required.

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     Original version: Timj, 1997 Oct 20
*     $Log$
*     Revision 1.8  2005/03/23 08:02:46  timj
*     Use NYQUIST as the input parameter instead of DIAMETER and WAVELENGTH
*
*     Revision 1.7  2005/03/18 19:32:45  timj
*     Replace plotting directly with PGPLOT to plotting using KAPLIBS and AST.
*
*     Revision 1.6  2004/09/08 02:03:33  timj
*     Add CNF_PVAL where appropriate
*
*     Revision 1.5  1999/08/19 03:37:42  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.4  1999/08/03 20:36:42  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     Revision 1.3  1998/06/03 23:34:18  timj
*     Free IJPOS_PTR. Add (commented out) GRID code.
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
      INCLUDE 'AST_PAR'                          ! AST constants

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
      DOUBLE PRECISION NYQUIST
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
      LOGICAL ALIGN                         ! DATA pic. aligned with a previous picture?
      INTEGER BIN_PTR                       ! Binned data
      INTEGER BIN_PTR_END                   ! End of BIN_PTR
      INTEGER BIN_POS_PTR                   ! Position of binned data
      INTEGER BIN_POS_END                   ! End of BIN_POS
      DOUBLE PRECISION BOX(4)               ! Dummy bounding box for dummy plot
      INTEGER GRID_END                      ! End of scratch array
      INTEGER GRID_PTR                      ! Scratch space
      INTEGER I                             ! Loop counter
      INTEGER IJPOS_PTR                     ! Ptr to array containing I,J's
      INTEGER IJPOS_END                     ! End of IJPOS_PTR
      INTEGER IMAX                          ! I pos of hist max
      INTEGER IPICD                         ! AGI identifier for the dummy DATA picture
      INTEGER IPICF                         ! AGI identifier for the dummy frame picture
      INTEGER IPLOT                         ! Pointer to AST Plot for dummy DATA picture
      INTEGER IPOS_END                      ! End of ipos_ptr
      INTEGER IPOS_PTR                      ! I positions in lookup table
      INTEGER JPOS_END                      ! End of jpos_ptr
      INTEGER JPOS_PTR                      ! J positions in lookup table
      INTEGER JMAX                          ! J pos of hist max
      REAL    MARGIN(4)                     ! Dummy margin for dummy plot
      INTEGER NFRM            ! Frame index increment between IWCS and IPLOT
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

* Variables required if GRID writing is turned back on:
*      INTEGER GRNDF                         ! NDF identifier
*      INTEGER GRPNTR                        ! Pointer to mapped data
*      INTEGER IERR                          ! For VEC_
*      INTEGER ITEMP                         ! Temp integer
*      INTEGER LBND ( 2 )                    ! Lower bounds of NDF
*      INTEGER NERR                          ! For VEC_
*      INTEGER UBND ( 2 )                    ! Upper bounds of NDF

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

      PLOT = .FALSE.

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

      OUT_PIXEL = REAL(NYQUIST) / 2.0

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

      DO I = 1, N_FILES

*     Fill the array with data. (1 file at a time)

         CALL SURFLIB_CALC_IJPOS(N_PTS(I), DBLE(OUT_PIXEL), ICEN, JCEN,
     :        %VAL(CNF_PVAL(BOL_RA_PTR(I))),
     :        %VAL(CNF_PVAL(BOL_DEC_PTR(I))),
     :        %VAL(CNF_PVAL(IJPOS_PTR) + (2 * OFFSET * VAL__NBR)),
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
     :        %VAL(CNF_PVAL(DATA_PTR(I))),
     :        %VAL(CNF_PVAL(QUALITY_PTR(I))), BADBIT(I),
     :        %VAL(CNF_PVAL(IJPOS_PTR) + (2 * OFFSET * VAL__NBI)),
     :        %VAL(CNF_PVAL(GRID_PTR)), IMAX, JMAX, NMAX, STATUS)


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
         CALL SCULIB_CFILLI(NX * NY, 0, %VAL(CNF_PVAL(GRID_PTR)))
         CALL SCULIB_CFILLR(NX * NY * NMAX, VAL__BADR,
     :                      %VAL(CNF_PVAL(BIN_PTR)))
         CALL SCULIB_CFILLI(NX * NY * NMAX, VAL__BADI,
     :        %VAL(CNF_PVAL(BIN_POS_PTR)))
      END IF

*     Now we need to copy the data into BIN_PTR and the positions
*     into BIN_POS_PTR.

      OFFSET = 0

      DO I = 1, N_FILES

         CALL SURFLIB_FILL_GRID(N_PTS(I), NX, NY, NMAX, OFFSET,
     :        %VAL(CNF_PVAL(DATA_PTR(I))),
     :        %VAL(CNF_PVAL(QUALITY_PTR(I))), BADBIT(I),
     :        %VAL(CNF_PVAL(IJPOS_PTR) + (2 * OFFSET * VAL__NBI)),
     :        %VAL(CNF_PVAL(GRID_PTR)), %VAL(CNF_PVAL(BIN_PTR)),
     :        %VAL(CNF_PVAL(BIN_POS_PTR)),
     :        STATUS)

*     Calculate the offset in the position array based on file number
*     for the next time round the loop
         OFFSET = OFFSET + N_PTS(I)

      END DO


*     Free the scratch memory used for the histogram and counting the
*     current position in the array.

      CALL SCULIB_FREE ('GRID_PTR', GRID_PTR, GRID_END, STATUS)
      CALL SCULIB_FREE ('IJPOS_PTR', IJPOS_PTR, IJPOS_END, STATUS)

*     Some scratch space for storing the numbers (size nmax)
*     in each bin. This scratch space is used in
*     SURFLIB_PLOT_GRID as well as SURFLIB_STATS_GRID
*     We use DOUBLE as that is required by AST in PLOT_GRID and is
*     fine for scratch space in STATS_GRID

      CALL SCULIB_MALLOC(NMAX * VAL__NBD, PNT_PTR, PNT_END, STATUS)
      CALL SCULIB_MALLOC(NMAX * VAL__NBD, SCRATCH_PTR, SCRATCH_END,
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

      IF (STATUS .EQ. SAI__OK) THEN

         PLOT = .FALSE.  ! Start by assuming no plot

*     This is going to be a call to a bogus plot device so that we can
*     determine whether the plot is required so that we can decide
*     which further parameters we need before calculating statistics.
*     It does not matter what BOX or MARGIN are set to as the device is closed
*     immediately. The important thing is that the status of the DEVICE parameter
*     is retained.
         MARGIN(1) = 0.1
         MARGIN(2) = 0.1
         MARGIN(3) = 0.1
         MARGIN(4) = 0.1

         BOX(1) = 0.0D0
         BOX(2) = 0.0D0
         BOX(3) = 1.0D0
         BOX(4) = 1.0D0

         CALL KPG1_PLOT( AST__NULL, 'NEW', 'SURF_DESPIKE',
     :        ' ', MARGIN, 0, ' ', ' ', 0.0, 0,' ',
     :        BOX, IPICD, IPICF, 0, IPLOT, NFRM, ALIGN, STATUS )

         IF (STATUS .EQ. PAR__NULL) THEN

*     Assume that a null means that people dont want the plot but do
*     want the despike
            CALL ERR_ANNUL(STATUS)

         ELSE IF (STATUS .EQ. SAI__OK) THEN

*     Close the dummy plot and indicate that a plot is required later
            CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )
            PLOT = .TRUE.

         END IF

      END IF

*     Since we are plotting then we can ask about the display mode

*     At this point we also need to find out the type of unwrapping
*     to use.

*     Find out the display mode if we are smoothing or if we are plotting

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((SMODE .NE. 'NONE') .OR. PLOT) THEN

            CALL PAR_CHOIC('DMODE', 'SPIRAL',
     :           'SPIRAL,XLINEAR,YLINEAR,DIAG1,DIAG2',
     :           .TRUE., UMODE, STATUS)

         END IF
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
      CALL SCULIB_MALLOC(3 * NX * NY * VAL__NBR, STATS_PTR, STATS_END,
     :     STATUS)

*     Calculate stats using the specified smoothing mode

      CALL SURFLIB_STATS_GRID(SMODE, NX, NY, NMAX, NSIGMA,
     :     %VAL(CNF_PVAL(IPOS_PTR)),
     :     %VAL(CNF_PVAL(JPOS_PTR)), %VAL(CNF_PVAL(BIN_PTR)),
     :     %VAL(CNF_PVAL(PNT_PTR)),
     :     %VAL(CNF_PVAL(STATS_PTR)), STATUS)

*     Write the median image to disk - now use MEDIAN regridding option
*      LBND ( 1 ) = 1
*      LBND ( 2 ) = 1
*      UBND ( 1 ) = NX
*      UBND ( 2 ) = NY
*      IF (STATUS .EQ. SAI__OK) THEN
*         CALL NDF_CREAT('GRID','_REAL', 2, LBND, UBND, GRNDF, STATUS)
*
*         IF (STATUS .NE. PAR__NULL) THEN
*
*            CALL NDF_MAP(GRNDF, 'DATA', '_REAL', 'WRITE', GRPNTR,
*     :           ITEMP, STATUS)
*            CALL VEC_RTOR(.FALSE., NX * NY, %VAL(STATS_PTR),
*     :           %VAL(GRPNTR), IERR, NERR, STATUS)
*
*            CALL NDF_UNMAP(GRNDF, '*', STATUS)
*            CALL NDF_ANNUL(GRNDF, STATUS)
*         ELSE
*            CALL ERR_ANNUL(STATUS)
*         END IF
*      END IF

*     Do this because:
*       1. More efficient to calculate stats once rather than for each plot
*       2. Allows smoothing of the ranges (although not sure this
*          is a good thing for bright sources).

******* PLOTTING **************************************

      IF (PLOT) THEN

*     Plot the data
         CALL SURFLIB_PLOT_GRID(UNIT, NX, NY, NMAX, NSIGMA,
     :        %VAL(CNF_PVAL(IPOS_PTR)), %VAL(CNF_PVAL(JPOS_PTR)),
     :        %VAL(CNF_PVAL(BIN_PTR)),
     :        %VAL(CNF_PVAL(STATS_PTR)), %VAL(CNF_PVAL(PNT_PTR)),
     :        %VAL(CNF_PVAL(SCRATCH_PTR)),
     :        STATUS)

*     Finish the plotting excursion

      END IF

****** CLIPPING *******************

*     Find the stats and clip the data.
*     Note that I do realise that SURFLIB_PLOT_GRID already calculates
*     the mean etc, but doesn't really help me here since it is
*     difficult to relate back to actual quality.

      CALL SURFLIB_CLIP_GRID(N_FILES, N_PTS, NX, NY, NMAX,
     :     N_BOLS, BITNUM, DATA_PTR, QUALITY_PTR,
     :     %VAL(CNF_PVAL(BIN_PTR)),
     :     %VAL(CNF_PVAL(BIN_POS_PTR)), %VAL(CNF_PVAL(STATS_PTR)),
     :     %VAL(CNF_PVAL(IPOS_PTR)),
     :     %VAL(CNF_PVAL(JPOS_PTR)), NSPIKES, STATUS)

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
