      SUBROUTINE SURF_GRID_CALCSKY( TSKNAME, N_FILES, N_PTS, N_POS,
     :     N_BOLS, NYQUIST, IMNDF, N_M_FITS, MODEL_FITS,
     :     CHOP_THROW, CHOP_PA, BOX_SIZE, BOL_RA_PTR,  BOL_DEC_PTR,
     :     DATA_PTR, QUALITY_PTR, SKY_PTR, SKY_ERR, BADBIT, STATUS )
*+
*  Name:
*     SURF_GRID_CALCSKY

*  Purpose:
*     Calculate sky contribution from median image

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SURF_GRID_CALCSKY ( TSKNAME, N_FILES, N_PTS, N_POS, N_BOLS,
*    :     N_BOLS, NYQUIST, IMNDF, N_M_FITS, MODEL_FITS,
*    :     CHOP_THROW, CHOP_PA, BOX_SIZE, BOL_RA_PTR,  BOL_DEC_PTR,
*    :     DATA_PTR, QUALITY_PTR, SKY_PTR, SKY_ERR, BADBIT, STATUS )

*  Description:
*     This routine calculates the sky contribution by attempting
*     to remove the source from the input data stream. The source
*     signal can either be calculated by this routine or by reading
*     in a model of the source from a file.
*
*     When calculating the source structure internally a similar
*     method to that used by DESPIKE is employed. The input data
*     are placed into bins of size one quarter beamwidth. The median
*     of each bin is calculated and this is treated as the source
*     model (cf. REBIN_METHOD=MEDIAN in REBIN).
*
*     Once the source model is available, it is removed from
*     all of the input data. The source-removed data are then analysed
*     with the sky emission derived from the mean of the signal across
*     the array for all the sample times.
*
*     Since the sky signal is expected to vary on timesales of the
*     order of one second, an option is included for smoothing the
*     sky signal. This is especially useful for scan map data where
*     samples are taken at 7.8 Hz.
*


*  Arguments:
*     TSKNAME = CHARACTER (Given)
*       Name of task to be used in output messages
*     N_FILES = INTEGER (Given)
*       Number of data sets (ie files)
*     N_PTS ( N_FILES ) = INTEGER (Given)
*       Total number of points in each map
*     N_POS( N_FILES ) = INTEGER (Given)
*       Number of positions per set (Y positions)
*     N_BOLS( N_FILES ) = INTEGER (Given)
*       Number of bolometers per set (X positions)
*     NYQUIST = DOUBLE PRECISION (Given)
*       Nyquist sampling (radians)
*     IMNDF = INTEGER (Given)
*       NDF identifier of the supplied model. If this is equal to
*       NDF__NOID then no external model is used.
*     N_M_FITS = INTEGER (Given)
*       Number of FITS keywords in the model
*     MODEL_FITS = CHAR*(80) (Given)
*       FITS keywords from the model
*     CHOP_THROW = REAL (Given)
*       Size of chop throw for the first input image (arcsec)
*     CHOP_PA = REAL (Given)
*       Chop position angle of first input image (degrees)
*     BOX_SIZE ( N_FILES ) = INTEGER (Given)
*       Size of smoothing box - pixels
*     BOL_RA_PTR( N_FILES ) = INTEGER (Given)
*       Array of pointers to position information (X coords)
*       Note that each data set has positions for N_POS * N_BOLS
*     BOL_RA_PTR( N_FILES ) = INTEGER (Given)
*       Array of pointers to position information (Y coords)
*     DATA_PTR( N_FILES ) = INTEGER (Given, modified data)
*       Pointers to actual data arrays. The arrays are modified
*       and contain the source removed data on exit.
*     QUALITY_PTR( N_FILES ) = INTEGER (Given)
*       Pointer to quality arrays
*     SKY_PTR ( N_FILES ) = INTEGER (Pointer Given, data returned)
*       Storage space for the sky signal
*     SKY_ERR ( N_FILES ) = INTEGER (Pointer Given, data returned)
*       Storage space for the error on sky signal
*     BADBIT ( N_FILES ) = BYTE (Given)
*       Bad bit mask for identifying bad pixels from quality
*     STATUS = INTEGER (Given & Returned)
*       Global Status

*  ADAM Parameters:
*     ADDCHOP = LOGICAL (Read)
*       Governs whether to add a chop function to the supplied model.
*     BOXSZ = INTEGER (Read)
*       Size of smoothing box in seconds. This is used to smooth
*       the time series. Default is 2.0 seconds.
*     CHOP = REAL (Read)
*       Chop throw to add to model (arcsec)
*     IN = CHAR (Read)
*        The name of the input file to be processed. This parameter is
*        requested repeatedly until a NULL value (!) is supplied.
*        LOOP must be TRUE. IN can include a SCUBA section.
*        Like the REF parameter this parameter accepts a text file.
*     LOOP = LOGICAL (Read)
*        Task will ask for multiple input files if true. Only REF is read
*        if noloop.
*     MODEL = NDF (Read)
*       NDF containing the model of the source. The astrometry
*       is read from this file. The model must have been generated
*       by SURF since it relies on the presence of certain FITS keywords.
*     MSG_FILTER = CHAR (Read)
*         Message filter level. Allowed values are QUIET, NORM and
*         VERBOSE. Default is NORM.
*     NOSRC = NDF (Write)
*       File to store source removed data. This can be used to
*       check the source removal. Note that this output file can
*       not be used directly by SURF for further processing since
*       the header is incomplete. No file is written by default.
*     OUT_COORDS = CHAR (Read)
*        The coordinate system to be used for the model determination.
*        Available coordinate systems are:
*        - AZ:  Azimuth/elevation offsets
*        - NA:  Nasmyth offsets
*        - PL:  RA/Dec Offsets from moving centre (eg Planets)
*        - RB:  RA/Dec (B1950)
*        - RJ:  RA/Dec (J2000)
*        - RD:  RA/Dec (epoch of observation)
*        - GA:  Galactic coordinates (J2000)
*
*        For RD current epoch is taken from the first input file.
*     PA = REAL (Given)
*        Position angle of chop added to model (degrees)
*     REF = CHAR (Given)
*        The name of the first NDF to be processed. The name may also be the
*        name of an ASCII text file containing NDF and parameter values.
*        REF can include a SCUBA section. See REBIN for more information
*        on the format of the ASCII input file.
*     SHIFT_DX = REAL (Read)
*        The pointing shift (in X) to be applied that would bring the
*        maps in line. This is a shift in the output coordinte frame.
*     SHIFT_DY = REAL (Read)
*        The pointing shift (in Y) to be applied that would bring the
*        maps in line. This is a shift in the output coordinate frame.
*     WEIGHT = REAL (Read)
*       This parameter does nothing in CALCSKY. It must be present
*       when using text file input. Any value is allowed.

*  Usage:
*     calcsky ref

*  Examples:
*     calcsky test_rlb model=! \\
*       Calculate sky for test_rlb.sdf. Only read in one file and
*       don't use an external source model.
*     calcsky list.inp model=m82 noloop\\
*       Read in the files specified in list.inp and use m82.sdf
*       as a model of the source.
*     calcsky file nosrc=nosrc boxsz=10.0 \\
*       Calculate sky for file.sdf. Store the source subtracted image
*       in nosrc.sdf. Use a smoothing size of 10 seconds.

*  Notes:
*     - The model itself is only an approximation
*       to the data (since the data points can fall anywhere within
*       a given cell) so some source signal will remain after source
*       subtraction.
*     - If a model is supplied externally (via MODEL parameter) the
*       cell size and the map centre of the model
*       is used for the source subtraction.
*     - The sky signal is stored in an NDF extension (.MORE.REDS.SKY).
*       The file must be processed by REMSKY to actually remove the
*       sky contribution.


*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)

*  Related Applications:
*     SURF: REMSKY


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     Original version: Timj, 1997 Oct 20
*     $Log$
*     Revision 1.13  2005/03/23 08:02:46  timj
*     Use NYQUIST as the input parameter instead of DIAMETER and WAVELENGTH
*
*     Revision 1.12  2005/03/18 06:27:21  timj
*     Protect some calls with STATUS checks
*
*     Revision 1.11  2004/09/08 02:03:33  timj
*     Add CNF_PVAL where appropriate
*
*     Revision 1.10  2000/08/24 03:18:23  timj
*     Add more informative error message if all input data are bad
*
*     Revision 1.9  1999/08/19 21:18:13  timj
*     Remove debug print statements.
*     Fix memory leak when using a MODEL.
*
*     Revision 1.8  1999/08/19 03:37:42  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.7  1999/08/03 20:36:42  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     Revision 1.6  1999/07/26 20:38:01  timj
*     Improve the checking for 0.0. Some indenting.
*
*     Revision 1.5  1999/07/17 02:56:38  timj
*     Further refinement of the sky removal using model.
*
*     Revision 1.4  1999/07/15 20:27:39  timj
*     First stab at improving external model input
*
*     Revision 1.3  1998/06/16 04:51:57  timj
*     Add examples.
*
*     Revision 1.2  1998/06/16 04:41:29  timj
*     Add documentation
*
*     Revision 1.1  1998/06/10 04:01:36  timj
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
      INCLUDE 'DAT_PAR'                          ! DAT__SZLOC
      INCLUDE 'PRM_PAR'                          ! Bad values
      INCLUDE 'PAR_ERR'                          ! PAR__NULL
      INCLUDE 'MSG_PAR'                          ! MSG__NORM
      INCLUDE 'SURF_PAR'                         ! SURF constants
      INCLUDE 'NDF_PAR'                          ! NDF__NOID
      INCLUDE 'CNF_PAR'                          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * (*) TSKNAME
      INTEGER N_FILES
      INTEGER N_M_FITS
      BYTE    BADBIT ( N_FILES )
      INTEGER N_POS ( N_FILES )
      INTEGER N_PTS ( N_FILES )
      INTEGER N_BOLS ( N_FILES )
      INTEGER BOL_DEC_PTR ( N_FILES )
      INTEGER BOL_RA_PTR ( N_FILES )
      INTEGER BOX_SIZE ( N_FILES )
      INTEGER DATA_PTR ( N_FILES )
      INTEGER QUALITY_PTR ( N_FILES )
      CHARACTER*(*) MODEL_FITS(N_M_FITS)
      DOUBLE PRECISION NYQUIST
      INTEGER IMNDF
      INTEGER NX
      INTEGER NY
      INTEGER ICEN
      INTEGER JCEN
      REAL    CHOP_PA
      REAL    CHOP_THROW

*  Arguments Returned:
      INTEGER SKY_ERR ( N_FILES )
      INTEGER SKY_PTR ( N_FILES )

*  Status:
      INTEGER STATUS                        ! Global status

*  Local Constants:

*  Local Variables:
      LOGICAL ADDCHOP                       ! To add or not to add the chop
      INTEGER BIN_PTR                       ! Binned data
      INTEGER BIN_PTR_END                   ! End of BIN_PTR
      INTEGER BIN_POS_PTR                   ! Position of binned data
      INTEGER BIN_POS_END                   ! End of BIN_POS
      INTEGER BOX_DIV                       ! Box size div by 2
      BYTE    BTEMP                         ! Temporary byte
      INTEGER GRID_END                      ! End of scratch array
      INTEGER GRID_PTR                      ! Scratch space
      LOGICAL HAVE_MODEL                    ! Have external image for source
      INTEGER I                             ! Loop counter
      INTEGER IERR                          ! For VEC_
      INTEGER IJPOS_PTR                     ! Ptr to array containing I,J's
      INTEGER IJPOS_END                     ! End of IJPOS_PTR
      INTEGER IMAX                          ! I pos of hist max
      INTEGER IPOS_END                      ! End of ipos_ptr
      INTEGER IPOS_PTR                      ! I positions in lookup table
      INTEGER ISTART                        ! Start of count
      INTEGER ISTOP                         ! End of count
      INTEGER ITEMP                         ! Temp integer
      INTEGER J                             ! Loop variable
      INTEGER JPOS_END                      ! End of jpos_ptr
      INTEGER JPOS_PTR                      ! J positions in lookup table
      INTEGER JMAX                          ! J pos of hist max
      INTEGER K                             ! Loop counter
      INTEGER LBND ( 2 )                    ! lower bounds of data array
      DOUBLE PRECISION MEAN                 ! Mean of sky
      DOUBLE PRECISION MEDIAN               ! Median of sky
      REAL    MODEL_PA                      ! Chop pa to add to model
      INTEGER MODEL_PTR                     ! Mapped data array of model
      REAL    MODEL_THROW                   ! Chop throw to add to model
      INTEGER NGOOD                         ! Number of good sky points
      INTEGER NERR                          ! For VEC_
      INTEGER NMAX                          ! Max entries per cell
      REAL    NSIGMA                        ! Despiking level
      INTEGER OFFSET                        ! Offset in data array
      REAL    OUT_PIXEL                     ! Pixel size in radians
      DOUBLE PRECISION PIXELSZ              ! Pixel size in image (ARCSEC)
      INTEGER PNT_END                       ! End of PNT_PTR
      INTEGER PNT_PTR                       ! Work space for gridded points
      REAL    RTEMP                         ! Scratch real
      INTEGER SCRATCH_END                   ! Scratch space
      INTEGER SCRATCH_PTR                   ! Scratch space
      INTEGER SCRATCH2_END                  ! Scratch space
      INTEGER SCRATCH2_PTR                  ! Scratch space
      INTEGER SCRATCHUB_END                 ! Scratch space
      INTEGER SCRATCHUB_PTR                 ! Scratch space
      CHARACTER * (10) SMODE                ! Smoothing mode
      INTEGER STATS_END                     ! End of STATS_PTR
      INTEGER STATS_PTR                     ! Bin statistics
      DOUBLE PRECISION STDEV                ! Standard deviation
      DOUBLE PRECISION SUM                  ! Sum of data
      DOUBLE PRECISION SUMSQ                ! Sum of squares data
      INTEGER TOT_PTS                       ! Total number of points to despike
      INTEGER UBND(2)                       ! Upper bounds of data array
      CHARACTER * (10) UMODE                ! Pixel unwrapping mode

      INTEGER GRPNTR
      INTEGER GRNDF

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

*     First need to find out how many data points we are dealing with

      TOT_PTS = 0

      DO I = 1, N_FILES

         TOT_PTS = TOT_PTS + N_PTS(I)

      END DO

*     Check to see whether we have an input model or not. We can decide
*     this by seeing whether IMNDF has a valid NDF id or not (NDF__NOID)
*     The model can be used as our source model from which we can derive
*     sky fluctuations.
*     Note that this model image must cover the full region covered
*     by the data (although pixel scale is not important since I can read
*     that from the header)

      IF (IMNDF .NE. NDF__NOID) THEN

         HAVE_MODEL = .TRUE.

*     Read PIXELSZ (pixel scale in arcsec)
         CALL SCULIB_GET_FITS_D(SCUBA__MAX_FITS, N_M_FITS, MODEL_FITS,
     :        'SCUPIXSZ', PIXELSZ, STATUS)

         OUT_PIXEL = PIXELSZ / R2AS

*     Get the size of the image and the indices of the reference pixel
         CALL NDF_BOUND (IMNDF, 2, LBND, UBND, ITEMP, STATUS)
         NX = UBND(1) - LBND(1) + 1
         NY = UBND(2) - LBND(2) + 1

         IF (ITEMP .NE. 2) THEN
            IF (STATUS .EQ. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP(' ','^TASK: Model image is not '//
     :              '2-dimensional', STATUS)
            END IF
         END IF

*     For new SURF tasks the reference pixel is defined by the
*     pixel coordinate frame 0,0. For older SURF we can read the
*     FITS header. The safest thing to do is to read the FITS
*     header first and then try the pixel origin.

         IF (STATUS .EQ. SAI__OK) THEN
            CALL SCULIB_GET_FITS_I(SCUBA__MAX_FITS, N_M_FITS,MODEL_FITS,
     :           'CRPIX1', ICEN, STATUS)
            CALL SCULIB_GET_FITS_I(SCUBA__MAX_FITS, N_M_FITS,MODEL_FITS,
     :           'CRPIX2', JCEN, STATUS)

*     Status bad - resort to calculating it from the bounds
            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_ANNUL(STATUS)
               ICEN = 1 - LBND(1)
               JCEN = 1 - LBND(2)
            END IF

         END IF

*     If no model supplied we have to derive the source signal from the
*     data. Calculate ourselves
      ELSE

         HAVE_MODEL = .FALSE.

*     Choose a pixel size based on wavelength
*     and dish diameter.
*     Try for quarter beam size first.

         OUT_PIXEL = REAL(NYQUIST) / 2.0

*     Now find out how big an output grid is needed

         CALL SURFLIB_CALC_OUTPUT_GRID(N_FILES, N_PTS, OUT_PIXEL,
     :        BOL_RA_PTR, BOL_DEC_PTR, NX, NY, ICEN, JCEN, STATUS)

      END IF

*      PRINT *, 'grid ', nx, ny, icen, jcen


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

         IF (STATUS .EQ. SAI__OK) THEN

            CALL SURFLIB_HISTOGRAM_GRID( N_PTS(I), NX, NY, .TRUE.,
     :           %VAL(CNF_PVAL(DATA_PTR(I))),
     :           %VAL(CNF_PVAL(QUALITY_PTR(I))), BADBIT(I),
     :           %VAL(CNF_PVAL(IJPOS_PTR) + (2 * OFFSET * VAL__NBI)),
     :           %VAL(CNF_PVAL(GRID_PTR)), IMAX, JMAX, NMAX, STATUS)

*     If status comes back as SAI__WARN our model does not cover
*     the same area as the data. This is fine, but we should warn
*     the user
            IF (STATUS .EQ. SAI__WARN) THEN
               CALL ERR_ANNUL(STATUS)

               IF (I .EQ. 1) THEN
                  CALL MSG_SETC('TSK',TSKNAME)
                  CALL MSG_OUTIF(MSG__QUIET,' ','^TSK Warning: '//
     :                 'The model covers a smaller area than the data',
     :                 STATUS)
               END IF
            END IF

*     Check that NMAX is reasonable
            IF (STATUS .EQ. SAI__OK .AND. NMAX .LE. 0) THEN
               CALL MSG_SETI('N', NMAX)
               STATUS = SAI__ERROR
               CALL ERR_REP(' ','SURF_GRID_CALCSKY: No data points'//
     :              ' in grid (NMAX=^N). Is all data bad?',
     :              STATUS)

            END IF

*     Calculate the offset in the position array based on file number
*     for the next time round the loop
            OFFSET = OFFSET + N_PTS(I)

         END IF

      END DO

*     Free the histogram memory. Do it here since we will not need
*     it any more if a model is supplied.
      CALL SCULIB_FREE('GRID_PTR', GRID_PTR, GRID_END, STATUS)

*     If we have an input image then the STATS for the image
*     are simply the mapped input data (dont care about variance)

      IF (HAVE_MODEL) THEN

         CALL NDF_MAP(IMNDF, 'DATA', '_REAL', 'READ', MODEL_PTR,
     :        ITEMP, STATUS)

*     It should be possible to add on the dual beam chop if required
*     at this point. It is difficult to know whether we should do
*     it without asking since we can not always assume that a CHOP_PA
*     CHOP_THROW in the header indicates that the image has not had
*     the chop signature removed.

*     Ask the user to see whether we wish to add a chopped signal
         CALL PAR_GET0L('ADDCHOP', ADDCHOP, STATUS)

         IF (ADDCHOP) THEN

*     Assume it is a dual beam signal
*     Now ask for the throw (in arcsec) and position angle
*     Defaults come from the first input file

*     Chop throw
            CALL PAR_DEF0R('CHOP', CHOP_THROW, STATUS)
            CALL PAR_GET0R('CHOP', MODEL_THROW, STATUS)

*     Convert chop throw to pixels
            MODEL_THROW = MODEL_THROW / PIXELSZ

*     Position angle
            CALL PAR_DEF0R('PA', CHOP_PA, STATUS)
            CALL PAR_GET0R('PA', MODEL_PA, STATUS)

*     Allocate some memory to store the chopped image
            STATS_PTR = 0
            STATS_END = 0
            CALL SCULIB_MALLOC(3 * NX * NY * VAL__NBR, STATS_PTR,
     :           STATS_END, STATUS)

*     Add the dual beam
            CALL SURFLIB_CALC_CHOPPED_IMAGE( 2, MODEL_THROW, MODEL_PA,
     :           NX, NY, %VAL(CNF_PVAL(MODEL_PTR)),
     :           %VAL(CNF_PVAL(STATS_PTR)), .FALSE.,
     :           0, 0, STATUS)

*     Write to disk - to test the dual beam
*            LBND(1) = 1
*            LBND(2) = 1
*            UBND(1) = NX
*            UBND(2) = NY
*            CALL NDF_PLACE(DAT__ROOT, 'model_test', ITEMP, STATUS)
*            CALL NDF_NEW('_REAL',2,LBND,UBND, ITEMP, GRNDF, STATUS)
*            CALL NDF_MAP(GRNDF, 'DATA','_REAL', 'WRITE', GRPNTR,
*     :           ITEMP, STATUS)
*            CALL VEC_RTOR(.FALSE., NX * NY,
*     :           %VAL(STATS_PTR),
*     :           %VAL(GRPNTR), IERR, NERR, STATUS)
*            CALL NDF_UNMAP(GRNDF, '*', STATUS)
*            CALL NDF_ANNUL(GRNDF, STATUS)

         ELSE

*     Copy the pointer
            STATS_PTR = MODEL_PTR

         END IF

      ELSE

*     Now we know where each pixel is. We need to put this data onto
*     a grid. From the histogram we know how big the array needs to
*     be that contains each data point.

*     Get some memory for the output grid
*     Need two arrays
*      1) Real array containing the data values (NX, NY, NMAX)
*      2) Integer array containing the positions for each value (NX,NY,NMAX)

         CALL SCULIB_MALLOC(NX * NY * NMAX * VAL__NBR, BIN_PTR,
     :        BIN_PTR_END, STATUS)

         CALL SCULIB_MALLOC(NX * NY * NMAX * VAL__NBI, BIN_POS_PTR,
     :        BIN_POS_END, STATUS)


*     We are going to use the histogram scratch space to keep track
*     of the current highest member used in BIN_PTR (etc).
*     Two options for doing this:
*       1. Reset to zero and increment each time a data point is entered.
*       2. Leave as is and decrement each time a data point is entered.
*     Not much difference so I will go for the increment option.
*     In fact, since the histogram scratch space may not be required
*     if we are using a MODEL then we have already freed the memory associated
*     with the histogram. Allocate some more now and then free it immediately
*     afterwards. This will have to be changed if we dont want to free it
*     before the IF (USEMODEL) section.

      CALL SCULIB_MALLOC(NX * NY * VAL__NBI, GRID_PTR, GRID_END,
     :     STATUS)


*     Initialise the work arrays

         IF (STATUS .EQ. SAI__OK) THEN
            CALL SCULIB_CFILLI(NX * NY, 0, %VAL(CNF_PVAL(GRID_PTR)))
            CALL SCULIB_CFILLR(NX * NY * NMAX, VAL__BADR,
     :                         %VAL(CNF_PVAL(BIN_PTR)))
            CALL SCULIB_CFILLI(NX * NY * NMAX, VAL__BADI,
     :           %VAL(CNF_PVAL(BIN_POS_PTR)))
         END IF

*     Now we need to copy the data into BIN_PTR and the positions
*     into BIN_POS_PTR.

         OFFSET = 0

         DO I = 1, N_FILES

            IF (STATUS .EQ. SAI__OK) THEN

               CALL SURFLIB_FILL_GRID(N_PTS(I), NX, NY, NMAX, OFFSET,
     :              %VAL(CNF_PVAL(DATA_PTR(I))),
     :              %VAL(CNF_PVAL(QUALITY_PTR(I))), BADBIT(I),
     :              %VAL(CNF_PVAL(IJPOS_PTR) + (2 * OFFSET * VAL__NBI)),
     :              %VAL(CNF_PVAL(GRID_PTR)), %VAL(CNF_PVAL(BIN_PTR)),
     :              %VAL(CNF_PVAL(BIN_POS_PTR)),
     :              STATUS)

*     If we have a return status of SAI__WARN, we can annull it without
*     action since we have already reported the associated message
               IF (STATUS .EQ. SAI__WARN) CALL ERR_ANNUL(STATUS)

*     Calculate the offset in the position array based on file number
*     for the next time round the loop
               OFFSET = OFFSET + N_PTS(I)

            END IF

         END DO


*     Free the scratch memory used for the histogram and counting the
*     current position in the array.

         CALL SCULIB_FREE ('GRID_PTR2', GRID_PTR, GRID_END, STATUS)

*     Some scratch space for storing the numbers (size nmax)
*     in each bin

         CALL SCULIB_MALLOC(NMAX * VAL__NBR, PNT_PTR, PNT_END, STATUS)
         CALL SCULIB_MALLOC(NMAX * VAL__NBR, SCRATCH_PTR, SCRATCH_END,
     :        STATUS)


*     Calculate the grid positions related to a given pixel index
*     Need to do this since some people want complicated spiral unwrapping
*     and it takes too long to calculate all this on the fly.

*     Allocate some memory

         IPOS_PTR = 0
         JPOS_PTR = 0
         IPOS_END = 0
         JPOS_END = 0

         CALL SCULIB_MALLOC(NX * NY * VAL__NBI, IPOS_PTR, IPOS_END,
     :        STATUS)
         CALL SCULIB_MALLOC(NX * NY * VAL__NBI, JPOS_PTR, JPOS_END,
     :        STATUS)

*     ...and calculate the new grid look up table

         UMODE = 'XLINEAR'
         CALL SURFLIB_CALC_GRIDIJ(UMODE, NX, NY, ICEN, JCEN,
     :        %VAL(CNF_PVAL(IPOS_PTR)), %VAL(CNF_PVAL(JPOS_PTR)),
     :        STATUS)


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
     :        STATUS)

*     Calculate stats using the specified smoothing mode

         SMODE = 'NONE'
         NSIGMA = 0.0
         CALL SURFLIB_STATS_GRID(SMODE, NX, NY, NMAX, NSIGMA,
     :        %VAL(CNF_PVAL(IPOS_PTR)),
     :        %VAL(CNF_PVAL(JPOS_PTR)), %VAL(CNF_PVAL(BIN_PTR)),
     :        %VAL(CNF_PVAL(PNT_PTR)),
     :        %VAL(CNF_PVAL(STATS_PTR)), STATUS)

*     Free memory
         CALL SCULIB_FREE('PNT_PTR', PNT_PTR, PNT_END, STATUS)
         CALL SCULIB_FREE('SCRATCH', SCRATCH_PTR, SCRATCH_END, STATUS)

*     Free positions
         CALL SCULIB_FREE('IPOS_PTR', IPOS_PTR, IPOS_END, STATUS)
         CALL SCULIB_FREE('JPOS_PTR', JPOS_PTR, JPOS_END, STATUS)


*     Free BIN_PTR
         CALL SCULIB_FREE('BIN_PTR', BIN_PTR, BIN_PTR_END, STATUS)
         CALL SCULIB_FREE('BIN_POS', BIN_POS_PTR, BIN_POS_END, STATUS)


      END IF

*     Now we have statistics so can now subtract the median
*     from each pixel in the input data
*     Loop over files so that we can pass data in rather than pointers

      OFFSET = 0
      DO I = 1, N_FILES

*     Subtract source model
         CALL SURFLIB_REM_GRID(N_PTS(I), NX, NY,
     :        %VAL(CNF_PVAL(IJPOS_PTR) + (2 * OFFSET * VAL__NBI)),
     :        %VAL(CNF_PVAL(STATS_PTR)), %VAL(CNF_PVAL(DATA_PTR(I))),
     :        STATUS)

*     Find new offset in lookup table
         OFFSET = OFFSET + N_PTS(I)

      END DO

*     Free Statistics and lookup table

*     Unmap any model
      IF (HAVE_MODEL) THEN
         CALL NDF_UNMAP(IMNDF, '*', STATUS)
      END IF

*     Free the memory if we didnt have a model or if we added a chop
*     to the model.
      IF (.NOT. HAVE_MODEL .OR. ADDCHOP) THEN
         CALL SCULIB_FREE('STATS_PTR', STATS_PTR, STATS_END, STATUS)
      END IF

*     Free the IJ lookup table
      CALL SCULIB_FREE ('IJPOS_PTR', IJPOS_PTR, IJPOS_END, STATUS)

*     Write data to file
      LBND(1) = 1
      LBND(2) = 1
      UBND(1) = N_BOLS(1)
      UBND(2) = N_POS(1)
      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_CREAT('NOSRC','_REAL', 2, LBND, UBND, GRNDF, STATUS)

         IF (STATUS .NE. PAR__NULL) THEN

            CALL NDF_MAP(GRNDF, 'DATA', '_REAL', 'WRITE', GRPNTR,
     :           ITEMP, STATUS)
            CALL VEC_RTOR(.FALSE., N_BOLS(1) * N_POS(1),
     :           %VAL(CNF_PVAL(DATA_PTR(1))),
     :           %VAL(CNF_PVAL(GRPNTR)), IERR, NERR, STATUS)
            CALL NDF_UNMAP(GRNDF, 'DATA', STATUS)
            CALL NDF_MAP(GRNDF, 'QUALITY', '_UBYTE', 'WRITE', GRPNTR,
     :           ITEMP, STATUS)
            CALL VEC_UBTOUB(.FALSE., N_BOLS(1) * N_POS(1),
     :           %VAL(CNF_PVAL(QUALITY_PTR(1))),
     :           %VAL(CNF_PVAL(GRPNTR)), IERR, NERR, STATUS)
            CALL NDF_UNMAP(GRNDF, 'QUALITY', STATUS)
            CALL NDF_SBB(BADBIT(1), GRNDF, STATUS)

            CALL NDF_ANNUL(GRNDF, STATUS)
         ELSE
            CALL ERR_ANNUL(STATUS)
         END IF
      END IF




*     The source has now been removed from the data.
*     We can now calculate the sky  by doing statistics
*     on each time slice and storing this in the sky array

*     Note that if the model is smaller than the scanned area, the
*     source subtraction will not have happened at the edges. This
*     will be a problem if there are sources on the edge of the map
*     since the average source flux will be removed and the edges
*     will be pulled down.

*     Allocate some scratch data space for the sorting
*     Simply allocate MAX_SCUBA_BOLS of data so that I don't
*     have to reallocate for each file since in principal there
*     can be different numbers of bolometers per file

      SCRATCH_END = 0
      SCRATCH_PTR = 0
      CALL SCULIB_MALLOC(SCUBA__NUM_CHAN * SCUBA__NUM_ADC * VAL__NBR,
     :     SCRATCH_PTR, SCRATCH_END, STATUS)

      SCRATCH2_END = 0
      SCRATCH2_PTR = 0
      CALL SCULIB_MALLOC(SCUBA__NUM_CHAN * SCUBA__NUM_ADC * VAL__NBR,
     :     SCRATCH2_PTR, SCRATCH2_END, STATUS)
      SCRATCHUB_END = 0
      SCRATCHUB_PTR = 0
      CALL SCULIB_MALLOC(SCUBA__NUM_CHAN * SCUBA__NUM_ADC * VAL__NBUB,
     :     SCRATCHUB_PTR, SCRATCHUB_END, STATUS)

*     Loop over files
      DO I = 1, N_FILES

         DO J = 1, N_POS(I)

            OFFSET = (J-1) * N_BOLS(I)

*     Need to remove all the points that are exactly 0.0D0 since
*     these do not provide a measure of the sky - ie the model fits
*     the data precisely which probably indicates that the
*     self-generated model only had one point in the relevant bin.
*     This is only relevant when using a self-generated model.
*     This is turned off when using an external model since then
*     the zeroes are relevant.

            NGOOD = 0
            DO K = 1, N_BOLS(I)

               CALL VEC_RTOR(.FALSE., 1,
     :   %VAL(CNF_PVAL(DATA_PTR(I)) + ((K-1) + OFFSET) * VAL__NBR),
     :              RTEMP, IERR, NERR, STATUS)

*       Skip if zero (but not if we are using an external model)
               IF (STATUS .EQ. SAI__OK .AND.
     :              RTEMP .NE. 0.0 .OR. HAVE_MODEL) THEN
                  CALL VEC_RTOR(.FALSE., 1, RTEMP,
     :                 %VAL(CNF_PVAL(SCRATCH2_PTR) + NGOOD*VAL__NBR),
     :                 IERR, NERR, STATUS)
                  CALL VEC_UBTOUB(.FALSE., 1,
     :   %VAL(CNF_PVAL(QUALITY_PTR(I)) + ((K-1)+OFFSET)* VAL__NBUB),
     :                 %VAL(CNF_PVAL(SCRATCHUB_PTR) + NGOOD*VAL__NBUB),
     :                 IERR, NERR, STATUS)

                  NGOOD = NGOOD + 1

               END IF

            END DO


*       Dont need to do anything special here. Just pass the data
*       to stats (bad pixel and all)

*            CALL SCULIB_STATR(N_BOLS(I), -1.0,
*     :           %VAL(DATA_PTR(I) + OFFSET * VAL__NBR),
*     :           %VAL(QUALITY_PTR(I) + OFFSET * VAL__NBUB),
            CALL SCULIB_STATR(NGOOD, -1.0,
     :           %VAL(CNF_PVAL(SCRATCH2_PTR)),
     :           %VAL(CNF_PVAL(SCRATCHUB_PTR)),
     :           BADBIT(I), ITEMP, MEAN, MEDIAN, SUM, SUMSQ,
     :           STDEV, %VAL(CNF_PVAL(SCRATCH_PTR)), STATUS)

*     Copy this value to the SKY_PTR array
            CALL VEC_DTOR(.TRUE., 1, MEDIAN,
     :           %VAL(CNF_PVAL(SKY_PTR(I)) + (J-1) * VAL__NBR),
     :           IERR, NERR, STATUS)
*     Copy in the error
            CALL VEC_DTOR(.TRUE., 1, STDEV,
     :           %VAL(CNF_PVAL(SKY_ERR(I)) + (J-1) * VAL__NBR),
     :           IERR, NERR, STATUS)

         END DO

      END DO

*     Free sort area.
      CALL SCULIB_FREE('SCRATCH', SCRATCH_PTR, SCRATCH_END, STATUS)
      CALL SCULIB_FREE('SCRAT2', SCRATCH2_PTR, SCRATCH2_END, STATUS)
      CALL SCULIB_FREE('SCRATUB', SCRATCHUB_PTR, SCRATCHUB_END, STATUS)

*     In some cases (eg SCAN/MAP) we need to smooth the time series
*     with a box average
*     In general the sky varies on a 10-20 second time scale whereas
*     we take scan map data at 1/8 second.

*     Loop over files
      DO I = 1, N_FILES

*     The number of pixels to bin over for each file is specified
*     in BIN_SIZE(I) and is passed in from above.
*     Divide size of bin by 2
         BOX_DIV = BOX_SIZE(I) / 2

*     Get some memory for scratch space
         CALL SCULIB_MALLOC(N_POS(I) * VAL__NBR, SCRATCH_PTR,
     :        SCRATCH_END, STATUS)
         CALL SCULIB_MALLOC(N_POS(I) * VAL__NBR, SCRATCH2_PTR,
     :        SCRATCH2_END, STATUS)
         CALL SCULIB_MALLOC(N_POS(I) * VAL__NBUB,
     :        SCRATCHUB_PTR, SCRATCHUB_END, STATUS)

*     Copy unsmoothed data into scratch space (variance from spread)
         CALL VEC_RTOR(.FALSE., N_POS(I), %VAL(CNF_PVAL(SKY_PTR(I))),
     :        %VAL(CNF_PVAL(SCRATCH_PTR)), IERR, NERR, STATUS)
         BTEMP = 0
         IF (STATUS .EQ. SAI__OK) THEN
            CALL SCULIB_CFILLB(N_POS(I), BTEMP,
     :                         %VAL(CNF_PVAL(SCRATCHUB_PTR)))
         END IF

*     Loop over positions
         IF (STATUS .EQ. SAI__OK) THEN
            DO J = 1, N_POS(I)

*     Determine start and end of box
               ISTART = MAX(1, J - BOX_DIV)
               ISTOP  = MIN(N_POS(I), J + BOX_DIV)

*     Now find stats of this section
               NGOOD = ISTOP - ISTART + 1

               CALL SCULIB_STATR(NGOOD, -1.0,
     :              %VAL(CNF_PVAL(SCRATCH_PTR) + (ISTART - 1)*VAL__NBR),
     :              %VAL(CNF_PVAL(SCRATCHUB_PTR)
     :                            + (ISTART - 1)*VAL__NBUB),
     :              BTEMP, ITEMP, MEAN, MEDIAN, SUM, SUMSQ,
     :              STDEV, %VAL(CNF_PVAL(SCRATCH2_PTR)), STATUS)

*     Copy result to output file
*     Copy this value to the SKY_PTR array
               CALL VEC_DTOR(.TRUE., 1, MEAN,
     :              %VAL(CNF_PVAL(SKY_PTR(I)) + (J-1) * VAL__NBR),
     :              IERR, NERR, STATUS)
*     Copy in the error if the status was good
*     if not (eg bin was 1 pixel wide). Just keep the variance
*     from the earlier calculation
               IF (STDEV .NE. VAL__BADD) THEN
                  CALL VEC_DTOR(.TRUE., 1, STDEV,
     :                 %VAL(CNF_PVAL(SKY_ERR(I)) + (J-1) * VAL__NBR),
     :                 IERR, NERR, STATUS)
               END IF

            END DO
         END IF

*     Free the scratch space
         CALL SCULIB_FREE('SCRAT_SM', SCRATCH_PTR, SCRATCH_END, STATUS)
         CALL SCULIB_FREE('SCRAT2_SM', SCRATCH2_PTR, SCRATCH2_END,
     :        STATUS)
         CALL SCULIB_FREE('SCRATUB_SM', SCRATCHUB_PTR, SCRATCHUB_END,
     :        STATUS)

      END DO

      END
