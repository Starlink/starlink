      SUBROUTINE SURFLIB_CLIP_GRID(N_FILES, N_PTS, NX, NY, NMAX, NSIGMA,
     :     N_BOLS, BITNUM, DATA_PTR, QUALITY_PTR, BINS, BIN_POS, PNTS, 
     :     NSPIKES, STATUS)
*+
*  Name:
*     SURFLIB_CLIP_GRID

*  Purpose:
*     Remove spikes from grid

*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL SURFLIB_CLIP_GRID(N_FILES, N_PTS, NX, NY, NMAX, NSIGMA,
*    :     N_BOLS, BITNUM, QUALITY_PTR, BINS, BIN_POS, PNTS, NSPIKES,
*    :     STATUS)

*  Description:
*     Given the binned data calculated by SURFLIB_FILL_GRID
*     Calculates the mean and standard deviation of all the data
*     in each cell and then clips at NSIGMA sigma.

*  Arguments:
*     N_FILES = INTEGER (Given)
*       Number of input data sets
*     N_PTS(N_FILES) = INTEGER (Given)
*       Number of points in each data set
*     NX = INTEGER (Given)
*       Size of X dimension
*     NY = INTEGER (Given)
*       Size of Y dimension
*     NMAX = INTEGER (Given)
*       Maximum value allowed for third dimension of BINS
*     NSIGMA = REAL (Given)
*       Sigma clipping level
*     N_BOLS ( N_FILES ) = INTEGER (Given)
*       Number of bolometers per file (used for constructing message for users)
*     BITNUM = INTEGER (Given)
*       Bit number that is modified when set bad
*     DATA_PTR( N_FILES ) = INTEGER (Given & Returned)
*       The input data with each spike replaced with a median value
*     QUALITY_PTR(N_FILES) = INTEGER (Given & Returned)
*       Pointers to quality arrays. Note that quality arrays are modified.
*       (The whole point of this routine) even though the pointers are not.
*     BINS(NX, NY, NMAX) = REAL (Given)
*       The data stored in relation to its position
*     BIN_POS (NX, NY, NMAX) = INTEGER (Given)
*       Position of each data point in quality arrays
*     PNTS(NMAX) = REAL (Given)
*       Scratch space for copying in the data from each I,J
*     NSPIKES = INTEGER (Returned)
*       Number of spikes removed for a given file
*     STATUS = INTEGER (Given & Returned)
*       Global Status

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)

*  Notes:

*  History:
*     Original version: Timj, 1997 Oct 21
*     $Log$
*     Revision 1.1  1997/11/10 19:42:03  timj
*     Initial revision
*
*
*
*

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'                          ! Bad values
      INCLUDE 'PAR_ERR'                          ! For PAR_NULL
      INCLUDE 'MSG_PAR'                          ! For MSG__VERB

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER NMAX
      INTEGER N_FILES
      INTEGER N_BOLS(N_FILES)
      INTEGER N_PTS(N_FILES)
      INTEGER BITNUM
      REAL    NSIGMA
      REAL    BINS (NX, NY, NMAX)
      REAL    PNTS(NMAX)
      INTEGER BIN_POS (NX, NY, NMAX)

*  Arguments Given & Returned:
      INTEGER DATA_PTR(N_FILES)
      INTEGER QUALITY_PTR(N_FILES)

*  Arguments Returned:
      INTEGER NSPIKES(N_FILES)

*     Status
      INTEGER STATUS

*  External References:
      BYTE    SCULIB_BITON

*  Local Variables:
      BYTE    BTEMP           ! Temporary byte
      INTEGER COUNT           ! Loop counter
      REAL    HIGH            ! Mean + sigma range
      INTEGER I               ! Loop variable
      INTEGER IERR            ! For VEC_
      INTEGER J               ! J coordinate
      REAL    LOW             ! Mena - sigma range
      DOUBLE PRECISION MEAN   ! Mean
      DOUBLE PRECISION MEDIAN ! Median
      INTEGER N               ! Z coordinate
      INTEGER NBS             ! X coordinate in input file
      INTEGER NN              ! Loop variable
      INTEGER NERR            ! For VEC_
      INTEGER NGOOD           ! Number of good points
      INTEGER NPS             ! Y position in input file
      INTEGER REAL_FILE       ! File number associated with point
      INTEGER REAL_POS        ! Position in file of point
      DOUBLE PRECISION STDEV  ! Standard deviation
      DOUBLE PRECISION SUM    ! Sum of data
      DOUBLE PRECISION SUMSQ  ! Sum of squares
      INTEGER QPTR            ! Pointer to quality byte
      INTEGER QUAL_END        ! end of dummy quality array
      INTEGER QUAL_PTR        ! Dummy quality array
      INTEGER QSORT_END       ! End of sorted array
      INTEGER QSORT_PTR       ! Sort array for statr

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Initialise pointers
      QUAL_PTR = 0
      QUAL_END = 0
      QSORT_PTR = 0
      QSORT_END = 0

*     Initialise the spike counting
      DO I = 1, N_FILES
         NSPIKES(I) = 0
      END DO

*     Get some memory for the stats routine

      CALL SCULIB_MALLOC(NMAX * VAL__NBR, QSORT_PTR, QSORT_END,
     :     STATUS)
      CALL SCULIB_MALLOC(NMAX * VAL__NBUB, QUAL_PTR, QUAL_END,
     :     STATUS)

*     Fill this dummy quality array with 0
      BTEMP = 0
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_CFILLB(NMAX, BTEMP, %VAL(QUAL_PTR))
      END IF      


*     Loop over each position in the grid

      DO COUNT = 1, NX*NY

*     Calculate the I,J corresponding to this pixel number

         J = INT((COUNT - 1) / NX) + 1 
         I = COUNT - ((J-1) * NX)

*     Copy the data for an I,J into the scratch array

         DO N = 1, NMAX

            PNTS(N) = BINS(I,J,N)

         END DO

*     Find the mean and sigma (Quality array is a dummy for this)
         CALL SCULIB_STATR(NMAX, NSIGMA, PNTS, %VAL(QUAL_PTR), 
     :        BTEMP, NGOOD, MEAN, MEDIAN, SUM, SUMSQ, STDEV, 
     :        %VAL(QSORT_PTR), STATUS)

*     Find the range outside of which  lie spikes
*     Use the mean (as long as it is good)

         IF (MEAN .NE. VAL__BADD .AND. STDEV .NE. VAL__BADD) THEN
         
            HIGH = REAL(MEAN) + (NSIGMA * REAL(STDEV))
            LOW  = REAL(MEAN) - (NSIGMA * REAL(STDEV))

*     Go through the data for this bin again

            DO N = 1, NMAX
               
               IF ((BINS(I,J,N) .LT. LOW .OR. BINS(I,J,N) .GT. HIGH)
     :              .AND. BINS(I,J,N) .NE. VAL__BADR) THEN

*     We have a bad point. Now need to decode the position into
*     a place in the data array
*     Subtract N_PTS for each file until the number of points left
*     is less than the number of points in the current file

                  REAL_POS = BIN_POS(I,J,N)
                  REAL_FILE = 0
                  NN = 1

                  DO WHILE (REAL_FILE .EQ. 0 .AND. NN.LE.N_FILES)

                     IF (REAL_POS .GT. N_PTS(NN)) THEN
                        REAL_POS = REAL_POS - N_PTS(NN)
                     ELSE
                        REAL_FILE = NN
                     END IF
                     
                     NN = NN + 1
                     
                  END DO

*     Mark the point bad (need to use indirection because of the
*     sodding pointers)
*     Make sure that we are in range (although we should be given
*     the above)

                  IF (REAL_FILE .LE. N_FILES .AND. 
     :                 REAL_POS .LE. N_PTS(REAL_FILE)) THEN

                     QPTR = QUALITY_PTR(REAL_FILE) + 
     :                    (REAL_POS - 1)*VAL__NBUB
                     BTEMP = SCULIB_BITON(%VAL(QPTR), BITNUM)
                  
                     CALL VEC_UBTOUB(.FALSE., 1, BTEMP, %VAL(QPTR),
     :                    IERR, NERR, STATUS)

*     It is conceivable that people will want to replace the spike with
*     a median value. This is possible in this routine but we have to make
*     sure that the input data is the version that we need to write out 
*     to the file. The problem here is that an SCUBA section (in this context)
*     works by setting all points not in the section to bad. Obviously
*     this would ruin the data when copied to the output file. Need to
*     work out how to give this routine untainted data AND let the 
*     SURFLIB_FILL_GRID routine only use the requested section.
*     Let's assume the data does not suffer from this problem.

                     QPTR = DATA_PTR(REAL_FILE) + 
     :                    (REAL_POS - 1) * VAL__NBR

                     CALL VEC_DTOR(.FALSE., 1, MEDIAN, %VAL(QPTR),
     :                    IERR, NERR, STATUS)


*     If this routine is being called iteratively (eg by a user
*     going round in a loop and selecting points by hand) then
*     we need to make sure that this data is no longer in the BINS
*     array as good. Set the spike to bad

                     BINS(I,J,N) = VAL__BADR


*     Store the number of spikes detected in each file
                     NSPIKES(REAL_FILE) = NSPIKES(REAL_FILE) + 1

*     Be nice and calculate where this spike was in the input file

                     NPS = INT((REAL_POS- 1) / N_BOLS(REAL_FILE)) + 1 
                     NBS = REAL_POS - ((NPS-1) * N_BOLS(REAL_FILE))

*     ...and write a message to the screen telling the user
*     the location of the spike

                     CALL MSG_SETI('NB', COUNT)
                     CALL MSG_SETI('NF', REAL_FILE)
                     CALL MSG_SETI('NPOS', NPS)
                     CALL MSG_SETI('NBOL', NBS)

                     CALL MSG_OUTIF(MSG__VERB, ' ', 
     :                    ' Spike found in bin ^NB, file ^NF, pos ('//
     :                    '^NBOL, ^NPOS)', STATUS)

                  END IF

               END IF


            END DO
         END IF

      END DO

*     Free the memory
      CALL SCULIB_FREE('QSORT', QSORT_PTR, QSORT_END, STATUS)
      CALL SCULIB_FREE('QUAL', QUAL_PTR, QUAL_END, STATUS)


      END
