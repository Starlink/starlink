      SUBROUTINE SURFLIB_CLIP_GRID(N_FILES, N_PTS, NX, NY, NMAX,
     :     N_BOLS, BITNUM, DATA_PTR, QUALITY_PTR, BINS, BIN_POS, STATS,
     :     IPOS, JPOS, NSPIKES, STATUS)
*+
*  Name:
*     SURFLIB_CLIP_GRID

*  Purpose:
*     Remove spikes from grid

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SURFLIB_CLIP_GRID(N_FILES, N_PTS, NX, NY, NMAX, NSIGMA,
*    :     N_BOLS, BITNUM, QUALITY_PTR, BINS, BIN_POS, STATS, IPOS,
*    :     JPOS, NSPIKES, STATUS)

*  Description:
*     Given the binned data calculated by SURFLIB_FILL_GRID
*     and the limits calculated by SURFLIB_STATS_GRID,
*     clip all points that lie outside the statistical limits provided
*     by the stats array.

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
*     STATS (NX, NY, 3) = REAL (Given)
*       Statistics of each bin. 1=Median, 2=High, 3=Low
*     PNTS(NMAX) = REAL (Given)
*       Scratch space for copying in the data from each I,J
*     IPOS(NX*NY) = INTEGER (Given)
*       I position in array
*     JPOS(NX*NY) = INTEGER (Given)
*       J position in array
*     NSPIKES = INTEGER (Returned)
*       Number of spikes removed for a given file
*     STATUS = INTEGER (Given & Returned)
*       Global Status

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)

*  Notes:


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     Original version: Timj, 1997 Oct 21
*     $Log$
*     Revision 1.5  2005/03/23 08:00:04  timj
*     Fix CNF_PVAL error
*
*     Revision 1.4  2004/09/01 01:02:03  timj
*     use CNF_PVAL
*
*     Revision 1.3  1999/08/03 19:32:48  timj
*     Add copyright message to header.
*
*     Revision 1.2  1997/11/12 00:10:48  timj
*     Pass STATS into subroutine rather than calculating inside.
*
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
      INCLUDE 'CNF_PAR'                          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER NMAX
      INTEGER N_FILES
      INTEGER N_BOLS(N_FILES)
      INTEGER N_PTS(N_FILES)
      INTEGER BITNUM
      REAL    STATS(NX, NY, 3)
      REAL    BINS (NX, NY, NMAX)
      INTEGER BIN_POS (NX, NY, NMAX)
      INTEGER IPOS(NX * NY)
      INTEGER JPOS(NX * NY)

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
      INTEGER N               ! Z coordinate
      INTEGER NBS             ! X coordinate in input file
      INTEGER NN              ! Loop variable
      INTEGER NERR            ! For VEC_
      INTEGER NPS             ! Y position in input file
      INTEGER OFFSET          ! OFFSET into array from pointer
      INTEGER REAL_FILE       ! File number associated with point
      INTEGER REAL_POS        ! Position in file of point
      INTEGER QPTR            ! Temporary pointer
*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Initialise the spike counting
      DO I = 1, N_FILES
         NSPIKES(I) = 0
      END DO

*     Loop over each position in the grid

      DO COUNT = 1, NX*NY

*     Calculate the I,J corresponding to this pixel number
*     There is in fact no need to use the unwrapping pointers
*     to find (I,J) except that people might want to see where their
*     bad points correspond to position on the plotted image.

*         J = INT((COUNT - 1) / NX) + 1
*         I = COUNT - ((J-1) * NX)

         I = IPOS(COUNT)
         J = JPOS(COUNT)

*     High and low clipping values are stored in the STATS array
         HIGH = STATS(I,J,2)
         LOW  = STATS(I,J,3)

*     Find the range outside of which  lie spikes
*     Use the mean (as long as it is good)

         IF ((HIGH .NE. VAL__BADR) .AND. (LOW .NE. VAL__BADR)) THEN

*     Go through the data for this bin

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

                     OFFSET = (REAL_POS - 1) * VAL__NBUB
                     QPTR = QUALITY_PTR(REAL_FILE)

                     BTEMP = SCULIB_BITON(%VAL(CNF_PVAL(QPTR)+OFFSET),
     :                    BITNUM)

                     CALL VEC_UBTOUB(.FALSE., 1, BTEMP,
     :                    %VAL(CNF_PVAL(QPTR)+OFFSET),
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

                     OFFSET = (REAL_POS - 1) * VAL__NBR
                     QPTR = DATA_PTR(REAL_FILE)

                     CALL VEC_RTOR(.FALSE., 1, STATS(I,J,1),
     :                    %VAL(CNF_PVAL(QPTR)+OFFSET),
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

      END
