      SUBROUTINE SURFLIB_PLOT_GRID (UNIT, NX, NY, NMAX, 
     :     NSIGMA, IPOS, JPOS, BINS, STATS, PNTS, POS, STATUS)
*+
*  Name:
*     SURFLIB_PLOT_GRID

*  Purpose:
*     Plot unwrapped grid

*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL SURFLIB_PLOT_GRID(UNIT, NX, NY, NMAX, NSIGMA, IPOS, JPOS
*    :    BINS, STATS, PNTS, POS, STATUS)

*  Description:
*     Given the binned data calculated by SURFLIB_FILL_GRID
*     Plot the unwrapped data as 'data' against 'bin'
*     Use this as a first attempt at spike detection

*  Arguments:
*     UNIT = INTEGER (Given)
*       Display number
*     NX = INTEGER (Given)
*       Size of X dimension
*     NY = INTEGER (Given)
*       Size of Y dimension
*     NMAX = INTEGER (Given)
*       Maximum value allowed for third dimension of BINS
*     NSIGMA = REAL (Given)
*       How many sigma away we plot the guide lines. If NSIGMA is negative
*       we dont plot the sigma ranges on the plot.
*     IPOS(NX * NY) = INTEGER (Given)
*       I coordinate for each pixel
*     JPOS(NX * NY) = INTEGER (Given)
*       J coordinate for each pixe
*     STATS(NX, NY, 3) = REAL (Given)
*       Statistics for each bin. 1=Median, 2=high, 3=low
*     BINS(NX, NY, NMAX) = REAL (Given)
*       The data stored in relation to its position
*     PNTS(NMAX) = REAL (Given)
*       Scratch space for copying in the data from each I,J
*     POS(NMAX) = REAL (Given)
*       Scratch space for storing the X positions for each marker on the plot
*     STATUS = INTEGER (Given & Returned)
*       Global Status

*  ADAM Parameters:
*     XRANGE = INTEGER (Given)
*       X Range of plot

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)

*  Notes:

*  History:
*     Original version: Timj, 1997 Oct 21
*     $Log$
*     Revision 1.2  1997/11/12 00:09:14  timj
*     Pass the line parameters (statistics) into subroutine.
*
*     Revision 1.1  1997/11/10 19:42:17  timj
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

*  Arguments Given:
      INTEGER UNIT
      INTEGER NX
      INTEGER NY
      INTEGER NMAX
      REAL    NSIGMA
      REAL    BINS(NX, NY, NMAX)
      REAL    PNTS(NMAX)
      REAL    POS (NMAX)
      INTEGER IPOS(NX * NY)
      INTEGER JPOS(NX * NY)
      REAL    STATS(NX, NY, 3)

*  Arguments Given & Returned:

*  Arguments Returned:

*     Status
      INTEGER STATUS

*     Local Variables:
      INTEGER AXIS            ! PGPLOT axis type
      INTEGER COUNT           ! Loop counter
      REAL    DMAX            ! Maximum of data
      REAL    DMIN            ! Minimum of data
      INTEGER I               ! Loop variable
      INTEGER IERR            ! For VEC_
      INTEGER J               ! J coordinate
      INTEGER MED_X_PTR       ! Position of medians
      INTEGER MED_X_END       ! End of MED_X_PTR
      INTEGER MED_PTR         ! Medians for each bin
      INTEGER MED_END         ! End of MED_PTR
      INTEGER N               ! Z coordinate
      INTEGER NERR            ! For VEC_
      INTEGER NGOOD           ! Number of good points
      INTEGER N_MEDIANS       ! Number of medians calculated
      INTEGER N_SIGS          ! Number of standard deviations calculated
      REAL    RANGE           ! Y extent
      INTEGER STEP            ! Step size for loop
      INTEGER STDEVM_PTR      ! Mean - stdev
      INTEGER STDEVM_END      ! End
      INTEGER STDEVP_PTR      ! Mean + stdev
      INTEGER STDEVP_END      ! End
      INTEGER SIG_X_PTR       ! Position of points in STDEVM/P_PTR
      INTEGER SIG_X_END       ! End
      INTEGER SYMBOL          ! PGPLOT symbol type
      REAL    XPOS            ! X position in plot
      INTEGER XRANGE( 2 )     ! Range of data to plot

*.
      IF (STATUS .NE. SAI__OK) RETURN

*     Initialise pointers
      MED_X_PTR = 0
      MED_X_END = 0
      MED_PTR = 0 
      MED_END = 0
      STDEVM_PTR = 0
      STDEVM_END = 0
      STDEVP_PTR = 0
      STDEVP_END = 0
      SIG_X_PTR = 0
      SIG_X_END = 0

*     Get the X plotting range
      XRANGE(1) = 1
      XRANGE(2) = NX*NY

*     Add the ability to keep in a loop until we get a null parameter
*     value from XRANGE

      DO WHILE (STATUS .EQ. SAI__OK)
            
         CALL PAR_GDR1I('XRANGE', 2, XRANGE, 1, NX*NY,
     :        .FALSE., XRANGE, STATUS)
         
         CALL PAR_CANCL('XRANGE', STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

*     Need to check that XRANGE is in the correct order
*     Just need to change the step to -1 
*     Alternatively I could swap round the range
            IF (XRANGE(1).GT. XRANGE(2)) THEN
                  
               STEP = -1
               
            ELSE

               STEP = 1

            END IF

*     Find the range of the data for plotting
         
            DMIN = VAL__MAXR
            DMAX = VAL__MINR

            DO COUNT = XRANGE(1), XRANGE(2), STEP

               I = IPOS(COUNT)
               J = JPOS(COUNT)
               
               DO N = 1, NMAX
                  
                  IF (BINS(I,J,N) .NE. VAL__BADR) THEN
                     DMIN = MIN(DMIN, BINS(I,J,N))
                     DMAX = MAX(DMAX, BINS(I,J,N))
                  END IF
                  
               END DO   
            END DO

*     Add a small bit to the range so that we can see all the points

            RANGE = DMAX - DMIN
            DMAX = DMAX + (0.05 * RANGE)
            DMIN = DMIN - (0.05 * RANGE)

*     Get some memory for the line drawing routine
            CALL SCULIB_MALLOC(NX*NY * VAL__NBR, MED_X_PTR, 
     :           MED_X_END, STATUS)
            CALL SCULIB_MALLOC(NX*NY * VAL__NBR, MED_PTR, MED_END,
     :           STATUS)
            CALL SCULIB_MALLOC(NX*NY * VAL__NBR, STDEVM_PTR, 
     :           STDEVM_END, STATUS)
            CALL SCULIB_MALLOC(NX*NY * VAL__NBR, STDEVP_PTR, 
     :           STDEVP_END, STATUS)
            CALL SCULIB_MALLOC(NX*NY * VAL__NBR, SIG_X_PTR, 
     :           SIG_X_END, STATUS)
        

*     Perform graphics operations on the device
*     Setup the axes

            AXIS = 0
            CALL PGSCI(1)
            
            CALL PGENV(REAL(XRANGE(1)), REAL(XRANGE(2)), DMIN, DMAX,
     :           0, AXIS)
            
            CALL PGSCI(3)
            
*     Choose a symbol
            SYMBOL = -1

*     Counters
            N_SIGS = 0
            N_MEDIANS = 0

*     Loop over each position in the grid

            DO COUNT = XRANGE(1), XRANGE(2), STEP

               I = IPOS(COUNT)
               J = JPOS(COUNT)

*     Copy the good data from this I,J to the work array 

               NGOOD = 0
               XPOS = REAL(COUNT)
               
               DO N = 1, NMAX
               
                  IF (BINS(I,J,N) .NE. VAL__BADR) THEN
                     NGOOD = NGOOD + 1
                     PNTS(NGOOD) = BINS(I,J,N)
                     POS(NGOOD)  = XPOS
                  END IF
                  
               END DO
            
            
               IF (NGOOD .GT. 0) THEN

*     Plot the points using PGPT
                  CALL PGPT(NGOOD, POS, PNTS, SYMBOL)

*     ...and read the statistics associated with this (I,J)
*     Have to use pointers again for the scratch space
                  
*     Store the median and sigma for later plotting
*     D**M pointers!
                  
                  CALL VEC_RTOR(.FALSE., 1, STATS(I,J,1), 
     :                 %VAL(MED_PTR + (N_MEDIANS * VAL__NBR)),
     :                 IERR, NERR, STATUS)
                  
                  CALL VEC_RTOR(.FALSE., 1, XPOS, 
     :                 %VAL(MED_X_PTR + (N_MEDIANS * VAL__NBR)),
     :                 IERR, NERR, STATUS)
                  
                  
                  N_MEDIANS = N_MEDIANS + 1
                  
                  IF (STATS(I,J,2) .NE. VAL__BADR) THEN

                     CALL VEC_RTOR(.FALSE., 1, STATS(I,J,2), 
     :                    %VAL(STDEVP_PTR + (N_SIGS * VAL__NBR)),
     :                    IERR, NERR, STATUS)
                     
                     CALL VEC_RTOR(.FALSE., 1, STATS(I,J,3), 
     :                    %VAL(STDEVM_PTR + (N_SIGS * VAL__NBR)),
     :                    IERR, NERR, STATUS)
                     
                     CALL VEC_RTOR(.FALSE., 1, XPOS, 
     :                    %VAL(SIG_X_PTR + (N_SIGS * VAL__NBR)),
     :                    IERR, NERR, STATUS)
                     
                     N_SIGS = N_SIGS + 1
                  END IF
                  
               END IF
                  
            END DO

*     Now draw on the lines corresponding to median and sigma
*     Have to enclose in an if since PGPLOT can not check status
            
            IF (STATUS .EQ. SAI__OK) THEN
               
               CALL PGSCI(1)
               CALL PGSLS(1)
               
               CALL PGLINE(N_MEDIANS, %VAL(MED_X_PTR), %VAL(MED_PTR))
               
               IF (NSIGMA .GT. 0.0) THEN
                  CALL PGSCI(2)
                  CALL PGLINE(N_SIGS, %VAL(SIG_X_PTR), 
     :                 %VAL(STDEVP_PTR))
                  CALL PGLINE(N_SIGS, %VAL(SIG_X_PTR), 
     :                 %VAL(STDEVM_PTR))
               END IF
            END IF
            
*     Free the memory
            CALL SCULIB_FREE('MED_PTR', MED_PTR, MED_END, STATUS)
            CALL SCULIB_FREE('MED_X_PTR',MED_X_PTR, MED_X_END, 
     :           STATUS)
            CALL SCULIB_FREE('SIG_X_PTR', SIG_X_PTR, SIG_X_END, 
     :           STATUS)
            CALL SCULIB_FREE('STDEVP', STDEVP_PTR, STDEVP_END, 
     :           STATUS)
            CALL SCULIB_FREE('STDEVM', STDEVM_PTR, STDEVM_END, 
     :           STATUS)

         END IF

      END DO

*     Check for null parameter
      IF (STATUS .EQ. PAR__NULL) CALL ERR_ANNUL(STATUS)
         
*     Release Device
      CALL PGP_ANNUL( UNIT, STATUS )

      END
