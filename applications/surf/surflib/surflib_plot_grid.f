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
*     PNTS(NMAX) = DOUBLE PRECISION (Given)
*       Scratch space for copying in the data from each I,J.
*       DOUBLE PRECISION since that is what AST uses for plotting.
*     POS(NMAX) = DOUBLE PRECISION (Given)
*       Scratch space for storing the X positions for each marker on the plot.
*       DOUBLE PRECISION since that is what AST uses for plotting.
*     STATUS = INTEGER (Given & Returned)
*       Global Status

*  ADAM Parameters:
*     XRANGE = INTEGER (Given)
*       X Range of plot

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)

*  Notes:


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     Original version: Timj, 1997 Oct 21
*     $Log$
*     Revision 1.7  2005/03/18 19:32:45  timj
*     Replace plotting directly with PGPLOT to plotting using KAPLIBS and AST.
*
*     Revision 1.6  2004/09/01 01:02:03  timj
*     use CNF_PVAL
*
*     Revision 1.5  1999/08/03 19:32:51  timj
*     Add copyright message to header.
*
*     Revision 1.4  1998/06/03 22:00:56  timj
*     Protect against all points being bad.
*
*     Revision 1.3  1997/11/27 23:11:10  timj
*     Protect against all points being bad.
*
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
      INCLUDE 'MSG_PAR'                          ! For MSG
      INCLUDE 'CNF_PAR'                          ! For CNF_PVAL function
      INCLUDE 'AST_PAR'                          ! AST constants

*  Arguments Given:
      INTEGER UNIT
      INTEGER NX
      INTEGER NY
      INTEGER NMAX
      REAL    NSIGMA
      REAL    BINS(NX, NY, NMAX)
      DOUBLE PRECISION PNTS(NMAX)
      DOUBLE PRECISION POS (NMAX)
      INTEGER IPOS(NX * NY)
      INTEGER JPOS(NX * NY)
      REAL    STATS(NX, NY, 3)

*  Arguments Given & Returned:

*  Arguments Returned:

*     Status
      INTEGER STATUS

*     Local Variables:
      LOGICAL ALIGN           ! DATA pic. aligned with a previous picture?
      DOUBLE PRECISION ATTRS( 20 ) ! Saved graphics attributes
      INTEGER AXMAPS(2)       ! Mapping from data frame to GRAPHICS frame
      DOUBLE PRECISION BOX(4) ! Data coordinates of the plot corners
      INTEGER COUNT           ! Loop counter
      REAL    DMAX            ! Maximum of data
      REAL    DMIN            ! Minimum of data
      LOGICAL FOUND           ! TRUE if there is at least one good point
      REAL    HIGH            ! Upper end of Xrange for plot
      INTEGER I               ! Loop variable
      INTEGER IERR            ! For VEC_
      INTEGER IPICD           ! AGI identifier for the DATA picture
      INTEGER IPICF           ! AGI identifier for the frame picture
      INTEGER IPLOT           ! Pointer to AST Plot for DATA picture
      INTEGER J               ! J coordinate
      REAL    LOW             ! Lower end of x for plot
      REAL    MARGIN(4)       ! Margin around the plot as fraction of plot
      INTEGER MED_X_PTR       ! Position of medians
      INTEGER MED_X_END       ! End of MED_X_PTR
      INTEGER MED_PTR         ! Medians for each bin
      INTEGER MED_END         ! End of MED_PTR
      INTEGER N               ! Z coordinate
      INTEGER NERR            ! For VEC_
      INTEGER NFRM            ! Frame index increment between IWCS and IPLOT
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
            FOUND = .FALSE.

            DO COUNT = XRANGE(1), XRANGE(2), STEP

               I = IPOS(COUNT)
               J = JPOS(COUNT)

               DO N = 1, NMAX

                  IF (BINS(I,J,N) .NE. VAL__BADR) THEN
                     DMIN = MIN(DMIN, BINS(I,J,N))
                     DMAX = MAX(DMAX, BINS(I,J,N))
                     FOUND = .TRUE.
                  END IF

               END DO
            END DO

*     Check that we have actually got some data to plot (ie not simply
*     a whole set of bad values

            IF (FOUND) THEN

*     Add a small bit to the range so that we can see all the points

               RANGE = DMAX - DMIN
               DMAX = DMAX + (0.05 * RANGE)
               DMIN = DMIN - (0.05 * RANGE)

*     Get some memory for the line drawing routine
               CALL SCULIB_MALLOC(NX*NY * VAL__NBD, MED_X_PTR,
     :              MED_X_END, STATUS)
               CALL SCULIB_MALLOC(NX*NY * VAL__NBD, MED_PTR, MED_END,
     :              STATUS)
               CALL SCULIB_MALLOC(NX*NY * VAL__NBD, STDEVM_PTR,
     :              STDEVM_END, STATUS)
               CALL SCULIB_MALLOC(NX*NY * VAL__NBD, STDEVP_PTR,
     :              STDEVP_END, STATUS)
               CALL SCULIB_MALLOC(NX*NY * VAL__NBD, SIG_X_PTR,
     :              SIG_X_END, STATUS)


*     Add a small bit on either end of the plot range
*     This also allows me to deal with LOW=HIGH plotting problems
               LOW = REAL(XRANGE(1))
               HIGH = REAL(XRANGE(2))

               IF (LOW .LE. HIGH) THEN
                  LOW = LOW - 0.5
                  HIGH = HIGH + 0.5
               ELSE
                  HIGH = HIGH - 0.5
                  LOW = LOW + 0.5
               END IF

*     Open the device for this plot and setup the margin, axes and labels
               MARGIN(1) = 0.15
               MARGIN(2) = 0.15
               MARGIN(3) = 0.15
               MARGIN(4) = 0.15

*     Use kaplibs to configure the AST plot device
               BOX(1) = DBLE(LOW)
               BOX(2) = DBLE(DMIN)
               BOX(3) = DBLE(HIGH)
               BOX(4) = DBLE(DMAX)
               CALL KPG1_PLOT( AST__NULL, 'NEW','SURF_DESPIKE',
     :              ' ', MARGIN, 0, ' ', ' ', 0.0, 0,' ',
     :              BOX, IPICD, IPICF, 0, IPLOT, NFRM, ALIGN,
     :              STATUS )

*     Set the plot attributes
               CALL AST_SETC( IPLOT, 'TITLE',' ', STATUS)
               CALL AST_SETC( IPLOT, 'LABEL(1)','Bin Number', STATUS)
               CALL AST_SETC( IPLOT, 'LABEL(2)','Data Value', STATUS)

*     Plot the grid
               CALL KPG1_ASGRD( IPLOT, IPICF, .TRUE., STATUS )

*     Get the mapping from current to BASE GRAPHICS
               CALL KPG1_ASSPL( IPLOT, 2, AXMAPS, STATUS )

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
                        PNTS(NGOOD) = DBLE(BINS(I,J,N))
                        POS(NGOOD)  = DBLE(XPOS)
                     END IF

                  END DO


                  IF (NGOOD .GT. 0) THEN

*     Transform the data coordinates to GRAPHICS coordinates required by KPG1_PLTLN
                     CALL AST_TRAN2( IPLOT, NGOOD, POS, PNTS, .FALSE.,
     :                    POS, PNTS, STATUS )

*     Specify a default color
                     CALL AST_SETI( IPLOT, 'COLOUR(MARK)', 3, STATUS )

*     Set the appearance of marks drawn using PGPLOT so that they mimic
*     curves produced using astMark.
                     CALL KPG1_PGSTY( IPLOT, 'MARKERS', .TRUE., ATTRS,
     :                    STATUS )

*     Plot the points using AST
                     CALL KPG1_PLTLN( NGOOD, 1, NGOOD,
     :                    POS, PNTS,
     :                    .FALSE., .FALSE.,
     :                    0.0D0, 0.0D0, 0.0D0, 'STYLE', IPLOT, 3,
     :                    SYMBOL, 0, 0,'SURF_DESPIKE', STATUS)

*     Reset the previous PGPLOT attributes
                     CALL KPG1_PGSTY( IPLOT, 'MARKERS', .FALSE., ATTRS,
     :                    STATUS)

*     ...and read the statistics associated with this (I,J)
*     Have to use pointers again for the scratch space

*     Store the median and sigma for later plotting
*     D**M pointers!

                     CALL VEC_RTOD(.TRUE., 1, STATS(I,J,1),
     :   %VAL(CNF_PVAL(MED_PTR) + (N_MEDIANS * VAL__NBD)),
     :                    IERR, NERR, STATUS)

                     CALL VEC_RTOD(.TRUE., 1, XPOS,
     :   %VAL(CNF_PVAL(MED_X_PTR) + (N_MEDIANS * VAL__NBD)),
     :                    IERR, NERR, STATUS)


                     N_MEDIANS = N_MEDIANS + 1

                     IF (STATS(I,J,2) .NE. VAL__BADR) THEN

                        CALL VEC_RTOD(.TRUE., 1, STATS(I,J,2),
     :   %VAL(CNF_PVAL(STDEVP_PTR) + (N_SIGS * VAL__NBD)),
     :                       IERR, NERR, STATUS)

                        CALL VEC_RTOD(.TRUE., 1, STATS(I,J,3),
     :   %VAL(CNF_PVAL(STDEVM_PTR) + (N_SIGS * VAL__NBD)),
     :                       IERR, NERR, STATUS)

                        CALL VEC_RTOD(.TRUE., 1, XPOS,
     :   %VAL(CNF_PVAL(SIG_X_PTR) + (N_SIGS * VAL__NBD)),
     :                       IERR, NERR, STATUS)

                       N_SIGS = N_SIGS + 1
                     END IF

                  END IF

               END DO

*     Now draw on the lines corresponding to median and sigma
               IF (N_MEDIANS .GT. 0) THEN

*     Transform the data coordinates to GRAPHICS coordinates required by KPG1_PLTLN
                  CALL AST_TRAN2( IPLOT, N_MEDIANS,
     :                 %VAL(CNF_PVAL(MED_X_PTR)),
     :                 %VAL(CNF_PVAL(MED_PTR)), .FALSE.,
     :                 %VAL(CNF_PVAL(MED_X_PTR)),
     :                 %VAL(CNF_PVAL(MED_PTR)), STATUS )


*     Specify a default color
                  CALL AST_SETI( IPLOT, 'COLOUR(CURVES)', 1, STATUS )

*     Set the appearance of lines drawn using PGPLOT so that they mimic
*     curves produced using astCurves.
                  CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTRS,
     :                    STATUS )

*     Draw the lines using AST
                  CALL KPG1_PLTLN( N_MEDIANS, 1, N_MEDIANS,
     :                 %VAL(CNF_PVAL(MED_X_PTR)),
     :                 %VAL(CNF_PVAL(MED_PTR)),
     :                 .FALSE., .FALSE.,
     :                 0.0D0, 0.0D0, 0.0D0, 'STYLE', IPLOT, 2,
     :                 0, 0, 0, 'SURF_DESPIKE', STATUS)

*     Reset the previous PGPLOT attributes
                  CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTRS,
     :                 STATUS)

*     The error envelope
                  IF (NSIGMA .GT. 0.0) THEN

*     The + SIGMA line

*     Transform the data coordinates to GRAPHICS coordinates required by KPG1_PLTLN
                     CALL AST_TRAN2( IPLOT, N_SIGS,
     :                    %VAL(CNF_PVAL(SIG_X_PTR)),
     :                    %VAL(CNF_PVAL(STDEVP_PTR)), .FALSE.,
     :                    %VAL(CNF_PVAL(SIG_X_PTR)),
     :                    %VAL(CNF_PVAL(STDEVP_PTR)), STATUS )

*     Specify a default color
                     CALL AST_SETI( IPLOT, 'COLOUR(CURVES)', 2, STATUS )

*     Set the appearance of lines drawn using PGPLOT so that they mimic
*     curves produced using astCurves.
                     CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTRS,
     :                    STATUS )

*     Draw the lines using AST
                  CALL KPG1_PLTLN( N_SIGS, 1, N_SIGS,
     :                 %VAL(CNF_PVAL(SIG_X_PTR)),
     :                 %VAL(CNF_PVAL(STDEVP_PTR)),
     :                 .FALSE., .FALSE.,
     :                 0.0D0, 0.0D0, 0.0D0, 'STYLE', IPLOT, 2,
     :                 0, 0, 0, 'SURF_DESPIKE', STATUS)

*     The - SIGMA line

*     First need to transform the X coordinates back to DATA from the previous
*     transformation (or allocate more memory)
                     CALL AST_TRAN2( IPLOT, N_SIGS,
     :                    %VAL(CNF_PVAL(SIG_X_PTR)),
     :                    %VAL(CNF_PVAL(STDEVP_PTR)), .TRUE.,
     :                    %VAL(CNF_PVAL(SIG_X_PTR)),
     :                    %VAL(CNF_PVAL(STDEVP_PTR)), STATUS )

*     Transform the data coordinates to GRAPHICS coordinates required by KPG1_PLTLN
                     CALL AST_TRAN2( IPLOT, N_SIGS,
     :                    %VAL(CNF_PVAL(SIG_X_PTR)),
     :                    %VAL(CNF_PVAL(STDEVM_PTR)), .FALSE.,
     :                    %VAL(CNF_PVAL(SIG_X_PTR)),
     :                    %VAL(CNF_PVAL(STDEVM_PTR)), STATUS )


*     Draw the lines using AST
                     CALL KPG1_PLTLN( N_SIGS, 1, N_SIGS,
     :                    %VAL(CNF_PVAL(SIG_X_PTR)),
     :                    %VAL(CNF_PVAL(STDEVM_PTR)),
     :                    .FALSE., .FALSE.,
     :                    0.0D0, 0.0D0, 0.0D0, 'STYLE', IPLOT, 2,
     :                    0, 0, 0, 'SURF_DESPIKE', STATUS)

*     Reset the previous PGPLOT attributes
                     CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTRS,
     :                    STATUS)

                  END IF

               END IF
C               END IF

*     Free the memory
               CALL SCULIB_FREE('MED_PTR', MED_PTR, MED_END, STATUS)
               CALL SCULIB_FREE('MED_X_PTR',MED_X_PTR, MED_X_END,
     :              STATUS)
               CALL SCULIB_FREE('SIG_X_PTR', SIG_X_PTR, SIG_X_END,
     :              STATUS)
               CALL SCULIB_FREE('STDEVP', STDEVP_PTR, STDEVP_END,
     :              STATUS)
               CALL SCULIB_FREE('STDEVM', STDEVM_PTR, STDEVM_END,
     :              STATUS)

*     Close the plot
               CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

            ELSE

               CALL MSG_OUTIF(MSG__QUIET,' ','PLOT_GRID: No good '//
     :              'points in selected range', STATUS)

            END IF

         END IF

      END DO

*     Check for null parameter
      IF (STATUS .EQ. PAR__NULL) CALL ERR_ANNUL(STATUS)

      END
