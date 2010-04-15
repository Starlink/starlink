      SUBROUTINE SCULIB_SPLINE_REGRID( METHOD, SFACTOR, MAX_MAPS,
     :     N_MAPS, N_BOLS, MAX_INTS, N_INTS, EFF_RADIUS,
     :     PXSIZE, NX_OUT, NY_OUT, I_CENTRE, J_CENTRE, WEIGHT,
     :     INT_LIST, DATA_PTR, VAR_PTR, XPOS_PTR, YPOS_PTR, OUT_DATA,
     :     OUT_VARIANCE, OUT_QUALITY, OUT_WEIGHT, STATUS )
*+
*  Name:
*     SCULIB_SPLINE_REGRID

*  Purpose:
*     Regrid supplied data onto a rectangular grid using a spline


*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_SPLINE_REGRID( METHOD, SFACTOR, MAX_MAPS, N_MAPS,
*    :     N_BOLS, N_INTS, EFF_RADIUS, PXSIZE, NX_OUT, NY_OUT,
*    :     I_CENTRE, J_CENTRE, WEIGHT, INT_PTR, DATA_PTR, VAR_PTR,
*    :     XPOS_PTR,YPOS_PTR, OUT_DATA, OUT_VARIANCE,
*    :     OUT_QUALITY, OUT_WEIGHT, STATUS )

*  Description:
*     This routine takes data with a variance array and x y positions
*     and regrids it onto a rectangular grid using a spline interpolation
*     algorithm (from PDA). Each integration is regridded separately and
*     added into a mean output image.

*  Arguments:
*     METHOD = CHARACTER (Given)
*        Spline method to use.
*     SFACTOR = REAL (Given)
*        Smoothing factor to be used for PDA_SURFIT
*     MAX_MAPS = INTEGER (Given)
*        Max allowed number of input maps (eg FILES in rebin)
*     N_MAPS = INTEGER (Given)
*        Number of input maps
*     N_BOLS( N_MAPS ) = INTEGER (Given)
*        Number of bolometers in each map
*     MAX_INTS = INTEGER (Given)
*        Maximum number of separate integrations
*     N_INTS( N_MAPS ) = INTEGER (Given)
*        Number of integrations in each input map
*     EFF_RADIUS = REAL (Given)
*        Radius of effective footprint of influence due to each
*        input data point. The resulting spline will have no good
*        data points at a distance greater than this from the
*        the original data. Units are the same as for PIXSPACE.
*     PXSIZE = REAL (Given)
*        Pixel size of output map (radians)
*     NX_OUT = INTEGER (Given)
*        Number of X pixels in output map
*     NY_OUT = INTEGER (Given)
*        Number of Y pixels in output map
*     I_CENTRE = INTEGER (Given)
*        Reference pixel in X
*     J_CENTRE = INTEGER (Given)
*        Reference pixel in Y
*     WEIGHT( N_MAPS ) = REAL (Given)
*        Relative weight of each input map
*     INT_LIST( MAX_MAPS, MAX_INTS + 1 ) = INTEGER (Given)
*        Position of each integration in data (cf DEM_PNTR for integrations)
*     DATA_PTR( N_MAPS ) = INTEGER (Given)
*        Array of pointers to input data
*     VAR_PTR( N_MAPS ) = INTEGER (Given)
*        Array of pointers to input data variance
*     XPOS_PTR( N_MAPS ) = INTEGER (Given)
*        Array of pointers to input data X positions
*     YPOS_PTR( N_MAPS ) = INTEGER (Given)
*        Array of pointers to input data Y positions
*     OUT_DATA( NX_OUT, NY_OUT ) = REAL (Returned)
*        Regular gridded data
*     OUT_VARIANCE( NX_OUT, NY_OUT ) = REAL (Returned)
*        Variance of output data
*     OUT_DATA( NX_OUT, NY_OUT ) = BYTE (Returned)
*        Quality of output map
*     OUT_WEIGHT( NX_OUT, NY_OUT) = REAL (Returned)
*        Total weight of each pixel
*     STATUS = _INTEGER (Given & Returned)
*        Global status

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL:  John Lightfoot (RoE)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1997 April 4 (TIMJ)
*        Extract from main tasks

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER N_MAPS
      INTEGER MAX_INTS
      INTEGER MAX_MAPS
      INTEGER DATA_PTR(N_MAPS)
      INTEGER INT_LIST(MAX_MAPS, MAX_INTS+1)
      INTEGER I_CENTRE
      INTEGER J_CENTRE
      CHARACTER * (*) METHOD
      INTEGER N_BOLS(N_MAPS)
      INTEGER N_INTS(N_MAPS)
      INTEGER NX_OUT
      INTEGER NY_OUT
      REAL    EFF_RADIUS
      REAL    PXSIZE
      REAL    SFACTOR
      INTEGER VAR_PTR(N_MAPS)
      REAL    WEIGHT(N_MAPS)
      INTEGER XPOS_PTR(N_MAPS)
      INTEGER YPOS_PTR(N_MAPS)

*  Arguments Returned:
      REAL    OUT_DATA(NX_OUT, NY_OUT)
      BYTE    OUT_QUALITY(NX_OUT, NY_OUT)
      REAL    OUT_VARIANCE(NX_OUT, NY_OUT)
      REAL    OUT_WEIGHT(NX_OUT, NY_OUT)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      REAL    SCULIB_SECNDS      ! Timer

*  Local Constants:
      INTEGER MIN__NPTS          ! Fewest points usable for regird
      PARAMETER (MIN__NPTS = 20)
      INTEGER MSG_LEV            ! Message output level
      PARAMETER (MSG_LEV = MSG__NORM)
      CHARACTER * 15 TSKNAME     ! Name of subroutine
      PARAMETER (TSKNAME = 'SPLINE_REGRID')

*  Local Variables:
      INTEGER DATA_OFFSET        ! Position in data array
      INTEGER GOOD_DATA_END      ! End of good input data
      INTEGER GOOD_DATA_PTR      ! Good input data
      INTEGER GOOD_VAR_END       ! End of GOOD_VAR_PTR
      INTEGER GOOD_VAR_PTR       ! Good input variance
      INTEGER GOOD_X_END         ! End of GOOD_X
      INTEGER GOOD_X_PTR         ! Good input X positions
      INTEGER GOOD_Y_END         ! End of GOOD_Y
      INTEGER GOOD_Y_PTR         ! Good input Y data
      INTEGER I                  ! Loop counter
      INTEGER IERR               ! Used for VEC_
      INTEGER INTEGRATION        ! Current integration
      INTEGER INT_START          ! Position of integration in data
      INTEGER INT_NEXT           ! Position of int+1
      INTEGER INT_QUALITY_END    ! End of temp quality
      INTEGER INT_QUALITY_PTR    ! quality for each integration
      INTEGER J                  ! Loop counter
      INTEGER NCOADD_END         ! End of NCOADD_PTR
      INTEGER NCOADD_PTR         ! Number of points coadded into average
      INTEGER NERR               ! Used for VEC_
      INTEGER NGOOD              ! Number of good data points in an integration
      INTEGER NMAP               ! Currrent map number
      INTEGER OUT_END            ! End of OUT_PTR
      INTEGER OUT_PTR            ! Output map of Spline
      REAL    T0                 ! Time at start of regrid
      REAL    T1                 ! Time at each stage
      INTEGER TOT_PTS            ! Number of points for an integration
      INTEGER XI_END             ! End of XI
      INTEGER XI_PTR             ! X positions in output data
      INTEGER YI_END             ! End of YI
      INTEGER YI_PTR             ! X*Y Y positions in output data
      INTEGER YIY_END            ! End of YI
      INTEGER YIY_PTR            ! One set of Y positions in output data
*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Initialise pointers
      INT_QUALITY_PTR = 0
      INT_QUALITY_END = 0
      OUT_PTR = 0
      OUT_END = 0
      NCOADD_PTR = 0
      NCOADD_END = 0
      XI_PTR = 0
      XI_END = 0
      YI_PTR = 0
      YI_END = 0
      YIY_PTR = 0
      YIY_END = 0
      GOOD_DATA_PTR = 0
      GOOD_DATA_END = 0
      GOOD_VAR_PTR = 0
      GOOD_VAR_END = 0
      GOOD_X_PTR = 0
      GOOD_X_END = 0
      GOOD_Y_PTR = 0
      GOOD_Y_END = 0

*     Start a timer
      T0 = SCULIB_SECNDS(0.0,STATUS)
      CALL MSG_SETC('PKG', TSKNAME)
      CALL MSG_OUTIF(MSG_LEV, ' ','^PKG: Beginning regrid process',
     :     STATUS)

*     Allocate output storage array. This is used to store each integration
*     before averaging into the output map.

      CALL SCULIB_MALLOC(NX_OUT * NY_OUT * VAL__NBUB, INT_QUALITY_PTR,
     :     INT_QUALITY_END, STATUS)
      CALL SCULIB_MALLOC(VAL__NBR * NX_OUT * NY_OUT, OUT_PTR, OUT_END,
     :     STATUS)
      CALL SCULIB_MALLOC(VAL__NBI * NX_OUT * NY_OUT, NCOADD_PTR,
     :     NCOADD_END, STATUS)

*     Fill NCOADD with 0
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_CFILLI(NX_OUT*NY_OUT, 0,
     :                      %VAL(CNF_PVAL(NCOADD_PTR)))
      END IF

*     The output grid is fixed for each integration (the extent of the entire
*     data is already known) and so can be done outside the loop

*     Allocate memory for the axis coordinates of output grid
      CALL SCULIB_MALLOC(NX_OUT * NY_OUT * VAL__NBR, XI_PTR, XI_END,
     :     STATUS)
      CALL SCULIB_MALLOC(NX_OUT * NY_OUT * VAL__NBR, YI_PTR, YI_END,
     :     STATUS)
      CALL SCULIB_MALLOC(NY_OUT * VAL__NBR, YIY_PTR, YIY_END, STATUS)

*     Set up the axes
*     These are 2 arrays of NX_OUT * NY_OUT containing X and Y positions
*     of each point in the output grid. The first NX_OUT points of the
*     X data can be passed to any routine that uses X whereas a new
*     array has to be created for the Y data if I just want to store Y offsets
*     and not X,Y pairs.

*     IDBVIP needs full array of X and Y positions
*     IDSFFT and SURFIT require just NX_OUT X positions and NY_OUT Y poss.

      DO J = 1,  NY_OUT

         CALL VEC_RTOR(.FALSE., 1, REAL(J - J_CENTRE) * PXSIZE,
     :        %VAL(CNF_PVAL(YIY_PTR) + (J-1) * VAL__NBR), IERR, NERR,
     :        STATUS)

         DO I = 1, NX_OUT

            DATA_OFFSET = (I - 1) + (J -1) * NX_OUT

*       Y axis
            CALL VEC_RTOR(.FALSE., 1, REAL(J - J_CENTRE) * PXSIZE,
     :           %VAL(CNF_PVAL(YI_PTR) + DATA_OFFSET * VAL__NBR),
     :           IERR, NERR,
     :           STATUS)

*       X axis
            CALL VEC_RTOR(.FALSE., 1, REAL(I_CENTRE - I) * PXSIZE,
     :           %VAL(CNF_PVAL(XI_PTR) + DATA_OFFSET * VAL__NBR),
     :           IERR, NERR,
     :           STATUS)

         END DO

      END DO

*     Now we need to loop over each integration and input file in turn
*     averaging into the output grid.

      DO NMAP = 1, N_MAPS
         DO INTEGRATION = 1, N_INTS(NMAP)

*     Find the position of the start of the current integration and
*     the position of the start of the next one

            INT_START = INT_LIST(NMAP, INTEGRATION)
            INT_NEXT  = INT_LIST(NMAP, INTEGRATION + 1)

*     Now calculate the number of data points in this integration.

            TOT_PTS = (INT_NEXT - INT_START) * N_BOLS(NMAP)

*     Check that this number is valid (ie greater than MIN__NPTS)

            IF (TOT_PTS .GE. MIN__NPTS) THEN

*     Allocate workspace for good data

               CALL SCULIB_MALLOC(VAL__NBR * TOT_PTS, GOOD_DATA_PTR,
     :              GOOD_DATA_END, STATUS)
               CALL SCULIB_MALLOC(VAL__NBR * TOT_PTS, GOOD_VAR_PTR,
     :              GOOD_VAR_END, STATUS)
               CALL SCULIB_MALLOC(VAL__NBR * TOT_PTS, GOOD_X_PTR,
     :              GOOD_X_END, STATUS)
               CALL SCULIB_MALLOC(VAL__NBR * TOT_PTS, GOOD_Y_PTR,
     :              GOOD_Y_END, STATUS)

*     Copy all data for this integration to a work array as long as
*     the data are GOOD since the interpolation routine does not
*     understand VAL__BADR.
*     Note that the XY data are no longer DOUBLE on return.

               DATA_OFFSET = (INT_START - 1) * N_BOLS(NMAP)

               CALL SCULIB_COPY_GOOD(TOT_PTS,
     :                               %VAL(CNF_PVAL(DATA_PTR(NMAP)) +
     :              DATA_OFFSET * VAL__NBR),
     :   %VAL(CNF_PVAL(VAR_PTR(NMAP)) + DATA_OFFSET * VAL__NBR),
     :   %VAL(CNF_PVAL(XPOS_PTR(NMAP)) + DATA_OFFSET * VAL__NBD),
     :   %VAL(CNF_PVAL(YPOS_PTR(NMAP)) + DATA_OFFSET * VAL__NBD),NGOOD,
     :              %VAL(CNF_PVAL(GOOD_DATA_PTR)),
     :              %VAL(CNF_PVAL(GOOD_VAR_PTR)),
     :              %VAL(CNF_PVAL(GOOD_X_PTR)),
     :              %VAL(CNF_PVAL(GOOD_Y_PTR)), STATUS)

            ELSE

*     Set NGOOD to zero so that the following logic can take
*     care of deciding whether we wish to proceed with the regrid.
*     This will stop the IF..THEN nesting getting out of hand.
               NGOOD = 0

            END IF

*     If there are too few points left after removing bad we need to
*     stop processing this particular integration

            IF (NGOOD .LT. MIN__NPTS) THEN
               CALL MSG_SETC('TASK', TSKNAME)
               CALL MSG_SETI('NP',NGOOD)
               CALL MSG_SETI('NI',INTEGRATION)
               CALL MSG_SETI('NM',NMAP)
               CALL MSG_OUTIF(MSG__QUIET, ' ', '^TASK: Not enough '//
     :              'data for spline interpolation (^NP points for'//
     :              ' integration ^NI from map ^NM)',
     :              STATUS)
            ELSE

*     Now we need to find out the extent of this input data and
*     generate a bad pixel mask for all the points on the output grid
*     that are too far from an input grid (cf sculib_wtfn_regrid_1.f)

               CALL SCULIB_SPLINE_REGRID_1 ( EFF_RADIUS,
     :              %val(cnf_pval(GOOD_DATA_PTR)),
     :              %VAL(CNF_PVAL(GOOD_X_PTR)),
     :              %VAL(CNF_PVAL(GOOD_Y_PTR)), NGOOD, PXSIZE, NX_OUT,
     :              NY_OUT, I_CENTRE, J_CENTRE,
     :              %VAL(CNF_PVAL(INT_QUALITY_PTR)),
     :              STATUS)

*     Now perform the spline interpolation using your favourite routine

               IF (STATUS .EQ. SAI__OK) THEN

                  IF (METHOD .EQ. 'IDBVIP') THEN

                     CALL SCULIB_SPLINE_PDA_IDBVIP(NGOOD,
     :                    %VAL(CNF_PVAL(GOOD_X_PTR)),
     :                    %VAL(CNF_PVAL(GOOD_Y_PTR)),
     :                    %VAL(CNF_PVAL(GOOD_DATA_PTR)),
     :                    NX_OUT * NY_OUT, %VAL(CNF_PVAL(XI_PTR)),
     :                    %VAL(CNF_PVAL(YI_PTR)),
     :                    %VAL(CNF_PVAL(OUT_PTR)), STATUS)

                  ELSE IF (METHOD .EQ. 'IDSFFT') THEN

                     CALL SCULIB_SPLINE_PDA_IDSFFT(NGOOD,
     :                    %VAL(CNF_PVAL(GOOD_X_PTR)),
     :                    %VAL(CNF_PVAL(GOOD_Y_PTR)),
     :                    %VAL(CNF_PVAL(GOOD_DATA_PTR)), NX_OUT,
     :                    NY_OUT, %VAL(CNF_PVAL(XI_PTR)),
     :                    %VAL(CNF_PVAL(YIY_PTR)),
     :                    %VAL(CNF_PVAL(OUT_PTR)), STATUS)

                  ELSE IF (METHOD .EQ. 'SURFIT') THEN

                     CALL SCULIB_SPLINE_PDA_SURFIT(NGOOD, SFACTOR,
     :                    %VAL(CNF_PVAL(GOOD_X_PTR)),
     :                    %VAL(CNF_PVAL(GOOD_Y_PTR)),
     :                    %VAL(CNF_PVAL(GOOD_DATA_PTR)),
     :                    %VAL(CNF_PVAL(GOOD_VAR_PTR)),
     :                    NX_OUT, NY_OUT, %VAL(CNF_PVAL(XI_PTR)),
     :                    %VAL(CNF_PVAL(YIY_PTR)),
     :                    %VAL(CNF_PVAL(OUT_PTR)), STATUS)

                  END IF

*     Check the return status of the fit (ie fit has failed)
                  IF (STATUS .NE. SAI__OK) THEN
                     CALL MSG_SETC('TASK', TSKNAME)
                     CALL MSG_SETI('NI',INTEGRATION)
                     CALL MSG_SETI('NM',NMAP)
                     CALL ERR_REP(' ', '^TASK: Error whilst '//
     :                    'calculating spline for integration '//
     :                    '^NI from map ^NM', STATUS)
                     CALL ERR_FLUSH(STATUS)

                  ELSE
*     This data must be coadded into the output map
*     using the quality mask for this data and the quality mask of the
*     coadded data [output points are good if any of the points going in to
*     the coadd are good] and scaling by weight of the input data set.

                     CALL SCULIB_COADD_MAPS(NX_OUT * NY_OUT,
     :                    %VAL(CNF_PVAL(OUT_PTR)),
     :                    %VAL(CNF_PVAL(INT_QUALITY_PTR)),
     :                    WEIGHT(NMAP), %VAL(CNF_PVAL(NCOADD_PTR)),
     :                    OUT_DATA, OUT_VARIANCE, OUT_QUALITY,
     :                    OUT_WEIGHT, STATUS)


                  END IF

               END IF

            END IF

*     Tidy up for this loop
            CALL SCULIB_FREE ('SPLINE_GOODDATA', GOOD_DATA_PTR,
     :           GOOD_DATA_END, STATUS)
            CALL SCULIB_FREE ('SPLINE_GOODVAR', GOOD_VAR_PTR,
     :           GOOD_VAR_END, STATUS)
            CALL SCULIB_FREE ('SPLINE_GOODY', GOOD_Y_PTR, GOOD_Y_END,
     :           STATUS)
            CALL SCULIB_FREE ('SPLINE_GOODX', GOOD_X_PTR, GOOD_X_END,
     :           STATUS)

         END DO
      END DO

*     Should divide by the weight here
*     (The equivalent of SCULIB_WTFN_REGRID_3)

*     Finish

      T1 = SCULIB_SECNDS(T0,STATUS)
      CALL MSG_SETR('T1', T1)
      CALL MSG_SETC('PKG', TSKNAME)
      CALL MSG_OUTIF(MSG_LEV, ' ',
     :     '^PKG: Regrid complete. Elapsed time = ^T1 seconds',
     :     STATUS)


*     Tidy up
      CALL SCULIB_FREE ('SPLINE_OUT_DATA', OUT_PTR, OUT_END, STATUS)
      CALL SCULIB_FREE ('SPLINE_OUT_QUALITY', INT_QUALITY_PTR,
     :     INT_QUALITY_END, STATUS)
      CALL SCULIB_FREE ('SPLINE_NCOADD', NCOADD_PTR, NCOADD_END,
     :     STATUS)
      CALL SCULIB_FREE ('SPLINE_X_AXIS', XI_PTR, XI_END, STATUS)
      CALL SCULIB_FREE ('SPLINE_Y_AXIS', YI_PTR, YI_END, STATUS)
      CALL SCULIB_FREE ('SPLINE_YY_AXIS', YIY_PTR, YIY_END, STATUS)

      END
