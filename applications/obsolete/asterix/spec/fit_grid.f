*+  FIT_GRID - Make a grid of the fit statistic and/or free pars vs grid pars
      SUBROUTINE FIT_GRID( NDS, OBDAT, INSTR, MODEL, OPCHAN, NAXIS,
     :             GRID, NGRID, GRIDPAR, NITMAX, NPAR, LB, UB, FROZEN,
     :             SSCALE, MINSLO,
     :             FSTAT, PREDICTOR, PREDDAT, PARAM, STATMIN,
     :             GDATA, GQUAL, GQMASK, STATUS )
*
*    Description :
*
*     FIT_GRID constructs NGRID grids of dimensionality NAXIS containing
*     either the fit statistic or a fit parameter. The former is selected
*     by specifying zero in the appropriate element of GRIDPAR - any fit
*     parameter is specified simply by its parameter number.
*
*     Only one fit grid is constructed. The variation of each grid parameter
*     is controlled by a GRID axis block. After evaluation of the fit
*     statistic at each point in the fit grid, those quantities specified
*     in the GRIDPAR array are transferred to their appropriate arrays. In
*     addition, a quality array is returned indicating success or failure
*     at each grid position. Fatal fitting errors (eg. matrix inversion
*     problems) generate "data missing quality" values. If the fitting
*     simply fails to converge, or a parameter is pegged then the "soft"
*     quality QUAL__IGNORE bit is set.
*
*     The fit statistic is selected using the FSTAT argument and is accessed
*     using the symbolic variables FIT__CHISQ and FIT__LOGL.
*     Parameters frozen on input remain frozen throughout the grid creation
*     process. Grid values are found by freezing all the parameters in the
*     the grid at their axis values. If there are any free parameters then
*     minimisation is performed using FIT_MIN controlled by the arguments
*     NITMAX, SSCALE,MINSLO. In the abscence of any free parameters a simple
*     statistic evaluation at each grid point is performed using FIT_STAT.
*
*     SSCALE is the statistic scale factor : this should be the number of
*     degrees of freedom less the number of unfrozen parameters when FSTAT is
*     FIT__CHISQ, but should be the sum over all the observed data when
*     equal to FIT__LOGL.
*
*     The minimum value of the statistic in the grid is returned in
*     STATMIN and the parameter set which produced it in PARMIN.
*
*     FIT_GRID checks that 0 <= GRIDPAR(i) <= NPAR but does not check that
*     parameters are free, or that they are not members of the grid parameter
*     set.
*
*    Method :
*
*     This routine simply constructs the grid dimensions in 7D, performs
*     some simple checking and passes control to FIT_GRID_INT.
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      1 Jun 92 : Original (DJA)
*     15 Jun 92 : FSTAT added and NDOF changed to SSCALE (DJA)
*     19 Jun 92 : Allows griding of free parameters as well as statistic (DJA)
*     18 Aug 92 : Statistic now double precision (DJA)
*      4 Jan 93 : Quality handling amended (DJA)
*     22 Mar 93 : Percentage progress reported (DJA)
*      8 Jul 93 : Fixed couple of bugs in FIT_GRID_INT. First one updated
*                 non-grid parameters only if fitting error occurred on
*                 previous iteration! (DJA)
*      1 Mar 94 : Updated quality handling for new BIT_ routines (DJA)
*      8 Jun 94 : Use AIO_REWRITE to handle completion reporting (DJA)
*      7 Sep 94 : Allow gridding of fit probability (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Import :
*
      INTEGER             NDS			! Number of observed datasets
      RECORD /DATASET/    OBDAT(NDS)		! Observed datasets
      RECORD /INSTR_RESP/ INSTR(NDS)		! Instrument responses
      RECORD /MODEL_SPEC/ MODEL			! Model specification
      INTEGER 		  OPCHAN		! Output channel for diagnostic
						! messages ( <1 for no messages)
      INTEGER             NAXIS                 ! Number of grid axes
      RECORD /GRID_AXIS/  GRID(*)               ! Grid axis definitions
      INTEGER             NGRID                 ! # of grids
      INTEGER             GRIDPAR(NGRID)        ! Quantity for grids (0->NPAR)
      INTEGER 		  NITMAX		! Return when NIT reaches NITMAX
      INTEGER 		  NPAR			! No of parameters
      REAL 		  LB(NPAMAX)		! Parameter lower bounds
      REAL 		  UB(NPAMAX)		! Parameter upper bounds
      LOGICAL 		  FROZEN(NPAMAX)	! Frozen parameter flag
      INTEGER 		  SSCALE		! Statistic scale factor
      INTEGER             FSTAT                 ! Fit statistic
      EXTERNAL 		  PREDICTOR             ! Model prediction routine
*
*    Import-Export :
*
      REAL 		  MINSLO		! Min scaled slope in statistic
                                                ! forcing continuation
      RECORD /PREDICTION/ PREDDAT(NDS)		! Data predicted by model
                                                ! (actually only the data
                                                ! pointed to are updated)
      REAL 		  PARAM(NPAMAX)		! Model parameters
      INTEGER 		  NIT			! Iteration number
*
*    Export :
*
      DOUBLE PRECISION    STATMIN               ! Minimum value of statistic
      INTEGER             GDATA(NGRID)          ! Ptrs to grid arrays. Only
                                                ! data pointed to is altered
      BYTE                GQUAL(*)              ! Grid of statistic quality
      BYTE                GQMASK                ! Grid quality mask
*
*    Status :
*
      INTEGER 		  STATUS
*
*    Local variables :
*
      INTEGER             DIMS(DAT__MXDIM)      ! Grid dimensions
      INTEGER             IAX                   ! Loop over grid axes
      INTEGER             IGR                   ! Loop over grids

      LOGICAL             USED(DAT__MXDIM)      ! Use dimension of grid?
*-

*    Status check
      IF ( STATUS.NE.SAI__OK ) RETURN

*   Some elimentary checks

*    Enough parameters for grid
      IF ( NAXIS .GT. NPAR ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Number of grid axes exceeds number'/
     :                               /' of parameters', STATUS )

*    Grid isn't too big?
      ELSE IF ( NAXIS .GT. DAT__MXDIM ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETI( 'MAX', DAT__MXDIM )
        CALL ERR_REP( ' ', 'Too many grid axes defined, must be '/
     :                                   /'^MAX or less', STATUS )

*    Check grid choices
      ELSE

*      Check grid quantity is valid and that memory is allocated
        DO IGR = 1, NGRID
          IF ( ((GRIDPAR(IGR).LT.0) .OR. (GRIDPAR(IGR).GT.NPAR)) .AND.
     :         (GRIDPAR(IGR).NE.99) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'N', IGR )
            CALL ERR_REP( ' ', 'Invalid grid parameter number, ^N',
     :                                                     STATUS )
          ELSE IF ( GDATA(IGR) .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'N', IGR )
            CALL ERR_REP( ' ', 'Memory not allocated for grid ^N',
     :                                                    STATUS )
          END IF
        END DO

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 9000

*    Construct grid dimensions and pad to 7D
      DO IAX = 1, NAXIS
        DIMS(IAX) = GRID(IAX).NVAL
        USED(IAX) = .TRUE.
      END DO
      DO IAX = NAXIS + 1, DAT__MXDIM
        DIMS(IAX) = 1
        USED(IAX) = .FALSE.
      END DO

*    Invoke internal routine
      CALL FIT_GRID_INT( NDS, OBDAT, INSTR, MODEL, OPCHAN, NAXIS, GRID,
     :           NGRID, GRIDPAR,
     :           NITMAX, NPAR, LB, UB, FROZEN, SSCALE, MINSLO, FSTAT,
     :           PREDICTOR,
     :           PREDDAT, PARAM, DIMS(1), DIMS(2), DIMS(3), DIMS(4),
     :           DIMS(5), DIMS(6), DIMS(7),
     :           USED, STATMIN, GDATA, GQUAL, GQMASK, STATUS )

*    Exit
 9000 IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'FIT_GRID', STATUS )
      END IF

      END


*+  FIT_GRID_AXVAL - Extract the N'th grid axis value from an axis description
      SUBROUTINE FIT_GRID_AXVAL( AXIS, N, PARAM, STATUS )
*
*    Description :
*
*    Method :
*
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      2 Jun 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Import :
*
      RECORD /GRID_AXIS/  AXIS                  ! The grid axis
      INTEGER             N                     ! Index of value required
*
*    Import / Export :
*
      REAL 		  PARAM                 ! Parameter value
*
*    Status :
*
      INTEGER 		  STATUS
*-

*    Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Select on axis type
      IF ( AXIS.REGULAR ) THEN
        PARAM = AXIS.BASE + (N-1)*AXIS.SCALE
      ELSE
        CALL ARR_ELEM1R( AXIS.VPTR, AXIS.NVAL, N, PARAM, STATUS )
      END IF
      IF ( AXIS.LOGARITHMIC ) PARAM = 10.0**PARAM

      END


*+  FIT_GRID_INT - Produce a grid of the fit statistic vs parameters
      SUBROUTINE FIT_GRID_INT( NDS, OBDAT, INSTR, MODEL, OPCHAN, NAXIS,
     :             GRID, NGRID, GRIDPAR, NITMAX, NPAR, LB, UB, FROZEN,
     :             SSCALE, MINSLO,
     :             FSTAT, PREDICTOR, PREDDAT, PARAM,
     :             L1, L2, L3, L4, L5, L6, L7, USED,
     :             STATMIN, GDATA, GQUAL, GQMASK, STATUS )
*
*    Description :
*
*     Constructs a grid of the fit statistic as a function of NAXIS
*     fit parameters. The variation of each such parameter is stored
*     in a GRID block. The results are stored in the GDATA array
*     which has NAXIS dimensions.
*
*     Parameters frozen on input remain frozen throughout the grid creation
*     process. Grid values are found by freezing all the parameters in the
*     the grid at their axis values. If there are any remaining free parameters
*     then minimisation is performed using FIT_MIN controlled by the arguments
*     NITMAX, SSCALE,MINSLO. In the absence of any free parameters a simple
*     statistic evaluation at each grid point is performed.
*
*     The minimum value of the statistic in the grid is returned in
*     STATMIN and the parameter set which produced it in PARAM.
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      1 Jun 92 : Original (DJA)
*     15 Jun 92 : FSTAT added and NDOF changed to SSCALE (DJA)
*     19 Jun 92 : Allows griding of free parameters as well as statistic (DJA)
*     18 Aug 92 : Statistic now double precision (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Import :
*
      INTEGER             NDS			! Number of observed datasets
      RECORD /DATASET/    OBDAT(NDS)		! Observed datasets
      RECORD /INSTR_RESP/ INSTR(NDS)		! Instrument responses
      RECORD /MODEL_SPEC/ MODEL			! Model specification
      INTEGER 		  OPCHAN		! Output channel for diagnostic
						! messages ( <1 for no messages)
      INTEGER             NAXIS                 ! Number of grid axes
      RECORD /GRID_AXIS/  GRID(*)               ! Grid axis definitions
      INTEGER             NGRID                 ! # of grids
      INTEGER             GRIDPAR(NGRID)        ! Quantity for grids (0->NPAR)
      INTEGER 		  NITMAX		! Return when NIT reaches NITMAX
      INTEGER 		  NPAR			! No of parameters
      REAL 		  LB(NPAMAX)		! Parameter lower bounds
      REAL 		  UB(NPAMAX)		! Parameter upper bounds
      LOGICAL 		  FROZEN(NPAMAX)	! Frozen parameter flag
      INTEGER 		  SSCALE		! Statistic scale factor
      INTEGER             FSTAT                 ! Fit statistic
      EXTERNAL 		  PREDICTOR             ! Model prediction routine
      INTEGER             L1,L2,L3,L4,L5,L6,L7  ! Grid dimensions
      LOGICAL             USED(DAT__MXDIM)      ! Grid dimension used?
*
*    Import-Export :
*
      REAL 		  MINSLO		! Min scaled slope in statistic
                                                ! forcing continuation
      RECORD /PREDICTION/ PREDDAT(NDS)		! Data predicted by model
                                                ! (actually only the data
                                                ! pointed to are updated)
      REAL 		  PARAM(NPAMAX)		! Model parameters. On output
                                                ! contain minimum grid
*
*    Export :
*
      DOUBLE PRECISION    STATMIN               ! Minimum value of statistic
      INTEGER             GDATA(NGRID)          ! Ptrs to grid arrays. Only
                                                ! data pointed to is altered
      BYTE                GQUAL(L1,L2,L3,L4,    ! Grid of statistic quality
     :                             L5,L6,L7)
      BYTE                GQMASK                ! Grid quality mask
*
*    Status :
*
      INTEGER 		  STATUS
*
*    Functions :
*
      BYTE		  BIT_ANDUB
      BYTE		  BIT_NOTUB
*
*    Local constants :
*
      REAL                BIG_STAT_VAL
        PARAMETER         ( BIG_STAT_VAL = 1.0E30 )
*
*    Local variables :
*
      CHARACTER*5	  OPSTR			! Output buffer

      DOUBLE PRECISION	  PROB			! Fit probability
      DOUBLE PRECISION    STAT                  ! Value of statistic at point

      REAL 		  DPAR(NPAMAX)		! Param increments for differencing
      REAL                LMINSLO               ! Local slope after minimisation
      REAL 		  LOCPAR(NPAMAX)	! Local parameter set

      INTEGER 		  FITERR		! Fitting error encountered
      INTEGER             G1,G2,G3,G4,G5,G6,G7  ! Grid indices
      INTEGER             GG1                   ! Grid index for 1st dimension
      INTEGER             GI(DAT__MXDIM)        ! Grid indices
      INTEGER 		  I  			! Parameter index
      INTEGER             IGR                   ! Loop over grids
      INTEGER             NDONE                 ! # of points done
      INTEGER 		  NIT			! Iteration number
      INTEGER             NTOT                  ! # of points in grid
      INTEGER 		  NUNFROZEN		! No of unfrozen parameters
      INTEGER             NUPAR                 ! # useful non-grid parameters
      INTEGER             UPAR(NPAMAX)          ! Useful non-grid parameters

      BYTE                LQUAL                 ! Local quality value
      BYTE                MASK                  ! Quality mask BAD & !IGNORE

      LOGICAL 		  FINISHED		! Minimum found?
      LOGICAL 		  INITIALISE		! Should be set true on first
						! call only - always returned F
      LOGICAL		  LOCFRO(NPAMAX)	! Local parameter freezing
      LOGICAL             ONGRID                ! Parameter on the grid?
      LOGICAL 		  PEGGED(NPAMAX)	! Parameter pegged on bound
*
*    Map grid indices to array :
*
      EQUIVALENCE         (G1,GI(1)),(G2,GI(2)),
     :                    (G3,GI(3)),(G4,GI(4)),
     :                    (G5,GI(5)),(G6,GI(6)),
     :                    (G7,GI(7))
*-

*    Status check
      IF ( STATUS.NE.SAI__OK ) RETURN

*    Quality mask
      MASK = BIT_ANDUB( QUAL__MASK, BIT_NOTUB(QUAL__IGNORE) )

*    All the parameters in the grid are frozen
      DO I = 1, NPAR
        LOCFRO(I) = FROZEN(I)
        LOCPAR(I) = PARAM(I)
      END DO
      DO I = 1, NAXIS
        LOCFRO(GRID(I).PAR) = .TRUE.
      END DO

*    Total number of elements
      NTOT = L1 * L2 * L3 * L4 * L5 * L6 * L7
      NDONE = 0

*    Construct array of those unfrozen parameters not in the grid
      NUPAR = 0
      DO I = 1, NPAR
        IF ( .NOT. LOCFRO(I) ) THEN
          NUPAR = NUPAR + 1
          UPAR(NUPAR) = I
        END IF
      END DO

*    Count unfrozen parameters
      NUNFROZEN = NUPAR

*    Initialise
      STATMIN = BIG_STAT_VAL
      FITERR = SAI__OK

*    Loop over each dimension of the grid
      DO G7 = 1, L7

        DO G6 = 1, L6

          DO G5 = 1, L5

            DO G4 = 1, L4

              DO G3 = 1, L3

                DO G2 = 1, L2

                  DO GG1 = 1, L1

*    Indentation moved 7 levels to the left <-------------------------

*    Set G1 depending on direction of travel
      IF ( (G2/2)*2 .EQ. G2 ) THEN
        G1 = L1 - GG1 + 1
      ELSE
        G1 = GG1
      END IF

*    Retain last good parameter set unless a fitting error occured
      IF ( FITERR .EQ. 0 ) THEN
        DO I = 1, NUPAR
          LOCPAR(UPAR(I)) = PARAM(UPAR(I))
        END DO
      END IF

*    Set up parameter values
      DO I = 1, NAXIS
        CALL FIT_GRID_AXVAL( GRID(I), GI(I), LOCPAR(GRID(I).PAR),
     :                                                   STATUS )
      END DO

*    The default is that the grid point will be filled with 0.0
      LQUAL = QUAL__MISSING

*    If there are no unfrozen parameters then no minimisation is required
      IF ( NUNFROZEN .EQ. 0 ) THEN

*      Just evaluate statistic
        CALL FIT_STAT( NDS, OBDAT, INSTR, MODEL, LOCPAR, FSTAT,
     :                       PREDICTOR, PREDDAT, STAT, STATUS )

*      Ok?
        IF ( STATUS .EQ. SAI__OK ) THEN
          LQUAL = QUAL__GOOD
        ELSE
          CALL ERR_ANNUL( STATUS )
        END IF

      ELSE

*      Initialise fitting
        INITIALISE = .TRUE.
        LMINSLO = MINSLO

*      Minimise with respect to parameters not on the grid
        CALL FIT_MIN( NDS, OBDAT, INSTR, MODEL, 0, NITMAX, NPAR,
     :                LB, UB, LOCFRO, SSCALE, INITIALISE, LMINSLO,
     :                FSTAT, PREDICTOR, PREDDAT, LOCPAR, DPAR, PEGGED,
     :                STAT, NIT, FINISHED, FITERR, STATUS )

*      Severe fitting error?
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )

*      Minimised ok?
        ELSE IF ( (FITERR .EQ. 0) .AND. FINISHED ) THEN
          LQUAL = QUAL__GOOD

*      Fitting error. Set quality to IGNORE which means we fill in the
*      grid value but set the quality mask such that it is hidden from the
*      user by default. All other fitting errors are regarded as hard
*      errors, causing the grid point to be zeroed.
        ELSE IF ( (FITERR.EQ.0) .OR. (FITERR.EQ.4) ) THEN
          LQUAL = QUAL__IGNORE

        END IF

      END IF

*    Good value less than current minimum?
      IF ( (BIT_ANDUB(LQUAL,MASK).EQ.QUAL__GOOD) .AND.
     :                       (STAT.LT.STATMIN) ) THEN

*      Update parameter set if no failure
        DO I = 1, NPAR
          PARAM(I) = LOCPAR(I)
        END DO
        STATMIN = STAT

      END IF

*    Set each grid value. If GRIDPAR(i) = 0 then store the fit statistic,
*    if 99 store the fit probability, otherwise store the GRIDPAR(i)'th
*    parameter value (which usually have been reoptimised)
      DO IGR = 1, NGRID
        IF ( GRIDPAR(IGR) .EQ. 0 ) THEN
          CALL FIT_GRID_SETV( REAL(STAT), LQUAL,
     :                        L1, L2, L3, L4, L5, L6, L7,
     :                        G1, G2, G3, G4, G5, G6, G7,
     :                        %VAL(GDATA(IGR)), GQUAL )

        ELSE IF ( GRIDPAR(IGR) .EQ. 99 ) THEN

*        Calculate fit probability
          CALL FIT_MPROB( NDS, OBDAT, FSTAT, SSCALE, PREDDAT, STAT,
     :                    PROB, STATUS )

*        Store in grid
          CALL FIT_GRID_SETV( REAL(PROB), LQUAL,
     :                        L1, L2, L3, L4, L5, L6, L7,
     :                        G1, G2, G3, G4, G5, G6, G7,
     :                        %VAL(GDATA(IGR)), GQUAL )

        ELSE
          CALL FIT_GRID_SETV( LOCPAR(GRIDPAR(IGR)), LQUAL,
     :                 L1, L2, L3, L4, L5, L6, L7,
     :                 G1, G2, G3, G4, G5, G6, G7,
     :                 %VAL(GDATA(IGR)), GQUAL )
        END IF
      END DO

*    Report percentage completed
      IF ( OPCHAN .GT. 0 ) THEN
        IF ( NDONE .EQ. 0 ) THEN
          CALL AIO_REWRITE( OPCHAN, 'NOCR', 'Progress : ', STATUS )
        END IF
        NDONE = NDONE + 1
        WRITE( OPSTR, '(I3,A)' ) NINT(100.0*REAL(NDONE)/REAL(NTOT)),
     :                            ' %'
        CALL AIO_REWRITE( OPCHAN, 'BACKSPACE', OPSTR, STATUS )
      END IF

*    Indentation moved 7 levels to the left <-------------------------

                  END DO

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

*    Terminate rewrite
      IF ( OPCHAN .GT. 0 ) THEN
        CALL AIO_REWRITE( OPCHAN, 'TERMINATE', 'DONE ', STATUS )
      END IF

*    No good value of statistic in grid
      IF ( STATMIN .EQ. BIG_STAT_VAL ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'No point in grid space produced good'/
     :                        /' value of fit statistic', STATUS )
      END IF

*    Set the grid quality mask
      GQMASK = MASK

      END


*+  FIT_GRID_SETV - Set a grid value in a grid array
      SUBROUTINE FIT_GRID_SETV( IN, INQ, L1, L2, L3, L4, L5, L6, L7,
     :                      G1, G2, G3, G4, G5, G6, G7, GRID, QUAL )
*
*    Description :
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     19 Jun 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'QUAL_PAR'
*
*    Import :
*
      REAL                IN                    ! Grid value
      BYTE                INQ                   ! Grid quality value
      INTEGER             L1,L2,L3,L4,L5,L6,L7  ! Grid dimensions
      INTEGER             G1,G2,G3,G4,G5,G6,G7  ! Grid indices
*
*    Import / Export :
*
      REAL                GRID(L1,L2,L3,L4,     ! The output grid
     :                            L5,L6,L7)
      BYTE                QUAL(L1,L2,L3,L4,     ! The output grid quality
     :                            L5,L6,L7)
*-

*    Quality ok?
      IF ( INQ .NE. QUAL__MISSING ) THEN
        GRID(G1,G2,G3,G4,G5,G6,G7) = IN
      ELSE
        GRID(G1,G2,G3,G4,G5,G6,G7) = 0.0
      END IF
      QUAL(G1,G2,G3,G4,G5,G6,G7) = INQ

      END
