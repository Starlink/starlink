*+  MATH_INTEGRATE - Uses Rhomberg's method to integrate a function.
      SUBROUTINE MATH_INTEGRATE (MAX_K, MIN_K, CHANGE, START, STOP, Fn,
     :                              WORK1, WORK2, WORK3, ANSWER, STATUS)
*    Description :
*     Integrates the DOUBLE PRECISION FUNCTION Fn(x) from START to STOP using
*     Rhomberg's method.
*
*     The function must be specified in the calling application, and
*     declared as EXTERNAL. It must be able to return the value of the
*     function for ANY value of x in the range START to STOP.
*
*     Any extra variables required by Fn must be passed to it via COMMON blocks.
*
*     The integration is REPEATED with K = K + 1 untill the desired accuracy,
*     expressed as the fractional change in the value of the integral
*     between one value of K and the next, is achieved, or untill K > MAX_K.
*
*     Idealy MAX_K should be chosen so that it is just large enough to give
*     the desired accuracy. MIN_K can then be MAX_K - 1.
*
*     For normal use, set MAX_K to something large (but not too large as there
*     are 2 work arrays of length 1+2^MAX_K), and set MIN_K to 1.
*
*     If MIN_K = MAX_K this routine will complain that it could not test the
*     accuracy of its solution. STATUS is not altered as the answer is
*     'correct' i.e. the best estimate possible with that value of K.
*
*     If the desired accuracy is not reached the application will also complain.
*     Again STATUS is not altered as the best estimate of the integral possible
*     with the given value of MAX_K has been obtained.
*    Method :
*     For a given value of K a series of trapezoidal approximations to the
*     integral using 1, 2, 4, ... 2^K panels are made. These are then combined,
*     useing Rhomberg's method, to eliminate errors to give a far superior
*     estimate of the value of the integral.
*    Authors :
*      Phillip Andrews
*    History :
*     15-MAR-1989 :  Original  (PLA_AST88@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE

      INCLUDE 'SAE_PAR'

*    Status:
      INTEGER             STATUS

*    Import :
      INTEGER             MAX_K             ! Upto 2^max_k trapezoidal panels.
      INTEGER             MIN_K             ! Minimum of 2^MIN_K trap. panels.

      DOUBLE PRECISION    CHANGE            ! Desired fractional change in
                                            ! integral value at end of calculation
      DOUBLE PRECISION    START             ! Start of integration
      DOUBLE PRECISION    STOP              ! End of integration

      DOUBLE PRECISION    Fn                ! Function supplying data values
                                            ! for all values between start &
                                            ! stop
      DOUBLE PRECISION    WORK1(0:MAX_K)    ! Work space (Rhomberg iterations)
      DOUBLE PRECISION    WORK2(0:2**MAX_K) ! Work space (Fn values)

      LOGICAL             WORK3(0:2**MAX_K) ! Work space (get Fn value?)

      EXTERNAL            Fn

*    Export :
      DOUBLE PRECISION    ANSWER            ! value of the integral

*    Local variables :
      Integer             size              ! 2^k
      Integer             k                 ! 2^k panels
      Integer             i,j               ! loop counters
      Integer             step              ! width in pixels of trapezoid
      Integer             next              ! j + step

      Double Precision    diff              ! fractional change in integral
      Double Precision    last              ! last value of integral
      Double Precision    len               ! size - 1
      Double Precision    temp
      Double Precision    temp1
      Double Precision    range             ! data range
      Double Precision    x                 ! ANY value between start & stop
      Double Precision    width             ! Width of current trapezoid

      Logical             first             ! first time round loop?
*-

*    Check status
      If (status .ne. sai__ok) return

*    Initialize
      size = 2**MAX_K

      Do i = 0, size
          work3(i) = .true.

      End do

      k = min_k

      If (k .lt. 1) then
        k = 1

      Else if (k .gt. max_k) then
        k = max_k

      End if

      first = .true.
      diff  = 2.0d0 * change
      range = stop - start
      len   = DBLE (size)

*    Loop over values of k untill integral ok, or k too big
      Do while (diff .gt. change .and. k .le. max_k)
        Do i = 0, k
          work1(i) = 0.0d0
          step     = size / 2**i
          width    = range * dble(step) / len

*        Calculate 'Trapezoidal Rule' integral with this step
          Do j = 0, size - step, step
            If (work3(j)) then
              x         = start + (range * dble(j) / len)
              work2(j)  = Fn(x)
              work3(j)  = .false.

            End if
            next = j + step

            If (work3(next)) then
              x            = start + (range * dble(next) / len)
              work2(next)  = Fn(x)
              work3(next)  = .false.

            End if

*          Area of trapezium (miss out the half)
            work1(i) = work1(i) + ((work2(j) + work2(next)) * width)

          End do
        End do

*      Include the half
        Do i = 0, k
          work1(i) = 0.5d0 * work1(i)

        End do

*      Initialize last if first time around loop.
        If (first) then
          first = .false.
          last  = work1(0)

        End if

*      Now for Rhomberg's bit
        Do i = 1, k
          temp  = 4.0d0**i
          temp1 = temp - 1.0d0

          Do j = 0, k - i
            work1(j) = ((temp * work1(j+1)) - work1(j)) / temp1

          End do
        End do

        diff = ABS ((work1(0) - last) / work1(0))
        last = work1(0)
        k    = k + 1

      End do

      If (min_k .eq. max_k) then
        Call msg_prnt ('WARNING: Unable to check accuracy of integral')

      Else if (diff .gt. change) then
        Call msg_prnt ('WARNING: Integral did not reach desired '//
     :                                                       'accuracy')

      End if

      answer = work1(0)

 999  If (status .ne. sai__ok) then
        Call err_rep ('e', '...from Integrate', status)

      End if
      END
*+  MATH_INTKING2D - Return array of integrated 2D King profile distribution
      SUBROUTINE MATH_INTKING2D( RC, ALPHA, X0, Y0, QX, QY,
     :                           DX, DY, NX, NY, ARRAY, STATUS )
*
*    Description :
*
*     The value of the integrated unit normalised King distribution
*     described by RC,ALPHA (core radius & index) is found for each
*     element of ARRAY.
*
*     The King profile is centred at 0,0. The centre of ARRAY is X0,Y0.
*
*    Method :
*
*     Each pixel of ARRAY is subdivided in X and Y depending on the
*     distance in terms of RC in each axis at that point. The formula used
*     coded in the function SPIX. The sub-pixelling is given by 10
*     times the binsize / sigma, modified by SQRT(offset/rc) to
*     tend to unity for large offsets. This combines high accuracy
*     where the gradient is highest, with fast operation in large
*     areas at large sigma values.
*
*     The spot value of the King profile is found in each sub-pixel, the
*     sum of the sub-pixels contributing to each major (ARRAY) pixel.
*     The normalisation of the King profile in 2 dimensions is
*     PI * RC^2 / (ALPHA-1).
*
*     The case where the array centre coincides with the centre of
*     the profile is handled separately as there is 4-fold symmetry
*     about X and Y axes.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      2 Aug 93 : Original, adapted from MATH_INTGAU2D (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
*
*    Import :
*
      REAL                     RC, ALPHA               ! King profile params
      REAL                     DX, DY, X0, Y0,QX,QY
      INTEGER                  NX,NY
*
*    Export :
*
      REAL                     ARRAY(NX,NY)            ! Gaussian int surface
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      REAL                     NORM                    ! Normalisation constant
      REAL                     RC2                     ! Core radius squared
      REAL                     SDX, SDY                ! Sub-pixel bin sizes
      REAL                     SUM                     ! Cumulative value
      REAL                     XP0, YP0                ! Major pixel centre
      REAL                     XPS, YPS                ! Sub-pixel pixel centre
      REAL                     YPS2                    ! Sub-pixel distance

      INTEGER                  I, J                    ! Major pixel loops
      INTEGER                  II, JJ                  ! Sub-pixel loops
      INTEGER                  MNX, MNY                ! Local calc bounds
      INTEGER                  XSUB, YSUB              ! Sub-pixel factors

      LOGICAL                  SYMMETRIC               ! Symmetric about centre?
*
*    Inline function :
*
      REAL                     DEL,SIG,PIX
      INTEGER                  SPIX
       SPIX(DEL,PIX) = MAX(1,NINT(abs(10.0*PIX)/RC/MAX(1.0,
     :                                SQRT(ABS(DEL/RC)))))
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Base coordinates
      XP0 = ( - REAL(NX)/2.0 ) * DX + X0 + QX
      YP0 = ( - REAL(NY)/2.0 ) * DY + Y0 + QY

*    To speed things up
      RC2 = RC*RC

*    Symmetric?
      SYMMETRIC = ( ( X0 .EQ. 0.0 ) .AND. ( Y0 .EQ. 0.0 )
     :        .AND. ( QX .EQ. 0.0 ) .AND. ( QY .EQ. 0.0 ) )

*    Bounds for calculation
      IF ( SYMMETRIC ) THEN

*      The "lower left corner" of the array. The +1 guarantees that the
*      centre pixel gets done for odd values of NX/Y
        MNX = (NX+1)/2
        MNY = (NY+1)/2

      ELSE

*      The whole array
        MNX = NX
        MNY = NY

      END IF

*    For each point requiring calculation
      DO J = 1, MNY

*      Find Y sub-pixelling
        YSUB = SPIX( YP0 + DY*REAL(J-1), DY )
        SDY = DY / YSUB

        DO I = 1, MNX

*        Zero
          SUM = 0.0

*        Find X sub-pixelling
          XSUB = SPIX( XP0 + DX*REAL(I-1), DX )
          SDX = DX / XSUB

*        X contribution to normalisation - hence total normalisation
          NORM = ABS( (ALPHA-1.0)*SDX*SDY/(RC2*MATH__PI) )

*        Y position of first sub-pixel centre
          YPS = YP0 + DY*(J-1) + 0.5*SDY

*        For each sub-pixel row
          DO JJ = 0, YSUB-1

*          Y centre of sub-pixel
            YPS2 = YPS*YPS

*          X position of first sub-pixel centre
            XPS = XP0 + DX*(I-1) + 0.5*SDX

*          For each sub-pixel
            DO II = 0, XSUB-1

*            Value of gaussian
              SUM = SUM + (1.0+(XPS*XPS+YPS2)/RC2)**(-ALPHA)

*            Next sub-pixel
              XPS = XPS + SDX

            END DO

*          Next row of sub-pixels
            YPS = YPS + SDY

          END DO

*        Set ARRAY value
          ARRAY(I,J) = SUM*NORM

        END DO

      END DO

*    Copy array around if symmetrical
      IF ( SYMMETRIC ) THEN

*      Transfer data to other 3 quadrants
        JJ = NY
        DO J = 1, MNY
          II = NX
          DO I = 1, MNX
            ARRAY(II,J) = ARRAY(I,J)
            ARRAY(II,JJ) = ARRAY(I,J)
            ARRAY(I,JJ) = ARRAY(I,J)
            II = II - 1
          END DO
          JJ = JJ - 1
        END DO

      END IF

      END
*+  MATH_S22MATINVR - Inverts symmetric 2*2 matrix
      SUBROUTINE MATH_S22MATINVR( A, IERR )
*    Description :
*     Inverts a REAL symmetric 2*2 matrix, represented as vector, length=3
*     where A(1) = (1,1); A(2) = (1,2) = (2,1); A(3) = (2,2)
*    History :
*     25/8/88:  original based on TIM_$TWIN (pla@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      REAL                   A(3)                                       ! Representaion of matrix to be inverted
*    Export :
      INTEGER                IERR                                       ! =0 if OK, =1 if inversion fails
*    Local constants :
      REAL                   LIMIT
         PARAMETER         ( LIMIT = 1.0E-15 )
*    Local variables :
      REAL                   DET                                        ! determinant of matrix
      REAL                   TEMP                                       ! Temporary value of A(1)
*-
      DET = A(1) * A(3) - (A(2) * A(2))

      IF ( ABS(DET) .LT. LIMIT ) THEN
         IERR = 1

      ELSE
         TEMP =  A(3) / DET
         A(2) = -A(2) / DET
         A(3) =  A(1) / DET
         A(1) =  TEMP
         IERR = 0.0

      END IF
      END
*+MATH_SPLWEIGHTS  Calculates the weights for points in a spline fit.
	SUBROUTINE MATH_SPLWEIGHTS(NPTS,X,LP2,LP3,LP4,DIM1,DIM2,
     &                                      DIM3,DIM4,VAR,WEIGHT)
*    Description :
*            This routine produces a set of weights for data points about
*           to be put into a fitting routine.
*    Parameters :
*    Method :
*            The data points are weighted by the reciprocal of their variance.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton (LTVAD::RDS)
*    History :
*     5 May 1988 Original (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
*    Import :
      INTEGER NPTS                         !Number of points in X array
      INTEGER X(NPTS)                      !Array of X values
      INTEGER LP2,LP3,LP4                  !These define the slice of the data
*                                          !array for which weights are wanted.
      INTEGER DIM1,DIM2,DIM3,DIM4          !Dimensions of the variance array
      REAL VAR(DIM1,DIM2,DIM3,DIM4)        !Variance array
*
*    Export :
      DOUBLE PRECISION WEIGHT(NPTS)        !Array of weights.
*
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER LOOP                   ! Loop counter
*    Local data :
*-
      DO LOOP=1,NPTS
*
* Weights must be positive
         IF ( VAR(X(LOOP),LP2,LP3,LP4) .GT. 0.0) THEN
            WEIGHT(LOOP)= 1.0 / VAR(X(LOOP),LP2,LP3,LP4)
         ELSE
            WRITE (*,*) ' Variance negative or zero - setting weight',
     &                  ' to zero'
            WEIGHT(LOOP)= 0.D0
         ENDIF
*
      ENDDO
*
      END
