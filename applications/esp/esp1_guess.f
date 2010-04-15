


      SUBROUTINE ELP1_GUESS(DMODE,ANGCON,ANGOFF,BACK,XCO,YCO,
     :                      ELEMS,ARRAY,PRANGE,POSANG,RADIUS,
     :                      ELLIP,STATUS)
*+
*  Name:
*     ELP1_GUESS

*  Purpose:
*     Determines the initial estimate of ellipticity and position angle to
*     be employed when the search for an ellipse fit begins. It also provides
*     a value of radius at which profiling may begin where there may be a
*     reasonable signal strength.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELP1_GUESS(DMODE,ANGCON,ANGOFF,BACK,XCO,YCO,ELEMS,
*                     ARRAY,PRANGE,POSANG,RADIUS,ELLIP,STATUS)

*  Description:
*     Looks outward from the chosen origin location along lines separated by
*     45 degrees. Determines how far along each of these lines you must
*     look to reach a pixel count value below an arbitrary value. The
*     ratio of the distances in different directions provides some
*     approximation of the ellipticity value and the sums of diametrically
*     opposite lines gives an approximation to the position angle.
*
*     A value is also provided for a radius at which the profiling may begin
*     where the radius will allow accurate results to be quickly obtained ie
*     not too small to include very few points, but small enough for the
*     signal strength to be high.

*  Arguments:
*     DMODE = INTEGER (Given)
*        Is a display to be generated? 0=No 1=Yes.
*     ANGCON = LOGICAL (Given)
*        Position angle convention. TRUE=clockwise is positive.
*     ANGOFF = REAL (Given)
*        Angular offset for the position angle.
*     BACK = REAL (Given)
*        Image background count value. Units counts.
*     XCO = REAL (Given)
*        Suggested X co-ordinate for the galaxy centre.
*     YCO = REAL (Given)
*        Suggested Y co-ordinate for the galaxy centre.
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        The image array.
*     PRANGE(2) = INTEGE (Given)
*        Size of the image axes. Units pixels.
*     POSANG = REAL (Returned)
*        Position angle of the galaxy.
*     RADIUS = REAL (Returned)
*        First radius of the galaxy to try. Units pixels.
*     ELLIP = REAL (Returned)
*        Approximate ellipticity of the galaxy.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     4-Apr-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'ELP_PAR'               ! ELLPRO constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      LOGICAL ANGCON                  ! Position angle convention
      INTEGER DMODE                   ! Generate a display? 0=No 1=Yes
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER PRANGE(2)               ! Size of the image
      REAL ANGOFF                     ! Position angle offset
      REAL ARRAY(ELEMS)               ! Image array
      REAL BACK                       ! Background count of the image
      REAL XCO                        ! X co-ord of the galaxy centre
      REAL YCO                        ! Y co-ord of the galaxy centre

*  Arguments Returned:
      REAL ELLIP                      ! Ellipticity estimate
      REAL POSANG                     ! Position angle estimate
      REAL RADIUS                     ! Radius estimate

*  Arguments Given and Returned:

*  Local variables:
      CHARACTER *(256) TOP            ! A heading
      INTEGER FAR                     ! Distance from X Y co-ords beyond
                                      ! which the image bounds must be
                                      ! exceeded
      INTEGER FOUND(10)               ! Was a distance value found
                                      ! for a given angle
      INTEGER I                       ! Temporary storage
      INTEGER J                       ! Temporary storage
      INTEGER LOW                     ! Index of the line with
                                      ! the smallest centre/threshold
                                      ! distance
      INTEGER HIGH                    ! Index of the line with
                                      ! the largest centre/threshold
                                      ! distance
      INTEGER MAXFOU                  ! Was a valid pixel found?
      INTEGER MEANC                   ! Counter
      INTEGER XI(8)                   ! X axis increment
      INTEGER YI(8)                   ! Y axis increment
      REAL ADD                        ! Array address
      REAL DIST(10)                   ! Distance from the galaxy centre
      REAL MAX                        ! Maximum temporary value
      REAL MEAN                       ! The mean pixel value found
      REAL MIN                        ! Minimum temporary value
      REAL POS1                       ! Temporary position angle storage
      REAL POS2                       ! Temporary position angle storage
      REAL TEMP                       ! Temporary storage
      REAL THRESH                     ! Threshold count value
      REAL VALUE                      ! Temporary storage
      REAL X                          ! Temporary X co-ordinate
      REAL Y                          ! Temporary Y co-ordinate
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Setup arrays containing the increments in X and Y for examining lines
*   of pixels outward from the proposed galaxy origin at angles of
*   0, 45, 90, 135, 180, 225, 270, 315 respectively.

*   X increments.
      XI(1)=0
      XI(2)=1
      XI(3)=1
      XI(4)=1
      XI(5)=0
      XI(6)=-1
      XI(7)=-1
      XI(8)=-1

*   Y increments.
      YI(1)=1
      YI(2)=1
      YI(3)=0
      YI(4)=-1
      YI(5)=-1
      YI(6)=-1
      YI(7)=0
      YI(8)=-1

*   Find an approximate value for the pixel count for near the
*   origin of the galaxy.
      MAX=BACK
      MIN=ELP__VBIG
      MAXFOU=0

*   Set mean initial values.
      MEAN=0.0
      MEANC=0

*   Look in a 13x13 box around the origin provided.
      DO 10 I=-6,6

*      Set the X co-ordinate.
         X=NINT(XCO)+I

         DO 20 J=-6,6

*          Set the Y co-ordinate.
             Y=NINT(YCO)+J

*          Check that the pixel is within the image.
             IF ((X.GE.1).AND.(X.LE.PRANGE(1)).AND.(Y.GE.1)
     :            .AND.(Y.LE.PRANGE(2))) THEN

*             Calculate the array address of the pixel required.
                ADD=(Y-1)*PRANGE(1)+X

*             Get the pixel value.
                VALUE=ARRAY(INT(ADD))

*             Check that it was not a BAD pixel.
                IF (VALUE.NE.VAL__BADR) THEN

*                Add to the mean accumulator.
                   MEAN=MEAN+VALUE
                   MEANC=MEANC+1

*                Is it bigger than the highest value yet found?
                   IF (VALUE.GT.MAX) THEN
                      MAX=VALUE
                      MAXFOU=1
                   END IF

*                Is it smaller than the lowest value found.
                   IF (VALUE.LT.MIN) MIN=VALUE

                END IF

             END IF

 20      CONTINUE

 10   CONTINUE

*   Calculate the mean.
      IF (MEANC.GT.0) THEN
         MEAN=MEAN/REAL(MEANC)
      ELSE
         MEAN=(MAX+MIN)/2.
      END IF

*   Set a suitable threshold value.
      IF (MAXFOU.NE.0) THEN

*      Use the figures obtained.
         THRESH=( (MAX+MIN)/2.+ MEAN )/2.

      ELSE

*      Use guesses and return immediately to the calling routine.
         RADIUS=6.0
         ELLIP=1.0
         POSANG=0.0
         GOTO 9998

      END IF

*   Clear the arrays required.
      DO 30 I=1,8
         DIST(I)=0.0
         FOUND(I)=0.0
 30   CONTINUE

*   Determine how far out from the co-ordinates the search must start.
      FAR=NINT(SQRT(1.*PRANGE(1)*PRANGE(1)+1.*PRANGE(2)*PRANGE(2)))

*   Look along lines inward toward the centre of the galaxy to find out
*   at what distance the pixel count value drops below the threshold.
      DO 40 J=1,8

*      Look inward along the required lines.
         DO 50 I=FAR,1,-1

*         Calculate the pixel co-ordinate.
            X=NINT(XCO)+I*XI(J)
            Y=NINT(YCO)+I*YI(J)

*         Check that the pixel is within the image.
            IF ((X.GE.1).AND.(X.LE.PRANGE(1)).AND.(Y.GE.1)
     :           .AND.(Y.LE.PRANGE(2))) THEN

*            Find the array address of the pixel required.
               ADD=(Y-1)*PRANGE(1)+X

*            Get its value.
               VALUE=ARRAY(INT(ADD))

*            Check that the value is not bad.
               IF (VALUE.NE.VAL__BADR) THEN

*               Only act if the value is still below the threshold
                  IF (VALUE.LT.THRESH) THEN

*                  Update the estimate of the distance away from the centre
*                  at which the threshold is crossed.
                     DIST(J)=I
                     FOUND(J)=1

                  END IF

               END IF

            END IF

 50      CONTINUE

 40   CONTINUE

*   Average estimates for 0/180 45/225 degrees etc.
      DO 60 I=1,4

*      Average the result if a value was found for both lines.
         IF ((FOUND(I).GT.0).AND.(FOUND(I+4).GT.0)) THEN

            DIST(I)=(DIST(I)+DIST(I+4))/2.

         ELSE

*         Take the only value found if one of the pair did not have a value
*         assigned.
            IF ((FOUND(I).GT.0).OR.(FOUND(I+4).GT.0)) THEN
               DIST(I)=DIST(I)+DIST(I+4)
               FOUND(I)=1
            END IF

         END IF

*      Allow for the diagonal lines being root(2) longer.
         IF ((I.EQ.2).OR.(I.EQ.4)) DIST(I)=DIST(I)*SQRT(2.)

 60   CONTINUE

*   Look to see which line (0,45,90,135 degrees) has the highest value
*   and which the lowest value. These will be used to give a rough
*   estimate of ellipse axis lengths and the ellipticity
      MAX=-1E+20
      MIN=1E+20
      HIGH=0
      LOW=0
      DO 70 I=1,4

*      Check that a value was found for the current pair of diametrically
*      opposed lines outward from the origin. Uses the sum of pairs since
*      (for example) -45 and +135 are for these purposes the same thing.
*      pairs are used to reduce the influence of BAD areas.
         IF (FOUND(I).GT.0) THEN

*         If current value is biggest then keep it.
            IF (DIST(I).GT.MAX) THEN
               MAX=DIST(I)
               HIGH=I
            END IF

*         If current value is smallest then keep it.
            IF (DIST(I).LT.MIN) THEN
               MIN=DIST(I)
               LOW=I
            END IF

         END IF

 70   CONTINUE

*   If no radius was found use a approximate one.
*   Then exit from the routine.
      IF ((HIGH.EQ.0).OR.(LOW.EQ.0)) THEN
         RADIUS=6.0
         ELLIP=1.0
         POSANG=0.0
         GOTO 9998
      END IF

*   Set value for first radius to be profiled. Set lower limit of 4.0
      RADIUS=MAX
      IF (RADIUS.LT.4.) RADIUS=4.

*   Set a value for the ellipticity.
      ELLIP=.5
      IF ((FOUND(HIGH).NE.0).AND.(FOUND(INT(MIN)).LT.MAX)) THEN

*      Check that the minimum axis is less that the maximum.
         IF (MIN.LT.MAX) ELLIP=MIN/MAX

*      Ensure that for the first profile the centre is within
*      the ellipse.
         IF (ELLIP*RADIUS.LT.3.0) ELLIP=4.0/RADIUS

      END IF

*   Determine a first estimate of the position angle of the ellipse.
      POSANG=45.
      IF (LOW.LT.HIGH) THEN
        POS1=(HIGH-1)*45.
        POS2=(LOW+1)*45.
      ELSE
         POS1=(HIGH-1)*45.
         POS2=(LOW-3)*45.
      END IF
      POSANG=(POS1+POS2)/2.

*   Line to abort to if no figure for the first guess is easily available.
 9998 CONTINUE

*   Impose sensible limits on ellipticity and position angle.
      IF (ELLIP.GT.0.85) ELLIP=0.85
      IF (ELLIP.LT.0.1) ELLIP=0.1
      IF (POSANG.GT.90.) POSANG=POSANG-180.

*   Display the first guess generated (if required).
      IF (DMODE.GT.0) THEN

         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','Initial parameter estimates',STATUS)
         CALL MSG_FMTR('RAD','F8.2',SQRT(RADIUS*RADIUS*ELLIP))
         IF (ANGCON) THEN
            TEMP=POSANG+ANGOFF
         ELSE
            TEMP=-POSANG+ANGOFF
         END IF
         CALL MSG_FMTR('POS','F6.1',TEMP)
         CALL MSG_FMTR('ELL','F4.3',ELLIP)
         CALL MSG_OUT(' ','Rad(a) ^RAD    Posang ^POS    1/Ellipt ^ELL',
     :                STATUS)

*      Heading for output.
         CALL MSG_BLANK(STATUS)
         TOP='  X       Y     Points   Rad(a)    Count     PA '//
     :       ' 1/Ellip  Dev.  PPU  Statistic'
         CALL MSG_OUT(' ',TOP,STATUS)

      END IF

 9999 CONTINUE

      END



      SUBROUTINE GAU1_GUESS(NSOUR,ANGCON,ANGOFF,PSIZE,
     :     SIGMA,NSIGMA,BACK,XCO,YCO,RLIM,ELEMS,ARRAY2,PRANGE,
     :     GUESS,HINT,STATUS)
*+
*  Name:
*     GAU1_GUESS

*  Purpose:
*     Determines the initial estimates of peak value, sigmax, sigmay
*     and position angle.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_GUESS(NSOUR,ANGCON,ANGOFF,PSIZE,SIGMA,NSIGMA,BACK,XCO,YCO,
*                     RLIM,ELEMS,ARRAY2,PRANGE,GUESS,HINT,STATUS)

*  Description:
*     Crudely fits a 2-D parabola to the Gaussian peak. The slice
*     used to generate the radius is rotated to determine the
*     approximate position angle, sigmas, peak and position of
*     of each source.

*  Arguments:
*     NSOUR = INTEGER (Given)
*        Number of sources.
*     ANGCON = LOGICAL (Given)
*        Angle rotation convention. Defines if clockwise or
*        anticlockwise is considered positive. TRUE=Clockwise.
*     ANGOFF = REAL (Given)
*        Angular offset for position angles generated. Units degrees.
*     PSIZE = REAL (Given)
*        Pixel size in arcsec
*     NSIGMA = REAL (Given)
*        Number of sigma at which the pixels become significant.
*     SIGMA = REAL (Given)
*        Standard deviation of the image background value.
*     BACK = REAL (Given)
*        Image background count value.
*     XCO(10,2) = REAL (Given)
*        Suggested X co-ordinate for the source centre.
*     YCO(10,2) = REAL (Given)
*        Suggested Y co-ordinate for the source centre.
*     RLIM(10) = REAL (Given)
*        The radius estimates.
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     ARRAY2(ELEMS) = REAL (Given)
*        The image array.
*     PRANGE(2) = INTEGE (Given)
*        Size of the image axes. Units pixels.
*     GUESS(10,7) = REAL (Returned)
*        First guesses at the source parameters.
*        [NG] for meanings, see docn for gau1_build
*     HINT(4,10) = REAL (Returned)
*        User angle, Sa, Sb and peak values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     4-Mar-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      LOGICAL ANGCON                  ! Angular rotation convention
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER NSOUR                   ! Number of sources
      INTEGER PRANGE(2)               ! Size of the image
      REAL ANGOFF                     ! Angular offset
      REAL PSIZE		      ! Pixel size in arcsec
      REAL ARRAY2(ELEMS)              ! Image array
      REAL BACK                       ! Background count of the image
      REAL NSIGMA                     ! Number of sigma at which pixels
                                      ! are considered
      REAL RLIM(10)                   ! Radius of each source
      REAL SIGMA                      ! Std deviation of the background count
      REAL XCO(10,2)                  ! X co-ord of the source centre
      REAL YCO(10,2)                  ! Y co-ord of the source centre

*  Arguments Returned:
      REAL HINT(4,10)                 ! User angle, Sa, Sb and peak values
      REAL GUESS(10,7)                ! Approximate parameters

*  Local variables:
      INTEGER ANG                     ! Angle of slice
      INTEGER BIGGER                  ! Total
      INTEGER DIST                    ! Distance to the source
      INTEGER I                       ! Temporary storage
      INTEGER J                       ! Temporary storage
      INTEGER K                       ! Temporary storage
      INTEGER NFOUND                  ! Number of solutions
      INTEGER XMAX                    ! Width of image
      INTEGER YMAX                    ! Depth of image
      REAL ADD                        ! Array address
      REAL DETERM                     ! Inverted matrix determinant
                                      ! (used to indicate failure)
      REAL DV                         ! Distance from origin
      REAL GX                         ! Increment in X
      REAL GY                         ! Increment in Y
      REAL INPMAT(3,3)                ! Matrix array passed to
                                      ! subroutine GAU1_GAUJO
      REAL MA                         ! Maximum std dev
      REAL MI                         ! Minimum std dev
      REAL PI2                        ! Useful conversion factor
      REAL R                          ! Distance from source
      REAL RMAX                       ! Maximum std dev ratio
      REAL SOL(90,5)                  ! Solutions found
      REAL TEMP(7)                    ! Temporary
      REAL TOTAL                      ! Used to calculate averages
      REAL VALUE                      ! Temporary storage
      REAL VECTOR(3)                  ! Vector array in which parabola
      REAL V1                         ! Sum of some sdev values
      REAL V2                         ! Sum of some sdev values
                                      ! coefficients are returned from
                                      ! subroutine GAU1_GAUJO
      REAL X                          ! Temporary X co-ordinate
      REAL XX(3)                      ! Temporary array
      REAL Y                          ! Temporary Y co-ordinate
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Assign value for width of image.
      XMAX=PRANGE(1)
      YMAX=PRANGE(2)

*   Assign a useful cnostant.
      PI2=2.*3.1415926/360.

*   Tell the user what is going on.
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','First estimates of source data',STATUS)
      CALL MSG_BLANK(STATUS)

**   Debugging
*      open (50,file='debuggaufit')
*      write (50,'("gau1_guess: nsour=",i2," xmax=",i10," ymax=",i10)')
*     :     nsour,xmax,ymax

*   For each source in turn get an average value for peak etc.
      DO 10 I=1,NSOUR

*      Set the radius limit.
         R=INT(RLIM(I))

*      Number of profile fits found.
         NFOUND=0

*      Look at the current angle. Using step 2 avoid division by zero.
        DO 20 ANG=1,180,2

*         Find the sin/cos values of the line to be used.
            GY=SIN(REAL(ANG)*PI2)
            GX=COS(REAL(ANG)*PI2)

*         Clear the arrays to be used.
            DO 450 K=1,3
               VECTOR(K)=0.0
               DO 440 J=1,3
                  INPMAT(J,K)=0.0
 440           CONTINUE
 450        CONTINUE

*         Look along the line.
            DO 30 DIST=INT(-R),INT(R)

*            Find the current pixel.
               X=NINT(XCO(I,1)+REAL(DIST)*GX)
               Y=NINT(YCO(I,1)+REAL(DIST)*GY)

**            Debugging
*               write (50,220) x,y,r,dist
* 220           format('gau1_guess: x=',g10.2,' y=',g10.2,' r=',g10.2,
*     :              ' dist=',i4)

*            Check that the pixel is within the image.
               IF ((X.GE.1).AND.(X.LE.XMAX).AND.(Y.GE.1)
     :            .AND.(Y.LE.YMAX)) THEN

*               Get the pixel address.
                  ADD=(NINT(Y)-1)*XMAX+NINT(X)

*               Get the pixel value.
                  VALUE=ARRAY2(INT(ADD))

*               Check that it was not a BAD pixel.
                  IF (VALUE.NE.VAL__BADR) THEN

*                  Convert to log.
                     VALUE=ALOG(VALUE)

*                  Prepare matrix for inversion.
                     XX(1)=1.0
                     XX(2)=REAL(DIST)
                     XX(3)=XX(2)*XX(2)
                     DO 470 J=1,3
                       VECTOR(J)=VECTOR(J)+XX(J)*VALUE
                       DO 460 K=1,3
                          INPMAT(K,J)=INPMAT(K,J)+XX(K)*XX(J)
 460                   CONTINUE
 470                CONTINUE

                  END IF

               END IF

 30         CONTINUE

*         If sufficient data points are available, perform the matrix
*         inversion.
            IF (INPMAT(1,1).GT.2.0) THEN

*            Matrix inversion.
               CALL GAU1_GAUJO(VECTOR,INPMAT,DETERM,STATUS)
               IF (STATUS.NE.SAI__OK) GOTO 9999

*            Test for direction of parabola.
               IF(VECTOR(3).LT.0.0) THEN

*               Increment the counter.
                  NFOUND=NFOUND+1

*               The amended position of the fit profile.
                  DV=-VECTOR(2)/2./VECTOR(3)
                  SOL(NFOUND,1)=XCO(I,1)+DV*GX
                  SOL(NFOUND,2)=YCO(I,1)+DV*GY

                  SOL(NFOUND,3)=SQRT(SQRT(-1./VECTOR(3)/2.))

*               The peak value. RMS.
                  SOL(NFOUND,4)=SQRT(EXP(VECTOR(1)-
     :                          (VECTOR(2)/2.)
     :                           **2/VECTOR(3)))

*               The angle.
                  SOL(NFOUND,5)=REAL(ANG)

**               Debugging
*                  write (50,230)nfound,(sol(nfound,k),k=1,5)
* 230              format('gau1_guess: sol(',i2,'):',5g10.2)

               END IF

            END IF

 20      CONTINUE

*      Find the average of each parameter.
         DO 91 J=1,4

            TOTAL=0.0
            DO 90 K=1,NFOUND
              TOTAL=TOTAL+SOL(K,J)
 90         CONTINUE

*         Create estimate. Use previous guess for peak height as well.
            VALUE=TOTAL/REAL(NFOUND)
            IF ((J.NE.4).OR.(GUESS(I,4).LT.-1E19)) THEN
               GUESS(I,J)=VALUE
               IF (J.EQ.3) GUESS(I,3)=VALUE*VALUE
            ELSE
               GUESS(I,4)=GUESS(I,4)*.95+VALUE*.05
            END IF

 91      CONTINUE


*      Now must find most likely direction.
*      Will take mean standard deviation of 5 orthogonal
*      angles and find the biggest ratio.
         IF(NFOUND.LT.6) THEN

*         Make a guess at parameters.
            VALUE=RLIM(I)/3.
            IF (VALUE.LT.2.0) VALUE=2.0

            GUESS(I,1)=XCO(I,1)
            GUESS(I,2)=YCO(I,1)

            GUESS(I,3)=VALUE

            GUESS(I,4)=GUESS(I,4)

            GUESS(I,5)=VALUE
            GUESS(I,6)=VALUE

            GUESS(I,7)=45.

*         Tell the user there are problems.
            CALL MSG_FMTI('I','I2',I)
            CALL MSG_OUT(' ','^I Could not fit a Gaussian to'/
     :       /' this source.',STATUS)

         ELSE

*         Look at sum of 5 sdevs in orthogonal directions.
            RMAX=0.0
            DO 100 J=3,NFOUND-2-45

*            Sum in each direction.
               V1=0.0
               V2=0.0
               DO 110 K=-2,2
                  V1=V1+SOL(J-K,3)
                  V2=V2+SOL(J-K+45,3)
 110           CONTINUE

*            Find ratio maximum and angle value at which it occurs.
               IF (ABS(V2-V1).GT.RMAX) THEN

                  RMAX=ABS(V2-V1)
                  GUESS(I,7)=(SOL(J-2,5)+SOL(J-1,5)+
     :                        SOL(J,5)+  SOL(J+1,5)+
     :                        SOL(J+2,5))/5.
                  IF(V2.GT.V1) GUESS(I,7)=GUESS(I,7)+90.

*               Keep std devs for orthogonal axes.
                  MA=V2
                  MI=V1

               END IF

 100        CONTINUE

*         Assign the values for parameters determined.
            VALUE=MAX(MI*MI/25.,MA*MA/25.)
            GUESS(I,5)=MIN(VALUE,GUESS(I,3))
            GUESS(I,6)=MIN(MI*MI/25.,MA*MA/25.)

         END IF

 10   CONTINUE

*   Check peak height is sane.
*   Only do this, however, if the background is positive.  The
*   background may legitimately be negative, if we're using the
*   non-linear least-squares fitting method; this indicates that
*   the background is to be fitted from the data, rather than subtracted
*   before fitting.
      IF (BACK .GE. 0.0) THEN
         BIGGER=0
         DO 95 K=1,NFOUND
            IF(GUESS(I,4).GT.BACK) BIGGER=BIGGER+1
 95      CONTINUE

*      Abort if that is the case.
         IF (BIGGER.EQ.NFOUND) THEN
            CALL MSG_OUT(' ','All the peaks are below'//
     :           ' the proposed background!',STATUS)
            CALL MSG_OUT(' ','Quitting!',STATUS)
            STATUS=SAI__ERROR
            GOTO 9999
         END IF
      END IF

*   Look at the sigma x/y values and determine if RLIM is too big
*   (the smaller the better for execution speed).
      DO 900 I=1,NSOUR

*      Compare 2 sigma points in each direction with RLIM.
         X=MIN(2.*GUESS(I,5),RLIM(I))
         Y=MIN(2.*GUESS(I,6),RLIM(I))

*      Make sure the final version of RLIM is smaller than
*      the 2 sigma point in each direction.
         RLIM(I)=MIN(X,Y)+1.

 900  CONTINUE

*   Use the HINT value supplied in the file if it is present.
      DO 950 I=1,NSOUR
         IF(HINT(1,I).NE.VAL__BADR) GUESS(I,5)=HINT(1,I)
         IF(HINT(2,I).NE.VAL__BADR) GUESS(I,6)=HINT(2,I)
         IF(HINT(3,I).NE.VAL__BADR) GUESS(I,7)=HINT(3,I)
         IF(HINT(4,I).NE.VAL__BADR) GUESS(I,4)=HINT(4,I)
 950  CONTINUE

*   Sort the values into descending order.
*   Speed is not critical for this small array manipulation.
      DO 1000 I=1,NSOUR-1
         DO 1100 J=I,NSOUR

*         Find biggest of pair.
            IF (GUESS(I,4).LT.GUESS(J,4)) THEN

*            Transfer the information to temp array.
               DO 1200 K=1,7
                  TEMP(K)=GUESS(I,K)
 1200          CONTINUE

*            Swap one set of values.
               DO 1300 K=1,7
                  GUESS(I,K)=GUESS(J,K)
 1300          CONTINUE

*            Sort out the remaining array using the data
*            stored in temporary array.
               DO 1400 K=1,7
                  GUESS(J,K)=TEMP(K)
 1400          CONTINUE

            END IF

 1100    CONTINUE
 1000 CONTINUE

*   Display the guessed parameter values.
      DO 1500 I=1,NSOUR
         CALL GAU1_DISP(I,ANGCON,ANGOFF,PSIZE,GUESS,STATUS)
 1500 CONTINUE

 9999 CONTINUE

      END
