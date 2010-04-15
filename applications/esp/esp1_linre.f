

      SUBROUTINE GRA1_LINRE(POINTS,LOWLIM,FTYPE,RRANGE,LOR,HIR,
     :                      RESULT,PSIZE,GRAD,CONS,SLEN,
     :                      NUMBP,REG,STATUS)
*+
*  Name:
*     GRA1_LINRE

*  Purpose:
*     Determines a least squares linear fit for data in arrays X and Y.
*     This fit is used to calculate value for the scale length of the
*     object if it was a spiral galaxy or elliptical galaxy.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRA1_LINRE(POINTS,LOWLIM,FTYPE,RRANGE,LOR,HIR,RESULT,
*                     PSIZE,BACK,CONS,SLEN,NUMBP,REG,STATUS)

*  Description:
*     Uses a normal least squares method to determine the coefficients
*     of a linear fit to the contents of arrays X and Y.

*  Arguments:
*     POINTS = INTEGER (Given)
*        Number of profiles available for the current galaxy.
*     LOWLIM = REAL (Given)
*        Number of arc seconds below which points are not included in
*        the profile calculation of the scale lengths.
*     FTYPE *(3) = CHARACTER (Returned)
*        The record type found in the results file. ELF=ELLFOU,
*        SEC=SECTOR and ELP=ELLPRO.
*     RRANGE = LOGICAL (Given)
*        Are the data points used in the analysis to be selected automatically?
*     LOR = REAL (Given)
*        The radius below which a data point should not be included
*        in the linear regression. Units arc seconds.
*     HIR = REAL (Given)
*        The radius above which a data point should not be included
*        in the linear regression. Units arc seconds.
*     RESULT(GRA__RESUL) = REAL (Given)
*        The summation of pixels counts at a given radius. Units counts.
*     PSIZE = REAL (Given)
*        Image pixel size in arc secs.
*     GRAD(2) = REAL (Returned)
*        The gradient values for the linear fit.
*     CONS(2) = REAL (Returned)
*        The constant values for the linear fit.
*     SLEN(2) = REAL (Returned)
*        The scale lengths of the two fits i.e. spiral and elliptical.
*        Units arc seconds.
*     NUMBP(2) = INTEGER (Returned)
*        The number of data points used during the linear regression.
*     REG(2) = REAL (Returned)
*        The square of the linear correlation coefficient.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-DEC-1995 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'GRA_PAR'               ! GRAPHS constants

*  Arguments Given:
      CHARACTER *(3) FTYPE            ! File header type
      LOGICAL RRANGE                  ! Select data points interactively?
      INTEGER POINTS                  ! Number of profiles available
      REAL HIR                        ! The upper limit of radius requested
      REAL LOR                        ! The lower limit of radius requested
      REAL LOWLIM                     ! The lower radius limit
      REAL PSIZE                      ! Size of the pixels in arc secs
      REAL RESULT(GRA__RESUL,17)      ! Summed counts for all pixels at a
                                      ! given radius

*  Arguments Returned:
      INTEGER NUMBP(2)                ! Number of data points used
      REAL CONS(2)                    ! Constant of linear equation
      REAL GRAD(2)                    ! Gradient of linear equation
      REAL REG(2)                     ! Regression coefficient squared
      REAL SLEN(2)                    ! Scale length of the galaxy

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      INTEGER FLAG                    ! Problems with the regresssion flag
      INTEGER I                       ! Loop variable
      INTEGER LLI                     ! Lowest useable pixel index
      INTEGER TYPE                    ! Defines the data transform type
      REAL LIMIT                      ! Number of pixels = lowlim
      REAL MEANELL                    ! Mean ellipticity value
      REAL MNVX                       ! Mean value of X1 array
      REAL MNVY                       ! Mean value of Y1 array
      REAL NUMB                       ! Number of data points
      REAL SUMX                       ! Sum of X1 array
      REAL SUMY                       ! Sum of Y1 array
      REAL TOT(7)                     ! Various total and summations used
                                      ! later.
      REAL TOTELL                     ! Total of ellipticity values
      REAL X                          ! Transformed raw data radius value
      REAL Y                          ! Transformed raw data pixel count



*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*    Set up the automatic values for LOR and HIR radius limits.
      IF (RRANGE) THEN

*      Assign default values.
         LOR=RESULT(1,4)
         HIR=RESULT(POINTS,4)

*      Find the number of pixels corresponding to LOWLIM arc secs
         LIMIT=LOWLIM/REAL(PSIZE)
         LLI=POINTS
         DO 100 I=POINTS,1,-1
            IF (RESULT(I,4).GE.LIMIT) LLI=I
 100     CONTINUE

*      Deal with a very small number of points.
         IF (POINTS.LT.4) THEN
            LOR=RESULT(1,4)
            HIR=RESULT(POINTS,4)
         END IF

*      Sufficient data points to allow the central limit to be imposed but not
*      the outer limit value.
         IF (POINTS-LLI+1.EQ.3) THEN
            LOR=RESULT(LLI,4)
            HIR=RESULT(POINTS,4)
         END IF

*      Sufficient data points to allow the central limit to be imposed and
*      also the outermost point to be ignored.
         IF (POINTS-LLI+1.GT.3) THEN
            LOR=RESULT(LLI,4)
            HIR=RESULT(POINTS-1,4)
         END IF

*      Find mean ellipticity value.
         TOTELL=0.0
         DO 20 I=1,POINTS
            TOTELL=TOTELL+RESULT(I,7)*RESULT(I,7)
 20      CONTINUE
         MEANELL=SQRT(TOTELL/REAL(POINTS))

*      Employ mean vlues of ellipticity to give radii limit modifications.
         LOR=PSIZE*SQRT(LOR*LOR*MEANELL)
         HIR=PSIZE*HIR

      END IF

*   Find the mean value of x and the sums of the x and y arrays.

*   Set an error flag.
      FLAG=0

*   Set the line fit values to zero.
      DO 10 I=1,2
         GRAD(I)=0.0
         CONS(I)=0.0
 10   CONTINUE

*   Calculate the linear regression for each of the galaxy types.
      DO 200 TYPE=1,2

*      Setup the initial values for the sums and counter.
         SUMX=0.0
         SUMY=0.0
         MNVX=0.0
         MNVY=0.0

         NUMBP(TYPE)=0

*      Loop through all the data points used.
         DO 500 I=1,POINTS

*         Calculate the radius.
            IF ((FTYPE.EQ.'ELF').OR.(FTYPE.EQ.'ELP')) THEN
               X=PSIZE*SQRT(RESULT(I,4)*RESULT(I,4)*RESULT(I,7))
            ELSE
               X=RESULT(I,4)*PSIZE
            END IF
            Y=LOG10(RESULT(I,5))

*         Only use the value if it is within the requested limits.
            IF ((X.GE.LOR).AND.(X.LE.HIR)) THEN

*            Increment the points used counter.
               NUMBP(TYPE)=NUMBP(TYPE)+1

*            Convert radius to form required for spiral galaxy or
*            for an elliptical galaxy.
               IF (TYPE.EQ.2) X=X**(0.25)

*            Sum the X and Y values found (radius and brightness).
               SUMX=SUMX+X
               SUMY=SUMY+Y

            END IF

 500     CONTINUE

*      Raise an error flag since the number of data points was too low.
         IF (NUMBP(TYPE).LT.2) THEN

            FLAG=1
            GOTO 200

         ELSE

*         Produce the mean value of X and a real representation of the number
*         of data points that contributed to the current mean.
            NUMB=REAL(NUMBP(TYPE))
            MNVX=SUMX/NUMB
            MNVY=SUMY/NUMB

         END IF

*      Calculate the squared sum of (x-xmean)
*      and thereby the gradient and constant terms in the equation.

*      Set up the initial values for the sums.
         DO 99 I=1,7
            TOT(I)=0.0
 99      CONTINUE

*      Loop through all the data points used.
         DO 510 I=1,POINTS

*         Calculate the radius and log of the count.
            IF ((FTYPE.EQ.'ELF').OR.(FTYPE.EQ.'ELP')) THEN
               X=PSIZE*SQRT(RESULT(I,4)*RESULT(I,4)*RESULT(I,7))
            ELSE
               X=RESULT(I,4)*PSIZE
            END IF
            Y=LOG10(RESULT(I,5))

*         Check that the radius is within the range requested.
            IF ((X.GE.LOR).AND.(X.LE.HIR)) THEN

*            Generate the radius and brightness values.
               IF (TYPE.EQ.2) X=X**(0.25)

*            Calculate X deviations from the mean.
               TOT(1)=X-MNVX
               TOT(2)=TOT(2)+TOT(1)*TOT(1)

*            Calculate Y deviations from the mean.
               TOT(4)=Y-MNVY

*            Needed to calculate gradient later.
               TOT(3)=TOT(3)+TOT(1)*Y

*            Needed to calculate correlation.
               TOT(5)=TOT(5)+TOT(1)*TOT(1)
               TOT(6)=TOT(6)+TOT(4)*TOT(4)
               TOT(7)=TOT(7)+TOT(1)*TOT(4)

            END IF

 510     CONTINUE

*      Check that more than one value of pixel count was found.
         IF (ABS(TOT(1)).GT.GRA__VSMAL) THEN

*         Calculate the linear regression co-efficients.

            GRAD(TYPE)=TOT(3)/TOT(2)
            CONS(TYPE)=(SUMY-SUMX*GRAD(TYPE))/NUMB

*         Check that the gradient is physically reasonable.
            IF (GRAD(TYPE).LT.0.0) THEN

*            Calculate the scale length.
               IF ((TYPE.EQ.1).AND.(GRAD(TYPE).NE.0.0)) THEN
                  SLEN(TYPE)=-LOG10(EXP(1.0))/GRAD(TYPE)
               ELSE
                  SLEN(TYPE)=LOG10(EXP(1.0))/((-GRAD(TYPE))**(4.0))
               END IF

*            Calculate regression.
               REG(TYPE)=TOT(7)/SQRT(TOT(5))/SQRT(TOT(6))
               REG(TYPE)=REG(TYPE)*REG(TYPE)

            ELSE

*            Set the error flag since the gradient was zero or greater.
               FLAG=3

            END IF

         ELSE

*      Raise an error flag if all the pixels had the same value.
            FLAG=2

         END IF

 200  CONTINUE

*   Display the reason that the linear regression failed.
      IF (FLAG.GT.0) THEN

         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','WARNING!!!',STATUS)

*      Too few data points found for a result to be calculated.
         IF (FLAG.EQ.1) THEN
            CALL MSG_OUT(' ','Not enough data points '//
     :                       'were selected.',STATUS)
         END IF

*      Not possible to calculate a meaningful slope.
         IF (FLAG.EQ.2) THEN
            CALL MSG_OUT(' ','The points selected had a '//
     :                       'single value.',STATUS)
         END IF

*      Negative or zero scale length.
         IF (FLAG.EQ.3) THEN
            CALL MSG_OUT(' ','The scale length calculated '//
     :                       'was not physically sensible.',STATUS)
         END IF

         CALL MSG_BLANK(STATUS)

      END IF

 9999 CONTINUE

      END


      SUBROUTINE HIS1_LINRE(X1,Y1,NUMDAT,GRAD,CONS,STATUS)
*+
*  Name:
*     HIS1_LINRE

*  Purpose:
*     Determines a least squares linear fit for data in arrays X and Y.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HIS1_LINRE(X1,Y1,NUMDAT,GRAD,CONS,STATUS)

*  Description:
*     Uses a normal least squares method to determine the coefficients
*     of a linear fit to the contents of arrays X and Y.

*  Arguments:
*     X1(HIS__CHORM) = REAL (Given)
*        The x values to be used.
*     Y1(HIS__CHORM) = REAL (Given)
*        The y values to be used.
*     NUMDAT = INTEGER (Given)
*        The number of data (X/Y) pairs to be fitted.
*     GRAD   = REAL (Returned)
*        The gradient value for the linear fit.
*     CONS   = REAL (Returned)
*        The constant value for the linear fit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-May-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'HIS_PAR'               ! HISTPEAK system variables

*  Arguments Given:
      INTEGER NUMDAT                  ! Number of data points
      REAL X1(HIS__CHORM)             ! Data points X1 value
      REAL Y1(HIS__CHORM)             ! Data points Y1 value

*  Arguments Returned:
      REAL CONS                       ! Constant of linear equation
      REAL GRAD                       ! Gradient of linear equation

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      INTEGER I                       ! Loop variable
      REAL MNVX                       ! Mean value of X1 array
      REAL NUMB                       ! Number of data points
      REAL SUMX                       ! Sum of X1 array
      REAL SUMY                       ! Sum of Y1 array
      REAL TOT1                       ! Absolute X1 deviation from
                                      ! the mean
      REAL TOT2                       ! Absolute X1 deviation squared
                                      ! sum
      REAl TOT3                       ! Absolute X1 deviation
                                      ! times Y1

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

      NUMB=REAL(NUMDAT)

*   Find the mean value of x and the sums of the x and y arrays.
      SUMX=0.0
      SUMY=0.0
      MNVX=0.0
      DO 500 I=1,NUMDAT
         SUMX=SUMX+X1(I)
         SUMY=SUMY+Y1(I)
 500  CONTINUE
      MNVX=SUMX/NUMB

*   Calculate the squared sum of (x-xmean)
*   and thereby the gradient and constant terms in the equation.
      TOT1=0.0
      TOT2=0.0
      TOT3=0.0
      DO 510 I=1,NUMDAT
         TOT1=X1(I)-MNVX
         TOT2=TOT2+TOT1*TOT1
         TOT3=TOT3+TOT1*Y1(I)
 510  CONTINUE

*   Check that more than one value of pixel count was found.
      IF (ABS(TOT1).GT.HIS__VSMAL) THEN
         GRAD=TOT3/TOT2
         CONS=(SUMY-SUMX*GRAD)/NUMB
      ELSE
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','All the histogram chords had the same'//
     :                    ' centre point. No projected'//
     :                    ' mode value will be generated.',STATUS)
         NUMDAT=0
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE HSB1_LINRE(X1,Y1,NUMDAT,STATUS,GRAD,CONS)
*+
*  Name:
*     HSB1_LINRE

*  Purpose:
*     Determines a least squares linear fit for data in arrays X and Y.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSB1_LINRE(X1,Y1,NUMDAT,STATUS,GRAD,CONS)

*  Description:
*     Uses a normal least squares method to determine the coefficients
*     of a linear fit to the contents of arrays X and Y.

*  Arguments:
*     X1(HSB__CHORM) = REAL (Given)
*        The x values to be used.
*     Y1(HSB__CHORM) = REAL (Given)
*        The y values to be used.
*     NUMDAT = INTEGER (Given)
*        The number of data (X/Y) pairs to be fitted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     GRAD   = REAL (Returned)
*        The gradient value for the linear fit.
*     CONS   = REAL (Returned)
*        The constant value for the linear fit.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-May-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'HSB_PAR'               ! HSUB system variables

*  Arguments Given:
      INTEGER NUMDAT                  ! Number of data points
      REAL X1(HSB__CHORM)             ! Data points X1 value
      REAL Y1(HSB__CHORM)             ! Data points Y1 value

*  Arguments Returned:
      REAL CONS                       ! Constant of linear equation
      REAL GRAD                       ! Gradient of linear equation

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      INTEGER I                       ! Loop variable
      REAL MNVX                       ! Mean value of X1 array
      REAL NUMB                       ! Number of data points
      REAL SUMX                       ! Sum of X1 array
      REAL SUMY                       ! Sum of Y1 array
      REAL TOT1                       ! Absolute X1 deviation from
                                      ! the mean
      REAL TOT2                       ! Absolute X1 deviation squared
                                      ! sum
      REAl TOT3                       ! Absolute X1 deviation
                                      ! times Y1

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

      NUMB=REAL(NUMDAT)

*   Find the mean value of x and the sums of the x and y arrays.
      SUMX=0.0
      SUMY=0.0
      MNVX=0.0
      DO 500 I=1,NUMDAT
         SUMX=SUMX+X1(I)
         SUMY=SUMY+Y1(I)
 500  CONTINUE
      MNVX=SUMX/NUMB

*   Calculate the squared sum of (x-xmean)
*   and thereby the gradient and constant terms in the equation.
      TOT1=0.0
      TOT2=0.0
      TOT3=0.0
      DO 510 I=1,NUMDAT
         TOT1=X1(I)-MNVX
         TOT2=TOT2+TOT1*TOT1
         TOT3=TOT3+TOT1*Y1(I)
 510  CONTINUE

*   Check that more than one value of pixel count was found.
      IF (ABS(TOT1).GT.HSB__VSMAL) THEN
         GRAD=TOT3/TOT2
         CONS=(SUMY-SUMX*GRAD)/NUMB
      ELSE
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','All the histogram chords had the same'//
     :                    ' centre point. No projected'//
     :                    ' mode value will be generated.',STATUS)
         NUMDAT=0
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE LOB1_LINRE(X1,Y1,NUMDAT,GRAD,CONS,STATUS)
*+
*  Name:
*     LOB1_LINRE

*  Purpose:
*     Determines a least squares linear fit for data in arrays X and Y.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LOB1_LINRE(X1,Y1,NUMDAT,GRAD,CONS,STATUS)

*  Description:
*     Uses a normal least squares method to determine the coefficients
*     of a linear fit to the contents of arrays X and Y.

*  Arguments:
*     X1(LOB__CHORM) = REAL (Given)
*        The x values to be used.
*     Y1(LOB__CHORM) = REAL (Given)
*        The y values to be used.
*     NUMDAT = INTEGER (Given)
*        The number of data (X/Y) pairs to be fitted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     GRAD   = REAL (Returned)
*        The gradient value for the linear fit.
*     CONS   = REAL (Returned)
*        The constant value for the linear fit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-May-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'LOB_PAR'               ! LOBACK system variables

*  Arguments Given:
      INTEGER NUMDAT                  ! Number of data points
      REAL X1(LOB__CHORM)             ! Data points X1 value
      REAL Y1(LOB__CHORM)             ! Data points Y1 value

*  Arguments Returned:
      REAL CONS                       ! Constant of linear equation
      REAL GRAD                       ! Gradient of linear equation

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      INTEGER I                       ! Loop variable
      REAL MNVX                       ! Mean value of X1 array
      REAL NUMB                       ! Number of data points
      REAL SUMX                       ! Sum of X1 array
      REAL SUMY                       ! Sum of Y1 array
      REAL TOT1                       ! Absolute X1 deviation from
                                      ! the mean
      REAL TOT2                       ! Absolute X1 deviation squared
                                      ! sum
      REAl TOT3                       ! Absolute X1 deviation
                                      ! times Y1

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

      NUMB=REAL(NUMDAT)

*   Find the mean value of x and the sums of the x and y arrays.
      SUMX=0.0
      SUMY=0.0
      MNVX=0.0
      DO 500 I=1,NUMDAT
         SUMX=SUMX+X1(I)
         SUMY=SUMY+Y1(I)
 500  CONTINUE
      MNVX=SUMX/NUMB

*   Calculate the squared sum of (x-xmean)
*   and thereby the gradient and constant terms in the equation.
      TOT1=0.0
      TOT2=0.0
      TOT3=0.0
      DO 510 I=1,NUMDAT
         TOT1=X1(I)-MNVX
         TOT2=TOT2+TOT1*TOT1
         TOT3=TOT3+TOT1*Y1(I)
 510  CONTINUE

*   Check that more than one value of pixel count was found.
      IF (ABS(TOT1).GT.LOB__VSMAL) THEN
         GRAD=TOT3/TOT2
         CONS=(SUMY-SUMX*GRAD)/NUMB
      ELSE
         CALL MSG_OUT(' ','WARNING!!!',STATUS)
         CALL MSG_OUT(' ','All the histogram chords had the same'//
     :                    ' centre point. No projected'//
     :                    ' mode value will be generated.',STATUS)
         NUMDAT=0
         GOTO 9999
      END IF

 9999 CONTINUE

      END


      SUBROUTINE SEC1_LINRE(LOR,HIR,NUMBER,SUMMAT,PSIZE,BACK,
     :                      NUMDAT,GRAD,CONS,COUNT,SLEN,STATUS)
*+
*  Name:
*     SEC1_LINRE

*  Purpose:
*     Determines a least squares linear fit for data in arrays X and Y.
*     This fit is used to calculate value for the scale length of the
*     object if it was a spiral galaxy or elliptical galaxy.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_LINRE(LOR,HIR,NUMBER,SUMMAT,PSIZE,BACK,
*                     NUMDAT,GRAD,CONS,COUNT,SLEN,STATUS)

*  Description:
*     Uses a normal least squares method to determine the coefficients
*     of a linear fit to the contents of arrays X and Y.

*  Arguments:
*     LOR = REAL (Given)
*        The radius below which a data point should not be included
*        in the linear regression. Units arc seconds.
*     HIR = REAL (Given)
*        The radius above which a data point should not be included
*        in the linear regression. Units arc seconds.
*     NUMBER(SEC__RESUL) = REAL (Given)
*        The number of pixels averaged at a given distance.
*     SUMMAT(SEC__RESUL) = REAL (Given)
*        The summation of pixels counts at a given radius. Units counts.
*     PSIZE = REAL (Given)
*        Image pixel size in arc secs.
*     BACK = REAL (Given)
*        Image background count value. Units counts.
*     NUMDAT = INTEGER (Given)
*        The number of data (X/Y) pairs to be fitted.
*     GRAD(2) = REAL (Returned)
*        The gradient values for the linear fit.
*     CONS(2) = REAL (Returned)
*        The constant values for the linear fit.
*     COUNT(2) = INTEGER (Returned)
*        The number of data points used during the linear regression.
*     SLEN(2) = REAL (Returned)
*        The scale lengths of the two fits i.e. spiral and elliptical.
*        Units arc seconds.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-Nov-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'SEC_PAR'               ! SECTOR constants

*  Arguments Given:
      INTEGER NUMDAT                  ! Number of data points
      REAL BACK                       ! The background count value
      REAL HIR                        ! The upper limit of radius requested
      REAL LOR                        ! The lower limit of radius requested
      REAL NUMBER(SEC__RESUL)         ! Number of pixels found at a given
                                      ! distance from the galaxy centre
      REAL PSIZE                      ! Size of the pixels in arc secs
      REAL SUMMAT(SEC__RESUL)         ! Summed counts for all pixels at a
                                      ! given radius

*  Arguments Returned:
      INTEGER COUNT(2)                ! Number of data points used
      REAL CONS(2)                    ! Constant of linear equation
      REAL GRAD(2)                    ! Gradient of linear equation
      REAL SLEN(2)                    ! Scale length of the galaxy

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      INTEGER FLAG                    ! Problems with the regresssion flag
      INTEGER I                       ! Loop variable
      INTEGER TYPE                    ! Defines the data transform type
      REAL MNVX                       ! Mean value of X1 array
      REAL NUMB                       ! Number of data points
      REAL SUMX                       ! Sum of X1 array
      REAL SUMY                       ! Sum of Y1 array
      REAL TOT1                       ! Absolute X1 deviation from
                                      ! the mean
      REAL TOT2                       ! Absolute X1 deviation squared
                                      ! sum
      REAL TOT3                       ! Absolute X1 deviation
                                      ! times Y1
      REAL X                          ! Transformed raw data radius value
      REAL Y                          ! Transformed raw data pixel count

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Find the mean value of x and the sums of the x and y arrays.

*   Set an error flag.
      FLAG=0

*   Calculate the linear regression for each of the gallaxy types.
      DO 200 TYPE=1,2

*      Setup the initial values for the sums and counter.
         SUMX=0.0
         SUMY=0.0
         MNVX=0.0
         COUNT(TYPE)=0

*      Loop through all the data points used.
         DO 500 I=1,NUMDAT

*         Ignore all values of radius where no data was collected.
           IF (NUMBER(I).GT.0.0) THEN

*            Ignore all radius values for which the mean pixel
*            value was below background.
               IF ((SUMMAT(I)/NUMBER(I)-BACK).GT.0.0) THEN

*               Calculate the radius.
                  X=(REAL(I)-1.)*PSIZE

*               Only use the value if it is within the requested limits.
                  IF ((X.GE.LOR).AND.(X.LE.HIR)) THEN

*                  Increment the points used counter.
                     COUNT(TYPE)=COUNT(TYPE)+1

*                 Convert radius to form required for spiral galaxy or
*                 for an elliptical galaxy.
                     IF (TYPE.EQ.1) THEN
                        X=(REAL(I)-1.)*PSIZE
                     ELSE
                        X=((REAL(I)-1.)*PSIZE)**(0.25)
                     END IF
                     Y=LOG10(SUMMAT(I)/NUMBER(I)-BACK)

*                  Sum the X and Y values found (radius and brightness).
                      SUMX=SUMX+X
                      SUMY=SUMY+Y

                  END IF

               END IF

            END IF

 500     CONTINUE

*      Raise an error flag since the number of data points was too low.
         IF (COUNT(TYPE).LT.2) THEN

            FLAG=1
            GOTO 200

         ELSE

*         Produce the mean value of X and a real representation of the number
*         of data points that contributed to the current mean.
            NUMB=REAL(COUNT(TYPE))
            MNVX=SUMX/NUMB

         END IF

*      Calculate the squared sum of (x-xmean)
*      and thereby the gradient and constant terms in the equation.

*      Set up the initial values for the sums.
         TOT1=0.0
         TOT2=0.0
         TOT3=0.0

*      Loop through all the data points used.
         DO 510 I=1,NUMDAT

*         Use only those radii where data was collected.
            IF (NUMBER(I).GT.0.0) THEN

*            Ignore a data point if the mean value was below sky.
               IF (SUMMAT(I)/NUMBER(I)-BACK.GT.0.0) THEN

*               Calculate the radius.
                  X=(REAL(I)-1.)*PSIZE

*               Check that the radius is within the range requested.
                  IF ((X.GE.LOR).AND.(X.LE.HIR)) THEN

*                  Generate the radius and brightness values.
                     IF (TYPE.EQ.1) THEN
                        X=(REAL(I)-1.)*PSIZE
                     ELSE
                        X=((REAL(I)-1.)*PSIZE)**(0.25)
                     END IF
                     Y=LOG10(SUMMAT(I)/NUMBER(I)-BACK)

*                  Calculate deviations from the mean.
                     TOT1=X-MNVX
                     TOT2=TOT2+TOT1*TOT1
                     TOT3=TOT3+TOT1*Y

                  END IF

               END IF

            END IF

 510     CONTINUE

*      Check that more than one value of pixel count was found.
         IF (ABS(TOT1).GT.SEC__VSMAL) THEN

*         Calculate the linear regression co-efficients.
            GRAD(TYPE)=TOT3/TOT2
            CONS(TYPE)=(SUMY-SUMX*GRAD(TYPE))/NUMB

*         Check that the gradient is physically reasonable.
            IF (GRAD(TYPE).LT.0.0) THEN

*            Calculate the scale length.
               IF ((TYPE.EQ.1).AND.(GRAD(TYPE).NE.0.0)) THEN
                  SLEN(TYPE)=-LOG10(EXP(1.0))/GRAD(TYPE)
               ELSE
                  SLEN(TYPE)=LOG10(EXP(1.0))/((-GRAD(TYPE))**(4.0))
               END IF
            ELSE

*            Set the error flag since the gradient was zero or greater.
               FLAG=3

            END IF

         ELSE

*      Raise an error flag if all the pixels had the same value.
            FLAG=2

         END IF

 200  CONTINUE

*   Display the reason that the linear regression failed.
      IF (FLAG.GT.0) THEN

         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','WARNING!!!',STATUS)

*      Too few data points found for a result to be calculated.
         IF (FLAG.EQ.1) THEN
            CALL MSG_OUT(' ','Not enough data points '//
     :                       'were selected.',STATUS)
         END IF

*      Not possible to calculate a meaningful slope.
         IF (FLAG.EQ.2) THEN
            CALL MSG_OUT(' ','The points selected had a '//
     :                       'single value.',STATUS)
         END IF

*      Negative or zero scale length.
         IF (FLAG.EQ.3) THEN
            CALL MSG_OUT(' ','The scale length calculated '//
     :                       'was not physically sensible.',STATUS)
         END IF

         CALL MSG_BLANK(STATUS)

      END IF

 9999 CONTINUE

      END
