*+  MATH_SPLFIT - Fits a spline to a 1-d array and outputs the coefficients.
	SUBROUTINE MATH_SPLFIT (NPTS, XIN, YIN, WEIGHT, XWORK,
     &            YWORK, WORK1, RWIDTH, N7, KNOT, COEFF, SS, STATUS)
*    Description :
*            This routine takes a weighted 1-d data series and fits it
*           with a cubic spline. The output coefficients can be fed into
*           the routine MATH_SPLREC which will take an array of X values
*           and produce the corresponding Y array.
*    Parameters :
*    Method :
*            The NAG routine E02BAF is used.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton (LTVAD::RDS)
*    History :
*     4 May 1988 Original (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS                 !i/o: Status return from the NAG routine
*    Local Constants :
      INTEGER MAXKNOT                ! Max no of knots used
        PARAMETER(MAXKNOT=500)
*    Import :
      INTEGER NPTS                   ! Number of points in X and Y arrays
      REAL XIN(NPTS)                 ! Array of X values
      REAL YIN(NPTS)                 ! Array of Y values
      DOUBLE PRECISION WEIGHT(NPTS)  ! Array of weights for each point.
      DOUBLE PRECISION XWORK(NPTS)   ! Work space to copy X aray to double prec.
      DOUBLE PRECISION YWORK(NPTS)   ! Work space to copy Y aray to double prec.
      DOUBLE PRECISION WORK1(NPTS)   ! Work space for NAG routine
      REAL RWIDTH                    ! Distance in X between knots
*    Export :
      INTEGER N7                     ! Number of intervals of the spline +7
      DOUBLE PRECISION KNOT(MAXKNOT) ! Array of knot positions (fn of X)
      DOUBLE PRECISION COEFF(MAXKNOT)! Array of spline coefficients
      INTEGER SS                     ! Sum of the residues (ie error)
*    Global variables :
*    Local variables :
      DOUBLE PRECISION WORK2(4,MAXKNOT)    ! Work space for E02BAF
      INTEGER NSECT                        ! Number of sections of the spline
      REAL DRANGE                          ! Range of X
      INTEGER N3                           ! Number of sections + 3
      INTEGER J,K                          ! Loop variable
      INTEGER LP
      REAL KINC                            ! Increment between knots
      INTEGER NKNOT                        ! Number of knots.
*    Local data :
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
* Convert input real arrays to double precision
*
      DO LP=1,NPTS
         XWORK(LP)=DBLE( XIN(LP) )
         YWORK(LP)=DBLE( YIN(LP) )
      ENDDO
*
* Calculate the number of knots and sections in the spline
*
      DRANGE=REAL(XWORK(NPTS)-XWORK(1))
      NKNOT = NINT( DRANGE / RWIDTH )
      NSECT=NKNOT+1
*
      N7 = NSECT + 7
      N3 = NSECT + 3
*
* Set up knots
* Knots must be inside the data points
*
*   Set extreme knots just inside the data set.
      KNOT(5)=XWORK(2)
      KNOT(N3)=XWORK(NPTS-1)
*
*   Space interior knots equidistantly
      KINC= (XWORK(NPTS-1)-XWORK(2)) / REAL(NKNOT-1)
*
      DO J=6,N3-1
         K = J-5
	 KNOT(J) = XWORK(2) + DBLE(K*KINC)
      ENDDO
*
* Do spline fit. Status should be set to zero anyway, but just make sure.
*
      STATUS = 0
      CALL E02BAF(NPTS,N7,XWORK,YWORK,WEIGHT,KNOT,WORK1,WORK2,
     &                                            COEFF,SS,STATUS)
*
* Check status and output appropriate messages.
*  This sadly doesn't work because NAG errors cause a Fortran STOP.
      IF (STATUS .EQ. 1) THEN
           CALL ERR_REP(' ',
     &     'MATH_SPLFIT: Knots are in wrong order or outside data',
     &      STATUS)
      ELSE IF (STATUS .EQ. 2) THEN
           CALL ERR_REP(' ',
     &     'MATH_SPLFIT: All the weights are not strictly positive',
     &      STATUS)
      ELSE IF (STATUS .EQ. 3) THEN
           CALL ERR_REP(' ',
     &     'MATH_SPLFIT: The X values are not in increasing order',
     &      STATUS)
      ELSE IF (STATUS .EQ. 4) THEN
           CALL ERR_REP(' ',
     &     'MATH_SPLFIT: Number of interior knots is negative',
     &      STATUS)
      ELSE IF (STATUS .EQ. 5) THEN
           CALL ERR_REP(' ',
     &     'MATH_SPLFIT: No unique solution',STATUS)
      ENDIF
*
      IF (STATUS .NE. SAI__OK) THEN
           CALL ERR_REP(' ','from MATH_SPLFIT',STATUS)
      ENDIF
*
      END
