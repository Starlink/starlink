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
*            The PDA routine PDA_DEFC is used.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton (LTVAD::RDS)
*     Richard Beard (Birmingham)
*    History :
*     4 May 1988 Original (LTVAD::RDS)
*     9 Jun 1997 COnvert to PDA (RB)
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
      DOUBLE PRECISION SD(1024)      ! Reciprocal of weights
      INTEGER SS                     ! Sum of the residues (ie error)
*    Global variables :
*    Local variables :
      INTEGER L
      PARAMETER (L = (500+6)*5 + (500+8)*5 + 2*(1024+500) + 500+7 + 16)
      DOUBLE PRECISION WORK2(L)  	   ! Work space for PDA_DEFC
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
      IF (NPTS .GT. 1024) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP('NPTS > 1024, SORRY...','from MATH_SPLFIT',STATUS)
         RETURN
      ENDIF
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
*   Set up the end point knots for PDA
      DO J=1,4
         KNOT(J) = XWORK(1)
         KNOT(N3+J) = XWORK(NPTS)
      END DO

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

      DO J=1,NPTS
         IF (WEIGHT(J) .NE. 0.0D0) THEN
            SD(J) = 1.0D0 / WEIGHT(J)
         ELSE
            SD(J) = 9.99D99
         END IF
      END DO
*
* Do spline fit. Status should be set to zero anyway, but just make sure.
*
      STATUS = 0
      CALL PDA_DEFC(NPTS,XWORK,YWORK,SD,4,N7,KNOT,1,J,COEFF,L,WORK2,
     &                                                        STATUS)
      SS = 0
*
      IF (STATUS .NE. SAI__OK .OR. J .NE. 1) THEN
           STATUS = SAI__ERROR
           CALL ERR_REP(' ','from MATH_SPLFIT',STATUS)
      ENDIF
*
      END
