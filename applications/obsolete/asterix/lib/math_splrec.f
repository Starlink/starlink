*+  MATH_SPLREC - Produces a 1-d array from spline coefficients.
	SUBROUTINE MATH_SPLREC (NPTS,XIN,N7,KNOT,COEFF,Y,NFAIL,STATUS)
*    Description :
*            This routine takes coefficients for a cubic spline, produced
*           by MATH_SPLFIT and calculates an array of points at X values
*           given by the input X array.
*             The XIN values must be inside the X values of the knots.
*           which means they must be inside the X values of the data used to
*           produce the spline in MATH_SPLFIT, otherwise the NAG routine will
*           return an IFAIL error. The number of points outside the range
*           of knots is returned
*    Parameters :
*    Method :
*            The PDA routine PDA_DBVALU is used.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton (LTVAD::RDS)
*     Richard Beard (Birmingham)
*    History :
*     4 May 1988 Original (LTVAD::RDS)
*     9 Jun 1997 Converted to PDA (RB)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status:
      INTEGER STATUS
*    Import :
      INTEGER MAXKNOT
      PARAMETER(MAXKNOT=500)
      INTEGER NPTS                   !Number of points in X array
      REAL XIN(NPTS)                 !Array of X values
      INTEGER N7                     !Number of intervals of the spline +7
      DOUBLE PRECISION KNOT(MAXKNOT) !Array of knot positions (fn of X)
      DOUBLE PRECISION COEFF(MAXKNOT)!Array of spline coefficients
*    Export :
      REAL Y(NPTS)                   !Array of Y values
*    Global variables :
      EXTERNAL PDA_DBVALU
      DOUBLE PRECISION PDA_DBVALU
*    Local Constants :
*    Local variables :
      INTEGER LP,INVB                ! Loop variable
      DOUBLE PRECISION X             ! X position to fit spline at
      DOUBLE PRECISION WORK(12)	     ! Workspace for PDA_DBVALU
      INTEGER NFAIL                  ! Number of failures of E02BB7
*    Local data :
*-
        IF (STATUS .NE. SAI__OK) RETURN
*
* Check for possible error with the knots
        IF (N7 .LT. 8) THEN
           CALL ERR_REP(' ','Not enough knots specified',STATUS)
           STATUS=SAI__ERROR
           GOTO 999
        ENDIF
*
        NFAIL=0
        INVB=1
*
        DO LP=1,NPTS
*
           X=DBLE( XIN(LP) )
*
* The NAG routine produces a Fortran STOP so check for possible errors now,
*  due to asking for X values outside the KNOT range.
*
           IF (X.LT.KNOT(4) .OR. X.GT.KNOT(N7-3)) THEN
               NFAIL=NFAIL+1
           ELSE
               Y(LP)=REAL(PDA_DBVALU(KNOT,COEFF,N7-4,4,0,X,INVB,WORK,
     :                                                          STATUS))
           ENDIF
*
	ENDDO
*
999     CONTINUE
        IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP(' ',' from MATH_SPLREC',STATUS)
        ENDIF
*
        END
