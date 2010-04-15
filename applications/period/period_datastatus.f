
      SUBROUTINE PERIOD_DATASTATUS(IPARRAY, NDATA, MXCOL,
     :                             YERROR, MXSLOT, DATA)

C=============================================================================
C Routine to return information about the data currently stored by PERIOD.
C
C Essentially written by Vikram Singh Dhillon @Sussex 4-June-1991.
C
C Converted to Double Precision (KPD), August 2001
C Power-raising modified to use INTEGER power (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C=============================================================================

      IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

      INTEGER NDATA, MXCOL, MXSLOT
      DOUBLE PRECISION IPARRAY(NDATA, MXCOL)

C-----------------------------------------------------------------------------
C PERIOD_STATUS declarations.
C-----------------------------------------------------------------------------

      INTEGER I, N
      DOUBLE PRECISION DATA(NDATA), AVE, ADEV, SDEV, VAR
      LOGICAL YERROR


      WRITE (*, *)
     :          '** OK: Number of X,Y data pairs in file = ',
     :          NDATA
      WRITE (*, *) '** OK: Sample Y data = ', IPARRAY(1, 2),
     :             IPARRAY(NDATA/2, 2),
     :             IPARRAY(NDATA, 2)

      DO 10 I = 1, NDATA
         DATA(I) = IPARRAY(I, 2)
  10  CONTINUE

      CALL PERIOD_MOMENT(DATA, NDATA, AVE, ADEV, SDEV, VAR)

      WRITE (*, *) '** OK: Y data mean = ', AVE
      WRITE (*, *) '** OK: Y data standard deviation = ',
     :             SDEV
      WRITE (*, *) '** OK: Sample X data = ', IPARRAY(1, 1),
     :             IPARRAY(NDATA/2, 1),
     :             IPARRAY(NDATA, 1)
      WRITE (*, *) '** OK: Sample X intervals = ',
     :             IPARRAY(2, 1) - IPARRAY(1, 1),
     :             IPARRAY((NDATA/2)+1, 1)
     :             - IPARRAY(NDATA/2, 1),
     :             IPARRAY(NDATA, 1)
     :             - IPARRAY(NDATA-1, 1)

      DO 260 I = 1, NDATA - 1
         DATA(I) = DSQRT((IPARRAY(I+1, 1)-IPARRAY(I, 1))**2)
 260  CONTINUE

      N = NDATA - 1

      CALL PERIOD_MOMENT(DATA, N, AVE, ADEV, SDEV, VAR)

      WRITE (*, *) '** OK: X data interval mean = ', AVE
      WRITE (*, *)
     :           '** OK: X data interval standard deviation'
     :           // ' = ', SDEV
      IF ( YERROR ) THEN
         WRITE (*, *)
     :             '** OK: Errors on Y data points = .TRUE.'
         WRITE (*, *) '** OK: Sample Y errors = ',
     :                IPARRAY(1, 3),
     :                IPARRAY(NDATA/2, 3),
     :                IPARRAY(NDATA, 3)

         DO 262 I = 1, NDATA
            DATA(I) = IPARRAY(I, 3)
 262     CONTINUE

         CALL PERIOD_MOMENT(DATA, NDATA, AVE, ADEV, SDEV,
     :                      VAR)
         WRITE (*, *) '** OK: Y data error mean = ', AVE
         WRITE (*, *)
     :            '** OK: Y data error standard deviation ='
     :            , SDEV
      ELSE
         WRITE (*, *)
     :            '** OK: Errors on Y data points = .FALSE.'
      END IF

      RETURN
      END
