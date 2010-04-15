C
      SUBROUTINE GAUS_RECORD(N,GINFP,GINFH,GINFW,EW,RMS,MERR,
     :ORD,SIG,ERR,LU,FAULT)
C
C     G A U S _ R E C O R D
C
C     Records on a data file the results of the Gaussian
C     fitting. The Gaussians are ordered into increasing wavelength order
C
C     Parameters -  (">" input, "<" output )
C
C     (>) N       (Integer) Number of fitted Gaussians
C     (>) GINFP   (Real array) X values of gaussian peaks
C     (>) GINFH   (Real array) Height of Gaussian peak
C     (>) GINFW   (Real array) Width of Gaussians
C     (>) EW      (Real) Equivalent width of total profile
C     (>) RMS     (Real) R.m.s. on Gaussian fit
C     (>) MERR    (Real) Mean fractional error on fit in terms of error bars
C     (>) ORD     (Integer) Order of fit to continuum
C     (>) SIG     (Real) Multiple of sigma for continuum point rejection
C     (>) ERR     (Real) Multiple of error for continuum point rejection
C     (<) LU      (Integer) Logical unit of file to use
C     (<) FAULT   (Logical) True if an error occurs writing to the data file
C
C     Subroutines called:
C       SORT -  sorts profile index into increasing wavelength order
C
C                                            JRW / AAO  February 1987
C
C
C     Modified:
C
C     23 Aug 1995  HME / UoE, Starlink.  No longer use NAG. Hence
C                  instead of calling SORT, call GEN_QFISORT. That
C                  returns the index instead of the rank and hence the
C                  assignments GJNF*()=GINF*() need their array indices
C                  exchanged.
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER N,ORD,LU
      REAL GINFP(N),GINFH(N),GINFW(N),EW,RMS,MERR,SIG,ERR
      LOGICAL FAULT
C
C     Local parameters
C
      INTEGER I,ORDER(10),A1,STATUS
      REAL FLU,GJNFP(10),GJNFH(10),GJNFW(10)

      FAULT=.FALSE.
C
C     Order the Gaussians into increasing wavelength order
C
      IF (N.EQ.1) THEN
        ORDER(1)=1
        GO TO 3
      END IF

      CALL GEN_QFISORT(GINFP,N,ORDER)
3     DO I=1,N
       A1=ORDER(I)
       GJNFP(I)=GINFP(A1)
       GJNFH(I)=GINFH(A1)
       GJNFW(I)=GINFW(A1)
      END DO

      FLU=1.06484*GJNFH(1)*GJNFW(1)*2.3540
      WRITE(LU,5,ERR=90) GJNFP(1),GJNFH(1),GJNFW(1),FLU,EW,RMS,
     :MERR,ORD,SIG,ERR
5     FORMAT(1X,F12.3,1X,E14.4,1X,F10.4,1X,E12.4,1X,F10.3,1X,
     :E11.4,1X,F10.4,6X,I5,4X,F8.3,4X,F8.3)
      DO I=2,N
        FLU=1.06484*GJNFH(I)*GJNFW(I)*2.3540
        WRITE(LU,6,ERR=90) GJNFP(I),GJNFH(I),GJNFW(I),FLU
      END DO
6     FORMAT(1X,F12.3,1X,E14.4,1X,F10.4,1X,E12.4)
      GO TO 99

90    CALL PAR_WRUSER('Error writing to output file',STATUS)
      FAULT=.TRUE.

99    END
