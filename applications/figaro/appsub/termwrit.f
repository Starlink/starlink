C
      SUBROUTINE TERMWRIT(N,GINFP,GINFH,GINFW,EW,RMS,ERRUSE,MERR,
     :XUNITS,ZUNITS,LALL)
C
C     T E R M W R I T
C
C     Writes to terminal parameters of fitted Gaussians
C
C     Parameters -  (">" input, "<" output )
C
C     (>) N       (Integer) Number of Gaussians fitted
C     (>) GINFP   (Real array) X values of Gaussian peak position
C     (>) GINFH   (Real array) Height of Gaussian peak
C     (>) GINFW   (Real array) Sigma of fitted Gaussian
C     (>) EW      (Real) Equivalent width of whole line
C     (>) RMS     (Real) R.m.s. on Gaussian fit to profile
C     (>) ERRUSE  (Logical) True if errors are avialable
C     (>) MERR    (Real) Mean error on fit in terms of error bars
C     (>) XUNITS  (Character) Units for X values
C     (>) ZUNITS  (Character) Units for Y values
C     (>) LALL    (Logical) True if all values of EW, rms etc are to be
C                 written to terminal
C                                          JRW / AAO February 1987
C
C     Modified:
C     20 Mar 1996  HME / UoE, Starlink.  Fixed broken string
C                  (the line just broke in the middle of the string,
C                  now two strings are concatenated.)
C     14 SEP 2000  ACD / UoE, Starlink.  Enhanced the format for the
C                  peak position to be able to cope with large and small
C                  values.  Also added the IOSTAT qualifier to internal
C                  writes (though the returned status is not checked)
C                  and removed an unused variable.
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER N
      REAL GINFP(N),GINFH(N),GINFW(N),EW,RMS,MERR
      CHARACTER*16 XUNITS,ZUNITS
      LOGICAL ERRUSE,LALL
C
C     Local variables
C
      INTEGER I,NEXT,INVOKE,STATUS,LSTAT
      INTEGER ICH_ENCODE
      REAL FLU
      CHARACTER*80 STRING

      STRING=' Index No.  Peak position     Peak height'//
     :   '        Sigma        Flux'
      CALL PAR_WRUSER(STRING,STATUS)
      CALL PAR_WRUSER('                      ',STATUS)
      DO I=1,N,1
        FLU=1.06484*GINFH(I)*GINFW(I)*2.3540
        IF (ABS(GINFP(I)) .LE. 1.0E6  .AND.
     :      ABS(GINFP(I)) .GE. 1.0E-1) THEN
          WRITE(STRING,10,IOSTAT=LSTAT)
     :      I,GINFP(I),GINFH(I),GINFW(I),FLU
10        FORMAT(2X,I4,5X,F12.3,4X,E14.4,1X,F12.3,1X,E14.4)
        ELSE
          WRITE(STRING,11,IOSTAT=LSTAT)
     :      I,GINFP(I),GINFH(I),GINFW(I),FLU
11        FORMAT(2X,I4,5X,E12.3,4X,E14.4,1X,F12.3,1X,E14.4)
        END IF
        CALL PAR_WRUSER(STRING,STATUS)
      END DO
      IF (.NOT.LALL) GO TO 99
      WRITE(STRING,12,IOSTAT=LSTAT) XUNITS,ZUNITS,XUNITS,ZUNITS,XUNITS
12    FORMAT(14X,A12,2X,A16,3X,A12,A10,'*',A10)
      CALL PAR_WRUSER(STRING,STATUS)
      CALL PAR_WRUSER('                     ',STATUS)
      STRING='              Equivalent width = '
      INVOKE=ICH_ENCODE(STRING,EW,33,10,NEXT)
      STRING(NEXT+1:)=XUNITS
      CALL PAR_WRUSER(STRING(:NEXT+20),STATUS)
      IF (ERRUSE) THEN
        STRING='               R.m.s. on fit = '
        INVOKE=ICH_ENCODE(STRING,RMS,33,10,NEXT)
        STRING(NEXT+1:)=ZUNITS
        CALL PAR_WRUSER(STRING(:NEXT+20),STATUS)
        STRING='Mean fractional error on fit = '
        INVOKE=ICH_ENCODE(STRING,MERR,33,10,NEXT)
        CALL PAR_WRUSER(STRING(:NEXT),STATUS)
      ELSE
        STRING='               R.m.s. on fit = '
        INVOKE=ICH_ENCODE(STRING,RMS,33,10,NEXT)
        STRING(NEXT+1:)=ZUNITS
        CALL PAR_WRUSER(STRING(:NEXT+20),STATUS)
      END IF

99    END
