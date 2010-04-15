C+
      SUBROUTINE FIG_NAGERR(IFAIL,ROUTINE)
C
C     F I G _ N A G E R R
C
C     Outputs an error message given a bad return code from
C     a NAG routine.  This is a Figaro utility, and the error
C     message is output using the PAR_WRUSER routine.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) IFAIL    (Integer) An error code returned by a NAG
C                  routine.  If IFAIL is zero (OK status), then
C                  FIG_NAGERR returns immediately.
C     (>) ROUTINE  (Character) The routine reporting the error. Need
C                  not be in upper case.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     ICH_LEN      (ICH_ package) Position of last non-blank char.
C     ICH_FOLD     ( "      "   ) Convert string to upper case.
C     PAR_WRUSER   (PAR_ package) Output message to user.
C
C     Note: 1) At present, this routine only supports a limited
C              number of NAG routines - those currently used
C              by Figaro.
C           2) Usually, the NAG routine in question should be
C              called with IFAIL set to 1, to suppress the normal
C              NAG error reporting (and the automatic abort on error!)
C
C                                         KS / AAO 25th March 1985
C     Modified:
C
C     2nd Sept 1986.  KS / AAO.  C06FAF, C06FCF, C06FJF and C06GCF added.
C     16th Jan 1987.  KS / AAO.  E04HBF, E04JBF, M01AAF added.
C     28th Jul 1993.  HME / UoE, Starlink.  Disuse STR$UPCASE.
C+
      IMPLICIT NONE
C
C     Paramters
C
      INTEGER IFAIL
      CHARACTER*(*) ROUTINE
C
C     Functions
C
      INTEGER ICH_LEN, ICH_FOLD
C
C     Local variables
C
      INTEGER DPT, EPT, IGNORE, STATUS
      CHARACTER DESCR*72, ERROR*80, UPNAME*6
C
C     Produce error message
C
      UPNAME=ROUTINE
      IGNORE=ICH_FOLD(UPNAME)
      IF (IFAIL.NE.0) THEN
         ERROR='NAG error, from '//UPNAME
         EPT=ICH_LEN(ERROR)+2
         WRITE (DESCR,'(A,I2,A)',IOSTAT=IGNORE) 'Code =',IFAIL,'. '
         DPT=ICH_LEN(DESCR)+3
C
C        C06FAF
C
         IF (UPNAME.EQ.'C06FAF') THEN
            ERROR(EPT:)='(Fourier transform of 1D real array)'
            IF (IFAIL.EQ.1) THEN
               DESCR(DPT:)=' At least 1 prime factor of N is > 19.'
            ELSE IF (IFAIL.EQ.2) THEN
               DESCR(DPT:)=' N has more than 20 prime factors.'
            ELSE IF (IFAIL.EQ.3) THEN
               DESCR(DPT:)=' Number of elements is <= 1.'
            ELSE
               DESCR(DPT:)=' Unexpected error code.'
            END IF
C
C        C06FCF
C
         ELSE IF (UPNAME.EQ.'C06FCF') THEN
            ERROR(EPT:)='(Fourier transform of 1D complex array)'
            IF (IFAIL.EQ.1) THEN
               DESCR(DPT:)=' At least 1 prime factor of N is > 19.'
            ELSE IF (IFAIL.EQ.2) THEN
               DESCR(DPT:)=' N has more than 20 prime factors.'
            ELSE IF (IFAIL.EQ.3) THEN
               DESCR(DPT:)=' Number of elements is <= 1.'
            ELSE
               DESCR(DPT:)=' Unexpected error code.'
            END IF
C
C        C06FJF
C
         ELSE IF (UPNAME.EQ.'C06FJF') THEN
            ERROR(EPT:)='(multi-dimensional complex FFT)'
            IF (IFAIL.EQ.1) THEN
               DESCR(DPT:)=' Number of dimensions is < 1.'
            ELSE IF (IFAIL.EQ.2) THEN
               DESCR(DPT:)=' # of elements does not match dimensions.'
            ELSE
               IF (MOD(IFAIL,10).EQ.1) THEN
                  DESCR(DPT:)=' A dimension has a prime factor > 19.'
               ELSE IF (MOD(IFAIL,10).EQ.2) THEN
                  DESCR(DPT:)=' A dimension has > 20 prime factors.'
               ELSE IF (MOD(IFAIL,10).EQ.3) THEN
                  DESCR(DPT:)=' A dimension is < 1.'
               ELSE IF (MOD(IFAIL,10).EQ.4) THEN
                  DESCR(DPT:)=' Work array is too small.'
               ELSE
                  DESCR(DPT:)=' Unexpected error code.'
               END IF
            END IF
C
C        C06GCF
C
         ELSE IF (UPNAME.EQ.'C06GCF') THEN
            ERROR(EPT:)='(forms complex conjugate of an array)'
            IF (IFAIL.EQ.1) THEN
               DESCR(DPT:)=' number of elements is <= 1.'
            ELSE
               DESCR(DPT:)=' Unexpected error code.'
            END IF
C
C        E01BAF
C
         ELSE IF (UPNAME.EQ.'E01BAF') THEN
            ERROR(EPT:)='(determines a cubic spline interpolant)'
            IF (IFAIL.EQ.1) THEN
               DESCR(DPT:)='Not enough points, or not enough workspace.'
            ELSE IF (IFAIL.EQ.2) THEN
               DESCR(DPT:)=' X values are not strictly increasing.'
            ELSE
               DESCR(DPT:)=' Unexpected error code'
            END IF
C
C        E02ADF
C
         ELSE IF (UPNAME.EQ.'E02ADF') THEN
            ERROR(EPT:)='(least-squares Chebyshev polynomial fit)'
            IF (IFAIL.EQ.1) THEN
               DESCR(DPT:)=' The weights are not all strictly positive.'
            ELSE IF (IFAIL.EQ.2) THEN
               DESCR(DPT:)=' X values are not in non-decreasing order.'
            ELSE IF (IFAIL.EQ.3) THEN
               DESCR(DPT:)=' All X values are the same.'
            ELSE IF (IFAIL.EQ.4) THEN
               DESCR(DPT:)=' Degree of fit specified is invalid.'
            ELSE IF (IFAIL.EQ.5) THEN
               DESCR(DPT:)=' Coefficient array is not large enough.'
            ELSE
               DESCR(DPT:)=' Unexpected error code.'
            END IF
C
C        E02AEF
C
         ELSE IF (UPNAME.EQ.'E02AEF') THEN
            ERROR(EPT:)='(evaluates a Chebyshev series polynomial)'
            IF (IFAIL.EQ.1) THEN
               DESCR(DPT:)=' X value is out of allowed range.'
            ELSE IF (IFAIL.EQ.2) THEN
               DESCR(DPT:)=' Number of terms in series is invalid.'
            ELSE
               DESCR(DPT:)=' Unexpected error code.'
            END IF
C
C        E02BBF
C
         ELSE IF (UPNAME.EQ.'E02BBF') THEN
            ERROR(EPT:)='(evaluates a cubic spline)'
            IF (IFAIL.EQ.1) THEN
               DESCR(DPT:)=' X value is out of allowed range.'
            ELSE IF (IFAIL.EQ.2) THEN
               DESCR(DPT:)=' Number of interior knots is negative.'
            ELSE
               DESCR(DPT:)=' Unexpected error code.'
            END IF
C
C        E04HBF
C
         ELSE IF (UPNAME.EQ.'E04HBF') THEN
            ERROR(EPT:)='(computes finite-difference intervals)'
            IF (IFAIL.LT.0) THEN
               DESCR(DPT:)=' IFLAG set to a negative value.'
            ELSE IF (IFAIL.EQ.1) THEN
               DESCR(DPT:)=' Parameter outside expected range.'
            ELSE IF (IFAIL.EQ.2) THEN
               DESCR(DPT:)=' Relative cancellation error > 0.1'
            ELSE
               DESCR(DPT:)=' Unexpected error code.'
            END IF
C
C        E04JBF
C
         ELSE IF (UPNAME.EQ.'E04JBF') THEN
            ERROR(EPT:)='(quasi-Newton minimisation)'
            IF (IFAIL.LT.0) THEN
               DESCR(DPT:)=' IFLAG set to a negative value.'
            ELSE IF (IFAIL.EQ.1) THEN
               DESCR(DPT:)=' Parameter outside expected range.'
            ELSE IF (IFAIL.EQ.2) THEN
               DESCR(DPT:)=' F(X) has no min, or MAXCAL too small.'
            ELSE IF (IFAIL.EQ.3) THEN
               DESCR(DPT:)=' Cannot converge to required accuracy.'
            ELSE IF (IFAIL.EQ.4) THEN
               DESCR(DPT:)=' Overflow would have occurred.'
            ELSE IF (IFAIL.EQ.5) THEN
               DESCR(DPT:)=' Function value not changing.'
            ELSE
               DESCR(DPT:)=' Unexpected error code.'
            END IF
C
C        M01AAF
C
         ELSE IF (UPNAME.EQ.'M01AAF') THEN
            ERROR(EPT:)='(ascending sort of pointer array)'
            IF (IFAIL.EQ.1) THEN
               DESCR(DPT:)=' Lower bound exceeds upper bound.'
            ELSE IF (IFAIL.EQ.2) THEN
               DESCR(DPT:)=' Lower bound less than 1.'
            ELSE IF (IFAIL.EQ.3) THEN
               DESCR(DPT:)=' Upper bound less than 1.'
            ELSE
               DESCR(DPT:)=' Unexpected error code.'
            END IF
C
         END IF
C
C        Output error messages
C
         CALL PAR_WRUSER(ERROR(:ICH_LEN(ERROR)),STATUS)
         CALL PAR_WRUSER(DESCR(:ICH_LEN(DESCR)),STATUS)
C
      END IF
C
      END
