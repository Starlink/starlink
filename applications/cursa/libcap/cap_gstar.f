      SUBROUTINE CAP_GSTAR (NUMROW, VALUES, SEQNO,
     :  MINVL, MINVLN,   MAXVL, MAXVLN,   RANGE, RANGEN,
     :  QUAR1, QUAR1N,   QUAR3, QUAR3N,   IQRNG, IQRNGN,
     :  MEDN,  MEDNN,    MEAN, MEANN,     MODE, MODEN,
     :  STDEV, STDEVN,   SKEW, SKEWN,     KURT,  KURTN,
     :  STATUS)
*+
*  Name:
*     CAP_GSTAR
*  Purpose:
*     Compute statistics on an array of values.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSTAR (NUMROW, VALUES; SEQNO;
*       MINVL, MINVLN,   MAXVL, MAXVLN,   RANGE, RANGEN,
*       QUAR1, QUAR1N,   QUAR3, QUAR3N,   IQRNG, IQRNGN,
*       MEDN,  MEDNN,    MEAN, MEANN,     MODE, MODEN,
*       STDEV, STDEVN,   SKEW, SKEWN,     KURT,  KURTN;
*       STATUS)
*  Description:
*     Compute statistics on an array of values.
*  Arguments:
*     NUMROW  =  INTEGER (Given)
*        Number of rows (or elements) in the array.
*     VALUES(NUMROW)  =  DOUBLE PRECISION (Given and Returned)
*        Array on which statistics are to be computed.
*     SEQNO(NUMROW  =  INTEGER (Used)
*        Array of sequence numbers for VALUES.
*     MINVL  =  DOUBLE PRECISION (Returned)
*        Minimum value.
*     MINVLN  =  LOGICAL (Returned)
*        Null value flag for the Minimum value.
*     MAXVL  =  DOUBLE PRECISION (Returned)
*        Maximum value.
*     MAXVLN  =  LOGICAL (Returned)
*        Null value flag for the Maximum value.
*     RANGE  =  DOUBLE PRECISION (Returned)
*        Total range of values (maximum - minimum).
*     RANGEN  =  LOGICAL (Returned)
*        Null value flag for the range of values.
*     QUAR1  =  DOUBLE PRECISION (Returned)
*        First quartile.
*     QUAR1N  =  LOGICAL (Returned)
*        Null value flag for the First quartile.
*     QUAR3  =  DOUBLE PRECISION (Returned)
*        Third quartile.
*     QUAR3N  =  LOGICAL (Returned)
*        Null value flag for the Third quartile.
*     IQRNG  =  DOUBLE PRECISION (Returned)
*        Interquartile range (third quartile - first quartile).
*     IQRNGN  =  LOGICAL (Returned)
*        Null value flag for the interquartile range.
*     MEDN  =  DOUBLE PRECISION (Returned)
*        Median.
*     MEDNN  =  LOGICAL (Returned)
*        Null value flag for the Median.
*     MEAN  =  DOUBLE PRECISION (Returned)
*        Mean.
*     MEANN  =  LOGICAL (Returned)
*        Null value flag for the Mean.
*     MODE  =  DOUBLE PRECISION (Returned)
*        Mode.
*     MODEN  =  LOGICAL (Returned)
*        Null value flag for the Mode.
*     STDEV  =  DOUBLE PRECISION (Returned)
*        Standard deviation.
*     STDEVN  =  LOGICAL (Returned)
*        Null value flag for the Standard deviation.
*     SKEW  =  DOUBLE PRECISION (Returned)
*        Skewness.
*     SKEWN  =  LOGICAL (Returned)
*        Null value flag for the Skewness.
*     KURT  =  DOUBLE PRECISION (Returned)
*        Kurtosis.
*     KURTN  =  LOGICAL (Returned)
*        Null value flag for the Kurtosis.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set all the null value flags to true.
*     If there are more than zero rows then
*       Sort the array.
*       Find the minimum and maximum values.
*       If there are more than five values then
*         Find the quartiles.
*       end if
*       Compute the sum.
*       Compute the mean.
*       Compute the mode.
*       If there are more than five values then
*         Compute the moments.
*         Compute the standard deviation, skewness and kurtosis.
*       end if
*     end if
*  References:
*     The algorithms used in routine are based on formulae given in:
*
*       'CRC Standard Mathematical Tables' 24th edition, W.H. Beyer (Ed),
*       1974 (CRC Press: Cleveland, Ohio) pp471-475.
*
*     The only exception is the formula for the Skewness, which is
*     taken from:
*
*       'Practical Statistics for Astronomers - I. Definitions, the
*       Normal Distribution, Detection of Signal', J.V. Wall, 1979,
*       Quart. J. Roy. Astron. Soc. v20, pp138-152.
*
*     (The definition of the skewness in Wall is the square of that in
*     the CRC tables).
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     28/11/96 (ACD): Original version.
*     4/12/96  (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
*  Arguments Given:
      INTEGER
     :  NUMROW
*  Arguments Given and Returned:
      DOUBLE PRECISION
     :  VALUES(NUMROW)
      INTEGER
     :  SEQNO(NUMROW)
*  Arguments Returned:
      DOUBLE PRECISION
     :  MINVL,
     :  MAXVL,
     :  RANGE,
     :  QUAR1,
     :  QUAR3,
     :  IQRNG,
     :  MEDN,
     :  MEAN,
     :  MODE,
     :  STDEV,
     :  SKEW,
     :  KURT
      LOGICAL
     :  MINVLN,
     :  MAXVLN,
     :  RANGEN,
     :  QUAR1N,
     :  QUAR3N,
     :  IQRNGN,
     :  MEDNN,
     :  MEANN,
     :  MODEN,
     :  STDEVN,
     :  SKEWN,
     :  KURTN
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Constants:
      DOUBLE PRECISION MINVAL     ! Minimum value for division.
      PARAMETER (MINVAL = 1.0D-10)
*  Local Variables:
      INTEGER
     :  CURROW,  ! Current row.
     :  Q1,      ! Index for the first  quartile.
     :  Q2,      !   "    "   "  second    "    .
     :  Q3       !   "    "   "  third     "    .
      DOUBLE PRECISION
     :  SUM,     ! Sum of values in the column.
     :  DIFF,    ! Difference between current point and the mean.
     :  M2,      ! Second moment.
     :  M3,      ! Third    "   .
     :  M4       ! Fourth   "   .
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Set all the null value flags to true.

         MINVLN = .TRUE.
         MAXVLN = .TRUE.
         RANGEN = .TRUE.

         QUAR1N = .TRUE.
         QUAR3N = .TRUE.
         IQRNGN = .TRUE.

         MEDNN = .TRUE.
         MEANN = .TRUE.
         MODEN = .TRUE.

         STDEVN = .TRUE.
         SKEWN = .TRUE.
         KURTN = .TRUE.

*
*       Check that there are some rows in the array.

         IF (NUMROW .GT. 0) THEN

*
*          Sort the array.

            DO CURROW = 1, NUMROW
               SEQNO(CURROW) = CURROW
            END DO

            CALL CAP_QSRTD (NUMROW, VALUES, SEQNO, STATUS)

*
*          Find the minimum and maximum values.

            MINVL = VALUES(SEQNO(1) )
            MINVLN = .FALSE.

            MAXVL = VALUES(SEQNO(NUMROW) )
            MAXVLN = .FALSE.

            IF (NUMROW .GT. 1) THEN
               RANGE = MAXVL - MINVL
               RANGEN = .FALSE.
            END IF

*
*          Find the quartiles if there are more than five values.

            IF (NUMROW .GT. 5) THEN
               Q1 = (NUMROW + 1) / 4
               Q3 = ((NUMROW + 1) * 3) / 4

               IF (MOD(NUMROW+1, 4) .EQ. 0) THEN
                  QUAR1 = VALUES(SEQNO(Q1) )
                  QUAR3 = VALUES(SEQNO(Q3) )
               ELSE
                  QUAR1 = (VALUES(SEQNO(Q1)) + VALUES(SEQNO(Q1+1))) / 2.0D0
                  QUAR3 = (VALUES(SEQNO(Q3)) + VALUES(SEQNO(Q3+1))) / 2.0D0
               END IF

               QUAR1N = .FALSE.
               QUAR3N = .FALSE.

               IQRNG = QUAR3 - QUAR1
               IQRNGN = .FALSE.

               Q2 = (NUMROW + 1) / 2

               IF (MOD(NUMROW+1, 2) .EQ. 0) THEN
                  MEDN = VALUES(SEQNO(Q2) )
               ELSE
                  MEDN = (VALUES(SEQNO(Q2)) + VALUES(SEQNO(Q2+1))) / 2.0D0
               END IF

               MEDNN = .FALSE.

            END IF

*
*          Compute the sum.

            SUM = 0.0D0

            DO CURROW = 1, NUMROW
               SUM = SUM + VALUES(CURROW)
            END DO

*
*          Compute the mean.

            MEAN = SUM / DBLE(NUMROW)
            MEANN = .FALSE.

*
*          Compute an approximation to the mode from the mean and
*          median.

            IF (.NOT. MEDNN) THEN
               MODE = MEAN - (3.0D0 * (MEAN - MEDN) )
               MODEN = .FALSE.
            END IF



*
*          If there are sufficient points then compute first the
*          moments and then the standard deviation, skewness and kurtosis.

            IF (NUMROW .GT. 5) THEN
               M2 = 0.0D0
               M3 = 0.0D0
               M4 = 0.0D0

               DO CURROW = 1, NUMROW
                  DIFF = VALUES(CURROW) - MEAN

                  M2 = M2 + (DIFF**2)
                  M3 = M3 + (DIFF**3)
                  M4 = M4 + (DIFF**4)
               END DO

               STDEV = SQRT(M2 / DBLE(NUMROW - 1) )
               STDEVN = .FALSE.

               M2 = M2 / DBLE(NUMROW)
               M3 = M3 / DBLE(NUMROW)
               M4 = M4 / DBLE(NUMROW)

               IF (M2 .GT. MINVAL) THEN
                  SKEW = (M3**2) / (M2**3)
                  SKEWN = .FALSE.

                  KURT = M4 / (M2**2)
                  KURTN = .FALSE.
               END IF
            END IF

         END IF

      END IF

      END
