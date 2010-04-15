*+
*  Name:
*     DSA_{FIBDUS}CSTR

*  Purpose:
*     Apply a scaling factor and offset to a data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_<X>CSTR( ELEMENTS, BSCALE, BZERO, BASE, ARRAY, ERRORS )

*  Description:
*     DSA_xCSTR covers the routines DSA_FCSTR, DSA_ICSTR, DSA_BCSTR,
*     DSA_DCSTR, DSA_UCSTR and DSA_SCSTR, which apply a scaling factor
*     and offset to data arrays of type float, integer, byte, double,
*     unsigned short and short respectively.  In each case the number
*     of artihmetic errors that result from the scaling is returned.

*  Arguments:
*     ELEMENTS = INTEGER (Given)
*        The number of elements in the array.
*     BSCALE = DOUBLE PRECISION (Given)
*        The scaling factor to be applied.
*     BZERO = DOUBLE PRECISION (Given)
*        The offset value to be applied.
*     BASE( ELEMENTS ) = INTEGER * 2 (Given)
*        The base array to be scaled.
*     ARRAY( ELEMENTS ) = <TYPE> (Returned)
*        The array to be scaled. The type varies with the routine. The
*        operation performed is
*        ARRAY(i) = ( BASE(i) * BSCALE ) + BZERO
*     ERRORS = INTEGER (Returned)
*        The number of errors (mostly overflows) resulting from the
*        application of the scale and zero values.

*  Algorithm:
*     The scaling is done in four steps:
*     (i)   Convert SHORT to DOUBLE.
*     (ii)  Scale without type conversion.
*     (iii) Apply offset without type conversion.
*     (iv)  Convert DOUBLE to <TYPE>.
*     Thus internally the scaling is done in double precision, the
*     conversion from any type to DOUBLE is (assumed to be) always
*     possible.
*
*     To avoid overflows in any of the stages, the BASE element is
*     checked against the minimum and maximum scaleable values SMIN and
*     SMAX. These in turn have to be worked out in advance. They depend
*     on BSCALE, BZERO, the range of numbers possible in <TYPE> and the
*     range of numbers possible in DOUBLE.
*
*     Basically, SMIN and SMAX are back-scaled VAL__MIN<T> and
*     VAL__MAX<T>. However during this back-scaling overflows can also
*     occur, and care must be taken. The advantage of the back-scaling
*     is that it has to be applied only for two numbers, not for each
*     element in BASE.
*
*     The back-scaling also splits into three steps:
*     (a) Convert VAL__MIN<T>, VAL__MAX<T> to DOUBLE. This is trivial.
*     (b) Apply the offset. Care must be taken because
*         DBLE(VAL__M%%<T>)-BZERO might overflow. If BZERO is positive
*         (negative) we must first get VAL__MIND+BZERO (VAL__MAXD+BZERO)
*         and compare it with DBLE(VAL__M%%<T>).
*     (c) Scale. Care must be taken because 1/BSCALE might be a division
*         by zero. Also if |BSCALE|<1 division by BSCALE might overflow.
*         And if BSCALE<0, minimum and maximum are exchanged.
*     (d) Do not convert to SHORT! While looping through BASE, each
*         element can be converted to DOUBLE without problem.
*         DBLE(BASE(i)) is compared to SMIN and SMAX.

*  Notes:
*     This is a generic routine. But since Figaro's type names are
*     different from Starlink's, some care must be taken when
*     substituting <X>, <T> and <TYPE>. The following is a complete list
*     for this routine:
*               <X> <T>  <TYPE>
*     DSA_FCSTR  F   R    REAL
*     DSA_ICSTR  I   I    INTEGER
*     DSA_BCSTR  B   B    BYTE
*     DSA_DCSTR  D   D    DOUBLE PRECISION
*     DSA_UCSTR  U   UW   INTEGER * 2
*     DSA_SCSTR  S   W    INTEGER * 2

*  Authors:
*     KS: Keith Shortridge (AAO)
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     03-JUL-1987 (KS):
*        Original version.
*     25-APR-1989 (KS):
*        Support for USHORT type added.
*     24-AUG-1992 (HME):
*        Re-written to avoid platform-dependent error handler.
*     31-AUG-1992 (KS):
*        INCLUDE file syntax modified for use on VAX systems.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      SUBROUTINE DSA_FCSTR( ELEMENTS, BSCALE, BZERO,
     :   BASE, ARRAY, ERRORS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! Range of valid numbers

*  Arguments Given:
      INTEGER ELEMENTS
      DOUBLE PRECISION BSCALE
      DOUBLE PRECISION BZERO
      INTEGER * 2 BASE( ELEMENTS )

*  Arguments Returned:
      REAL ARRAY( ELEMENTS )
      INTEGER ERRORS

*  Local Variables:
      INTEGER I
      DOUBLE PRECISION SMIN
      DOUBLE PRECISION SMAX
      DOUBLE PRECISION TEST

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Convert the smallest and largest possible numbers of type <TYPE> into
*  double precision.
      SMIN = NUM_RTOD( VAL__MINR )
      SMAX = NUM_RTOD( VAL__MAXR )


*  Apply BZERO offset to SMIN, SMAX.
*  We would like to subtract BZERO from both, but must be wary of
*  overflows.

*  If BZERO negative.
      IF ( BZERO .LT. 0D0 ) THEN

*     Subtraction might overflow beyond VAL__MAXD. So we first find
*     VAL__MAXD+BZERO.
         TEST = VAL__MAXD + BZERO
         SMIN = MIN( SMIN, TEST )
         SMAX = MIN( SMAX, TEST )

*  Else if BZERO positive.
      ELSE IF ( BZERO .GT. 0D0 ) THEN

*     Subtraction might overflow beyond VAL__MIND. So we first find
*     VAL__MIND+BZERO.
         TEST = VAL__MIND + BZERO
         SMIN = MAX( SMIN, TEST )
         SMAX = MAX( SMAX, TEST )
      END IF

*  Now we can subtract BZERO.
      SMIN = SMIN - BZERO
      SMAX = SMAX - BZERO


*  Apply BSCALE to SMIN, SMAX.
*  We would like to divide both by BSCALE, but must be wary of division
*  by zero, overflows, and inversion (SMIN>SMAX).

*  If BSCALE is negative.
      IF ( BSCALE .LT. 0D0 ) THEN

*     If BSCALE is between 0 and -1.
         IF ( BSCALE .GT. -1D0 ) THEN

*        Division by BSCALE might overflow. So we first get
*        VAL_M%%D*BSCALE. (Note that BSCALE is negative and minimum and
*        maximum exchanged.)
            TEST = VAL__MAXD * BSCALE
            SMIN = MAX( SMIN, TEST )
            TEST = VAL__MIND * BSCALE
            SMAX = MIN( SMAX, TEST )
         END IF

*     Now we can divide by BSCALE. (Note that BSCALE is negative and
*     minimum and maximum exchanged.)
         SMIN = SMIN / BSCALE
         SMAX = SMAX / BSCALE
         TEST = SMIN
         SMIN = SMAX
         SMAX = TEST

*  Else if BSCALE is positive.
      ELSE IF ( BSCALE .GT. 0D0 ) THEN

*     If BSCALE is between 0 and 1.
         IF ( BSCALE .LT. 1D0 ) THEN

*        Division by BSCALE might overflow. So we first get
*        VAL_M%%D*BSCALE.
            TEST = VAL__MIND * BSCALE
            SMIN = MAX( SMIN, TEST )
            TEST = VAL__MAXD * BSCALE
            SMAX = MIN( SMAX, TEST )
         END IF

*     Now we can divide by BSCALE.
         SMIN = SMIN / BSCALE
         SMAX = SMAX / BSCALE

*  Else (BSCALE is zero).
      ELSE

*     We have already applied BZERO to SMIN and SMAX. And we know that
*     all BASE(i) * SCALE are zero. The issue here is then whether
*     zero is in the interval [SMIN,SMAX]. If not, we know that each
*     element of BASE causes an overflow. We can set ERRORS=ELEMENTS and
*     return without further action.
         IF ( SMIN .GT. 0D0 .OR. SMAX .LT. 0D0 ) THEN
            ERRORS = ELEMENTS
            GO TO 500
         END IF
      END IF


*  SMIN and SMAX are now directly comparable to DBLE(BASE(i)). We can go
*  about scaling each element.

      ERRORS = 0
      DO 1 I = 1, ELEMENTS
         TEST = NUM_WTOD( BASE(I) )
         IF ( TEST .GE. SMIN .AND. TEST .LE. SMAX ) THEN
            TEST = TEST * BSCALE + BZERO
            ARRAY(I) = NUM_DTOR( TEST )
         ELSE
            ERRORS = ERRORS + 1
         END IF
 1    CONTINUE


*  Return.
 500  CONTINUE
      END

      SUBROUTINE DSA_ICSTR( ELEMENTS, BSCALE, BZERO,
     :   BASE, ARRAY, ERRORS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! Range of valid numbers

*  Arguments Given:
      INTEGER ELEMENTS
      DOUBLE PRECISION BSCALE
      DOUBLE PRECISION BZERO
      INTEGER * 2 BASE( ELEMENTS )

*  Arguments Returned:
      INTEGER ARRAY( ELEMENTS )
      INTEGER ERRORS

*  Local Variables:
      INTEGER I
      DOUBLE PRECISION SMIN
      DOUBLE PRECISION SMAX
      DOUBLE PRECISION TEST

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Convert the smallest and largest possible numbers of type <TYPE> into
*  double precision.
      SMIN = NUM_ITOD( VAL__MINI )
      SMAX = NUM_ITOD( VAL__MAXI )


*  Apply BZERO offset to SMIN, SMAX.
*  We would like to subtract BZERO from both, but must be wary of
*  overflows.

*  If BZERO negative.
      IF ( BZERO .LT. 0D0 ) THEN

*     Subtraction might overflow beyond VAL__MAXD. So we first find
*     VAL__MAXD+BZERO.
         TEST = VAL__MAXD + BZERO
         SMIN = MIN( SMIN, TEST )
         SMAX = MIN( SMAX, TEST )

*  Else if BZERO positive.
      ELSE IF ( BZERO .GT. 0D0 ) THEN

*     Subtraction might overflow beyond VAL__MIND. So we first find
*     VAL__MIND+BZERO.
         TEST = VAL__MIND + BZERO
         SMIN = MAX( SMIN, TEST )
         SMAX = MAX( SMAX, TEST )
      END IF

*  Now we can subtract BZERO.
      SMIN = SMIN - BZERO
      SMAX = SMAX - BZERO


*  Apply BSCALE to SMIN, SMAX.
*  We would like to divide both by BSCALE, but must be wary of division
*  by zero, overflows, and inversion (SMIN>SMAX).

*  If BSCALE is negative.
      IF ( BSCALE .LT. 0D0 ) THEN

*     If BSCALE is between 0 and -1.
         IF ( BSCALE .GT. -1D0 ) THEN

*        Division by BSCALE might overflow. So we first get
*        VAL_M%%D*BSCALE. (Note that BSCALE is negative and minimum and
*        maximum exchanged.)
            TEST = VAL__MAXD * BSCALE
            SMIN = MAX( SMIN, TEST )
            TEST = VAL__MIND * BSCALE
            SMAX = MIN( SMAX, TEST )
         END IF

*     Now we can divide by BSCALE. (Note that BSCALE is negative and
*     minimum and maximum exchanged.)
         SMIN = SMIN / BSCALE
         SMAX = SMAX / BSCALE
         TEST = SMIN
         SMIN = SMAX
         SMAX = TEST

*  Else if BSCALE is positive.
      ELSE IF ( BSCALE .GT. 0D0 ) THEN

*     If BSCALE is between 0 and 1.
         IF ( BSCALE .LT. 1D0 ) THEN

*        Division by BSCALE might overflow. So we first get
*        VAL_M%%D*BSCALE.
            TEST = VAL__MIND * BSCALE
            SMIN = MAX( SMIN, TEST )
            TEST = VAL__MAXD * BSCALE
            SMAX = MIN( SMAX, TEST )
         END IF

*     Now we can divide by BSCALE.
         SMIN = SMIN / BSCALE
         SMAX = SMAX / BSCALE

*  Else (BSCALE is zero).
      ELSE

*     We have already applied BZERO to SMIN and SMAX. And we know that
*     all BASE(i) * SCALE are zero. The issue here is then whether
*     zero is in the interval [SMIN,SMAX]. If not, we know that each
*     element of BASE causes an overflow. We can set ERRORS=ELEMENTS and
*     return without further action.
         IF ( SMIN .GT. 0D0 .OR. SMAX .LT. 0D0 ) THEN
            ERRORS = ELEMENTS
            GO TO 500
         END IF
      END IF


*  SMIN and SMAX are now directly comparable to DBLE(BASE(i)). We can go
*  about scaling each element.

      ERRORS = 0
      DO 1 I = 1, ELEMENTS
         TEST = NUM_WTOD( BASE(I) )
         IF ( TEST .GE. SMIN .AND. TEST .LE. SMAX ) THEN
            TEST = TEST * BSCALE + BZERO
            ARRAY(I) = NUM_DTOI( TEST )
         ELSE
            ERRORS = ERRORS + 1
         END IF
 1    CONTINUE


*  Return.
 500  CONTINUE
      END

      SUBROUTINE DSA_BCSTR( ELEMENTS, BSCALE, BZERO,
     :   BASE, ARRAY, ERRORS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! Range of valid numbers

*  Arguments Given:
      INTEGER ELEMENTS
      DOUBLE PRECISION BSCALE
      DOUBLE PRECISION BZERO
      INTEGER * 2 BASE( ELEMENTS )

*  Arguments Returned:
      BYTE ARRAY( ELEMENTS )
      INTEGER ERRORS

*  Local Variables:
      INTEGER I
      DOUBLE PRECISION SMIN
      DOUBLE PRECISION SMAX
      DOUBLE PRECISION TEST

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Convert the smallest and largest possible numbers of type <TYPE> into
*  double precision.
      SMIN = NUM_BTOD( VAL__MINB )
      SMAX = NUM_BTOD( VAL__MAXB )


*  Apply BZERO offset to SMIN, SMAX.
*  We would like to subtract BZERO from both, but must be wary of
*  overflows.

*  If BZERO negative.
      IF ( BZERO .LT. 0D0 ) THEN

*     Subtraction might overflow beyond VAL__MAXD. So we first find
*     VAL__MAXD+BZERO.
         TEST = VAL__MAXD + BZERO
         SMIN = MIN( SMIN, TEST )
         SMAX = MIN( SMAX, TEST )

*  Else if BZERO positive.
      ELSE IF ( BZERO .GT. 0D0 ) THEN

*     Subtraction might overflow beyond VAL__MIND. So we first find
*     VAL__MIND+BZERO.
         TEST = VAL__MIND + BZERO
         SMIN = MAX( SMIN, TEST )
         SMAX = MAX( SMAX, TEST )
      END IF

*  Now we can subtract BZERO.
      SMIN = SMIN - BZERO
      SMAX = SMAX - BZERO


*  Apply BSCALE to SMIN, SMAX.
*  We would like to divide both by BSCALE, but must be wary of division
*  by zero, overflows, and inversion (SMIN>SMAX).

*  If BSCALE is negative.
      IF ( BSCALE .LT. 0D0 ) THEN

*     If BSCALE is between 0 and -1.
         IF ( BSCALE .GT. -1D0 ) THEN

*        Division by BSCALE might overflow. So we first get
*        VAL_M%%D*BSCALE. (Note that BSCALE is negative and minimum and
*        maximum exchanged.)
            TEST = VAL__MAXD * BSCALE
            SMIN = MAX( SMIN, TEST )
            TEST = VAL__MIND * BSCALE
            SMAX = MIN( SMAX, TEST )
         END IF

*     Now we can divide by BSCALE. (Note that BSCALE is negative and
*     minimum and maximum exchanged.)
         SMIN = SMIN / BSCALE
         SMAX = SMAX / BSCALE
         TEST = SMIN
         SMIN = SMAX
         SMAX = TEST

*  Else if BSCALE is positive.
      ELSE IF ( BSCALE .GT. 0D0 ) THEN

*     If BSCALE is between 0 and 1.
         IF ( BSCALE .LT. 1D0 ) THEN

*        Division by BSCALE might overflow. So we first get
*        VAL_M%%D*BSCALE.
            TEST = VAL__MIND * BSCALE
            SMIN = MAX( SMIN, TEST )
            TEST = VAL__MAXD * BSCALE
            SMAX = MIN( SMAX, TEST )
         END IF

*     Now we can divide by BSCALE.
         SMIN = SMIN / BSCALE
         SMAX = SMAX / BSCALE

*  Else (BSCALE is zero).
      ELSE

*     We have already applied BZERO to SMIN and SMAX. And we know that
*     all BASE(i) * SCALE are zero. The issue here is then whether
*     zero is in the interval [SMIN,SMAX]. If not, we know that each
*     element of BASE causes an overflow. We can set ERRORS=ELEMENTS and
*     return without further action.
         IF ( SMIN .GT. 0D0 .OR. SMAX .LT. 0D0 ) THEN
            ERRORS = ELEMENTS
            GO TO 500
         END IF
      END IF


*  SMIN and SMAX are now directly comparable to DBLE(BASE(i)). We can go
*  about scaling each element.

      ERRORS = 0
      DO 1 I = 1, ELEMENTS
         TEST = NUM_WTOD( BASE(I) )
         IF ( TEST .GE. SMIN .AND. TEST .LE. SMAX ) THEN
            TEST = TEST * BSCALE + BZERO
            ARRAY(I) = NUM_DTOB( TEST )
         ELSE
            ERRORS = ERRORS + 1
         END IF
 1    CONTINUE


*  Return.
 500  CONTINUE
      END

      SUBROUTINE DSA_DCSTR( ELEMENTS, BSCALE, BZERO,
     :   BASE, ARRAY, ERRORS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! Range of valid numbers

*  Arguments Given:
      INTEGER ELEMENTS
      DOUBLE PRECISION BSCALE
      DOUBLE PRECISION BZERO
      INTEGER * 2 BASE( ELEMENTS )

*  Arguments Returned:
      DOUBLE PRECISION ARRAY( ELEMENTS )
      INTEGER ERRORS

*  Local Variables:
      INTEGER I
      DOUBLE PRECISION SMIN
      DOUBLE PRECISION SMAX
      DOUBLE PRECISION TEST

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Convert the smallest and largest possible numbers of type <TYPE> into
*  double precision.
      SMIN = NUM_DTOD( VAL__MIND )
      SMAX = NUM_DTOD( VAL__MAXD )


*  Apply BZERO offset to SMIN, SMAX.
*  We would like to subtract BZERO from both, but must be wary of
*  overflows.

*  If BZERO negative.
      IF ( BZERO .LT. 0D0 ) THEN

*     Subtraction might overflow beyond VAL__MAXD. So we first find
*     VAL__MAXD+BZERO.
         TEST = VAL__MAXD + BZERO
         SMIN = MIN( SMIN, TEST )
         SMAX = MIN( SMAX, TEST )

*  Else if BZERO positive.
      ELSE IF ( BZERO .GT. 0D0 ) THEN

*     Subtraction might overflow beyond VAL__MIND. So we first find
*     VAL__MIND+BZERO.
         TEST = VAL__MIND + BZERO
         SMIN = MAX( SMIN, TEST )
         SMAX = MAX( SMAX, TEST )
      END IF

*  Now we can subtract BZERO.
      SMIN = SMIN - BZERO
      SMAX = SMAX - BZERO


*  Apply BSCALE to SMIN, SMAX.
*  We would like to divide both by BSCALE, but must be wary of division
*  by zero, overflows, and inversion (SMIN>SMAX).

*  If BSCALE is negative.
      IF ( BSCALE .LT. 0D0 ) THEN

*     If BSCALE is between 0 and -1.
         IF ( BSCALE .GT. -1D0 ) THEN

*        Division by BSCALE might overflow. So we first get
*        VAL_M%%D*BSCALE. (Note that BSCALE is negative and minimum and
*        maximum exchanged.)
            TEST = VAL__MAXD * BSCALE
            SMIN = MAX( SMIN, TEST )
            TEST = VAL__MIND * BSCALE
            SMAX = MIN( SMAX, TEST )
         END IF

*     Now we can divide by BSCALE. (Note that BSCALE is negative and
*     minimum and maximum exchanged.)
         SMIN = SMIN / BSCALE
         SMAX = SMAX / BSCALE
         TEST = SMIN
         SMIN = SMAX
         SMAX = TEST

*  Else if BSCALE is positive.
      ELSE IF ( BSCALE .GT. 0D0 ) THEN

*     If BSCALE is between 0 and 1.
         IF ( BSCALE .LT. 1D0 ) THEN

*        Division by BSCALE might overflow. So we first get
*        VAL_M%%D*BSCALE.
            TEST = VAL__MIND * BSCALE
            SMIN = MAX( SMIN, TEST )
            TEST = VAL__MAXD * BSCALE
            SMAX = MIN( SMAX, TEST )
         END IF

*     Now we can divide by BSCALE.
         SMIN = SMIN / BSCALE
         SMAX = SMAX / BSCALE

*  Else (BSCALE is zero).
      ELSE

*     We have already applied BZERO to SMIN and SMAX. And we know that
*     all BASE(i) * SCALE are zero. The issue here is then whether
*     zero is in the interval [SMIN,SMAX]. If not, we know that each
*     element of BASE causes an overflow. We can set ERRORS=ELEMENTS and
*     return without further action.
         IF ( SMIN .GT. 0D0 .OR. SMAX .LT. 0D0 ) THEN
            ERRORS = ELEMENTS
            GO TO 500
         END IF
      END IF


*  SMIN and SMAX are now directly comparable to DBLE(BASE(i)). We can go
*  about scaling each element.

      ERRORS = 0
      DO 1 I = 1, ELEMENTS
         TEST = NUM_WTOD( BASE(I) )
         IF ( TEST .GE. SMIN .AND. TEST .LE. SMAX ) THEN
            TEST = TEST * BSCALE + BZERO
            ARRAY(I) = NUM_DTOD( TEST )
         ELSE
            ERRORS = ERRORS + 1
         END IF
 1    CONTINUE


*  Return.
 500  CONTINUE
      END

      SUBROUTINE DSA_UCSTR( ELEMENTS, BSCALE, BZERO,
     :   BASE, ARRAY, ERRORS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! Range of valid numbers

*  Arguments Given:
      INTEGER ELEMENTS
      DOUBLE PRECISION BSCALE
      DOUBLE PRECISION BZERO
      INTEGER * 2 BASE( ELEMENTS )

*  Arguments Returned:
      INTEGER * 2 ARRAY( ELEMENTS )
      INTEGER ERRORS

*  Local Variables:
      INTEGER I
      DOUBLE PRECISION SMIN
      DOUBLE PRECISION SMAX
      DOUBLE PRECISION TEST

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Convert the smallest and largest possible numbers of type <TYPE> into
*  double precision.
      SMIN = NUM_UWTOD( VAL__MINUW )
      SMAX = NUM_UWTOD( VAL__MAXUW )


*  Apply BZERO offset to SMIN, SMAX.
*  We would like to subtract BZERO from both, but must be wary of
*  overflows.

*  If BZERO negative.
      IF ( BZERO .LT. 0D0 ) THEN

*     Subtraction might overflow beyond VAL__MAXD. So we first find
*     VAL__MAXD+BZERO.
         TEST = VAL__MAXD + BZERO
         SMIN = MIN( SMIN, TEST )
         SMAX = MIN( SMAX, TEST )

*  Else if BZERO positive.
      ELSE IF ( BZERO .GT. 0D0 ) THEN

*     Subtraction might overflow beyond VAL__MIND. So we first find
*     VAL__MIND+BZERO.
         TEST = VAL__MIND + BZERO
         SMIN = MAX( SMIN, TEST )
         SMAX = MAX( SMAX, TEST )
      END IF

*  Now we can subtract BZERO.
      SMIN = SMIN - BZERO
      SMAX = SMAX - BZERO


*  Apply BSCALE to SMIN, SMAX.
*  We would like to divide both by BSCALE, but must be wary of division
*  by zero, overflows, and inversion (SMIN>SMAX).

*  If BSCALE is negative.
      IF ( BSCALE .LT. 0D0 ) THEN

*     If BSCALE is between 0 and -1.
         IF ( BSCALE .GT. -1D0 ) THEN

*        Division by BSCALE might overflow. So we first get
*        VAL_M%%D*BSCALE. (Note that BSCALE is negative and minimum and
*        maximum exchanged.)
            TEST = VAL__MAXD * BSCALE
            SMIN = MAX( SMIN, TEST )
            TEST = VAL__MIND * BSCALE
            SMAX = MIN( SMAX, TEST )
         END IF

*     Now we can divide by BSCALE. (Note that BSCALE is negative and
*     minimum and maximum exchanged.)
         SMIN = SMIN / BSCALE
         SMAX = SMAX / BSCALE
         TEST = SMIN
         SMIN = SMAX
         SMAX = TEST

*  Else if BSCALE is positive.
      ELSE IF ( BSCALE .GT. 0D0 ) THEN

*     If BSCALE is between 0 and 1.
         IF ( BSCALE .LT. 1D0 ) THEN

*        Division by BSCALE might overflow. So we first get
*        VAL_M%%D*BSCALE.
            TEST = VAL__MIND * BSCALE
            SMIN = MAX( SMIN, TEST )
            TEST = VAL__MAXD * BSCALE
            SMAX = MIN( SMAX, TEST )
         END IF

*     Now we can divide by BSCALE.
         SMIN = SMIN / BSCALE
         SMAX = SMAX / BSCALE

*  Else (BSCALE is zero).
      ELSE

*     We have already applied BZERO to SMIN and SMAX. And we know that
*     all BASE(i) * SCALE are zero. The issue here is then whether
*     zero is in the interval [SMIN,SMAX]. If not, we know that each
*     element of BASE causes an overflow. We can set ERRORS=ELEMENTS and
*     return without further action.
         IF ( SMIN .GT. 0D0 .OR. SMAX .LT. 0D0 ) THEN
            ERRORS = ELEMENTS
            GO TO 500
         END IF
      END IF


*  SMIN and SMAX are now directly comparable to DBLE(BASE(i)). We can go
*  about scaling each element.

      ERRORS = 0
      DO 1 I = 1, ELEMENTS
         TEST = NUM_WTOD( BASE(I) )
         IF ( TEST .GE. SMIN .AND. TEST .LE. SMAX ) THEN
            TEST = TEST * BSCALE + BZERO
            ARRAY(I) = NUM_DTOUW( TEST )
         ELSE
            ERRORS = ERRORS + 1
         END IF
 1    CONTINUE


*  Return.
 500  CONTINUE
      END

      SUBROUTINE DSA_SCSTR( ELEMENTS, BSCALE, BZERO,
     :   BASE, ARRAY, ERRORS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! Range of valid numbers

*  Arguments Given:
      INTEGER ELEMENTS
      DOUBLE PRECISION BSCALE
      DOUBLE PRECISION BZERO
      INTEGER * 2 BASE( ELEMENTS )

*  Arguments Returned:
      INTEGER * 2 ARRAY( ELEMENTS )
      INTEGER ERRORS

*  Local Variables:
      INTEGER I
      DOUBLE PRECISION SMIN
      DOUBLE PRECISION SMAX
      DOUBLE PRECISION TEST

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Convert the smallest and largest possible numbers of type <TYPE> into
*  double precision.
      SMIN = NUM_WTOD( VAL__MINW )
      SMAX = NUM_WTOD( VAL__MAXW )


*  Apply BZERO offset to SMIN, SMAX.
*  We would like to subtract BZERO from both, but must be wary of
*  overflows.

*  If BZERO negative.
      IF ( BZERO .LT. 0D0 ) THEN

*     Subtraction might overflow beyond VAL__MAXD. So we first find
*     VAL__MAXD+BZERO.
         TEST = VAL__MAXD + BZERO
         SMIN = MIN( SMIN, TEST )
         SMAX = MIN( SMAX, TEST )

*  Else if BZERO positive.
      ELSE IF ( BZERO .GT. 0D0 ) THEN

*     Subtraction might overflow beyond VAL__MIND. So we first find
*     VAL__MIND+BZERO.
         TEST = VAL__MIND + BZERO
         SMIN = MAX( SMIN, TEST )
         SMAX = MAX( SMAX, TEST )
      END IF

*  Now we can subtract BZERO.
      SMIN = SMIN - BZERO
      SMAX = SMAX - BZERO


*  Apply BSCALE to SMIN, SMAX.
*  We would like to divide both by BSCALE, but must be wary of division
*  by zero, overflows, and inversion (SMIN>SMAX).

*  If BSCALE is negative.
      IF ( BSCALE .LT. 0D0 ) THEN

*     If BSCALE is between 0 and -1.
         IF ( BSCALE .GT. -1D0 ) THEN

*        Division by BSCALE might overflow. So we first get
*        VAL_M%%D*BSCALE. (Note that BSCALE is negative and minimum and
*        maximum exchanged.)
            TEST = VAL__MAXD * BSCALE
            SMIN = MAX( SMIN, TEST )
            TEST = VAL__MIND * BSCALE
            SMAX = MIN( SMAX, TEST )
         END IF

*     Now we can divide by BSCALE. (Note that BSCALE is negative and
*     minimum and maximum exchanged.)
         SMIN = SMIN / BSCALE
         SMAX = SMAX / BSCALE
         TEST = SMIN
         SMIN = SMAX
         SMAX = TEST

*  Else if BSCALE is positive.
      ELSE IF ( BSCALE .GT. 0D0 ) THEN

*     If BSCALE is between 0 and 1.
         IF ( BSCALE .LT. 1D0 ) THEN

*        Division by BSCALE might overflow. So we first get
*        VAL_M%%D*BSCALE.
            TEST = VAL__MIND * BSCALE
            SMIN = MAX( SMIN, TEST )
            TEST = VAL__MAXD * BSCALE
            SMAX = MIN( SMAX, TEST )
         END IF

*     Now we can divide by BSCALE.
         SMIN = SMIN / BSCALE
         SMAX = SMAX / BSCALE

*  Else (BSCALE is zero).
      ELSE

*     We have already applied BZERO to SMIN and SMAX. And we know that
*     all BASE(i) * SCALE are zero. The issue here is then whether
*     zero is in the interval [SMIN,SMAX]. If not, we know that each
*     element of BASE causes an overflow. We can set ERRORS=ELEMENTS and
*     return without further action.
         IF ( SMIN .GT. 0D0 .OR. SMAX .LT. 0D0 ) THEN
            ERRORS = ELEMENTS
            GO TO 500
         END IF
      END IF


*  SMIN and SMAX are now directly comparable to DBLE(BASE(i)). We can go
*  about scaling each element.

      ERRORS = 0
      DO 1 I = 1, ELEMENTS
         TEST = NUM_WTOD( BASE(I) )
         IF ( TEST .GE. SMIN .AND. TEST .LE. SMAX ) THEN
            TEST = TEST * BSCALE + BZERO
            ARRAY(I) = NUM_DTOW( TEST )
         ELSE
            ERRORS = ERRORS + 1
         END IF
 1    CONTINUE


*  Return.
 500  CONTINUE
      END
