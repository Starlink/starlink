      SUBROUTINE SCULIB_MULTARE (NELM, ARRAY1, ARRAY2, ARRAY3, Q1DATA,
     :  Q2DATA, Q3DATA, E1DATA, E2DATA, E3DATA, QUAL, FLAGS, FBAD,
     :  VARIANCE)
*+
*  Name:
*     SCULIB_MULTARE

*  Purpose:
*     multiplies 2 real arrays with optional error and quality
*     handling

*  Description:
*     Multiplies two floating point arrays.  The arrays
*     may have any dimensions; they are treated here as
*     linear in order to generate more efficient code.

*  Invocation:
*     CALL SCULIB_MULTARE (NELM, ARRAY1, ARRAY2, ARRAY3, Q1DATA,
*    :  Q2DATA, Q3DATA, E1DATA, E2DATA, E3DATA, QUAL, FLAGS, FBAD,
*    :  VARIANCE)

*  Arguments:
*     NELM                = INTEGER (Given)
*              Number of elements in each array
*     ARRAY1 (NELM)       = REAL (Given)
*              Input array
*     ARRAY2 (NELM)       = REAL (Given)
*              Second input array.
*     ARRAY3 (NELM)       = REAL (Returned)
*              Result array.  ARRAY3=ARRAY1*ARRAY2
*     Q1DATA (NELM)       = INTEGER (Given)
*              Quality array for first input array
*     Q2DATA (NELM)       = INTEGER (Given)
*              Quality array for second input array
*     Q3DATA (NELM)       = INTEGER (Returned)
*              Quality array for output array
*     E1DATA (NELM)       = REAL (Given)
*              Variance array for input array
*     E2DATA (NELM)       = REAL (Given)
*              Variance array for second input array
*     E3DATA (NELM)       = REAL (Returned)
*              Variance array for output array
*     QUAL                = LOGICAL (Given)
*              True if input has quality information
*     FLAGS               = LOGICAL (Given)
*              True if input has flagged data values
*     FBAD                = REAL (Given)
*              Flag value
*     VARIANCE            = LOGICAL (Given)
*              True if both input arrays have variance arrays

*  Notes:
*     - Any of the arrays may be the same.
*     - Consider using VEC_MULR (SUN/39) instead.

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  History:
*     $Id$
*     24-SEP-1993: Original, re-badged version of GEN_MULTAFE by KS.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      LOGICAL  VARIANCE, QUAL, FLAGS
      INTEGER  NELM
      INTEGER Q1DATA(NELM), Q2DATA(NELM)
      REAL E1DATA(NELM), E2DATA(NELM)
      REAL ARRAY1(NELM), ARRAY2(NELM)
      REAL FBAD

*  Arguments Given & Returned:

*  Arguments Returned:
      INTEGER Q3DATA(NELM)
      REAL E3DATA(NELM)
      REAL ARRAY3(NELM)

*  Status:

*  External references:

*  Global variables:

*  Local Constants:
      INTEGER  GOOD, BAD
      PARAMETER (BAD = 1, GOOD = 0)

*  Local variables:
      INTEGER I

*  Internal References:

*  Local data:

*.

*  Handle different quality methods separately.

      IF (QUAL) THEN

*  Image had quality data

         DO I=1,NELM
            IF ((Q1DATA(I).EQ.GOOD).AND.(Q2DATA(I).EQ.GOOD)) THEN
               ARRAY3(I)=ARRAY1(I)*ARRAY2(I)
               IF(VARIANCE)THEN
                  E3DATA(I)=E2DATA(I)*ARRAY1(I)*ARRAY1(I)+
     :                                     E1DATA(I)*ARRAY2(I)*ARRAY2(I)
               ENDIF
            ELSE
               Q3DATA(I)=BAD
            ENDIF
         END DO

      ELSE IF (FLAGS) THEN

*  Image had flagged data values

         DO I=1,NELM
            IF ((ARRAY1(I).NE.FBAD).AND.(ARRAY2(I).NE.FBAD)) THEN
               ARRAY3(I)=ARRAY1(I)*ARRAY2(I)
               IF(VARIANCE)THEN
                  E3DATA(I)=E2DATA(I)*ARRAY1(I)*ARRAY1(I)+
     :                                     E1DATA(I)*ARRAY2(I)*ARRAY2(I)
               ENDIF
            ELSE
               ARRAY3(I)=FBAD
            END IF
         ENDDO
      ELSE

*  Image had no quality information

         IF (VARIANCE) THEN
            DO I=1,NELM
               ARRAY3(I)=ARRAY2(I)*ARRAY1(I)
               E3DATA(I)=E2DATA(I)*ARRAY1(I)*ARRAY1(I)+
     :                                     E1DATA(I)*ARRAY2(I)*ARRAY2(I)
            ENDDO
         ELSE
            DO I=1,NELM
               ARRAY3(I)=ARRAY2(I)*ARRAY1(I)
            ENDDO
         END IF

      END IF

      END
