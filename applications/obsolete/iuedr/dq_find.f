      INTEGER FUNCTION dq_FIND(MASK, DQ, NPOINT)

*+
*
*   Name:
*      INTEGER FUNCTION dq_AND
*
*   Description:
*      Look for the first zero dq_AND in data quality list.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      30-JUN-81
*         AT4 version.
*      Paul Rees          25-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          23-MAY-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      Commence search from start of list. Return zero if no match found.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER MASK           ! data quality mask to be found
      INTEGER NPOINT         ! number of dq in list
      INTEGER DQ(NPOINT)     ! list of dq to be searched

*   External references:
      INTEGER dq_AND         ! data quality AND operation

      dq_FIND = 1

      DO WHILE (dq_FIND.LE.NPOINT)

         IF (dq_AND(DQ(dq_FIND), MASK).EQ.0) THEN
            GO TO 100
         ELSE
            dq_FIND = dq_FIND + 1
         END IF
      END DO

      dq_FIND = 0
 100  CONTINUE

      END
