      SUBROUTINE CLA_MED(N,A,MEDIAN)
*+
*  Name:
*     CLA_MED

*  Purpose:
*     Find the median of an array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CLA_MED(N,A,MEDIAN)

*  Description:
*     Find the median of an array. This is done by sorting the array into
*     ascending order and the median is then either the central value or
*     the average of the two central values (if N is even). The sorting is
*     done using what is essentially the `quicksort' algorithm which works
*     by partitioning the array repeatedly. For finding the median it is
*     not necessary to fully sort the array, but only at each step to sort
*     the partition which contains the central value. This makes it easier
*     to use the (essentially recursive) quicksort algorithm in Fortran
*     since it is not necessary to keep track of the other unsorted partitions.
*
*     When the partition size falls to less than seven elements the remaining
*     data is sorted by straight insertion which is generally faster for
*     small datasets.


*  Arguments:
*     N = INTEGER (Given)
*        Number of elements in the array
*     A(N) = REAL (Given and Returned)
*        Array to find the median of. This array will be partially
*        reordered, so must have write access. It should not contain
*        any bad values.
*     MEDIAN = REAL (Returned)
*        NDF identifier of resulting NDF

*  Notes:
*     -  This routine cannot cope with bad pixels, so these must be removed
*        first if present (e.g. by copying the array with CLA_$MOVG1).
*

*  Authors:
*     JAB: Jeremy Bailey  (AAO)
*     {enter_new_authors_here}

*  History:
*     8-JUL-1992 (JAB)
*         Original version
*     5-APR-1993 (JAB)
*         Handle the case where the array is completely
*         sorted by the Quicksort stage and does not
*         require the final staricht insertion sort

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments
      INTEGER N
      REAL A(N),MEDIAN

*  Local variables
      INTEGER I,J,FIRST,LAST,CENT
      REAL VMID,A1,TEMP
      LOGICAL SORTED

      FIRST = 1
      LAST = N
      CENT = N/2 + 1
      SORTED = .FALSE.

*  Use modified quicksort algorithm for N > 7
      DO WHILE ((.NOT. SORTED) .AND. (LAST-FIRST .GT. 7))
          I = FIRST
          J = LAST
          VMID = A((I+J)/2)
          DO WHILE (I .LE. J)
              DO WHILE (A(I) .LT. VMID)
                  I=I+1
              ENDDO
              DO WHILE (VMID .LT. A(J))
                  J=J-1
              ENDDO
              IF (I .LE. J) THEN
                  TEMP = A(I)
                  A(I) = A(J)
                  A(J) = TEMP
                  I = I+1
                  J = J-1
              ENDIF
          ENDDO

*  Set FIRST and LAST to sort whichever partition contains the central value
          IF (FIRST .LT. J .AND. CENT .LE. J) THEN
              LAST = J
          ELSE IF (I .LE. CENT .AND. I .LT. LAST) THEN
              FIRST = I
          ELSE
              SORTED = .TRUE.
          ENDIF
      ENDDO

*  When less than 7 elements sort by straight insertion
      IF (.NOT. SORTED) THEN
          DO J=FIRST+1,LAST
              A1 = A(J)
              DO I=J-1,FIRST,-1
                  IF (A(I) .LE. A1) GOTO 10
                  A(I+1)=A(I)
              ENDDO
              I=FIRST-1
10            CONTINUE
              A(I+1)=A1
          ENDDO
      ENDIF

*  For odd number of elements median is the central value
      MEDIAN = A(CENT)

      IF (MOD(N,2) .EQ. 0) THEN

*  For even number we have to use the average of this and the next highest
          TEMP = A(CENT-1)
          DO I=1,CENT-2
              IF (A(I) .GT. TEMP) TEMP = A(I)
          ENDDO
          MEDIAN = (MEDIAN+TEMP)*0.5
      ENDIF
      END
