      SUBROUTINE SORT( STATUS )
*
*   Given Integer Array NA, of Dim. (2,IDIM)
*   sorts the first N Items into
*   Ascending Numerical Order using NAG routines.
*   The key is NA(1,K) - The Magnitude
*   The routine produces an index in the Array IP
*   which points to the sorted array elements.
*
*   Arguments:
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*
*     8-DEC-1988
*       Modified by Peter Allan (MAVAD::PMA) to replace the call to the
*       NAG routine M01ALF as it was withdrawn at Mark 13.
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*     4-AUG-2004 (TIMJ):
*       Remove TYPE and replace with PRINT
*       Remove NAG dependency
*

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MAIN'
      INTEGER IA(IDIM)
      INTEGER K, J

*  Status:
      INTEGER STATUS             ! Global status

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Always initialise the array IP to be in Ascending
*   Order ; even if NO SORTING to be done
*
      DO K= 1,NUM
         IP(K) = K
      ENDDO
*
*   If Sorting Needed
*
      IF (NUM.GT.MAXNUM) THEN
         DO J = 1,NUM
            IA(J) = NSTAR(1,J)
         ENDDO
         CALL PDA_QSIAI(NUM, IA, IP)
*
*   From now on only consider the 'MAXNUM'
*   Brightest Stars.
*
         NUM = MAXNUM
      ENDIF
      END

