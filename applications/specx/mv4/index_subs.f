      SUBROUTINE SEARCH_INDEX (INDEX, VALUE, I, J, IFAIL)

C   Routine to search a map index array for a scan in the specified position
C   and to find out what sky position it corresponds to.

      IMPLICIT NONE

C   Include files

      INCLUDE 'MAPHD'

C   Formal parameter:

      INTEGER*4 INDEX(MSTEP,NSTEP) ! Map file index
      INTEGER*4 VALUE              ! Index value to search for
      INTEGER*4 I,J                ! Return values of indices with this value
      INTEGER*4 IFAIL              ! SPECX error return

C   Other variables

      IFAIL = 0

      DO J = 1,NSTEP
        DO I = 1,MSTEP
          IF (INDEX(I,J).EQ.VALUE) RETURN
        END DO
      END DO

      IFAIL = 60

      RETURN
      END



      SUBROUTINE SEARCH_ARRAY (INDEX, I, IFAIL)

C   Routine to search a map index array for a free position.

      IMPLICIT NONE

C   Include files

      INCLUDE 'MAPHD'

C   Formal parameter:

      INTEGER*4 INDEX(*)           ! Map file index
      INTEGER*4 I                  ! Return values of free position
      INTEGER*4 IFAIL              ! SPECX error return

C   Other variables

      IFAIL = 0

      DO I = 1,MSTEP*NSTEP
        IF (INDEX(I).LE.0) RETURN
      END DO

      IFAIL = 60

      RETURN
      END



      SUBROUTINE INVERT_INDEX (INDEX1, SIZE, INDEX2)

C   routine to produce "inverse" array from a SPECX map index: i.e. elements
C   have values equal to position of corresponding scan on the map

      IMPLICIT NONE

C   Formal parameters:

      INTEGER*4 INDEX1(*)
      INTEGER*4 SIZE
      INTEGER*4 INDEX2(*)

C   Other parameters:

      INTEGER*4 J, IPOS

      CALL INIT_ARRAY (SIZE, INDEX2, -1000)

      DO J = 1, SIZE
        IPOS = INDEX1(J)
        IF (IPOS.GT.0 .AND. IPOS.LE.SIZE) INDEX2(IPOS) = J
      END DO

      RETURN
      END
