C+
C                           F I G _ F I L L _ O R D S
C
C  Routine name:
C     FIG_FILL_ORDS
C
C  Function:
C     Fills up the order number array for the ECHSELECT output structures.
C
C  Description:
C     Given the order number range and direction for the orders selected
C     by ECHSELECT, this routine fills up the order axis array for the
C     output structures it creates.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL FIG_FILL_ORDS (MINORD,NORD,MDELTA,MDATA)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) MINORD          (Integer,ref) The lowest order number selected
C     (>) NORD            (Integer,ref) The number of orders selected
C     (>) MDELTA          (Integer,ref) The order for the order numbers -
C                         -1 => descending, +1 => ascending.
C     (<) MDATA           (Real array,ref) The axis data array to be filled.
C                         This should have NORD elements.
C
C  External variables used:  None.
C
C  External subroutines / functions used:  None.
C
C  Prior requirements:
C     Called as an internal routine of ECHSELECT.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 21st Feb 1989
C-
C  History:
C     21st Feb 1989.  Original version.  KS / AAO.
C+
      SUBROUTINE FIG_FILL_ORDS (MINORD,NORD,MDELTA,MDATA)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER MINORD, NORD, MDELTA
      REAL    MDATA(NORD)
C
C     Local variables
C
      INTEGER I                  ! Loop variable through orders
C
      IF (MDELTA.EQ.-1) THEN
         DO I=1,NORD
            MDATA(I)=MINORD+NORD-I
         END DO
      ELSE
         DO I=1,NORD
            MDATA(I)=MINORD+I-1
         END DO
      END IF
C
      END
