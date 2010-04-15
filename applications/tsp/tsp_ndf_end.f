C
      SUBROUTINE TSP_NDF_END (STATUS)
C
C                               T S P _ N D F _ E N D
C
C  Routine name:
C     TSP_NDF_END
C
C  Function:
C     End NDF context
C
C  Description:
C     This is a replacement for the routine NDF_END which does not report
C     an error if the data array is undefined
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_NDF_END (STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (!) STATUS	(Integer,ref) The Status
C
C  External subroutines / functions used:
C
C     NDF_END, ERR_ANNUL
C
C  Support: Jeremy Bailey, JAC
C
C  Version date: 4/4/1991
C
C
C  Subroutine / function details:
C
C  History:
C     4/4/1991   Original version.  JAB / AAO.
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'NDF_ERR'
C
C     Parameters
C
      INTEGER STATUS
C
C     Local variables
C
C

      CALL NDF_END(STATUS)
      IF (STATUS .EQ. NDF__DUDEF) THEN
          CALL ERR_ANNUL(STATUS)
      ENDIF
      END
