C+
      SUBROUTINE TSP_WLU_Y (LOC,LABEL,UNITS,STATUS)
C
C                               T S P _ W L U _ Y
C
C  Routine name:
C     TSP_WLU_Y
C
C  Function:
C     Write the LABEL and UNITS of the Y axis
C
C  Description:
C     Given a locator to an TSP structure, write values for the LABEL and UNITS
C     of the Y axis.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_WLU_Y (LOC,LABEL,UNITS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) LOC		(Fixed string,descr) A locator to the NDF
C                       structure.
C     (<) LABEL         (Fixed string,descr) Label string.
C     (<) UNITS         (Fixed string,descr) Units string.
C     (!) STATUS	(Integer,ref) The Status
C
C  External subroutines / functions used:
C
C     TSP_WLU,
C     Various HDS routines
C
C  Support: Jeremy Bailey, JAC
C
C  Version date: 19/10/1989
C
C-
C  Subroutine / function details:
C
C  History:
C     19/10/1989   Original version.  JAB / AAO.
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
C
C     Parameters
C
      CHARACTER*(DAT__SZLOC) LOC
      CHARACTER*(*) LABEL,UNITS
      INTEGER STATUS
C
C     Local variables
C
      INTEGER ID

C

      IF (STATUS .EQ. SAI__OK) THEN

          CALL NDF_IMPRT(LOC,ID,STATUS)
          CALL NDF_ACPUT(LABEL,ID,'LABEL',2,STATUS)
          CALL NDF_ACPUT(UNITS,ID,'UNITS',2,STATUS)

      END IF
C
      END

