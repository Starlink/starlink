C+
      SUBROUTINE TSP_MAP_Y (LOC,MODE,PTR,LOC2,STATUS)
C
C                               T S P _ M A P _ Y
C
C  Routine name:
C     TSP_MAP_Y
C
C  Function:
C     Map the Y axis of a TSP structure
C
C  Description:
C     Given a locator to a 3D TSP structure map its Y axis data.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_MAP_Y (LOC,MODE,PTR,LOC2,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) LOC		(Fixed string,descr) A locator to the NDF
C                       structure.
C     (>) MODE          (Fixed string,descr) The access mode,
C                       'READ','WRITE' or 'UPDATE'.
C     (<) PTR           (Integer,ref) Pointer to the mapped data.
C     (<) LOC2		(Fixed string,descr) The locator to the
C                       mapped data object - needed so that it can
C                       be unmapped.
C     (!) STATUS	(Integer,ref) The Status
C
C  External subroutines / functions used:
C
C     TSP_MAP_DATA,
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
      CHARACTER*(*) MODE
      CHARACTER*(DAT__SZLOC) LOC2
      INTEGER PTR
      INTEGER STATUS
C
C     Local variables
C
      INTEGER ID,LENGTH

C

      IF (STATUS .EQ. SAI__OK) THEN

          CALL NDF_IMPRT(LOC,ID,STATUS)
          CALL NDF_AMAP(ID,'CENTRE',2,'_REAL',MODE,PTR,LENGTH,STATUS)

      END IF
C
      END

