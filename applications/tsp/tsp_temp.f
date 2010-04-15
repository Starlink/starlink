C+
      SUBROUTINE TSP_TEMP (SIZE,TYPE,PTR,LOC,STATUS)
C
C                               T S P _ T E M P
C
C  Routine name:
C     TSP_TEMP
C
C  Function:
C     Create a temporary array.
C
C  Description:
C     Create a temporary array, map it, and return a pointer to it.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_TEMP (SIZE,TYPE,PTR,LOC,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) SIZE          (Integer,ref) The size of the array to be created.
C     (>) TYPE          (Fixed string,descr) The type of the array -
C                       one of the HDS primitive type names.
C     (<) PTR           (Integer,ref) Pointer to the array created.
C     (<) LOC		(Fixed string,descr) A locator to the array
C                       structure (so that it can be unmapped).
C     (!) STATUS	(Integer,ref) The Status
C
C  External subroutines / functions used:
C
C     Various HDS routines
C
C  Support: Jeremy Bailey, JAC
C
C  Version date: 27/2/1988
C
C-
C  Subroutine / function details:
C
C  History:
C     27/2/1988   Original version.  JAB / AAO.
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
C
C     Parameters
C
      INTEGER SIZE
      CHARACTER*(*) TYPE
      INTEGER PTR
      CHARACTER*(DAT__SZLOC) LOC
      INTEGER STATUS
C
C     Local variables
C
      INTEGER LENGTH,PLACE,ID
C

      IF (STATUS .EQ. SAI__OK) THEN

          CALL NDF_TEMP(PLACE,STATUS)
          CALL NDF_NEWP(TYPE,1,SIZE,PLACE,ID,STATUS)
          CALL NDF_MAP(ID,'DATA',TYPE,'WRITE',PTR,LENGTH,STATUS)

      END IF
C
      END
