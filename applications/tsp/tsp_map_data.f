C+
      SUBROUTINE TSP_MAP_DATA (LOC,MODE,PTR,LOC2,STATUS)
C
C                               T S P _ M A P _ D A T A
C
C  Routine name:
C     TSP_MAP_DATA
C
C  Function:
C     Map the data array of an NDF structure
C
C  Description:
C     Given a locator to an NDF structure map its main DATA_ARRAY.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_MAP_DATA (LOC,MODE,PTR,LOC2,STATUS)
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
C     Various NDF routines
C
C  Support: Jeremy Bailey, JAC
C
C  Version date: 26/2/1988
C
C-
C  Subroutine / function details:
C
C  History:
C     26/2/1988   Original version.  JAB / AAO.
C     28/2/1988   Use DAT_MAPR rather than DAT_BASIC.   JAB / AAO.
C     15/3/1991   NDF version        JAB / JAC.
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
          CALL NDF_MAP(ID,'DATA','_REAL',MODE,PTR,LENGTH,STATUS)

      END IF
C
      END
