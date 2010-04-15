C+
      SUBROUTINE TSP_MAP_VAR (LOC,MODE,PTR,LOC2,STATUS)
C
C                               T S P _ M A P _ V A R
C
C  Routine name:
C     TSP_MAP_VAR
C
C  Function:
C     Map the variance array of an NDF structure
C
C  Description:
C     Given a locator to an NDF structure map its VARIANCE array.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_MAP_VAR (LOC,MODE,PTR,LOC2,STATUS)
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
C     Various HDS routines
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
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'NDF_ERR'
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
      LOGICAL DEFINED
C
      IF (STATUS .EQ. SAI__OK) THEN

          CALL NDF_IMPRT(LOC,ID,STATUS)
          IF (MODE .EQ. 'READ' .OR. MODE .EQ. 'UPDATE') THEN
              CALL NDF_STATE(ID,'VARIANCE',DEFINED,STATUS)
              IF (.NOT. DEFINED) STATUS = NDF__VUDEF
          ENDIF
          CALL NDF_MAP(ID,'VARIANCE','_REAL',MODE,PTR,LENGTH,STATUS)

      END IF
C
      END

