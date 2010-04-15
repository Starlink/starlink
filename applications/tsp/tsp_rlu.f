C+
      SUBROUTINE TSP_RLU (LOC,LABEL,UNITS,STATUS)
C
C                               T S P _ R L U
C
C  Routine name:
C     TSP_RLU
C
C  Function:
C     Read the LABEL and UNITS of a structure
C
C  Description:
C     Given a locator to an TSP structure, return the LABEL and UNITS
C     of the data array.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_RLU (LOC,LABEL,UNITS,STATUS)
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
C     Various HDS routines
C
C  Support: Jeremy Bailey, JAC
C
C  Version date: 11/3/1988
C
C-
C  Subroutine / function details:
C
C  History:
C     27/2/1988   Original version.  JAB / AAO.
C     11/3/1988   Return blank strings if not found.   JAB/AAO.
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
          CALL NDF_CGET(ID,'LABEL',LABEL,STATUS)
          CALL NDF_CGET(ID,'UNITS',UNITS,STATUS)

      END IF
C
      END

