C+
      SUBROUTINE TSP_WLU (LOC,LABEL,UNITS,STATUS)
C
C                               T S P _ W L U
C
C  Routine name:
C     TSP_WLU
C
C  Function:
C     Write the LABEL and UNITS of a structure
C
C  Description:
C     Given a locator to an TSP structure, write values for the LABEL and UNITS
C     of the data array.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_WLU (LOC,LABEL,UNITS,STATUS)
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
C  Version date: 27/2/1988
C
C-
C  Subroutine / function details:
C
C  History:
C     27/2/1988   Original version.  JAB / AAO.
C     4/4/1991    NDF version.   JAB / JAC.
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
          CALL NDF_CPUT(LABEL,ID,'LABEL',STATUS)
          CALL NDF_CPUT(UNITS,ID,'UNITS',STATUS)

      END IF
C
      END

