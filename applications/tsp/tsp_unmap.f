C+
      SUBROUTINE TSP_UNMAP (LOC,STATUS)
C
C                               T S P _ U N M A P
C
C  Routine name:
C     TSP_UNMAP
C
C  Function:
C     Unmap a mapped data array
C
C  Description:
C     Given a locator to a mapped object, unmap it, and annul the locator.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_UNMAP(LOC,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) LOC		(Fixed string,descr) A locator to a mapped
C                       object - returned by one of the TSP_MAP... routines
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
C     15/3/1991   NDF version    JAB/JAC
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
C
C     Parameters
C
      CHARACTER*(DAT__SZLOC) LOC
      INTEGER STATUS
C
C     This does nothing in the NDF version - The final NDF_END call handles
C     unmapping
C
      END

