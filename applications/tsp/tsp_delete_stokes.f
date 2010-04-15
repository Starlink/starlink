C+
      SUBROUTINE TSP_DELETE_STOKES (LOC,STOKES,STATUS)
C
C                         T S P _ D E L E T E _ S T O K E S
C
C  Routine name:
C     TSP_DELETE_STOKES
C
C  Function:
C     Delete a Stokes component from a polarimetry structure.
C
C  Description:
C     Given a locator to a polarimetry structure, Delete a specified Stokes
C     component from it.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_DELETE_STOKES (LOC,STOKES,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) LOC		(Fixed string,descr) A locator to the polarimetry
C                       structure.
C     (>) STOKES        (Fixed string,descr) The name of the
C                       Stokes parameter ('Q', 'U' or 'V')
C     (!) STATUS	(Integer,ref) The Status
C
C  External subroutines / functions used:
C
C     Various HDS routines
C
C  Support: Jeremy Bailey, JAC
C
C  Version date: 11/8/1988
C
C-
C  Subroutine / function details:
C
C  History:
C     11/8/1988   Original version.  JAB / AAO.
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
C
C     Parameters
C
      CHARACTER*(DAT__SZLOC) LOC
      CHARACTER*(*) STOKES
      INTEGER STATUS
C
C     Local variables
C
      CHARACTER*(DAT__SZLOC) MLOC,PLOC
      CHARACTER*80 TSTRING
C
      IF (STATUS .EQ. SAI__OK) THEN

          CALL DAT_FIND(LOC,'MORE',MLOC,STATUS)
          CALL DAT_FIND(MLOC,'POLARIMETRY',PLOC,STATUS)
          TSTRING = 'STOKES_'//STOKES
          CALL DAT_ERASE(PLOC,TSTRING,STATUS)
          CALL DAT_ANNUL(PLOC,STATUS)
          CALL DAT_ANNUL(MLOC,STATUS)
      END IF
C
      END


