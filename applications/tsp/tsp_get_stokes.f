C+
      SUBROUTINE TSP_GET_STOKES (LOC,STOKES,LOC2,STATUS)
C
C                               T S P _ G E T _ S T O K E S
C
C  Routine name:
C     TSP_GET_STOKES
C
C  Function:
C     Get locator to a Stokes component of a polarimetry structure.
C
C  Description:
C     Given a locator to a polarimetry structure, return a locator
C     to one of its Stokes parameters.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_GET_STOKES (LOC,STOKES,LOC2,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) LOC		(Fixed string,descr) A locator to the polarimetry
C                       structure.
C     (>) STOKES        (Fixed string,descr) The name of the
C                       Stokes parameter ('Q', 'U' or 'V')
C     (<) LOC2		(Fixed string,descr) The locator to the
C                       Stokes NDF object.
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
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
C
C     Parameters
C
      CHARACTER*(DAT__SZLOC) LOC
      CHARACTER*(*) STOKES
      CHARACTER*(DAT__SZLOC) LOC2
      INTEGER STATUS
C
C     Local variables
C
      CHARACTER*(DAT__SZLOC) MLOC,PLOC
C
      IF (STATUS .EQ. SAI__OK) THEN


          CALL DAT_FIND(LOC,'MORE',MLOC,STATUS)
          CALL DAT_FIND(MLOC,'POLARIMETRY',PLOC,STATUS)
          IF (STOKES .EQ. 'Q') THEN
              CALL DAT_FIND(PLOC,'STOKES_Q',LOC2,STATUS)
          ELSE IF (STOKES .EQ. 'U') THEN
              CALL DAT_FIND(PLOC,'STOKES_U',LOC2,STATUS)
          ELSE IF (STOKES .EQ. 'V') THEN
              CALL DAT_FIND(PLOC,'STOKES_V',LOC2,STATUS)
          END IF
          CALL DAT_ANNUL(PLOC,STATUS)
          CALL DAT_ANNUL(MLOC,STATUS)

      END IF
C
      END

