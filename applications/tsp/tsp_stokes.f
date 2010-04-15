C+
      SUBROUTINE TSP_STOKES (LOC,NUM,Q,U,V,STATUS)
C
C                               T S P _ S T O K E S
C
C  Routine name:
C     TSP_STOKES
C
C  Function:
C     Find out which Stokes parameters are present in a dataset
C
C  Description:
C     Given a locator to a polarimetry structure, return the number
C     of Stokes parameters, and their identities.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_STOKES (LOC,NUM,Q,U,V,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) LOC		(Fixed string,descr) A locator to the polarimetry
C                       structure.
C     (<) NUM           (Integer,ref) The number of Stokes
C                       parameters in the dataset
C     (<) Q             (Logical,ref) True if the Q Stokes parameter
C                       is present
C     (<) U             (Logical,ref) True if the U Stokes parameter
C                       is present
C     (<) V             (Logical,ref) True if the V Stokes parameter
C                       is present
C     (!) STATUS	(Integer,ref) The Status
C
C  External subroutines / functions used:
C
C     Various HDS routines
C
C  Support: Jeremy Bailey, JAC
C
C  Version date: 29/2/1988
C
C-
C  Subroutine / function details:
C
C  History:
C     29/2/1988   Original version.  JAB / AAO.
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
C
C     Parameters
C
      CHARACTER*(DAT__SZLOC) LOC
      INTEGER NUM
      LOGICAL Q,U,V
      INTEGER STATUS
C
C     Local variables
C
      CHARACTER*(DAT__SZLOC) MLOC,PLOC
C
      IF (STATUS .EQ. SAI__OK) THEN


          CALL DAT_FIND(LOC,'MORE',MLOC,STATUS)
          CALL DAT_FIND(MLOC,'POLARIMETRY',PLOC,STATUS)
          CALL DAT_THERE(PLOC,'STOKES_Q',Q,STATUS)
          CALL DAT_THERE(PLOC,'STOKES_U',U,STATUS)
          CALL DAT_THERE(PLOC,'STOKES_V',V,STATUS)
          NUM = 0
          IF (Q) NUM = NUM+1
          IF (U) NUM = NUM+1
          IF (V) NUM = NUM+1
          CALL DAT_ANNUL(PLOC,STATUS)
          CALL DAT_ANNUL(MLOC,STATUS)

      END IF
C
      END

