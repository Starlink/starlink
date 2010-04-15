C+
      SUBROUTINE TSP_SIZE (LOC,MAXDIM,DIMS,ACTDIM,STATUS)
C
C                               T S P _ S I Z E
C
C  Routine name:
C     TSP_SIZE
C
C  Function:
C     Return the dimensions of a TSP structure
C
C  Description:
C     Given a locator to an TSP structure, return the dimensions and
C     number of dimensions.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_SIZE (LOC,MAXDIM,DIMS,ACTDIM,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) LOC		(Fixed string,descr) A locator to the NDF
C                       structure.
C     (>) MAXDIM        (Integer,ref) Size of DIMS
C     (<) DIMS          (Integer array,ref) Array to receive the
C                       size of each dimension.
C     (<) ACTDIM        (Integer,ref) Actual number of dimensions.
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
C     5/4/1991    NDF version.       JAB / JAC.
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
C
C     Parameters
C
      CHARACTER*(DAT__SZLOC) LOC
      INTEGER MAXDIM,ACTDIM
      INTEGER DIMS(MAXDIM)
      INTEGER STATUS
C
C     Local variables
C
      INTEGER ID

C

      IF (STATUS .EQ. SAI__OK) THEN

          CALL NDF_IMPRT(LOC,ID,STATUS)
          CALL NDF_DIM(ID,MAXDIM,DIMS,ACTDIM,STATUS)

      END IF
C
      END

