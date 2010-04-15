C+
      SUBROUTINE TSP_RESIZE (LOC,NDIM,DIMS,STATUS)
C
C                               T S P _ R E S I Z E
C
C  Routine name:
C     TSP_RESIZE
C
C  Function:
C     Change the size of all the data arrays in a structure
C
C  Description:
C     Given a locator to a polarimetry structure, change the
C     size of the arrays in the structure. If the change
C     consists solely of a change to the last dimension then
C     data in the arrays will retain values from corresponding
C     components in the original array. With more complex
C     changes the data values will be lost. Axis arrays will
C     retain their values where still present in the output array.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_RESIZE (LOC,NDIM,DIMS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) LOC		(Fixed string,descr) A locator to the polarimetry
C                       structure.
C     (>) NDIM          (Integer,ref) The new number of Dimensions for
C                       the structure
C     (>) DIMS          (Integer Array,ref) The new dimensions for the
C                       structure.
C     (!) STATUS	(Integer,ref) The Status
C
C  External subroutines / functions used:
C
C     Various HDS routines
C
C  Support: Jeremy Bailey, JAC
C
C  Version date: 1/3/1988
C
C-
C  Subroutine / function details:
C
C  History:
C     1/3/1988   Original version.  JAB / AAO.
C     11/8/1988  Modified to support arbitrary changes in size and
C                dimensionality.  JAB / AAO.
C     15/3/1991  NDF version     JAB/JAC
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
C
C     Parameters
C
      CHARACTER*(DAT__SZLOC) LOC
      INTEGER NDIM,DIMS(3)
      INTEGER STATUS
C
C     Local variables
C
      INTEGER ID,I,LOW(3)
      CHARACTER*(DAT__SZLOC) SLOC
C
      IF (STATUS .EQ. SAI__OK) THEN

          CALL NDF_BEGIN
          CALL NDF_IMPRT(LOC,ID,STATUS)
          DO I=1,3
              LOW(I)=1
          ENDDO
          CALL NDF_SBND(NDIM,LOW,DIMS,ID,STATUS)
          CALL TSP_GET_STOKES(LOC,'Q',SLOC,STATUS)
          IF (STATUS .EQ. SAI__OK) THEN
              CALL NDF_IMPRT(SLOC,ID,STATUS)
              CALL NDF_SBND(NDIM,LOW,DIMS,ID,STATUS)
              CALL DAT_ANNUL(SLOC,STATUS)
          ELSE
              STATUS = SAI__OK
          ENDIF
          CALL TSP_GET_STOKES(LOC,'U',SLOC,STATUS)
          IF (STATUS .EQ. SAI__OK) THEN
              CALL NDF_IMPRT(SLOC,ID,STATUS)
              CALL NDF_SBND(NDIM,LOW,DIMS,ID,STATUS)
              CALL DAT_ANNUL(SLOC,STATUS)
          ELSE
              STATUS = SAI__OK
          ENDIF
          CALL TSP_GET_STOKES(LOC,'V',SLOC,STATUS)
          IF (STATUS .EQ. SAI__OK) THEN
              CALL NDF_IMPRT(SLOC,ID,STATUS)
              CALL NDF_SBND(NDIM,LOW,DIMS,ID,STATUS)
              CALL DAT_ANNUL(SLOC,STATUS)
          ELSE
              STATUS = SAI__OK
          ENDIF
          CALL TSP_NDF_END(STATUS)

      END IF
C
      END

