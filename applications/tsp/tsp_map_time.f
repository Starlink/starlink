C+
      SUBROUTINE TSP_MAP_TIME (LOC,MODE,PTR,LOC2,STATUS)
C
C                               T S P _ M A P _ T I M E
C
C  Routine name:
C     TSP_MAP_TIME
C
C  Function:
C     Map the time axis of a TSP structure
C
C  Description:
C     Given a locator to an TSP structure, map its time axis data.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_MAP_TIME (LOC,MODE,PTR,LOC2,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) LOC		(Fixed string,descr) A locator to the NDF
C                       structure.
C     (>) MODE          (Fixed string,descr) The access mode,
C                       'READ','WRITE' or 'UPDATE'.
C     (<) PTR           (Integer,ref) Pointer to the mapped data.
C                       A double precision array of MJD.
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
C     27/2/1988   Fix bug in determining size of AXIS(n).   JAB/AAO.
C     28/2/1988   Use DAT_MAPD rather than TSP_MAP_DATA.   JAB/AAO.
C     15/3/1991   NDF version    JAB/JAC
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
      INTEGER ID,LENGTH,ACTDIM,DIMS(7)

C

      IF (STATUS .EQ. SAI__OK) THEN

*     Find the Axis component

          CALL NDF_IMPRT(LOC,ID,STATUS)

          CALL TSP_SIZE(LOC,3,DIMS,ACTDIM,STATUS)
          IF (ACTDIM .EQ. 2) THEN
              CALL NDF_AMAP(ID,'CENTRE',2,'_DOUBLE',MODE,PTR,
     :            LENGTH,STATUS)
          ELSE IF (ACTDIM .EQ. 3) THEN
              CALL NDF_AMAP(ID,'CENTRE',3,'_DOUBLE',MODE,PTR,
     :            LENGTH,STATUS)
          ELSE
              CALL MSG_OUT(' ','Attempt to map time axis of 1D data',
     :            STATUS)
          ENDIF
      END IF
C
      END

