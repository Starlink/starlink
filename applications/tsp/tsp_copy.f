C+
      SUBROUTINE TSP_COPY (LOC,LOC2,STATUS)
C
C                               T S P _ C O P Y
C
C  Routine name:
C     TSP_COPY
C
C  Function:
C     Copy a TSP structure from one locator to another
C
C  Description:
C     Given a locator to a TSP structure, a complete copy of the structure
C     is created at a second locator.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_COPY (LOC,LOC2,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) LOC		(Fixed string,descr) A locator to the
C                       top level of the object to
C                       be copied (e.g. supplied by DAT_ASSOC)
C     (>) LOC2          (Fixed string,descr) A locator to the top level
C                       object of an empty structure in which the
C                       copy will be created (e.g. supplied by DAT_CREAT)
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
C     27/2/1988   Prevent final DAT_ANNUL returning
C                 bad status.        JAB / AAO.
C     15/3/1991   NDF version        JAB / JAC.
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
C
C     Parameters
C
      CHARACTER*(DAT__SZLOC) LOC,LOC2
      INTEGER STATUS
C
C     Local variables
C
      CHARACTER*(DAT__SZLOC) TLOC,TLOC2,TLOC3
      INTEGER ID1,ID2,PLACE
      INTEGER STAT
      LOGICAL DEFINED
      LOGICAL STATE,PRIM
      INTEGER NDIMS,I,SUB(1)
C
      IF (STATUS .EQ. SAI__OK) THEN

          CALL NDF_BEGIN
          CALL NDF_IMPRT(LOC,ID1,STATUS)
          CALL NDF_PLACE(LOC2,' ',PLACE,STATUS)
          CALL NDF_COPY(ID1,PLACE,ID2,STATUS)
          IF (STATUS .EQ. SAI__OK) THEN
             CALL TSP_NDF_END(STATUS)
             IF (STATUS .NE. SAI__OK) CALL ERR_ANNUL(STATUS)
          ENDIF
C
C  Make sure items are defined as some NDF applications will complain
C  about undefined items. (It doesn't seem to be possible to do this
C  using NDF itself).
C
          CALL ERR_MARK
          IF (STATUS .NE. SAI__OK) RETURN
          CALL DAT_FIND(LOC2,'TITLE',TLOC,STATUS)
          IF (STATUS .EQ. SAI__OK) THEN
              CALL DAT_STATE(TLOC,DEFINED,STATUS)
              IF (.NOT. DEFINED) THEN
                  CALL DAT_PUT0C(TLOC,' ',STATUS)
              ENDIF
              CALL DAT_ANNUL(TLOC,STATUS)
          ELSE
              CALL ERR_ANNUL(STATUS)
          ENDIF
          CALL DAT_FIND(LOC2,'LABEL',TLOC,STATUS)
          CALL DAT_STATE(TLOC,DEFINED,STATUS)
          IF (.NOT. DEFINED) THEN
              CALL DAT_PUT0C(TLOC,' ',STATUS)
          ENDIF
          CALL DAT_ANNUL(TLOC,STATUS)
          CALL DAT_FIND(LOC2,'UNITS',TLOC,STATUS)
          CALL DAT_STATE(TLOC,DEFINED,STATUS)
          IF (.NOT. DEFINED) THEN
              CALL DAT_PUT0C(TLOC,' ',STATUS)
          ENDIF
          CALL DAT_ANNUL(TLOC,STATUS)
          CALL DAT_FIND(LOC2,'AXIS',TLOC,STATUS)
          CALL DAT_SIZE(TLOC,NDIMS,STATUS)
          DO I=1,NDIMS
              SUB(1)=I
              CALL DAT_CELL(TLOC,1,SUB,TLOC2,STATUS)
              CALL DAT_FIND(TLOC2,'LABEL',TLOC3,STATUS)
              CALL DAT_STATE(TLOC3,DEFINED,STATUS)
              IF (.NOT. DEFINED) THEN
                  CALL DAT_PUT0C(TLOC3,' ',STATUS)
              ENDIF
              CALL DAT_ANNUL(TLOC3,STATUS)
              CALL DAT_FIND(TLOC2,'UNITS',TLOC3,STATUS)
              CALL DAT_STATE(TLOC3,DEFINED,STATUS)
              IF (.NOT. DEFINED) THEN
                  CALL DAT_PUT0C(TLOC3,' ',STATUS)
              ENDIF
              CALL DAT_ANNUL(TLOC3,STATUS)
              CALL DAT_FIND(TLOC2,'DATA_ARRAY',TLOC3,STATUS)
              CALL DAT_PRIM(TLOC3,PRIM,STATUS)
              IF (PRIM) THEN
                  CALL DAT_STATE(TLOC3,DEFINED,STATUS)
                  IF (.NOT. DEFINED) THEN
                      CALL TSP__FILL(TLOC3,STATUS)
                  ENDIF
              ENDIF
              CALL DAT_ANNUL(TLOC3,STATUS)
              CALL DAT_ANNUL(TLOC2,STATUS)
          ENDDO
          CALL DAT_ANNUL(TLOC,STATUS)
          IF (STATUS .NE. SAI__OK) CALL ERR_ANNUL(STATUS)
          CALL ERR_RLSE
      END IF
      END


      SUBROUTINE TSP__FILL(LOC,STATUS)

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      CHARACTER*(DAT__SZLOC) LOC
      INTEGER STATUS
      INTEGER DIM(1),SIZE,PTR

      CALL DAT_SIZE(LOC,SIZE,STATUS)
      DIM(1)=SIZE
      CALL DAT_MAPR(LOC,'WRITE',1,DIM,PTR,STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP__FILL2(SIZE,%VAL(PTR))
      ENDIF
      END


      SUBROUTINE TSP__FILL2(SIZE,X)

      IMPLICIT NONE
      INTEGER SIZE
      REAL X(SIZE)
      INTEGER I
      DO I=1,SIZE
          X(I) = REAL(I)-0.5
      ENDDO
      END
