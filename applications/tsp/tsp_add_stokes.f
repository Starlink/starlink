C+
      SUBROUTINE TSP_ADD_STOKES (LOC,STOKES,V,STATUS)
C
C                               T S P _ A D D _ S T O K E S
C
C  Routine name:
C     TSP_ADD_STOKES
C
C  Function:
C     Add a new Stokes component to a polarimetry structure.
C
C  Description:
C     Given a locator to a polarimetry structure, add a new Stokes
C     component to it.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_ADD_STOKES (LOC,STOKES,V,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) LOC		(Fixed string,descr) A locator to the polarimetry
C                       structure.
C     (>) STOKES        (Fixed string,descr) The name of the
C                       Stokes parameter ('Q', 'U' or 'V')
C     (>) V             (Logical,ref) True if a variance array is to be
C                       included.
C     (!) STATUS	(Integer,ref) The Status
C
C  External subroutines / functions used:
C
C     Various NDF routines,
C     TSP_SIZE
C
C  Support: Jeremy Bailey, JAC
C
C  Version date: 30/4/1988
C
C-
C  Subroutine / function details:
C
C  History:
C     28/2/1988   Original version.  JAB / AAO.
C     30/4/1988   Create the Polarimetry extension
C                  if it does not exist.   JAB/AAO
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
C
C     Parameters
C
      CHARACTER*(DAT__SZLOC) LOC
      CHARACTER*(*) STOKES
      LOGICAL V
      INTEGER STATUS
C
C     Local variables
C
      CHARACTER*(DAT__SZLOC) PLOC
      INTEGER ACTDIM,DIMS(7),LOW(7)
      INTEGER ID,I,PLACE,ID2
      LOGICAL THERE
C
      IF (STATUS .EQ. SAI__OK) THEN


          CALL TSP_SIZE(LOC,7,DIMS,ACTDIM,STATUS)
          CALL NDF_BEGIN
          CALL NDF_IMPRT(LOC,ID,STATUS)
          CALL NDF_XSTAT(ID,'POLARIMETRY',THERE,STATUS)
          IF (.NOT. THERE) THEN
              CALL NDF_XNEW(ID,'POLARIMETRY','EXT',0,0,PLOC,STATUS)
          ELSE
              CALL NDF_XLOC(ID,'POLARIMETRY','UPDATE',PLOC,STATUS)
          END IF
          DO I=1,ACTDIM
              LOW(I)=1
          ENDDO
          IF (STOKES .EQ. 'Q') THEN
              CALL NDF_PLACE(PLOC,'STOKES_Q',PLACE,STATUS)
              CALL NDF_NEW('_REAL',ACTDIM,LOW,DIMS,PLACE,ID2,STATUS)
          ELSE IF (STOKES .EQ. 'U') THEN
              CALL NDF_PLACE(PLOC,'STOKES_U',PLACE,STATUS)
              CALL NDF_NEW('_REAL',ACTDIM,LOW,DIMS,PLACE,ID2,STATUS)
          ELSE IF (STOKES .EQ. 'V') THEN
              CALL NDF_PLACE(PLOC,'STOKES_V',PLACE,STATUS)
              CALL NDF_NEW('_REAL',ACTDIM,LOW,DIMS,PLACE,ID2,STATUS)
          ENDIF
          CALL DAT_ANNUL(PLOC,STATUS)
          CALL TSP_NDF_END(STATUS)
      END IF
C
      END

