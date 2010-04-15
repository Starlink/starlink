C+
      SUBROUTINE TSP_CREATE_2D (LOC,LSIZE,TSIZE,STOKES,VI,VS,STATUS)
C
C                               T S P _ C R E A T E _ 2 D
C
C  Routine name:
C     TSP_CREATE_2D
C
C  Function:
C     Create a 2D TSP structure
C
C  Description:
C     A 2D TSP structure (representing a time series polarization spectrum) is
C     created of the specified size.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL TSP_CREATE_2D (LOC,LSIZE,TSIZE,STOKES,VI,VS,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) LOC		(Fixed string,descr) A locator to the
C                       top level of the object to
C                       be created (e.g. supplied by DAT_CREAT)
C     (>) LSIZE		(Integer,ref) The size of the array to be created
C                       in the wavelength axis.
C     (>) TSIZE         (Integer,ref) The size of the array to be created
C                       in the wavelength axis.
C     (>) STOKES        (Fixed string,descr) A string specifying which
C                       Stokes parameters are to be included in the
C                       structure. This must contain some combination
C                       of the letters 'Q', 'U' and 'V'
C     (>) VI		(Logical,ref) True if the variance of the intensity
C                       is to be included in the structure.
C     (>) VS		(Logical,ref) True if the variance of the Stokes
C                       parameters is to be included in the structure.
C     (!) STATUS	(Integer,ref) The Status
C
C  External subroutines / functions used:
C
C     Various NDF routines
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
C     28/2/1988   Fix bug in location of .POLARIMETRY   JAB/AAO.
C     15/3/1991   NDF version   JAB/JAC.
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
C
C     Parameters
C
      CHARACTER*(DAT__SZLOC) LOC
      INTEGER LSIZE,TSIZE
      CHARACTER*(*) STOKES
      LOGICAL VI,VS
      INTEGER STATUS
C
C     Local variables
C
      CHARACTER*(DAT__SZLOC) PLOC
      INTEGER HIGH(2),LOW(2)
      INTEGER ID,PLACE,IDQ,IDU,IDV
C
      IF (STATUS .EQ. SAI__OK) THEN

*     Create the top level components

          CALL NDF_BEGIN
          CALL NDF_PLACE(LOC,' ',PLACE,STATUS)
          LOW(1)=1
          LOW(2)=1
          HIGH(1)=LSIZE
          HIGH(2)=TSIZE
          CALL NDF_NEW('_REAL',2,LOW,HIGH,PLACE,ID,STATUS)

*  Create components of the axis structure

          CALL NDF_ACRE(ID,STATUS)
          CALL NDF_ASTYP('_DOUBLE',ID,'CENTRE',2,STATUS)

*  Create the polarimetry extension structure

          IF (STOKES .NE. ' ') THEN
              CALL NDF_XNEW(ID,'POLARIMETRY','EXT',0,0,PLOC,STATUS)
              IF (INDEX(STOKES,'Q') .NE. 0) THEN
                  CALL NDF_PLACE(PLOC,'STOKES_Q',PLACE,STATUS)
                  CALL NDF_NEW('_REAL',2,LOW,HIGH,PLACE,IDQ,STATUS)
              END IF
              IF (INDEX(STOKES,'U') .NE. 0) THEN
                  CALL NDF_PLACE(PLOC,'STOKES_U',PLACE,STATUS)
                  CALL NDF_NEW('_REAL',2,LOW,HIGH,PLACE,IDU,STATUS)
              END IF
              IF (INDEX(STOKES,'V') .NE. 0) THEN
                  CALL NDF_PLACE(PLOC,'STOKES_V',PLACE,STATUS)
                  CALL NDF_NEW('_REAL',2,LOW,HIGH,PLACE,IDV,STATUS)
              END IF
              CALL DAT_ANNUL(PLOC,STATUS)
          END IF
          CALL TSP_NDF_END(STATUS)
      END IF
C
      END
