C+
      SUBROUTINE REVERSE(STATUS)
C
C            R E V E R S E
C
C     Command name:
C        REVERSE
C
C     Function:
C        Reverse a spectrum in the wavelength axis.
C
C     Description:
C        All the data arrays contained in a polarization spectrum are
C        reversed in order along the wavelength axis.
C
C     Parameters:
C    (1) INPUT      (TSP, 1D)  The input dataset.
C    (2) OUTPUT     (TSP, 1D)  The output dataset.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         27/4/1988
C
C-
C
C  History:
C    27/4/1988   Original Version.   JAB/AAO
C    13/12/1991  Reverse the intensity variance   JAB/AAO
C    13/12/1991  Don't fail if variance is not present for a Stokes
C                        parameter      JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointer
      INTEGER SPTR

*  Data size
      INTEGER SIZE

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,OLOC,SLOC,SDLOC,DLOC

*  Array dimensions
      INTEGER ACTDIM,DIMS(7)
      INTEGER NUM
      LOGICAL QZ,UZ,VZ

*  Get the data
      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Create the output dataset
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output
      CALL TSP_COPY(LOC,OLOC,STATUS)

*  Get size of data
      CALL TSP_SIZE(OLOC,7,DIMS,ACTDIM,STATUS)

*  Check that it is one dimensional
      IF (ACTDIM .NE. 1) THEN
          CALL MSG_OUT('OUT','Invalid Dimensions',STATUS)
          STATUS = USER__001
      ENDIF
      SIZE = DIMS(1)

*  Map the intensity data
      CALL TSP_MAP_DATA(OLOC,'UPDATE',SPTR,DLOC,STATUS)

*  Reverse the intensity data
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_REVERSE(SIZE,%VAL(SPTR))
      ENDIF

*  Unmap the data
      CALL TSP_UNMAP(DLOC,STATUS)

*  Map the intensity variance
      CALL TSP_MAP_VAR(OLOC,'UPDATE',SPTR,DLOC,STATUS)

*  Reverse the intensity variance
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_REVERSE(SIZE,%VAL(SPTR))
      ELSE
          STATUS = SAI__OK
      ENDIF

*  Unmap the intensity variance
      CALL TSP_UNMAP(DLOC,STATUS)

*  Find the Stokes parameters
      CALL TSP_STOKES(OLOC,NUM,QZ,UZ,VZ,STATUS)
      IF (QZ) THEN

*  Q stokes paramaters
          CALL TSP_GET_STOKES(OLOC,'Q',SLOC,STATUS)

*  Map the Q stokes data
          CALL TSP_MAP_DATA(SLOC,'UPDATE',SPTR,DLOC,STATUS)

*  Reverse the Q stokes data
          IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_REVERSE(SIZE,%VAL(SPTR))
          ENDIF

*  Unmap the Q stokes data
          CALL TSP_UNMAP(DLOC,STATUS)

*  Map the Q stokes variance
          CALL TSP_MAP_VAR(SLOC,'UPDATE',SPTR,DLOC,STATUS)

*  Reverse the Q stokes variance
          IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_REVERSE(SIZE,%VAL(SPTR))
          ELSE
              STATUS = SAI__OK
          ENDIF

*  Unmap the Q stokes variance
          CALL TSP_UNMAP(DLOC,STATUS)
          CALL DAT_ANNUL(SLOC,STATUS)
      ENDIF
      IF (UZ) THEN

*  U Stokes parameter
          CALL TSP_GET_STOKES(OLOC,'U',SLOC,STATUS)

*  Map the U stokes data
          CALL TSP_MAP_DATA(SLOC,'UPDATE',SPTR,DLOC,STATUS)

*  Reverse the U stokes data
          IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_REVERSE(SIZE,%VAL(SPTR))
          ENDIF

*  Unmap the U stokes data
          CALL TSP_UNMAP(DLOC,STATUS)

*  Map the U stokes variance
          CALL TSP_MAP_VAR(SLOC,'UPDATE',SPTR,DLOC,STATUS)

*  Reverse the U stokes variance
          IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_REVERSE(SIZE,%VAL(SPTR))
          ELSE
              STATUS = SAI__OK
          ENDIF

*  Unmap the U stokes variance
          CALL TSP_UNMAP(DLOC,STATUS)
          CALL DAT_ANNUL(SLOC,STATUS)
      ENDIF
      IF (VZ) THEN

*  V stokes parameter
          CALL TSP_GET_STOKES(OLOC,'V',SLOC,STATUS)

*  Map the V stokes data
          CALL TSP_MAP_DATA(SLOC,'UPDATE',SPTR,DLOC,STATUS)

*  Reverse the V stokes data
          IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_REVERSE(SIZE,%VAL(SPTR))
          ENDIF

*  Unmap the V stokes data
          CALL TSP_UNMAP(DLOC,STATUS)

*  Map the V stokes variance
          CALL TSP_MAP_VAR(SLOC,'UPDATE',SPTR,DLOC,STATUS)
          IF (STATUS .EQ. SAI__OK) THEN
              CALL TSP_REVERSE(SIZE,%VAL(SPTR))
          ELSE
              STATUS = SAI__OK
          ENDIF

*  Unmap the V stokes variance
          CALL TSP_UNMAP(DLOC,STATUS)
          CALL DAT_ANNUL(SLOC,STATUS)
      ENDIF

*  Tidy up

      CALL DAT_ANNUL(OLOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      END



       SUBROUTINE TSP_REVERSE(SIZE,S)
*+
*
*   T S P _ R E V E R S E
*
*   REVERSE command
*
*   Subroutine to reverse a spectrum in the wavelength direction by
*   swapping points.
*
*   Parameters:
*    (>)  SIZE  (Integer)            The number of spectral points
*    (!)  S     (Real array(SIZE))   Array to be reversed
*
*   Jeremy Bailey   27/4/1988
*
*   Modified:
*       13/12/1991
*
*+

      IMPLICIT NONE

*  Parameters
      INTEGER SIZE
      REAL S(SIZE)

*  Local variables
      INTEGER I
      REAL X

*  Loop over half the points, swapping each one with the corresponding
*  'mirror image' point. If the array size is odd the middle point doesn't
*  need swapping

          DO I=1,SIZE/2
              X = S(I)
              S(I) = S(SIZE-I+1)
              S(SIZE-I+1) = X
          ENDDO

      END

