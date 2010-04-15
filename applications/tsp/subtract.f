C+
      SUBROUTINE SUBTRACT(STATUS)
C
C            S U B T R A C T
C
C     Command name:
C        SUBTRACT
C
C     Function:
C        Subtract two Polarization spectra.
C
C     Description:
C        Two Polarization spectra covering the same wavelength range
C        are subtracted to form a new spectrum giving the difference of
C        the intensity and Stokes parameters.
C
C        Any number of Stokes parameters may be present in the
C        spectra, but only Stokes parameters present in both spectra
C        will appear in the output spectrum.
C
C
C
C     Parameters:
C    (1) INPUT1     (TSP, 1D)  The first input Stokes spectrum.
C    (2) INPUT2     (TSP, 1D)  The second input Stokes spectrum.
C    (3) OUTPUT     (TSP, 1D)  The output dataset.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         4/12/1988
C
C-
C
C  History:
C    27/4/1988   Original Version (COMBINE).   JAB/AAO
C    15/8/1988   Allow more than one stokes parameter in
C                the input spectra.   JAB/AAO
C    19/8/1988   Correct bug in above change.   JAB/AAO
C    4/12/1988   Modified as SUBTRACT    JAB/JAC
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER PTR1,PTR2,SPTR1,SPTR2,VPTR1,VPTR2

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC1,LOC2,OLOC,DLOC1,DLOC2
      CHARACTER*(DAT__SZLOC) SDLOC1,SDLOC2,VDLOC1,VDLOC2
      CHARACTER*(DAT__SZLOC) QLOC1,QLOC2,ULOC1,ULOC2,VLOC1,VLOC2

*  Array dimensions
      INTEGER NDIMS,DIMS(7)

*  Size of arrays
      INTEGER SIZE

*  Number of Stokes parameters
      INTEGER NUM

*  Stokes parameter flags in the two arrays
      LOGICAL QZ1,UZ1,VZ1,QZ2,UZ2,VZ2

*  Get the input locators

      CALL DAT_ASSOC('INPUT1','READ',LOC1,STATUS)
      CALL DAT_ASSOC('INPUT2','READ',LOC2,STATUS)

*  Create the output file
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output
      CALL TSP_COPY(LOC1,OLOC,STATUS)

*  Get the intensity data from both datasets and check dimensions

*  Get size of first dataset
      CALL TSP_SIZE(LOC1,7,DIMS,NDIMS,STATUS)

*  Check that it is one dimensional
      IF (NDIMS .NE. 1) THEN
         CALL MSG_OUT('MSG','Invalid Dimensions for INPUT1',STATUS)
         STATUS = USER__001
      ELSE
         SIZE = DIMS(1)
      ENDIF

*  Get size of second dataset
      CALL TSP_SIZE(LOC2,7,DIMS,NDIMS,STATUS)

*  Check that it is one dimensional
      IF (NDIMS .NE. 1) THEN
         CALL MSG_OUT('MSG','Invalid Dimensions for INPUT2',STATUS)
         STATUS = USER__001
      ENDIF

*  Check that the size of the two datasets are the same
      IF (DIMS(1) .NE. SIZE) THEN
         CALL MSG_OUT('MSG','Input 1 and 2 Dimensions are different',
     :          STATUS)
         STATUS = USER__001
      ENDIF

*  Map the intensity data from both datasets
      CALL TSP_MAP_DATA(OLOC,'UPDATE',PTR1,DLOC1,STATUS)
      CALL TSP_MAP_DATA(LOC2,'READ',PTR2,DLOC2,STATUS)

*  Get the Stokes data from output dataset

      CALL TSP_STOKES(OLOC,NUM,QZ1,UZ1,VZ1,STATUS)

*  Q Stokes parameter
      IF (QZ1) THEN
          CALL TSP_GET_STOKES(OLOC,'Q',QLOC1,STATUS)
      ENDIF

*  U Stokes parameter
      IF (UZ1) THEN
          CALL TSP_GET_STOKES(OLOC,'U',ULOC1,STATUS)
      ENDIF

*  V Stokes parameter
      IF (VZ1) THEN
          CALL TSP_GET_STOKES(OLOC,'V',VLOC1,STATUS)
      ENDIF

*  Get the Stokes data from the second dataset
      CALL TSP_STOKES(LOC2,NUM,QZ2,UZ2,VZ2,STATUS)

*  Q Stokes parameter
      IF (QZ2) THEN
          CALL TSP_GET_STOKES(LOC2,'Q',QLOC2,STATUS)
      ENDIF

*  U Stokes parameter
      IF (UZ2) THEN
          CALL TSP_GET_STOKES(LOC2,'U',ULOC2,STATUS)
      ENDIF

*  V Stokes parameter
      IF (VZ2) THEN
          CALL TSP_GET_STOKES(LOC2,'V',VLOC2,STATUS)
      ENDIF

*  Subtract the Stokes arrays and sum the variances

      IF (QZ1) THEN

*  Map the Q stokes data from the first dataset
          CALL TSP_MAP_DATA(QLOC1,'UPDATE',SPTR1,SDLOC1,STATUS)

*  Map the Q stokes variance from the first dataset
          CALL TSP_MAP_VAR(QLOC1,'UPDATE',VPTR1,VDLOC1,STATUS)
          IF (QZ2) THEN

*  Map the Q stokes data from the second dataset
              CALL TSP_MAP_DATA(QLOC2,'READ',SPTR2,SDLOC2,STATUS)

*  Map the Q stokes variance from the second dataset
              CALL TSP_MAP_VAR(QLOC2,'READ',VPTR2,VDLOC2,STATUS)

*  Subtract the Q stokes data
              CALL TSP_SUBTRACT(SIZE,%VAL(SPTR1),%VAL(SPTR2))

*  Add the Q stokes variance
              CALL TSP_SCOMBINE(SIZE,%VAL(VPTR1),%VAL(VPTR2))

*  Unmap the arrays
              CALL TSP_UNMAP(SDLOC2,STATUS)
              CALL TSP_UNMAP(VDLOC2,STATUS)
              CALL DAT_ANNUL(QLOC2,STATUS)
          ELSE
              CALL MSG_OUT(' ','Q present in only one dataset',STATUS)
              CALL TSP_DELETE_STOKES(LOC1,'Q',STATUS)
          ENDIF
          CALL TSP_UNMAP(SDLOC1,STATUS)
          CALL TSP_UNMAP(VDLOC1,STATUS)
          CALL DAT_ANNUL(QLOC1,STATUS)
      ENDIF

      IF (UZ1) THEN

*  Map the U Stokes data from the first dataset
          CALL TSP_MAP_DATA(ULOC1,'UPDATE',SPTR1,SDLOC1,STATUS)

*  Map the U Stokes variance from the first dataset
          CALL TSP_MAP_VAR(ULOC1,'UPDATE',VPTR1,VDLOC1,STATUS)
          IF (UZ2) THEN

*  Map the U Stokes data from the second dataset
              CALL TSP_MAP_DATA(ULOC2,'READ',SPTR2,SDLOC2,STATUS)

*  Map the U Stokes variance from the second dataset
              CALL TSP_MAP_VAR(ULOC2,'READ',VPTR2,VDLOC2,STATUS)

*  Subtract the U Stokes data
              CALL TSP_SUBTRACT(SIZE,%VAL(SPTR1),%VAL(SPTR2))

*  Add the U Stokes variance
              CALL TSP_SCOMBINE(SIZE,%VAL(VPTR1),%VAL(VPTR2))

*  Unmap the arrays
              CALL TSP_UNMAP(SDLOC2,STATUS)
              CALL TSP_UNMAP(VDLOC2,STATUS)
              CALL DAT_ANNUL(ULOC2,STATUS)
          ELSE
              CALL MSG_OUT(' ','U present in only one dataset',STATUS)
              CALL TSP_DELETE_STOKES(LOC1,'U',STATUS)
          ENDIF
          CALL TSP_UNMAP(SDLOC1,STATUS)
          CALL TSP_UNMAP(VDLOC1,STATUS)
          CALL DAT_ANNUL(ULOC1,STATUS)
      ENDIF

      IF (VZ1) THEN

*  Map the V stokes data from the first dataset
          CALL TSP_MAP_DATA(VLOC1,'UPDATE',SPTR1,SDLOC1,STATUS)

*  Map the V stokes variance from the first dataset
          CALL TSP_MAP_VAR(VLOC1,'UPDATE',VPTR1,VDLOC1,STATUS)
          IF (VZ2) THEN

*  Map the V stokes data from the second dataset
              CALL TSP_MAP_DATA(VLOC2,'READ',SPTR2,SDLOC2,STATUS)

*  Map the V stokes variance from the second dataset
              CALL TSP_MAP_VAR(VLOC2,'READ',VPTR2,VDLOC2,STATUS)

*  Subtract the V stokes parameter arrays
              CALL TSP_SUBTRACT(SIZE,%VAL(SPTR1),%VAL(SPTR2))

*  Add the V stokes variance arrays
              CALL TSP_SCOMBINE(SIZE,%VAL(VPTR1),%VAL(VPTR2))

*  Unmap the arrays
              CALL TSP_UNMAP(SDLOC2,STATUS)
              CALL TSP_UNMAP(VDLOC2,STATUS)
              CALL DAT_ANNUL(VLOC2,STATUS)
          ELSE
              CALL MSG_OUT(' ','V present in only one dataset',STATUS)
              CALL TSP_DELETE_STOKES(LOC1,'V',STATUS)
          ENDIF
          CALL TSP_UNMAP(SDLOC1,STATUS)
          CALL TSP_UNMAP(VDLOC1,STATUS)
          CALL DAT_ANNUL(VLOC1,STATUS)
      ENDIF

*  subtract the intensities

      IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_SUBTRACT(SIZE,%VAL(PTR1),%VAL(PTR2))
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(DLOC1,STATUS)
      CALL TSP_UNMAP(DLOC2,STATUS)
      CALL DAT_ANNUL(LOC1,STATUS)
      CALL DAT_ANNUL(LOC2,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      END


      SUBROUTINE TSP_SCOMBINE(SIZE,I1,I2)
*+
*
*   T S P _ S C O M B I N E
*
*   SUBTRACT command
*
*   Subroutine to add the data from two input arrays - used to combine the
*   variances from the two datasets
*
*   Parameters:
*
*   (>)   SIZE    (Integer)          Size of the data arrays
*   (!)   I1      (Real array(SIZE)) First input array (and output)
*   (>)   I2      (Real array(SIZE)) Second input array
*
*   Jeremy Bailey   4/12/1988
*
*   Modified:
*      17/12/1991   -  Handle bad values
*
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL I1(SIZE),I2(SIZE)

*  Local variables
      INTEGER I

*  Loop over points adding the data when both are good
      DO I=1,SIZE
        IF (I1(I) .NE. VAL__BADR .AND. I2(I) .NE. VAL__BADR) THEN
          I1(I)=I1(I)+I2(I)
        ELSE
          I1(I)=VAL__BADR
        ENDIF
      ENDDO
      END


      SUBROUTINE TSP_SUBTRACT(SIZE,I1,I2)
*+
*
*   T S P _ S U B T R A C T
*
*   SUBTRACT command
*
*   Subroutine to subtract the data from two input arrays - used to combine the
*   intensity and Stokes parameters from the two datasets
*
*   Parameters:
*
*   (>)   SIZE    (Integer)          Size of the data arrays
*   (!)   I1      (Real array(SIZE)) First input array (and output)
*   (>)   I2      (Real array(SIZE)) Second input array
*
*   Jeremy Bailey   4/12/1988
*
*   Modified:
*      17/12/1991   -  Handle bad values
*
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL I1(SIZE),I2(SIZE)

*  Local variables
      INTEGER I

*  Loop over points adding the data when both are good
      DO I=1,SIZE
        IF (I1(I) .NE. VAL__BADR .AND. I2(I) .NE. VAL__BADR) THEN
          I1(I)=I1(I)-I2(I)
        ELSE
          I1(I)=VAL__BADR
        ENDIF
      ENDDO
      END


