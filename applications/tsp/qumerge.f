C+
      SUBROUTINE QUMERGE(STATUS)
C
C            Q U M E R G E
C
C     Command name:
C        QUMERGE
C
C     Function:
C        Merge Q and U spectra into single dataset.
C
C     Description:
C        QUMERGE merges two separate datasets containing Q and U Stokes
C        parameters to form a single dataset containing Q and U. It can
C        be used to combine data obtained with IPCS2STOKES or CCD2STOKES
C        where the two stokes parameters have been obtained independently.
C
C        The program CCD2POL is equivalent to using CC2STOKES to derive
C        the two Stokes parameters, and then combining them with QUMERGE.
C
C     Parameters:
C    (1) QQ         (TSP, 1D)  The input Q dataset.
C    (2) U          (TSP, 1D)  The input U dataset.
C    (3) OUTPUT     (TSP, 1D)  The output merged dataset.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         19/8/1988
C
C-
C
C  History:
C    Aug/1987   Original Version.   JAB/AAO
C    27/2/1988   TSP Monolith version.  JAB/AAO
C    19/8/1988   Prevent crash on null inputs.  JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER QPTR,UPTR,QSPTR,USPTR,QVPTR,UVPTR,OUSPTR,OUVPTR

*  HDS locators
      CHARACTER*(DAT__SZLOC) QLOC,ULOC,OLOC,QDLOC,UDLOC
      CHARACTER*(DAT__SZLOC) Q1LOC,U1LOC,QSLOC,USLOC,QVLOC,UVLOC
      CHARACTER*(DAT__SZLOC) U2LOC,OUSLOC,OUVLOC

*  Data array dimensions
      INTEGER NDIMS,DIMS(7)

*  Number of spectral channels
      INTEGER SIZE

*  Get the input locators

      CALL DAT_ASSOC('QQ','READ',QLOC,STATUS)
      CALL DAT_ASSOC('U','READ',ULOC,STATUS)

*  Create the output file

      CALL DAT_CREAT('OUTPUT','ENDIF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy Q data to output

      CALL TSP_COPY(QLOC,OLOC,STATUS)

*  Add U Stokes Parameter to the output dataset

      CALL TSP_ADD_STOKES(OLOC,'U',.TRUE.,STATUS)

*  Get the size of the Q dataset

      CALL TSP_SIZE(QLOC,7,DIMS,NDIMS,STATUS)

*  Check that it is one dimensional

      IF (NDIMS .NE. 1) THEN
         CALL MSG_OUT('MSG','Invalid Dimensions for Q data',STATUS)
         STATUS = USER__001
      ELSE
         SIZE = DIMS(1)
      ENDIF

*  Get the size of the U dataset

      CALL TSP_SIZE(ULOC,7,DIMS,NDIMS,STATUS)

*  Check that it is one dimensional

      IF (NDIMS .NE. 1) THEN
         CALL MSG_OUT('MSG','Invalid Dimensions for U data',STATUS)
         STATUS = USER__001
      ENDIF

*  Check that the dimensions match

      IF (DIMS(1) .NE. SIZE) THEN
         CALL MSG_OUT('MSG','Q and U Dimensions are different',STATUS)
         STATUS = USER__001
      ENDIF

*  Map the two intensity arrays

      CALL TSP_MAP_DATA(OLOC,'UPDATE',QPTR,QDLOC,STATUS)
      CALL TSP_MAP_DATA(ULOC,'READ',UPTR,UDLOC,STATUS)

*  Get the Stokes data from output and input datasets

      CALL TSP_GET_STOKES(OLOC,'Q',Q1LOC,STATUS)
      CALL TSP_GET_STOKES(ULOC,'U',U1LOC,STATUS)
      CALL TSP_GET_STOKES(OLOC,'U',U2LOC,STATUS)

*  Map the Stokes arrays and variances

      CALL TSP_MAP_DATA(Q1LOC,'UPDATE',QSPTR,QSLOC,STATUS)
      CALL TSP_MAP_DATA(U1LOC,'READ',USPTR,USLOC,STATUS)
      CALL TSP_MAP_VAR(Q1LOC,'UPDATE',QVPTR,QVLOC,STATUS)
      CALL TSP_MAP_VAR(U1LOC,'READ',UVPTR,UVLOC,STATUS)
      CALL TSP_MAP_DATA(U2LOC,'WRITE',OUSPTR,OUSLOC,STATUS)
      CALL TSP_MAP_VAR(U2LOC,'WRITE',OUVPTR,OUVLOC,STATUS)

*  sum the intensities and rescale the Stokes arrays and variances
*  for the new intensity

      IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_QUMERGE(SIZE,%VAL(QPTR),%VAL(UPTR),%VAL(QSPTR),
     :      %VAL(USPTR),%VAL(QVPTR),%VAL(UVPTR),%VAL(OUSPTR),
     :      %VAL(OUVPTR),STATUS)
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(UVLOC,STATUS)
      CALL TSP_UNMAP(QVLOC,STATUS)
      CALL TSP_UNMAP(USLOC,STATUS)
      CALL TSP_UNMAP(OUVLOC,STATUS)
      CALL TSP_UNMAP(OUSLOC,STATUS)
      CALL TSP_UNMAP(QSLOC,STATUS)
      CALL DAT_ANNUL(Q1LOC,STATUS)
      CALL DAT_ANNUL(U1LOC,STATUS)
      CALL DAT_ANNUL(U2LOC,STATUS)
      CALL TSP_UNMAP(QDLOC,STATUS)
      CALL TSP_UNMAP(UDLOC,STATUS)
      CALL DAT_ANNUL(QLOC,STATUS)
      CALL DAT_ANNUL(ULOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      END


      SUBROUTINE TSP_QUMERGE(SIZE,INT,IU,QS,US,QV,UV,OUS,OUV,STATUS)
*+
*
*   T S P _ Q U M E R G E
*
*   QUMERGE command
*
*   Subroutine to do the merging of the data
*
*    Sum the intensities for the two datasets, and scale up the
*    Stokes arrays and variances to be correct for the new intensity
*
*   Parameters
*
*   (>)   SIZE   (Integer)           The number of spectral points
*   (!)   INT    (Real array(SIZE))  The intensity array
*   (>)   IU     (Real array(SIZE))  The intensity from the U data set
*   (!)   QS     (Real array(SIZE))  The Q stokes parameter array
*   (>)   US     (Real array(SIZE))  The U stokes parameter array
*   (!)   QV     (Real array(SIZE))  The Q variance array
*   (>)   UV     (Real array(SIZE))  The U variance array
*   (<)   OUS    (Real array(SIZE))  The output U stokes parameter array
*   (<)   OUV    (Real array(SIZE))  The output U variance array
*   (!)   STATUS (Integer)           Status value
*
*    Jeremy Bailey   19/8/1988
*
*   Modified:
*      11/12/1991  -  Handle bad values
*
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  parameters
      INTEGER SIZE
      REAL INT(SIZE),IU(SIZE),QS(SIZE),US(SIZE),QV(SIZE),UV(SIZE)
      REAL OUS(SIZE),OUV(SIZE)

*  local variables
      REAL QFAC,UFAC,NEWINT,I1S,I2S
      INTEGER STATUS
      INTEGER I

      IF (STATUS .EQ. SAI__OK) THEN

*  Zero initial values of summed intensities
         I1S = 0.0
         I2S = 0.0

*  Loop over all good points in data set summing the intensities
         DO I=1,SIZE
            IF (INT(I).NE.VAL__BADR .AND. IU(I).NE.VAL__BADR) THEN
               I1S = I1S+INT(I)
               I2S = I2S+IU(I)
            ENDIF
         ENDDO

*  Determine scaling factor for Q data according to fraction of intensity
*  in the Q dataset

         NEWINT=I1S+I2S
         IF (I1S .GT. 0.0) THEN
            QFAC=NEWINT/I1S
         ELSE
            QFAC=0.0
         ENDIF

*  Determine scaling factor for U data according to fraction of intensity
*  in the U dataset

         IF (I2S .GT. 0.0) THEN
            UFAC=NEWINT/I2S
         ELSE
            UFAC=0.0
         ENDIF

*  Loop over points scaling all the Stokes parameters according to the
*  factors just determined.

         DO I=1,SIZE
            IF (QS(I) .NE. VAL__BADR) QS(I)=QS(I)*QFAC
            IF (US(I) .NE. VAL__BADR) THEN
                OUS(I)=US(I)*UFAC
            ELSE
                OUS(I)=VAL__BADR
            ENDIF

*  Scale the variances by the square of the factor

            IF (QV(I) .NE. VAL__BADR) QV(I)=QV(I)*QFAC*QFAC
            IF (UV(I) .NE. VAL__BADR) THEN
                OUV(I)=UV(I)*UFAC*UFAC
            ELSE
                OUV(I)=VAL__BADR
            ENDIF

*  And sum the intensities from the two datasets

            IF (INT(I) .NE. VAL__BADR .AND. IU(I).NE.VAL__BADR) THEN
                INT(I)=INT(I)+IU(I)
            ELSE
                INT(I)=VAL__BADR
            ENDIF
         ENDDO
      ENDIF
      END

