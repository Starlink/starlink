C+
      SUBROUTINE QUSUB(STATUS)
C
C            Q U S U B
C
C     Command name:
C        QUSUB
C
C     Function:
C        Subtract a Q,U vector from a polarization spectrum.
C
C     Description:
C        QUSUB subtracts a percentage polarization expressed as a Q,U
C        vector from a polarization spectrum. This can be used as a
C        crude correction for interstellar polarization.
C
C     Parameters:
C        INPUT      (TSP, 1D)  The input dataset, a spectrum which must
C                               have Q and U Stokes parameters present.
C        QVAL       (Real)     The Q value (per cent) to subtract.
C        UVAL       (Real)     The U value (per cent) to subtract.
C        OUTPUT     (TSP, 1D)  The corrected dataset.
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 28/2/1988
C
C-
C
C  History:
C    Jan/1988   Original Version.   JAB/AAO
C    28/2/1988   TSP Monolith version.  JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR,QPTR,UPTR

*  Data size
      INTEGER SIZE

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,OLOC,QLOC,ULOC,IDLOC,QDLOC,UDLOC

*  Q and U values to subtract
      REAL QVAL,UVAL
      INTEGER ACTDIM,DIMS(7)

*  Get the data

      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Get the Q,U vector

      CALL PAR_GET0R('QVAL',QVAL,STATUS)
      CALL PAR_GET0R('UVAL',UVAL,STATUS)

*  Create the output dataset

      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output

      CALL TSP_COPY(LOC,OLOC,STATUS)


*  Get size of data

      CALL TSP_SIZE(OLOC,7,DIMS,ACTDIM,STATUS)

*  Check it is one dimensional

      IF (ACTDIM .NE. 1) THEN
          CALL MSG_OUT('OUT','Invalid Dimensions',STATUS)
          STATUS = USER__001
      ENDIF
      SIZE = DIMS(1)

*  Map the intensity data

      CALL TSP_MAP_DATA(OLOC,'READ',IPTR,IDLOC,STATUS)

*  Get the Stokes parameters ....

      CALL TSP_GET_STOKES(OLOC,'Q',QLOC,STATUS)
      CALL TSP_GET_STOKES(OLOC,'U',ULOC,STATUS)

*  ... and map the Stokes data arrays

      CALL TSP_MAP_DATA(QLOC,'UPDATE',QPTR,QDLOC,STATUS)
      CALL TSP_MAP_DATA(ULOC,'UPDATE',UPTR,UDLOC,STATUS)

*  Do the subtraction

      IF (STATUS .EQ. SAI__OK) THEN
        CALL TSP_QUSUB(SIZE,%VAL(IPTR),%VAL(QPTR),%VAL(UPTR),
     :    QVAL,UVAL,STATUS)
      ENDIF

*  Tidy up

      CALL TSP_UNMAP(IDLOC,STATUS)
      CALL TSP_UNMAP(QDLOC,STATUS)
      CALL TSP_UNMAP(UDLOC,STATUS)
      CALL DAT_ANNUL(QLOC,STATUS)
      CALL DAT_ANNUL(ULOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      END



       SUBROUTINE TSP_QUSUB(SIZE,INT,Q,U,QVAL,UVAL,STATUS)
*+
*
*   T S P _ Q U S U B
*
*   QUSUB command
*
*   Subroutine to do the polarization vector subtraction
*
*   (>)  SIZE  (Integer)          The number of spectral points
*   (>)  INT   (Real array(SIZE)) The intensity array
*   (>)  Q     (Real array(SIZE)) The Q array
*   (>)  U     (Real array(SIZE)) The U array
*   (>)  QVAL  (Real array(SIZE)) The percentage Q value to subtract
*   (>)  UVAL  (Real array(SIZE)) The percentage U value to subtract
*   (!)  STATUS (Integer)         Status value
*
*   Jeremy Bailey   28/2/1988
*
*   Modified:
*       13/12/1991   -   Handle bad values
*
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE
      REAL INT(SIZE),Q(SIZE),U(SIZE)
      REAL QVAL,UVAL
      INTEGER STATUS

*  Local variables
      INTEGER I

      IF (STATUS .EQ. SAI__OK) THEN

*  Loop over points
          DO I=1,SIZE
              IF (INT(I) .NE. VAL__BADR) THEN

*  Subtract Q correction
                  IF (Q(I) .NE. VAL__BADR) THEN
                      Q(I)=Q(I) - QVAL*INT(I)/100.0
                  ENDIF

*  Subtract U correction
                  IF (U(I) .NE. VAL__BADR) THEN
                      U(I)=U(I) - UVAL*INT(I)/100.0
                  ENDIF
              ENDIF
          ENDDO

      ENDIF
      END

