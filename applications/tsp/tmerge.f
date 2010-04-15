C+
      SUBROUTINE TMERGE(STATUS)
C
C            T M E R G E
C
C     Command name:
C        TMERGE
C
C     Function:
C        Merge two time series datasets.
C
C     Description:
C        TMERGE merges two time series datasets covering different times
C        but with the same number of wavelength channels, to form a single
C        dataset. The files should be merged in their time order, i.e. INPUT1
C        should be earlier than INPUT2 to retain increasing time in the
C        merged dataset. The operation can be applied to both 2D or 3D
C        datasets
C
C     Parameters:
C    (1) INPUT1     (TSP, 2D or 3D)  The first input dataset.
C    (2) INPUT2     (TSP, 2D or 3D)  The second input dataset.
C    (3) OUTPUT     (TSP, 2D or 3D)  The output merged dataset.
C
C     Support:
C          Jeremy Bailey, AAO
C
C     Version date:
C          31/10/1989
C
C-
C
C  History:
C    Nov/1987   Original Version.   JAB/AAO
C    27/2/1988   TSP Monolith version.  JAB/AAO
C    31/10/1989  Allow 3D data          JAB/JAC
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS
      INTEGER STAT1

*  Data pointers
      INTEGER PTR1,PTR2

*  HDS locators
      CHARACTER*(DAT__SZLOC) ILOC,OLOC,ILOC1,SLOC,SLOC2
      CHARACTER*(DAT__SZLOC) LOC2,ILOC2,LOC1

*  Array dimensions
      INTEGER NDIMS,DIMS(7),DIMS1(7),DIMS2(7)

*  Array sizes
      INTEGER SIZE,SIZE1,SIZE2,NEWSIZE
      CHARACTER*40 LABEL1,LABEL2,UNITS1,UNITS2

*  Get the input and output locators

*  Get first input file
      CALL DAT_ASSOC('INPUT1','READ',LOC1,STATUS)

*  Get second input file
      CALL DAT_ASSOC('INPUT2','READ',LOC2,STATUS)

*  Create output file
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy dataset1 to output

      CALL TSP_COPY(LOC1,OLOC,STATUS)

*  Get size of first dataset
      CALL TSP_SIZE(LOC1,7,DIMS1,NDIMS,STATUS)

*  Check that it is 2 or 3 dimensional
      IF (NDIMS .NE. 2 .AND. NDIMS .NE. 3) THEN
         CALL MSG_OUT(' ','Invalid Dimensions for 1st dataset',
     :       STATUS)
         STATUS = USER__001
      ELSE IF (NDIMS .EQ. 2) THEN

*  Set size for 2 dimensional array
         SIZE = DIMS1(1)
         SIZE1 = DIMS1(2)
      ELSE IF (NDIMS .EQ. 3) THEN

*  If it is 3 dimensional pretend it is 2D by setting SIZE to the product
*  of the first two dimensions
         SIZE = DIMS1(1)*DIMS1(2)
         SIZE1 = DIMS1(3)
      ENDIF

*  Get size of second dataset
      CALL TSP_SIZE(LOC2,7,DIMS2,NDIMS,STATUS)

*  Check it is 2 or 3 dimesional
      IF (NDIMS .NE. 2 .AND. NDIMS .NE. 3) THEN
         CALL MSG_OUT(' ','Invalid Dimensions for 2nd dataset',
     :       STATUS)
         STATUS = USER__001
      ENDIF

*  Copy size of time axis
      IF (NDIMS .EQ. 3) THEN
          SIZE2 = DIMS2(3)
      ELSE IF (NDIMS .EQ. 2) THEN
          SIZE2 = DIMS2(2)
      ENDIF

*  Form size of output dataset
      NEWSIZE = SIZE1 + SIZE2

*  Set up dimesions of output dataset

      DIMS(1) = DIMS1(1)
      IF (NDIMS .EQ. 2) THEN
          DIMS(2) = NEWSIZE
      ELSE
          DIMS(2) = DIMS1(2)
          DIMS(3) = NEWSIZE
      ENDIF

*  Resize output dataset
      CALL TSP_RESIZE(OLOC,NDIMS,DIMS,STATUS)

*  Copy the data from the second dataset into the output dataset

*  Copy the Time axis data

      IF (STATUS .EQ. SAI__OK) THEN

*  Get time axis label and units
        CALL TSP_RLU_TIME(LOC1,LABEL1,UNITS1,STATUS)
        CALL TSP_RLU_TIME(LOC2,LABEL2,UNITS2,STATUS)

*  Check that they match
        IF (LABEL1 .NE. LABEL2) THEN
          CALL MSG_OUT('MSG','Warning - Time Scales are Different',
     :     STATUS)
          CALL MSG_OUT('MSG','1 - '//LABEL1,STATUS)
          CALL MSG_OUT('MSG','2 - '//LABEL2,STATUS)
        ENDIF

*  Map time axis of output data
        CALL TSP_MAP_TIME(OLOC,'UPDATE',PTR1,ILOC,STATUS)

*  Map time axis of second dataset
        CALL TSP_MAP_TIME(LOC2,'READ',PTR2,ILOC2,STATUS)

*  Copy the time axis data
        IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_TMERGECPYD(SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ENDIF

*  Unmap arrays
        CALL TSP_UNMAP(ILOC,STATUS)
        CALL TSP_UNMAP(ILOC2,STATUS)

*  Copy the intensity data

*  Map the intensity data from the first dataset
        CALL TSP_MAP_DATA(OLOC,'UPDATE',PTR1,ILOC,STATUS)

*  Map the intensity data from the second dataset
        CALL TSP_MAP_DATA(LOC2,'READ',PTR2,ILOC2,STATUS)

*  Copy the intensity data
        IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_TMERGECPY(SIZE,SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ENDIF

*  Unmap arrays
        CALL TSP_UNMAP(ILOC,STATUS)
        CALL TSP_UNMAP(ILOC2,STATUS)

*  Copy the intensity variance

*  Map the intensity variance for the output dataset
        CALL TSP_MAP_VAR(OLOC,'UPDATE',PTR1,ILOC,STATUS)

*  Map the intensity variance for the second dataset
        CALL TSP_MAP_VAR(LOC2,'READ',PTR2,ILOC2,STATUS)

*  Copy the intensity variance
        IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_TMERGECPY(SIZE,SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ELSE
          CALL ERR_ANNUL(STATUS)
        ENDIF

*  Unmap the arrays
        CALL TSP_UNMAP(ILOC,STATUS)
        CALL TSP_UNMAP(ILOC2,STATUS)
        STATUS = SAI__OK

*  Copy the Q Stokes data

*  Get the Q stokes parameter
        CALL TSP_GET_STOKES(OLOC,'Q',SLOC,STATUS)

*  Map the Q stokes data for the output dataset
        CALL TSP_MAP_DATA(SLOC,'UPDATE',PTR1,ILOC,STATUS)
        STAT1 = STATUS

*  Map the Q stokes data for the second dataset
        CALL TSP_GET_STOKES(LOC2,'Q',SLOC2,STATUS)
        CALL TSP_MAP_DATA(SLOC2,'READ',PTR2,ILOC2,STATUS)

*  Copy the Q stokes data
        IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_TMERGECPY(SIZE,SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ELSE
         CALL ERR_ANNUL(STATUS)
         IF (STAT1 .EQ. SAI__OK) THEN
           CALL TSP_TMERGEZERO(SIZE,SIZE2,NEWSIZE,%VAL(PTR1))
         ELSE
           CALL ERR_ANNUL(STAT1)
         ENDIF
        ENDIF

*  Unmap the arrays
        CALL TSP_UNMAP(ILOC,STATUS)
        CALL TSP_UNMAP(ILOC2,STATUS)

*  Copy the Q Stokes Variance

*  Map the Q stokes variance from the output array
        CALL TSP_MAP_VAR(SLOC,'UPDATE',PTR1,ILOC,STATUS)
        STAT1 = STATUS

*  Map the Q stokes variance from the second dataset
        CALL TSP_MAP_VAR(SLOC2,'READ',PTR2,ILOC2,STATUS)

*  Combine the Q stokes variances
        IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_TMERGECPY(SIZE,SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ELSE
         CALL ERR_ANNUL(STATUS)
         IF (STAT1 .EQ. SAI__OK) THEN
          CALL TSP_TMERGEZERO(SIZE,SIZE2,NEWSIZE,%VAL(PTR1))
         ELSE
          CALL ERR_ANNUL(STAT1)
         ENDIF
        ENDIF

*  Unmap the arrays
        CALL TSP_UNMAP(ILOC,STATUS)
        CALL TSP_UNMAP(ILOC2,STATUS)
        CALL DAT_ANNUL(SLOC,STATUS)
        CALL DAT_ANNUL(SLOC2,STATUS)
        STATUS = SAI__OK

*  Copy the U Stokes data

*  Get the U stokes parameter
        CALL TSP_GET_STOKES(OLOC,'U',SLOC,STATUS)

*  Map the U stokes data from the output dataset
        CALL TSP_MAP_DATA(SLOC,'UPDATE',PTR1,ILOC,STATUS)
        STAT1 = STATUS

*  Map the U stokes data from the second dataset
        CALL TSP_GET_STOKES(LOC2,'U',SLOC2,STATUS)
        CALL TSP_MAP_DATA(SLOC2,'READ',PTR2,ILOC2,STATUS)

*  Copy the U stokes data
        IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_TMERGECPY(SIZE,SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ELSE
         CALL ERR_ANNUL(STATUS)
         IF (STAT1 .EQ. SAI__OK) THEN
          CALL TSP_TMERGEZERO(SIZE,SIZE2,NEWSIZE,%VAL(PTR1))
         ELSE
          CALL ERR_ANNUL(STAT1)
         ENDIF
        ENDIF

*  Unmap the arrays
        CALL TSP_UNMAP(ILOC,STATUS)
        CALL TSP_UNMAP(ILOC2,STATUS)
        STATUS = SAI__OK

*  Copy the U Stokes Variance

*  Map the U Stokes variance from the output dataset
        CALL TSP_MAP_VAR(SLOC,'UPDATE',PTR1,ILOC,STATUS)
        STAT1 = STATUS

*  Map the U stokes variance from the second dataset
        CALL TSP_MAP_VAR(SLOC2,'READ',PTR2,ILOC2,STATUS)

*  Copy the U stokes variance
        IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_TMERGECPY(SIZE,SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ELSE
         CALL ERR_ANNUL(STATUS)
         IF (STAT1 .EQ. SAI__OK) THEN
          CALL TSP_TMERGEZERO(SIZE,SIZE2,NEWSIZE,%VAL(PTR1))
         ELSE
          CALL ERR_ANNUL(STAT1)
         ENDIF
        ENDIF

*  Unmap the arrays
        CALL TSP_UNMAP(ILOC,STATUS)
        CALL TSP_UNMAP(ILOC2,STATUS)
        CALL DAT_ANNUL(SLOC,STATUS)
        CALL DAT_ANNUL(SLOC2,STATUS)
        STATUS = SAI__OK

*  Copy the V Stokes data

*  Get the V stokes parameter
        CALL TSP_GET_STOKES(OLOC,'V',SLOC,STATUS)

*  Map the V stokes data from the output dataset
        CALL TSP_MAP_DATA(SLOC,'UPDATE',PTR1,ILOC,STATUS)
        STAT1 = STATUS

*  Map the V stokes data from the second dataset
        CALL TSP_GET_STOKES(LOC2,'V',SLOC2,STATUS)
        CALL TSP_MAP_DATA(SLOC2,'READ',PTR2,ILOC2,STATUS)

*  Copy the V stokes parameter
        IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_TMERGECPY(SIZE,SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ELSE
         CALL ERR_ANNUL(STATUS)
         IF (STAT1 .EQ. SAI__OK) THEN
          CALL TSP_TMERGEZERO(SIZE,SIZE2,NEWSIZE,%VAL(PTR1))
         ELSE
          CALL ERR_ANNUL(STAT1)
         ENDIF
        ENDIF

*  Unmap arrays
        CALL TSP_UNMAP(ILOC,STATUS)
        CALL TSP_UNMAP(ILOC2,STATUS)
        STATUS = SAI__OK

*  Copy the V Stokes Variance

*  Map the V stokes variance for the output dataset
        CALL TSP_MAP_VAR(SLOC,'UPDATE',PTR1,ILOC,STATUS)
        STAT1 = STATUS

*  Map the V stokes variance for the second dataset
        CALL TSP_MAP_VAR(SLOC2,'READ',PTR2,ILOC2,STATUS)

*  Copy the V stokes variance
        IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_TMERGECPY(SIZE,SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ELSE
         CALL ERR_ANNUL(STATUS)
         IF (STAT1 .EQ. SAI__OK) THEN
          CALL TSP_TMERGEZERO(SIZE,SIZE2,NEWSIZE,%VAL(PTR1))
         ELSE
          CALL ERR_ANNUL(STAT1)
         ENDIF
        ENDIF

*  Unmap the arrays
        CALL TSP_UNMAP(ILOC,STATUS)
        CALL TSP_UNMAP(ILOC2,STATUS)
        CALL DAT_ANNUL(SLOC,STATUS)
        CALL DAT_ANNUL(SLOC2,STATUS)
        STATUS = SAI__OK

      ENDIF
      CALL DAT_ANNUL(LOC1,STATUS)
      CALL DAT_ANNUL(LOC2,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)


      END


      SUBROUTINE TSP_TMERGECPY(D1,D2,DS,OUT,IN)
*+
*
*   T S P _ T M E R G E C P Y
*
*   TMERGE command
*
*   Copy the array IN into the last D2 elements of the output array
*   for all wavelength channels.
*   Used to merge the intensity, variances, stokes parameters.
*
*   Parameters:
*
*   (>)  D1    (Integer)          First dimension of arrays
*   (>)  D2    (Integer)          Size of input array
*   (>)  DS    (Integer)          Size of output array
*   (!)  OUT   (Real array(DS))   Output array
*   (>)  IN    (Real array(D2))   Input array
*
*   Jeremy Bailey   15/8/1990
*
*   Modified:
*       10/12/1991
*+


      IMPLICIT NONE

*  Parameters
      INTEGER D1,D2,DS
      REAL IN(D1,D2),OUT(D1,DS)

*  Local variables
      INTEGER I,IS,J

      IS = DS-D2

*  Copy input into last D2 elements of output
      DO I=1,D2
          DO J=1,D1
              OUT(J,I+IS) = IN(J,I)
          ENDDO
      ENDDO
      END


      SUBROUTINE TSP_TMERGEZERO(D1,D2,DS,OUT)
*+
*
*   T S P _ T M E R G E Z E R O
*
*   TMERGE command
*
*   Fill the last D2 elements of the output array with zeros for each
*   wavelength channel.
*   This is used in case where the second input array does not have
*   a Stokes parameter which is in the first input array.
*
*   Parameters:
*
*   (>)  D1    (Integer)          First dimension of arrays
*   (>)  D2    (Integer)          Number of elements to fill with zero
*   (>)  DS    (Integer)          Size of output array
*   (!)  OUT   (Real array(DS))   Output array
*
*   Jeremy Bailey   15/8/1990
*
*   Modified:
*       10/12/1991
*
*+


      IMPLICIT NONE

*  Parameters
      INTEGER D1,D2,DS
      REAL OUT(D1,DS)

*  Local variables
      INTEGER I,IS,J

      IS = DS-D2

*  Fill last D2 elements with zero
      DO I=1,D2
          DO J=1,D1
              OUT(J,I+IS) = 0.0
          ENDDO
      ENDDO
      END


      SUBROUTINE TSP_TMERGECPYD(D2,DS,OUT,IN)
*+
*
*   T S P _ T M E R G E C P Y D
*
*   TMERGE
*
*   Copy the array IN into the last D2 elements of the output array.
*   Double precision version used to merge the time axis arrays.
*
*   Parameters:
*
*   (>)  D2    (Integer)          Size of input array
*   (>)  DS    (Integer)          Size of output array
*   (!)  OUT   (Double array(DS)) Output array
*   (>)  IN    (Double array(D2)) Input array
*
*   Jeremy Bailey   15/8/1990
*
*   Modified:
*       10/12/1991
*+


      IMPLICIT NONE

*  Parameters
      INTEGER D2,DS
      DOUBLE PRECISION IN(D2),OUT(DS)

*  Local variables
      INTEGER I,IS

      IS = DS-D2

*  Copy input into last D2 elements of output
      DO I=1,D2
          OUT(I+IS) = IN(I)
      ENDDO
      END

