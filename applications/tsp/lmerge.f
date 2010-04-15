C+
      SUBROUTINE LMERGE(STATUS)
C
C            L M E R G E
C
C     Command name:
C        LMERGE
C
C     Function:
C        Merge two polarization spectra.
C
C     Description:
C        LMERGE merges two polarization spectra covering different wavelength
C        ranges, to form a single dataset.
C
C        LMERGE simply appends the data from the second dataset to the first.
C        There is no guarantee that the output data will thus be in order
C        of increasing wavelength and this may cause problems for some other
C        programs. To ensure this does not occur the two datasets can be
C        SUBSETed or SCRUNCHed before merging so that they do not overlap,
C        and should be merged with the higher wavelength dataset as the
C        second input file.
C
C     Parameters:
C    (1) INPUT1     (TSP, 1D)  The first input dataset.
C    (2) INPUT2     (TSP, 1D)  The second input dataset.
C    (3) OUTPUT     (TSP, 1D)  The output merged dataset.
C
C     Support: Jeremy Bailey, JAC
C
C     Version date: 15/8/1990
C
C-
C
C  History:
C    15/8/1990   Original Version.   JAB/AAO
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

*  Get the input file locators

      CALL DAT_ASSOC('INPUT1','READ',LOC1,STATUS)
      CALL DAT_ASSOC('INPUT2','READ',LOC2,STATUS)

*  Create the output file

      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy dataset1 to output

      CALL TSP_COPY(LOC1,OLOC,STATUS)

*  Get size of first dataset

      CALL TSP_SIZE(LOC1,7,DIMS1,NDIMS,STATUS)

*  Check it is one dimensional

      IF (NDIMS .NE. 1) THEN
         CALL MSG_OUT('MSG','Invalid Dimensions for 1st dataset',
     :       STATUS)
         STATUS = USER__001
      ELSE
         SIZE1 = DIMS1(1)
      ENDIF
      CALL TSP_SIZE(LOC2,7,DIMS2,NDIMS,STATUS)

*  Check second dataset is one dimensional

      IF (NDIMS .NE. 1) THEN
         CALL MSG_OUT('MSG','Invalid Dimensions for 2nd dataset',
     :       STATUS)
         STATUS = USER__001
      ENDIF
      SIZE2 = DIMS2(1)

*  Output size is sum of two input sizes

      NEWSIZE = SIZE1 + SIZE2

*  Resize the output dataset

      DIMS(1)=NEWSIZE
      CALL TSP_RESIZE(OLOC,1,DIMS,STATUS)
      IF (STATUS .EQ. SAI__OK) THEN

*  Map the wavelength axes

        CALL TSP_MAP_LAMBDA(OLOC,'UPDATE',PTR1,ILOC,STATUS)
        CALL TSP_MAP_LAMBDA(LOC2,'READ',PTR2,ILOC2,STATUS)

*  Copy the wavelength axis data

        IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_LMERGECPY(SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ENDIF

*  Unmap arrays

        CALL TSP_UNMAP(ILOC,STATUS)
        CALL TSP_UNMAP(ILOC2,STATUS)

*  Map the intensity data

        CALL TSP_MAP_DATA(OLOC,'UPDATE',PTR1,ILOC,STATUS)
        CALL TSP_MAP_DATA(LOC2,'READ',PTR2,ILOC2,STATUS)

*  Copy the intensity data

        IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_LMERGECPY(SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ENDIF

*  Unmap the arrays

        CALL TSP_UNMAP(ILOC,STATUS)
        CALL TSP_UNMAP(ILOC2,STATUS)

*  Map the intensity variances

        CALL TSP_MAP_VAR(OLOC,'UPDATE',PTR1,ILOC,STATUS)
        CALL TSP_MAP_VAR(LOC2,'READ',PTR2,ILOC2,STATUS)

*  Copy the intensity variance

        IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_LMERGECPY(SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ENDIF

*  Unmap the arrays

        CALL TSP_UNMAP(ILOC,STATUS)
        CALL TSP_UNMAP(ILOC2,STATUS)

*  Set status OK in case there wasn't a variance

        STATUS = SAI__OK

*  Copy the Q Stokes data - With all Stokes parameters, if only the first
*  array contains the Stokes parameter the second part of the array is
*  filled with zeros

        CALL TSP_GET_STOKES(OLOC,'Q',SLOC,STATUS)
        CALL TSP_MAP_DATA(SLOC,'UPDATE',PTR1,ILOC,STATUS)
        STAT1 = STATUS
        CALL TSP_GET_STOKES(LOC2,'Q',SLOC2,STATUS)
        CALL TSP_MAP_DATA(SLOC2,'READ',PTR2,ILOC2,STATUS)
        IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_LMERGECPY(SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ELSE IF (STAT1 .EQ. SAI__OK) THEN
         CALL TSP_LMERGEZERO(SIZE2,NEWSIZE,%VAL(PTR1))
        ENDIF

*  Unmap arrays

        CALL TSP_UNMAP(ILOC,STATUS)
        CALL TSP_UNMAP(ILOC2,STATUS)
        STATUS = SAI__OK

*  Copy the Q Stokes Variance

        CALL TSP_MAP_VAR(SLOC,'UPDATE',PTR1,ILOC,STATUS)
        STAT1 = STATUS
        CALL TSP_MAP_VAR(SLOC2,'READ',PTR2,ILOC2,STATUS)
        IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_LMERGECPY(SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ELSE IF (STAT1 .EQ. SAI__OK) THEN
         CALL TSP_LMERGEZERO(SIZE2,NEWSIZE,%VAL(PTR1))
        ENDIF

*  Unmap arrays

        CALL TSP_UNMAP(ILOC,STATUS)
        CALL TSP_UNMAP(ILOC2,STATUS)
        CALL DAT_ANNUL(SLOC,STATUS)
        CALL DAT_ANNUL(SLOC2,STATUS)
        STATUS = SAI__OK

*  Copy the U Stokes data

        CALL TSP_GET_STOKES(OLOC,'U',SLOC,STATUS)
        CALL TSP_MAP_DATA(SLOC,'UPDATE',PTR1,ILOC,STATUS)
        STAT1 = STATUS
        CALL TSP_GET_STOKES(LOC2,'U',SLOC2,STATUS)
        CALL TSP_MAP_DATA(SLOC2,'READ',PTR2,ILOC2,STATUS)
        IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_LMERGECPY(SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ELSE IF (STAT1 .EQ. SAI__OK) THEN
         CALL TSP_LMERGEZERO(SIZE2,NEWSIZE,%VAL(PTR1))
        ENDIF

*  Unmap arrays

        CALL TSP_UNMAP(ILOC,STATUS)
        CALL TSP_UNMAP(ILOC2,STATUS)
        STATUS = SAI__OK

*  Copy the U Stokes Variance

        CALL TSP_MAP_VAR(SLOC,'UPDATE',PTR1,ILOC,STATUS)
        STAT1 = STATUS
        CALL TSP_MAP_VAR(SLOC2,'READ',PTR2,ILOC2,STATUS)
        IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_LMERGECPY(SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ELSE IF (STAT1 .EQ. SAI__OK) THEN
         CALL TSP_LMERGEZERO(SIZE2,NEWSIZE,%VAL(PTR1))
        ENDIF

*  Unmap arrays

        CALL TSP_UNMAP(ILOC,STATUS)
        CALL TSP_UNMAP(ILOC2,STATUS)
        CALL DAT_ANNUL(SLOC,STATUS)
        CALL DAT_ANNUL(SLOC2,STATUS)
        STATUS = SAI__OK

*  Copy the V Stokes data

        CALL TSP_GET_STOKES(OLOC,'V',SLOC,STATUS)
        CALL TSP_MAP_DATA(SLOC,'UPDATE',PTR1,ILOC,STATUS)
        STAT1 = STATUS
        CALL TSP_GET_STOKES(LOC2,'V',SLOC2,STATUS)
        CALL TSP_MAP_DATA(SLOC2,'READ',PTR2,ILOC2,STATUS)
        IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_LMERGECPY(SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ELSE IF (STAT1 .EQ. SAI__OK) THEN
         CALL TSP_LMERGEZERO(SIZE2,NEWSIZE,%VAL(PTR1))
        ENDIF

*  Unmap arrays

        CALL TSP_UNMAP(ILOC,STATUS)
        CALL TSP_UNMAP(ILOC2,STATUS)
        STATUS = SAI__OK

*  Copy the V Stokes Variance

        CALL TSP_MAP_VAR(SLOC,'UPDATE',PTR1,ILOC,STATUS)
        STAT1 = STATUS
        CALL TSP_MAP_VAR(SLOC2,'READ',PTR2,ILOC2,STATUS)
        IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_LMERGECPY(SIZE2,NEWSIZE,%VAL(PTR1),%VAL(PTR2))
        ELSE IF (STAT1 .EQ. SAI__OK) THEN
         CALL TSP_LMERGEZERO(SIZE2,NEWSIZE,%VAL(PTR1))
        ENDIF

*  Unmap arrays

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



      SUBROUTINE TSP_LMERGEZERO(D2,DS,OUT)
*+
*
*   T S P _ L M E R G E Z E R O
*
*   Fill the last D2 elements of the output array with zeros.
*   This is used in case where the second input array does not have
*   a Stokes parameter which is in the first input array.
*
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
      INTEGER D2,DS
      REAL OUT(DS)

*  Local variables
      INTEGER I,IS

      IS = DS-D2

*  Fill last D2 elements with zero
      DO I=1,D2
          OUT(I+IS) = 0.0
      ENDDO
      END


      SUBROUTINE TSP_LMERGECPY(D2,DS,OUT,IN)
*+
*
*   T S P _ L M E R G E C P Y
*
*   Copy the array IN into the last D2 elements of the output array.
*   Used to merge the intensity, variances, stokes parameters and
*   wavelength arrays.
*
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
      INTEGER D2,DS
      REAL IN(D2),OUT(DS)

*  Local variables
      INTEGER I,IS

      IS = DS-D2

*  Copy input into last D2 elements of output
      DO I=1,D2
          OUT(I+IS) = IN(I)
      ENDDO
      END

