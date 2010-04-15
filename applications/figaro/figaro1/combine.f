C+
      SUBROUTINE COMBINE
C
C     C O M B I N E
C
C     Combine two spectra or images. If error or variance information
C     is available on the two images a weighted mean of the two
C     is formed. If error information is missing for one or both
C     images a simple average is formed.
C
C     Command parameters -
C
C     SPECTRUM  The name of the structure containing the first spectrum
C
C     SPECTRUM1 The name of the structure containing the second spectrum
C
C     OUTPUT    The name of the result of the operation.  This can
C               be the same as for SPECTRUM.  If not, a new structure
C               is created, with everything but the data a direct
C               copy of the input.
C
C                                          JAB / AAO  14th July 1986
C     Modified:
C
C     15th July 1986  KS / AAO. Minor changes so routine will work
C                     on data with more than 1 dimension.
C     22nd July 1986  KS / AAO. Modified to work with absolute rather
C                     than percentage error values.
C     13th Aug  1987  DJA/ AAO. Revised DSA_ routines - some specs changed.
C                     Now uses DYN_ package for dynamic memory handling.
C                     Changed inconsistent variable naming. The variables for
C                     the first spectrum are numbered 1, until they are
C                     superceded by the OUTPUT vars.  and those of the
C                     second 2 : after the output filename is obtained, the
C                     names are prefixed with an 'O'.
C      8th Dec  1990  JAB / JAC. Use Data Quality.
C      1st Mar  1991  JAB / JAC. Rewrite subroutine performing the combine.
C      2nd Apr  1991  JMS / AAO. Added the correction, made by KS, on the use
C                     of 'UPDATE' and 'WRITE' in mapping calls.
C      26th Apr 1991  JMS / AAO. Fixed variance mapping bug - now maps from
C                     'SPECT2' instead of 'OUTPUT'.
C      29th Sep 1992  HME / UoE, Starlink.  INCLUDE changed, TABs
C                     removed.
C      2005 June 7    MJC / Starlink  Use CNF_PVAL for pointers to
C                     mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Local variables
C
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      INTEGER      V2PTR        ! Dynamic memory pointer to 2nd variance array
      LOGICAL      ER2EXIST     ! TRUE if spectrum 2 has valid error data
      INTEGER      ER2SLOT      ! Map slot number of 2nd error array
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      NELM         ! Total number of elements in data
      INTEGER      IGNORE       ! Used to pass ignorable status
      LOGICAL      OEREXIST     ! TRUE if spectrum/output has error data
      INTEGER      OVPTR        ! Dynamic memory pointer to output variance data
      INTEGER      OERSLOT      ! Map slot number of output error data
      INTEGER      OPTR         ! Dynamic-memory pointer to output data array
      INTEGER      OSLOT        ! Map slot number outputdata array
      INTEGER      OQPTR        ! Dynamic memory pointer to output quality
      INTEGER      OQSLOT       ! Map slot number of output quality
      INTEGER      S2PTR        ! Dynamic memory pointer to spec2's data array
      INTEGER      S2SLOT       ! Map slot number of spectrum2's data array
      INTEGER      Q2PTR        ! Dynamic memory pointer to spec2's quality
      INTEGER      Q2SLOT       ! Map slot number of spectrum2's quality
      INTEGER      STATUS       ! Running status for DSA_ routines
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get first input name
C
      CALL DSA_INPUT('SPECT1','SPECTRUM',STATUS)
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE('SPECT1',10,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get spectrum 2 name
C
      CALL DSA_INPUT('SPECT2','SPECTRUM1',STATUS)
      CALL DSA_USE_QUALITY('SPECT2',STATUS)
C
C     Check that sizes and dimensions of data structures are the same
C
      CALL DSA_MATCH_SIZES('SPECT1','SPECT2',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get output structure name
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','SPECT1',0,0,STATUS)
      CALL DSA_USE_QUALITY('OUTPUT',STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map input and output data
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      CALL DSA_MAP_QUALITY('OUTPUT','UPDATE','BYTE',OQPTR,OQSLOT,
     :                     STATUS)
      CALL DSA_SEEK_ERRORS('OUTPUT',OEREXIST,STATUS)
      IF (STATUS.NE.0) GOTO 500
      IF (OEREXIST) THEN
         CALL DSA_MAP_VARIANCE('OUTPUT','UPDATE','FLOAT',OVPTR,
     :                         OERSLOT,STATUS)
      ELSE
         CALL PAR_WRUSER('Will not perform error calculation',IGNORE)
      END IF
C
C     Map spectrum 2 data
C
      CALL DSA_MAP_DATA('SPECT2','READ','FLOAT',S2PTR,S2SLOT,STATUS)
      CALL DSA_MAP_QUALITY('SPECT2','READ','BYTE',Q2PTR,Q2SLOT,STATUS)
      CALL DSA_SEEK_ERRORS('SPECT2',ER2EXIST,STATUS)
      IF (STATUS.NE.0) GOTO 500
      IF (ER2EXIST) THEN
         CALL DSA_MAP_VARIANCE('SPECT2','READ','FLOAT',V2PTR,
     :                         ER2SLOT,STATUS)
      ELSE
         CALL PAR_WRUSER('No error information for spectrum 2 data',
     :                                                       IGNORE)
      END IF
C
C     Check that axes of the two spectra match
C
      CALL DSA_MATCH_AXES('SPECT1','SPECT2',STATUS)
C
C     Operate on the spectra
C
      CALL FIG_COMBINE(OEREXIST,ER2EXIST,NELM,%VAL(CNF_PVAL(OPTR)),
     :                 %VAL(CNF_PVAL(OVPTR)),%VAL(CNF_PVAL(OQPTR)),
     :                 %VAL(CNF_PVAL(S2PTR)),%VAL(CNF_PVAL(V2PTR)),
     :                 %VAL(CNF_PVAL(Q2PTR)))
C
C     Tidy up
C
  500 CONTINUE
C
C     Closedown everything
C
      CALL DSA_CLOSE(STATUS)
C
      END


      SUBROUTINE FIG_COMBINE(E1MAP,E2MAP,N,SPEC1,VSPEC1,QSPEC1,SPEC2,
     :                                          VSPEC2,QSPEC2)
C
C     Combine two spectra weighting according to 1/variance
C
C     E1MAP     (Logical) - True if spectrum 1 has errors
C     E2MAP     (Logical) - True if spectrum 2 has errors
C     N         (Integer) - Number of elements of the spectra
C     SPEC1     (Real array SPEC1(N)) - The first spectrum
C     VSPEC1    (Real array VSPEC1(N)) - The variance on the first spectrum
C     QSPEC1    (Byte array QSPEC1(N)) - The quality of the first spectrum
C     SPEC2     (Real array SPEC2(N)) - The second spectrum
C     VSPEC2    (Real array VSPEC2(N)) - The variance on the second spectrum
C     QSPEC2    (Byte array QSPEC2(N) - The quality of the second spectrum
C
C     Modified -
C
C     22nd July 1986.  KS / AAO.  Now expects absolute error values
C                      rather than percentage errors.
C      8th Dec 1990.   JAB / JAC.  Use quality arrays.
C      1st Mar 1991.   JAB / JAC.  Use variance rather than errors.
C                                  Perform weighting properly.
C                                  Form average rather than sum when
C                                    errors not available.
C                                  Allow for flagged values in variance arrays.
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL E1MAP,E2MAP
      INTEGER N
      REAL SPEC1(N), VSPEC1(N), SPEC2(N), VSPEC2(N)
      BYTE QSPEC1(N), QSPEC2(N)
C
C     Local variables
C
      INTEGER STATUS,I
      REAL WT1,WT2
      REAL FUNKNOWN_IN,FUNKNOWN_OUT
C
C     Set up values for unknown variance - This is to support a possible
C     future upgrade to DSA to allow flagged values in error and variance
C     arrays. When such a DSA version is available FUNKNOWN_OUT should be
C     set equal to the flag value
C
      STATUS = 0
      CALL DSA_GET_FLAG_VALUE('FLOAT',FUNKNOWN_IN,STATUS)
      FUNKNOWN_OUT = 0
      DO I=1,N
C
C     If quality bad on one array output is other array
C
         IF ((QSPEC1(I) .NE. 0) .AND. (QSPEC2(I) .EQ. 0)) THEN
            SPEC1(I)=SPEC2(I)
            VSPEC1(I)=VSPEC2(I)
            QSPEC1(I)=0
         ELSE IF (QSPEC2(I) .NE. 0) THEN
C
C     Nothing to do - Spectrum 1 is the output (even if its quality is bad)
C
            CONTINUE
         ELSE IF ((.NOT. E1MAP) .OR. (.NOT. E2MAP)) THEN
C
C     If no error array on one or other spectrum just form average
C
            SPEC1(I) = (SPEC1(I)+SPEC2(I))/2.0
         ELSE IF ((VSPEC1(I) .EQ. FUNKNOWN_IN) .OR.
     :       (VSPEC2(I) .EQ. FUNKNOWN_IN)) THEN
C
C     If no errors on one or other spectrum just form average and set output
C     variance to unknown
C
            SPEC1(I) = (SPEC1(I)+SPEC2(I))/2.0
            VSPEC1(I) = FUNKNOWN_OUT
         ELSE IF (VSPEC1(I) .EQ. 0.0) THEN
C
C     If one or other of the variances are zero we have to give special
C     treatment because the normal weight calculation would crash with a
C     divide by zero
C
            IF (VSPEC2(I) .EQ. 0.0) THEN
C
C     Both variances are zero so form simple average, and output variance is
C     zero
C
               SPEC1(I) = (SPEC1(I)+SPEC2(I))/2.0
               VSPEC1(I) = 0.0
            ELSE
C
C     Only 1st spectrum has variance of zero, so it has infinite weight
C     and is therefore the output spectrum - hence nothing to do
C
               CONTINUE
            END IF
         ELSE IF (VSPEC2(I) .EQ. 0.0) THEN
C
C     Only 2nd spectrum has variance of zero, so it has infinite weight
C     and is therefore the output spectrum - copy it to the output
C
            SPEC1(I) = SPEC2(I)
            VSPEC1(I) = VSPEC2(I)
         ELSE
C
C     Form weighted average, weighting as 1/variance
C
            WT1 = 1.0/VSPEC1(I)
            WT2 = 1.0/VSPEC2(I)
C
C     Form combined value
C
            SPEC1(I) = (SPEC1(I)*WT1 + SPEC2(I)*WT2)/(WT1 + WT2)
C
C     Form combined variance
C
            VSPEC1(I) = 1.0/(WT1+WT2)
         END IF
      END DO
      END
