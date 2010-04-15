C+
      SUBROUTINE RED3_ADJOIN (STATUS)
C
C     A D J O I N
C
C     ADJOIN is a Figaro routine whose primary function is to
C     append one spectrum to another.  That is, given two spectra,
C     it produces one output spectrum where the .X and .Z arrays
C     are formed by appending the second spectrum data onto the end
C     of the data from the first.  In detail, ADJOIN is a little
C     more complex, since it produces a spectrum in which the
C     X data (the contents of the data object .X.DATA) increase.
C     This may involve the sorting of the various arrays, so ADJOIN
C     can be regarded as a program that merges two spectra into
C     increasing X order. The resulting spectrum makes perfect
C     sense if the data represent flux density measurements, but
C     may be misleading if the data represent total flux measured
C     within wavelength bins.  The X array may well not represent
C     even a smooth wavelength vs channel relationship, let alone
C     scrunched data.  Care should be taken in the use of this routine.
C
C     Command parameters -
C
C     SPECTRUM    (Character) The first of the two spectra.
C     SPECTRUM2   (Character) The second of the two spectra.
C     OUTPUT      (Character) The resulting output spectrum.
C
C     Command keywords - None
C
C                                            KS / AAO 18th June 1985
C  ADAMised, adapted to use DSA and errors  JFL / ROE 15th July 1990
C  removed adamdefns, adamerrs for unix porting KK / 30-Nov-1995
*  27-Feb-96 : rename from red4_
C-
      IMPLICIT NONE

C     GLOBAL VARIABLES
      INCLUDE 'SAE_PAR'

      INTEGER STATUS
C
C     External routines
C
      INTEGER GEN_INCCHK
C
C     Local variables
C
      LOGICAL FAULT
      LOGICAL QUAL, FLAGS, ERRORS, ERRORS1
      LOGICAL REVERSE, OVRLAP
      LOGICAL SORT
      INTEGER NDIM, DIMS(10), NELM
      INTEGER NX, NX1
      INTEGER BYTES
      INTEGER ADDRESS
      INTEGER D_SLOT, D_PTR
      INTEGER A_SLOT, A_PTR
      INTEGER Q_SLOT, Q_PTR
      INTEGER V_SLOT, V_PTR
      INTEGER D1_SLOT, D1_PTR
      INTEGER A1_SLOT, A1_PTR
      INTEGER Q1_SLOT, Q1_PTR
      INTEGER V1_SLOT, V1_PTR
      INTEGER DO_SLOT, DO_PTR
      INTEGER AO_SLOT, AO_PTR
      INTEGER QO_SLOT, QO_PTR
      INTEGER VO_SLOT, VO_PTR
      INTEGER W_SLOT, WK_PTR, VW_PTR
      REAL XMIN, XMAX, X1MIN, X1MAX
      CHARACTER*80 SPECT, SPECT1, OUTPUT
      CHARACTER UNITS*64, UNITS1*64
C
C     Initial values
C
      FAULT=.FALSE.

      IF (STATUS .NE. SAI__OK) RETURN
C
C     Get SPECTRUM. Open the file and check data dimensions.
C
      CALL PAR_GET0C ('SPECTRUM', SPECT, STATUS)
      CALL DSA_NAMED_INPUT ('SPECT', SPECT, STATUS)
      CALL DSA_DATA_SIZE ('SPECT', 1, NDIM, DIMS, NELM, STATUS)
      NX=DIMS(1)
C
C     Ditto for SPECTRUM1
C
      CALL PAR_GET0C ('SPECTRUM1', SPECT1, STATUS)
      CALL DSA_NAMED_INPUT ('SPECT1', SPECT1, STATUS)
      CALL DSA_DATA_SIZE ('SPECT1', 1, NDIM, DIMS, NELM, STATUS)
      NX1=DIMS(1)
C
C     See if the units for the spectra match - in X and in Z
C
      CALL DSA_GET_AXIS_INFO ('SPECT', 1, 1, UNITS, 0, 0, STATUS)
      CALL DSA_GET_AXIS_INFO ('SPECT1', 1, 1, UNITS1, 0, 0, STATUS)
      IF (UNITS1.NE.UNITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER(
     :           'Warning: X-axis units for the two spectra differ.',
     :                                                         STATUS)
         ENDIF
      ENDIF
      CALL DSA_GET_DATA_INFO ('SPECT', 1, UNITS, 0, 0, STATUS)
      CALL DSA_GET_DATA_INFO ('SPECT1', 1, UNITS1, 0, 0, STATUS)
      IF (UNITS1.NE.UNITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_WRUSER(
     :           'Warning: Data units for the two spectra differ.',
     :                                                         STATUS)
         ENDIF
      ENDIF
C
C     Look for quality, magic values,
C
      CALL DSA_SEEK_QUALITY ('SPECT', QUAL, STATUS)
      IF (.NOT. QUAL) THEN
         CALL DSA_SEEK_QUALITY ('SPECT1', QUAL, STATUS)
      ENDIF
      CALL DSA_SEEK_FLAGGED_VALUES ('SPECT', FLAGS, STATUS)
      IF (.NOT. FLAGS) THEN
         CALL DSA_SEEK_FLAGGED_VALUES ('SPECT1', FLAGS, STATUS)
      ENDIF
C
C     Prefer quality rather than magic values
C
      IF (QUAL) THEN
         CALL DSA_USE_QUALITY ('SPECT', STATUS)
         CALL DSA_USE_QUALITY ('SPECT1', STATUS)
         FLAGS = .FALSE.
      ENDIF
      IF (FLAGS) THEN
         CALL DSA_USE_FLAGGED_VALUES ('SPECT', STATUS)
         CALL DSA_USE_FLAGGED_VALUES ('SPECT1', STATUS)
      ENDIF
C
C     Look for error arrays, warn user if only one spectrum has them
C
      CALL DSA_SEEK_ERRORS ('SPECT', ERRORS, STATUS)
      CALL DSA_SEEK_ERRORS ('SPECT1', ERRORS1, STATUS)
      IF ((ERRORS.AND.(.NOT.ERRORS1)).OR.((.NOT.ERRORS).AND.ERRORS1))
     :                                                         THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL DSA_WRUSER (
     :         'Warning: only one array has an error array.\N')
            CALL DSA_WRUSER (
     :         'Error information will not be propagated.\N')
         ENDIF
      ENDIF
      ERRORS = (ERRORS .AND. ERRORS1)
C
C     Map in all the arrays
C
      CALL DSA_MAP_DATA ('SPECT', 'READ', 'FLOAT', ADDRESS, D_SLOT,
     :   STATUS)
      D_PTR = ADDRESS
      CALL DSA_MAP_AXIS_DATA ('SPECT', 1, 'READ', 'FLOAT', ADDRESS,
     :   A_SLOT, STATUS)
      A_PTR = ADDRESS
      IF (QUAL) THEN
         CALL DSA_MAP_QUALITY ('SPECT', 'READ', 'INT', ADDRESS,
     :      Q_SLOT, STATUS)
         Q_PTR = ADDRESS
      ENDIF
      IF (ERRORS) THEN
         CALL DSA_MAP_VARIANCE ('SPECT', 'READ', 'FLOAT', ADDRESS,
     :      V_SLOT, STATUS)
         V_PTR = ADDRESS
      ENDIF

      CALL DSA_MAP_DATA ('SPECT1', 'READ', 'FLOAT', ADDRESS, D1_SLOT,
     :   STATUS)
      D1_PTR = ADDRESS
      CALL DSA_MAP_AXIS_DATA ('SPECT1', 1, 'READ', 'FLOAT', ADDRESS,
     :   A1_SLOT, STATUS)
      A1_PTR = ADDRESS
      IF (QUAL) THEN
         CALL DSA_MAP_QUALITY ('SPECT1', 'READ', 'INT', ADDRESS,
     :      Q1_SLOT, STATUS)
         Q1_PTR = ADDRESS
      ENDIF
      IF (ERRORS) THEN
         CALL DSA_MAP_VARIANCE ('SPECT1', 'READ', 'FLOAT', ADDRESS,
     :      V1_SLOT, STATUS)
         V1_PTR = ADDRESS
      ENDIF
C
C     Check the X order of the two files.  We try to put the one with
C     the lower X values into the output spectrum first, to avoid the
C     sorting overhead.  Check for an overlap in values.
C
      IF (STATUS .EQ. SAI__OK) THEN
         CALL GEN_RANGEF(%VAL(A_PTR),1,NX,XMAX,XMIN)
         CALL GEN_RANGEF(%VAL(A1_PTR),1,NX1,X1MAX,X1MIN)
      ENDIF
      IF (XMIN.GT.X1MIN) THEN
         REVERSE=.TRUE.
         OVRLAP=X1MAX.GT.XMIN
      ELSE
         OVRLAP=XMAX.GT.X1MIN
         REVERSE=.FALSE.
      END IF
      IF (OVRLAP) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL DSA_WRUSER(
     :         'Note: There is an overlap between the X-ranges '//
     :         'of the two spectra.')
            CALL DSA_WRUSER(
     :         'They will be merged to produce the output '//
     :         'spectrum\N')
         ENDIF
      ELSE IF (REVERSE) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL DSA_WRUSER(
     :         'Note: The first spectrum begins at a higher ')
            CALL DSA_WRUSER (
     :         'wavelength than does the second. Their order ')
            CALL DSA_WRUSER (
     :         'will be reversed.\N')
         END IF
      END IF
C
C     Get the name of the output file and create it. Use SPECT as the
C     basis but don't copy the data and axis structures.
C
      CALL PAR_GET0C ('OUTPUT', OUTPUT, STATUS)
      CALL DSA_NAMED_OUTPUT ('OUTPUT', OUTPUT, 'SPECT', 0, 0, STATUS)
C
C     Create the axis and data structures with the right size
C
      NDIM = 1
      DIMS(1) = NX + NX1
      CALL DSA_RESHAPE_AXIS ('OUTPUT', 1, 'SPECT', 1, NDIM, DIMS,
     :   STATUS)
      CALL DSA_RESHAPE_DATA ('OUTPUT', 'SPECT', NDIM, DIMS, STATUS)
C
C     Map the output arrays
C
      IF (QUAL) THEN
         CALL DSA_USE_QUALITY ('OUTPUT', STATUS)
      ELSE IF (FLAGS) THEN
         CALL DSA_USE_FLAGGED_VALUES ('OUTPUT', STATUS)
      ENDIF
      CALL DSA_MAP_AXIS_DATA ('OUTPUT', 1, 'WRITE', 'FLOAT', ADDRESS,
     :   AO_SLOT, STATUS)
      AO_PTR = ADDRESS
      CALL DSA_MAP_DATA ('OUTPUT', 'WRITE', 'FLOAT', ADDRESS, DO_SLOT,
     :   STATUS)
      DO_PTR = ADDRESS
      IF (QUAL) THEN
         CALL DSA_MAP_QUALITY ('OUTPUT', 'WRITE', 'INT', ADDRESS,
     :      QO_SLOT, STATUS)
         QO_PTR = ADDRESS
      ENDIF
      IF (ERRORS) THEN
         CALL DSA_MAP_VARIANCE ('OUTPUT', 'WRITE', 'FLOAT', ADDRESS,
     :      VO_SLOT, STATUS)
         VO_PTR = ADDRESS
      ENDIF
C
C     Copy the input X arrays into the output X array.
C
      IF (REVERSE) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL GEN_MOVE(NX1*4,%VAL(A1_PTR),%VAL(AO_PTR))
            CALL GEN_MOVE(NX*4,%VAL(A_PTR),%VAL(AO_PTR+NX1*4))
         ENDIF
      ELSE
         IF (STATUS .EQ. SAI__OK) THEN
            CALL GEN_MOVE(NX*4,%VAL(A_PTR),%VAL(AO_PTR))
            CALL GEN_MOVE(NX1*4,%VAL(A1_PTR),%VAL(AO_PTR+NX*4))
         ENDIF
      END IF
C
C     See if we are going to have to sort the data.  If we are, get
C     workspace for the sort vector, calculate it, and re-order the
C     output X array.  The workspace obtained contains two arrays,
C     one the sort vector and one the workspace needed by the indirect
C     sorting routine GEN_FVSORT.
C
      SORT=.FALSE.
      IF (STATUS .EQ. SAI__OK) THEN
         IF (.NOT.GEN_INCCHK(%VAL(AO_PTR),NX+NX1)) THEN
            IF (.NOT.OVRLAP) THEN
               CALL DSA_WRUSER(
     :           'The resulting X array is not in increasing order.')
               CALL DSA_WRUSER(
     :           'The data arrays will be sorted into increasing ')
               CALL DSA_WRUSER('X-order.\N')
            ENDIF
            SORT=.TRUE.
            BYTES=(NX+NX1)*4*2
            CALL DSA_GET_WORKSPACE (BYTES, ADDRESS, W_SLOT, STATUS)
            VW_PTR = ADDRESS
            WK_PTR=VW_PTR+(NX+NX1)*4
            IF (STATUS .EQ. SAI__OK) THEN
               CALL GEN_QFISORT(%VAL(AO_PTR),NX+NX1,%VAL(VW_PTR))
               CALL GEN_FVSORT(%VAL(VW_PTR),NX+NX1,1,%VAL(WK_PTR),
     :            %VAL(AO_PTR))
            ENDIF
         ENDIF
      END IF
C
C     Now copy the data, error and quality arrays as necessary into
C     the output structure. Sort them according to the sorted axis
C     array as required
C
      IF (STATUS .EQ. SAI__OK) THEN
         IF (REVERSE) THEN
            CALL GEN_MOVE(NX1*4,%VAL(D1_PTR),%VAL(DO_PTR))
            CALL GEN_MOVE(NX*4,%VAL(D_PTR),%VAL(DO_PTR+NX1*4))
            IF (QUAL) THEN
               CALL GEN_MOVE(NX1*4,%VAL(Q1_PTR),%VAL(QO_PTR))
               CALL GEN_MOVE(NX*4,%VAL(Q_PTR),%VAL(QO_PTR+NX1*4))
            ENDIF
            IF (ERRORS) THEN
               CALL GEN_MOVE(NX1*4,%VAL(V1_PTR),%VAL(VO_PTR))
               CALL GEN_MOVE(NX*4,%VAL(V_PTR),%VAL(VO_PTR+NX1*4))
            ENDIF
         ELSE
            CALL GEN_MOVE(NX*4,%VAL(D_PTR),%VAL(DO_PTR))
            CALL GEN_MOVE(NX1*4,%VAL(D1_PTR),%VAL(DO_PTR+NX*4))
            IF (QUAL) THEN
               CALL GEN_MOVE(NX*4,%VAL(Q_PTR),%VAL(QO_PTR))
               CALL GEN_MOVE(NX1*4,%VAL(Q1_PTR),%VAL(QO_PTR+NX*4))
            ENDIF
            IF (ERRORS) THEN
               CALL GEN_MOVE(NX*4,%VAL(V_PTR),%VAL(VO_PTR))
               CALL GEN_MOVE(NX1*4,%VAL(V1_PTR),%VAL(VO_PTR+NX*4))
            ENDIF
         ENDIF
         IF (SORT.AND.(STATUS .EQ. SAI__OK)) THEN
            CALL GEN_FVSORT(%VAL(VW_PTR),NX+NX1,1,
     :                                    %VAL(WK_PTR),%VAL(DO_PTR))
            IF (QUAL) THEN
               CALL GEN_IVSORT(%VAL(VW_PTR),NX+NX1,1,
     :                                    %VAL(WK_PTR),%VAL(QO_PTR))
            ENDIF
            IF (ERRORS) THEN
               CALL GEN_FVSORT(%VAL(VW_PTR),NX+NX1,1,
     :                                    %VAL(WK_PTR),%VAL(VO_PTR))
            ENDIF
         ENDIF
      ENDIF
C
C     Tidy up
C
  500 CONTINUE
C
C     Close everything down
C
      CALL DSA_CLOSE (STATUS)
C
C      IF (FAULT) CALL FIG_SETERR
C
      END
