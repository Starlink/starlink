C+
      SUBROUTINE ADJOIN
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
C     SPECTRUM1   (Character) The second of the two spectra.
C     OUTPUT      (Character) The resulting output spectrum.
C
C     Command keywords - None
C
C     18 Jun 1985  Original. KS / AAO
C     15 Jul 1990  ADAMised, adapted to use DSA and errors. JFL / ROE
C     13 Dec 1990  deADAMised!! JAB / JAC
C     13 Dec 1990  Use DYN_ routines and DSA_TYPESIZE. JAB / JAC
C     11 Feb 1991  If x-data are pixno., just append. HME / UoE
C     11 Feb 1991  Reshape data before axis. HME / UoE
C     29 Mar 1991  Added DSA_OPEN and STATUS checks to support user
C                  requested aborts. JMS / AAO
C     10 Sep 1992  Changed INCLUDE. Changed DSA_WRUSER to PAR_WRUSER.
C                  HME / UoE
C     15 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  Quality is unsigned byte.
C                  Bad pixel handling.
C     21 Jun 1996  Allowed overwriting of the input spectrum
C                  MJC / Starlink, RAL.
C     24 Jul 1996  MJCL / Starlink, UCL.  Corrected type of GEN_INCCHK.
C     29 Jul 1996  MJCL / Starlink, UCL.  Catch-all status check before
C                  calling any GEN_ routines.
C+
      IMPLICIT NONE

      INTEGER STATUS
C
C     External routines
C
      LOGICAL GEN_CHKNSF
      LOGICAL GEN_INCCHK
      INTEGER DYN_ELEMENT,DYN_INCREMENT
      INTEGER DSA_TYPESIZE
C
C     Local variables
C
      LOGICAL FAULT
      LOGICAL FLAGS, ERRORS, ERRORS1
      LOGICAL REVERSE, OVRLAP
      LOGICAL SORT
      LOGICAL XIST, XIST1      ! True if axis data exist and NE pix.no.
      LOGICAL EASY             ! True if both spectra have only pix.no.
      INTEGER NDIM, DIMS(10), NELM
      INTEGER NX, NX1
      INTEGER BYTES
      INTEGER ADDRESS
      INTEGER D_SLOT, D_PTR
      INTEGER A_SLOT, A_PTR
      INTEGER V_SLOT, V_PTR
      INTEGER D1_SLOT, D1_PTR
      INTEGER A1_SLOT, A1_PTR
      INTEGER V1_SLOT, V1_PTR
      INTEGER DO_SLOT, DO_PTR
      INTEGER AO_SLOT, AO_PTR
      INTEGER VO_SLOT, VO_PTR
      INTEGER W_SLOT, WK_PTR, VW_PTR
      REAL XMIN, XMAX, X1MIN, X1MAX
      CHARACTER*80 SPECT, SPECT1, OUTPUT
      CHARACTER UNITS*64, UNITS1*64
C
C     Required for dynamic memory handling - defines DYNAMIC_MEM
C
      INCLUDE 'DYNAMIC_MEMORY'
C
C     Initial values
C
      STATUS = 0
      FAULT=.FALSE.
C
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get SPECTRUM. Open the file and check data dimensions.
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GOTO 500
      CALL DSA_DATA_SIZE ('SPECT', 1, NDIM, DIMS, NELM, STATUS)
      NX=DIMS(1)
C
C     Ditto for SPECTRUM1
C
      CALL DSA_INPUT('SPECT1','SPECTRUM1',STATUS)
      IF (STATUS.NE.0) GOTO 500
      CALL DSA_DATA_SIZE ('SPECT1', 1, NDIM, DIMS, NELM, STATUS)
      NX1=DIMS(1)
C
C     If both spectra have no x-data, life is easy.
C
      CALL DSA_SEEK_AXIS( 'SPECT', 1, XIST, STATUS )
      CALL DSA_SEEK_AXIS( 'SPECT1', 1, XIST1, STATUS )
      EASY = (.NOT. XIST) .AND. (.NOT. XIST1)
C
C     See if the units for the spectra match - in X and in Z
C
      IF (.NOT. EASY) THEN
         CALL DSA_GET_AXIS_INFO ('SPECT', 1, 1, UNITS, 0, 0, STATUS)
         CALL DSA_GET_AXIS_INFO ('SPECT1', 1, 1, UNITS1, 0, 0, STATUS)
         IF ( UNITS1 .NE. UNITS .AND. STATUS .EQ. 0 ) THEN
            CALL PAR_WRUSER(
     :         'Warning: X-axis units for the two spectra differ.',
     :          STATUS)
         ENDIF
      ENDIF
      CALL DSA_GET_DATA_INFO ('SPECT', 1, UNITS, 0, 0, STATUS)
      CALL DSA_GET_DATA_INFO ('SPECT1', 1, UNITS1, 0, 0, STATUS)
      IF ( UNITS1 .NE. UNITS .AND. STATUS .EQ. 0 ) THEN
         CALL PAR_WRUSER(
     :      'Warning: Data units for the two spectra differ.',
     :      STATUS)
      ENDIF
C
C     Use flagged values, seek for them later after mapping.
C
      CALL DSA_USE_FLAGGED_VALUES ('SPECT', STATUS)
      CALL DSA_USE_FLAGGED_VALUES ('SPECT1', STATUS)
C
C     Look for error arrays, warn user if only one spectrum has them
C
      CALL DSA_SEEK_ERRORS ('SPECT', ERRORS, STATUS)
      CALL DSA_SEEK_ERRORS ('SPECT1', ERRORS1, STATUS)
      IF ((ERRORS.AND.(.NOT.ERRORS1)).OR.((.NOT.ERRORS).AND.ERRORS1))
     :                                                         THEN
         IF (STATUS .EQ. 0) THEN
            CALL PAR_WRUSER (
     :         'Warning: only one array has an error array.',STATUS)
            CALL PAR_WRUSER (
     :         'Error information will not be propagated.',STATUS)
         ENDIF
      ENDIF
      ERRORS = (ERRORS .AND. ERRORS1)
C
C     Map in all the arrays
C
      CALL DSA_MAP_DATA ('SPECT', 'READ', 'FLOAT', ADDRESS, D_SLOT,
     :   STATUS)
      D_PTR = DYN_ELEMENT(ADDRESS)
      CALL DSA_MAP_AXIS_DATA ('SPECT', 1, 'READ', 'FLOAT', ADDRESS,
     :   A_SLOT, STATUS)
      A_PTR = DYN_ELEMENT(ADDRESS)
      IF (ERRORS) THEN
         CALL DSA_MAP_ERRORS ('SPECT', 'READ', 'FLOAT', ADDRESS,
     :      V_SLOT, STATUS)
         V_PTR = DYN_ELEMENT(ADDRESS)
      ENDIF
      CALL DSA_MAP_DATA ('SPECT1', 'READ', 'FLOAT', ADDRESS, D1_SLOT,
     :   STATUS)
      D1_PTR = DYN_ELEMENT(ADDRESS)
      CALL DSA_MAP_AXIS_DATA ('SPECT1', 1, 'READ', 'FLOAT', ADDRESS,
     :   A1_SLOT, STATUS)
      A1_PTR = DYN_ELEMENT(ADDRESS)
      IF (ERRORS) THEN
         CALL DSA_MAP_ERRORS ('SPECT1', 'READ', 'FLOAT', ADDRESS,
     :      V1_SLOT, STATUS)
         V1_PTR = DYN_ELEMENT(ADDRESS)
      ENDIF
C
C     Both data arrays have been mapped. Now is the best chance to
C     look for bad values.
C
      CALL DSA_SEEK_FLAGGED_VALUES ('SPECT', FLAGS, STATUS)
      IF (.NOT. FLAGS)
     :   CALL DSA_SEEK_FLAGGED_VALUES ('SPECT1', FLAGS, STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     If existing X data are just pixel numbers, life is easy too.
C
      IF ( .NOT. EASY ) THEN
         IF ( XIST ) XIST = .NOT. GEN_CHKNSF( DYNAMIC_MEM(A_PTR), NX )
         IF ( XIST1) XIST1 = .NOT. GEN_CHKNSF( DYNAMIC_MEM(A_PTR), NX1 )
         EASY = (.NOT. XIST) .AND. (.NOT. XIST1)
      ENDIF
C
C     Check the X order of the two files.  We try to put the one with
C     the lower X values into the output spectrum first, to avoid the
C     sorting overhead.  Check for an overlap in values.
C
      IF ( EASY ) THEN
         REVERSE = .FALSE.
         OVRLAP = .FALSE.
      ELSE
         IF (STATUS .EQ. 0) THEN
            CALL GEN_RANGEF(DYNAMIC_MEM(A_PTR),1,NX,XMAX,XMIN)
            CALL GEN_RANGEF(DYNAMIC_MEM(A1_PTR),1,NX1,X1MAX,X1MIN)
         ENDIF
         IF (XMIN.GT.X1MIN) THEN
            REVERSE=.TRUE.
            OVRLAP=X1MAX.GT.XMIN
         ELSE
            OVRLAP=XMAX.GT.X1MIN
            REVERSE=.FALSE.
         END IF
      ENDIF
      IF (OVRLAP) THEN
         IF (STATUS .EQ. 0) THEN
            CALL PAR_WRUSER(
     :         'Note: There is an overlap between the X-ranges '//
     :         'of the two spectra.',STATUS)
            CALL PAR_WRUSER(
     :         'They will be merged to produce the output '//
     :         'spectrum.',STATUS)
         ENDIF
      ELSE IF (REVERSE) THEN
         IF (STATUS .EQ. 0) THEN
            CALL PAR_WRUSER(
     :         'Note: The first spectrum begins at a higher '//
     :         'wavelength than does the second. ',STATUS)
            CALL PAR_WRUSER ('Their order will be reversed.',STATUS)
         END IF
      END IF
C
C     Get the name of the output file and create it. Use SPECT as the
C     basis and allow overwriting of an input file.
C
      CALL DSA_OUTPUT ('OUTPUT', 'OUTPUT', 'SPECT', 0, 0, STATUS)
C
C     Create the axis and data structures with the right size
C
      NDIM = 1
      DIMS(1) = NX + NX1
      CALL DSA_RESHAPE_DATA ('OUTPUT', 'SPECT', NDIM, DIMS, STATUS)
      IF ( .NOT. EASY )
     :   CALL DSA_RESHAPE_AXIS ('OUTPUT', 1, 'SPECT', 1, NDIM, DIMS,
     :      STATUS)
C
C     Map the output arrays
C
      IF (FLAGS)
     :   CALL DSA_USE_FLAGGED_VALUES ('OUTPUT', STATUS)
      IF ( .NOT. EASY ) THEN
         CALL DSA_MAP_AXIS_DATA ('OUTPUT', 1, 'WRITE', 'FLOAT', ADDRESS,
     :      AO_SLOT, STATUS)
         AO_PTR = DYN_ELEMENT(ADDRESS)
      ENDIF
      CALL DSA_MAP_DATA ('OUTPUT', 'WRITE', 'FLOAT', ADDRESS, DO_SLOT,
     :   STATUS)
      DO_PTR = DYN_ELEMENT(ADDRESS)
      IF (ERRORS) THEN
         CALL DSA_MAP_ERRORS ('OUTPUT', 'WRITE', 'FLOAT', ADDRESS,
     :      VO_SLOT, STATUS)
         VO_PTR = DYN_ELEMENT(ADDRESS)
      ENDIF
C
C     Copy the input X arrays into the output X array.
C
      IF ( (.NOT. EASY) .AND. STATUS .EQ. 0 ) THEN
         IF (REVERSE) THEN
            CALL GEN_MOVE(NX1*DSA_TYPESIZE('FLOAT',STATUS),
     :         DYNAMIC_MEM(A1_PTR),DYNAMIC_MEM(AO_PTR))
            CALL GEN_MOVE(NX*DSA_TYPESIZE('FLOAT',STATUS),
     :         DYNAMIC_MEM(A_PTR),
     :         DYNAMIC_MEM(DYN_INCREMENT(AO_PTR,'FLOAT',NX1)))
         ELSE
            CALL GEN_MOVE(NX*DSA_TYPESIZE('FLOAT',STATUS),
     :         DYNAMIC_MEM(A_PTR),DYNAMIC_MEM(AO_PTR))
            CALL GEN_MOVE(NX1*DSA_TYPESIZE('FLOAT',STATUS),
     :         DYNAMIC_MEM(A1_PTR),
     :         DYNAMIC_MEM(DYN_INCREMENT(AO_PTR,'FLOAT',NX)))
         ENDIF
      ENDIF
C
C     See if we are going to have to sort the data.  If we are, get
C     workspace for the sort vector, calculate it, and re-order the
C     output X array.  The workspace obtained contains two arrays,
C     one the sort vector and one the workspace needed by the indirect
C     sorting routine GEN_FVSORT.
C
      SORT=.FALSE.
      IF ( (.NOT. EASY) .AND. STATUS .EQ. 0 ) THEN
         IF (.NOT.GEN_INCCHK(DYNAMIC_MEM(AO_PTR),NX+NX1)) THEN
            IF (.NOT.OVRLAP) THEN
               CALL PAR_WRUSER(
     :           'The resulting X array is not in increasing order.',
     :           STATUS)
               CALL PAR_WRUSER(
     :           'The data arrays will be sorted into increasing '//
     :           'X-order.',STATUS)
            ENDIF
            SORT=.TRUE.
            BYTES=(NX+NX1)*DSA_TYPESIZE('FLOAT',STATUS)*2
            CALL DSA_GET_WORKSPACE (BYTES, ADDRESS, W_SLOT, STATUS)
            VW_PTR = DYN_ELEMENT(ADDRESS)
            WK_PTR=VW_PTR+(NX+NX1)*DSA_TYPESIZE('FLOAT',STATUS)
            IF (STATUS .EQ. 0) THEN
               CALL GEN_QFISORT(DYNAMIC_MEM(AO_PTR),NX+NX1,
     :               DYNAMIC_MEM(VW_PTR))
               CALL GEN_FVSORT(DYNAMIC_MEM(VW_PTR),NX+NX1,1,
     :               DYNAMIC_MEM(WK_PTR),DYNAMIC_MEM(AO_PTR))
            ENDIF
         ENDIF
      END IF
C
C     Now copy the data and error arrays as necessary into
C     the output structure. Sort them according to the sorted axis
C     array as required
C
      IF (STATUS .EQ. 0) THEN
         IF (REVERSE) THEN
            CALL GEN_MOVE(NX1*DSA_TYPESIZE('FLOAT',STATUS),
     :            DYNAMIC_MEM(D1_PTR),DYNAMIC_MEM(DO_PTR))
            CALL GEN_MOVE(NX*DSA_TYPESIZE('FLOAT',STATUS),
     :           DYNAMIC_MEM(D_PTR),
     :           DYNAMIC_MEM(DYN_INCREMENT(DO_PTR,'FLOAT',NX1)))
            IF (ERRORS) THEN
               CALL GEN_MOVE(NX1*DSA_TYPESIZE('FLOAT',STATUS),
     :            DYNAMIC_MEM(V1_PTR),DYNAMIC_MEM(VO_PTR))
               CALL GEN_MOVE(NX*DSA_TYPESIZE('FLOAT',STATUS),
     :            DYNAMIC_MEM(V_PTR),
     :            DYNAMIC_MEM(DYN_INCREMENT(VO_PTR,'FLOAT',NX1)))
            ENDIF
         ELSE
            CALL GEN_MOVE(NX*DSA_TYPESIZE('FLOAT',STATUS),
     :            DYNAMIC_MEM(D_PTR),DYNAMIC_MEM(DO_PTR))
            CALL GEN_MOVE(NX1*DSA_TYPESIZE('FLOAT',STATUS),
     :            DYNAMIC_MEM(D1_PTR),
     :            DYNAMIC_MEM(DYN_INCREMENT(DO_PTR,'FLOAT',NX)))
            IF (ERRORS) THEN
               CALL GEN_MOVE(NX*DSA_TYPESIZE('FLOAT',STATUS),
     :            DYNAMIC_MEM(V_PTR),DYNAMIC_MEM(VO_PTR))
               CALL GEN_MOVE(NX1*DSA_TYPESIZE('FLOAT',STATUS),
     :            DYNAMIC_MEM(V1_PTR),
     :            DYNAMIC_MEM(DYN_INCREMENT(VO_PTR,'FLOAT',NX)))
            ENDIF
         ENDIF
         IF (SORT.AND.(STATUS .EQ. 0)) THEN
            CALL GEN_FVSORT(DYNAMIC_MEM(VW_PTR),NX+NX1,1,
     :                     DYNAMIC_MEM(WK_PTR),DYNAMIC_MEM(DO_PTR))
            IF (ERRORS) THEN
               CALL GEN_FVSORT(DYNAMIC_MEM(VW_PTR),NX+NX1,1,
     :                     DYNAMIC_MEM(WK_PTR),DYNAMIC_MEM(VO_PTR))
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
