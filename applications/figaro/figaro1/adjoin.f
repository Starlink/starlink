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
C     2005 June 6  MJC / Starlink  Use CNF_PVAL for pointers to mapped
C                  data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

      INTEGER STATUS
C
C     External routines
C
      LOGICAL GEN_CHKNSF
      LOGICAL GEN_INCCHK
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
      INTEGER D_SLOT, D_PTR
      INTEGER A_SLOT, A_PTR
      INTEGER V_SLOT, V_PTR
      INTEGER D1_SLOT, D1_PTR
      INTEGER A1_SLOT, A1_PTR
      INTEGER V1_SLOT, V1_PTR
      INTEGER DO_SLOT, DO_PTR, ODO_PTR
      INTEGER AO_SLOT, AO_PTR, OAO_PTR
      INTEGER VO_SLOT, VO_PTR, OVO_PTR
      INTEGER W_SLOT, VW_PTR
      INTEGER WK_SLOT, WK_PTR
      LOGICAL ISNEW, ISNEWV
      REAL XMIN, XMAX, X1MIN, X1MAX
      CHARACTER UNITS*64, UNITS1*64
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
         END IF
      END IF
      CALL DSA_GET_DATA_INFO ('SPECT', 1, UNITS, 0, 0, STATUS)
      CALL DSA_GET_DATA_INFO ('SPECT1', 1, UNITS1, 0, 0, STATUS)
      IF ( UNITS1 .NE. UNITS .AND. STATUS .EQ. 0 ) THEN
         CALL PAR_WRUSER(
     :      'Warning: Data units for the two spectra differ.',
     :      STATUS)
      END IF
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
         END IF
      END IF
      ERRORS = (ERRORS .AND. ERRORS1)
C
C     Map in all the arrays
C
      CALL DSA_MAP_DATA ('SPECT', 'READ', 'FLOAT', D_PTR, D_SLOT,
     :                   STATUS)
      CALL DSA_MAP_AXIS_DATA ('SPECT', 1, 'READ', 'FLOAT', A_PTR,
     :                        A_SLOT, STATUS)
      IF (ERRORS) THEN
         CALL DSA_MAP_ERRORS ('SPECT', 'READ', 'FLOAT', V_PTR,
     :                        V_SLOT, STATUS)
      END IF
      CALL DSA_MAP_DATA ('SPECT1', 'READ', 'FLOAT', D1_PTR, D1_SLOT,
     :                   STATUS)
      CALL DSA_MAP_AXIS_DATA ('SPECT1', 1, 'READ', 'FLOAT', A1_PTR,
     :                        A1_SLOT, STATUS)
      IF (ERRORS) THEN
         CALL DSA_MAP_ERRORS ('SPECT1', 'READ', 'FLOAT', v1_PTR,
     :                        V1_SLOT, STATUS)
      END IF
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
         IF ( XIST )
     :     XIST = .NOT. GEN_CHKNSF( %VAL(CNF_PVAL(A_PTR)), NX )
         IF ( XIST1)
     :     XIST1 = .NOT. GEN_CHKNSF( %VAL(CNF_PVAL(A_PTR)), NX1 )
         EASY = (.NOT. XIST) .AND. (.NOT. XIST1)
      END IF
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
            CALL GEN_RANGEF(%VAL(CNF_PVAL(A_PTR)),1,NX,XMAX,XMIN)
            CALL GEN_RANGEF(%VAL(CNF_PVAL(A1_PTR)),1,NX1,X1MAX,X1MIN)
         END IF
         IF (XMIN.GT.X1MIN) THEN
            REVERSE=.TRUE.
            OVRLAP=X1MAX.GT.XMIN
         ELSE
            OVRLAP=XMAX.GT.X1MIN
            REVERSE=.FALSE.
         END IF
      END IF
      IF (OVRLAP) THEN
         IF (STATUS .EQ. 0) THEN
            CALL PAR_WRUSER(
     :         'Note: There is an overlap between the X-ranges '//
     :         'of the two spectra.',STATUS)
            CALL PAR_WRUSER(
     :         'They will be merged to produce the output '//
     :         'spectrum.',STATUS)
         END IF
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
         CALL DSA_MAP_AXIS_DATA ('OUTPUT', 1, 'WRITE', 'FLOAT', AO_PTR,
     :                           AO_SLOT, STATUS)
      END IF
      CALL DSA_MAP_DATA ('OUTPUT', 'WRITE', 'FLOAT', DO_PTR, DO_SLOT,
     :                   STATUS)
      IF (ERRORS) THEN
         CALL DSA_MAP_ERRORS ('OUTPUT', 'WRITE', 'FLOAT', VO_PTR,
     :                        VO_SLOT, STATUS)
      END IF
C
C     Copy the input X arrays into the output X array.
C
      IF ( (.NOT. EASY) .AND. STATUS .EQ. 0 ) THEN
         IF (REVERSE) THEN
            CALL GEN_MOVE(NX1*DSA_TYPESIZE('FLOAT',STATUS),
     :                    %VAL(CNF_PVAL(A1_PTR)),%VAL(CNF_PVAL(AO_PTR)))
            CALL DYN_INCAD(AO_PTR,'FLOAT',NX1,OAO_PTR,ISNEW,STATUS)
            CALL GEN_MOVE(NX*DSA_TYPESIZE('FLOAT',STATUS),
     :                    %VAL(CNF_PVAL(A_PTR)),
     :                    %VAL(CNF_PVAL(OAO_PTR)))
         ELSE
            CALL GEN_MOVE(NX*DSA_TYPESIZE('FLOAT',STATUS),
     :                    %VAL(CNF_PVAL(A_PTR)),%VAL(CNF_PVAL(AO_PTR)))
            CALL DYN_INCAD(AO_PTR,'FLOAT',NX,OAO_PTR,ISNEW,STATUS)
            CALL GEN_MOVE(NX1*DSA_TYPESIZE('FLOAT',STATUS),
     :                    %VAL(CNF_PVAL(A1_PTR)),
     :                    %VAL(CNF_PVAL(OAO_PTR)))
         END IF
         IF (ISNEW) CALL CNF_UNREGP(OAO_PTR)
      END IF
C
C     See if we are going to have to sort the data.  If we are, get
C     workspace for the sort vector, calculate it, and re-order the
C     output X array.  The workspace obtained contains two arrays,
C     one the sort vector and one the workspace needed by the indirect
C     sorting routine GEN_FVSORT.
C
      SORT=.FALSE.
      IF ( (.NOT. EASY) .AND. STATUS .EQ. 0 ) THEN
         IF (.NOT.GEN_INCCHK(%VAL(CNF_PVAL(AO_PTR)),NX+NX1)) THEN
            IF (.NOT.OVRLAP) THEN
               CALL PAR_WRUSER(
     :           'The resulting X array is not in increasing order.',
     :           STATUS)
               CALL PAR_WRUSER(
     :           'The data arrays will be sorted into increasing '//
     :           'X-order.',STATUS)
            END IF
            SORT=.TRUE.
            BYTES=(NX+NX1)*DSA_TYPESIZE('FLOAT',STATUS)*2
            CALL DSA_GET_WORK_ARRAY (NX+NX1, 'FLOAT', VW_PTR, W_SLOT,
     :                               STATUS)
            CALL DSA_GET_WORK_ARRAY (NX+NX1, 'FLOAT', WK_PTR, WK_SLOT,
     :                               STATUS)
            IF (STATUS .EQ. 0) THEN
               CALL GEN_QFISORT(%VAL(CNF_PVAL(AO_PTR)),NX+NX1,
     :                          %VAL(CNF_PVAL(VW_PTR)))
               CALL GEN_FVSORT(%VAL(CNF_PVAL(VW_PTR)),NX+NX1,1,
     :                         %VAL(CNF_PVAL(WK_PTR)),
     :                         %VAL(CNF_PVAL(AO_PTR)))
            END IF
         END IF
      END IF
C
C     Now copy the data and error arrays as necessary into
C     the output structure. Sort them according to the sorted axis
C     array as required
C
      IF (STATUS .EQ. 0) THEN
         IF (REVERSE) THEN
            CALL DYN_INCAD(DO_PTR,'FLOAT',NX1,ODO_PTR,ISNEW,STATUS)
            CALL GEN_MOVE(NX1*DSA_TYPESIZE('FLOAT',STATUS),
     :                    %VAL(CNF_PVAL(D1_PTR)),%VAL(CNF_PVAL(DO_PTR)))
            CALL GEN_MOVE(NX*DSA_TYPESIZE('FLOAT',STATUS),
     :                    %VAL(CNF_PVAL(D_PTR)),
     :                    %VAL(CNF_PVAL(ODO_PTR)))
            IF (ERRORS) THEN
               CALL DYN_INCAD(VO_PTR,'FLOAT',NX1,OVO_PTR,ISNEWV,STATUS)
               CALL GEN_MOVE(NX1*DSA_TYPESIZE('FLOAT',STATUS),
     :                       %VAL(CNF_PVAL(V1_PTR)),
     :                       %VAL(CNF_PVAL(VO_PTR)))
               CALL GEN_MOVE(NX*DSA_TYPESIZE('FLOAT',STATUS),
     :                       %VAL(CNF_PVAL(V_PTR)),
     :                       %VAL(CNF_PVAL(OVO_PTR)))
            END IF
         ELSE
            CALL DYN_INCAD(DO_PTR,'FLOAT',NX,ODO_PTR,ISNEW,STATUS)
            CALL GEN_MOVE(NX*DSA_TYPESIZE('FLOAT',STATUS),
     :                    %VAL(CNF_PVAL(D_PTR)),%VAL(CNF_PVAL(DO_PTR)))
            CALL GEN_MOVE(NX1*DSA_TYPESIZE('FLOAT',STATUS),
     :                    %VAL(CNF_PVAL(D1_PTR)),
     :                    %VAL(CNF_PVAL(ODO_PTR)))
            IF (ERRORS) THEN
               CALL DYN_INCAD(VO_PTR,'FLOAT',NX,OVO_PTR,ISNEWV,STATUS)
               CALL GEN_MOVE(NX*DSA_TYPESIZE('FLOAT',STATUS),
     :                       %VAL(CNF_PVAL(V_PTR)),
     :                       %VAL(CNF_PVAL(VO_PTR)))
               CALL GEN_MOVE(NX1*DSA_TYPESIZE('FLOAT',STATUS),
     :                       %VAL(CNF_PVAL(V1_PTR)),
     :                       %VAL(CNF_PVAL(OVO_PTR)))
            END IF
         END IF
         IF (ISNEW) CALL CNF_UNREGP(ODO_PTR)
         IF (ERRORS.AND.ISNEW) CALL CNF_UNREGP(OVO_PTR)

         IF (SORT.AND.(STATUS .EQ. 0)) THEN
            CALL GEN_FVSORT(%VAL(CNF_PVAL(VW_PTR)),NX+NX1,1,
     :                      %VAL(CNF_PVAL(WK_PTR)),
     :                      %VAL(CNF_PVAL(DO_PTR)))
            IF (ERRORS) THEN
               CALL GEN_FVSORT(%VAL(CNF_PVAL(VW_PTR)),NX+NX1,1,
     :                         %VAL(CNF_PVAL(WK_PTR)),
     :                         %VAL(CNF_PVAL(VO_PTR)))
            END IF
         END IF
      END IF
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
