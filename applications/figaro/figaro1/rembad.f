C+
      SUBROUTINE REMBAD
C
C     R E M B A D
C
C     Remove from a spectrum points which are flagged as bad or
C     shown as bad in the quality array. The main purpose of this
C     command is to allow such spectra to be correctly processed by
C     FIGARO commands which do not support data quality.
C
C     Command parameters -
C
C     SPECTRUM    The spectrum from which bad points will be removed.
C     OUTPUT      The resulting spectrum.
C
C     Command keywords -
C
C
C                                         JAB/ JAC  16th Dec 1990
C
C     Note that this subroutine ignores any quality array associated
C     with the input image and creates no quality array for the
C     output image.
C                                      ACD / Starlink 23 Feb 2001
C
C     Modified:
C
C     04 Feb 1991  JMS / AAO. Added STATUS checks to abort. Modified
C                  to use flagged data. Now aborts if all data is bad,
C                  or if quality data is non-existent. Tidied up last
C                  message.
C     24 Mar 1991  JMS / AAO. Removed 'COMMAND' option (not used at
C                  all).
C     07 Oct 1992  HME / UoE, Starlink.  INCLUDE changed.
C                  Map the data after (!) telling DSA that we use
C                  quality or bad values. Otherwise we don't get any
C                  from DSA. Also we have to actually call
C                  DSA_USE_FLA...
C     16 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  Bad pixel handling.
C     23 Feb 2001  ACD / UoE, Starlink. Initialise the pointer to
C                  the quality array to zero (previously it was
C                  uninitialised).
C     2005 June 10 MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
CC
C     Functions
C
      CHARACTER ICH_CI*13
      INTEGER ICH_LEN
C
C     Local variables
C
      INTEGER   DPTR             ! Dynamic-memory pointer to data array
      INTEGER   DSLOT            ! Map slot number used for data
      INTEGER   EPTR             ! Dynamic-memory pointer to error array
      LOGICAL   ERRUSE           ! True if errors to be used
      INTEGER   ESLOT            ! Map slot used for error data
      REAL      FBAD             ! Value of flagged data
      LOGICAL   FLAGS            ! True if flagged data exists
      INTEGER   IGNORE           ! Used for disregarded status codes
      INTEGER   IPT              ! Used to print message at end
      INTEGER   NDIM             ! Dimensionality of input spectrum
      INTEGER   NELM             ! Number of elements in data
      INTEGER   NGOOD            ! Number of good points in spectrum
      INTEGER   NX               ! Number of elements in data - ignored
      INTEGER   ODPTR            ! Temporary Data pointer
      INTEGER   OEPTR            ! Temporary Error pointer
      INTEGER   OXPTR            ! Temporary X pointer
      INTEGER   OSLOT            ! Temporary slot
      INTEGER   QPTR             ! Dynamic-memory pointer to quality
      INTEGER   STATUS           ! Status return from DSA_ routines
      CHARACTER*80 STRING        ! To print message at end of programme
      INTEGER   XPTR             ! Dynamic-memory pointer to X-axis data
      INTEGER   XSLOT            ! Map slot used for X-axis data
C
C     Initialisation of DSA_ routines
C
      STATUS=0
C
      CALL DSA_OPEN (STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Initial assumptions
C
      ERRUSE=.FALSE.
C
C     Open input spectrum.
C
      CALL DSA_INPUT ('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensions of data
C
      CALL DSA_DATA_SIZE ('SPECT',1,NDIM,NX,NELM,STATUS)
C
C     Map the X-axis data array (will be 1..N if no such array).
C
      CALL DSA_MAP_AXIS_DATA ('SPECT',1,'READ','FLOAT',XPTR,
     :                        XSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map the spectrum data
C
      CALL DSA_GET_FLAG_VALUE('FLOAT',FBAD,STATUS)
      CALL DSA_USE_FLAGGED_VALUES('SPECT',STATUS)
      CALL DSA_MAP_DATA ('SPECT','READ','FLOAT',DPTR,DSLOT,STATUS)
      CALL DSA_SEEK_FLAGGED_VALUES('SPECT',FLAGS,STATUS)
      IF (.NOT.FLAGS) THEN
         CALL DSA_WRUSER('Quality data does not exist - the data '//
     :                  'file will be left unaltered.')
         GO TO 500
      END IF
C
C     Try to map the error data
C
      CALL DSA_SEEK_ERRORS ('SPECT',ERRUSE,STATUS)
      IF (ERRUSE) THEN
         CALL DSA_MAP_ERRORS ('SPECT','READ','FLOAT',EPTR,ESLOT,STATUS)
      END IF
      IF (STATUS.NE.0) GO TO 500
C
C     Are there any bad points? If so remove them.
C
      QPTR = 0
      CALL FIG_REMBAD_NGOOD(NELM,%VAL(CNF_PVAL(QPTR)),
     :                      %VAL(CNF_PVAL(DPTR)),
     :                      .FALSE.,FLAGS,FBAD,NGOOD)
C
      IF (NGOOD.EQ.0) THEN
         CALL DSA_WRUSER(
     :      'All data points in the file are bad.')
         GO TO 500
      END IF
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','SPECT',1,1,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
      CALL DSA_RESHAPE_DATA('OUTPUT','SPECT',1,NGOOD,STATUS)
      CALL DSA_RESHAPE_AXIS('OUTPUT',1,'SPECT',1,1,NGOOD,STATUS)
      CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',ODPTR,OSLOT,STATUS)
C
      IF (ERRUSE) THEN
          CALL DSA_MAP_ERRORS('OUTPUT','WRITE','FLOAT',OEPTR,
     :                        OSLOT,STATUS)
      END IF
C
      CALL DSA_MAP_AXIS_DATA('OUTPUT',1,'WRITE','FLOAT',OXPTR,
     :                       OSLOT,STATUS)
C
      IF (STATUS .EQ. 0) THEN
          CALL FIG_REMBAD(NELM,%VAL(CNF_PVAL(DPTR)),
     :                    %VAL(CNF_PVAL(EPTR)),%VAL(CNF_PVAL(XPTR)),
     :                    %VAL(CNF_PVAL(QPTR)),ERRUSE,.FALSE.,FLAGS,
     :                    FBAD,NGOOD,%VAL(CNF_PVAL(ODPTR)),
     :                    %VAL(CNF_PVAL(OEPTR)),%VAL(CNF_PVAL(OXPTR)))
      END IF
C
C     Inform user of the number of bad points removed
C
      STRING=ICH_CI(NELM-NGOOD)
      IPT=ICH_LEN(STRING)
      STRING(IPT+2:)='bad points removed.'
      CALL PAR_WRUSER(STRING,IGNORE)
C
C     Close down everything
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END


      SUBROUTINE FIG_REMBAD_NGOOD(NELM,QUALITY,DATA,QUAL,FLAGS,FBAD,
     :                                                         NGOOD)
C
C     Count the number of good points in a spectrum
C
      INTEGER NELM,NGOOD
      BYTE QUALITY(NELM)
      REAL FBAD,DATA(NELM)
      LOGICAL QUAL,FLAGS

      INTEGER IX

      NGOOD=0
      DO IX=1,NELM
          IF ((QUAL.AND.(QUALITY(IX).EQ.0)).OR.
     :       (FLAGS.AND.(DATA(IX).NE.FBAD))) NGOOD=NGOOD+1
      END DO
      END


      SUBROUTINE FIG_REMBAD(NELM,D1,E1,X1,QUALITY,ERRUSE,QUAL,FLAGS,
     :                                            FBAD,NGOOD,D2,E2,X2)
C
C     Make copy of spectrum with bad points removed
C
      IMPLICIT NONE
      INTEGER NELM,NGOOD
      REAL D1(NELM),E1(NELM),X1(NELM),D2(NGOOD),E2(NGOOD),X2(NGOOD)
      REAL FBAD
      BYTE QUALITY(NELM)
      LOGICAL ERRUSE,QUAL,FLAGS

      INTEGER I2,I1

      I2=1
      DO I1=1,NELM
          IF ((QUAL.AND.(QUALITY(I1).EQ.0)).OR.
     :                  (FLAGS.AND.(D1(I1).NE.FBAD))) THEN
              D2(I2)=D1(I1)
              X2(I2)=X1(I1)
              IF (ERRUSE) E2(I2)=E1(I1)
              I2=I2+1
          END IF
      END DO
      END
