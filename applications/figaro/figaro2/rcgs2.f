      SUBROUTINE RCGS2
C+
C     R C G S 2
C
C     Reads a UKIRT CGS2 spectrum out of its original container file
C     and create a Figaro spectrum from it. (Also works for UKT9
C     and UKT6 CVF spectra).
C
C     Command parameters -
C
C     FILE    (Character) The name of the container file
C     OBS     (Numeric) The observation number to be read from
C             the container file.
C     SCAN    (Numeric) The scan number to be read from the observation
C             (Only used if TWOD is not set) use zero to read the
C             coadded data.
C     OUTPUT  (Character) The name of the Figaro file to be created.
C
C     Command keywords -
C
C     TWOD    If set create a 2D array of wavelength by scan
C             number.
C
C     User variables used - None
C
C                                      JAB / JAC  25th Feb 1990
C
C    Modified:
C      7 Dec 1990  JAB/JAC. Add TWOD option.
C      8 Dec 1990  JAB/JAC. Use quality array.
C     10 Dec 1990  JAB/JAC. Build Pixels structure.
C      5 Oct 1992  HME/UoE, Starlink. INCLUDEs changed.
C     13 Mar 1996  HME / UoE, Starlink.  Adapt to the FDA library.
C                  No map access with DTA. Get work array and read/write
C                  instead.
C                  No error messages from DTA. Call PAR_WRUSER instead
C                  of DTA_ERROR.
C     18 Jul 1996  MJCL / Starlink, UCL.  Set variables for storage of
C                  file names to 132 chars.
C     11 Dec 1997  ACD / UoE, Starlink. Fixed bug so that locator SLOC
C                  is checked for validity prior to the final call to
C                  annul it.
C     2005 June 14 MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions used
C
      CHARACTER*12 ICH_CI
      INTEGER ICH_LEN
C
C
C     Local variables
C
      INTEGER STATUS
      INTEGER STAT
      CHARACTER*132  FILE
      CHARACTER*(DAT__SZLOC) FLOC,OLOC,SLOC,ZLOC,XLOC,ELOC,QLOC,LOC
      CHARACTER*(DAT__SZLOC) CLOC
      LOGICAL SLOCV
      INTEGER IGNORE
      REAL VALUE
      INTEGER OBS
      INTEGER SCAN
      INTEGER NDIMO
      INTEGER DIMS(2),DIMS2(2)
      INTEGER DIM
      INTEGER LEN
      INTEGER NDIM,NDIM2
      INTEGER ZPTR,XPTR,EPTR,QPTR,CPTR
      INTEGER OZPTR,OXPTR,OEPTR,OQPTR,OPPTR,OSPTR
      INTEGER ZSLOT,XSLOT,ESLOT,QSLOT,SLOT
      CHARACTER*20 CBUF(2)
      CHARACTER*20 INST,OBJECT
      CHARACTER*132 PIXNAM,PNAME,PONAME
      LOGICAL FOPEN
      LOGICAL TWOD
      INTEGER NSCANS
C
C     Initialization of DSA routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS .NE. 0) GOTO 500
C
C     Get the name of the container file to be opened
C
      CALL PAR_RDCHAR('FILE',' ',FILE)
      CALL HDS_OPEN(FILE,'READ',FLOC,STATUS)
      IF (STATUS .NE. 0) THEN
          CALL PAR_WRUSER('Unable to open container file '//FILE,
     :        IGNORE)
          FOPEN = .FALSE.
          GOTO 500
      ELSE
          FOPEN = .TRUE.
      END IF
C
C     Get observation number
C
      CALL PAR_RDVAL('OBS',0.0,200.0,1.0,' ',VALUE)
      OBS = NINT(VALUE)
      CALL DAT_FIND(FLOC,'OBS_'//ICH_CI(OBS),OLOC,STATUS)
      IF (STATUS .NE. 0) THEN
          CALL PAR_WRUSER('No observation number '//ICH_CI(OBS),
     :        IGNORE)
          GOTO 500
      END IF
C
C     Get TWOD keyword
C
      CALL PAR_RDKEY('TWOD',.FALSE.,TWOD)
C
C     Get scan number - if zero find coadded data arrays, else find
C      data arrays for scan.
C
      IF (.NOT. TWOD) THEN
          CALL PAR_RDVAL('SCAN',0.0,1000.0,0.0,' ',VALUE)
          SCAN = NINT(VALUE)
      ELSE
          SCAN = 1
      END IF
      IF (SCAN .EQ. 0 .AND. (.NOT. TWOD)) THEN
          CALL DAT_FIND(OLOC,'DATA_ARRAY',ZLOC,STATUS)
          CALL DAT_FIND(OLOC,'DATA_ERROR',ELOC,STATUS)
          CALL DAT_FIND(OLOC,'DATA_QUALITY',QLOC,STATUS)
          CALL DAT_FIND(OLOC,'AXIS1_DATA',XLOC,STATUS)
          CALL DAT_FIND(OLOC,'CGS2_CROSSREF',CLOC,STATUS)
      ELSE
          CALL DAT_FIND(OLOC,'SCAN_'//ICH_CI(SCAN),SLOC,STATUS)
          IF (STATUS .NE. 0) THEN
              CALL PAR_WRUSER('No scan number '//ICH_CI(OBS),
     :            IGNORE)
              GOTO 500
          END IF
          CALL DAT_FIND(SLOC,'DATA_ARRAY',ZLOC,STATUS)
          CALL DAT_FIND(SLOC,'DATA_ERROR',ELOC,STATUS)
          CALL DAT_FIND(SLOC,'DATA_QUALITY',QLOC,STATUS)
          CALL DAT_FIND(SLOC,'AXIS1_DATA',XLOC,STATUS)
          CALL DAT_FIND(OLOC,'CGS2_CROSSREF',CLOC,STATUS)
      END IF
      IF (STATUS .NE. 0) THEN
          CALL PAR_WRUSER('Component not found ',IGNORE)
          GOTO 500
      END IF
C
C     Get number of scans
C
      IF (TWOD) THEN
          CALL DAT_FIND(OLOC,'SCAN_TOT',LOC,STATUS)
          CALL DAT_GET0I(LOC,NSCANS,STATUS)
          CALL DAT_ANNUL(LOC,STATUS)
      ELSE
          NSCANS = 1
      END IF
      DIMS(2) = NSCANS
C
C     Get dimensions of data
C
      CALL DAT_SHAPE(ZLOC,1,DIM,NDIM,STATUS)
      IF (NDIM .NE. 1) THEN
          CALL PAR_WRUSER('Not 1 Dimensional Data',IGNORE)
          GOTO 500
      END IF
      DIMS(1) = DIM
C
C     Map input data
C
      CALL DAT_MAPR(ZLOC,'READ',NDIM,DIM,ZPTR,STATUS)
      CALL DAT_MAPR(XLOC,'READ',NDIM,DIM,XPTR,STATUS)
      CALL DAT_MAPR(ELOC,'READ',NDIM,DIM,EPTR,STATUS)
      CALL DAT_MAPI(QLOC,'READ',NDIM,DIM,QPTR,STATUS)
      CALL DAT_SHAPE(CLOC,2,DIMS2,NDIM2,STATUS)
      CALL DAT_MAPI(CLOC,'READ',NDIM2,DIMS2,CPTR,STATUS)
      IF (STATUS .NE. 0) THEN
          CALL PAR_WRUSER('Error mapping input data',IGNORE)
          GOTO 500
      END IF
C
C     Create output structure
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT',' ',0,1,STATUS)
      IF (TWOD) THEN
         NDIMO=2
      ELSE
         NDIMO=1
         DIMS(2)=1
      END IF
      CALL DSA_SIMPLE_OUTPUT('OUTPUT','D,A1,E,Q','FLOAT',NDIMO,DIMS,
     :           STATUS)
C
      CALL DSA_USE_QUALITY('OUTPUT',STATUS)
C
C     Create PIXELS structure
C
      CALL DSA_SPECIFIC_STRUCTURE('OUTPUT','MORE','WRITE',
     :    PIXNAM,STATUS)
      LEN = ICH_LEN(PIXNAM)
      CALL DTA_CRVAR(PIXNAM(1:LEN)//'.PIXELS','Struct',STATUS)
      CALL DTA_CRNAM(PIXNAM(1:LEN)//'.PIXELS','PIXEL',1,DIMS,PNAME,
     :     STATUS)
      CALL DTA_CRVAR(PNAME,'INT',STATUS)
      CALL DTA_CRNAM(PIXNAM(1:LEN)//'.PIXELS','POSITION',1,DIMS,
     :     PONAME,STATUS)
      CALL DTA_CRVAR(PONAME,'INT',STATUS)
      CALL DTA_CRVAR(PIXNAM(1:LEN)//'.PIXELS.NPIXELS','INT',STATUS)
      CALL DTA_CRVAR(PIXNAM(1:LEN)//'.PIXELS.NPOSITIONS','INT',STATUS)
      CALL DTA_WRVARI(PIXNAM(1:LEN)//'.PIXELS.NPIXELS',1,
     :    DIMS2(1),STATUS)
      CALL DTA_WRVARI(PIXNAM(1:LEN)//'.PIXELS.NPOSITIONS',1,
     :    DIMS2(2),STATUS)
C
C     Create axis label and units
C
      CBUF(2) = 'Wavelength'
      CBUF(1) = 'microns'
      CALL DSA_SET_AXIS_INFO('OUTPUT',1,2,CBUF,0,0D0,STATUS)
C
C     Get information on observation
C
      CALL DAT_FIND(OLOC,'INSTRUMENT',LOC,STATUS)
      CALL DAT_GET0C(LOC,INST,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      CALL DAT_FIND(OLOC,'OBJECT_NAME',LOC,STATUS)
      CALL DAT_GET0C(LOC,OBJECT,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
C
C     Map output arrays
C
      CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',OZPTR,ZSLOT,STATUS)
      CALL DSA_MAP_ERRORS('OUTPUT','WRITE','FLOAT',OEPTR,ESLOT,STATUS)
      CALL DSA_MAP_QUALITY('OUTPUT','WRITE','BYTE',OQPTR,QSLOT,STATUS)
      CALL DSA_MAP_AXIS_DATA('OUTPUT',1,'WRITE','FLOAT',OXPTR,XSLOT,
     :                       STATUS)
      CALL DTA_CRNAM(PIXNAM(1:LEN)//'.PIXELS','PIXEL',0,0,PNAME,
     :               STATUS)
      CALL DTA_CRNAM(PIXNAM(1:LEN)//'.PIXELS','POSITION',0,0,
     :               PONAME,STATUS)
C     CALL DTA_MUVARI(PNAME,DIMS(1),OPPTR,STATUS)
C     CALL DTA_MUVARI(PONAME,DIMS(1),OSPTR,STATUS)
      CALL DSA_GET_WORK_ARRAY(DIMS(1),'INT',OPPTR,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(DIMS(1),'INT',OSPTR,SLOT,STATUS)
      IF (STATUS .NE. 0) THEN
         CALL PAR_WRUSER('Error accessing output arrays.',IGNORE)
         GOTO 500
      END IF
C
C     Copy data to output arrays
C
      IF (STATUS .EQ. 0) THEN
         CALL RCGS2_COPY2(DIM,NSCANS,1,%VAL(CNF_PVAL(ZPTR)),
     :                    %VAL(CNF_PVAL(OZPTR)))
         CALL RCGS2_COPY(DIM,%VAL(CNF_PVAL(XPTR)),
     :                   %VAL(CNF_PVAL(OXPTR)))
         CALL RCGS2_COPY2(DIM,NSCANS,1,%VAL(CNF_PVAL(EPTR)),
     :                    %VAL(CNF_PVAL(OEPTR)))
         CALL RCGS2_COPYQ(DIM,NSCANS,1,%VAL(CNF_PVAL(QPTR)),
     :                    %VAL(CNF_PVAL(OQPTR)))
         CALL RCGS2_COPYP(DIM,DIMS2(1),DIMS2(2),%VAL(CNF_PVAL(CPTR)),
     :                    %VAL(CNF_PVAL(OPPTR)),%VAL(CNF_PVAL(OSPTR)))
      END IF
C
C     If two-d copy remaining scans
C
      IF (TWOD) THEN
         CALL DAT_ANNUL(ZLOC,STATUS)
         CALL DAT_ANNUL(ELOC,STATUS)
         CALL DAT_ANNUL(QLOC,STATUS)
         CALL DAT_ANNUL(SLOC,STATUS)
         DO SCAN = 2,NSCANS
            CALL DAT_FIND(OLOC,'SCAN_'//ICH_CI(SCAN),SLOC,STATUS)
            IF (STATUS .NE. 0) THEN
               CALL PAR_WRUSER('No scan number '//ICH_CI(OBS),IGNORE)
               GOTO 500
            END IF
           CALL DAT_FIND(SLOC,'DATA_ARRAY',ZLOC,STATUS)
           CALL DAT_FIND(SLOC,'DATA_ERROR',ELOC,STATUS)
           CALL DAT_FIND(SLOC,'DATA_QUALITY',QLOC,STATUS)
           CALL DAT_MAPR(ZLOC,'READ',NDIM,DIM,ZPTR,STATUS)
           CALL DAT_MAPR(ELOC,'READ',NDIM,DIM,EPTR,STATUS)
           CALL DAT_MAPI(QLOC,'READ',NDIM,DIM,QPTR,STATUS)
           IF (STATUS .EQ. 0) THEN
              CALL RCGS2_COPY2(DIM,NSCANS,SCAN,%VAL(CNF_PVAL(ZPTR)),
     :                         %VAL(CNF_PVAL(OZPTR)))
              CALL RCGS2_COPY2(DIM,NSCANS,SCAN,%VAL(CNF_PVAL(EPTR)),
     :                         %VAL(CNF_PVAL(OEPTR)))
              CALL RCGS2_COPYQ(DIM,NSCANS,SCAN,%VAL(CNF_PVAL(QPTR)),
     :                         %VAL(CNF_PVAL(OQPTR)))
           END IF
           CALL DAT_ANNUL(ZLOC,STATUS)
           CALL DAT_ANNUL(ELOC,STATUS)
           CALL DAT_ANNUL(QLOC,STATUS)
           CALL DAT_ANNUL(SLOC,STATUS)
         END DO
      END IF
C
C     Tidy up.  Note that SLOC may or may not be valid.  Therefore it
C     is checked for validity prior to annulling it.
C
500   IF (FOPEN) THEN
C        CALL DTA_FRVAR(PNAME,IGNORE)
C        CALL DTA_FRVAR(PONAME,IGNORE)
         CALL DTA_WRVARI(PNAME,DIMS(1),%VAL(CNF_PVAL(OPPTR)),STATUS)
         CALL DTA_WRVARI(PONAME,DIMS(1),%VAL(CNF_PVAL(OSPTR)),STATUS)
         CALL DAT_ANNUL(XLOC,STATUS)
C
         CALL DAT_VALID(SLOC,SLOCV,STATUS)
         IF (SLOCV) THEN
            CALL DAT_ANNUL(SLOC,STATUS)
         END IF
C
         CALL DAT_ANNUL(CLOC,STATUS)
         CALL DAT_ANNUL(OLOC,STATUS)
C
         STAT = 0
         CALL HDS_CLOSE(FLOC,STAT)
      END IF
      CALL DSA_CLOSE(STATUS)
      END


      SUBROUTINE RCGS2_COPY(NX,XA,XB)
C
C     Copy input array XA to output array XB
C
      IMPLICIT NONE
      INTEGER NX
      REAL XA(NX),XB(NX)
      INTEGER I
      DO I=1,NX
         XB(I) = XA(I)
      END DO
      END


      SUBROUTINE RCGS2_COPY2(NX,NY,IY,XA,XB)
C
C     Copy input array XA to output array XB
C
      IMPLICIT NONE
      INTEGER NX,NY,IY
      REAL XA(NX),XB(NX,NY)
      INTEGER I
      DO I=1,NX
         XB(I,IY) = XA(I)
      END DO
      END


      SUBROUTINE RCGS2_COPYQ(NX,NY,IY,XA,XB)
C
C     Copy input quality array XA to output array XB
C
      IMPLICIT NONE
      INTEGER NX,NY,IY
      INTEGER XA(NX)
      BYTE XB(NX,NY)
      INTEGER I
      DO I=1,NX
         XB(I,IY) = XA(I)
      END DO
      END


      SUBROUTINE RCGS2_COPYP(NX,NP,NS,IN,PIX,SCAN)
C
C   Build Pixel and Scan Position arrays using input Cross Reference
C    array
C

      IMPLICIT NONE
      INTEGER NX,NP,NS,IN(NP,NS),PIX(NX),SCAN(NX)
      INTEGER P,S

      DO P=1,NP
         DO S=1,NS
            PIX(IN(P,S)) = P
            SCAN(IN(P,S)) = S
         END DO
      END DO
      END
