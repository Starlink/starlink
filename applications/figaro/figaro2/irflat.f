C+
      SUBROUTINE IRFLAT
C
C     I R F L A T
C
C     Figaro function that produces a "flatfield" ripple 
C     spectrum from a infrared  spectrum, by averaging the data from 
C     regions of the spectrum uncontaminated with spectral features
C     (i.e. assumed flat) to determine the relative response of each
C     detector or scan. The output spectrum can be divided into the original
C     spectrum using IDIV to flat field the data.
C
C     The program is used to remove two kinds of ripple from spectra.
C     In instruments which interleave a number of scan positions to give
C     a fully sampled spectrum (such as CGS3 and CGS4), the program removes 
C     ripple which results from seeing or transparency fluctuations between
C     scan positions. In an instrument such as CGS2 it can remove the ripple
C     which results from the fact that the flatfield (i.e. relative detector
C     responses) is different for extended and point sources. In the case of
C     CGS2 data it makes use of a .MORE.PIXELS extension in the data which
C     specifies the detector and scan position corresponding to each pixel.
C     If this structure is not present it prompts for a period and assumes
C     a periodic ripple. The period will normally be the oversampling factor,
C     typically 2 or 3 for CGS4 or CGS3 data.
C
C     If the program is run in batch only one region can be specified.
C     Multiple regions can only be specified in interactive mode.
C
C     Command parameters -
C
C     SPECTRUM    (Character) The name of the file containing the
C                 spectrum to be used.
C     CGS2        (Logical) Flag indicating whether to operate in 'CGS 2'
C                 mode.
C     PERIOD      (Real) The period of the ripple (in pixels).
C     OUTPUT      (Character) The name of the resulting ripple spectrum.
C     XSTART      (Real) First X value for region to be used.
C     XEND        (Real) Second X value for region to be used.
C
C     Command keywords -
C
C     MORE        If TRUE the prompts for XSTART and XEND are repeated for
C                 another region.
C
C     10th Dec 1990 - JAB / JAC
C
C     Modified:
C        15 May 1991  Add handling for seeing ripple in CGS4 data.
C        04 Sep 1992  TAB removed, INCLUDE changed. HME/UoE.
C        21 Jul 1993  HME / UoE, Starlink. Swap the statements that ask
C                     for and cancel the MORE parameter: Now cancels after
C                     asking, thus command line argument is actually used.
C        28 Jun 1993  PND / JAC.  Output file not same as input file.
C                     This change incorporated into Portable Figaro by
C                     HME on 20 Jan 1994. But use PAR_WRUSER instead of
C                     DSA_WRUSER.
C        13 Mar 1996  HME / UoE, Starlink.  Adapt to the FDA library.
C                     Avoid _NAMED_ routines.
C                     No map access with DTA. Get work array and read/write
C                     instead.
C        18 Jul 1996  MJCL / Starlink, UCL.  Set variables for storage of
C                     file names to 132 chars.
C        19 Jul 1996  MJC / Starlink.  Extend the number of scan
C                     positions to ensure the scan-position array is
C                     filled.
C        29 Jul 1996  MJCL / Starlink, UCL.  PAR_ABORT checking.
C        11 Jul 2001  ACD  / Starlink, UoE.  Suppressed the error messages
C                     issued in the case where the CGS2 'PIXELS' structure
C                     could not be found in the input data file.
C        13 Jul 2001  ACD  / Starlink, UoE.  Made to check whether a
C                     valid value for 'PERIOD' is available prior to
C                     checking for the CGS2 'PIXELS' structure.  These
C                     changes supercede those of 11 Jul 2001.
C        17 Jul 2001  ACD  / Starlink, UoE.  Improved the prologue
C                     comments.
C+
      IMPLICIT NONE
C
C     Functions used
C
      INTEGER ICH_ENCODE, ICH_LEN, DYN_ELEMENT
      INTEGER DSA_TYPESIZE
      LOGICAL PAR_BATCH
      LOGICAL PAR_ABORT         ! (F)PAR abort flag
C
C     Local variables
C
      INTEGER      ADDRESS      ! Address of dynamic memory element
      LOGICAL      CGS2         ! True for CGS2 data
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      INTEGER      DPTR         ! Dynamic-memory pointer to detector numbers
      INTEGER      EPTR         ! Dynamic-memory pointer to error array
      CHARACTER*80 ERR          ! Error string
      CHARACTER*132 SPECTRUM    ! The input spectrum name
      CHARACTER*132 OUTPUT      ! The output spectrum name
      INTEGER      I            !
      INTEGER      IGNORE       ! Used to pass ignorable status
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      IXST         ! start pixel for flatfielding
      INTEGER      IXEN         ! end pixel for flatfielding
      INTEGER      LEN          ! Length of PIXNAME
      LOGICAL      MORE         ! TRUE if more ranges
      INTEGER      ND           ! Number of pixels
      INTEGER      NELM         ! Total number of elements in data
      INTEGER      NPTR         ! Dynamic-memory pointer to NUM array
      INTEGER      NS           ! Number of scan positions
      INTEGER      NX           ! Size of 1st dimension
      INTEGER      OPTR         ! Dynamic-memory pointer to output data array
      REAL         PERIOD       ! Ripple period
      CHARACTER*132 PIXNAME     ! Name of Pixels structure
      CHARACTER*132 PNAME       ! Name of .PIXEL array
      CHARACTER*132 PONAME      ! Name of .POSITION array
      INTEGER      QPTR         ! Dynamic-memory pointer to quality array
      INTEGER      SLOT         ! Map slot number outputdata array
      INTEGER      SPTR         ! Dynamic-memory pointer to scan positions
      INTEGER      STATUS       ! Running status for DSA_ routines
      CHARACTER*80 STRING       ! Output string
      INTEGER      SUMPTR       ! Dynamic-memory pointer to SUM array
      INTEGER      UPTR         ! Dynamic-memory pointer to USES array
      REAL         XS           ! Starting X value
      REAL         XE           ! Ending X value
C    
C     Dynamic memory support - defines DYNAMIC_MEM
C
      INCLUDE 'DYNAMIC_MEMORY'
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get name of Spectrum to be used and open it
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      CALL DSA_GET_ACTUAL_NAME('SPECT',SPECTRUM,STATUS)
      CALL DSA_DATA_SIZE('SPECT',1,NDIM,DIMS,NX,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Attempt to obtain the mode.
C
      CALL PAR_GET0L ('CGS2',CGS2,STATUS)
C
      IF (.NOT. CGS2) THEN
         CALL PAR_RDVAL('PERIOD',1.0,REAL(NX),2.0,' ',PERIOD)
         IF ( PAR_ABORT() ) GO TO 500
         ND = NINT(PERIOD)
         NS=NX/ND
         IF ( NS * ND .LT. NX ) NS = NS + 1
         CALL DSA_GET_WORK_ARRAY(NX,'INT',ADDRESS,SLOT,STATUS)
         SPTR=DYN_ELEMENT(ADDRESS)
         CALL DSA_GET_WORK_ARRAY(NX,'INT',ADDRESS,SLOT,STATUS)
         DPTR=DYN_ELEMENT(ADDRESS)
         CALL IRFLAT_FILL(NX,ND,NS,DYNAMIC_MEM(SPTR),
     :      DYNAMIC_MEM(DPTR))
      ELSE
C
C     Find and map PIXEL and POSITIONS arrays
C
         CALL DSA_SPECIFIC_STRUCTURE('SPECT','MORE','READ',PIXNAME,
     :     STATUS)
         LEN=ICH_LEN(PIXNAME)
         IF (STATUS .EQ. 0)
     :     CALL DTA_CRNAM(PIXNAME(1:LEN)//'.PIXELS','PIXEL',0,0,PNAME,
     :        STATUS)
         IF (STATUS .EQ. 0)
     :     CALL DTA_CRNAM(PIXNAME(1:LEN)//'.PIXELS','POSITION',0,0,
     :        PONAME,STATUS)
         IF (STATUS .EQ. 0)
     :     CALL DTA_RDVARI(PIXNAME(1:LEN)//'.PIXELS.NPIXELS',1,ND,
     :        STATUS)
         IF (STATUS .EQ. 0)
     :     CALL DTA_RDVARI(PIXNAME(1:LEN)//'.PIXELS.NPOSITIONS',1,NS,
     :        STATUS)
C
         CALL DSA_GET_WORK_ARRAY(NX,'INT',ADDRESS,SLOT,STATUS)
         DPTR=DYN_ELEMENT(ADDRESS)
         IF (STATUS .EQ. 0)
     :      CALL DTA_RDVARI(PNAME,NX,DYNAMIC_MEM(DPTR),STATUS)
         CALL DSA_GET_WORK_ARRAY(NX,'INT',ADDRESS,SLOT,STATUS)
         SPTR=DYN_ELEMENT(ADDRESS)
         IF (STATUS .EQ. 0)
     :      CALL DTA_RDVARI(PONAME,NX,DYNAMIC_MEM(SPTR),STATUS)
         IF (STATUS .NE. 0) THEN
             CALL PAR_WRUSER('Error Reading PIXELS Structure',IGNORE)
             GOTO 500
         ENDIF
      ENDIF
C
C     Get name of resulting output spectrum
C
      CALL PAR_SDCHAR('OUTPUT','RIP',STATUS)
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','SPECT',0,1,STATUS)
      CALL DSA_GET_ACTUAL_NAME('OUTPUT',OUTPUT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL PAR_WRUSER( 'Input spectrum name is '/
     :  /SPECTRUM(:ICH_LEN(SPECTRUM)), STATUS )
      CALL PAR_WRUSER( 'Output will be written to '/
     :  /OUTPUT(:ICH_LEN(OUTPUT)), STATUS )
      CALL DSA_USE_QUALITY('OUTPUT',STATUS)
C
C     Force creation of error array by mapping it, then zero it
C
      CALL DSA_MAP_ERRORS('OUTPUT','UPDATE','FLOAT',ADDRESS,SLOT,STATUS)
      EPTR = DYN_ELEMENT(ADDRESS)
      CALL GEN_FILL(NX*DSA_TYPESIZE('FLOAT',STATUS),0,
     :     DYNAMIC_MEM(EPTR))
C
C     Map data  and quality array
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',ADDRESS,SLOT,STATUS)
      OPTR=DYN_ELEMENT(ADDRESS)
      CALL DSA_MAP_QUALITY('OUTPUT','UPDATE','BYTE',ADDRESS,SLOT,STATUS)
      QPTR=DYN_ELEMENT(ADDRESS)
      IF (STATUS.NE.0) GOTO 500
C
C     Select initial region
C
      CALL PAR_WRUSER('Select Regions to be used to '//
     :    'generate flat field spectrum',IGNORE)
      CALL DSA_AXIS_RANGE('SPECT',1,' ',.FALSE.,XS,XE,IXST,IXEN,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get workspace arrays
C
      CALL DSA_GET_WORK_ARRAY(NS,'INT',ADDRESS,SLOT,STATUS)
      UPTR=DYN_ELEMENT(ADDRESS)
      CALL DSA_GET_WORK_ARRAY(ND,'FLOAT',ADDRESS,SLOT,STATUS)
      SUMPTR=DYN_ELEMENT(ADDRESS)
      CALL DSA_GET_WORK_ARRAY(ND,'INT',ADDRESS,SLOT,STATUS)
      NPTR=DYN_ELEMENT(ADDRESS)
      CALL GEN_FILL(ND*DSA_TYPESIZE('FLOAT',STATUS),0,
     :    DYNAMIC_MEM(SUMPTR))
      CALL GEN_FILL(ND*DSA_TYPESIZE('INT',STATUS),0,
     :    DYNAMIC_MEM(NPTR))
C
C     Operate on data
C
      CALL IRFLAT_FOLD(DYNAMIC_MEM(OPTR),DYNAMIC_MEM(QPTR),NX,
     :    IXST,IXEN,DYNAMIC_MEM(DPTR),
     :    DYNAMIC_MEM(SPTR),DYNAMIC_MEM(UPTR),NS,ND,DYNAMIC_MEM(SUMPTR),
     :    DYNAMIC_MEM(NPTR))
C
C     Repeat for more ranges if necessary (if not in batch)
C
      MORE = .NOT. (PAR_BATCH())
      DO WHILE (MORE)
          CALL PAR_RDKEY('MORE',.FALSE.,MORE)
          CALL PAR_CNPAR('MORE')
          IF (MORE) THEN
              CALL PAR_CNPAR('XSTART')
              CALL PAR_CNPAR('XEND')
              CALL DSA_AXIS_RANGE('SPECT',1,' ',.FALSE.,XS,XE,
     :             IXST,IXEN,STATUS)
              CALL IRFLAT_FOLD(DYNAMIC_MEM(OPTR),
     :            DYNAMIC_MEM(QPTR),NX,
     :            IXST,IXEN,DYNAMIC_MEM(DPTR),DYNAMIC_MEM(SPTR),
     :            DYNAMIC_MEM(UPTR),NS,ND,DYNAMIC_MEM(SUMPTR),
     :            DYNAMIC_MEM(NPTR))
          ENDIF
      ENDDO
C
C     Make the flat field spectrum
C
      CALL IRFLAT_WORK(ND,DYNAMIC_MEM(SUMPTR),DYNAMIC_MEM(NPTR),NX,
     :    DYNAMIC_MEM(DPTR),DYNAMIC_MEM(OPTR))
C
C     Tidy up
C
  500 CONTINUE
C
C     Closedown everything
C
C     IF (CGS2) THEN
C        CALL DTA_FRVAR(PNAME,IGNORE)
C        CALL DTA_FRVAR(PONAME,STATUS)
C     ENDIF
      CALL DSA_CLOSE(STATUS)
C
      END

      SUBROUTINE IRFLAT_FOLD(DATA,QUALITY,NX,IXST,IXEN,DETS,SC,USES,
     :     NS,ND,SUM,NUM)
C
C     Fold data for all scan positions contained entirely within the
C     range IXST to IXEN into the array SUM(ND) where ND is the number
C     of detectors
C
C    (>) DATA      (Real array DATA(NX)) The input data array
C    (>) QUALITY   (Byte array QUALITY(NX)) The input quality array
C    (>) NX        (Integer) The number of data points
C    (>) IXST      (Integer) The first data value to use
C    (>) IXEN      (Integer) The last data value to use
C    (>) DETS      (Integer array DETS(NX)) The array of detector numbers
C    (>) SC        (Integer array SC(NX)) The array of scan position numbers
C    (>) USES      (Logical array USES(NS)) Workspace array of positions to use
C    (>) NS        (Integer) Number of scan positions
C    (>) ND        (Integer) Number of detector positions
C    (<) SUM       (Real array SUM(ND)) Array of folded data
C    (<) NUM       (Integer array NUM(ND)) Number of points in each element of
C                            SUM.
C
      IMPLICIT NONE
      INTEGER NX,IXST,IXEN,NS,ND
      REAL DATA(NX),SUM(ND)
      INTEGER DETS(NX),NUM(ND),SC(NX)
      LOGICAL USES(NS)
      BYTE QUALITY(NX)
C
      INTEGER IS,IX,ID
C
C     Set USES array to TRUE
C
      DO IS=1,NS
          USES(IS) = .TRUE.
      ENDDO
C
C     Set USES array to FALSE for all positions partially outside range
C
      DO IX=1,IXST
          USES(SC(IX))=.FALSE.
      ENDDO
      DO IX=IXEN,NX
          USES(SC(IX))=.FALSE.
      ENDDO
C
C     Fold remaining data
C
      DO IX=IXST,IXEN
          IF (USES(SC(IX)) .AND. (QUALITY(IX) .EQ. 0)) THEN
              ID = DETS(IX)
              SUM(ID) = SUM(ID)+DATA(IX)
              NUM(ID) = NUM(ID)+1
          ENDIF
      ENDDO
      END


      SUBROUTINE IRFLAT_WORK(ND,SUM,NUM,NX,DETS,DATA)
      IMPLICIT NONE
      INTEGER ND,NX
      REAL SUM(ND),DATA(NX)
      INTEGER NUM(ND),DETS(NX)

      INTEGER ID,IX,NS,IGNORE
      REAL SS

      CHARACTER*13 ICH_CI,ICH_CF

      SS=0.0
      NS=0
      DO ID=1,ND
          IF (NUM(ID) .EQ. 0) THEN
              SUM(ID)=0.0
          ELSE
              SUM(ID)=SUM(ID)/NUM(ID)
              SS=SS+SUM(ID)
              NS=NS+1
          ENDIF
      ENDDO
      SS=SS/NS
      DO ID=1,ND
          SUM(ID)=SUM(ID)/SS
          CALL PAR_WRUSER(ICH_CI(ID)//ICH_CF(SUM(ID)),IGNORE)
      ENDDO
      DO IX=1,NX
          DATA(IX)=SUM(DETS(IX))
      ENDDO
      END



      SUBROUTINE IRFLAT_FILL(NX,ND,NS,S,D)
      IMPLICIT NONE
      INTEGER NX,ND,NS,D(NX),S(NX)
      INTEGER IX,ID,IS

      IX=1
      DO IS=1,NS-1
         DO ID=1,ND
            D(IX)=ID
            S(IX)=IS
            IX=IX+1
         ENDDO
      ENDDO

      DO IS = IX, NX
         D(IS)=IS-IX+1
         S(IS)=NS
      ENDDO

      END
