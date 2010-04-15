*+RE_TIMEXP - Exposure correct ROSAT WFC data.
      PROGRAM RE_TIMEXP
      IMPLICIT NONE

* Input/Output
      INTEGER		STATUS

* M. Denby Sep 88
* Mods for S3 - P .McGale Sept 92.
* Mods for UNIX - P McGale Apr 95.
*-
*    Local variables :
      INCLUDE 		'CONSTANTS.INC'
      INCLUDE 		'EXPOS_DEF.INC'
      INCLUDE		'ASR_EXPATT.INC'
      include 		'HKR_BUFFER.INC'
      INCLUDE 		'CIN_ALIGN_LOW.INC'
      INCLUDE		'CIN_ALIGN.INC'
      include           'DAT_PAR'

      character*30	version
	parameter (version ='RE_TIMEXP Version 090595')

      CHARACTER*(DAT__SZLOC) 	ILOC 	   ! Locator to input datafile
      CHARACTER*(DAT__SZLOC) 	OLOC       ! Locator to output datafile
      CHARACTER*(DAT__SZLOC) 	DLOC  	   ! Locator to output data
      CHARACTER*(DAT__SZLOC) 	QLOC       ! Locator to output QUALITY
      CHARACTER*32 	TYPE
      CHARACTER*132 	INF, OUF
      CHARACTER*255	TROOT
      character*80	c_dum
      character*80      re_res		   ! Path to reserv. files
*
      INTEGER		NVAL       ! Number of mapped data values.
      INTEGER           NDIM       ! Dimensionality of the data
      INTEGER           APTR       ! Ptr to effective area*time prod
      INTEGER		CPTR	   ! Pointer to exposure loop counter
      INTEGER           DPTR       ! Pointer to output data array.
      INTEGER           QPTR       ! Pointer to output QUALITY array.
      INTEGER           VPTR       ! Pointer to output VARIANCE array.
      INTEGER		I	   ! Loop counters
      INTEGER		IDIM
      INTEGER		NSTEP
      INTEGER		MX_LASTPC
      integer 		fc, lc
      integer 		blksz
      integer 		hdutyp

      LOGICAL		EXIST
      LOGICAL           OK         ! Data is OK.
      LOGICAL     	SRC	   ! Analysing source or background.
      logical		anyf

      REAL		BINSIZ
      REAL		TOLER
      PARAMETER (TOLER = 1.E-5)

      REAL		STINW(3)

      RECORD /EXPOS_DEF/     EXP   ! Time windows, etc ...
      record /hkr_buffer/    hkr
      RECORD /ATTSTRUC/      ASPECT

* Initialise the HDS and PAR systems
      write(*,*)
      WRITE(*,*) version
      write(*,*)

      CALL HDS_START (STATUS)
      CALL PAR_cmdl (' ', STATUS)

*   Initialize Asterix routines
      CALL AST_INIT()

*   Open the input & output files
      CALL PAR_GETLC ('INP Uncorrected dataset', INF, STATUS)
      CALL HDS_OPEN (INF, 'READ', ILOC, STATUS)
      CALL DAT_TYPE (ILOC, TYPE, STATUS)

      call par_put0l('SRC', .TRUE., status)
      call par_get0l('SRC Apply psf correction? ',src,status)

      call mx_root(inf, ouf)
      call chr_fandl(ouf, fc, lc)
      ouf = ouf(fc:lc)//'c'
      call par_put0c('OUT', ouf, status)
      CALL PAR_GETlC ('OUT Corrected dataset', OUF, STATUS)
      CALL HDS_NEW (OUF, TYPE, TYPE, 0, 0, OLOC, STATUS)

      CALL HDX_COPY (ILOC, OLOC, STATUS)
      IF (STATUS.NE.0) STOP '   Unable to create o/p file'

*   Check that the data is OK
      CALL BDA_CHKDATA (ILOC, OK, NDIM, IDIM, STATUS)
      IF (NDIM.NE.1) STOP '   Dataset must be one dimensional'

*   Map output QUALITY array
      CALL BDA_CREQUAL (OLOC, NDIM, IDIM, STATUS)
      CALL BDA_LOCQUAL (OLOC, QLOC, STATUS)
      CALL DAT_MAPV (QLOC, '_INTEGER', 'WRITE', QPTR, NVAL, STATUS)

*   (Create) Map output VARIANCE array.
      CALL DAT_THERE (OLOC, 'VARIANCE', EXIST, STATUS)
      IF (EXIST) THEN
        CALL BDA_MAPVAR (OLOC, 'WRITE', VPTR, STATUS)
      ELSE
	CALL BDA_CREVAR (OLOC, NDIM, IDIM, STATUS)
        CALL BDA_MAPVAR (OLOC, 'WRITE', VPTR, STATUS)
      ENDIF

*   Map output DATA_ARRAY array
      CALL BDA_LOCDATA (OLOC, DLOC, STATUS)
      CALL DAT_MAPV (DLOC, '_REAL', 'UPDATE', DPTR, NVAL, STATUS)

*   Obtain info from the EXPOSURE structure of the input
      CALL EXPOS_GETDEF (ILOC, EXP, STATUS)

      BINSIZ = EXP.DUR/IDIM
      EXP.TDELT = MIN(2.,BINSIZ)
      NSTEP = INT((BINSIZ-0.001)/EXP.TDELT)+1
      EXP.TDELT = BINSIZ/NSTEP
      WRITE(*,*) '   Tstep set to ',EXP.TDELT,' secs ',NSTEP,
     :		 ' steps/bin'

      CALL DYN_MAPR (1, IDIM, APTR, STATUS)
      CALL DYN_MAPI (1, IDIM, CPTR, STATUS)

* Read the WFC to ST Eulers (ROSAT frame !) from CAL_WFC_MASTER
      CAL.MJD = DBLE(EXP.BMJD) + (EXP.BUTC/86400.D0)
      CALL CAL_GET_ALIGN ('WFC_TO_ST', CAL, STATUS)
      IF (STATUS.NE.0) STOP '   Error reading Eulers from MCF'

      DO I = 1, 3
	STINW(I) = CAL.WFC_TO_ST.EULER(I)
      ENDDO

*   Open the HK rates file (used in Dead time calculation)
      call getenv("RECAL", re_res)
      if (re_res .eq. ' ') then
        write(*,*)
        write(*,*)'   RE_TIMEXP'
	write(*,*)'   Can''t get RECAL environment variable.'
        write(*,*)
      endif
      call chr_fandl(re_res, fc, lc)
      exp.eve = re_res(fc:lc)//'/re_hkrres.fit'
      call ftgiou(hkr.ihkr, status)
      call ftopen(hkr.ihkr, exp.eve, 0, blksz, status)
      IF (STATUS.NE.0) THEN
	STOP '   Error opening Rates file'
      ELSE
        call chr_fandl(exp.eve, fc, lc)
	WRITE(*,*) '   Using ',exp.eve(fc:lc)
	hkr.c_rec = 0
      ENDIF

*   Open the aspect file using same root
      call chr_fandl(re_res, fc, lc)
      exp.eve = re_res(fc:lc)//'/re_ateres.fit'
      call ftgiou(aspect.iatt, status)
      call ftopen(aspect.iatt, exp.eve, 0, blksz, status)
      IF (STATUS.NE.0) THEN
	STOP '   Error opening Aspect file'
      ELSE
        call chr_fandl(exp.eve, fc, lc)
	WRITE(*,*) '   Using ',exp.eve(fc:lc)
	aspect.c_rec = 0
      ENDIF


*   Calculate exposure corrections array
      WRITE(*,*) '   Calculating the exposure array'
      CALL TIMEXP(SRC, hkr, aspect, STINW, EXP, %VAL(APTR),
     &                              %VAL(CPTR), IDIM, STATUS)

*   Apply exposure corrections to the data
      WRITE(*,*) '   Correcting raw data array'
      CALL TIMCOR(EXP,BINSIZ,%VAL(APTR),%VAL(CPTR),%VAL(DPTR),
     :				%VAL(QPTR),%VAL(VPTR),IDIM,STATUS)

*   Delete unwanted structures in OLOC, set flags etc
      CALL BDA_PUTUNITS (OLOC, 'count/s', STATUS)
      CALL EXPOS_S_SET  (OLOC, STATUS)

*   Tidy up
      CALL DAT_UNMAP (QLOC, STATUS)
      CALL DAT_UNMAP (DLOC, STATUS)

      CALL AST_CLOSE
      call ftclos(hkr.ihkr, status)
      call ftclos(aspect.iatt, status)
      call ftfiou(-1, status)

      END
