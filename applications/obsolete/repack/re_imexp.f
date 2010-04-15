*+RE_IMEXP - Exposure correct ROSAT WFC survey image.
      PROGRAM RE_IMEXP
      IMPLICIT NONE

* M. Denby Oct 91
* P. McGale Oct 94 - UNIX mods and correctly added QUAL and VAR arrays.
* P. McGale May 95 - Use FITS format input files.
* P  McGale May 95 - Option to use RGW det efficiency corrections.
*-
*    Local variables :
      INCLUDE 		'CONSTANTS.INC'
      INCLUDE		'EXPOS_DEF.INC'
      INCLUDE           'DAT_PAR'

      integer		xdim1, xdim2
	parameter (xdim1=360, xdim2=181)

      CHARACTER*(DAT__SZLOC) 	ILOC 	   ! Locator to input datafile
      CHARACTER*(DAT__SZLOC) 	OLOC       ! Locator to output datafile
      CHARACTER*(DAT__SZLOC) 	DILOC  	   ! Locator to output data
      CHARACTER*(DAT__SZLOC) 	DOLOC  	   ! Locator to output data
      CHARACTER*(DAT__SZLOC) 	QLOC       ! Locator to output QUALITY
      CHARACTER*(DAT__SZLOC) 	XLOC       ! Locator to Exposure image
      CHARACTER*32 	TYPE
      CHARACTER*132 	INF, OUF
      CHARACTER*132 	ximag
      character*80	re_res

      INTEGER           FC, LC
      integer		lunit
      integer		blksz
      integer		hdutyp
      INTEGER		NVAL       ! Number of mapped data values.
      INTEGER           NDIM       ! Dimensionality of the data
      INTEGER           DIPTR      ! Pointer to input data array.
      INTEGER           DOPTR      ! Pointer to input data array.
      INTEGER           QPTR       ! Pointer to output QUALITY array.
      INTEGER           VPTR       ! Pointer to output VARIANCE array.
      INTEGER		IPTR	   ! Pointer to exposure image
      INTEGER		I	   ! Loop counters
      INTEGER		IDIM(2)	   ! Dimensions of raw image array
      INTEGER		STATUS
      integer		filt

      real		expmap(xdim1,xdim2)	! Sky exposure image

      logical		anyf
      LOGICAL		EXIST
      LOGICAL           OK         ! Data is OK.
      logical		effcor	   ! Correct for detector efficiency

      RECORD 		/EXPOS_DEF/EXP

      WRITE(*,*) '   RE_IMEXP Version 310595'

      CALL HDS_START (STATUS)
      CALL PAR_CMDL (' ', STATUS)

*   Initialize Asterix routines
      CALL AST_INIT()

*   Open the input & output files
      CALL PAR_GETLC ('INP Uncorrected image', INF, STATUS)
      CALL HDS_OPEN (INF, 'READ', ILOC, STATUS)
      CALL DAT_TYPE (ILOC, TYPE, STATUS)

      call par_put0l('EFFCOR', .TRUE., status)
      call par_get0l('EFFCOR correct for detector degredation',
     &                                               effcor, status)

      call mx_root(inf, ouf)
      call chr_fandl(ouf, fc, lc)
      ouf = ouf(fc:lc)//'c'
      call par_put0c('OUT', ouf, status)
      CALL PAR_GETLC ('OUT Corrected image', OUF, STATUS)
      CALL HDS_NEW (OUF, TYPE, TYPE, 0, 0, OLOC, STATUS)

      WRITE(*,*) '   Copying i/p image to o/p'
      CALL HDX_COPY (ILOC, OLOC, STATUS)
      IF (STATUS.NE.0) STOP '   Error creating o/p dataset'

*   Check that the data is OK
      CALL BDA_CHKDATA (ILOC, OK, NDIM, IDIM, STATUS)
      IF (NDIM.NE.2) STOP '   I/p dataset must be 2 dimensional'

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

* Rewrite output DATA_ARRAY if it's not type REAL....

* ...First map the input array as REAL, then get the type
      CALL BDA_LOCDATA (ILOC, DILOC, STATUS)
      CALL DAT_MAPV (DILOC, '_REAL', 'READ', DIPTR, NVAL, STATUS)
      CALL BDA_LOCDATA (OLOC, DOLOC, STATUS)
      CALL DAT_TYPE (DOLOC, TYPE, STATUS)

* If the type is not REAL reformat it
      IF (TYPE(1:5) .NE. '_REAL') THEN
	CALL CMP_MOD (OLOC,'DATA_ARRAY','_REAL',NDIM,IDIM,STATUS)
      ENDIF

* Copy input DATA_ARRAY to output then remap it
      CALL CMP_PUTVR (OLOC,'DATA_ARRAY',NVAL,%VAL(DIPTR),STATUS)
      CALL CMP_MAPN (OLOC, 'DATA_ARRAY', '_REAL', 'UPDATE',
     :					 NDIM, DOPTR, IDIM, STATUS)

* Get info from the image header
      CALL EXPOS_GETDEF (OLOC, EXP, STATUS)

* Open and read the appropriate exposure image
      call getenv("RECAL", re_res)
      if (re_res .eq. ' ') then
	write(*,*)
	write(*,*)'   RE_IMEXP'
	write(*,*)'   Can''t get RECAL environment variable'
      endif
      call chr_fandl(re_res, fc, lc)
      IF (EXP.FILT .EQ. 8) THEN
        ximag = re_res(fc:lc)//'/reskyexp_f1.fit'
	filt = 1
      ELSEIF (EXP.FILT .EQ. 6) THEN
        ximag = re_res(fc:lc)//'/reskyexp_f2.fit'
	filt = 2
      ENDIF
      call ftgiou(lunit, status)
      call ftopen(lunit, ximag, 0, blksz, status)
      call ftg2de(lunit, 1, -1, xdim1, xdim1, xdim2,
     &                                   expmap, anyf, status)
      IF (STATUS .NE. 0) THEN
	STOP '   Error in RE_IMEXP - Opening the exposure image'
      ENDIF


      WRITE(*,*) '   Correcting the raw image'
      CALL IMEXP(EXP,effcor,filt,expmap,%VAL(DOPTR),%VAL(VPTR),
     &                     %VAL(QPTR),IDIM(1),IDIM(2),STATUS)

*   Delete unwanted structures in OLOC, set flags etc
      CALL BDA_PUTUNITS (OLOC, 'count/s', STATUS)
      CALL EXPOS_S_SET  (OLOC, STATUS)

      call ftclos(lunit, status)
      CALL AST_CLOSE

      END

