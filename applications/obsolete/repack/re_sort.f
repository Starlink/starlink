*+RE_SORT - Sorts survey small maps into data sets of specified field
      PROGRAM RE_SORT
      IMPLICIT NONE

* Include files:
      INCLUDE 'SMAPDEF.INC'		! Small map linked list deftn.
      INCLUDE 'SLIST.INC'
      INCLUDE 'SORT_DEF.INC'
      include 'DAT_PAR'

* Output:
      INTEGER		STATUS
* P McGale Apr 95.
*-

*    Local Constants :
      INTEGER		MINC
	 PARAMETER	(MINC = 10000)
      character*25 	version
         parameter      (version='RE_SORT Version 110595')

*    Local variables :
      CHARACTER*80      EVEFIL          ! Input small map file
      CHARACTER*(DAT__SZLOC) LLOC(5)	! Locators to EVDS lists
      CHARACTER*(DAT__SZLOC) DLOC(5)	! Locators to EVDS DATA_ARRAYs
      CHARACTER*(DAT__SZLOC) LOC, LOCC
      CHARACTER*6	OUTYPE

      INTEGER           APTR          	! Pointer to mapped image array
      INTEGER           SMF             ! Unit for small map file
      INTEGER		NXREC		! Small map record #
      INTEGER		MLIM		! mapping extent of EVDS lists
      INTEGER		DPTR(5)		! Ptr EVDS mapped DATA_ARRAYs
      INTEGER		IDIMS(2)
      INTEGER		IMAX, NEWPTR
      integer		fc, lc

      LOGICAL		OPLOG		! Map file opened flag
      LOGICAL		SHOW		! Display the header
      LOGICAL		INTOK

      REAL		RANGE, FRMAX

      DOUBLE PRECISION  MTOL(3,3)       ! Dcm map locals to image locals

      RECORD /EBLOCK/ EBUF
      RECORD /SORT_DEF/ SRT

* Initialise the HDS and PAR systems
      write(*,*)
      WRITE(*,*) version
      write(*,*)
      STATUS = 0
      CALL HDS_START (STATUS)
      CALL par_cmdl (' ', STATUS)
      CALL AST_INIT()


*   Open the sky map file and read the header
      CALL DBM_OPMAP ('EVEFIL Event File', EVEFIL, SMF, 0,
     :							STATUS)
      IF (STATUS .NE. 0) GOTO 999
      call mx_root(evefil, ihead.mdr_seq)

      CALL GETSHED (SMF, IHEAD, STATUS)
      IF (STATUS .NE. 0) GOTO 999

*   Show the header to the user
      call par_put0l('SHOW', .FALSE., status)
      call par_get0l('SHOW header records', SHOW, status)
      IF (SHOW) THEN
        CALL DBM_S_DMPHED (IHEAD, STATUS)
        IF (STATUS .NE. 0) GOTO 999
      ENDIF

*   Get the sort control information from the user.
      CALL SORT_S_SETUP (SMF, SRT, STATUS)
      IF (STATUS .NE. 0) GOTO 999

*   Sorting to an image data set ?
      IF (SRT.DTYPE(1:1).EQ.'I' .OR. SRT.DTYPE(1:1).EQ.'L') THEN

*   Create the output image data set.
        CALL SORT_CRE_IMAGEDS (SRT, LOC, APTR, STATUS)
        call hist_add(loc, 'Created by '//version, status)
        IF (STATUS .NE. 0) GOTO 999

	IF (SRT.DTYPE(1:1).EQ.'L') THEN
          CALL SORT_S_D_DOIT (SMF, SRT, SRT.NXPIX, SRT.NYPIX,
     :						%VAL(APTR), STATUS)
	ELSE
          CALL SORT_S_I_DOIT (SMF, SRT, SRT.NXPIX, SRT.NYPIX,
     :						%VAL(APTR), STATUS)
	ENDIF
	CALL ARANGE (SRT.NXPIX*SRT.NYPIX, %VAL(APTR), RANGE, FRMAX)
	IF (ABS(RANGE) .LE. 8000000.0) THEN
	  IMAX = NINT(RANGE)
	  INTOK = FRMAX/MAX(RANGE,1.0) .LT. 1.0E-5
	ELSE
	  INTOK = .FALSE.
	ENDIF
	IF (INTOK .AND. IMAX .LT. 128) THEN
	  OUTYPE = '_BYTE'
	ELSEIF (INTOK .AND. IMAX .LT. 32768) THEN
	  OUTYPE = '_WORD'
	ELSE
	  OUTYPE = '_REAL'
	ENDIF

	IDIMS(1) = SRT.NXPIX
	IDIMS(2) = SRT.NYPIX
	CALL DAT_NEW (LOC, 'DATA_ARRAY', OUTYPE, 2, IDIMS, STATUS)
	CALL DAT_FIND (LOC, 'DATA_ARRAY', LOCC, STATUS)
	CALL DAT_MAP (LOCC, OUTYPE, 'WRITE', 2, IDIMS, NEWPTR, STATUS)
	IF (OUTYPE .EQ. '_BYTE') THEN
	  CALL REAL2BYTE (SRT.NXPIX*SRT.NYPIX, %VAL(APTR),
     :	    				        %VAL(NEWPTR))
	ELSEIF (OUTYPE .EQ. '_WORD') THEN
	  CALL REAL2I2 (SRT.NXPIX*SRT.NYPIX, %VAL(APTR),
     :	 					%VAL(NEWPTR))
	ELSE
	  CALL REAL2REAL (SRT.NXPIX*SRT.NYPIX, %VAL(APTR),
     :							%VAL(NEWPTR))
	ENDIF
        IF (STATUS .NE. 0) GOTO 999


*  Sorting to an Event data set
      ELSEIF (SRT.DTYPE(1:1) .EQ. 'E') THEN

*   Create & map the output Event data set.
        MLIM = MINC
        CALL SORT_CRE_EVENTDS(SRT, LOC, MLIM, LLOC, DLOC, DPTR, STATUS)
        call hist_add(loc, 'Created by '//version, status)
        IF (STATUS .NE. 0) GOTO 999

*   Do the rest
        CALL SORT_S_E_DOIT (SMF, SRT, LLOC, DLOC, DPTR, MLIM, STATUS)

        IF (STATUS .NE. 0) GOTO 999


*  Sorting to time series data set
      ELSEIF (SRT.DTYPE(1:1) .EQ. 'T') THEN

*   Create & map the output objects
        CALL SORT_CRE_TIMEDS (SRT, LOC, APTR, STATUS)
        call hist_add(loc, 'Created by '//version, status)
        IF (STATUS .NE. 0) GOTO 999

*   Do the rest
        CALL SORT_S_T_DOIT (SMF, SRT, SRT.NBINS, %VAL(APTR), STATUS)
        IF (STATUS .NE. 0) GOTO 999
      ENDIF

*   Tidy up
      if (status .ne.0) call printerror(status)
      call ftclos(smf, status)
      if (status .ne.0) call printerror(status)
      CALL ftfiou(smf, status)
      if (status .ne.0) call printerror(status)

999   IF (STATUS .NE. 0) THEN
	 WRITE(*,*) '   Error in RE_SORT'
      END IF

      CALL AST_CLOSE

      END

