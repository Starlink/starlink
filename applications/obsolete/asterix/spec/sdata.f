*+  SDATA - Sets up file of data object names for fitting of multiple datasets
	SUBROUTINE SDATA(STATUS)
*
*    Description :
*
*     Accepts a sequence of up to NDSMAX dataset names from the user and writes
*     references to them into a reference data object. This is then assigned to
*     global parameter FIT_DATA, so as to be picked up automatically by
*     subsequent applications. If an object entered is a SPECTRAL_SET then the
*     user can enter a range of detectors to be selected from the set.
*
*    Environment parameters :
*
*     REF_OBJ=UNIV(W)
*            dataset to contain references
*     INP1..NDSMAX=UNIV(R)
*            dataset name
*     DETNO=CHAR(R)
*            string specifying selected detector range
*    Method :
*
*     Uses the Starlink REFERENCE system.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman (BHVAD::TJP)
*    History :
*
*      1 Jun 87 : V0.6-1 Original (BHVAD::TJP)
*      7 Jun 89 : V1.0-1 ASTERIX88 version - handles spectral sets (TJP)
*     14 Nov 89 : V1.0-2 BDA_CLOSE on error (TJP)
*     18 May 90 : V1.2-1 Use SPEC_SETSEARCH (TJP)
*      1 Mar 93 : V1.7-0 Allows maximum number of files. SHOW mode added.
*                        Error handling corrected (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'FIT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      INTEGER                  CHR_LEN
*
*    Local constants :
*
      CHARACTER*(DAT__SZTYP)   FILE_TYPE
        PARAMETER              (FILE_TYPE = 'REF_FILE')
*
*    Local variables :
*
      CHARACTER*2            CHARRNO	! Dataset number as char string
      CHARACTER*(DAT__SZLOC) DCLOC	! Locator to input data
      CHARACTER*132          FILE,PATH  ! TRACE data
      CHARACTER*(DAT__SZLOC) ILOC	! Locator to input data
      CHARACTER*(DAT__SZLOC) LOC	! Locator to input data
      CHARACTER*5            PARNAME	! Name of input parameter
      CHARACTER*(DAT__SZLOC) REFLOC	! Locator to reference data object
      CHARACTER*(DAT__SZNAM) REFNAME	! Name of reference component
      CHARACTER*(DAT__SZNAM) SELNAME	! Name of selection component
      CHARACTER*2            STRING	! String containing integer
      CHARACTER*(DAT__SZTYP) TYPE       ! Input file type

      REAL      RANGES(2*NDETMAX)	! Range values

      INTEGER        DETSEL(NDSMAX)	! Detectors selected
      INTEGER        LEVS       	! TRACE data
      INTEGER        N			! Dataset index
      INTEGER        NCHAR		! No of non-blank characters
      INTEGER        NDIM		! Dimensionality of data array
      INTEGER        DIMS(DAT__MXDIM)   ! Array dimensions
      INTEGER        SETSIZE		! Size of spectral set
      INTEGER        LSTRING		! Length of non-blank string
      INTEGER        NCOMP              ! Number of components in ref file
      INTEGER        NRANGES		! No of ranges entered
      INTEGER        NSEL		! No of detectors selected
      INTEGER        I,J		! Loop indices

      LOGICAL        IPRIM              ! Input primitive?
      LOGICAL        OK			! Data OK?
      LOGICAL        SET		! Spectral set?
      LOGICAL        SHOW               ! Show mode?
*
*    Version :
*
      CHARACTER*30 VERSION
	PARAMETER		(VERSION='SDATA Version 1.8-0')
*-

*    Version
      CALL MSG_PRNT( VERSION )

*    Initialise ASTERIX
      CALL AST_INIT()

*    Show mode?
      CALL USI_GET0L( 'SHOW', SHOW, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 9000
      IF ( SHOW ) THEN

*      Access old reference object
	CALL USI_ASSOCI( 'REF_OBJ', 'READ', REFLOC, IPRIM, STATUS )
	IF(STATUS.NE.SAI__OK) GO TO 9000

*      Check this is a REF_FILE
        IF ( IPRIM ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'SDATA reference file expected, not'/
     :                  /' primitive input', STATUS )
        ELSE
          CALL DAT_TYPE( REFLOC, TYPE, STATUS )
          IF ( TYPE .NE. FILE_TYPE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Input is not a SDATA reference file',
     :                                                       STATUS )
          END IF
        END IF
	IF(STATUS.NE.SAI__OK) GO TO 9000

*      Loop over contents of reference file
	CALL DAT_NCOMP( REFLOC, NCOMP, STATUS )
        J = 0
	DO I = 1, NCOMP
	  CALL DAT_INDEX( REFLOC, I, LOC, STATUS )
	  CALL DAT_TYPE( LOC, TYPE, STATUS )
	  IF ( TYPE .EQ. 'REFERENCE_OBJ' ) THEN
            J = J + 1
	    CALL REF_GET( LOC, 'READ', DCLOC, STATUS )

*          Spectral set?
	    CALL SPEC_SETSEARCH( DCLOC, SET, STATUS )
	    IF ( SET ) THEN

*            Find which spectra are to be used
	      CALL DAT_NAME(LOC,REFNAME,STATUS)	! Name should be REFnnn
	      SELNAME='SEL'//REFNAME(4:CHR_LEN(REFNAME))
	      CALL CMP_GET1I( REFLOC, SELNAME, NDETMAX, DETSEL,
     :                                           NSEL, STATUS )
	      IF ( STATUS .NE. SAI__OK ) THEN
	        CALL ERR_ANNUL(STATUS)
	        NSEL = 0
	      END IF
	    END IF

*          Report the file
	    CALL CHR_ITOC(J,CHARRNO,NCHAR)
	    CALL UTIL_SHOW( DCLOC, 'Dataset '//CHARRNO(1:NCHAR), FILE,
     :                                            PATH, LEVS, STATUS )

*          And detectors if a set
            IF ( SET ) THEN
              IF ( NSEL .EQ. 0 ) THEN
                CALL MSG_PRNT( '    Detectors : All' )
              ELSE
                CALL STR_DIMTOC( NSEL, DETSEL, PATH )
                CALL MSG_PRNT( '    Detectors : '/
     :                     /PATH(:CHR_LEN(PATH)) )
              END IF
            END IF

	  END IF
	  CALL DAT_ANNUL(LOC,STATUS)

	END DO
	IF ( STATUS .NE. SAI__OK ) GOTO 9000

*    Create new file
      ELSE

*      Create reference object
	CALL USI_ASSOCO( 'REF_OBJ', FILE_TYPE, REFLOC, STATUS )
	IF(STATUS.NE.SAI__OK) GO TO 9000

* Enter references, terminated with null entry
	DO N=1,NDSMAX
	  CALL CHR_ITOC(N,CHARRNO,NCHAR)
	  PARNAME='INP'//CHARRNO
	  REFNAME='REF'//CHARRNO
	  CALL USI_DASSOC(PARNAME,'READ',ILOC,STATUS)
	  IF(STATUS.EQ.PAR__NULL) GO TO 1000
	  CALL REF_CRPUT(REFLOC,REFNAME,ILOC,.FALSE.,STATUS)
	  IF(STATUS.NE.SAI__OK) GO TO 9000

* Check for spectral set
	  CALL SPEC_SETSEARCH(ILOC,SET,STATUS)
	  IF(SET)THEN

* Spectral set - allow selection of spectra from set

*    Get set size
	    CALL BDA_CHKDATA(ILOC,OK,NDIM,DIMS,STATUS)
	    IF(STATUS.NE.SAI__OK) GO TO 9000
	    IF(.NOT.OK)THEN
	      STATUS=SAI__ERROR
	      CALL ERR_REP('BADDAT','Bad input dataset',STATUS)
	    ELSE IF(NDIM.NE.2)THEN
	      STATUS=SAI__ERROR
	      CALL ERR_REP('BADDIM','Spectral set has incorrect '//
     :        'dimensionality',STATUS)
	    ENDIF
	    IF(STATUS.NE.SAI__OK) GO TO 9000
	    SETSIZE=DIMS(2)		! Assumes 1st dimension is spectral

*    Inform user
	    CALL MSG_SETI('NSPEC',SETSIZE)
	    CALL MSG_PRNT('Spectral set containing ^NSPEC spectra')

*    Get required detector ranges
	    CALL CHR_ITOC(SETSIZE,STRING,LSTRING)
	    CALL USI_DEF0C('DETNO','1:'//STRING(1:LSTRING),STATUS)
	    CALL PRS_GETRANGES('DETNO',NDETMAX,1,1,SETSIZE,RANGES,NRANGES,
     :      STATUS)
	    IF(STATUS.NE.SAI__OK) GO TO 9000

*    Convert to an array of selected detector numbers
	    IF(NRANGES.GT.0)THEN
	      NSEL=0
	      DO I=1,NRANGES
	        DO J=NINT(RANGES(2*I-1)),NINT(RANGES(2*I))
	          IF(J.GT.SETSIZE)THEN
	            STATUS=SAI__ERROR
	            CALL ERR_REP('BADNO','Spectrum number out of bounds',
     :              STATUS)
	            GO TO 9000
	          ENDIF
	          NSEL=NSEL+1
	          DETSEL(NSEL)=J
	        ENDDO
	      ENDDO

*     Write selection array to reference file
	      SELNAME='SEL'//CHARRNO
	      CALL DAT_NEW1I(REFLOC,SELNAME,NSEL,STATUS)
	      CALL CMP_PUT1I(REFLOC,SELNAME,NSEL,DETSEL,STATUS)
	      IF(STATUS.NE.SAI__OK) GO TO 9000
	    ENDIF
	    CALL USI_CANCL('DETNO',STATUS)
	  ENDIF
D         print *,'nsel,detsel: ',nsel,detsel
	  CALL DAT_ANNUL(ILOC,STATUS)
C	  print *,'after dat_annul; status,iloc: ',status,iloc
	ENDDO
	IF(STATUS.NE.SAI__OK) GO TO 9000

      END IF

*    Annul null status
 1000 CALL ERR_ANNUL( STATUS )

* Exit
 9000 CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
