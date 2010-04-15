*+DBM_S_DMPHED Dump the header records from a small map file
	SUBROUTINE DBM_S_DMPHED (HEAD, STATUS)
	IMPLICIT NONE
	INCLUDE		'SMAPDEF.INC'
	INCLUDE		'SINDEX.INC'
* Input
	RECORD		/HEADEF/ HEAD
* Output
	INTEGER		STATUS			! Status return
* M. Denby Mar-88
* P McGale Apr 95 - UNIX mods
*-
* local
	CHARACTER*3	CAL_FILT_N2S
	CHARACTER*2	ZOOM(0:1), WIND(0:1), INV(0:1)
	CHARACTER*13	STIM
	DATA 		ZOOM, WIND, INV/'  ',' z','  ',' w','  ',' i'/
	INTEGER		NS			! Loop counter
	INTEGER		CW			! Temp config word
	INTEGER		NXREC			! Map file rec pointer
	REAL*8		DELT

	IF (STATUS .NE. 0) RETURN

* Dump key parameters to user
	WRITE(ILIS,10) HEAD.MODE, HEAD.MDR_SEQ,
     :		    HEAD.NLON, HEAD.NLAT,
     :		    HEAD.NOM_RA, HEAD.NOM_DEC,
     :		    HEAD.TARGET, HEAD.OBSERVER,
     :		    HEAD.INSTRUMENT, HEAD.REF_MJD,
     :		    HEAD.BASE_DATE, HEAD.BASE_MJD,
     :		    HEAD.END_MJD, HEAD.NEVENT,
     :		    HEAD.NMAP, HEAD.CRE_DATE,
     :		    HEAD.REVISION

10	FORMAT (//,' Mode                    : ',A1,/,
     :		   ' MDR Sequence #          : ',A10,/,
     :		   ' # Azim maps             : ',I3,/,
     :		   ' # Elev maps             : ',I2,/,
     :		   ' Asc Node Ra             : ',F6.1,/,
     :		   ' Asc Node Dec            : ',F6.1,/,
     :		   ' Target                  : ',A40,/,
     :		   ' Observer                : ',A40,/,
     :		   ' Instrument              : ',A40,/,
     :		   ' Ref MJD                 : ',F10.4,/,
     :		   ' Base date               : ',A11,/,
     :		   ' Base MJD                : ',F10.4,/,
     :		   ' End MJD                 : ',F10.4,/,
     :		   ' Total Events            : ',I8,/,
     :		   ' Active maps             : ',I5,/,
     :             ' File Creation           : ',A9,/,
     :		   ' File Revision           : ',A4,//)



999	IF (STATUS .NE. 0) THEN
	  WRITE(*,*) '   Error in DBM_S_DMPHED'
	ENDIF

	END
