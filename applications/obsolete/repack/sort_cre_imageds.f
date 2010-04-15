*+SORT_CRE_IMAGEDS - Create output image dataset
	SUBROUTINE SORT_CRE_IMAGEDS (SRT, OLOC, DPTR, STATUS)
	IMPLICIT NONE

	INCLUDE 'SMAPDEF.INC'
	INCLUDE 'SORT_DEF.INC'
        include 'DAT_PAR'

* Input
	RECORD /SORT_DEF/		 SRT

* Output
	INTEGER		     DPTR    ! Pointer to mapped image array
	INTEGER		     STATUS
* M. Denby
* P McGale Apr 95
*-
* local
	CHARACTER*(DAT__SZLOC) OLOC    ! HDS locator to IMDS
	CHARACTER*(DAT__SZLOC) TLOC    ! HDS locator to TEMP
	CHARACTER*132	     IMF
	INTEGER		     IDIMS(2)  ! Image dimensions

*   Check status - return if bad
	IF (STATUS .NE. 0) RETURN

*   Create output dataset
	CALL PAR_GETLC ('OUTPUT dataset', IMF, STATUS)
	CALL HDS_NEW (IMF, 'IMAGE', 'IMAGE', 0, 0, OLOC, STATUS)

	IDIMS(1) = SRT.NXPIX
	IDIMS(2) = SRT.NYPIX

        if (srt.filt .eq. 8) then
	  CALL BDA_PUTTITLE (OLOC, 'ROSAT WFC Survey Image - S1a', STATUS)
        elseif (srt.filt .eq. 6) then
	  CALL BDA_PUTTITLE (OLOC, 'ROSAT WFC Survey Image - S2a', STATUS)
	else
	  CALL BDA_PUTTITLE (OLOC, 'ROSAT WFC Survey Image', STATUS)
	endif
	CALL BDA_PUTLABEL (OLOC, 'Intensity', STATUS)
	CALL BDA_PUTUNITS (OLOC, 'count',     STATUS)

* Map temp output array
	CALL DAT_TEMP ('_REAL', 2, IDIMS, TLOC, STATUS)
	CALL DAT_MAPN (TLOC, '_REAL', 'WRITE', 2, DPTR, IDIMS, STATUS)

*   Create and fill axis data
	CALL BDA_CREAXES (OLOC, 2, STATUS)

*   Create & write x axis components
	CALL BDA_PUTAXLABEL (OLOC, 1, 'X_CORR', STATUS)
	CALL BDA_PUTAXUNITS (OLOC, 1, 'arcmin', STATUS)
	CALL BDA_PUTAXVAL   (OLOC, 1, (SRT.XCMIN/DTOR)*60.,
     :			(SRT.XPIXEL/DTOR)*60., SRT.NXPIX, STATUS)

*   Create & write y axis components
	CALL BDA_PUTAXLABEL (OLOC, 2, 'Y_CORR', STATUS)
	CALL BDA_PUTAXUNITS (OLOC, 2, 'arcmin', STATUS)
	CALL BDA_PUTAXVAL   (OLOC, 2, (SRT.YCMIN/DTOR)*60.,
     :			(SRT.YPIXEL/DTOR)*60., SRT.NYPIX, STATUS)

*	Now create the ASTERIX (HDS) structure
	CALL SORT_CRE_ASTERIX (OLOC, SRT, STATUS)

	IF (STATUS .NE. 0) THEN
	WRITE(*,*) '   Error in SORT_CRE_IMAGEDS'
	ENDIF

	END
