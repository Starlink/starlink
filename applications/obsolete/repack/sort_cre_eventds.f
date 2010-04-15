*+SORT_CRE_EVENTDS - Create & map the output event dataset
      SUBROUTINE SORT_CRE_EVENTDS(SRT, OLOC, MLIM, LLOC, DLOC,
     &                                               DPTR, STATUS)
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SMAPDEF.INC'
      INCLUDE 'SORT_DEF.INC'
      include 'DAT_PAR'

* Input:
      INTEGER	MLIM		! Current mapping extent of lists
      RECORD /SORT_DEF/		 SRT

* Output:
      CHARACTER*(DAT__SZLOC) OLOC	! Locator to Event dataset
      CHARACTER*(DAT__SZLOC) LLOC(5)	! Locators to list objects
      CHARACTER*(DAT__SZLOC) DLOC(5)	! Locators to list DATA_ARRAY
      INTEGER	   	     DPTR(5)	! Pointers to mapped DATA_ARRAYS
      INTEGER	   	     STATUS
* M. Denby
* P McGale May 95 UNIX mods
*-

*    Local variables :
      CHARACTER*132 	      EVF
      INTEGER	    	      I		! Loop counter
      DOUBLE PRECISION	      CEL(2)
      DOUBLE PRECISION	      CTOL(3,3), LTOC(3,3)
      DOUBLE PRECISION	      AZP, ELP

*   Check status
      IF (STATUS .NE. 0) RETURN

*   Create output dataset
      CALL PAR_GETLC ('OUTPUT Dataset', EVF, STATUS)
      CALL HDS_NEW (EVF, 'EVENT_DATA', 'EVENT_DATA', 0, 0, OLOC, STATUS)

*   Check status - drop out if bad
      IF (STATUS .NE. 0) GOTO 999

*   Create the list structures to hold the event data
      CALL LIST_NEW (OLOC, 'X_RAW', '_REAL', MLIM, .TRUE., LLOC(1),
     :								STATUS)
      CALL LIST_NEW (OLOC, 'Y_RAW', '_REAL', MLIM, .TRUE., LLOC(2),
     :								STATUS)
      CALL LIST_NEW (OLOC, 'X_CORR','_REAL', MLIM, .TRUE., LLOC(3),
     :								STATUS)
      CALL LIST_NEW (OLOC, 'Y_CORR','_REAL', MLIM, .TRUE., LLOC(4),
     :								STATUS)
      CALL LIST_NEW (OLOC, 'RAW_TIMETAG', '_REAL', MLIM, .TRUE.,
     :						       LLOC(5), STATUS)

*   Tell ASTERIX which way round to make images
      CALL DAT_NEW (LLOC(1), 'DECREASING', '_LOGICAL', 0, 0, STATUS)
      CALL DAT_NEW (LLOC(3), 'DECREASING', '_LOGICAL', 0, 0, STATUS)
      CALL CMP_PUT0L(LLOC(1), 'DECREASING', .TRUE., STATUS)
      CALL CMP_PUT0L(LLOC(3), 'DECREASING', .TRUE., STATUS)

*   Write UNITS, FIELD_MIN & FIELD_MAX to the lists.
      CALL CMP_PUT0C(LLOC(1), 'UNITS',  'arcmin',    STATUS)
      CALL CMP_PUT0C(LLOC(2), 'UNITS',  'arcmin',    STATUS)
      CALL CMP_PUT0C(LLOC(3), 'UNITS',  'arcmin',    STATUS)
      CALL CMP_PUT0C(LLOC(4), 'UNITS',  'arcmin',    STATUS)
      CALL CMP_PUT0C(LLOC(5), 'UNITS',  'second',    STATUS)

*   Write FIELD_MAX
      CEL(1) = DBLE(SRT.ARA)
      CEL(2) = DBLE(SRT.ADEC)
      CALL AX_DMAT (CEL, DBLE(SRT.ROLL), CTOL, LTOC)
      CALL AX_DTRANS (DBLE(SRT.FRA), DBLE(SRT.FDEC), CTOL, .TRUE.,
     :							    AZP, ELP)
      IF (AZP .GT. PI) THEN
	AZP = AZP - TWOPI
      ENDIF

      CALL CMP_PUT0R(LLOC(1), 'FIELD_MAX', 2.5*60.,           STATUS)
      CALL CMP_PUT0R(LLOC(2), 'FIELD_MAX', 2.5*60.,           STATUS)
      CALL CMP_PUT0R(LLOC(3), 'FIELD_MAX',
     :	((SRT.DAZ + REAL(AZP))/DTOR) * 60.,  STATUS)
      CALL CMP_PUT0R(LLOC(4), 'FIELD_MAX',
     :	((SRT.DEL + REAL(ELP))/DTOR) * 60.,  STATUS)
      CALL CMP_PUT0R(LLOC(5), 'FIELD_MAX',
     :	REAL((SRT.EMJD - SRT.SMJD)*86400.D0), STATUS)

*   Write FIELD_MIN
      CALL CMP_PUT0R(LLOC(1), 'FIELD_MIN', -2.5*60.,  STATUS)
      CALL CMP_PUT0R(LLOC(2), 'FIELD_MIN', -2.5*60.,  STATUS)
      CALL CMP_PUT0R(LLOC(3), 'FIELD_MIN',
     :	((REAL(AZP) - SRT.DAZ)/DTOR) * 60.,  STATUS)
      CALL CMP_PUT0R(LLOC(4), 'FIELD_MIN',
     :	((REAL(ELP) - SRT.DEL)/DTOR) * 60.,  STATUS)
      CALL CMP_PUT0R(LLOC(5), 'FIELD_MIN', 0.0,STATUS)

*   Check status - drop out if bad
      IF (STATUS .NE. 0) GOTO 999

*   Write the title and units
      if (srt.filt .eq. 8) then
        CALL BDA_PUTTITLE (OLOC, 'ROSAT WFC Survey Events - S1a',STATUS)
      elseif (srt.filt .eq. 6) then
        CALL BDA_PUTTITLE (OLOC, 'ROSAT WFC Survey Events - S2a',STATUS)
      else
        CALL BDA_PUTTITLE (OLOC, 'ROSAT WFC Survey Events', STATUS)
      endif
      CALL BDA_PUTLABEL (OLOC, 'Event lists',    STATUS)
      CALL BDA_PUTUNITS (OLOC, 'count',         STATUS)

*   Check status - drop out if bad
      IF (STATUS .NE. 0) GOTO 999

*   Create ASTERIX structure.
      CALL SORT_CRE_ASTERIX(OLOC, SRT, STATUS)

*   Check status - drop out if bad
      IF (STATUS .NE. 0) GOTO 999

*   Map the data array components
      DO I = 1, 5
        CALL DAT_FIND(LLOC(I), 'DATA_ARRAY', DLOC(I), STATUS)
        CALL DAT_MAPV(DLOC(I), '_REAL', 'WRITE', DPTR(I), MLIM, STATUS)
      END DO

999   IF (STATUS .NE. 0) THEN
	WRITE(*,*) '   Error in SORT_CRE_EVENTDS'
      ENDIF

      END
