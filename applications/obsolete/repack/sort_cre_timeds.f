*+SORT_CRE_TIMEDS - Create output time series dataset
      SUBROUTINE SORT_CRE_TIMEDS (SRT, OLOC, DPTR, STATUS)
      IMPLICIT NONE

      INCLUDE 'SMAPDEF.INC'
      INCLUDE 'SORT_DEF.INC'
      include 'DAT_PAR'

* Input
      RECORD /SORT_DEF/		 SRT

*    Status
      INTEGER                 STATUS

* Output
      CHARACTER*(DAT__SZLOC)  OLOC    ! HDS locator to IMDS
      INTEGER                 DPTR    ! Ptr to mapped time series

* Local
      CHARACTER*132	      TMF
      real		      TBIN

* P McGale Apr 95
*-
*   Check status - return if bad
      IF (STATUS .NE. 0) RETURN

*   Create output dataset
      CALL PAR_GETLC ('OUTPUT Dataset', TMF, STATUS)
      CALL HDS_NEW (TMF, 'TIME_SERIES', 'TIME_SERIES', 0, 0,
     :							OLOC, STATUS)
      if (srt.filt .eq. 8) then
        CALL BDA_PUTTITLE (OLOC, 'ROSAT WFC Survey Time series - S1a',
     &                                                          STATUS)
      elseif (srt.filt .eq. 6) then
        CALL BDA_PUTTITLE (OLOC, 'ROSAT WFC Survey Time series - S2a',
     &                                                          STATUS)
      else
        CALL BDA_PUTTITLE (OLOC, 'ROSAT WFC Survey Time series', STATUS)
      endif
      CALL BDA_PUTLABEL (OLOC, 'Intensity', STATUS)
      CALL BDA_PUTUNITS (OLOC, 'count',    STATUS)
      CALL BDA_CREDATA  (OLOC, 1, SRT.NBINS, STATUS)

* Map output array
      CALL BDA_MAPDATA (OLOC, 'WRITE', DPTR, STATUS)

*   Create and fill axis data
      CALL BDA_CREAXES (OLOC, 1, STATUS)

*   Create & write  axis components
      TBIN = real( (SRT.EMJD - SRT.SMJD)*86400./SRT.NBINS )
      CALL BDA_PUTAXLABEL (OLOC, 1, 'RAW_TIMETAG', STATUS)
      CALL BDA_PUTAXUNITS (OLOC, 1, 'second', STATUS)
      CALL BDA_PUTAXVAL   (OLOC, 1, TBIN/2., TBIN, SRT.NBINS, STATUS)

*      Now create the ASTERIX (HDS) structure
      CALL SORT_CRE_ASTERIX (OLOC, SRT, STATUS)

      IF (STATUS .NE. 0) THEN
	WRITE(*,*) '   Error in SORT_CRE_TIMEDS'
      ENDIF

      END
