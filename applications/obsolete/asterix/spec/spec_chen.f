*+  SPEC_CHEN - Finds energy centre and half-width for each spectral channel
	SUBROUTINE SPEC_CHEN(NCHAN,NRESP,ENIN,CHIN,RESP,EN,CHEN,CHWID,
     :                       STATUS)
*    Description :
*     Searches through instrument response matrix values to establish energy
*     values for channel boundaries. Then converts these to channel centre
*     energies and half-widths.
*     NOTE: Assumes that instrument response lists are stored with energy index
*     non-decreasing, and channel index increasing for each energy index value,
*     e.g.		ENERGY		CHANNEL		RESPONSE
*			  2		  4		  ...
*			  2		  5		  ...
*			  2		  6		  ...
*			  2		  7		  ...
*			  3		  5		  ...
*			  3		  6		  ...
*			 etc		 etc
*
*     NOTE: Not guaranteed proof against oddly configured response matrices -
*     should be tested for each new instrument.
*    History :
*
*     24 Aug 88 : Original (BHVAD::TJP)
*     25 Feb 93 : Error handling corrected (DJA)
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
	INCLUDE 'SAE_PAR'
	INCLUDE 'PAR_ERR'
*    Import :
	INTEGER NCHAN		! No of spectral channels to be scanned
	INTEGER NRESP		! No of response elements
	INTEGER ENIN(NRESP)	! Energy indices
	INTEGER CHIN(NRESP)	! Channel indices
	REAL RESP(NRESP)	! Response values
	REAL EN(*)		! Energy values (translation of indices)
*    Import-Export :
*    Export :
	REAL CHEN(NCHAN)	! Central energies for spectral channels
	REAL CHWID(NCHAN)	! Channel half-widths (in energy units)
*    Status :
	INTEGER STATUS
*    Local Constants :
*    Local variables :
	INTEGER I
	INTEGER IEFIRST		! Lowest energy index
	INTEGER IELAST		! Highest energy index
	INTEGER ICHFIRST	! Lowest channel index
	INTEGER ICHLAST		! Highest channel index
	INTEGER IEC		! Current energy index
	INTEGER IEL		! Previous energy index
	INTEGER ICC		! Current channel index
	INTEGER ICL		! Previous channel index
	INTEGER IR		! Current response matrix index
	INTEGER NB		! Current channel bound number
	INTEGER IRSTART		! Response index for last en. & chan. (approx)
				! (safe starting point for forward search)
	REAL A,B		! Variables used in interpolation
	REAL ETOP		! Energy of topmost channel bound
	REAL RELCL		! Response of last chan at last energy
	REAL RELCC		! Response of current chan at last energy
	REAL RECCL		! Response of last chan at current energy
	REAL RECCC		! Response of current chan at current energy
*-

* NOTE: To avoid additional dynamical storage requirements, array CHEN
*       is used to store the derived channel bound energies (apart from the
*       top one) during the calculation. Centre values are loaded into it
*       at the end.

* Get first and last indices (assumes standard ordering)
	IEFIRST=ENIN(1)
	IELAST=ENIN(NRESP)
	ICHFIRST=CHIN(1)
	ICHLAST=CHIN(NRESP)

* Check number of channels
	IF((ICHLAST-ICHFIRST+1).NE.NCHAN)THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP('BADNCHAN','NCHAN doesn''t match channel range in'
     :    //' response matrix',STATUS)
	  GO TO 9000
	ENDIF

* Initialise
	NB=1
	IR=1

* Lower bound of first channel
	DO WHILE ((CHIN(IR).NE.ICHFIRST).OR.(RESP(IR).LE.0.0))
	  IR=IR+1
	ENDDO
	RELCL=RESP(IR)
	IEL=ENIN(IR)
	ICL=ICHFIRST
	CHEN(NB)=EN(IEL)		! Energy of 1st non-zero response
D	print *,'bottom bound ',nb,chen(nb)

* Check that first channel is dominant at its lowest non-zero response value
	IF(ENIN(IR+1).EQ.ENIN(IR))THEN
	  IF(RESP(IR+1).GT.RESP(IR))THEN
	    STATUS=SAI__ERROR
	    CALL ERR_REP('BAD1','Response matrix doesn''t cover 1st channel',
     :      STATUS)
	    GO TO 9000
	  ELSE
	    RELCC=RESP(IR+1)
	    IRSTART=IR+1
	  ENDIF
	ELSE
	  RELCC=0.0
	  IRSTART=IR
	ENDIF

* Lower bounds of remaining channels
	IEC=IEL+1
	NB=NB+1
	DO ICC=ICHFIRST+1,ICHLAST

*    Step through response lists and find elements for current energy channel
 100	  DO WHILE (ENIN(IR).LT.IEC)
	    IR=IR+1
	  ENDDO

*    Find required channel values
	  DO WHILE (CHIN(IR).LT.ICL)
	    IR=IR+1
	  ENDDO

*    Evaluate response elements for current energy channel
	  IF(CHIN(IR).EQ.ICL)THEN
	    RECCL=RESP(IR)
	    IF(CHIN(IR+1).EQ.ICC)THEN
	      RECCC=RESP(IR+1)
	    ELSE
	      RECCC=0.0
	    ENDIF
	  ELSE
	    RECCL=0.0
	    IF(CHIN(IR).EQ.ICC)THEN
	      RECCC=RESP(IR)
	    ELSE
	      RECCC=0.0
	      IEL=IEC
	      IEC=IEC+1
	      GO TO 100
	    ENDIF
	  ENDIF

*    Search for point where current channel overtakes the previous one
	  IF(RECCC.LT.RECCL)THEN

*       Update previous response elements and loop back for next energy channel
	    IF(IEC.LT.IELAST)THEN
	      RELCL=RECCL
	      RELCC=RECCC
	      IEL=IEC
	      IEC=IEC+1
	      IRSTART=IR-1
	      GO TO 100
	    ELSE
	      STATUS=SAI__ERROR
	      CALL ERR_REP('BADTOP','Response matrix doesn''t cover'//
     :        ' top channels',STATUS)
	      GO TO 9000
	    ENDIF
	  ENDIF

*    Lower bound found exactly
	  IF(RECCC.EQ.RECCL)THEN
	    CHEN(NB)=EN(IEC)
D	    print *,'exact bound ',nb,chen(nb)
	    NB=NB+1
	    RELCL=RECCC			! Since energy and chan. will increment
	    IF(ENIN(IR+1).EQ.IEC)THEN
	      RELCC=RESP(IR+1)
	      IR=IR+1
	    ELSE
	      RELCC=0.0
	    ENDIF
	    IRSTART=IR-1
	    IEL=IEC
	    IEC=IEC+1
	    ICL=ICC

*    Lower bound crossed - interpolate to find it
	  ELSE
	    A=RELCL-RELCC
	    IF(A.LT.0.0)THEN
	      STATUS=SAI__ERROR
	      CALL ERR_REP('BADMATRIX','Bad response matrix',STATUS)
	      GO TO 9000
	    ENDIF
	    B=RECCC-RECCL
	    CHEN(NB)=(A*EN(IEC)+B*EN(IEL))/(A+B)	! Linear interpolation
D	    print *,'interpolated bound ',nb,chen(nb)
	    NB=NB+1

*       Prepare to increment channel, but not energy
	    RELCL=RELCC
	    IR=IRSTART
	    IF(ICC.LT.ICHLAST)THEN

*          Search for response element for new channel & last energy
	      DO WHILE(ENIN(IR).LT.IEL)
	        IR=IR+1
	      ENDDO
	      DO WHILE((CHIN(IR).LE.ICC).AND.(ENIN(IR).EQ.IEL))
	        IR=IR+1
	      ENDDO
	      IF((CHIN(IR).EQ.ICC+1).AND.(ENIN(IR).EQ.IEL))THEN
	        RELCC=RESP(IR)
	      ELSE
	        RELCC=0.0
	      ENDIF
	      IR=IRSTART			! Reset for next search
	      ICL=ICC
	    ENDIF
	  ENDIF
	ENDDO

* Find top channel bound (assign this where response falls by a factor of 5
*                         from its value immediately above the last bound)
	DO I=IR,NRESP
	  IF((CHIN(I).EQ.ICHLAST).AND.(RESP(I).GT.0.2*RECCC))THEN
	    IRSTART=I
	  ENDIF
	ENDDO
	ETOP=EN(ENIN(IRSTART))
D	print *,'top bound ',nb,etop

* Convert bounds to channel centres & half-widths
	DO I=1,NCHAN-1
	  CHEN(I)=(CHEN(I)+CHEN(I+1))/2
	  CHWID(I)=CHEN(I+1)-CHEN(I)		! Note: CHEN(I) is now CENTRE
	ENDDO
	CHEN(NCHAN)=(CHEN(NCHAN)+ETOP)/2
	CHWID(NCHAN)=ETOP-CHEN(NCHAN)

D	do i=1,nchan
D	  print *,i,chen(i),chwid(i)
D	enddo

* Exit
 9000	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR','from SPEC_CHEN',
     :  STATUS)
	END
