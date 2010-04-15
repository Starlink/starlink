*+WMERGE Merge two series of on/off time windows and overwrite 2nd series
	SUBROUTINE WMERGE(TS1,TE1,NW1,TS2,TE2,NW2)
	IMPLICIT NONE
* Input
	INTEGER		NW1		! # Windows in series 1
	DOUBLE PRECISION TS1(*),TE1(*)	! Start/end MJDs for series 1
* In/Out
	DOUBLE PRECISION TS2(*),TE2(*)	! Start/end MJDs for series 2 & merged
	INTEGER		NW2		! # windows in series 2 & merged

* Modified version of TMERGE.
*-
* local
	INTEGER		N, N1, N2, N12, NC, NTOT
	INTEGER		I, J, NCOUNT
	INTEGER		MAXWIN
	DOUBLE PRECISION TC(240000), TEMP
	INTEGER 	STATE(240000), STEMP, ON, OFF
	PARAMETER 	(ON=1, OFF=0)
	PARAMETER	(MAXWIN = 60000)

* Form the i/p windows series into single vectors with alternating ON,OFF states
	N1 = 2*NW1
	N2 = 2*NW2

* Do Series 1
	DO N = 1, NW1
	  TC(2*N-1) = TS1(N)
	  STATE(2*N-1) = ON
	  TC(2*N) = TE1(N)
	  STATE(2*N) = OFF
	ENDDO

* Do series 2
	DO N=1, NW2
	  TC((2*N-1)+N1) = TS2(N)
	  STATE((2*N-1)+N1) = ON
	  TC(2*N+N1) = TE2(N)
	  STATE(2*N+N1) = OFF
	ENDDO

* Bubble sort the times - do states at the same time
	NTOT = N1 + N2
	DO J = 2, NTOT
	  DO I = J, 2, -1
	    IF(TC(I) .GE. TC(I-1)) GOTO 10
	      TEMP = TC(I)
	      TC(I) = TC(I-1)
	      TC(I-1) = TEMP
	      STEMP = STATE(I)
	      STATE(I) = STATE(I-1)
	      STATE(I-1) = STEMP
	    ENDDO
10	ENDDO

* Now do the clever bit - sort out the overlaps. (ie find the time intervals
* of commonality between the two i/p series)
	N12 = 0
	NCOUNT = 0
	DO I = 1, NTOT
	  IF (STATE(I) .EQ. ON) THEN
	    IF (NCOUNT .EQ. 1) THEN
	      NCOUNT = NCOUNT + 1
	      N12 = N12 + 1
	      TC(N12) = TC(I)
	    ELSEIF(NCOUNT .EQ. 0)THEN
	      NCOUNT = NCOUNT + 1
	    ENDIF
	  ELSEIF (STATE(I) .EQ. OFF) THEN
	    IF (NCOUNT .EQ. 2) THEN
	      NCOUNT = NCOUNT - 1
	      N12 = N12 + 1
	      TC(N12) = TC(I)
	    ELSEIF (NCOUNT .EQ. 1) THEN
	      NCOUNT = NCOUNT - 1
	    ENDIF
	  ENDIF
	ENDDO

* Form the processed list of alternating ON,OFFs into merged windows
* Overwrite second input time series.

	NW2=MIN(N12/2,MAXWIN)
	DO N=1,NW2
	  TS2(N)=TC(2*N-1)
	  TE2(N)=TC(2*N)
	ENDDO

	END
