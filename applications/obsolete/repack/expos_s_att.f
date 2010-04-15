*+EXPOS_S_ATT Read a record from the aspect file
	SUBROUTINE EXPOS_S_ATT (DMJD, ASPECT, SCTOC, ATT_S_T,
     :	ATT_E_T, STATUS)
	IMPLICIT NONE
* Input
	DOUBLE PRECISION DMJD		  ! Time for att request
	INCLUDE	'ASR_EXPATT.INC'
	record /ATTSTRUC/ aspect

* Output
	DOUBLE PRECISION SCTOC(3,3)	  ! DCM St ref to Cel
	DOUBLE PRECISION ATT_S_T, ATT_E_T ! Start end times of the ATT record
	INTEGER	STATUS	    	          ! Status flag
* M. Denby Sep 88
* P McGale   UNIX mods May 95
*-
* local
	INTEGER	I, J
	INTEGER ERRTYPE

* Get record from aspect file.
        CALL GET_ASPECT(DMJD, ASPECT, STATUS)
        DO J = 1,3
          DO I = 1,3
            SCTOC(I,J) = DBLE(ASPECT.FOV2SKY(J,I))
          END DO
        END DO

* Construct time window for this aspect record
	ATT_S_T = ASPECT.REC_START_MJD
	ATT_E_T = ASPECT.REC_END_MJD

	END
