      SUBROUTINE GET_TRACK(YPOS, TOFF, STATUS)
*
*     Reads track file and returns the values. The NDF system
*     must have been started. The first time this is used it
*     will prompt for the track file and not calculate anything,
*     thereafter, it will return the computed X position of the
*     corresponding to the Y value supplied.
*
* D>  YPOS    -- Y value at which to compute track offset
* D<  TOFF    -- the computed track offset.
* I>< STATUS  -- Error status
*
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CNF_PAR'
      INTEGER NPOS
      DOUBLE PRECISION YPOS, TOFF, YT(1), TT(1)
      LOGICAL INIT
      INTEGER TRACK, STATUS, NPOLY, NSPLINE, NORD, NPCHECK
      INTEGER NDIM, TPTR, EL, XPTR, CPTR
      CHARACTER*(DAT__SZLOC) LOC, LOC1
      SAVE INIT, TRACK, NPOLY, NSPLINE, NORD, TPTR
      SAVE XPTR, CPTR
      DATA INIT/.TRUE./

      IF(STATUS .NE. SAI__OK) RETURN

      IF(INIT) THEN
         CALL NDF_ASSOC('TRACK','READ',TRACK,STATUS)
C
C     Recover and store any old poly fit.
C
         CALL NDF_XGT0I(TRACK, 'PAMELA', 'TRACK.NPOLY', NPOLY, STATUS)
         IF(NPOLY.GT.0) THEN
            CALL NDF_DIM(TRACK, 1, NPCHECK, NDIM, STATUS)
            IF(NPCHECK.NE.NPOLY) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP('GET_TRACK',
     &              'Conflict between PAMELA.TRACK.NPOLY' //
     &              ' and actual number of coefficients', STATUS)
            END IF
            CALL NDF_MAP(TRACK,'Data','_DOUBLE','READ',TPTR,EL,STATUS)
         END IF
C
C     Prompt for new number of poly terms that need not be related
C     in any way to the old number. Then modify the data array
C     appropriately. Finally recover spline info.
C
         CALL NDF_XGT0I(TRACK, 'PAMELA', 'TRACK.NSPLINE',NSPLINE,STATUS)
         CALL NDF_XGT0I(TRACK, 'PAMELA', 'TRACK.NORD', NORD, STATUS)
         CALL NDF_XLOC(TRACK,'PAMELA','READ',LOC,STATUS)
         CALL DAT_FIND(LOC, 'TRACK', LOC1, STATUS)
         CALL CMP_MAPV(LOC1,'XKNOT','_DOUBLE','READ',XPTR,EL,STATUS)
         IF(NSPLINE+2*NORD-1.NE.EL) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP('GET_TRACK',
     &           'Incorrect number of knots given NSPLINE & NORD',
     &           STATUS)
         END IF
         CALL CMP_MAPV(LOC1,'CSPLINE','_DOUBLE','READ',CPTR,EL,STATUS )
         IF(NSPLINE+NORD-1.NE.EL) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP('GET_TRACK',
     &           'Incorrect number of coeffs given NSPLINE & NORD',
     &           STATUS)
         END IF
         CALL DAT_ANNUL(LOC1, STATUS)
         CALL DAT_ANNUL(LOC, STATUS)
         INIT = .FALSE.
      ELSE
         YT(1) = YPOS
         CALL SPLCALCB(1,YT,%VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(CPTR)),
     &        NSPLINE,NORD,TT,STATUS)
         TOFF = TT(1)
         IF(NPOLY.GT.0)
     &        CALL CALPOLY(YPOS, %VAL(CNF_PVAL(XPTR)), NSPLINE+2*NORD-1,
     &        %VAL(CNF_PVAL(TPTR)), NPOLY, TOFF, STATUS)
      END IF
      RETURN
      END

      SUBROUTINE CALPOLY(YPOS, XKNOT, NKNOT, CPOLY, NPOLY,
     &     TOFF, STATUS)
*
*     Adds polynomial part of the track offset
*
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER NPOS, NSPLINE, NORD, NPOLY, STATUS, NKNOT
      DOUBLE PRECISION YPOS, TOFF, POLY
      DOUBLE PRECISION XKNOT(NKNOT), CPOLY(NPOLY)
      DOUBLE PRECISION XMID, XHRANGE, YSCALE

      IF(STATUS .NE. SAI__OK) RETURN

      XMID    = (XKNOT(1)+XKNOT(NKNOT))/2.
      XHRANGE = (XKNOT(NKNOT)-XKNOT(1))/2.
      TOFF = TOFF + POLY(CPOLY, NPOLY, (YPOS-XMID)/XHRANGE)
      RETURN
      END
