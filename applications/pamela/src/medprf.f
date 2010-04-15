*MEDPRF
*
* MEDPRF -- Flat fields, median filters and collapses an image in the
*           Y direction. Can use distortion file from 'track'.
*
* Parameters:
*
*  IMAGE       -- Input image
*
*  FLAT        -- Balance frame
*
*  TRACE       -- Distortion file to be used?
*
*  TRACK       -- Name of distortion file if wanted
*
*  PROFILE     -- Output profile
*
*  XSTART,XEND -- X pixel limits of region to be treated.
*
*  YSTART,YEND -- Y pixel limits of region to be treated.
*
*  NWIDTH      -- Half width for median filter in pixels
*
*  METHOD      -- Rebinning method. Linear, Quadratic, or Sinc
*                 (For trace=true only)
*
*MEDPRF
      SUBROUTINE MEDPRF(STATUS)
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'CNF_PAR'
      INTEGER STATUS, IMAGE, FLAT, NDIM, EL, SMALL, PROFILE
      INTEGER LBND(2), UBND(2), XLO, XHI, YLO, YHI, NXS, NYS
      INTEGER TEMP, NWIDTH, IPTR, FPTR, PPTR, PLACE, WORK, WPTR
      DOUBLE PRECISION YPOS, TOFF
      LOGICAL TRACE, BASE
      CHARACTER*1 METHOD
C
C     Start
C
      IF(STATUS.NE.SAI__OK) RETURN
      CALL NDF_BEGIN
C
C     Open data file
C
      CALL NDF_ASSOC('IMAGE','READ',IMAGE,STATUS)
C
C     Open balance frame
C
      CALL NDF_ASSOC('FLAT','READ',FLAT,STATUS)
C
C     Force to same size
C
      CALL NDF_MBND('TRIM', IMAGE, FLAT, STATUS)
C
C     See if distortion file is wanted
C
      CALL PAR_GET0L('TRACE', TRACE, STATUS)
      IF(TRACE)
     &     CALL GET_TRACK(YPOS, TOFF, STATUS)
C
C     Get profile output name
C
      CALL NDF_SECT(IMAGE,1,1,1,SMALL,STATUS)
      CALL NDF_PROP(SMALL,' ','PROFILE',PROFILE,STATUS)
      CALL NDF_RESET(PROFILE,'Title',STATUS)
      CALL NDF_CPUT('MEDPRF output',PROFILE,'Title',STATUS)
C
C     What region of frame to use
C
      CALL NDF_ISBAS(IMAGE, BASE, STATUS)
      CALL NDF_BOUND(IMAGE, 2, LBND, UBND, NDIM, STATUS)
      IF(BASE .AND. NDIM.EQ.2) THEN
         CALL PAR_GDR0I('XSTART',LBND(1),LBND(1),UBND(1),
     &        .FALSE.,XLO,STATUS)
         CALL PAR_GDR0I('XEND',UBND(1),XLO,UBND(1),
     &        .FALSE.,XHI,STATUS)
         CALL PAR_GDR0I('YSTART',LBND(2),LBND(2),UBND(2),
     &        .FALSE.,YLO,STATUS)
         CALL PAR_GDR0I('YEND',UBND(2),YLO,UBND(2),
     &        .FALSE.,YHI,STATUS)
         IF(XLO.NE.LBND(1) .OR. XHI.NE.UBND(1) .OR.
     &        YLO.NE.LBND(2) .OR. YHI.NE.UBND(2)) THEN
            LBND(1) = XLO
            UBND(1) = XHI
            LBND(2) = YLO
            UBND(2) = YHI
            CALL NDF_SECT(IMAGE, 2, LBND, UBND, TEMP, STATUS)
            CALL NDF_ANNUL(IMAGE, STATUS)
            CALL NDF_CLONE(TEMP, IMAGE, STATUS)
            CALL NDF_ANNUL(TEMP, STATUS)
            CALL NDF_SECT(FLAT, 2, LBND, UBND, TEMP, STATUS)
            CALL NDF_ANNUL(FLAT, STATUS)
            CALL NDF_CLONE(TEMP, FLAT, STATUS)
            CALL NDF_ANNUL(TEMP, STATUS)
         END IF
      END IF
C
C     Section dimensions
C
      NXS = UBND(1)-LBND(1)+1
      NYS = UBND(2)-LBND(2)+1
      XLO = LBND(1)
      YLO = LBND(2)
      CALL PAR_GODD('NWIDTH',11,3,1001,.FALSE.,NWIDTH,STATUS)
      IF(TRACE)
     &     CALL PAR_CHOIC('METHOD','Q','L,Q,S',.FALSE.,METHOD,STATUS)
C
C     Set profile NDF up
C
      CALL NDF_RESET(PROFILE,'Data',STATUS)
      CALL NDF_SBND(1,LBND(1),UBND(1),PROFILE,STATUS)
      CALL NDF_STYPE('_REAL',PROFILE,'Data',STATUS)
C
C     Map files
C
      CALL NDF_MAP(IMAGE,'Data','_REAL','READ',IPTR,EL,STATUS)
      CALL NDF_MAP(FLAT,'Data','_REAL','READ',FPTR,EL,STATUS)
      CALL NDF_MAP(PROFILE,'Data','_REAL','WRITE',PPTR,EL,STATUS)
      IF(TRACE) THEN
         CALL NDF_TEMP(PLACE, STATUS)
         CALL NDF_NEW('_REAL',2,LBND,UBND,PLACE,WORK,STATUS)
         CALL NDF_MAP(WORK,'Data','_REAL','WRITE',WPTR,EL,STATUS)
      END IF
      CALL MED_PRF(%VAL(CNF_PVAL(IPTR)), %VAL(CNF_PVAL(FPTR)),
     &     NXS, NYS, XLO, YLO, %VAL(CNF_PVAL(PPTR)), TRACE,
     &     %VAL(CNF_PVAL(WPTR)), NWIDTH, METHOD, STATUS)
      CALL NDF_END(STATUS)
      RETURN
      END

      SUBROUTINE MED_PRF(DATA, BAL, NXS, NYS, XLO, YLO, PROF, TRACE,
     &     WORK, NWIDTH, METHOD, STATUS)
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      LOGICAL TRACE
      INTEGER NXS, NYS, NWIDTH, XLO, YLO, STATUS, IX, IY, NOK
      INTEGER IFAIL, I1, I2
      REAL DATA(NXS,NYS), BAL(NXS,NYS), WORK(NXS,NYS), PROF(NXS)
      REAL XSHIFT
      DOUBLE PRECISION YREF, YD, XD, XREF, SUM, POLY
      CHARACTER*(*) METHOD
      INTEGER MAXX, MAXY
      PARAMETER (MAXX=3000, MAXY=5000)
      REAL XDATA(MAXX), TPROF(MAXX)
      REAL YDATA(MAXY), YFILT(MAXY)
C
      IF(STATUS.NE.SAI__OK) RETURN
      IF(MAXX.LT.NXS .OR. MAXY.LT.NYS) THEN
         CALL MSG_SETI('MAXX',MAXX)
         CALL MSG_SETI('MAXY',MAXY)
         CALL MSG_OUT(' ','Frame large than maximum ^MAXX, ^MAXY',
     &        STATUS)
         CALL MSG_OUT(' ','Fixed buffers need increasing',
     &        STATUS)
         STATUS = SAI__ERROR
         RETURN
      END IF
C
C     Compute average profile by collapsing in Y
C
      IF(.NOT.TRACE) THEN
C
C     No distortion map case
C
         DO IX = 1, NXS
            NOK = 0
            DO IY = 1, NYS
               IF(DATA(IX,IY).NE.VAL__BADR .AND.
     &              BAL(IX,IY).NE.VAL__BADR) THEN
                  NOK = NOK + 1
                  YDATA(NOK) = BAL(IX,IY)*DATA(IX,IY)
               END IF
            END DO
            IF(NOK.GT.0) THEN
               CALL MEDFILT(YDATA,YFILT,NOK,NWIDTH,IFAIL)
               SUM = 0.D0
               DO IY = 1, NOK
                  SUM = SUM + YFILT(IY)
               END DO
               PROF(IX) = REAL(SUM/REAL(NOK))
            ELSE
               PROF(IX) = VAL__BADR
            END IF
         END DO
      ELSE
C
C     Distortion coefficient case
C
         YREF  = DBLE(2*YLO+NYS-1)/2.
         CALL GET_TRACK(YREF, XREF, STATUS)
         DO IY = 1, NYS
C
C     Compute shift, load into buffer, shift to the right and then
C     add into profile
C
            CALL GET_TRACK(DBLE(IY+YLO-1), XD, STATUS)
            XSHIFT = REAL(XD - XREF)
            DO IX = 1, NXS
               IF(DATA(IX,IY).NE.VAL__BADR .AND.
     &              BAL(IX,IY).NE.VAL__BADR) THEN
                  XDATA(IX) = BAL(IX,IY)*DATA(IX,IY)
               ELSE
                  XDATA(IX)  = 0.
               END IF
            END DO
            IF(METHOD.EQ.'L') THEN
               CALL REBIN(0,0,XDATA,NXS,TPROF,1,XSHIFT,
     &              0,0.,0,0.)
            ELSE IF(METHOD.EQ.'Q') THEN
               CALL REBIN(0,1,XDATA,NXS,TPROF,1,XSHIFT,
     &              0,0.,0,0.)
            ELSE IF(METHOD.EQ.'S') THEN
               CALL SINCBIN(0,XDATA,NXS,TPROF,NXS,XSHIFT,
     &              0,0.,0,0.,0.,0.)
            END IF
C
C     Mask pixels affected by bad data (only works in linear case)
C
            DO IX = 1, NXS
               I1 = INT(REAL(IX)-XSHIFT)
               I2 = I1+1
               IF((I1.GE.1 .AND. I1.LE.NXS .AND.
     &              (DATA(I1,IY).EQ.VAL__BADR .OR.
     &              BAL(I1,IY).EQ.VAL__BADR)) .OR.
     &              (I2.GE.1 .AND. I2.LE.NXS .AND.
     &              (DATA(I2,IY).EQ.VAL__BADR .OR.
     &              BAL(I2,IY).EQ.VAL__BADR))) THEN
                  WORK(IX,IY) = VAL__BADR
               ELSE
                  WORK(IX,IY) = TPROF(IX)
               END IF
            END DO
         END DO
C
C     Now median filter
C
         DO IX = 1, NXS
            NOK = 0
            DO IY = 1, NYS
               IF(WORK(IX,IY).NE.VAL__BADR) THEN
                  NOK = NOK + 1
                  YDATA(NOK) = WORK(IX,IY)
               END IF
            END DO
            IF(NOK.GT.0) THEN
               CALL MEDFILT(YDATA,YFILT,NOK,NWIDTH,IFAIL)
               SUM = 0.
               DO IY = 1, NOK
                  SUM = SUM + YFILT(IY)
               END DO
               PROF(IX) = REAL(SUM/REAL(NOK))
            ELSE
               PROF(IX) = VAL__BADR
            END IF
         END DO
      END IF
      RETURN
      END
