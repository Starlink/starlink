*SKEW
*
* SKEW  -- shifts row of a frame to add or subtract positions computed from
*          a fit file dumped by track. i.e. skew can straigten a spectrum or
*          do the reverse.
*
* The shifting is accomplished by rebinning which smooths somewhat and
* therefore skew is not a reversible operation. It should not be used say in
* combination with extopt to extract spectra. Use profit and optext
* instead.
*
* Parameters:
*
*  IMAGE   -- Data frame to skew
*
*  TRACK   -- spectrum position poly to use
*
*  SKEWED  -- the output frame
*
*  YSTART, YEND  -- the range in Y to skew
*
*  REMOVE  -- TRUE to remove rather than add the fit.
*
*  REBIN   -- Method of rebinning. Linear, quadratic or sin x/x
*             interpolation. (The latter is best if profiles are
*             undersampled.)
*
*SKEW
      SUBROUTINE SKEW(STATUS)
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'CNF_PAR'
      INTEGER STATUS, IMAGE, NDIM, LBND(2), UBND(2), SMALL, SKEWED
      INTEGER TEMP, YLO, YHI, NXS, NYS, IPTR, EL, OPTR
      LOGICAL BASE, REMOVE
      DOUBLE PRECISION YPOS, TOFF
      CHARACTER*1 REBIN
C
C     Start
C
      IF(STATUS.NE.SAI__OK) RETURN
      CALL NDF_BEGIN
C
C     Open data file
C
      CALL NDF_ASSOC('IMAGE', 'READ',IMAGE, STATUS)
C
C     Open track file
C
      CALL GET_TRACK(YPOS, TOFF, STATUS)
C
C     Get skewed output file
C
      CALL NDF_SECT(IMAGE,1,1,1,SMALL,STATUS)
      CALL NDF_PROP(SMALL,' ','SKEWED',SKEWED,STATUS)
C
C     Get region to correct
C
      CALL NDF_ISBAS(IMAGE, BASE, STATUS)
      CALL NDF_BOUND(IMAGE, 2, LBND, UBND, NDIM, STATUS)
      IF(BASE .AND. NDIM.EQ.2) THEN
         CALL PAR_GDR0I('YSTART',LBND(2),LBND(2),UBND(2),
     &        .FALSE.,YLO,STATUS)
         CALL PAR_GDR0I('YEND',UBND(2),YLO,UBND(2),
     &        .FALSE.,YHI,STATUS)
         IF(YLO.NE.LBND(2) .OR. YHI.NE.UBND(2)) THEN
            LBND(2) = YLO
            UBND(2) = YHI
            CALL NDF_SECT(IMAGE, 2, LBND, UBND, TEMP, STATUS)
            CALL NDF_ANNUL(IMAGE, STATUS)
            CALL NDF_CLONE(TEMP, IMAGE, STATUS)
            CALL NDF_ANNUL(TEMP, STATUS)
         END IF
      ELSE
         YLO = LBND(2)
         YHI = UBND(2)
      END IF
      NXS     = UBND(1)-LBND(1)+1
      NYS     = YHI-YLO+1
C
C     Add or remove trace?
C
      CALL PAR_GET0L('REMOVE',REMOVE,STATUS)
C
C     What type of rebinning.
C
      CALL PAR_CHOIC('REBIN','Q','L,Q,S',.FALSE.,REBIN,STATUS)
C
C     Get output into right format
C
      CALL NDF_RESET(SKEWED,'Data',STATUS)
      CALL NDF_SBND(2,LBND,UBND,SKEWED,STATUS)
      CALL NDF_STYPE('_REAL',SKEWED,'Data',STATUS)
C
C     Map
C
      CALL NDF_MAP(IMAGE,'Data','_REAL','READ',IPTR,EL,STATUS)
      CALL NDF_MAP(SKEWED,'Data','_REAL','WRITE',OPTR,EL,STATUS)
C
C     Skew frame
C
      CALL PIC_SKEW(%VAL(CNF_PVAL(IPTR)), %VAL(CNF_PVAL(OPTR)),
     &     NXS, NYS, YLO, REMOVE, REBIN, STATUS)
      CALL NDF_END(STATUS)
      RETURN
      END

      SUBROUTINE PIC_SKEW(INPUT, OUTPUT, NXS, NYS, YLO,
     &     REMOVE, METHOD, STATUS)
C
C Written by T.R. Marsh August 1988
C
C Subroutine to shift a frame in the X direction by a Y
C dependent amount fixed by a track file
C
C R*4 INPUT(NXS,NYS)  -- Data frame, input
C R*4 OUTPUT(NXS,NYS) -- Data frame, output.
C I*4 NXS, NYS        -- Dimensions of data sections
C I*4 YLO             -- Lower Y limit
C L   REMOVE          -- TRUE = Remove shift, FALSE = add shift.
C C*(*) METHOD        -- 'L' = Linear rebin, 'Q' = Quadratic,
C                        'S' = Sinc function
C I*4 STATUS
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER STATUS, NXS, NYS, YLO, IY
      REAL INPUT(NXS,NYS), OUTPUT(NXS,NYS), XSHIFT
      DOUBLE PRECISION POLY, XREF, XD, YD
      CHARACTER*(*) METHOD
      LOGICAL REMOVE
C
      CALL GET_TRACK(DBLE(2*YLO+NYS-1)/2.D0, XREF, STATUS)
      IF(STATUS.NE.SAI__OK) RETURN
      CALL MSG_BLANK(STATUS)
      DO IY = 1, NYS
         CALL GET_TRACK(DBLE(IY+YLO-1), XD, STATUS)
         IF(STATUS.NE.SAI__OK) RETURN
         XSHIFT = REAL(XREF - XD)
         IF(IY.EQ.1 .OR. IY.EQ.NYS) THEN
            CALL MSG_SETI('IY',IY+YLO-1)
            CALL MSG_SETR('XSHIFT',-XSHIFT)
            CALL MSG_OUT(' ','Shift at Y = ^IY = ^XSHIFT pixels.',
     &           STATUS)
         END IF
         IF(REMOVE) XSHIFT = -XSHIFT
         IF(METHOD.EQ.'L') THEN
            CALL REBIN(0,0,INPUT(1,IY),NXS,OUTPUT(1,IY),1,XSHIFT,
     &           0,0.,0,0.)
         ELSE IF(METHOD.EQ.'Q') THEN
            CALL REBIN(0,1,INPUT(1,IY),NXS,OUTPUT(1,IY),1,XSHIFT,
     &           0,0.,0,0.)
         ELSE IF(METHOD.EQ.'S') THEN
            CALL SINCBIN(0,INPUT(1,IY),NXS,OUTPUT(1,IY),NXS,XSHIFT,
     &           0,0.,0,0.,0.,0.)
         END IF
      END DO
      RETURN
      END

