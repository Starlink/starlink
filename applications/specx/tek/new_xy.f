*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
C-----------------------------------------------------------------------

      SUBROUTINE NEW_XY (P, Q, OLD_LIMITS)

C   Subroutine to set new limits for current plot. Modifies limits held
C   in header records in plot file.

      LOGICAL*4 OLD_LIMITS
      REAL*4    P(2),Q(2)

      INCLUDE  'FLAGCOMM'
      INCLUDE  'PLOTPAR1'
      INCLUDE  'NEWXY'

      IF (.NOT.ISETSC) THEN
        XXST  = XST
        YYST  = YST
        XXEND = XEND
        YYEND = YEND
      ELSE
        XXST  = XRANGE(1)
        YYST  = YRANGE(1)
        XXEND = XRANGE(2)
        YYEND = YRANGE(2)
      END IF

CD    CALL SXGTIDLE()
CD    PRINT *,'Hard plot x-limits:',XST,XEND
CD    PRINT *,'Current x-limits:  ',XST1,XEND1
CD    PRINT *,'Requested x-limits:',P(1),P(2)
CD    PRINT *,'Hard plot y-limits:',YST,YEND
CD    PRINT *,'Current y-limits:  ',YST1,YEND1
CD    PRINT *,'Requested y-limits:',Q(1),Q(2)

      IF (OLD_LIMITS) THEN
        IF (P(1).GE.MIN(XST1,XEND1) .AND. P(1).LE.MAX(XST1,XEND1))
     &      XXST  = P(1)
        IF (P(2).GE.MIN(XST1,XEND1) .AND. P(2).LE.MAX(XST1,XEND1))
     &      XXEND = P(2)
        IF (Q(1).GE.MIN(YST1,YEND1) .AND. Q(1).LE.MAX(YST1,YEND1))
     &      YYST  = Q(1)
        IF (Q(2).GE.MIN(YST1,YEND1) .AND. Q(2).LE.MAX(YST1,YEND1))
     &      YYEND = Q(2)
      ELSE
        XXST  = P(1)
        XXEND = P(2)
        YYST  = Q(1)
        YYEND = Q(2)
      END IF

      CALL AUTORANGE (XXST, XXEND, NX)
      CALL AUTORANGE (YYST, YYEND, NY)

CD    PRINT *,'Revised x-limits:  ',XXST,XXEND
CD    PRINT *,'Revised y-limits:  ',YYST,YYEND

      CHANGE_SCALES = .TRUE.

      RETURN
      END
