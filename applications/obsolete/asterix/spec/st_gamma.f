*
*-------------------------------------------------------------------------------
*
        REAL*8 FUNCTION ST_GAMMA(PARAM)
*
* This routine is designed to help prevent NAG hard failures in S14AAF
* (S14AAF caclulates the Gamma function). Before calling S14AAF with an
* input parameter PARAM this routine should be called with the same parameter.
* If PARAM is in a range likely to cause problems it is adjusted accordingly.
*
*        PARAM is the input argument for S14AAF
*        ST_GAMMA is the suggested value to avoid hard failures
*
        REAL*8 PARAM
*
        INTEGER STATUS
        LOGICAL TGFLAG
        REAL*8 INTBIT
        REAL*8 NEGMAX
        REAL*8 NEGMIN
        REAL*8 POSMAX
        REAL*8 POSMIN
*
        DATA NEGMAX/-25.1D0/,NEGMIN/-1.0D-25/
        DATA POSMAX/25.0D0/,POSMIN/1.0D-25/
*
        TGFLAG=.FALSE.
*
        IF (PARAM .GT. POSMAX) THEN
          ST_GAMMA=POSMAX
          TGFLAG=.TRUE.
        ELSE IF (PARAM .GT. 0.0D0 .AND. PARAM .LT. POSMIN) THEN
          ST_GAMMA=POSMIN
          TGFLAG=.TRUE.
        ELSE IF (PARAM .LT. NEGMAX) THEN
          ST_GAMMA=NEGMAX
          TGFLAG=.TRUE.
        ELSE IF (PARAM .LT. 0.0D0 .AND. PARAM .GT. NEGMIN) THEN
          ST_GAMMA=NEGMIN
          TGFLAG=.TRUE.
        ELSE
          ST_GAMMA=PARAM
        END IF
*
        IF (PARAM .LT. 0.0D0) THEN
          INTBIT=DMOD(ST_GAMMA,1.0D0)
          IF (INTBIT .EQ. 0.0D0) THEN
            ST_GAMMA=0.99999D0*ST_GAMMA
            TGFLAG=.TRUE.
          END IF
        END IF
*
        IF (TGFLAG) CALL MSG_OUT('CHANGED',
     :    'Warning parameter for Gamma function adjusted by ST_GAMMA',
     :    STATUS)
*
        END
