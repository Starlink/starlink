C# IL>=a, OL>=0
      SUBROUTINE GQWKS (IWKID,IER,ISTATE)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE WORKSTATION STATE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns workstation state.
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWKID  - workstation identifier
*     OUT IER    - error indicator
*     OUT ISTATE - workstation state
*
      INTEGER IWKID, IER, ISTATE
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
      INTEGER I,IWKIX
*
*  ERRORS
*  ------
*     20   Specified workstation identifier is invalid
*     25   Specified workstation is not open
*     33   Specified workstation is of category MI
*     35   Specified workstation is of category INPUT
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GWSOP,GSGOP)

      IER = KERROR
      IF (KERROR.EQ.0) THEN

        IF (IWKID.LE.0) THEN
          IER = 20

        ELSE

* See if workstation identifier exists

          DO 10 I=1,KWK
            IF (KWKID(I).EQ.IWKID) GOTO 20
   10     CONTINUE

          IER = 25
          GOTO 999

   20     CONTINUE
          IWKIX = I

          IF (KWKC(IWKIX).EQ.GMI) THEN
            IER = 33

          ELSEIF (KWKC(IWKIX).EQ.GINPUT) THEN
            IER = 35

          ELSE

            DO 30 I=1,KNACWK
              IF (KACPT(I).EQ.IWKIX) GOTO 40
   30       CONTINUE

            ISTATE = GINACT
            GOTO 999

   40       CONTINUE
            ISTATE = GACTIV

          ENDIF

        ENDIF

      ENDIF

  999 CONTINUE
      END
