       SUBROUTINE ERMESS (COMND,MAXCL,J)

       LOGICAL BEEP
       COMMON /BEEP/ BEEP
       CHARACTER*1 BLEEP
       COMMON /BLEEP/ BLEEP
       CHARACTER*1 COMND

       IF (BEEP) THEN
          IF (J.LE.0) THEN
             WRITE (*,
     :       '(''   ISINP_'',A1,'':  negative cloud numbers '',
     :         ''forbidden'',A)') COMND, BLEEP
          ELSEIF (J.GT.MAXCL) THEN
             WRITE (*,
     :       '(''   ISINP_'',A1,'':  maximimum'',I3,
     :       '' clouds allowed'',A)') COMND, MAXCL, BLEEP
          ELSE
             WRITE (*,
     :       '(''   ISINP_'',A1,'':  cloud does not exist'',A)')
     :       COMND, BLEEP
          ENDIF
       ELSE
          IF (J.LE.0) THEN
             WRITE (*,
     :       '(''   ISINP_'',A1,'':  negative cloud numbers '',
     :         ''forbidden'')') COMND
          ELSEIF (J.GT.MAXCL) THEN
             WRITE (*,
     :       '(''   ISINP_'',A1,'':  maximimum'',I3,
     :       '' clouds allowed'')') COMND, MAXCL
          ELSE
             WRITE (*,
     :       '(''   ISINP_'',A1,'':  cloud does not exist'')')
     :       COMND
          ENDIF
       ENDIF

       END
