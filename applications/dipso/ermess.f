       SUBROUTINE ERMESS (COMND,MAXCL,J)

       LOGICAL BEEP
       COMMON /BEEP/ BEEP
       CHARACTER*1 COMND

       IF (BEEP) THEN
          IF (J.LE.0) THEN
             WRITE (*,
     :       '(''   ISINP_'',A1,'':  negative cloud numbers '',
     :         ''forbidden'',A)') COMND, 7
          ELSEIF (J.GT.MAXCL) THEN
             WRITE (*,
     :       '(''   ISINP_'',A1,'':  maximimum'',I3,
     :       '' clouds allowed'',A)') COMND, MAXCL, 7
          ELSE
             WRITE (*,
     :       '(''   ISINP_'',A1,'':  cloud does not exist'',A)')
     :       COMND, 7
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
