      SUBROUTINE PDA_XERRWV( XMESS, NMESS, NERR, LEVEL, NI, I1, I2,
     :                       NR, R1, R2 )
C
C     Replacement subroutine for the SLATEC XERRWV package subroutine.
C     This version routes all output via PDA_XERMSG to conform to
C     PDA output standards.
C
      CHARACTER *( * ) XMESS
      CHARACTER *( 256 ) REPORT
      INTEGER IAT, STATUS
      REPORT = ' '
      IF ( LEVEL .LT. 1 ) RETURN

      STATUS = 0
      CALL PDA_XERMSG( ' ', ' ', XMESS, NERR, LEVEL, STATUS )
      WRITE(REPORT,
     :     '('' ERROR NUMBER = '',I5,'', MESSAGE LEVEL = '',I5)')
     :     NERR,LEVEL
      STATUS = 0
      CALL PDA_XERMSG( ' ', ' ', REPORT, NERR, LEVEL, STATUS )
      ASSIGN 80 TO IGOIPR
      GO TO (10,20),NI
      GO TO 80
   10 CONTINUE
C
C     WRITE ONE INTEGER VALUE.
      REPORT = ' '
      WRITE(REPORT,'('' I1 = '',I8)') I1
      STATUS = 0
      CALL PDA_XERMSG( ' ', ' ', REPORT, NERR, LEVEL, STATUS )
      GO TO IGOIPR
   20 ASSIGN 30 TO IGOIPR
      GO TO 10
   30 CONTINUE
C
C     WRITE ONE INTEGER VALUE.
      REPORT = ' '
      WRITE(REPORT,'('' I2 = '',I8)') I2
      STATUS = 0
      CALL PDA_XERMSG( ' ', ' ', REPORT, NERR, LEVEL, STATUS )
   80 CONTINUE
      ASSIGN 40 TO IGOIPR
      GO TO (50,60),NR
   40 RETURN
   50 CONTINUE
C
C     WRITE ONE REAL VALUE.
      REPORT = ' '
      WRITE(REPORT,'('' R1 = '',1PE15.7)') R1
      STATUS = 0
      CALL PDA_XERMSG( ' ', ' ', REPORT, NERR, LEVEL, STATUS )
      GO TO IGOIPR
   60 ASSIGN 70 TO IGOIPR
      GO TO 50
   70 CONTINUE
C
C     WRITE REAL VALUE.
      REPORT = ' '
      WRITE(REPORT,'('' R2 = '',1PE15.7)') R2
      STATUS = 0
      CALL PDA_XERMSG( ' ', ' ', REPORT, NERR, LEVEL, STATUS )
      GO TO 40
      END
