      SUBROUTINE ENCD (VALU,ASH,IOUT,NC,IOFFD)
C
C
C
C
C ON INPUT     VALU      FLOATING POINT NUMBER FROM WHICH THE LABEL IS
C                        TO BE CREATED.
C              ASH       SEE IOFFD.
C              IOFFD     IF IOFFD .EQ. 0, A LABEL WHICH REFLECTS THE
C                             MAGNITUDE OF VALU IS TO BE CREATED.
C                             .1 .LE. ABS(VALU) .LE. 99999.49999...
C                             OR VALUE .EQ. 0.0.  THE LABEL CREATED
C                             SHOULD HAVE 3 TO 5 CHARACTERS DEPENDING
C                             ON THE MAGNITUDE OF VALU.  SEE IOUT.
C                        IF IOFFD .NE. 0, A LABEL WHICH DOES NOT REFLECT
C                             THE MAGNITUDE OF VALU IS TO BE CREATED.
C                             ASH IS USED AS THE NORMALIZATION FACTOR.
C                             1. .LE. ASH*ABS(VALU) .LT. 1000. OR
C                             VALU .EQ. 0.0.  THE LABEL CREATED SHOULD
C                             HAVE 1 TO 3 CHARACTERS, DEPENDING ON THE
C                             MAGNITUDE OF ASH*VALU.  SEE IOUT.
C ON OUTPUT    IOUT      CONTAINS THE LABEL CREATED.  IT SHOULD HAVE NO
C                        LEADING BLANKS.  SEE NC.
C              NC        THE NUMBERS IN THE LABEL IN IOUT.  SHOULD BE
C                        1 TO 5.
C
      SAVE
      CHARACTER*11    IFMT   ,IOUT*6
C
C IFMT MUST HOLD 11 CHARACTERS
C
C
C
C
C
      VAL = VALU
      IF (IOFFD .NE. 0) GO TO 103
      IF (VAL) 101,104,101
  101 LOG = IFIX((ALOG10(ABS(VAL))+.00001)+5000.)-5000
      V = VAL
      NS = MAX0(4,MIN0(6,LOG+2))
      ND = MIN0(3,MAX0(0,2-LOG))
      IF (VAL.LT.0)  NS = NS + 1
  102 WRITE (IFMT,'(A2,I2,A1,I1,A1)') '(F',NS,'.',ND,')'
      WRITE (IOUT,IFMT) V
      NC = NS
      IF (LOG.GE.3)  NC = NC - 1
      RETURN
  103 NS = 4
      IF (VAL.LT.0.)  NS=5
      IF (VAL.EQ.0.)  NS=2
      ND = 0
      V = VAL*ASH
      LOG = 100
      GO TO 102
  104 NS = 3
      ND = 1
      LOG = -100
      V = 0.
      GO TO 102
C
C1001 FORMAT('(F',I2,'.',I1,',1H',A1,')')
C
      END
