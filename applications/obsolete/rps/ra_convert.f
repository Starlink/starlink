*+RA_CONVERT       Converts a character RA angle to radians, makes 'standard'
      SUBROUTINE RA_CONVERT(CVALRA, RA, LSTATUS)
 
*  Type Declaration
      IMPLICIT NONE
 
*  Calling Arguments
      CHARACTER*(*) CVALRA		! will be changed if degrees or separator not a space
      DOUBLE PRECISION    RA
      LOGICAL LSTATUS
      INTEGER  FIND_FIRST
*  Pi data
      INCLUDE 'zpidata.inc'
  
********************************************************************************
*  Method
*     Reads input as text.  A valid separator indicates not degrees.
*     Allowed: ':'  '/'  ','  ' '
*     The input can be hour,minute,second or degrees
*
*  History
*     1988 Jan		M Bush, RAL	1st version
*          May		M Ricketts ::	Modified to accept degrees also
*     1989 Feb		   ::		Name, args changed from INRA2
*     1992 June		M. Duesterhaus  remove VAX RTL calls
*-******************************************************************************

*  Functions
 
*   Local variables:

      CHARACTER*1 SEP(4)/':', '/', ',', ' '/
      CHARACTER*1 CHARSEP
      INTEGER INDEX, SUBINDEX, START,N1,N2,N3, INDEX2, ISTAT, NCHAR
      INTEGER HOUR,MINUTE
      REAL SECOND
      INTEGER STATUS
      DOUBLE PRECISION DEGREES
      INTEGER MDH_ENDWORD
      INTEGER IHMSF(4)
      CHARACTER*10 CVALNEW, CSIGN*1
      LOGICAL REFORMAT

*  Executable Code
 
      LSTATUS = .TRUE.
      NCHAR = MDH_ENDWORD(CVALRA)
      START = 1									! find first non-space character
      DO WHILE (CVALRA(START:START) .EQ. ' ' .AND. START .LT. NCHAR)
         START = START + 1
      END DO
 
      ISTAT = FIND_FIRST (CVALRA(START:NCHAR), ':/, ', SUBINDEX)	!See if separator, ie hms
      INDEX = ISTAT + START - 1
 
         IF (ISTAT.EQ.0) THEN		! Assume its degrees
 
            READ(CVALRA,*,IOSTAT=ISTAT) DEGREES
            IF (ISTAT.NE.0) GOTO 20
            IF (DEGREES .LT. 0.D0  .OR. DEGREES .GT. 360.0D0 )GOTO 20
            RA = DEGREES * DDTOR
            REFORMAT = .TRUE.
 
         ELSE				! Its hour / minute / second in some form
 
            N1 = INDEX - 1
	    READ(CVALRA(START:N1),'(I)')HOUR
 
            CHARSEP = SEP(SUBINDEX)
C            IF (CHARSEP .NE. ' ' ) THEN
               REFORMAT = .TRUE.
C            ELSE
C               REFORMAT = .FALSE.
C            END IF
            START = INDEX + 1
	    INDEX2 = FIND_FIRST (CVALRA(START:),CHARSEP,SUBINDEX)
            IF (INDEX2.EQ.0) GOTO 20
            N2 = START + INDEX2 - 2
 
	    READ(CVALRA(START:N2),*)MINUTE
 
            N3 = N2 + 2
            IF (N3.GT.NCHAR) THEN
              SECOND = 0.0
            ELSE
              READ(CVALRA(N3:NCHAR), * , IOSTAT = STATUS) SECOND				! this may be real
              IF (STATUS .NE.0) GOTO 20
            END IF
            CALL SLA_DTF2R( HOUR, MINUTE, DBLE(SECOND), RA, STATUS)
            IF (STATUS.NE.0) GOTO 20

         END IF
      GOTO 30
  
20    CONTINUE
      REFORMAT = .FALSE.
      LSTATUS = .FALSE.
30    CONTINUE

      IF (REFORMAT) THEN
         CALL SLA_DR2TF(1, RA, CSIGN, IHMSF )					! If degrees or separator not ' ', reformat
         WRITE(CVALNEW, '(I2.2,1X,I2.2,1X,I2.2,A,I1)' ) IHMSF(1), IHMSF(2), IHMSF(3), '.', IHMSF(4)
         CVALRA = CVALNEW
      END IF
 
      END
