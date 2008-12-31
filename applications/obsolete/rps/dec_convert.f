*+DEC_CONVERT      Converts a character angle to radians.
      SUBROUTINE DEC_CONVERT(CVALDEC, DEC, LSTATUS)

*  Type Declaration
      IMPLICIT NONE
 
*  Calling Arguments
      CHARACTER*(*) CVALDEC
      DOUBLE PRECISION DEC
      LOGICAL LSTATUS
      INTEGER FIND_FIRST
 
********************************************************************************
*  Method
*     Reads input as text.  A valid separator indicates not degrees.
*     Allowed: ':'  '/'  ','  ' '
*     The input can be degree,minute,second  or decimal degrees
*
*  History
*     1988 Jan		M Bush, RAL	1st version
*          May		M Ricketts ::	Modified to accept degrees also
*     1989 Feb		   ::		Name, args changed from INDEG2, etc...
*     1992 JUNE		M. DUESTERHAUS  REMOVE VAX RTL CALLS
*-******************************************************************************

*  Functions
      INTEGER MDH_ENDWORD

*  Common Parameters
      INCLUDE 'zpidata.inc'
 
*   Local variables:

      CHARACTER*1 SEP(4)/':', '/', ',', ' '/
      CHARACTER*1 CHARSEP
      INTEGER INDEX, SUBINDEX, START,N1,N2,N3, INDEX2, ISTAT, NCHAR
      INTEGER STATUS
      INTEGER DEG, MINUTE
      REAL    SECOND, DEC_SP
      DOUBLE PRECISION DEGREES
      LOGICAL NEGATIVE
      INTEGER IDMSF(4)
      CHARACTER*11 CVALNEW, CSIGN*1
      LOGICAL REFORMAT

*  Executable Code

      LSTATUS = .TRUE.
      NCHAR = MDH_ENDWORD(CVALDEC)
      START = 1
      DO WHILE (CVALDEC(START:START) .EQ. ' ' .AND. START .LT. NCHAR )
         START = START + 1
      END DO
      ISTAT = FIND_FIRST(CVALDEC(START:NCHAR),':/, ',  SUBINDEX)
      INDEX = ISTAT + START - 1							! Make index work from 1st char. of line

      IF (ISTAT.EQ.0) THEN							! Assume its degrees

            READ(CVALDEC,*,IOSTAT=ISTAT) DEGREES
            IF (ISTAT.NE.0) GOTO 20
            DEC = DEGREES * DDTOR
            REFORMAT = .TRUE.
 
         ELSE				! Its degree / minute / second in some form
 
            IF (CVALDEC(START:START) .EQ. '-' ) THEN
               NEGATIVE = .TRUE.
               START = START + 1
            ELSE
               NEGATIVE = .FALSE.
            END IF

            N1 = INDEX - 1
	    READ(CVALDEC(START:N1),'(I)')DEG
            CHARSEP = SEP(SUBINDEX)

c            IF (CHARSEP .NE. ' ' ) THEN
               REFORMAT = .TRUE.
c            ELSE
c               REFORMAT = .FALSE.
c            END IF

            START = INDEX+1
	    INDEX2 = FIND_FIRST(CVALDEC(START:),CHARSEP,SUBINDEX)
            IF (INDEX2.EQ.0) GOTO 20
 
            N2 = START + INDEX2 - 2
 
	    READ(CVALDEC(START:N2),*)MINUTE 

            N3 = N2 + 2
            IF (N3.GT.NCHAR) THEN
               SECOND = 0.0
            ELSE 
               READ(CVALDEC(N3:NCHAR), * , IOSTAT = STATUS) SECOND
               IF ( STATUS .NE. 0) GOTO 20
            END IF

            CALL SLA_CAF2R( IABS(DEG), MINUTE, SECOND, DEC_SP, STATUS)
            DEC = DEC_SP						! Jan 94 found sla routine has sp!

            IF (STATUS .NE.0) GOTO 20
            IF (NEGATIVE) DEC = -DEC					! set sign
         END IF								! End choice of decoding methods

         IF (DEC .LE. DPIBY2 .AND. DEC .GE. -DPIBY2 ) GOTO 30

20    CONTINUE
      LSTATUS = .FALSE.
      REFORMAT = .FALSE.
30    CONTINUE
      IF (REFORMAT ) THEN
         CALL SLA_DR2AF( 1, DEC, CSIGN, IDMSF)
         WRITE(CVALNEW,'(A1,I2.2,1X,I2.2,1X,I2.2,A,I1)' ) CSIGN, IDMSF(1), IDMSF(2), IDMSF(3),'.',IDMSF(4)
         CVALDEC = CVALNEW
      END IF

      END
