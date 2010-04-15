*+H_GET            Asks query, returns a char input/default from terminal
      SUBROUTINE H_GET( QUERY , FORMAT , DEFAULT , REPLY, STATUS)
      IMPLICIT NONE

*  Calling Arguments
      CHARACTER*(*) DEFAULT 	! In	Default answer.
     & ,            FORMAT      ! 	Format of answer.
     & ,            QUERY       ! 	Question.
      CHARACTER*(*) REPLY	! Out	Returned string
      INTEGER STATUS		! 	Status, 0: OK, 1: EOF, 2: Need Help

**********************************************************************************
*  History
*     1987 July		M.D.C.Harris, RAL	1st vsn MDH_GET
*     1989 Jan		M Ricketts		Mod to pass Help signal, subr'ne
**********************************************************************************

*  Local Variables
      CHARACTER*60  CDEF, CLOCAL        ! Character default, value
      INTEGER       ILOCAL        ! Integer 	::
      INTEGER	    NBLANKS, QST, QEND, I
      LOGICAL       LLOCAL        ! Logical 	::
     & ,            OKAY        	! Error indicator.
     & ,	    DEFIN		! True if default given
      DOUBLE PRECISION DLOCAL        ! Double precision   ::

*  Functions
      INTEGER MDH_CTOI
      LOGICAL MDH_CTOL
      DOUBLE PRECISION MDH_CTOD
      INTEGER MDH_ENDWORD

*  __________________________ Executable Code _____________________________

      CALL MDH_CHECK( DEFAULT , FORMAT , CDEF , DEFIN )                                ! Check and reformat it.

      QST = 1
      DO WHILE ( QUERY(QST:QST) .EQ. '/' )					! Convert initial '/'s to blank lines
         WRITE(*,*)
         QST = QST + 1
      END DO
      QEND = MDH_ENDWORD( QUERY )						! Convert end '/'s to line count
      NBLANKS = 0
      DO WHILE (QUERY(QEND:QEND) .EQ. '/' )
         QEND = QEND - 1
         NBLANKS = NBLANKS + 1
      END DO

      IF ( INDEX( FORMAT , 'I' ) .NE. 0 ) THEN
         IF (DEFIN) THEN
            CALL H_GETI( QUERY(QST:QEND), .TRUE., MDH_CTOI(CDEF), ILOCAL, STATUS)
         ELSE
            CALL H_GETI(QUERY(QST:QEND), .FALSE., ' ',            ILOCAL, STATUS)
         END IF

         IF (STATUS.NE.0) GOTO 90
         WRITE( REPLY , '(' // FORMAT // ')' ) ILOCAL

      ELSE IF ( INDEX( FORMAT , 'L' ) .NE. 0 ) THEN
         IF (DEFIN) THEN
            CALL H_GETL( QUERY(QST:QEND) , .TRUE., MDH_CTOL( CDEF ) ,LLOCAL, STATUS)
         ELSE
            CALL H_GETL( QUERY(QST:QEND) , .FALSE., ' ',             LLOCAL, STATUS)
         END IF
         IF (STATUS.NE.0) GOTO 90
         IF (LLOCAL) THEN
           REPLY = 'Y'
         ELSE
           REPLY = 'N'
         END IF
C         WRITE( REPLY , '(' // FORMAT // ')' ) LLOCAL

      ELSE IF ( index( format , 'F' ) .ne. 0 .or.
     &            INDEX( FORMAT , 'D' ) .NE. 0 .AND.
     &            INDEX( FORMAT , 'DD' ) .EQ. 0 ) THEN
         IF (DEFIN) THEN
            CALL H_GETD( QUERY(QST:QEND) , .TRUE., MDH_CTOD( CDEF ) , DLOCAL, STATUS)
         ELSE
            CALL H_GETD( QUERY(QST:QEND) , .FALSE., ' ',              DLOCAL, STATUS)
         END IF
         IF (STATUS.NE.0) GOTO 90
         WRITE( REPLY , '(' // FORMAT // ')' ) DLOCAL

      ELSE									!  Transfer input from called function to return -
         OKAY = .FALSE.
         DO WHILE ( .NOT. OKAY )
            IF (DEFIN) THEN
               CALL H_GETC(QUERY(QST:QEND), CDEF, CLOCAL, STATUS)
            ELSE
               CALL H_GETC(QUERY(QST:QEND), ' ', CLOCAL, STATUS)
            END IF
            IF (STATUS.NE.0) GOTO 90
            CALL MDH_CHECK( CLOCAL, FORMAT , REPLY, OKAY )			! Check and reformat it.
         END DO
      END IF

90    CONTINUE
      DO I=1, NBLANKS
         WRITE(*,*)
      END DO

      END
