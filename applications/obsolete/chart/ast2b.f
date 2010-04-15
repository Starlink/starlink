      PROGRAM AST2B

*+
*- - - - - -
* A S T 2 B
*- - - - - -
*
* A program which converts an ASCII version of the star catalog in
* ASTROM format, as output by AST2A, back into it's original binary
* format.
*     AST2A and this program, AST2B, are intended to make possible the
* porting of ASTROM format star catalog files between systems, by
* converting the file into ASCII to make it portable, and back to binary
* on the target machine.
*
* The program will ask the user which machine it is running on, first,
* to be able to select the right record length for the RECL specifier
* of the OPEN statement, as the VAX expects it in longwords for
* unformatted files. It will then prompt the user for the pathnames
* for both the existing ASTROM ASCII file and the binary file which is
* to be output.
*
* It's all standard FORTRAN 77, and uses no external or internal
* subroutines.
*
* Author:
*       A J J Broderick, Starlink, 27 Jan 1993
*-



      INTEGER LEN, START, NUMBER, IOSTAT, NCAT, IPA, IPD, IRA,
     +        IDEC, COUNT

      CHARACTER MACH, SPEC*2, FILE*50
      LOGICAL*1 MAG

*  Read and validate choice of hardware
      MACH = 'A'
 100  CONTINUE
      WRITE(*,*) 'What machine is this running on ? Enter V for any'//
     +           ' DEC kit, O for other.'
      READ(*, '(A)') MACH
      IF ((MACH .NE. 'O') .AND. (MACH .NE. 'o') .AND. (MACH .NE. 'V')
     +         .AND. (MACH .NE. 'v')) THEN
         GOTO 100
      ENDIF

*  Set record length in longwords for DEC hardware or bytes for other
      IF ((MACH .EQ. 'V') .OR. (MACH .EQ. 'v')) THEN
         LEN = 6
      ELSE
         LEN = 24
      ENDIF

      WRITE(*,*)
      WRITE(*,*) 'Pathname of ASTROM ASCII text file ?'
      READ(*, '(A)') FILE

      OPEN(UNIT = 11, FILE = FILE, FORM = 'FORMATTED', STATUS = 'OLD')

      WRITE(*,*)
      WRITE(*,*) 'Pathname for ASTROM binary file ?'
      READ(*, '(A)') FILE

      OPEN(UNIT = 10, FILE = FILE, ACCESS = 'DIRECT', RECL = LEN,
     +      FORM = 'UNFORMATTED', STATUS = 'NEW')

      WRITE(*,*)
      WRITE(*,*) 'Processing . . .'

* Read formatted/write unformatted loop for first 200 records, which
* each contain 2 integers

      DO 200 COUNT = 1, 199
         READ(11, '(2I7)') START, NUMBER
         WRITE(10, REC = COUNT, IOSTAT = IOSTAT) START, NUMBER
 200  CONTINUE

* Read formatted/write unformatted loop for rest of file, until end of
* file or error occurs

      COUNT = 200
      READ(11, 1010, IOSTAT = IOSTAT) NCAT, MAG, SPEC,
     +        IPA, IPD, IRA, IDEC

 300  WRITE(10, REC = COUNT, IOSTAT = IOSTAT) NCAT, MAG, SPEC, IPA,
     +        IPD, IRA, IDEC
      COUNT = COUNT + 1
      READ(11, 1010, IOSTAT = IOSTAT) NCAT, MAG, SPEC,
     +        IPA, IPD, IRA, IDEC
      IF ((MOD(COUNT, 10000)) .EQ. 0) WRITE(*,*) 'Done ', COUNT
      IF (IOSTAT .EQ. 0) GOTO 300
      WRITE(*,*) 'Finished !'

 1010 FORMAT(I10, I4, A2, I8, I6, I8, I10)
      END



