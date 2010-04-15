      PROGRAM AST2A
*+
*- - - - - -
* A S T 2 A
*- - - - - -
*
* A program to convert star catalogs in ASTROM format from binary to
* ASCII format, to enable them to be ported from one system to another.
* There is, as you might expect, an equal and opposite program which
* converts the ASCII format catalog back to binary format once it has
* been ported, called AST2B.
*
* The program will ask the user which machine it is running on, first,
* to be able to select the right record length for the RECL specifier
* of the OPEN statement, as the VAX expects it in longwords for
* unformatted files. It will then prompt the user for the pathnames
* for both the existing ASTROM binary file and the text file which is
* to be output.
* Warning: the text files get pretty big - about 15 Mbytes.
*
* It's all standard FORTRAN 77, and uses no external or internal
* subroutines.
*
* Author:
* 	A J J Broderick, Starlink, 27 Jan 1993
*-
*

      INTEGER LEN, START, NUMBER, IOSTAT, NCAT, IPA, IPD, IRA,
     +        IDEC, COUNT
      LOGICAL*1 MAG
      CHARACTER MACH, SPEC*2, FILE*50

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
      WRITE(*,*) 'Pathname of ASTROM binary file ?'
      READ(*, '(A)') FILE

      OPEN(UNIT = 10, FILE = FILE, ACCESS = 'DIRECT', RECL = LEN,
     :      FORM = 'UNFORMATTED', STATUS = 'OLD',
     +      IOSTAT = IOSTAT)

      WRITE(*,*)
      WRITE(*,*) 'Pathname for ASTROM text file ?'
      READ(*, '(A)') FILE

      OPEN(UNIT = 11, FILE = FILE, FORM = 'FORMATTED', STATUS = 'NEW',
     +     IOSTAT = IOSTAT)

      WRITE(*,*)
      WRITE(*,*) 'Processing . . .'

* Read unformatted/write formatted loop for first 200 records, which
* each contain 2 integers

      DO 200 COUNT = 1, 199
         READ(10, REC = COUNT) START, NUMBER
         WRITE(11, '(2I7)') START, NUMBER
 200  CONTINUE

* Read unformatted/write formatted rest of file (until end of file
* or error)

      COUNT = 200
      READ(10, REC = COUNT, IOSTAT = IOSTAT) NCAT, MAG, SPEC, IPA,
     +        IPD, IRA, IDEC

 300  WRITE(11, 1010, IOSTAT = IOSTAT) NCAT, MAG, SPEC,
     +        IPA, IPD, IRA, IDEC
      COUNT = COUNT + 1
      READ(10, REC = COUNT, IOSTAT = IOSTAT) NCAT, MAG, SPEC, IPA,
     +        IPD, IRA, IDEC
      IF (MOD(COUNT, 10000) .EQ. 0)  WRITE(*,*) 'Done ', COUNT
      IF (IOSTAT .EQ. 0) GOTO 300
      WRITE(*,*) 'Finished !'

 1010 FORMAT(I10, I4, A2, I8, I6, I8, I10)
      END



