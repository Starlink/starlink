      PROGRAM NSO2A
*+
*- - - - - -
* N S O 2 A
*- - - - - -
*
* A program to convert star catalogs in NSO format from binary to
* ASCII format, to enable them to be ported from one system to another.
* There is, as you might expect, an equal and opposite program which
* converts the ASCII format catalog back to binary format once it has
* been ported, called NSO2B.
*
* The program will ask the user which machine it is running on, first,
* to be able to select the right record length for the RECL specifier
* of the OPEN statement, as the VAX expects it in longwords for
* unformatted files. It will then prompt the user for the pathnames
* for both the existing ASTROM binary file and the text file which is
* to be output.
* Warning: the text files get pretty big - about 12 Mbytes.
*
* It's all standard FORTRAN 77, and uses no external or internal
* subroutines.
*
* Author:
* 	A J J Broderick, Starlink, 27 Jan 1993
*-

      IMPLICIT NONE

      INTEGER COUNT, LEN, START, NUMBER, IOSTAT
      CHARACTER MACH, NAM*14, DES*24, FILE*30
      INTEGER*2 LDIAM, I2MAG
      REAL SRA, SDEC

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
         LEN = 13
      ELSE
         LEN = 52
      ENDIF

      WRITE(*,*)
      WRITE(*,*) 'Pathname of NSO binary file ?'
      READ(*, '(A)') FILE

      OPEN(UNIT = 10, FILE = FILE, ACCESS = 'DIRECT', RECL = LEN,
     :      FORM = 'UNFORMATTED', STATUS = 'OLD')

      WRITE(*,*)
      WRITE(*,*) 'Pathname for NSO ASCII file ?'
      READ(*, '(A)') FILE

      OPEN(UNIT = 11, FILE = FILE,
     +     FORM = 'FORMATTED', STATUS = 'NEW')

      WRITE(*,*)
      WRITE(*,*) 'Processing . . .'

* Read unformatted/write formatted loop for first 200 records, which
* each contain 2 integers

      DO 200 COUNT = 1, 204
         READ(10, REC = COUNT) START, NUMBER
         WRITE(11, '(2I7)') START, NUMBER
 200  CONTINUE

* Read unformatted/write formatted rest of file (until EOF)

      COUNT = 205
      READ(10, REC = COUNT, IOSTAT = IOSTAT) NAM, SRA, SDEC, LDIAM,
     +         I2MAG, DES

* Read formatted/write unformatted loop for rest of file, until end of
* file or error occurs

 300  WRITE(11, 1010, IOSTAT = IOSTAT)  NAM, SRA, SDEC,
     +      LDIAM, I2MAG, DES
      COUNT = COUNT + 1
      READ(10, REC = COUNT, IOSTAT = IOSTAT) NAM, SRA, SDEC, LDIAM,
     +         I2MAG, DES
      IF (MOD(COUNT, 10000) .EQ. 0)  WRITE(*,*) 'Done ', COUNT
      IF (IOSTAT .EQ. 0) GOTO 300
      WRITE(*,*) 'Finished !'

 1010 FORMAT(A14, G30.20, G30.20, I6, I6, A24)
      END




