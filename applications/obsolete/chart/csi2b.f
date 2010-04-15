      PROGRAM CSI2B

*+
*- - - - - -
* C S I 2 B
*- - - - - -
*
* A program which converts an ASCII version of the star catalog in
* CSI format, as output by CSI2A, back into it's original binary
* format.
*     CSI2A and this program, CSI2B, are intended to make possible the
* porting of CSI format star catalog files between systems, by
* converting the file into ASCII to make it portable, and back to binary
* on the target machine.
*
* The program will ask the user which machine it is running on, first,
* to be able to select the right record length for the RECL specifier
* of the OPEN statement, as the VAX expects it in longwords for
* unformatted files. It will then prompt the user for the pathnames
* for both the existing CSI ASCII file and the binary file which is
* to be output.
* The binary file is 14.9 Mbytes.
*
* It's all standard FORTRAN 77, and uses no external or internal
* subroutines.
*
* Author:
*       A J J Broderick, Starlink, 27 Jan 1993
*-

      IMPLICIT NONE

      INTEGER LEN, COUNT, IOSTAT, NHD, IRA, IDEC, START, NUMBER
      INTEGER*2 MB, MV, DMZ, DMN
      CHARACTER*2 SPEC, DM
      CHARACTER MACH, DMSIGN, FILE*50
      LOGICAL*1 DMSUPP, ID(8)

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
         LEN = 9
      ELSE
         LEN = 36
      ENDIF

      WRITE(*,*)
      WRITE(*,*) 'Pathname of CSI ASCII file ?'
      READ(*, '(A)') FILE

      OPEN(UNIT = 11, FILE = FILE, FORM = 'FORMATTED', STATUS = 'OLD')

      WRITE(*,*)
      WRITE(*,*) 'Pathname for CSI binary file ?'
      READ(*, '(A)') FILE

      OPEN(UNIT = 10, FILE = FILE, ACCESS = 'DIRECT', RECL = LEN,
     +     FORM = 'UNFORMATTED', STATUS = 'NEW')

      WRITE(*,*)
      WRITE(*,*) 'Processing . . .'

* Read formatted/write unformatted loop for first 200 records, which
* each contain 2 integers

      DO 200 COUNT = 1, 199
         READ(11, '(2I7)') START, NUMBER
         WRITE(10, REC = COUNT, IOSTAT = IOSTAT) START, NUMBER
 200  CONTINUE

      COUNT = 200

      READ(11, 1010, IOSTAT = IOSTAT) NHD, SPEC, MB, MV,
     +    IRA, IDEC, DM, DMSIGN, DMZ, DMN, DMSUPP, ID

* Read formatted/write unformatted loop for rest of file, until end of
* file or error occurs

 300  WRITE(10, REC = COUNT, IOSTAT = IOSTAT)  NHD, SPEC, MB, MV,
     +     IRA, IDEC, DM, DMSIGN, DMZ, DMN, DMSUPP, ID

      COUNT = COUNT + 1

      READ(11, 1010, IOSTAT = IOSTAT) NHD, SPEC, MB, MV,
     +    IRA, IDEC, DM, DMSIGN, DMZ, DMN, DMSUPP, ID
      IF ((MOD(COUNT, 10000)) .EQ. 0) WRITE(*,*) 'Done ', COUNT
      IF (IOSTAT .EQ. 0) GOTO 300
      WRITE(*,*) 'Finished !'

 1010 FORMAT(I10, A2, I5, I5, I10, I10, A2, A1, I4, I5, I4, 8I4)
      END



