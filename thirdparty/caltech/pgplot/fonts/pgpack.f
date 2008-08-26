      PROGRAM PACK
C-----------------------------------------------------------------------
C Convert unpacked (ASCII) representation of GRFONT into packed
C (binary) representation used by PGPLOT.
C
C This version ignores characters in the input file with Hershey
C numbers 1000-1999 ("indexical" fonts) and 3000-3999 ("triplex"
C and "gothic" fonts).
C
C The binary file contains one record, and is a direct copy of the
C internal data structure used in PGPLOT. The format of the internal
C data structure (and the binary file) are private to PGPLOT: i.e.,
C they may be changed in a future release.
C
C NC1	Integer*4	Smallest Hershey number defined in file (1)
C NC2	Integer*4	Largest Hershey number defined in file (3000)
C NC3	Integer*4	Number of words of buffer space used
C INDEX Integer*4 array (dimension 3000)
C			Element NC of INDEX contains either 0 if
C			NC is not a defined Hershey character, or the
C			index in array BUFFER at which the digitization
C                       of character number NC begins
C BUFFER Integer*2 array (dimension 27000)
C			Coordinate pairs defining each character are
C			packed two to a word in this array.
C
C Note: the array sizes are fixed by dimension statements in PGPLOT.
C New characters cannot be added if they would increase the size of
C the arrays.  Array INDEX is not very efficiently used as only about
C 1000 of the possible 3000 characters are defined.
C
C 25-Aug-2008 - g95 will not dump all elements from array into an file
C               without being told how many elements to dump [TimJ/JAC]
C-----------------------------------------------------------------------
      INTEGER MAXCHR, MAXBUF
      PARAMETER (MAXCHR=3000)
      PARAMETER (MAXBUF=27000)
C
      INTEGER   INDEX(MAXCHR)
      INTEGER*2 BUFFER(MAXBUF)
      INTEGER   I, LENGTH, LOC, NC, NC1, NC2, NCHAR, XYGRID(400)
C-----------------------------------------------------------------------
 1000 FORMAT (7(2X,2I4))
 2000 FORMAT (' Characters defined: ', I5/
     1        ' Array cells used:   ', I5)
 3000 FORMAT (' ++ERROR++ Buffer is too small: ',I7)
C-----------------------------------------------------------------------
C
C Initialize index.
C
      DO 1 I=1,MAXCHR
          INDEX(I) = 0
    1 CONTINUE
      LOC = 0
      NCHAR = 0
C
C Open stdin.
C
C Read input file.
C
   10 CONTINUE
C         -- read next character
          READ (*,1000,END=20) NC,LENGTH,(XYGRID(I),I=1,5)
          READ (*,1000) (XYGRID(I),I=6,LENGTH)
C         -- skip if Hershey number is outside required range
          IF (NC.LT.1 .OR. (NC.GT.999.AND.NC.LT.2000) .OR.
     1        NC.GT.2999) GOTO 10
C         -- store in index and buffer
          NCHAR = NCHAR+1
          LOC = LOC+1
          IF (LOC.GT.MAXBUF) GOTO 500
          INDEX(NC) = LOC
          BUFFER(LOC) = XYGRID(1)
          DO 15 I=2,LENGTH,2
              LOC = LOC + 1
              IF (LOC.GT.MAXBUF) GOTO 500
              BUFFER(LOC) = 128*(XYGRID(I)+64) + XYGRID(I+1) + 64
   15     CONTINUE
      GOTO 10
   20 CONTINUE
C
C Write output file.
C
      OPEN (UNIT=2, STATUS='NEW', FORM='UNFORMATTED', FILE='grfont.dat')
      NC1 = 1
      NC2 = 3000
      WRITE (2) NC1,NC2,LOC,(INDEX(I),I=1,MAXCHR),(BUFFER(I),I=1,MAXBUF)
      CLOSE (UNIT=2)
C
C Write summary.
C
      WRITE (6,2000) NCHAR, LOC
      STOP
C
C Error exit.
C
  500 WRITE (6,3000) MAXBUF
C-----------------------------------------------------------------------
      END
