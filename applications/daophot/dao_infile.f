**==infile.spg  processed by SPAG 4.54K  at 14:22 on  4 Oct 1996

************************************************************************
      SUBROUTINE INFILE(IFILE,FILE,IFLAG)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      INTEGER IFILE , IFLAG
C*** End of declarations inserted by SPAG
C
C=======================================================================
C
C VAX/VMS FORTRAN-specific subroutine to open a sequential disk data
C file for reading.
C
C Input arguments
C
C IFILE  is the logical unit number to be used.
C
C  FILE  is the filename.
C
C Output argument
C
C IFLAG  is an error flag: = 0 if no problem; = -1 if the file could
C        not be opened.
C
C=======================================================================
C
      CHARACTER*30 FILE
C
C-----------------------------------------------------------------------
C
      OPEN (IFILE,FILE=FILE,STATUS='OLD',ERR=100)
      IFLAG = 0
      RETURN                                             ! Normal return
C
C-----------------------------------------------------------------------
C
C Error
C
 100  CONTINUE
      IFLAG = -1
      RETURN
C
      END
