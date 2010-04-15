**==delpic.spg  processed by SPAG 4.54K  at 14:22 on  4 Oct 1996

************************************************************************
      SUBROUTINE DELPIC(FILE,IFLAG)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      INTEGER IFLAG
C*** End of declarations inserted by SPAG
C
C=======================================================================
C
C Simple subroutine to delete a disk picture file.  File must be closed
C before DELEPIC is called.
C
C Arguments
C
C  FILE (INPUT) is the disk filename of the file to be deleted.
C
C IFLAG (OUTPUT) is an error flag.  If all goes well IFLAG = 0,
C       otherwise not.
C
C=======================================================================
C
      CHARACTER*30 FILE
C
C-----------------------------------------------------------------------
C
      IFLAG = 0
      OPEN (9,FILE=FILE,STATUS='OLD',ERR=100)
      CLOSE (9,STATUS='DELETE',ERR=100)
      RETURN                                             ! Normal return
C
C-----------------------------------------------------------------------
C
C Irrecoverable error.
C
 100  CONTINUE
      IFLAG = 1
      RETURN
C
      END
