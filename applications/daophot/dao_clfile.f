**==clfile.spg  processed by SPAG 4.54K  at 14:22 on  4 Oct 1996

************************************************************************
      SUBROUTINE CLFILE(IFILE)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      INTEGER IFILE
C*** End of declarations inserted by SPAG
C
C=======================================================================
C
C VAX/VMS FORTRAN-specific subroutine to close a sequential disk data
C file.
C
C Input argument
C
C IFILE  is the logical unit number.
C
C=======================================================================
C
!
! CLOSE has a name clash with a routine in VAXCRTL
! Use a FORTRAN CLOSE instead.
!      CALL CLOSE (IFILE)
!
      CLOSE (IFILE)
      RETURN
C
      END
