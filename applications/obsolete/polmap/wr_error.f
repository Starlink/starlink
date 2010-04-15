      SUBROUTINE WR_ERROR(MSG,OUT_LU)
C+
C
C Subroutine:
C
C   W R _ E R R O R
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C MSG (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C
C This subroutine writes out the error message
C
C
C
C
C-
      CHARACTER*(*) MSG
      CHARACTER*75 ERROR
      INTEGER OUT_LU
C
      ERROR='! '//MSG
C
      WRITE(OUT_LU,'(1X,A)') ERROR
      END
