C+
      SUBROUTINE FIT_NOTERM
C
C     F I T _ N O T E R M
C
C     Normally, when FIT_CLOSE is called and a tape is being used for the
C     output, it writes two final file marks after the data and then
C     backspaces over the second of the two. This leaves the tape properly
C     terminated, but positioned ready to write the next FITS image.
C     However, on some devices - such as exabyte tapes - the backspacing
C     over the second file mark is slow and unnecessary if a further
C     file is to be written immediately. For such devices, and under such
C     conditions, a call to FIT_NOTERM before the call to FIT_CLOSE will
C     suppress the writing of and backspacing back over the second file
C     mark. This can speed things up under some circumstances - writing
C     many small files to an exabyte, for example - but risks leaving
C     a tape not properly terminated. This should normally only be done
C     when a sequence of images is being written automatically, and should
C     definitely not be done for the last of such a sequence of images.
C
C     Parameters - None.
C
C     Common variables used -
C
C     (!) NOTERM    (Logical) If set, suppresses proper termination of tape.
C
C                   Defined in the file COMF.INC.
C
C     Subroutines / functions used - None.
C
C
C                                       KS / AAO  5th March 1993.
C+
      IMPLICIT NONE
C
C     Common blocks
C
      INCLUDE 'COMF'
C
C     Set 'suppress termination' flag in common where FIT_CLOSE will
C     pick it up.
C
      NOTERM = .TRUE.
C
      END
