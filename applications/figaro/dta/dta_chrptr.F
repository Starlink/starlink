C+
      SUBROUTINE DTA_CHRPTR
C
C     D T A _ C H R P T R
C
C     On a VAX, this is a routine that, given a string, returns a pointer
C     to the start of the actual characters in the string.  This UNIX version
C     is a dummy, since the function it performs is not needed. For more
C     details, see the listing for DTA_MVAR, which is the only routine that
C     uses it.  (It's arguably easier to produce a system-specific version
C     of the routine - in which case a make file can handle it properly, than
C     to have to use a preprocessor to prevent DTA_MVAR calling the routine
C     under UNIX.)
C
C     Parameters -  (">" input, "<" output, 'I' ignored)
C
C     (I) STRING    (Address of an array of bytes) The string.
C     (I) POINTER   (Integer) The address of the first character.
C
C     Common variables used - None
C
C     Functions / subroutines used - None
C
C                                       28th March 1986  KS / AAO
C     Modified:
C
C     20th Jan 1992.  KS / AAO.  This UNIX version produced.
C     12th Feb 1992.  KS / AAO.  Name changed (Z added) to emphasise system-
C                     dependent nature of routine.
C     18th Aug 1992.  HME / UoE. Changed back to DTA_CHRPTR. The platform
C                     dependence is expressed in having several source files
C                     dta_chrptr.f-<mach> and a soft link dta_chrptr.f to
C                     the correct one. The VAX biuld.com will compile
C                     DTA_CHRPTR.F-VAX.
C      1st Jul 2004   AA / Exeter, Starlink. Generic routine, will be run
C                     through the fortran pre-processor before compiling
C+
      END
