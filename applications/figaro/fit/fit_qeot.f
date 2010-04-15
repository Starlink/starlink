C+
      LOGICAL FUNCTION FIT_QEOT (STATUS)
C
C     F I T _ Q E O T
C
C     Determines whether a FIT_ package status code indicates that
C     the end of the tape has been reached.  This allows the user
C     to decide what action is to be taken when this happens.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) STATUS    (Integer) A status code returned by a FIT_ routine.
C
C     Returns -
C
C     (<) FIT_QEOT  (Logical) True if the end of tape has been reached,
C                   false otherwise.
C
C     Common variables used -  None.
C
C     Subroutines / functions used - None.
C
C     Modified 10/12/92 by BDC  /UNSW
C     Dummy up the function for a test of fits library
C     5/3/93 KS/AAO. Define the error code explicitly for portability.
C     1st April 1993. BDC/UNSW. INTEGER TIO_EONOTAPE typo error fixed.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
C
C     We define TIO__ENDOFTAPE here. This is a bit messy. The original
C     VAX-specific TIO routines did not intend anyone to test for specific
C     status values, and so no TIO error code include file was produced.
C     You were supposed to use routines such as TIO_EOF to check for
C     specific conditions. But there was no TIO_EOT test defined, and so
C     this routine had to make use of the fact that the VAX TIO implementation
C     returned VMS system status codes, and could include the SSDEF include
C     file and test against SS$_ENDOFTAPE. That would still work under VMS,
C     but under UNIX no such include file exists. However, the UNIX
C     implementation of TIO follows VMS status conventions, so it is still
C     possible to test for a STATUS value equal to whatever number VMS uses
C     for SS$_ENDOFTAPE. This happens to be 2168. So defining it explicitly
C     as such here produces code that works on all systems, without needing
C     special include files. It just looks a bit odd.
C
      INTEGER TIO__ENDOFTAPE
      PARAMETER (TIO__ENDOFTAPE = 2168)
C
C     Test for end of tape status
C
      FIT_QEOT = STATUS .EQ. TIO__ENDOFTAPE
C
      END

