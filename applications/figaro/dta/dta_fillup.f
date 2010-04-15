C+
      SUBROUTINE DTA_FILLUP (ITYPE,NITEM,BUFFER)
C
C     D T A _ F I L L U P
C
C     Sets a numeric array of any type to zeros.
C
C     Parameters  (">" input, "<" output)
C
C     (>) ITYPE    (Integer) The DTA type code for the data (TYP_xxxx)
C     (>) NITEM    (Integer) The number of elements in buffer.
C     (<) BUFFER   (Numeric array, any type) The array to be zeroed.
C
C     Common variables used - None
C
C     Subroutines / functions used -  None
C
C                                       KS / AAO 20th March 1986
C     Modified:
C
C     10th Jan  1992.  KS / AAO.  Syntax of include statements changed to
C                      remove VMS logical names and to use lower case, to
C                      enable compilation on a SUN.
C     25th Jul  1996.  MJCL/Starlink, UCL. Moved DTATYPES.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER ITYPE,NITEM
      BYTE BUFFER(*)
C
C     DTA system type definitions and codes.  Used are
C
C     TSIZE     (Array) Sizes in bytes of elements of different types
C
      INCLUDE 'DTATCON'
C
C     Local variables
C
      INTEGER BYTES,I
C
C     DTATYPES contains DATA statements
C
      INCLUDE 'DTATYPES'
C
C     Calculate size in bytes of BUFFER and set it to zero
C
      BYTES=NITEM*TSIZE(ITYPE)
      DO I=1,BYTES
         BUFFER(I)=0
      END DO
C
      END

