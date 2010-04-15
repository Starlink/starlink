C+
C                         D S A _ C A L C _ W I D T H
C
C  Routine name:
C     DSA_CALC_WIDTH
C
C  Function:
C     Calcualtes axis width values from an axis data array
C
C  Description:
C     This routine, a utility used by DSA_MAP_WIDTH, is used to
C     calculate axis width values on the assumption that the width of
C     a data element is the axis coverage from half-way between the
C     element and the one below, to half way between the element and
C     the one above.  That is, that the axis coverage is continuous
C     and non-overlapping.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_CALC_WIDTH (DATA,NX,NSECT,WIDTH,STATUS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) DATA      (Double array,ref) The axis data array.  Note that
C                   this is treated as a 2D array of NSECT cross-sections
C                   each NX elements long, as is the width array.
C     (>) NX        (Integer,ref) The length of each cross-section of the
C                   data and width arrays.
C     (>) NSECT     (Integer,ref) The number of cross-sections in the
C                   data and width arrays.
C     (<) WIDTH     (Double array,ref) The axis width data array to be
C                   calculated.
C     (!) STATUS    (Integer,ref) Status code.  If bad status is passed
C                   to it, this routine returns immediately.
C
C  External variables used:  None
C
C  External subroutines / functions used: None.
C
C  Prior requirements:
C     This routine is intended to be called from DSA_FILL_WIDTH, as part
C     of the processing performed by DSA_MAP_WIDTH.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  History:
C     29th Aug 1988.  Original version.  KS / AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_CALC_WIDTH (DATA,NX,NSECT,WIDTH,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NSECT, STATUS
      DOUBLE PRECISION DATA(NX,NSECT), WIDTH(NX,NSECT)
C
C     Local variables
C
      INTEGER IELM                   ! Loop index through each cross-section
      INTEGER ISECT                  ! Loop index through cross-sections
C
      DO ISECT=1,NSECT
         WIDTH(1,ISECT)=ABS(DATA(2,ISECT)-DATA(1,ISECT))
         DO IELM=2,NX-1
            WIDTH(IELM,ISECT)=
     :               0.5*ABS(DATA(IELM+1,ISECT)-DATA(IELM-1,ISECT))
         END DO
         WIDTH(NX,ISECT)=ABS(DATA(NX,ISECT)-DATA(NX-1,ISECT))
      END DO
C
      END
