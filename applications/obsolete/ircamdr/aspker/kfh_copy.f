*+  KFH_COPY - Makes a copy of a histogram.
      SUBROUTINE KFH_COPY(HIST,CHIST,NHIST)
*    Description :
*     This routine makes a copy of a histogram.
*    Invocation :
*     CALL KFH_COPY(HIST,CHIST,NHIST)
*    Parameters :
*     HIST(NHIST) = INTEGER
*           This is the histogram to be copied.
*     CHIST(NHIST) = REAL
*           This is the copy of the histogram.
*     NHIST = INTEGER
*           This is the size of the histogram.
*    Method :
*     The histogram is copied by setting up a loop
*     and equating the elements of the new histogram
*     with the elements of the old histogram.
*    Author :
*     S.Chan (RGVAD::KFH)
*    History :
*     28 September 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Local variables :
      INTEGER NHIST                      ! Size of the histogram.
      REAL CHIST(NHIST)                  ! Array holding the copy of the
*                                        ! histogram.
      INTEGER HIST(NHIST)                ! The histogram to be copied.
      INTEGER I                          ! General variable.

*
*    This routine takes data in from an image and
*    stores it in an array.
*

      DO I = 1,NHIST

         CHIST(I) = FLOAT(HIST(I))

      END DO

      END
