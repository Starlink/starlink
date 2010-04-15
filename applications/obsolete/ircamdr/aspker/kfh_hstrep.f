
*+  KFH_HSTREP - Produces a summary of a histogram.
      SUBROUTINE KFH_HSTREP(HIST,HRMIN,HRMAX,NUMBIN,STATUS)
*    Description :
*     This routine produces a summary of the histogram of
*     an image or a sub-section of an image. The histogram
*     is compressed to 16 bins and is displayed to the user.
*    Invocation :
*     CALL KFH_HSTREP(HIST,HRMIN,HRMAX,NUMBIN,STATUS)
*    Parameters :
*     HIST(NUMBIN) = INTEGER
*           The array holding the histogram.
*     HRMIN = REAL
*           The minimum value of the histogram.
*     HRMAX = REAL
*           The maximum value of the histogram.
*     NUMBIN = INTEGER
*           The number of bins in the histogram.
*     STATUS = INTEGER
*           The status value on entry to this
*           subroutine.
*    Method :
*     The size of the report bins is first calculated
*     by dividing the number of bins in the histogram
*     by 16. The histogram bins are then added together
*     to form the report bins. The summary is output to
*     the user.
*    Authors :
*     S.Chan (RGVAD::KFH)
*    History :
*     10 September 1983: Original (RGVAD::KFH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants:
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER NUMBIN                     ! The number of bins in the
*                                        ! histogram.
      REAL BINSZ                         ! The size of the bins in the
*                                        ! histogram report.
      INTEGER HIST(NUMBIN)               ! Array holding the histogram.
      REAL HRMIN                         ! Minimum value of the histogram.
      REAL HRMAX                         ! Maximum value of the histogram.
      INTEGER I                          ! General variable.
      INTEGER J                          ! General variable.
      INTEGER K                          ! General variable.
      REAL NBINWD                        ! Width of the report bins.
      INTEGER NHIST(16)                  ! Array holding the histogram
*                                        ! summary.
      REAL RANGE                         ! The difference between the
*                                        ! maximum and the minimum values.
      REAL SIZBIN                        ! Size of summary bins.
      REAL SBIN                          ! Size of summary bins used
*                                        ! in an incremental capacity.
      REAL SZBIN                         ! Count of the summary bins.
      CHARACTER*50 TEXT                  ! Text string containing the
*                                        ! summary of the histogram.
*-

*
*    If the status is bad, then return to the main program.
*

      IF (STATUS.NE.SAI__OK) THEN

         RETURN

      ELSE

*
*       Calculate the size of the bins for the report.
*

         SIZBIN = FLOAT(NUMBIN)/ 16
         SBIN = SIZBIN
*         SZBIN = SIZBIN+1

         IF (NUMBIN.LT.16) THEN

            SZBIN = SIZBIN+1

         ELSE

            SZBIN = SIZBIN

         ENDIF

*
*       Initialise the new compressed histogram.
*

         DO I = 1,16,1

            NHIST(I) = 0

         END DO

*
*       Compression mechanism.
*

         I = 1

         DO J = 1,NUMBIN,1

            IF (J.LE.INT(SZBIN)) THEN

               NHIST(I) = NHIST(I) + HIST(J)

            ELSE

               NHIST(I+1) = NHIST(I+1) + HIST(J)
               SZBIN = SZBIN + SBIN
               I = I + 1

            ENDIF

         END DO

*
*       Output results.
*

         CALL MSG_OUT('LINE',' ',STATUS)
         CALL MSG_OUT('TITLE','           Summary of histogram',
     :    STATUS)
         CALL MSG_OUT('LINE',' ', STATUS)

         RANGE = HRMAX-HRMIN+1
         BINSZ = RANGE/16
         NBINWD = BINSZ

         DO K = 1,16,1

            WRITE (TEXT,10),HRMIN+(K-1)*NBINWD,HRMIN+K*NBINWD,
     :       NHIST(K)
 10         FORMAT(' ',1PG15.7,'TO',1PG15.7,' ',I8,' PIXELS')
            CALL MSG_OUT('OUTLINE',TEXT,STATUS)

         END DO

         CALL MSG_OUT('LINE',' ',STATUS)

      ENDIF

      END
