C*PGEND -- close all open graphics devices
C%void cpgend(void);
C+
      SUBROUTINE PGEND
C
C Close and release any open graphics devices. All devices must be
C closed by calling either PGCLOS (for each device) or PGEND before
C the program terminates. If a device is not closed properly, some
C or all of the graphical output may be lost.
C
C Arguments: none
C--
C 22-Dec-1995 [TJP] - revised to call PGCLOS for each open device.
C 25-Feb-1997 [TJP] - revised description.
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INTEGER I
C
      DO 10 I=1,PGMAXD
         IF (PGDEVS(I).EQ.1) THEN
            CALL PGSLCT(I)
            CALL PGCLOS
         END IF
 10   CONTINUE
      END
