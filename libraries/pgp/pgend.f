C*PGEND -- terminate PGPLOT
C%void cpgend(void);
C+
      SUBROUTINE PGEND
C
C Terminate PGPLOT, close and release any open graphics devices.
C If the call to PGEND is omitted, some or all of any open plots
C may be lost.
C
C Arguments: none
C--
C 22-Dec-1995 - revised to call PGCLOS for each open device.
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
