C*PGCLOS -- close the selected graphics device
C%void cpgclos(void);
C+
      SUBROUTINE PGCLOS
C
C Close the currently selected graphics device. After the device has
C been closed, either another open device must be selected with PGSLCT
C or another device must be opened with PGOPEN before any further
C plotting can be done. If the call to PGCLOS is omitted, some or all 
C of the plot may be lost.
C
C [This routine was added to PGPLOT in Version 5.1.0. Older programs
C use PGEND instead.]
C
C Arguments: none
C--
C 22-Dec-1995 - new routine, derived from the old PGEND.
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      CHARACTER*16 DEFSTR
      LOGICAL PGNOTO
C
      IF (.NOT.PGNOTO('PGCLOS')) THEN
         CALL GRTERM
         IF (PGPRMP(PGID)) THEN
            CALL GRQCAP(DEFSTR)
            IF (DEFSTR(8:8).EQ.'V') CALL GRPROM
         END IF
         CALL GRCLOS
         PGDEVS(PGID) = 0
         PGID = 0
      END IF
C     WRITE (*,*) 'PGCLOS', PGID, ':', PGDEVS
      END
