C*PGSUBP -- subdivide view surface into panels
C%void cpgsubp(int nxsub, int nysub);
C+
      SUBROUTINE PGSUBP (NXSUB, NYSUB)
      INTEGER NXSUB, NYSUB
C
C PGPLOT divides the physical surface of the plotting device (screen,
C window, or sheet of paper) into NXSUB x NYSUB `panels'. When the 
C view surface is sub-divided in this way, PGPAGE moves to the next
C panel, not the next physical page. The initial subdivision of the
C view surface is set in the call to PGBEG. When PGSUBP is called,
C it forces the next call to PGPAGE to start a new physical page,
C subdivided in the manner indicated. No plotting should be done
C between a call of PGSUBP and a call of PGPAGE (or PGENV, which calls
C PGPAGE).
C
C If NXSUB > 0, PGPLOT uses the panels in row order; if <0, 
C PGPLOT uses them in column order, e.g.,
C      
C  NXSUB=3, NYSUB=2            NXSUB=-3, NYSUB=2   
C                                                
C +-----+-----+-----+         +-----+-----+-----+
C |  1  |  2  |  3  |         |  1  |  3  |  5  |
C +-----+-----+-----+         +-----+-----+-----+
C |  4  |  5  |  6  |         |  2  |  4  |  6  |
C +-----+-----+-----+         +-----+-----+-----+
C
C PGPLOT advances from one panels to the next when PGPAGE is called,
C clearing the screen or starting a new page when the last panel has
C been used. It is also possible to jump from one panel to another
C in random order by calling PGPANL.
C 
C Arguments:
C  NXSUB  (input)  : the number of subdivisions of the view surface in
C                    X (>0 or <0).
C  NYSUB  (input)  : the number of subdivisions of the view surface in
C                    Y (>0).
C--
C 15-Nov-1993 [TJP] - new routine.
C 19-Feb-1994 [TJP] - rescale viewport when panel size changes.
C 23-Sep-1996 [TJP] - correct bug in assignment of PGROWS.
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      REAL     CH, XFSZ, YFSZ
      LOGICAL  PGNOTO
      REAL     XVP1, XVP2, YVP1, YVP2

C
      IF (PGNOTO('PGSUBP')) RETURN
C
C Find current character size and viewport (NDC).
C
      CALL PGQCH(CH)
      CALL PGQVP(0, XVP1, XVP2, YVP1, YVP2)
C
C Set the subdivisions.
C
      XFSZ = PGNX(PGID)*PGXSZ(PGID)
      YFSZ = PGNY(PGID)*PGYSZ(PGID)
      PGROWS(PGID) = (NXSUB.GE.0)
      PGNX(PGID) = MAX(ABS(NXSUB),1)
      PGNY(PGID) = MAX(ABS(NYSUB),1)
      PGXSZ(PGID) = XFSZ/PGNX(PGID)
      PGYSZ(PGID) = YFSZ/PGNY(PGID)
C
C The current panel is the last on the physical page, to force
C a new physical page at next PGPAGE.
C
      PGNXC(PGID) = PGNX(PGID)
      PGNYC(PGID) = PGNY(PGID)
C
C Rescale the character size and viewport to the new panel size.
C
      CALL PGSCH(CH)
      CALL PGSVP(XVP1, XVP2, YVP1, YVP2)
C
      END
