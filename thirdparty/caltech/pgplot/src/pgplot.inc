C-----------------------------------------------------------------------
C PGPLOT: common block definition.
C-----------------------------------------------------------------------
C Maximum number of concurrent devices (should match GRIMAX).
C-----------------------------------------------------------------------
      INTEGER PGMAXD
      PARAMETER (PGMAXD=8)
C-----------------------------------------------------------------------
C Indentifier of currently selected device.
C-----------------------------------------------------------------------
      INTEGER PGID
C-----------------------------------------------------------------------
C Device status (indexed by device identifier).
C-----------------------------------------------------------------------
C PGDEVS  =0 if device is not open; 1 if device is open.
C PGADVS  Set to 0 by PGBEGIN, set to 1 by PGPAGE; used to suppress
C         the prompt for the first page.
C PROMPT  If .TRUE., ask user before clearing page; set by PGASK
C         and (indirectly) by PGBEGIN, used in PGENV.
C PGBLEV  Buffering level: incremented by PGBBUF, decremented by
C         PGEBUF.
C PGPFIX  TRUE if PGPAP has been called, FALSE otherwise.
C
      INTEGER PGDEVS(PGMAXD), PGADVS(PGMAXD), PGBLEV(PGMAXD)
      LOGICAL PGPRMP(PGMAXD), PGPFIX(PGMAXD)
C-----------------------------------------------------------------------
C Panel parameters (indexed by device identification).
C-----------------------------------------------------------------------
C NX      Number of panels in x direction
C NY      Number of panels in y direction
C NXC     Ordinal number of current X panel
C NYC     Ordinal number of current Y panel
C XSZ     X dimension of panel (device units)
C YSZ     Y dimension of panel (device units)
C PGROWS  TRUE if panels are used in row order, FALSE for column
C         order.
C
      INTEGER PGNX  (PGMAXD), PGNY  (PGMAXD)
      INTEGER PGNXC (PGMAXD), PGNYC (PGMAXD)
      REAL    PGXSZ (PGMAXD), PGYSZ (PGMAXD)
      LOGICAL PGROWS(PGMAXD)
C-----------------------------------------------------------------------
C Attributes (indexed by device identification).
C-----------------------------------------------------------------------
C PGCLP   clipping enabled/disabed
C PGFAS   fill-area style
C PGCHSZ  character height
C PGAHS   arrow-head fill style
C PGAHA   arrow-head angle
C PGAHV   arrow-head vent
C PGTBCI  text background color index
C PGMNCI  lower range of color indices available to PGGRAY/PGIMAG
C PGMXCI  upper range of color indices available to PGGRAY/PGIMAG
C PGITF   type of transfer function used by PGGRAY/PGIMAG
C PGHSA   hatching line angle
C PGHSS   hatching line separation
C PGHSP   hatching line phase
C
      INTEGER PGCLP (PGMAXD)
      INTEGER PGFAS (PGMAXD)
      REAL    PGCHSZ(PGMAXD)
      INTEGER PGAHS (PGMAXD)
      REAL    PGAHA (PGMAXD)
      REAL    PGAHV (PGMAXD)
      INTEGER PGTBCI(PGMAXD)
      INTEGER PGMNCI(PGMAXD)
      INTEGER PGMXCI(PGMAXD)
      INTEGER PGITF (PGMAXD)
      REAL    PGHSA (PGMAXD)
      REAL    PGHSS (PGMAXD)
      REAL    PGHSP (PGMAXD)
C-----------------------------------------------------------------------
C Viewport parameters (indexed by device identification); all are device
C coordinates:
C-----------------------------------------------------------------------
C PGXOFF  X coordinate of blc of viewport.
C PGYOFF  Y coordinate of blc of viewport.
C PGXVP   X coordinate of blc of viewport, relative to blc of subpage.
C PGYVP   Y coordinate of blc of viewport, relative to blc of subpage.
C PGXLEN  Width of viewport. 
C PGYLEN  Height of viewport.
C
      REAL   PGXOFF(PGMAXD), PGYOFF(PGMAXD)
      REAL   PGXVP (PGMAXD), PGYVP (PGMAXD)
      REAL   PGXLEN(PGMAXD), PGYLEN(PGMAXD)
C-----------------------------------------------------------------------
C Scaling parameters (indexed by device identification):
C-----------------------------------------------------------------------
C PGXORG  device coordinate value corresponding to world X=0
C PGYORG  device coordinate value corresponding to world Y=0
C PGXSCL  scale in x (device units per world coordinate unit)
C PGYSCL  scale in y (device units per world coordinate unit)
C PGXPIN  device x scale in device units/inch
C PGYPIN  device y scale in device units/inch
C PGXSP   Character X spacing (device units)
C PGYSP   Character Y spacing (device units)
C
      REAL   PGXORG(PGMAXD), PGYORG(PGMAXD)
      REAL   PGXSCL(PGMAXD), PGYSCL(PGMAXD)
      REAL   PGXPIN(PGMAXD), PGYPIN(PGMAXD)
      REAL   PGXSP (PGMAXD), PGYSP (PGMAXD)
C-----------------------------------------------------------------------
C Window parameters (indexed by device identification); all are world
C coordinate values:
C-----------------------------------------------------------------------
C PGXBLC  world X at bottom left corner of window
C PGXTRC  world X at top right corner of window
C PGYBLC  world Y at bottom left corner of window
C PGYTRC  world Y at top right corner of window
C
      REAL    PGXBLC(PGMAXD), PGXTRC(PGMAXD)
      REAL    PGYBLC(PGMAXD), PGYTRC(PGMAXD)
C-----------------------------------------------------------------------
C The following parameters are used in the contouring routines to pass
C information to the action routine. They do not need to be indexed.
C-----------------------------------------------------------------------
C TRANS   Transformation matrix for contour plots; copied
C         from argument list by PGCONT and used by PGCP.
C
      INTEGER PGCINT, PGCMIN
      REAL    TRANS(6)
      CHARACTER*32 PGCLAB
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      COMMON  /PGPLT1/ PGID,PGDEVS,PGADVS,PGNX,  PGNY,  PGNXC, PGNYC ,
     1        PGXPIN,PGYPIN,PGXSP, PGYSP, PGXSZ, PGYSZ,
     2        PGXOFF,PGYOFF,PGXVP, PGYVP, PGXLEN,PGYLEN,PGXORG,PGYORG,
     3        PGXSCL,PGYSCL,PGXBLC,PGXTRC,PGYBLC,PGYTRC,TRANS,
     4        PGPRMP,PGCLP, PGFAS, PGCHSZ,PGBLEV,PGROWS,
     5        PGAHS, PGAHA, PGAHV, PGTBCI,PGMNCI,PGMXCI,PGCINT,PGCMIN,
     6        PGPFIX,PGITF, PGHSA, PGHSS, PGHSP
      COMMON  /PGPLT2/ PGCLAB
      SAVE    /PGPLT1/
      SAVE    /PGPLT2/
C-----------------------------------------------------------------------
