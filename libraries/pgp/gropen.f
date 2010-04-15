      INTEGER FUNCTION GROPEN(ITYPE,IDUMMY,FILE,ID)
*+
*     - - - - - - - -
*       G R O P E N   (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Opens a device for plotting:
*
*   if this is the first call to GROPEN then
*      The font database is initialized
*      All GRPCKG devices are marked as closed
*
*    A free GRPCKG device id is selected
*
*    If GKS is not open the workstation is opened
*
*    Aspect source flags are set
*    Line and marker attributes are set
*    Deferral and update modes set
*    The workstation transformation is set to the entire display surface
*    Default colour table is set up
*
*
*   Given
*      ITYPE    i     Default device type (not used)
*      IDUMMY   i     Not used
*      FILE     c     Device name
*
*   Returned
*      ID       i     Device identifier
*      func val i     Status (1=>success)
*
*   Read from COMMON
*      GRDVOP   i()   Device open flag
*
*   Written to COMMON
*      GRCIDE   i     Current device id
*      GRDVOP   l()   Device open flag
*      GRXSCL   r()   X scale
*      GRYSCL   r()   Y scale
*      GRXORG   r()   X origin
*      GRYORG   r()   Y origin
*      GRXPRE   r()   Current point X
*      GRYPRE   r()   Current point Y
*      GRBPT    i     Line buffer pointer
*      GRSTYL   i()   Line style
*      GRWIDT   i()   Line width
*      GRCCOL   i()   Line colour
*      GRIPAT   i()   Line pattern segement number
*      GRPOFF   r()   Line pattern offset
*      GRFAST   r()   Fill area style
*      GRCFAC   r()   User character scale factor
*      GRCFNT   i()   Font
*      GRCSCL   i()   Character scale factor
*      GRTYP    i()   Workstation type
*      GRWKID   i()   Workstation id
*      GRVIEW   r()   Workstatation viewport
*      GRWIND   r()   Workstation window
*      GRXMAX   r()   Workstation size (x)
*      GRYMAX   r()   Workstation size (y)
*      GRXMIN   r()   Workstation origin (x)
*      GRYMIN   r()   Workstation origin (y)
*      GRXPIN   r()   Resolution (x)
*      GRYPIN   r()   Resolution (y)
*      GRGRLO   l()   Grey scale loaded flag
*      GRWSOP   l()   W/S opened by GRPCKG
*      GRVPVI   l()   Viewport visible
*
*   Constants from GRECOM
*      MAXDEV   i     Maximum number of open GRPCKG devices
*      TRN      i     Transformation used by GRPCKG
*
*   Constants from GKS_PAR
*      GGKCL    i     GKS closed
*      GACTIV   i     Workstation active
*      GINDIV   i     ASF - individual
*      GCOLOR   i     Device has colour
*      GBNIG    i     Before next interaction globally
*      GSUPPD   i     Implicit regeneration suppressed
*      GMO      i     Workstation catagory - Metafile output
*      GREALI   i     Realized value
*      GIRG     i     Implicit regeneration necessary
*      GEMPTY   I     Display surface empty
*      GSOLID   i     Interior style solid
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE

      INTEGER ITYPE,ID,IDUMMY
      CHARACTER*(*) FILE

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'

      INCLUDE 'GKS_PAR'


      INTEGER ISTAT, I, J, ASF(13), ICON, IERR, ITUS, IOSTAT, OLDID
      INTEGER IDEFM, IREGM, IEMPTY, NFRAME, ISTATE, ITRAN
      INTEGER NCOLA, NCOLI, NPCI, MPLBTE, MPMBTE, MTXBTE, MFABTE
      INTEGER IPLBUN, IPMBUN, ITXBUN, IFABUN, IPAREP, ICOLRE, IWKTR
      INTEGER MPAI, MCOLI, LX, LY, IUNIT, ICOL
      REAL AR, XM, YM, RWINDO(4), CWINDO(4), RVIEWP(4), CVIEWP(4)
      REAL C0(3), C1(3), C3(3), XNDC2P, YNDC2P

*   Default colours
      REAL R(2:15), G(2:15), B(2:15)
      DATA R/1.0,0.0,0.0,0.0,1.0,1.0,1.0,0.5,0.0,0.0,0.5,1.0,0.33,0.66/
      DATA G/0.0,1.0,0.0,1.0,0.0,1.0,0.5,1.0,1.0,0.5,0.0,0.0,0.33,0.66/
      DATA B/0.0,0.0,1.0,1.0,1.0,0.0,0.0,0.0,0.5,1.0,1.0,0.5,0.33,0.66/

*   Save the current device ID and deactivate the device
      OLDID = GRCIDE
      IF (GRCIDE.GT.0) THEN
          CALL GRTERM
          CALL GDAWK(GRWKID(GRCIDE))
          CALL GQNT(TRN,IERR,GRWIND(1,GRCIDE),GRVIEW(1,GRCIDE))
          IF (IERR.NE.0) THEN
             CALL GRQREP('GROPEN', 'GQNT', IERR)
             GO TO 9999
          END IF
          GRCIDE = 0
      END IF

*   Select a free GRPCKG workstation id
      DO 40 I = 1,MAXDEV
         IF (.NOT.GRDVOP(I)) GO TO 60
   40 CONTINUE
      CALL ERR_REP('GRTMAP', 'GROPEN - too many active plots', GRTMAP)
      GROPEN = -1
      GO TO 9999

   60 CONTINUE
      ID = I

      CALL GQOPS(ISTATE)

      IF (ISTATE .NE. GGKCL .AND. ISTATE .NE. GGKOP) THEN
         READ(UNIT=FILE,FMT='(BN,I64)',IOSTAT=IOSTAT) GRWKID(ID)
         IF (IOSTAT.EQ.0) THEN
            CALL GQWKC(GRWKID(ID),IERR,ICON,GRTYP(ID))
            IF (IERR.EQ.0) THEN

*     Deactivate it if it is already active (GRSELECT will activate it
*     again) and record that we didn't open it.

               CALL GQWKS(GRWKID(ID),IERR,ISTATE)
               IF (IERR.NE.0) THEN
                  CALL GRQREP('GROPEN', 'GQWKS', IERR)
                  GO TO 9999
               END IF
               IF (ISTATE.EQ.GACTIV) CALL GDAWK(GRWKID(ID))
               GRWSOP(ID) = .FALSE.

*     Save all the global GKS attributes that GRPCKG may change
               CALL GRSAVE(.TRUE.)

*     Record the current normalization transformation
               CALL GQCNTN(IERR,ITRAN)
               IF (IERR.NE.0) THEN
                  CALL GRQREP('GROPEN', 'GQCNTN', IERR)
                  GO TO 9999
               END IF
* Success!
	       GO TO 111
            END IF
         END IF
      END IF

*   Open GKS and the workstation
      CALL GROPWK(FILE,ITYPE,GRWKID(ID),GRTYP(ID),ISTAT)
      IF (ISTAT.NE.1) THEN
         GROPEN = -1
         GO TO 9999
      END IF
      GRWSOP(ID) = .TRUE.

 111  CONTINUE


*   Set ASFs for polylines fill area and markers to individual
      CALL GQASF(IERR,ASF)
      ASF(1) = GINDIV
      ASF(2) = GINDIV
      ASF(3) = GINDIV
      ASF(4) = GINDIV
      ASF(6) = GINDIV
      ASF(11) = GINDIV
      ASF(13) = GINDIV
      IF (IERR.NE.0) THEN
         CALL GRQREP('GROPEN', 'GQASF', IERR)
         GO TO 9999
      END IF
      CALL GSASF(ASF)

*   Set marker type 1
      CALL GSMK(1)

*   Set line type and line width scale factor
      CALL GSLN(1)
      CALL GSLWSC(1.0)

*     Select interior style SOLID
      CALL GSFAIS(GSOLID)

*   Set buffering and implicit regeneration flags
      CALL GSDS(GRWKID(ID),GBNIG,GSUPPD)

*   Find the size and resolution of the workstation
      CALL GQDSP(GRTYP(ID),IERR,IUNIT,XM,YM,LX,LY)

      IF (IERR.NE.0) THEN
         CALL GRQREP('GROPEN', 'GQMDS', IERR)
         GO TO 9999
      END IF

      IF (IUNIT.EQ.GMETRE) THEN

*          GKS has device size in metres
         GRXPIN(ID) = (LX-1)/XM/39.37
         GRYPIN(ID) = (LY-1)/YM/39.37
      ELSE

*          Assume nominal 10 inches in x
         GRXPIN(ID) = (LX-1)/10.0
         GRYPIN(ID) = (LY-1)/(10.0*YM/XM)
      END IF

*   For newly opened workstations set the viewports to the full size of
*   the display.
      IF (GRWSOP(ID)) THEN
         CALL GSWKVP(GRWKID(ID),0.0,XM,0.0,YM)

*   Set the viewport and workstation window to the maximum possible
*   area of the same aspect ratio.
         AR = XM/YM
         CALL GSWKWN(GRWKID(ID),0.0,MIN(1.0,AR),0.0,MIN(1.0,1.0/AR))
         CALL GSVP(TRN,0.0,MIN(1.0,AR),0.0,MIN(1.0,1.0/AR))

*   Current transformation is TRN
         ITRAN = TRN
      END IF

*   Record the current viewport and window in the common area.
      CALL GQNT(ITRAN,IERR,GRWIND(1,ID),GRVIEW(1,ID))
      IF (IERR.NE.0) THEN
         CALL GRQREP('GROPEN', 'GQNT', IERR)
         GO TO 9999
      END IF

*   Set the window so that world coordinates match device pixels
      CALL GQWKT(GRWKID(ID),IERR,ITUS,RWINDO,CWINDO,RVIEWP,CVIEWP)
      XNDC2P = (CVIEWP(2)-CVIEWP(1))/(CWINDO(2)-CWINDO(1)) *
     1   REAL(LX-1)/XM
      YNDC2P = (CVIEWP(4)-CVIEWP(3))/(CWINDO(4)-CWINDO(3)) *
     1   REAL(LY-1)/YM

      GRWIND(1,ID) = 0.0
      GRWIND(2,ID) = (GRVIEW(2,ID) - GRVIEW(1,ID)) * XNDC2P
      GRWIND(3,ID) = 0.0
      GRWIND(4,ID) = (GRVIEW(4,ID) - GRVIEW(3,ID)) * YNDC2P

*   Save workstation size
      GRXMIN(ID) = GRVIEW(1,ID) * XNDC2P
      GRXMAX(ID) = GRVIEW(2,ID) * XNDC2P
      GRYMIN(ID) = GRVIEW(3,ID) * YNDC2P
      GRYMAX(ID) = GRVIEW(4,ID) * YNDC2P

*   Copy viewport and window to define the full workstation (used for
*   plotting text which is not clipped at the viewport but is clipped
*   at the workstation edge).
      DO 100 I = 1,4
         GRVIE2(I,ID) = GRVIEW(I,ID)
         GRWIN2(I,ID) = GRWIND(I,ID)
  100 CONTINUE

*   Viewport is visible (overlaps the display surface)
      GRVPVI(ID) = .TRUE.

*   Grey scale not loaded yet
      GRGRLO(ID) = .FALSE.

*  See if the workstation has a dynamic colour table
      CALL GQDWKA(GRTYP(ID),IERR,IPLBUN,IPMBUN,ITXBUN,IFABUN,IPAREP,
     :                             ICOLRE,IWKTR)
      IF (IERR.NE.0) THEN
         CALL GRQREP('GROPEN', 'GQDWKA', IERR)
         GO TO 9999
      END IF

*  If not then only proceed with setting colours if the workstation
*  is empty.
      CALL GQWKDU(GRWKID(ID),IERR,IDEFM,IREGM,IEMPTY,NFRAME)
      IF (IERR.NE.0) THEN
         CALL GRQREP('GROPEN', 'GQDWDU', IERR)
         GO TO 9999
      END IF
      IF (IEMPTY.EQ.GEMPTY .OR. ICOLRE.EQ.GIRG) THEN

*      Set up default colours (if the device has colour)
         CALL GQCF(GRTYP(ID),IERR,NCOLI,NCOLA,NPCI)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GROPEN', 'GQCF', IERR)
            GO TO 9999
         END IF
         CALL GQLWK(GRTYP(ID),IERR,MPLBTE,MPMBTE,MTXBTE,MFABTE,MPAI,
     1              MCOLI)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GROPEN', 'GQLWK', IERR)
            GO TO 9999
         END IF

C Reserving 16 colours on grey scale devices is wasteful but we can't
C avoid it because it is hard wired into PGBEG.
C
C         IF (NCOLA.EQ.GCOLOR) THEN
            DO 120 ICOL = 2,MIN(MCOLI-1,15)
               CALL GSCR(GRWKID(ID),ICOL,R(ICOL),G(ICOL),B(ICOL))
  120       CONTINUE
C         ELSE
C            ICOL = 2
C         END IF

*      If the colour table is not dynamic then fill rest of table with
*      grey scale by interpolating between the index 0 (the background)
*      and index 1 (the foreground). If it is dynamic the loading is
*      postponed until GRGREY is used.
         IF (ICOLRE.EQ.GIRG) THEN
            CALL GQCR(GRWKID(ID),0,GREALI,IERR,C0(1),C0(2),C0(3))
            IF (IERR.NE.0) THEN
               CALL GRQREP('GROPEN', 'GQCR', IERR)
               GO TO 9999
            END IF
            CALL GQCR(GRWKID(ID),1,GREALI,IERR,C1(1),C1(2),C1(3))
            IF (IERR.NE.0) THEN
               CALL GRQREP('GROPEN', 'GQCR', IERR)
               GO TO 9999
            END IF

            DO 160 I = ICOL,MCOLI-1
                DO 140 J = 1,3
                   C3(J) = C0(J) + (C1(J)-C0(J)) *
     :                            REAL(I-ICOL)/REAL(MCOLI-ICOL-1)
  140           CONTINUE
                CALL GSCR(GRWKID(ID),I,C3(1),C3(2),C3(3))
  160       CONTINUE
            GRGRLO(ID) = .TRUE.
         END IF

*     Otherwise leave colours as they are and assume that the grey scale
*     has been loaded
      ELSE
         GRGRLO(ID) = .TRUE.
      END IF

*    Initial coordinate transformation
      GRXSCL(ID) = 1.0
      GRYSCL(ID) = 1.0
      GRXORG(ID) = 0.0
      GRYORG(ID) = 0.0

*    Current point
      GRXPRE(ID) = 0.0
      GRYPRE(ID) = 0.0

*    Line attributes
      GRSTYL(ID) = 1
      GRWIDT(ID) = 1
      GRCCOL(ID) = 1

*    Line pattern
      GRPOFF(ID) = 0.0
      GRIPAT(ID) = 1

*    Fill area style
      GRFAST(ID) = 1

*    Character attributes
      GRCFAC(ID) = 1.0
      GRCFNT(ID) = 1
      GRCSCL(ID) = 1.0

*    Device name
      GRNAME(ID) = FILE

*   Select normalization transformation
      CALL GSELNT(TRN)

*   Device is now open (at last!)
      GRDVOP(ID) = .TRUE.

*   So select it
      CALL GRSLCT(ID)

*   Return success
      GROPEN = 1
      RETURN

 9999 CONTINUE

*   Reactive old device
      CALL GRSLCT(OLDID)

      END
