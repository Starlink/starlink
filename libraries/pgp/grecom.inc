*+
*  Common code for the GKS emulation of GRPCKG
*
*  D.L.Terrett  Starlink  Aug 1987
*+

*  Parameter to define the maximum number of simultaniously open devices.
*  The value comes from the maximum number of simultaniously open devices
*  allowed by GKS-UK.
      INTEGER MAXDEV
      PARAMETER (MAXDEV=4)

*  GKS transformations used by GRPCKG
      INTEGER TRN, TRN2
      PARAMETER (TRN=1, TRN2=2)

*  Default character size and spacings
      REAL GRCXSZ, GRCYSZ, GRCXSP, GRCYSP
      PARAMETER( GRCXSZ =  7.0, GRCYSZ =  9.0)
      PARAMETER( GRCXSP = 10.0, GRCYSP = 13.0)

*  The currently selected GRPCKG device
      INTEGER GRCIDE

*  Table of GRPCKG device info.
*    Table entry in use flag
        LOGICAL GRDVOP(MAXDEV)

*    GKS Viewport and Window. The viewport defines the PGPLOT viewport
*    (unless it lies completely outside the GRPCKG workstation) and
*    world coordinates are in units of pixels relative to the GRPCKG
*    workstation origin (also referred to a absolute coordinates).
*    These are set up by GROPEN and saved and restored by GRSELECT and
*    are not used at any other time.
         REAL GRVIEW(4,MAXDEV)
         REAL GRWIND(4,MAXDEV)

*    Window and viewport that covers the whole workstation.
         REAL GRVIE2(4,MAXDEV)
         REAL GRWIN2(4,MAXDEV)
      
*    PGPLOT world coordinate to absolute coordinate (=GKS world coords)
*    scale factors and origin.
*
*        X(abs) = XSCL * X(world) + XORG etc.

        REAL GRXSCL(MAXDEV), GRYSCL(MAXDEV)
        REAL GRXORG(MAXDEV), GRYORG(MAXDEV)

*    Current point (in absolute coordinates)
        REAL GRXPRE(MAXDEV), GRYPRE(MAXDEV)

*    Line style, width, and colour
        INTEGER GRSTYL(MAXDEV), GRWIDT(MAXDEV), GRCCOL(MAXDEV)

*    Fill area style
        INTEGER GRFAST(MAXDEV)

*    Dash pattern control
        REAL GRPOFF(MAXDEV)
        INTEGER GRIPAT(MAXDEV)

*    User character scaling factor, font
        REAL GRCFAC(MAXDEV)
        INTEGER GRCFNT(MAXDEV)

*    Device character scaling factor. This allows different devices to
*    have different default character sizes but currently is set to 1.0
*    for all workstations.
        REAL GRCSCL(MAXDEV)

*    Buffers for accumulating polylines etc.
        INTEGER GRBSIZ, GRBPT
        PARAMETER (GRBSIZ = 1024)
        REAL GRXBUF(GRBSIZ), GRYBUF(GRBSIZ)

*    Workstation and type and identifier
        INTEGER GRTYP(MAXDEV), GRWKID(MAXDEV)

*    Limits (in pixels) of the GPRCKG workstation. For GKS workstations
*    opened by GRPCKG these are zero and the size of the display surface
*    respectively.
        REAL GRXMIN(MAXDEV), GRYMIN(MAXDEV)
        REAL GRXMAX(MAXDEV), GRYMAX(MAXDEV)

*    Device resolution in pixels per inch
        REAL GRXPIN(MAXDEV),GRYPIN(MAXDEV)

*    Flag to show if grey scale has been loaded
        LOGICAL GRGRLO(MAXDEV)

*    Flag to show whether the workstation had to be opened by GRPCKG
        LOGICAL GRWSOP(MAXDEV)

*    Flag that indicates that the viewport overlaps the display
*    surface.
        LOGICAL GRVPVI(MAXDEV)

*    Device name
        CHARACTER*80 GRNAME(MAXDEV)

      COMMON /GRECOM/ GRXBUF, GRYBUF, GRBPT,
     :                GRCIDE, GRDVOP,
     :                GRVIEW, GRWIND,
     :                GRVIE2, GRWIN2,
     :                GRXSCL, GRYSCL, GRXORG, GRYORG,
     :                GRXPRE, GRYPRE,
     :                GRSTYL, GRWIDT, GRCCOL,
     :                GRFAST,
     :                GRPOFF, GRIPAT,
     :                GRCFAC, GRCFNT, GRCSCL,
     :                GRTYP,  GRWKID,
     :                GRXMAX, GRYMAX, GRXMIN, GRYMIN,
     :                GRXPIN, GRYPIN,
     :                GRGRLO, GRWSOP, GRVPVI

      COMMON /GRECCH/ GRNAME

      SAVE /GRECOM/,/GRECCH/
