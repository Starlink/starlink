        BLOCK DATA GRIDT
C
        COMMON /LAB/ SIZX,SIZY,XDEC,YDEC,IXORI
        COMMON /CLAB/ XFMT, YFMT
        COMMON /TICK/ MAJX, MINX, MAJY, MINY
        COMMON /GRIINT/ IGRIMJ, IGRIMN, IGRITX
        CHARACTER*8 XFMT,YFMT
        REAL MAJX,MINX,MAJY,MINY
        SAVE
C
        DATA XFMT,YFMT /'(E10.3) ','(E10.3) '/
        DATA SIZX,SIZY / 0.01, 0.01 /
        DATA XDEC,YDEC / 0., 0. /
        DATA IXORI / 0 /
        DATA MAJX,MINX,MAJY,MINY / 0., 0., 0., 0./
        DATA IGRIMJ,IGRIMN,IGRITX / 1, 1, 1/
C REVISION HISTORY---------------
C----------------------------------------------------------
        END
