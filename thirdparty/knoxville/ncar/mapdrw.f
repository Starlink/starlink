      SUBROUTINE MAPDRW
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
C
C The following call gathers statistics on library usage at NCAR.
C
      CALL Q8QST4 ('GRAPHX','EZMAP','MAPDRW','VERSION  1')
C
C Initialize the package, draw and label the grid, and draw outlines.
C
      IF (INTF) CALL MAPINT
      CALL MAPGRD
      CALL MAPLBL
      CALL MAPLOT
C
      RETURN
      END
