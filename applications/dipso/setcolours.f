*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*   SUBROUTINE SETCOLOURS (calls PPALET)
*
*   SELECT PLOTTING IN PARTICULAR PALET COLOUR
*
*   IMPORTS:
*     IPAL  (INTEGER) PALET SELECTION INDEX
*
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE SETCOLOURS(DEVTYP,NCOLOURS)
*
*
*   DECLARATIONS
*
*
*
*
       INTEGER IPAL
       INTEGER DEVTYP
       INTEGER MAXPAL
*
*
       INTEGER ASF(13),WKID
       REAL TIPAL(12)
       DATA ASF/13*0/
*
*
*   COMMON AREAS
*
*
       COMMON /COLCOM/ MAXPAL, TIPAL
*
*
*   ONLY WORK IN RANGE
          MAXPAL=NCOLOURS
** Set GKS aspect source flags to individual to enable setting up
** of colour indices
         DO I=3,13
           ASF(I)=1
         ENDDO
         CALL GSASF(ASF)
** Setting up ARGS colour table
** 1 = White         6 = cyan            11 = olive
** 2 = red           7 = magenta         12 = gold
** 3 = green         8 = pink            13 = violet
** 4 = blue          9 = orange          14 = light green
** 5 = yellow       10 = turquoise       15 = grey
          CALL GSCR(1,5,1.0,1.0,0.0)
          CALL GSCR(1,6,0.0,1.0,1.0)
          CALL GSCR(1,7,1.0,0.0,1.0)
          CALL GSCR(1,8,1.0,0.4,0.4)
          CALL GSCR(1,9,1.0,0.4,0.0)
          CALL GSCR(1,10,0.1,0.8,0.8)
          CALL GSCR(1,11,0.8,1.0,0.2)
          CALL GSCR(1,12,1.0,0.6,0.0)
          CALL GSCR(1,13,0.8,0.4,1.0)
          CALL GSCR(1,14,0.4,0.9,0.3)
          CALL GSCR(1,15,0.5,0.5,0.5)

          RETURN
          END
