C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPCHI (IPRT,IDTG,IDPT)
C
C MAPCHI is called by various EZMAP routines to reset the intensity,
C dotting, and dash pattern before and after drawing parts of a map.
C
C The argument IPRT, if positive, says which part of the map is about
C to be drawn, as follows:
C
C     IPRT    Part of map.
C     ----    ------------
C       1     Perimeter.
C       2     Grid.
C       3     Labelling.
C       4     Limb lines.
C       5     Outline point group, continental.
C       6     Outline point group, U.S.
C       7     Outline point group, country.
C
C A call with IPRT equal to the negative of one of these values asks
C that the intensity saved by the last call, with IPRT positive, be
C restored.
C
C When IPRT is positive, IDTG is zero if solid lines are to be used, 1
C if dotted lines are to be used.  If IPRT is negative, IDTG is ignored.
C
C When IPRT is positive and IDTG is zero, IDPT is the dash pattern to be
C used.  If IPRT is negative or IDTG is non-zero, IDPT is ignored.
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      COMMON /MAPNTS/ INTS(7)
C
C Declare one of the dash-package common blocks, too.
C
      COMMON /SMFLAG/ ISMO
C
C The variables INTO, IDTS, and ISMS need to be saved between calls.
C
      SAVE INTO,IDTS,ISMS
C
C Flush all buffers before changing anything.
C
      CALL MAPIQ
C
C Set/reset intensity, dotting, and dash pattern.  The user has the
C last word.
C
      IF (IPRT.GT.0) THEN
        ISMS=ISMO
        ISMO=1
        IDTS=IDTL
        IDTL=IDTG
        IF (IDTL.EQ.0) CALL DASHDB (IDPT)
C
C The following lines have been commented out because the intensity
C setting causes some strange behaviour on certain terminals and
C workstations.
C
C        CALL GETUSV ('IN',INTO)
C        CALL SETUSV ('IN',IFIX(10000.*FLOAT(INTS(IPRT))/255.))
        CALL MAPUSR (IPRT)
      ELSE
        CALL MAPUSR (IPRT)
C
C The following line have been commented out because the intensity
C setting causes some strange behaviour on certain terminals and
C workstations.
C
C        CALL SETUSV ('IN',INTO)
        IF (IDTL.EQ.0) CALL DASHDB (IOR(ISHIFT(32767,1),1))
        IDTL=IDTS
        ISMO=ISMS
      END IF
C
C Done.
C
      RETURN
C
      END
