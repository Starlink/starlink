*---------------------------------------------------------------------
      SUBROUTINE GK1APA
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Send Pattern for a filled area or GDP into PostScript file.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*  ARGUMENTS
*  ---------
*      none
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*  COSA    - Cosinus for pattern transformation.
*  COSB    - Cosinus for pattern transformation.
*  DUMMY   - Dummy character, required by the buffering routine.
*  IC      - Current Column in Pattern
*  ICHUNK  - Chunk length of Pattern data
*  ICOLR   - Workspace offset for workstation colour type
*  INTA    - Integer array to hold the directory data.
*  IR      - Current Row in Pattern
*  IREM    - Dummy integer, required by the buffering routine.
*  JH      - Current Position in Pattern Hex String
*  N       - Count for stack.
*  MODUA   - Moduo of pattern size hight component.
*  MODUB   - Moduo of pattern size width component.
*  REALA   - Real array to hold the directory data.
*  S       - Character variable, via which chunks of PostScript are sent for
*            buffering.
*  SINA    - Sinus for pattern transformation.
*  SINB    - Sinus for pattern transformation.
*

*     Offsets in KWKDAT
      INTEGER    ICOLR
      PARAMETER (ICOLR=17)

*     small real
      REAL       SMALL
      PARAMETER (SMALL=1.0E-4)

*     Chunk Length
      INTEGER    ICHUNK
      PARAMETER (ICHUNK=79)
*
      CHARACTER S*100, DUMMY
      INTEGER IREM, INTA(3), IC,IR, JH, N, NBC
      REAL REALA(1)
      REAL COSA, COSB, SINA, SINB, MODUA, MODUB
*
*  ALGORITHM
*  ---------
*     Prepare data for fapdo and send it. Transforming as necessary.
*
*---------------------------------------------------------------------
*
*     Get pattern information from the directory: INTA(1) will
*     hold the number of columns and INTA(2) number of rows in
*     pattern definition.
      CALL GKDRGE(KPABPT(KWKIX),KWFASI(KWKIX),3,0,INTA,REALA)

*
*     Send down all that is necessary for initialization.
*
      CALL GKFOCO(KIOSN,DUMMY,IREM)
*
*     Work out the transformation parameters.
*
      MODUA = SQRT(QWPAWX(KWKIX)*QWPAWX(KWKIX) +
     :             QWPAWY(KWKIX)*QWPAWY(KWKIX))
      MODUB = SQRT(QWPAHX(KWKIX)*QWPAHX(KWKIX) +
     :             QWPAHY(KWKIX)*QWPAHY(KWKIX))
*
      IF(MODUA.LT.SMALL) MODUA = 1.0
      IF(MODUB.LT.SMALL) MODUB = 1.0
*
      SINA = QWPAWY(KWKIX)/MODUA
      COSA = QWPAWX(KWKIX)/MODUA
      SINB = QWPAHY(KWKIX)/MODUB
      COSB = QWPAHX(KWKIX)/MODUB
*
*     Concatenate to current transformation.
*
      WRITE(S,205) COSA, SINA, COSB, SINB
  205 FORMAT('[ ', 4F9.5, ' 0 0 ] concat')
      CALL GKFOCO(KIOPB,S(1:51),IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
*
*     Determine the number of bytes per colour code
      IF(KWKDAT(ICOLR,KWKIX) .EQ. GCOLOR)THEN
         NBC = 3
      ELSE
         NBC = 1
      ENDIF
*
*     Send the parameters for initialization (transform the reference point).
*
      WRITE(S,210) INTA(1), INTA(2),
     :             QWPAX(KWKIX)*COSA + QWPAY(KWKIX)*SINA,
     :             QWPAX(KWKIX)*COSB + QWPAY(KWKIX)*SINB,
     :             MODUA, MODUB
  210 FORMAT(2I5, 4F11.3, ' fapi')
      CALL GKFOCO(KIOPB,S(1:59),IREM)

*     These loops send pattern data in hexadecimal to the
*     Postscript (current) file, where fapi expects to find
*     it. INTA(3) holds a heap pointer to pattern data.
      DO 350 IR=0,INTA(2)-1
         S(1:1)=' '
         JH = 2
         DO 360 IC=0,INTA(1)-1
*           If no room for additional data, send data and clear string.
            IF(JH+2*NBC-1 .GT. ICHUNK)THEN
               CALL GKFOCO(KIOPB,S(1:JH-1),IREM)
               CALL GKFOCO(KIOSN,DUMMY,IREM)
               JH = 2
            ENDIF
*           im operator expects Hexcodes for Grey or RGB Values.
            CALL GK1ACX(KHP(KHPXI(INTA(3)) + IR*INTA(1) + IC),NBC,
     :                    S(JH:JH+2*NBC-1))
            JH = JH + 2*NBC
  360    CONTINUE
         CALL GKFOCO(KIOPB,S(1:JH-1),IREM)
  350 CONTINUE

*  fapdo does the actual output, using the image or colorimage operator.
      CALL GKFOCO(KIOPB,' fapdo', IREM)

      END
