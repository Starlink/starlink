*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      SUBROUTINE MAP_COORDS (DMS, LABEL, IX, IY, XLIM, YLIM,
     &                       MAP_RA, MAP_DEC)

*  Routine to label a 2-D map. If required then it labels position axes
*  as absolute R.A. or Dec.

      IMPLICIT  NONE

*     Formal parameters:

      LOGICAL   DMS
      LOGICAL   LABEL
      INTEGER   IX,      IY
      REAL      XLIM(2), YLIM(2)
      REAL*8    MAP_RA,  MAP_DEC

*     Local variables:

      INTEGER   LXT,    LYT
      REAL      XRDL,   XRDH
      REAL      YRDL,   YRDH
      CHARACTER RASTR*12,  DECSTR*12
      CHARACTER LABELX*64, LABELY*64

*     Common blocks/include files

      INCLUDE  'MAPTITLES'

*     Functions etc:

      INTEGER   GEN_ILEN

* Ok, go...

      LABELX = MAPTIT(IX)//'('//AXTIT(IX)//')'
      LXT    = GEN_ILEN (LABELX)
      LABELY = MAPTIT(IY)//'('//AXTIT(IY)//')'
      LYT    = GEN_ILEN (LABELY)

*     Label R.A. and Dec. offsets only (also use offsets if map axes
*     not aligned with RA/Dec)

      IF (.NOT.DMS .or. MAPTIT(1)(1:4).ne.'R.A.') THEN

        CALL DEG_TO_STRING (MAP_RA/15.,  RASTR)
        CALL DEG_TO_STRING (MAP_DEC,     DECSTR)

        IF (IX.EQ.1) THEN
          LABELX = LABELX(:LXT)//' from '//RASTR(2:12)
        ELSE IF (IX.EQ.2) THEN
          LABELX = LABELX(:LXT)//' from '//DECSTR(:12)
        END IF
        LXT = GEN_ILEN (LABELX)

        IF (IY.EQ.1) THEN
          LABELY = LABELY(:LYT)//' from '//RASTR(2:12)
        ELSE IF (IY.EQ.2) THEN
          LABELY = LABELY(:LYT)//' from '//DECSTR(:12)
        END IF
        LYT = GEN_ILEN (LABELY)

        IF (LABEL) THEN
          CALL SXGBOX    (1, 1)
          CALL SXGYLABEL (LABELY)
          CALL SXGXLABEL (LABELX)
        ELSE
          CALL SXGBOX    (0, 0)
        END IF

C     ... Or, label true RA and Dec

      ELSE
        LABELX = MAPTIT(IX)(:4)
        IF (IX.EQ.1) THEN
          XRDL = XLIM(1)/(COS(MAP_DEC*0.01745329252)*15.) + 240.*MAP_RA
          XRDH = XLIM(2)/(COS(MAP_DEC*0.01745329252)*15.) + 240.*MAP_RA
          CALL SXGXSIX (1)
        ELSE IF (IX.EQ.2) THEN
          XRDL =  XLIM(1)+3600.*MAP_DEC
          XRDH =  XLIM(2)+3600.*MAP_DEC
          CALL SXGXSIX (0)
        ELSE
          XRDL = XLIM(1)
          XRDH = XLIM(2)
          LABELX = MAPTIT(IX)//' ('//AXTIT(IX)//')'
        END IF
        LXT = GEN_ILEN (LABELX)

        LABELY = MAPTIT(IY)(:4)
        IF (IY.EQ.1) THEN
          YRDL = YLIM(1)/(COS(MAP_DEC*0.01745329252)*15.) + 240.*MAP_RA
          YRDH = YLIM(2)/(COS(MAP_DEC*0.01745329252)*15.) + 240.*MAP_RA
          CALL SXGYSIX (1)
        ELSE IF (IY.EQ.2) THEN
          YRDL =  YLIM(1)+3600.*MAP_DEC
          YRDH =  YLIM(2)+3600.*MAP_DEC
          CALL SXGYSIX (0)
        ELSE
          YRDL = YLIM(1)
          YRDH = YLIM(2)
          LABELY = MAPTIT(IY)//' ('//AXTIT(IY)//')'
        END IF
        LYT = GEN_ILEN (LABELY)

CD      PRINT *, 'Offset for absolute positions:'
CD      PRINT *, '     RDCENX = ', RDCENX
CD      PRINT *, '     RDCENY = ', RDCENY
CD      PRINT *, '     MAPRA  = ', MAP_RA
CD      PRINT *, '     MAPDEC = ', MAP_DEC

        CALL SXGLIMITS   (XRDL, XRDH, YRDL, YRDH)
        IF (LABEL) THEN
          CALL SXGRDBOX  (1,  1)
          CALL SXGXLABEL (LABELX)
          CALL SXGYLABEL (LABELY)
        ELSE
          CALL SXGRDBOX  (0,  0)
        END IF
        CALL SXGLIMITS   (XLIM(1), XLIM(2), YLIM(1), YLIM(2))
      END IF

      RETURN
      END

*-----------------------------------------------------------------------
