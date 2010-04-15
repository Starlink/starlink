*  History:
*     17 Nov 1993 (hme):
*        Change common name from LABEL_MAP to LABELMAP.
*        This change is necessary only in this routine, which indicates
*        that a SAVE statement might be enough and no COMMON is needed.
*     01 Jan 1994 (rp):
*        Add one line change from V6.3 on the VAX.
*     28 Jan 1994 (hme):
*        Remove the desparate TYPE* statements.
*     31 Jan 1994 (hme):
*        Disuse <> in formats.
*     24 Apr 1994 (rp):
*        Change COMMON blocks to include files
*     28 Sep 2000 (ajc):
*        SAVE IZ
C-----------------------------------------------------------------------

      SUBROUTINE LABEL_MAP

C  Routine to label map with salient details so as not to overwrite the
C  map itself.

      IMPLICIT NONE

*     Include files

      INCLUDE 'FLAGCOMM'
      INCLUDE 'MAPHD'
      INCLUDE 'MAPTITLES'
      INCLUDE 'PLOT2D'

*     Formal parameters


      INTEGER   NZ
      REAL*4    DZ
      REAL*4    ZC(NZ)

      INTEGER   NC
      REAL*4    ZLEV(NC)

      REAL*4    CMIN, CMAX
      CHARACTER ITEM*(*)

      REAL*4    ARRMAX, ARRMIN
      REAL*4    VST,    VEND

      REAL*4    VST1,   VEND1,   VINC

      REAL*8    RA,     DEC

*     Local variables

      INTEGER  IERR
      INTEGER  IX, IY, IZ
      INTEGER  ILIT
      INTEGER  ISP
      INTEGER  J
      INTEGER  K,  K1, K2
      INTEGER  NLINES

      CHARACTER MAPMAX*9, MAPMIN*9
      CHARACTER CONTINT*6, CONBASE*8
      CHARACTER CONTNO*2, CSTRING*72
      CHARACTER RASTRING*12, DECSTRING*12
      CHARACTER STRING*80
      CHARACTER VEL1*9, VEL2*9, DELV*9
      CHARACTER XTITLE*6, XTYPE*10
      CHARACTER MAPTITLE*28

*     Local common (saved)

      REAL      YLABEL
      REAL      XSIZED,   YSIZED
      CHARACTER IPROC*10
      COMMON /LABEL__MAP/ YLABEL, XSIZED, YSIZED, IPROC
      SAVE   /LABEL__MAP/
      SAVE   IZ

*     Functions

      INTEGER   GEN_ILEN

*  Ok, go...

      ENTRY LABEL_MAP_SOURCE (RA, DEC)
*     ----------------------------------

      IX = LINK(1)
      IY = LINK(2)
      IZ = LINK(3)

      XTYPE  = XAXIS_NAME
      XTITLE = XAXIS_UNITS

      MAPTIT(3) = XTYPE
      AXTIT(3)  = XTITLE

      IF (ISUM) THEN
        UNITS = 'K'//AXTIT(IZ)
        IPROC = 'integrated'
      ELSE
        UNITS = 'K'
        IPROC = ' averaged '
      END IF

C  Write out information about map

      MAPTITLE = MAP_NAME(:MIN(28,GEN_ILEN(MAP_NAME)))

      CALL SXGLTYPE      (0)
      CALL SXGDEVINFO    (XSIZED, YSIZED)
      CALL SXGVWINDOW    (0.0, XSIZED, 0.0, YSIZED)
      CALL SXGLIMITS     (0.0, XSIZED, 0.0, YSIZED)

      CALL SXGVRELOCATE  (1.0,       24.)
      CALL SXGVDRAW      (XSIZED-1., 24.)

      CALL DEG_TO_STRING (RA/15.D0, RASTRING)
      CALL DEG_TO_STRING (DEC,      DECSTRING)
      CALL SXGVLABEL  (2.0, 20.0, MAPTITLE
     &                    //'  Map centre RA'//RASTRING//', Dec.'
     &                    //DECSTRING)

      YLABEL = 16.
      RETURN

      ENTRY LABEL_MAP_CONT (ZC, DZ, NZ)
*     ---------------------------------

      IF (NZ .EQ. 0) RETURN

      WRITE  (CONTINT, '(F6.2)', IOSTAT=IERR) DZ
      WRITE  (CONBASE, '(F8.2)', IOSTAT=IERR) ZC(1)

      CALL SXGLTYPE     (0)
      CALL SXGVLABEL    (5.0, YLABEL, 'Contour intl: '//CONTINT
     &                      //'; Base level '//CONBASE//' '//UNITS)
      CALL SXGVRELOCATE (0.,     0.)
      CALL SXGVDRAW     (XSIZED, 0.)

      YLABEL = YLABEL - 4.
      RETURN

      ENTRY LABEL_MAP_PANEL (ITEM, CMIN, CMAX)
*     ----------------------------------------

      STRING = ' '
      ILIT   = GEN_ILEN (ITEM)
      ISP    = 18 - ILIT
      WRITE  (STRING, '(A18,'' - min & max = '','//
     &       'F6.1,'','',F6.1)',
     &       IOSTAT=IERR) ITEM, CMIN, CMAX

      CALL SXGLTYPE  (0)
      CALL SXGVLABEL (5.0, YLABEL, STRING)

      YLABEL = YLABEL - 4.
      RETURN

      ENTRY LABEL_MAP_MCONT (ZLEV, NC)
*     ---------------------------------

      IF (NC .EQ. 0) RETURN

      WRITE (CONTNO, '(I2)', IOSTAT=IERR) NC

      CALL SXGLTYPE      (0)
      CALL SXGVLABEL     (5.0, YLABEL,
     &                       CONTNO//' contours set manually:')

      YLABEL = YLABEL - 4.

      NLINES = (NC-1)/8 + 1
      DO J = 1, NLINES
        K1 = (J-1)*8 + 1
        K2 = MIN (NC, 8*J)
        WRITE             (CSTRING, '(8(F9.2,1X))', IOSTAT=IERR)
     &                    (ZLEV(K), K = K1,K2)
        CALL SXGVLABEL    (5.0, YLABEL, CSTRING(:GEN_ILEN(CSTRING)))
        YLABEL = YLABEL - 4.
      END DO

      CALL SXGVRELOCATE   (0.,     0.)
      CALL SXGVDRAW       (XSIZED, 0.)

      RETURN

      ENTRY LABEL_MAP_Z (ARRMAX, ARRMIN, VST, VEND)
*     ---------------------------------------------

      WRITE  (MAPMAX, '(F9.2)', IOSTAT=IERR) ARRMAX
      WRITE  (MAPMIN, '(F9.2)', IOSTAT=IERR) ARRMIN
      WRITE  (VEL1,   '(F9.2)', IOSTAT=IERR) VST
      WRITE  (VEL2,   '(F9.2)', IOSTAT=IERR) VEND

      CALL SXGLTYPE     (0)
      CALL SXGVLABEL    (5.0, YLABEL,
     &                   'Ta '//IPROC//': '//VEL1//' to '//VEL2
     &                   //'; Max:'//MAPMAX//'; Min: '//MAPMIN//' '
     &                   //UNITS)

      YLABEL = YLABEL - 4.
      RETURN

      ENTRY LABEL_MAP_ZRANGE (VST1, VEND1, VINC)
*     ------------------------------------------

      WRITE (VEL1, '(F9.2)', IOSTAT=IERR) VST1
      WRITE (VEL2, '(F9.2)', IOSTAT=IERR) VEND1
      WRITE (DELV, '(F9.2)', IOSTAT=IERR) VINC

      CALL SXGLTYPE     (0)
      CALL SXGVLABEL    (5.0, YLABEL,
     &                      'Ta '//IPROC//' in channels of width '
     &                      //DELV//' from '//VEL1//' to '
     &                      //VEL2//AXTIT(IZ))

      YLABEL = YLABEL - 4.
      RETURN

      END

C----------------------------------------------------------------------
