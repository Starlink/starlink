      PROGRAM SGS_TEST
*+
*  Name:
*     SGS_TEST

*  Purpose:
*     Program to exercise all SGS routines.

*  Language:
*     Starlink Fortran 77

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      CHARACTER*40 WS
      INTEGER IZS(4)
      DATA MSGLU/6/

      CALL SGS_INIT(6,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_INIT - status',ISTAT
         GOTO 9999
      END IF


      CALL SGS_WLIST(6)

      READ (*,'(A)') WS

      CALL SGS_WIDEN(WS,ITYPE,ICON,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_WIDEN - status',ISTAT
         GOTO 9999
      END IF
      WRITE (MSGLU,*) WS,' translates to ',ITYPE,ICON

      CALL SGS_OPNWK(WS,IBASET,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_OPNWK - status',ISTAT
         GOTO 9999
      END IF

      CALL SGS_CLSWK(IBASET,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_CLSWK - status',ISTAT
         GOTO 9999
      END IF

      CALL SGS_OPNWK(WS,IBASE,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_OPNWK - status',ISTAT
         GOTO 9999
      END IF

      CALL SGS_ICURZ(I)
      IF (I.NE.IBASE)
     :   WRITE (MSGLU,*) 'SGS_ICURZ returns', I, ' should be',IBASE

*   Clear zone (workstation is empty so should have no effect)
      CALL SGS_CLRZ

      CALL SGS_ZSHAP(3.0/2.0,'cc',IZ,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_ZSHAP - status',ISTAT
         GOTO 9999
      END IF
      CALL SGS_CLRZ

*  Divide zone into 4 and outline each
      CALL SGS_ZPART(2,2,IZS,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_ZPART - status',ISTAT
         GOTO 9999
      END IF
      DO I = 1,4
         CALL SGS_SELZ(IZS(I),ISTAT)
         IF (ISTAT.NE.0) THEN
            WRITE (MSGLU,*) 'SGS_SELZ - status',ISTAT
            GOTO 9999
         END IF
         CALL SGS_IZONE(X1,X2,Y1,Y2,XM,YM)
         CALL SGS_BOX(X1,X2,Y1,Y2)
      END DO

*   Plot reference picture
      CALL SGS_SELZ(IZ,ISTAT)
      CALL REF(MSGLU)

      CALL SGS_CLOSE

 9999 CONTINUE
      END


      SUBROUTINE REF(MSGLU)
*+
*  Name:
*     REF

*  Purpose:
*     Plots reference picture.

*  Language:
*     Starlink Fortran 77

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      CHARACTER*2 TXJ
      INTEGER IZS(4)
      REAL XSTART(2:5),YSTART(2:5)
      DATA XSTART/0.1,0.4,0.7,0.4/
      DATA YSTART/0.5,0.85,0.5,0.15/

*  Inquire current WC
      CALL SGS_IZONE(X1,X2,Y1,Y2,XM,YM)

*  border
      CALL SGS_BOX(X1,X2,Y1,Y2)

      XSTEP = (X2-X1)/50.0
      YSTEP = (Y2-Y1)/50.0

*  test all 4 pens plotting lines markers and text
      XPOS = X1+XSTEP*5
      CALL SGS_STXJ('CL')
      DO 10 I = 1,4
        CALL SGS_SPEN(I)
        CALL SGS_IPEN(N)
        IF (I.NE.N)
     :   WRITE (MSGLU,*) 'SGS_IPEN returns', N, ' should be',I
        YPOS = Y2-YSTEP*(8+I*2)
        CALL SGS_TX(XPOS,YPOS,'Pen')
        CALL SGS_ATXI(I,1)
        CALL SGS_ATXB('Marker',2)
        CALL SGS_ATXI(I,1)
        CALL SGS_ITXB(XT,YT,NT,DX,DY)
        IF (XT.NE.XPOS.OR.YT.NE.YPOS.OR.NT.NE.15) THEN
           WRITE (MSGLU,*) 'SGS_TXB returns',XT,YT,NT,' should be',
     :                               XPOS,YPOS,15
        END IF
        CALL SGS_ITXA(NF,NPR,HT,AR,XU,YU,SP,TXJ)
        CALL SGS_MARK(XPOS+DX+(AR*HT),YPOS,I)
        CALL SGS_BOX(X1+XSTEP*I,X2-XSTEP*I,Y1+YSTEP*I,Y2-YSTEP*I)
   10 CONTINUE

*  Display device resolution + angled text
      CALL SGS_SPEN(1)
      XPOS = X1+XSTEP*12.0
      YPOS = Y2-YSTEP*20.0
      CALL SGS_SUPTX(1.2,1.0)
      CALL SGS_TX(XPOS,YPOS,'Device resolution')
      CALL SGS_IDUN(DXW,DYW)
      CALL SGS_ATXR(DXW*XM/(X2-X1)*1000.0,-6,3)
      CALL SGS_ATXB('by',1)
      CALL SGS_ATXR(DYW*YM/(Y2-Y1)*1000.0,-6,3)
      CALL SGS_ATXB('mm',1)
      CALL SGS_OTEXT
      CALL SGS_SUPTX(0.0,1.0)

*  Plot different text justifications
      XPOS = X1+XSTEP*10.0
      YPOS = Y1+YSTEP*10.0
      CALL SGS_STXJ('TR')
      CALL SGS_TX(XPOS,YPOS,'TOP')
      CALL SGS_STXJ('BR')
      CALL SGS_TX(XPOS,YPOS,'BOTTOM')
      CALL SGS_STXJ('CL')
      CALL SGS_TX(XPOS,YPOS,'CENTRE')

      XPOS = X1+XSTEP*10.0
      YPOS = Y1+YSTEP*15.0
      CALL SGS_STXJ('TL')
      CALL SGS_TX(XPOS,YPOS,'LEFT')
      CALL SGS_STXJ('TR')
      CALL SGS_TX(XPOS,YPOS,'RIGHT')
      CALL SGS_STXJ('BC')
      CALL SGS_TX(XPOS,YPOS,'CENTRE')
      CALL SGS_SFONT(103)
      CALL SGS_SHTX(0.03)
      CALL SGS_SARTX(1.0)
      CALL SGS_SUPTX(1.0,1.0)
      CALL SGS_SSPTX(0.2)
      CALL SGS_STXJ('BL')

*  Change all text attributes
      XPOS = X1+XSTEP*5.0
      YPOS = Y1+YSTEP*30.0
      CALL SGS_BTEXT(XPOS,YPOS)
      CALL SGS_ATEXT('ABCDEFGHIJK')
      CALL SGS_ITXB(XT,YT,NT,DX,DY)
      CALL SGS_OTEXT

*  Plot box around text to test text inquiry
      CALL SGS_ITXA(NF1,NPR1,HT1,AR1,XU1,YU1,SP1,TXJ)
      CALL SGS_LINE(XPOS,YPOS,XPOS+DX,YPOS+DY)
      CALL SGS_APOLY(XPOS+DX+HT1*XU1,YPOS+DY+HT1*YU1)
      CALL SGS_APOLY(XPOS+HT1*XU1,YPOS+HT1*YU1)
      CALL SGS_APOLY(XPOS,YPOS)
      CALL SGS_OPOLY

*  New zone inside test lines
      CALL SGS_ZONE(X1+XSTEP*5,X2-XSTEP*5,Y1+YSTEP*5,Y2-YSTEP*5,IZ2,
     :              ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_ZONE - status',ISTAT
         GOTO 9999
      END IF

*  Split into four equal parts
      CALL SGS_ZPART(2,2,IZS,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_ZPART - status',ISTAT
         GOTO 9999
      END IF

*  Select bottom left  and test zshap
      CALL SGS_SELZ(IZS(2),ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_SELZ - status',ISTAT
         GOTO 9999
      END IF
      CALL SGS_ZSHAP(1.0,'CC',IZT,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_ZSHAP - status',ISTAT
         GOTO 9999
      END IF

*  Plot cross to test that it is really square
      CALL SGS_LINE(0.0,0.0,1.0,1.0)
      CALL SGS_LINE(1.0,0.0,0.0,1.0)

*  Circles with all pens
      DO 20 I=1,4
        CALL SGS_SPEN(I)
        CALL SGS_CIRCL(0.5,0.5,0.5-((I-1)*0.05))
   20 CONTINUE
      CALL SGS_SPEN(1)
      CALL SGS_RELZ(IZT)
      CALL SGS_SELZ(IZS(4),ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_SELZ - status',ISTAT
         GOTO 9999
      END IF

*  Test size
      CALL SGS_ZSIZE(0.02,0.02,'CC',IZT,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_ZSIZE - status',ISTAT
         GOTO 9999
      END IF
      CALL SGS_SFONT(NF)
      CALL SGS_SHTX(0.1)
      CALL SGS_SARTX(AR)
      CALL SGS_SUPTX(UX,UY)
      CALL SGS_SSPTX(SP)
      CALL SGS_STXJ('CC')
      CALL SGS_BOX(0.0,1.0,0.0,1.0)
      CALL SGS_TX(0.5,0.5,'2 cm')

*  Select outer zone and test tpz
      CALL SGS_SELZ(IZS(4),ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_SELZ - status',ISTAT
         GOTO 9999
      END IF
      CALL SGS_IZONE(X1,X2,Y1,Y2,XM,YM)

      CALL SGS_BOX(X1,X2,Y1,Y2)

      CALL SGS_TPZ(IZT,0.0,0.0,IZS(4),X,Y,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_TPZ - status',ISTAT
         GOTO 9999
      END IF
      CALL SGS_LINE(X1,Y1,X,Y)
      CALL SGS_TPZ(IZT,1.0,0.0,IZS(4),X,Y,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_TPZ - status',ISTAT
         GOTO 9999
      END IF
      CALL SGS_LINE(X2,Y1,X,Y)
      CALL SGS_TPZ(IZT,1.0,1.0,IZS(4),X,Y,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_TPZ - status',ISTAT
         GOTO 9999
      END IF
      CALL SGS_LINE(X2,Y2,X,Y)
      CALL SGS_TPZ(IZT,0.0,1.0,IZS(4),X,Y,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_TPZ - status',ISTAT
         GOTO 9999
      END IF
      CALL SGS_LINE(X1,Y2,X,Y)

*  Set coordinate to 0-1 in x and y
      CALL SGS_SW(0.0,1.0,0.0,1.0,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (MSGLU,*) 'SGS_SW - status',ISTAT
         GOTO 9999
      END IF

*  Test markl with all markers
      XINC = 0.1
      DO 30 I = 2,5
        CALL SGS_BPOLY(XSTART(I),YSTART(I))
        DO 40 J = 1,3
           CALL SGS_APOLY(XSTART(I)+XINC*(J-1),YSTART(I))
           CALL SGS_MARKL(I)
   40   CONTINUE
        CALL SGS_OPOLY
   30 CONTINUE

*  release all zones
      CALL SGS_RELZ(IZT)
      CALL SGS_RELZ(IZ2)
      CALL SGS_RELZ(IZS(1))
      CALL SGS_RELZ(IZS(2))
      CALL SGS_RELZ(IZS(3))
      CALL SGS_RELZ(IZS(4))
 9999 CONTINUE
      RETURN
      END
