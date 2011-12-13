      PROGRAM SGSX2

*+
*  Name:
*     sgsx2

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
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Puepose:
*     DEMONSTRATION OF SGS PACKAGE

*-

      CHARACTER*20 WKSTN

*  LIST WORKSTATIONS
      CALL SGS_WLIST(6)

*  GET WORKSTATION NAME
      PRINT *, 'WORKSTATION?'
      READ (*,'(A)') WKSTN

*  OPEN
      CALL SGS_OPEN(WKSTN,IZBASE,JSTAT)
      CALL SGS_ICURW(IWKID)

*  SPECIFY TEXT SIZE
      CALL SGS_SHTX(0.3)
      CALL SGS_SPREC(2)

*  CREATE SQUARE ZONE AND PLOT IN IT
      CALL SGS_ZSHAP(1.0,'BL',IZONA,J)
      CALL GRAPH

*  CREATE A SUBZONE AND PLOT IN THAT
      CALL SGS_ZONE(-4.0,0.0,-2.25,1.75,IZONEB,J)
      CALL GRAPH

*  CREATE A SUBSUBZONE AND PLOT IN THAT
      CALL SGS_ZONE(-4.0,0.0,-2.25,1.75,IZONEC,J)
      CALL GRAPH

*  WRAP UP
      CALL SGS_CLOSE

      END


      SUBROUTINE GRAPH
*
*  PLOT INTO A ZONE
*
*  SETS WINDOW AND PEN
*


*  SET WINDOW TO +/- 5.0 IN BOTH X AND Y
      CALL SGS_SW(-5.0,5.0,-5.0,5.0,J)

*  TRY TO CLEAR THE AREA
      CALL SGS_CLRBL(-5.0,5.0,-5.0,5.0)

*  SELECT FIRST PEN
      CALL SGS_SPEN(1)

*  FRAME
      CALL SGS_BOX(-5.0,5.0,-5.0,5.0)

*  2.4 RADIUS CIRCLE CENTRED ON 2.5,2.5
      CALL SGS_SPEN(2)
      CALL SGS_CIRCL(2.5,2.5,2.4)

*  SOME TEXT INCLUDING NUMBERS
      CALL SGS_BTEXT(-4.0,-3.0)
      CALL SGS_ATEXT('ABCDEFGHI')
      CALL SGS_ATXI(654321,-10)
      CALL SGS_BTEXT(-4.0,-4.9)
      CALL SGS_ATXL('INTEGER =          ')
      CALL SGS_ATXI(123,1)
      CALL SGS_ATXB('      EXACTLY',1)
      CALL SGS_BTEXT(-4.0,4.0)
      CALL SGS_ATXL('REALS =')
      CALL SGS_ATXR(123.456,1,3)
      CALL SGS_ATXR(99.0,1,-1)
      CALL SGS_ATXR(-987.6,-10,1)


*  AXES
      CALL SGS_SPEN(3)
      CALL SGS_BPOLY(-5.0,0.0)
      CALL SGS_APOLY(5.0,0.0)
      CALL SGS_BPOLY(0.0,5.0)
      CALL SGS_APOLY(0.0,-5.0)
      X0=2.5
      Y0=-2.5
      CALL SGS_BPOLY(X0,Y0)

*  SOME ISOLATED STRINGS AND NUMBERS
      CALL SGS_TX(-4.0,1.0,'ENOUGH!')
      CALL SGS_TXI(-4.0,2.0,12,0)
      CALL SGS_TXR(-4.0,3.0,12.34,0,2)


*  SOME MARKERS
      MARK = 0
      Y = 3.0
      DO I = 1, 4
         MARK = MARK + 1
         X = -2.0
         DO J = 1, 4
            CALL SGS_MARK(X,Y,MARK)
            X = X + 0.25
         END DO
         Y = Y - 0.25
      END DO


*  SPIRAL
      CALL SGS_SPEN(4)
      DO N=1,1000
         W=REAL(N)
         R=W/500.0
         T=W/10.0
         CALL SGS_APOLY(R*COS(T)+X0,R*SIN(T)+Y0)
      END DO

*  SINE WAVE WITH MARKERS
      CALL SGS_SPEN(1)
      DO I=-4,3
         DO J=0,50
            X=REAL(50*I+J)/50.0
            Y=4.0*SIN(3.14159*X/4.0)
            IF (I.EQ.-4.AND.J.EQ.0) THEN
               CALL SGS_BPOLY(X,Y)
               CALL SGS_MARKL(5)
            ELSE
               CALL SGS_APOLY(X,Y)
            END IF
         END DO
         CALL SGS_MARKL(5)
      END DO

      END
