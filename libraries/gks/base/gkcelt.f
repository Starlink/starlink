C# IL>=a, OL>=0
      SUBROUTINE GKCELT (DCX,DCY,TRNCEL)
*
* (C) COPYRIGHT ICL & SERC  1988
*

*---------------------------------------------------------------------
*  Type of routine:    UTILITY
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Get cell array box and transformation from WCA data
*
*     The CELL ARRAY TRANSFORMATION maps the box
*     (M-1,N-1),(M,N-1),(M,N),(M-1,N) ONTO the cell in
*     column N and row M in DC, if M and N are whole numbers.
*
*  MAINTENANCE LOG
*  ---------------
*     16/03/88  KEVP  Created
*     31/07/90  PLP   Added include/check.inc, brought commenting in
*                     line with the standard format.
*
*  ARGUMENTS
*  ---------
*     OUT  DCX,DCY   Cell array box in DC
*     OUT  TRNCEL    Cell array transformation
*                    (see PUROSE and ALGORITHM)
*
      REAL    DCX(4),DCY(4), TRNCEL (3,2)

*  COMMON BLOCK USAGE
*  ------------------
*    W/S Comms Area data used:
*     KWKIX     : Workstation Identifier
*     QWR1,QWR2 : WC X,Y of point P = (DCX(1),DCY(1))
*     QWR3,QWR4 : WC X,Y of point Q = (DCX(3),DCY(3))
*     QWR5,QWR6 : WC X,Y of point R = (DCX(2),DCY(2))
*
*     KWI5,KWI6 : number of columns, number of rows
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     NCOLM     Number of cell array columns
*     NROW      Number of cell array rows
*     WCX,WCY   Three corners of Cell array box in WC

      INTEGER NCOLM,NROW
      REAL WCX(3),WCY(3)

*  ERRORS
*  ------
*     91  Cell array dimensions not both positive
*
*  ALGORITHM
*  ---------
*     The three corner points P by cell(1,1),
*     Q by cell(NCOLM,NROW) and R by cell(NCOLM,1)
*     are taken from the WCA and converted to DC
*     and the fourth corner P+Q-R is added.
*
*     The points are put in order (P, R, Q, P-R+Q)
*     so that the box can be drawn as a FILL AREA.
*
*     The cell vectors are obtained by dividing
*     R-P by NCOLM and Q-R by NROW.
*
*     The cell vectors and first corner P are put into
*     the transformation matrix.
*
*     TRNCEL(1,*)  =  Cell Width Vector
*     TRNCEL(2,*)  =  Cell Height Vector
*     TRNCEL(3,*)  =  First corner of cell array P
*

*---------------------------------------------------------------------

* Extract cell array dimensions
      NCOLM = KWI5
      NROW  = KWI6

* Reject Cell array of non-positive dimensions
      IF ((NCOLM .LE. 0) .OR. (NROW .LE. 0)) THEN
         KERROR = 91
         GOTO 999
      ENDIF

* get corners (in world coordinates)
* [PX,PY] = Point P
      WCX(1) = QWR1
      WCY(1) = QWR2

* [QX,QY] = Point Q
      WCX(3) = QWR3
      WCY(3) = QWR4

* [RX,RY] = Point R
      WCX(2) = QWR5
      WCY(2) = QWR6

*
* transform to device coordinates
      CALL GKTWD(3,WCX,WCY,DCX,DCY)
* ..and derive Pixel Coordinate Frame

* (point 1 is P; point 2 is R; point 3 is Q)

*  add fourth point
      DCX(4) = DCX(1) - DCX(2) + DCX(3)
      DCY(4) = DCY(1) - DCY(2) + DCY(3)

*  put point P into transformation
      TRNCEL(3,1) = DCX(1)
      TRNCEL(3,2) = DCY(1)

* Determine cell width and height vectors,
*    putting them into the transformation.
      TRNCEL(1,1) = (DCX(2) - DCX(1))/FLOAT(NCOLM)
      TRNCEL(1,2) = (DCY(2) - DCY(1))/FLOAT(NCOLM)
      TRNCEL(2,1) = (DCX(3) - DCX(2))/FLOAT(NROW)
      TRNCEL(2,2) = (DCY(3) - DCY(2))/FLOAT(NROW)

  999 CONTINUE
      RETURN

      END
