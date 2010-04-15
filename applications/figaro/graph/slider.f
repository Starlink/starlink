      SUBROUTINE SLIDER(TITLE,METHOD,COORDS,VALUE,OLDVAL)
*+
*  Name:
*     SLIDER

*  Purpose:
*     Produce or manipulate slider

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SLIDER(TITLE,METHOD,COORDS,VALUE,OLDVAL)

*  Description:
*     This will plot a slider or move the indicator in it.
*     It is assumed that the values of Y1 and Y2 in COORDS represent the
*     ends of the arrows, and that the ends of the sliders proper are
*     0.04 within those.

*  Arguments:
*     TITLE = CHARACTER*(*) (Given)
*        Title for slider
*     METHOD = CHARACTER (Given)
*        What to do, either 'D' for draw or 'M' for move, either case
*     COORDS(6) = REAL ARRAY (Given)
*        Coordinates for slider, X1, X2, Y1, Y2, MINVAL, MAXVAL
*     VALUE = INTEGER (Given)
*        Value to indicate on slider
*     OLDVAL = INTEGER (Given)
*        Old value indicated on slider (method "M" only)

*  Authors:
*     TNW: T.N.Wilkins (Durham)
*     {enter_new_authors_here}

*  History:
*     9-FEB-1994 (TNW):
*        Original version.
*-
      IMPLICIT NONE
      CHARACTER*(*) TITLE
      CHARACTER METHOD
      REAL COORDS(6)
      INTEGER VALUE,OLDVAL

* Local

      REAL TRIX(3),TRIY(3)
      CHARACTER CHR_UPPER,UMETHD
      REAL YLOWER,YUPPER,DELTA,Y1,Y2,YDUPPER,YDLOWER

      UMETHD = CHR_UPPER(METHOD)

      YLOWER = COORDS(5) - 0.5
      YUPPER = COORDS(6) + 0.5

      YDUPPER = COORDS(4) - 0.04
      YDLOWER = COORDS(3) + 0.04

      DELTA = 0.005 * (YUPPER - YLOWER) / (YDUPPER - YDLOWER)

      IF(UMETHD.EQ.'D') THEN

* Draw slider

         CALL PGVPORT(COORDS(1),COORDS(2),YDLOWER,YDUPPER)
         CALL PGWINDOW(0.0,1.0,YLOWER,YUPPER)
         CALL PGBOX('BC',0.0,0,'BCINTSV',0.0,0)

*  Indicate value

         Y1 = REAL(VALUE) - DELTA
         Y2 = Y1 + DELTA + DELTA
         CALL PGSFS(1)
         CALL PGRECT(0.1,0.9,Y1,Y2)
         CALL PGMTEXT('T',1.8,0.5,0.5,TITLE)
         CALL PGVPORT(0.0,1.0,0.0,1.0)
         CALL PGWINDOW(0.0,1.0,0.0,1.0)

* Put arrows on ends

         TRIX(1) = COORDS(1)
         TRIX(2) = COORDS(2)
         TRIX(3) = (COORDS(1)+COORDS(2))*0.5
         TRIY(1) = COORDS(3) + 0.03
         TRIY(2) = TRIY(1)
         TRIY(3) = COORDS(3)
         CALL PGPOLY(3,TRIX,TRIY)

*  TRIX unchanged

         TRIY(1) = COORDS(4) - 0.03
         TRIY(2) = TRIY(1)
         TRIY(3) = COORDS(4)
         CALL PGPOLY(3,TRIX,TRIY)

      ELSE IF(UMETHD.EQ.'M') THEN

*   Move slider

         CALL PGVPORT(COORDS(1),COORDS(2),YDLOWER,YDUPPER)
         CALL PGWINDOW(0.0,1.0,YLOWER,YUPPER)

*   Erase old value

         Y1 = REAL(OLDVAL) - DELTA
         Y2 = Y1 + DELTA + DELTA
         CALL PGSFS(1)
         CALL PGSCI(0)

*   Draw new value

         CALL PGRECT(0.1,0.9,Y1,Y2)
         Y1 = REAL(VALUE) - DELTA
         Y2 = Y1 + DELTA + DELTA
         CALL PGSCI(1)
         CALL PGRECT(0.1,0.9,Y1,Y2)
      ENDIF
      END
