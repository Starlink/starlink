      SUBROUTINE GRGRA1 (A, IDIM, JDIM, I1, I2, J1, J2, BLACK, WHITE,
     :                   TR, MODE)
*+
*     - - - - - - - -
*       G R G R A 1     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Draw gray-scale map of an array in current window. Array values between
*   BLACK and WHITE are shaded in gray levels determined by linear
*   interpolation. BLACK may be either less than or greater than WHITE.  Array
*   values outside the range BLACK to WHITE are shaded black or white as
*   appropriate.
*
*   GRGRA1 calculates a random number for each pixel of the picture, and can
*   therefore be very slow. The result is rather more successful on some
*   devices than others.
*
*
*   Given
*      A        r() The array to be plotted.
*      IDIM     i   The first dimension of array A.
*      JDIM     i   The second dimension of array A.
*      I1, I2   i   The inclusive range of the first index (I) to be plotted.
*      J1, J2   i   The inclusive range of the second index (J) to be plotted.
*      BLACK    r   The array value which is to appear black (all pixels
*                   filled in).
*      WHITE    r   The array value which is to appear white (no pixels
*                   filled in).
*      TR       r   Transformation matrix between array grid and world
*                   coordinates (see GRCONT).
*      MODE     i   Transfer function.
*
* N.B. Arguments are assumed to be valid (checked by PGGRAY).
*
*   D.L.Terrett  Starlink  Apr 1991 (after TJP)
*   D.L.Terrett  Starlink  Feb 1995
*        Changed for new transformation definition in pgplot 5.0 and
*        add support for transfer function.
*+
      IMPLICIT NONE
      INTEGER IDIM, JDIM, I1, I2, J1, J2, MODE
      REAL    A(IDIM,JDIM)
      REAL    BLACK, WHITE
      REAL TR(6), FAC, FACL
      PARAMETER (FAC=65000.0)

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'

      INCLUDE 'SAE_PAR'


      INTEGER  I,IX,IX1,IX2,IY,IY1,IY2,J, IERR, IRAN, MAXRAN, ISTAT
      REAL     DEN,VALUE,BW,VIEWP(4),WINDO(4),X(2),Y(2),FRAN
      REAL     XXAA,XXBB,YYAA,YYBB,XYAA,XYBB,YXAA,YXBB,XYAAIY,YXAAIY
      LOGICAL  PENUP

      ISTAT = SAI__OK

      CALL GQNT(TRN,IERR,WINDO,VIEWP)
      IF (IERR.NE.0) THEN
         CALL GRQREP('GRGRA1', 'GQNT', IERR)
         GO TO 9999
      END IF

      IX1 = INT(WINDO(1))
      IX2 = INT(WINDO(2))
      IY1 = INT(WINDO(3))
      IY2 = INT(WINDO(4))
      DEN = TR(2)*TR(6)-TR(3)*TR(5)

*   Calculate constants.
      BW = BLACK-WHITE
      FACL = LOG(1.0+FAC)
      XXAA = -TR(6)*TR(1)/DEN
      XXBB = TR(6)/DEN
      XYAA = -TR(3)*TR(4)/DEN
      XYBB = TR(3)/DEN
      YYAA = -TR(2)*TR(4)/DEN
      YYBB = TR(2)/DEN
      YXAA = -TR(5)*TR(1)/DEN
      YXBB = TR(5)/DEN

      IF(TR(3).EQ.0..AND.TR(5).EQ.0.) THEN

*     Vector plotting, without rotation.
         DO 20 IY=IY1,IY2
            J = NINT(YYAA+YYBB*IY)
            IF (J.GE.J1.AND.J.LE.J2) THEN
               PENUP = .TRUE.
               Y(2) = REAL(IY)
               Y(1) = REAL(IY)
               DO 10 IX=IX1,IX2
                  I = NINT(XXAA+XXBB*IX)
                  IF (I.GE.I1.AND.I.LE.I2) THEN
                     VALUE = (A(I,J)-WHITE)/(BW)
                     IF (MODE.EQ.0) THEN
*                     -- "linear"
                        CONTINUE
                     ELSE IF (MODE.EQ.1) THEN
*                     -- "logarithmic"
                        VALUE = LOG(1.0+FAC*VALUE)/FACL
                     ELSE IF (MODE.EQ.2) THEN
*                     -- "square root"
                        VALUE = SQRT(VALUE)
                     END IF
                     CALL PSX_RAND( IRAN, MAXRAN, FRAN, ISTAT)
                     IF (VALUE.GT.0 .AND. VALUE.GT.FRAN) THEN
                        IF (PENUP) THEN
                           X(1) = REAL(IX)
                           PENUP = .FALSE.
                        END IF
                        X(2) = REAL(IX)
                     ELSE
                        IF (.NOT.PENUP) THEN
                           CALL GPL(2,X,Y)
                           PENUP = .TRUE.
                        END IF
                     END IF
                  END IF
   10          CONTINUE
               IF (.NOT.PENUP) CALL GPL(2,X,Y)
            END IF
   20    CONTINUE

      ELSE

*     Vector plotting, with rotation.
         DO 120 IY=IY1,IY2
            XYAAIY = XXAA-XYAA-XYBB*IY
            YXAAIY = YYAA+YYBB*IY-YXAA
            Y(2) = REAL(IY)
            Y(1) = REAL(IY)
            PENUP = .TRUE.
            DO 110 IX=IX1,IX2
               I = NINT(XYAAIY+XXBB*IX)
               IF (I.GE.I1.AND.I.LE.I2) THEN
                  J = NINT(YXAAIY-YXBB*IX)
                  IF (J.GE.J1.AND.J.LE.J2) THEN
                     VALUE = (A(I,J)-WHITE)/(BW)
                     CALL PSX_RAND( IRAN, MAXRAN, FRAN, ISTAT)
                     IF (VALUE.GT.0 .AND. VALUE.GT.FRAN) THEN
                        IF (PENUP) THEN
                           X(1) = REAL(IX)
                           PENUP = .FALSE.
                        END IF
                        X(2) = REAL(IX)
                     ELSE
                        IF (.NOT.PENUP) THEN
                           CALL GPL(2,X,Y)
                           PENUP = .TRUE.
                        END IF
                     END IF
                  END IF
               END IF
  110       CONTINUE
            IF (.NOT.PENUP) CALL GPL(2,X,Y)
  120     CONTINUE
      END IF
 9999 CONTINUE
      END
