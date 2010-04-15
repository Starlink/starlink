      SUBROUTINE GRGRA2(A,IDIM,JDIM,I1,I2,J1,J2,FOREGR,BACKGR,TR,
     :                  MINCI, MAXCI, MODE)
*+
*
*     - - - - - - - -
*       G R G R A 2     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Plots a cell array
*
*   Given
*      A        r()   Data array
*      IDIM     i     First dimension of A
*      JDIM     i     Second dimension of A
*      I1       i     Array subset
*      I2       i       "     "
*      J1       i       "     "
*      J2       i       "     "
*      FOREGR   r     Data value to map to foreground colour
*      BACKGR   r     Data value to map to background colour
*      MINCI    i     Lowest colour index to use
*      MAXCI    i     Highest colour index to use
*      TR       r()   transformation from array indices to world coordinates
*
*   Read from COMMON
*      GRCIDE   i     Current device
*      GRXORG   r()   x origin
*      GRYORG   r()   y origin
*      GRXSCL   r()   x scale
*      GRYSCL   r()   y scale
*      GRVPVI   l()   Viewport visible
*
*   Constants from GKS_PAR
*      GREALI   i     Realized value
*
*   Externals
*      GQASWK GCRSG GSSG GCLSG GCA GQCR GSCR GRGRA1 GRQREP GRCOCI
*      ERR_REP PSX_CALLOC PSX_FREE
*
*   D.L.Terrett  Starlink  Aug 1987
*       Fixed calculation of segment transformation
*         "        "       " world coordinates for no-rotation case
*         "   drawing of cell array outline
*         "   transformation in no rotation case
*   D.L.Terrett  Starlink  May 1988
*       Use GQSGA instead of GQASWK because of GKS bug
*       Change buffer size to avoid GKS cell array bug
*   D.L.Terrett  Starlink Jun 1988
*       Simulate with random dotting on non grey-scale devices
*   D.L.Terrett  Starlink May 1990
*       Increase buffer size
*   D.L.Terrett  Starlink Dec 1990
*       Remove redundant code
*       Attempt to work around stripes problems
*   D.L.Terrett  Starlink Feb 1991
*       Don't use segments for flipped case
*   D.L.Terrett  Starlink Oct 1991
*       Allocate dynamic workspace with PSX and nail the strips problem at
*       last
*   D.L.Terrett  Starlink Jan 1992
*       Plot cell arrays the correct way up
*   D.L.Terrett  Starlink Feb 1995
*       Change transformation to match pgplot 5.0
*   T. Jenness   JACH     Aug 2004
*       Use CNF_PVAL for %val
*+
      IMPLICIT NONE

      INTEGER IDIM, JDIM, I1, I2, J1, J2, PNTR, ISTAT, MODE
      INTEGER MINCI, MAXCI
      REAL A(IDIM,JDIM), BACKGR, FOREGR, XSC, YSC
      REAL GTR(2,3), TR(6)
      REAL WINDO(4), VIEWP(4)

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'

      INCLUDE 'GKS_PAR'

      INCLUDE 'SAE_PAR'

      INCLUDE 'CNF_PAR'     ! For CNF_PVAL

      REAL X1, X2, Y1, Y2
      INTEGER NSEG, IERR, NL, IWK
      LOGICAL ROTATE

* Save the segment name so that we can avoid re-using the same name.
* (causes a crash with the X windows workstation)
      SAVE NSEG
      DATA NSEG/0/


      IF (GRVPVI(GRCIDE)) THEN


*   Inquire the current normalization transformation
         CALL GQNT(TRN,IERR,WINDO,VIEWP)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRGRA2', 'GQNT', IERR)
            GO TO 9999
         END IF

*      See if array is rotated
         ROTATE = (TR(3).NE.0.0) .OR. (TR(5).NE.0.0)

*      If it is we need a segment to do the rotation
         IF (ROTATE) THEN

*         Find a free segment name
   30       CONTINUE
               NSEG = NSEG + 1
               CALL GQASWK(NSEG,1,IERR,NL,IWK)
               IF (IERR.EQ.0) GO TO 30

*         Create it
            CALL GCRSG(NSEG)

*         Calculate the WC to NDC scaling
            XSC = (VIEWP(2)-VIEWP(1))/(WINDO(2)-WINDO(1))
            YSC = (VIEWP(4)-VIEWP(3))/(WINDO(4)-WINDO(3))

*         The only place we can do the general transformation is with
*         the segment transformation which transforms NDC to NDC. The
*         transformation supplied is from array coordinates to scaled
*         coordinates so we have to do some sums to find the correct
*         segment transformation.
*
*         If  A = array coords to absolute coords (= GKS WC)
*             C = WC to NDC
*             D = segment transformation (NDC to NDC)
*         then
*             A.C = C.D
*
*         fortunately only A and D have a rotational component and C
*         has only scaling and no origin shift. Hence (not forgeting
*         that the GKS transformation matrix is in a different order)
*
            GTR(1,1) = TR(2)
            GTR(1,2) = TR(3)*XSC/YSC
            GTR(1,3) = TR(1)*XSC
            GTR(2,2) = TR(6)
            GTR(2,1) = TR(5)*YSC/XSC
            GTR(2,3) = TR(4)*YSC

*         Set the segment transformation
            CALL GSSGT(NSEG,GTR)

         END IF

*      We have to copy the array to a local buffer to convert it to
*      the appropriate colour indices

*               Allocate workspace (PGGRAY has already checked that I1 etc
*               are sensible)
            ISTAT = SAI__OK
            CALL PSX_CALLOC( (I2 - I1 + 1) * (J2 - J1 + 1),
     :               '_INTEGER', PNTR, ISTAT)
            IF (ISTAT.NE.SAI__OK) THEN
               CALL ERR_REP('GRDYNE',
     :                  'GRGRA2 - Unable to allocate dynamic memory',
     :                  GRDYNE)
               GO TO 9999
            END IF

*         Copy the data from the input data array to the workspace
*         converting the real numbers to GKS colour indices
            CALL GRCOCI(A, IDIM, JDIM, I1, I2, J1, J2, BACKGR,
     :                FOREGR, MINCI, MAXCI, MODE, %VAL(CNF_PVAL(PNTR)))

*         Calculate corners of this cell array fragment
            IF (ROTATE) THEN
               X1 = REAL(I1)-0.5
               Y1 = REAL(J1)-0.5
               X2 = REAL(I2)+0.5
               Y2 = REAL(J2)+0.5
            ELSE

*            For non rotated case transform corners to World Coordinates
               X1 = (REAL(I1)-0.5) * TR(2) + TR(1)
               Y1 = (REAL(J1)-0.5) * TR(6) + TR(4)
               X2 = (REAL(I2)+0.5) * TR(2) + TR(1)
               Y2 = (REAL(J2)+0.5) * TR(6) + TR(4)
            END IF

*         Plot the cell array
            CALL GCA(X1, Y1, X2, Y2, I2-I1+1, J2-J1+1, 1, 1,
     :               I2-I1+1, J2-J1+1, %VAL(CNF_PVAL(PNTR)))

*         Release the dynamic memory
            CALL PSX_FREE( PNTR, ISTAT)

*       Close the segment
         IF (ROTATE) THEN
             CALL GCLSG
         ELSE

*        Restore the Window
            CALL GSWN(TRN,WINDO(1),WINDO(2),WINDO(3),WINDO(4))

*  bug fix
*     This shouldn't be necessary but if I don't do it the segment
*     transformation still applies
            CALL GSELNT(0)
            CALL GSELNT(1)
*  end bugfix
         ENDIF
      ENDIF

 9999 CONTINUE
      END
