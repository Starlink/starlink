C
      SUBROUTINE GAUS_XZPLOT(XVALS,GXVALS,ZVALS,ERRORS,
     :    ZRESID,CONVALS,ICONO,GAUSUM,GAUFS,GZRESID,NX,CX,GX,NG,
     :    ICST,IGST,HIGH,LOW,HIGHR,LOWR,XLAB,ZLAB,PLAB,ERASE,DEVICES,
     :    HARD,DEVICEH,ERRUSE,LCON,CRES,GRES,GTOT,GALL,XVST,XVEN,
     :    STATUS)
C
C     G A U S _ X Z P L O T
C
C     Plots an array (ZVALS) against another array (XVALS) in a lower
C     plot ( occupying 0.75 of vertical plotting area ). The continuum
C     fit, sum of fitting Gaussians and individual fiting Gaussians
C     can also be ploted in the lower plot. The errors on the Z values
C     ( if available ) are plotted on the upper plot ( occupying 0.25
C     of vertical plotting space ) as well as residuals on the line and
C     continuum fit. The XVALS represent the coordinates at the centre
C     of each 'bin'.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) XVALS    (Real array) The abscissae for the plotted points.
C     (>) GXVALS   (Real array) The x-coordinates of line points
C     (>) ZVALS    (Real array) The observed data to be plotted.
C     (>) ERRORS   (Real array) The Y errors on the observed data.
C     (>) ZRESID   (Real array) The observed -fitted residuals on the
C                  continuum fit ( values over the line not plotted )
C     (>) CONVALS  (Real array) The Y values of the fitted continuum
C     (>) ICONO    (Integer array) A value of 1 indicates a continuum point
C     (>) GAUSUM   (Real array) The Y values of the sum of the fitting
C                  Gaussians
C     (>) GAUFS    (Real array) The Y values of the individual fitting
C     (>) GZRESID  (Real array) The residuals on the line fit
C     (>) NX       (Integer) Number of elements in XVALS and ZVALS.
C     (>) CX       (Integer) Number of points in fitted continuum
C     (>) GX       (Integer) Number of elements in GXVALS
C     (>) NG       (Integer) Number of Gaussians in fit
C     (>) ICST     (Integer) The first element of fitted continuum to be plotted
C     (>) IGST     (Integer) The first element of GXVALS to be plotted
C     (>) HIGH     (Real) The maximum value for the plot.
C     (>) LOW      (Real) The minimum value for the plot.
C     (>) HIGHR    (Real) The maximum value for the residuals plot.
C     (>) LOWR     (Real) The minimum value for the residuals plot.
C     (>) XLAB     (Character) The X-label for the plot.
C     (>) ZLAB     (Character) The Z-label for the plot.
C     (>) PLAB     (Character) The label for the plot as a whole.
C     (>) ERASE    (Logical) True if device is to be erased first.
C     (>) DEVICES  (Character) The device/type to be used for the
C                  soft plot - see PGPLOT documentation for details.
C     (>) DEVICEH  (Character) The device/type to be used for the
C                  hard plot - see PGPLOT documentation for details.
C     (>) HARD     (Logical) True if a hard copy is to be made
C     (>) ERRUSE   (Logical) True if erros are available
C     (>) LCON     (Logical) True if continuum is to be plotted
C     (>) CRES     (Logical) True if continuum residuals are to be plotted.
C     (>) GRES     (Logical) True if line fit residuals are to be plotted.
C     (>) GTOT     (Logical) True if sum of fitting Gaussians is to be plotted
C     (>) GALL     (Logical) True if each fitting Gaussian is to be plotted
C     (>) XVST     (Real) The actual x-start value for the plot.
C     (>) XVEN     (Real) The actual x-end value for the plot.
C     (<) STATUS   (Integer) Returns plot status.
C                  0 => OK, non zero => some error opening the plot.
C
C                                       JRW / AAO February 1987
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL HARD,ERASE,ERRUSE,LCON,CRES,GRES,GTOT,GALL
      INTEGER ICST,IGST,NX,CX,GX,NG,ICONO(NX),STATUS
      REAL XVALS(NX),GXVALS(GX),ZVALS(NX),ZRESID(NX),
     :ERRORS(NX),CONVALS(NX),GAUSUM(GX),GAUFS(GX,NG),GZRESID(GX),
     :HIGH,LOW,HIGHR,LOWR,XVST,XVEN
      CHARACTER*(*) DEVICES,DEVICEH,XLAB,ZLAB,PLAB
C
C     Functions
C
      INTEGER PGBEGIN
C
C     Colour for axes
C
      INTEGER WHITE,RED,YELL
      PARAMETER (WHITE=1,RED=2,YELL=7)
C
C     Pen thickness
C
      INTEGER THICK,THIN
      PARAMETER (THICK=2,THIN=1)
C
C     Title for residuals plot
C
      CHARACTER RLAB*14
C
C     Local variables
C
      INTEGER I,J,CKEY,SYM,IGNORE
      REAL RFAC,XP(4096),ZP(4096),YVAL1,YVAL2
C
C     Is this a 'hard' plot?
C
      IF (.NOT.HARD) THEN
C
C        First do the lower spectrum plot
C
         IF (ERASE) CALL PGADVANCE
C
C        If to be erased clear whole plot
C
         CALL PGSCI(WHITE)
         CALL PGSLW(THIN)
         CALL PGVPORT(0.08,0.97,0.09,0.74)
         CALL PGWINDOW(XVST,XVEN,LOW,HIGH)
         IF (ERASE) THEN
           CALL PGBOX('ABCINST',0.,0,'BCINST',0.,0)
           CALL PGLABEL(XLAB,ZLAB,' ')
         END IF
C
C        Whole observed spectrum as a histo plot
C
         IF (ERASE) THEN
           CALL PGBIN(NX,XVALS,ZVALS,.TRUE.)
         END IF
C
C        Continuum fit as a line plot
C
         IF (LCON) THEN
           CALL PGSCI(RED)
           J=1
           DO I=ICST,ICST+CX-1,1
             XP(J)=XVALS(I)
             ZP(J)=CONVALS(I)
             J=J+1
           END DO
           CALL PGLINE(CX,XP,ZP)
           CALL PGSCI(WHITE)
         END IF

         IF (GTOT) THEN
C
C        Sum of fitting Gaussians in RED as a line plot over the extent
C        of the line
C
           CALL PGSCI(RED)
           CALL PGLINE(GX,GXVALS,GAUSUM)
           CALL PGSCI(WHITE)
         END IF

         IF (GALL) THEN
C
C          Individual fitting Gaussians in increasing pen order ( above 3 )
C          over the extent of the line
C
           CKEY=3
           DO I=1,NG
             CALL PGSCI(CKEY)
             DO J=1,GX
               ZP(J)=GAUFS(J,I)
             END DO
             CALL PGLINE(GX,GXVALS,ZP)
             CKEY=CKEY+1
             IF (CKEY.GT.13) THEN
               CKEY=3
             END IF
           END DO
           CALL PGSCI(WHITE)
         END IF
C
C  Next the upper residual spectrum plot over the extent of the continuum
C
         CALL PGSLW(THIN)
         CALL PGVPORT(0.08,0.97,0.75,0.92)
C
C        Set up the scaling factor for the residuals plot in terms
C        of the lower spectrum plot and write to the label for the
C        upper plot ( if this is non-blank )
C
         RFAC=REAL(NINT(ABS((HIGH-LOW)/(HIGHR-LOWR))))
         IF (RFAC.EQ.0.0) THEN
           RFAC=1.0
         END IF
         WRITE(RLAB,11,IOSTAT=IGNORE) RFAC
11       FORMAT('Resid.*',F4.0)
         RFAC=(ABS((HIGH-LOW)/(HIGHR-LOWR)))/RFAC
         CALL PGWINDOW(XVST,XVEN,LOWR,HIGHR)
         IF (ERASE) THEN
           CALL PGBOX('ABC',0.,0,'BCINST',0.,0)
           IF (ERRUSE) THEN
             CALL PGLABEL(' ',RLAB,PLAB)
           END IF
           IF (.NOT.ERRUSE) THEN
             IF (.NOT.CRES) THEN
               CALL PGLABEL(' ','RESIDS.',PLAB)
             ELSE
               CALL PGLABEL(' ',RLAB,PLAB)
             END IF
           END IF
C
C          If available plot error bars for whole plot
C
           IF (ERRUSE) THEN
             CALL PGSCI(YELL)
             DO I=1,NX
               YVAL1=RFAC*(0.0-ERRORS(I))
               YVAL2=RFAC*(0.0+ERRORS(I))
               CALL PGERRY(1,XVALS(I),YVAL1,YVAL2,0.0)
             END DO
           END IF
           CALL PGSCI(WHITE)
         END IF
C
C        Plot residuals on continuum over extent of continuum but
C        excluding lines
C
         IF (CRES) THEN
           DO I=ICST,ICST+CX-1,1
             IF (ICONO(I).EQ.1) THEN
               CALL PGPOINT(1,XVALS(I),RFAC*ZRESID(I),5)
             END IF
           END DO
         END IF
C
C        Plot residuals on profile fit over line extent
C
         IF (GRES) THEN
           DO I=1,GX
             ZP(I)=GZRESID(I)*RFAC
           END DO
           CALL PGPOINT(GX,GXVALS,ZP,5)
         END IF
         CALL PGUPDT(2)
         GO TO 600
      ELSE
C
C        A 'hard' plot.  Close down the soft plot and re-open
C        PGPLOT on the HARD device
C
         CALL PGEND
         STATUS=PGBEGIN(0,DEVICEH,1,1)
         IF (STATUS.NE.1) THEN
           CALL PAR_WRUSER('Error opening hard device',STATUS)
           GO TO 590
         END IF
C
C        First do the lower spectrum plot
C
         CALL PGSCI(WHITE)
         CALL PGSLW(THIN)
         CALL PGVPORT(0.08,0.97,0.09,0.74)
         CALL PGWINDOW(XVST,XVEN,LOW,HIGH)
         CALL PGBOX('ABCINST',0.,0,'BCINST',0.,0)
         CALL PGLABEL(XLAB,ZLAB,' ')
C
C        Whole observed spectrum as a histo plot
C
         CALL PGBIN(NX,XVALS,ZVALS,.TRUE.)
C
C        Continuum fit as a line plot
C
         J=1
         DO I=ICST,ICST+CX-1,1
           XP(J)=XVALS(I)
           ZP(J)=CONVALS(I)
           J=J+1
         END DO
         CALL PGLINE(CX,XP,ZP)
C
C        Sum of fitting Gaussians in RED as a line plot over the extent
C        of the line
C
         CALL PGLINE(GX,GXVALS,GAUSUM)
C
C        Individual fitting Gaussians in increasing symbol number order
C        over the extent of the line
C
         SYM=1
         DO I=1,NG
           DO J=1,GX
             ZP(J)=GAUFS(J,I)
           END DO
           CALL PGPOINT(GX,GXVALS,ZP,SYM)
           SYM=SYM+1
         END DO
C
C  Next the upper residual spectrum plot over the extent of the continuum
C
         CALL PGSLW(THIN)
         CALL PGVPORT(0.08,0.97,0.75,0.92)
C
C        Set up the scaling factor for the residuals plot in terms
C        of the lower spectrum plot and write to the label for the
C        upper plot ( if this is non-blank )
C
         RFAC=REAL(NINT(ABS((HIGH-LOW)/(HIGHR-LOWR))))
         IF (RFAC.EQ.0.0) THEN
           RFAC=1.0
         END IF
         WRITE(RLAB,11,IOSTAT=IGNORE) RFAC
         RFAC=(ABS((HIGH-LOW)/(HIGHR-LOWR)))/RFAC
         CALL PGWINDOW(XVST,XVEN,LOWR,HIGHR)
         CALL PGBOX('ABC',0.,0,'BCINST',0.,0)
         IF (ERRUSE) THEN
           CALL PGLABEL(' ',RLAB,PLAB)
         END IF
         IF (.NOT.ERRUSE) THEN
           CALL PGLABEL(' ',RLAB,PLAB)
         END IF
C
C        If available plot error bars for whole plot
C
         IF (ERRUSE) THEN
           DO I=1,NX
             YVAL1=RFAC*(0.0-ERRORS(I))
             YVAL2=RFAC*(0.0+ERRORS(I))
             CALL PGERRY(1,XVALS(I),YVAL1,YVAL2,0.0)
           END DO
         END IF
C
C        Plot residuals on continuum over extent of continuum but
C        excluding lines
C
         DO I=ICST,ICST+CX-1,1
           IF (ICONO(I).EQ.1) THEN
             CALL PGPOINT(1,XVALS(I),RFAC*ZRESID(I),5)
           END IF
         END DO
C
C        Plot residuals on profile fit over line extent
C
         DO I=1,GX
           ZP(I)=GZRESID(I)*RFAC
         END DO
         CALL PGPOINT(GX,GXVALS,ZP,5)
C
C        Close down plot
C
         CALL PGEND
         GO TO 600
      END IF
C
C     Error exit for hard plot
C
590   CALL PAR_WRUSER(' No hard plot produced',STATUS)
C
C     Succesful exit
C
600   STATUS=0
      END
