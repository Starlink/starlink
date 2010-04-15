*PPLOT
*
* PPLOT   -- Plots slices of a data frame in either X or Y direction.
*
* PPLOT is useful for quick examination of 2D data, determination of bias
* regions, good data regions etc.
*
* Parameters:
*
*   IMAGE       -- The data in question. Note that if you want to specify
*                  a subregion then put e.g. 'run356(1:20,400:500)' or
*                  'flat(~100,~100)' to get central 100 by 100 region
*                   etc. See NDF help for more details on this. The
*                  old method prompting for a region is still enabled if
*                  you specify a plain file.
*
*
*   XSTART      -- Lower X limit (if no section defined in IMAGE)
*
*   XEND        -- Upper X limit (if no section defined in IMAGE)
*
*   YSTART      -- Lower Y limit (if no section defined in IMAGE)
*
*   YEND        -- Upper Y limit (if no section defined in IMAGE)
*
*   DIRN        -- If 2-D data, what direction to plot slice, X or Y?
*
*   AUTO        -- Plot limits are set automatically from the data or not.
*
*   LIMITS      -- Plot limits.
*
*   DEVICE      -- Plot device. This will be ignored if one is already
*                  open.
*
* History:
*
*  NDF version created 08/01/1998 by TRM.
*
*  Changed to account for specification of region through NDF section
*  as well as old method.
*
*PPLOT
      SUBROUTINE PPLOT(STATUS)
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'CNF_PAR'
      INTEGER STATUS, IMAGE, NDIMX, IAXIS, TEMP
      PARAMETER (NDIMX=2)
      INTEGER NDIM, DIM(NDIMX), LBND(NDIMX), UBND(NDIMX)
      INTEGER IPTR, VPTR, APTR, EL, PLACE
      INTEGER AXIS, WORK1, WORK2, WPTR1, WPTR2
      INTEGER NSET, FLEN, XLO, XHI, YLO, YHI, BLOC
      LOGICAL EXIST, STATE, AUTO, BASE
      CHARACTER*1 PDIR
      REAL LIMITS(4)
      CHARACTER*128 DEVICE, TOPLABEL
      CHARACTER*64 XLABEL, YLABEL, FILE

      IF(STATUS.NE.SAI__OK) RETURN
C
C Open file, get name for later use in plot label.
C
      CALL NDF_BEGIN
      CALL NDF_ASSOC('IMAGE', 'READ', IMAGE, STATUS)
C
C Get pixels bounds of ndf
C
      CALL NDF_BOUND(IMAGE, NDIMX, LBND, UBND, NDIM, STATUS)
C
C If NDF is a base component and has 2 dimensions then
C prompt for region. If this is less than whole NDF,
C create a temporary NDF section of this size, and then
C move it back to IMAGE.
C
      CALL NDF_ISBAS(IMAGE, BASE, STATUS)
      IF(BASE .AND. NDIM.EQ.2) THEN
         CALL PAR_GDR0I('XSTART',LBND(1),LBND(1),UBND(1),
     &        .FALSE.,XLO,STATUS)
         CALL PAR_GDR0I('XEND',UBND(1),XLO,UBND(1),
     &        .FALSE.,XHI,STATUS)
         CALL PAR_GDR0I('YSTART',LBND(2),LBND(2),UBND(2),
     &        .FALSE.,YLO,STATUS)
         CALL PAR_GDR0I('YEND',UBND(2),YLO,UBND(2),
     &        .FALSE.,YHI,STATUS)

         IF(XLO.NE.LBND(1) .OR. XHI.NE.UBND(1) .OR.
     &        YLO.NE.LBND(2) .OR. YHI.NE.UBND(2)) THEN
            LBND(1) = XLO
            UBND(1) = XHI
            LBND(2) = YLO
            UBND(2) = YHI
            CALL NDF_SECT(IMAGE, 2, LBND, UBND, TEMP, STATUS)
            CALL NDF_ANNUL(IMAGE, STATUS)
            CALL NDF_CLONE(TEMP, IMAGE, STATUS)
         END IF
      END IF
C
C IMAGE now refers to an NDF covering just the region of interest.
C
      DIM(1) = UBND(1)-LBND(1)+1
      DIM(2) = UBND(2)-LBND(2)+1
      IF(NDIM.EQ.1 .OR. LBND(2).EQ.UBND(2)) THEN
         PDIR = 'X'
      ELSE IF(LBND(1).EQ.UBND(1)) THEN
         PDIR = 'Y'
      ELSE
         CALL PAR_CHOIC('DIRN','X','X,Y',.FALSE.,
     &        PDIR,STATUS)
      END IF
      IF(PDIR.EQ.'X') THEN
         IAXIS = 1
      ELSE
         IAXIS = 2
      END IF
C
C See if automatic estimation of limits wanted. If not can prompt
C immediately
C
      CALL PAR_GET0L('AUTO',AUTO,STATUS)
      IF(.NOT.AUTO) THEN
         CALL PAR_GET1R('LIMITS',4,LIMITS,NSET,STATUS)
      END IF
      CALL PAR_GET0C('DEVICE',DEVICE,STATUS)
C
C Map data
C
      CALL NDF_MAP(IMAGE,'Data','_REAL','READ',IPTR,EL,STATUS)
C
C Map variance
C
      CALL NDF_STATE(IMAGE,'Variance',EXIST,STATUS)
      IF(EXIST)
     &     CALL NDF_MAP(IMAGE,'Variance','_REAL','READ',
     &     VPTR,EL,STATUS)
C
      CALL NDF_ASTAT(IMAGE, 'Centre', IAXIS, STATE, STATUS)
      IF(STATE) THEN
         CALL NDF_AMAP(IMAGE,'Centre',IAXIS,'_REAL','READ',
     &        APTR,EL,STATUS)
      ELSE
         CALL NDF_TEMP(PLACE, STATUS)
         CALL NDF_NEW('_REAL',1,1,DIM(IAXIS),PLACE,AXIS,STATUS)
         CALL NDF_MAP(AXIS,'Data','_REAL','WRITE',APTR,EL,STATUS)
         CALL SET_AXIS(%VAL(CNF_PVAL(APTR)),
     :                 DIM(IAXIS),LBND(IAXIS),STATUS)
      END IF
C
C     Get workspace for slice
C
      CALL NDF_TEMP(PLACE, STATUS)
      CALL NDF_NEW('_REAL',1,1,DIM(IAXIS),PLACE,WORK1,STATUS)
      CALL NDF_MAP(WORK1,'Data','_REAL','WRITE',WPTR1,EL,STATUS)
      CALL NDF_TEMP(PLACE, STATUS)
      CALL NDF_NEW('_REAL',1,1,DIM(IAXIS),PLACE,WORK2,STATUS)
      CALL NDF_MAP(WORK2,'Data','_REAL','WRITE',WPTR2,EL,STATUS)
C
C Get axis label which will be used as the X label of the plot
C
      XLABEL = 'Pixels'
      CALL NDF_ACGET(IMAGE,'Label',IAXIS,XLABEL,STATUS)
C
C Get units which will be used as the Y label of the plot
C
      YLABEL = 'Image value'
      CALL NDF_CGET(IMAGE,'Units',YLABEL,STATUS)
C
C Get name of file
C
      CALL NDF_MSG('NAME', IMAGE)
      CALL MSG_LOAD('BLA','^NAME',FILE,FLEN,STATUS)
C
C     Generate plot label and then plot
C
      BLOC = INDEX(FILE,'(')
      IF(BLOC.GT.0) FLEN = BLOC - 1
      IF(NDIM.EQ.2) THEN
         IF(PDIR.EQ.'X') THEN
            WRITE(TOPLABEL,'(2A,4(A,I5))')
     &           'Mean X profile of file ',
     &           FILE(:FLEN),', region: ',
     &           LBND(1),':',UBND(1),',',LBND(2),':',UBND(2)
         ELSE
            WRITE(TOPLABEL,'(2A,4(A,I5))')
     &           'Mean Y profile of file ',
     &           FILE(:FLEN),', region: ',
     &           LBND(1),':',UBND(1),',',LBND(2),':',UBND(2)
         END IF
      ELSE IF(NDIM.EQ.1) THEN
         WRITE(TOPLABEL,'(2A,2(A,I5))') 'File: ',
     &        FILE(:FLEN),', region: ',
     &        LBND(1),':',UBND(1)
      ELSE
         WRITE(TOPLABEL,'(2A,4(A,I5))')
     &        'File: ',
     &        FILE(:FLEN),', region: ',
     &        LBND(1),':',UBND(1),',',LBND(2),':',UBND(2)
      END IF
C
C Compute and plot profile.
C
      CALL MSG_SYNC(STATUS)
      CALL PLOT_SLICE(%VAL(CNF_PVAL(IPTR)), DIM(1), DIM(2), PDIR,
     &     %VAL(CNF_PVAL(APTR)), EXIST, %VAL(CNF_PVAL(VPTR)),
     :     %VAL(CNF_PVAL(WPTR1)),
     &     %VAL(CNF_PVAL(WPTR2)), DIM(IAXIS), AUTO, LIMITS,
     &     DEVICE, XLABEL, YLABEL, TOPLABEL, STATUS)
C
C Now finish off AUTO case.
C
      IF(AUTO) THEN
         CALL PAR_DEF1R('LIMITS',4,LIMITS,STATUS)
         CALL PAR_GET1R('LIMITS',4,LIMITS,NSET,STATUS)
      END IF
C
C     Tidy up
C
      CALL NDF_END(STATUS)
      RETURN
      END

      SUBROUTINE PLOT_SLICE(IMAGE,NX,NY,PDIR,XDATA,EXIST,VAR,
     &     PLOT1,PLOT2,MXWORK,AUTO,LIMITS,DEVICE,XLABEL,YLABEL,
     &     TOPLABEL,STATUS)
C
C     Collapses region and derives suggested plot limits if needed
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INTEGER NX, NY, MXWORK, I, J, NPLOT, STATUS, NPL
      INTEGER I1, I2, NBAD
      REAL IMAGE(NX,NY), VAR(NX,NY), XDATA(1)
      LOGICAL EXIST, AUTO
      INTEGER NPIX(MXWORK)
      REAL PLOT1(MXWORK), PLOT2(MXWORK)
      REAL LIMITS(4), YMIN, YMAX, XMIN, XMAX, DIFFY, DIFFX
      REAL X1, X2, Y1, Y2
      DOUBLE PRECISION SUM, SUM1
      CHARACTER*(*) PDIR, DEVICE, XLABEL, YLABEL, TOPLABEL
      INTEGER ID, PGOPEN
*
      IF(STATUS.NE.SAI__OK) RETURN

      IF(PDIR.EQ.'X') THEN
         NPLOT = NX
      ELSE
         NPLOT = NY
      END IF
      IF(NPLOT.GT.MXWORK) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Too many points for buffers',STATUS)
         RETURN
      END IF
      NBAD = 0
      IF(PDIR.EQ.'X') THEN
        DO I = 1, NX
          PLOT1(I) = 0.
          PLOT2(I) = 0.
          NPIX(I) = 0
        END DO
        DO J = 1, NY
           DO I = 1, NX
              IF(IMAGE(I,J).NE.VAL__BADR) THEN
                 PLOT1(I) = PLOT1(I) + IMAGE(I,J)
                 IF(EXIST) PLOT2(I) = PLOT2(I) + VAR(I,J)
                 NPIX(I) = NPIX(I) + 1
              ELSE
                 NBAD = NBAD + 1
              END IF
          END DO
        END DO
        DO I = 1, NX
           IF(NPIX(I).GT.0) THEN
              PLOT1(I) = PLOT1(I)/REAL(NPIX(I))
              IF(EXIST) PLOT2(I) = SQRT(PLOT2(I))/REAL(NPIX(I))
           END IF
        END DO
      ELSE
        DO J = 1, NY
           PLOT1(J) = 0.
           PLOT2(J) = 0.
        END DO
        DO J = 1, NY
           SUM  = 0.
           NPL  = 0
           DO I = 1, NX
              IF(IMAGE(I,J).NE.VAL__BADR) THEN
                 SUM = SUM +  IMAGE(I,J)
                 IF(EXIST) SUM1 = SUM1 + VAR(I,J)
                 NPL = NPL + 1
              ELSE
                 NBAD = NBAD + 1
              END IF
           END DO
           IF(NPL.GT.0) THEN
              PLOT1(J) = REAL(SUM/REAL(NPL))
              IF(EXIST) PLOT2(J) = REAL(SQRT(SUM1)/REAL(NPL))
           END IF
           NPIX(J) = NPL
        END DO
      END IF
C
C     Compute automatic plot limits.
C     These are returned to provide better defaults next time.
C
      IF(AUTO) THEN
         YMIN =  1.E30
         YMAX = -1.E30
         DO I = 1, NPLOT
            IF(NPIX(I).GT.0) THEN
               YMIN = MIN(PLOT1(I),YMIN)
               YMAX = MAX(PLOT1(I),YMAX)
            END IF
         END DO
         DO I = 1, NPLOT
            IF(NPIX(I).EQ.0) THEN
               PLOT1(I) = (YMIN+YMAX)/2.
            END IF
         END DO
         DIFFY = 10**REAL(NINT(LOG10((YMAX-YMIN)/20.)))
         XMIN = XDATA(1)
         XMAX = XMIN
         DO I = 2, NPLOT
            XMIN = MIN(XDATA(I),XMIN)
            XMAX = MAX(XDATA(I),XMAX)
         END DO
         DIFFX = 10**REAL(NINT(LOG10((XMAX-XMIN)/20.)))
C
C     Get plot limits
C
         CALL PGLIMIT(NPLOT, XDATA, X1, X2, DIFFX)
         CALL PGLIMIT(NPLOT, PLOT1, Y1, Y2, DIFFY)
         LIMITS(1) = X1
         LIMITS(2) = X2
         LIMITS(3) = Y1
         LIMITS(4) = Y2
      END IF
C
C     Plot
C
      ID = PGOPEN(DEVICE)
      IF(ID.LE.0) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC('DEVICE',DEVICE)
         CALL ERR_REP(' ','Failed to open plot device ^DEVICE',
     &        STATUS)
         RETURN
      END IF
      CALL PGSCI(5)
      CALL PGENV(LIMITS(1),LIMITS(2),LIMITS(3),LIMITS(4),0,0)
      CALL PGSCI(7)
      CALL PGLABEL(XLABEL,YLABEL,TOPLABEL)
      CALL PGSCI(1)
      I1 = 1
      DO WHILE(I1.LT.NPLOT)
         DO WHILE(NPIX(I1).EQ.0 .AND. I1.LT.NPLOT)
            I1 = I1 + 1
         END DO
         IF(NPIX(I1).GT.0) THEN
            I2 = I1
            DO WHILE(I2+1.LE.NPLOT .AND. NPIX(I2+1).GT.0)
               I2 = I2 + 1
            END DO
            CALL PGBIN(I2-I1+1, XDATA(I1), PLOT1(I1), .TRUE.)
            IF(EXIST) THEN
               CALL PGSCI(2)
               CALL PGBIN(I2-I1+1, XDATA(I1), PLOT2(I1), .TRUE.)
               CALL PGSCI(1)
            END IF
            I1 = I2 + 1
         END IF
      END DO
      CALL PGIDEN
      CALL PGCLOS
      IF(NBAD.EQ.0) THEN
         CALL MSG_OUT(' ',
     &        'There were no bad pixels in selected region',
     &        STATUS)
      ELSE IF(NBAD.EQ.1) THEN
         CALL MSG_OUT(' ',
     &        'There was one bad pixel in selected region',
     &        STATUS)
      ELSE IF(NBAD.GT.1) THEN
         CALL MSG_SETI('NBAD',NBAD)
         CALL MSG_OUT(' ',
     &        'There were ^NBAD bad pixels in selected region',
     &        STATUS)
      END IF

      RETURN
      END
C
C Generate fake axis. Pixel I has value I+LBND-1
C
      SUBROUTINE SET_AXIS(AXIS,NPIX,LBND,STATUS)
      INCLUDE 'SAE_PAR'
      INTEGER I, NPIX, STATUS, LBND
      REAL AXIS(NPIX)

      IF(STATUS.NE.SAI__OK) RETURN

      DO I = 1, NPIX
         AXIS(I) = REAL(I+LBND-1)
      END DO
      RETURN
      END
