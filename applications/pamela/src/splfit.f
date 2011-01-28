      SUBROUTINE SPLCALC( NDATA, XDATA, XKNOT, CSPLINE, NCAP7,
     &     YFIT, IFAIL )
*     
*     EVALUATES SPLINE FIT
*     
*     Input:
*     NDATA      = NUMBER OF DATA PAIRS
*     XDATA      = DATA X VALUES
*     XKNOT      = KNOT X VALUES
*     CSPLINE    = SPLINE COEFFS
*     NCAP7      = NUMBER OF KNOTS
*     Output:
*     YFIT      = CORRESPONDING Y VALUE OF SPLINE FIT
*     IFAIL      = 0 IF SUCCESSFUL, 1 IF FAILED.
*     
*     
      IMPLICIT NONE
      INTEGER NDATA, IFAIL
      REAL  XDATA(NDATA), YFIT(NDATA)
*     
      INTEGER NCAP7, I,  KNOT
      DOUBLE PRECISION DERIV
      DOUBLE PRECISION XKNOT(NCAP7), CSPLINE(NCAP7-4)
      DOUBLE PRECISION WORK(12)
      DOUBLE PRECISION PDA_DBVALU
      INTEGER INBV,K
 
      INBV = 1
      IFAIL = 0
      K=4
C
C     Loop thru the data
C
      DO I=1,NDATA
C         
C     Evaluate spline at desired x-value
C     

         IF( XDATA(I).GE.XKNOT(1).AND.
     &        XDATA(I).LE.XKNOT(NCAP7) ) THEN
            YFIT(I)  = REAL(PDA_DBVALU(XKNOT,CSPLINE,NCAP7-K
     &           ,K,0,DBLE(XDATA(I)),INBV,WORK,IFAIL))
            IF( IFAIL.NE.0) THEN
               WRITE(*,*) '** PDA_DBVALU failed.(1) IFAIL=',IFAIL
               print *,' XDATA(',I,') = ',XDATA(I)
               print *,' XKNOT(1) = ', XKNOT(1)
               print *,' XKNOT(',NCAP7,') = ', XKNOT(NCAP7)
               RETURN  
            END IF
            
         ELSE
C            
C     Evaluate spline and first derivative at the end knot
C
            IF (XDATA(I).GT.XKNOT(NCAP7)) THEN
               KNOT=NCAP7
            ELSE 
               KNOT = 1
            ENDIF
            
            YFIT(I) = REAL(PDA_DBVALU(XKNOT,CSPLINE,NCAP7-4
     +           ,4,0,XKNOT(KNOT),INBV,WORK,IFAIL))
            IF( IFAIL.NE.0) THEN
               WRITE(*,*) '** PDA_DBVALU Failed.(2) IFAIL=',IFAIL
               print *,xknot(1),xknot(knot),xknot(ncap7)
               RETURN
            ENDIF
            
            DERIV = PDA_DBVALU(XKNOT,CSPLINE,NCAP7-4
     +           ,4,1,XKNOT(KNOT),INBV,WORK,IFAIL)
            IF( IFAIL.NE.0) THEN
               WRITE(*,*) '** PDA_DBVALU Failed.(3) IFAIL=',IFAIL
               RETURN
            ENDIF
C            
C     Linear extrapolation to desired point
C
            
            YFIT(I) = YFIT(I) + REAL(DERIV*(XDATA(I)-XKNOT(KNOT)))
         END IF     
      ENDDO
      RETURN
      END

      SUBROUTINE SPLFIT( NDATA, XDATA, YDATA, YSIGMA,
     *     YFIT, KNOTREQ, IFAIL )
*     
*     COMPUTES WEIGHTED LEAST-SQUARES SPLINE FIT TO DATA PAIRS
*     
*     Input:
*     NDATA   = NUMBER OF DATA PAIRS
*     XDATA   = DATA X VALUES
*     YDATA   = DATA Y VALUES
*     YSIGMA  = UNCERTAINTY IN Y (1-SIGMA) (NEGATIVE TO REJECT)
*     KNOTREQ = NUMBER OF SPLINES REQUESTED
*     Output:
*     YFIT    = FITTED Y VALUES
*     RMS     = RMS NORMALIZED RESIDUAL OF POINTS NOT REJECTED
*     IFAIL   = 0 IF SUCCESSFUL, 1 IF FAILED.
*     
*     
*     SEPT 1984 SORTING AND EXTRAPOLATION ADDED BY KDH
*     JULY 1984 BY KEITH HORNE
*     
      IMPLICIT NONE
      INTEGER NDATA, KNOTREQ, IFAIL, MAXFIT, MAXKNOT, NCAP7
      INTEGER NFIT, LOCMAX, LOCMIN, I, ISTEP, IGET1, IGET2
      INTEGER IPUT1, IPUT2, IPUT, IGET, IGOT, IWISH, ITEST
      INTEGER NSPLINE, LOOKUP, MAXWORK, J
      REAL XDATA(NDATA), YDATA(NDATA), YSIGMA(NDATA), YFIT(NDATA)
      REAL YMEAN, PART, RMS
      PARAMETER (MAXFIT = 15000)
      PARAMETER (MAXKNOT = 500)
      DOUBLE PRECISION X(MAXFIT+2), Y(MAXFIT+2), W(MAXFIT+2)
      INTEGER KEY(MAXFIT+2)   ! Key to sorted data
      INTEGER LW
      DOUBLE PRECISION XKNOT(MAXKNOT+7), CALC
      DOUBLE PRECISION WORK1(MAXFIT+2)
      DOUBLE PRECISION CSPLINE(MAXKNOT+3)
      COMMON/SPLFIT0/NCAP7
      COMMON/SPLFIT1/XKNOT
      COMMON/SPLFIT2/CSPLINE
*     
      IF(NDATA.LE.0) THEN
         PRINT *,'** NO DATA IN ''SPLFIT1'','
         GOTO 999
      END IF
*     
*     Count and compute mean of YDATA with positive YSIGMA,
*     and locate extreme values of XDATA.
*     
      NFIT = 0
      LOCMAX = 1
      LOCMIN = 1
      CALC = 0.D0
      DO I=1,NDATA
         IF( XDATA(I) .GE. XDATA(LOCMAX) ) LOCMAX = I
         IF( XDATA(I) .LT. XDATA(LOCMIN) ) LOCMIN = I
         IF( YSIGMA(I).GT.0. ) THEN
            NFIT = NFIT + 1
            CALC = CALC + YDATA(I)
         END IF
      END DO
      IF(NFIT.LE.0) THEN
         PRINT *,'** NO DATA IN ''SPLFIT1'','
         GOTO 999
      END IF

      YMEAN = REAL(CALC/NFIT)
*     
*     Use mean of YDATA if there is not enough data for a spline fit
*     
      IF(NFIT.LT.3) THEN
         WRITE(*,*) '** WARNING: MEAN OF', NFIT,
     &        ' DATA POINTS INSTEAD SPLINE FIT.'
         DO I=1,NDATA
            YFIT(I) = YMEAN
         END DO
         GOTO 1000
      END IF
*     
*     Decide in which direction to load the data values
*     
      IF( LOCMAX.GT.LOCMIN ) THEN
         ISTEP = 1              ! Forward loading
         IGET1 = 1
         IGET2 = NDATA
      ELSE
         ISTEP = -1             ! Backward loading
         IGET1 = NDATA
         IGET2 = 1
      END IF
*     
      IPUT1 = 2                 ! Skip one datum at beginning
*     
*     Load data arrays for spline fit ---------------------------------------
*     
      IF( NFIT.LE.MAXFIT ) THEN
*     
         IPUT = IPUT1 - 1
         DO IGET= IGET1, IGET2, ISTEP
            IF( YSIGMA(IGET).GT.0.) THEN ! Load only data with positive sigma
               IPUT = IPUT + 1
               X(IPUT) = XDATA(IGET)
               Y(IPUT) = YDATA(IGET)
               W(IPUT) = YSIGMA(IGET)
            END IF
         END DO
         IPUT2 = IPUT
         NFIT = IPUT2 - IPUT1 + 1
*     
*     Too much data, some points must be skipped
*     
      ELSE
*     
         PRINT *,'** TOO MUCH DATA FOR ''SPLFIT''.', NFIT, MAXFIT
         PRINT *,'** INTERSPERSED DATA WILL BE SKIPPED.'
         IGET = IGET1
         IPUT = IPUT1 - 1
         IGOT = 0
         DO I=1,MAXFIT
            PART = (I-1.)/(MAXFIT-1.)
            IWISH = NINT( 1.-PART + NFIT*PART )
 30         IF(IWISH .GT. IGOT ) THEN
 32            IF( YSIGMA(IGET).LE.0. ) THEN
                  IGET = IGET + ISTEP
                  GOTO 32
               END IF
               IGOT = IGOT + 1
               GOTO 30
            END IF
            IPUT = IPUT + 1
            X(IPUT) = XDATA(IGET)
            Y(IPUT) = YDATA(IGET)
            W(IPUT) = YSIGMA(IGET)
         END DO
         IPUT2 = IPUT
         NFIT = IPUT2 - IPUT1 + 1
*     
      END IF
*     
*     Re-scale weights to their RMS value
*     
      CALC = 0.D0
      DO I = IPUT1, IPUT2
         CALC = CALC + W(I)*W(I)
      END DO
      RMS = REAL(SQRT(CALC/NFIT))
      DO I=IPUT1,IPUT2

C     The conversion of weights to inverse weights as a consequence of
C     changing E02BAF to PDA_DEFC is done here - pflm

         W(I) = W(I)/RMS
      END DO
C     
C     Sort data so that X values increase -------------------------------
C     
      DO ITEST = IPUT1+1, IPUT2
*     
         IF( X(ITEST) .LT. X(ITEST-1) ) THEN
            print *,'Shell sort to make X-values ascend.'
            CALL SHELLSORT( NFIT, X(IPUT1), KEY(IPUT1) )
            DO I=IPUT1,IPUT2
               WORK1(I) = X(KEY(I))
            END DO
            DO I=IPUT1,IPUT2
               X(I) = WORK1(I)
            END DO
            DO I=IPUT1,IPUT2
               WORK1(I) = Y(KEY(I))
            END DO
            DO I=IPUT1,IPUT2
               Y(I) = WORK1(I)
            END DO
            DO I=IPUT1,IPUT2
               WORK1(I) = W(KEY(I))
            END DO
            DO I=IPUT1,IPUT2
               W(I) = WORK1(I)
            END DO
            print *,'Sort finished'
            GOTO 50
         END IF
      END DO
C
C     Decide how many splines will be used
C     
 50   NSPLINE = MAX(1, MIN(KNOTREQ, MIN(NFIT/2+1, MAXKNOT) ) )
      IF(NSPLINE.LT.KNOTREQ) THEN
         PRINT *,'** NUMBER OF SPLINES REDUCED FROM', 
     &        KNOTREQ,' TO', NSPLINE
      END IF
C     
C     Distribute spline knots
C     
      DO I = 1, NSPLINE+1
         PART = (I-1.)/NSPLINE
         LOOKUP = NINT( IPUT1*(1.-PART) + IPUT2*PART )
         XKNOT(I+3) = X(LOOKUP)
      END DO
*     
*     Call NAG routine to compute spline fit
*
C     IFAIL = 1
C     CALL E02BAF( NFIT, NSPLINE+7, X(IPUT1), Y(IPUT1), W(IPUT1), 
C     &  XKNOT, WORK1, WORK2, CSPLINE, CALC, IFAIL )
      
C     Initialize end knots

      DO I=1,3
         XKNOT(I)           =  X(IPUT1)
         XKNOT(NSPLINE+4+I) =  X(IPUT2)
      ENDDO
      
      MAXWORK = MAXFIT + 2
      NCAP7   = NSPLINE+7
      LW      = (NCAP7-1)*5 + (NCAP7+1)*5
     &     + 2*MAX(NFIT,NCAP7) + NCAP7 + 16
      IF (LW .GT. MAXWORK) THEN 
         WRITE(*,*) 'SPLFIT: INSUFFICIENT WORKSPACE FOR PDA_DEFC'
         WRITE(*,*) 'Array elements required : ',LW
         WRITE(*,*) 'Array elements available: ',MAXWORK
         IFAIL = 99
         RETURN
      ENDIF
      IFAIL = 0
      CALL PDA_DEFC( NFIT, X(IPUT1), Y(IPUT1), W(IPUT1),
     &     4,NSPLINE+7,XKNOT,1,J,  CSPLINE, MAXWORK, WORK1, IFAIL)
      IF( J.NE.1 .OR. IFAIL.NE.0) THEN
         WRITE(*,*) '** PDA_DEFC failed.  IFAIL= ',IFAIL
         WRITE(*,*) '** PDA_DEFC failed.  MDEOUT=',J
         RETURN       
      END IF
C     
C     Evaluate spline at required points
C     
      CALL SPLCALC( NDATA, XDATA, XKNOT, CSPLINE, NCAP7,
     &     YFIT, IFAIL )
      IF(IFAIL.NE.0) THEN
         PRINT *,'** ''SPLCALC'' FAILED.  IFAIL=',IFAIL
         RETURN
      END IF
C     
C     NORMAL RETURN
C     
 1000 IFAIL = 0
      RETURN
C     
C     ERROR RETURN
C     
 999  IFAIL = 1
      RETURN
*     
      END
      
      SUBROUTINE SPLFITA( NDATA, XDATA, YDATA, YSIGMA, YFIT,
     #     NSPLINE, NCYCLE, THRHI, THRLO, IFAIL )
*
*     Fits spline to data and performs reject cycles with
*     no user interaction.
*     
*     Inputs:
*     NDATA       = number of data values
*     XDATA       = X data values
*     YDATA       = Y data values
*     YSIGMA      = Y uncertainties (1-sigma) (negative to reject)
*     NSPLINE     = Number of spline nodes
*     NCYCLE      = Max number of reject cycles
*     THRHI       = high threshold for sigma clipping
*     THRLO       = low threshold for sigma clipping
*     
*     Output:
*     YFIT       = Y FIT VALUES
*     IFAIL      = 0 IF SUCCESSFUL, 1 IF FAILED.
*     
*     Oct 1986 KDH @ STScI
*     
      IMPLICIT NONE
      INTEGER NDATA, NSPLINE, NCYCLE, NREJ, ICYCLE
      INTEGER LASTREJ, IFAIL
      REAL XDATA(NDATA), YDATA(NDATA), YSIGMA(NDATA)
      REAL YFIT(NDATA), THRHI, THRLO, RMS
C     
C     Test inputs
C
      IF(NDATA.LE.0) THEN
         WRITE(*,*) '** No data.', NDATA
         GOTO 999
      END IF
      
      NREJ = 0
      ICYCLE = -1
 100  ICYCLE = ICYCLE + 1
      LASTREJ = NREJ
C     
C     Fit the spline
C
      CALL SPLFIT( NDATA, XDATA, YDATA, YSIGMA, YFIT, 
     &     NSPLINE, IFAIL)
      IF( IFAIL.NE.0 ) GOTO 999
C     
C     Evaluate the fit
C     Do not restore any points
C     
      NREJ = -1
C
      IF( NCYCLE.EQ.0 ) THEN
         CALL REJECT( NDATA, YDATA, YSIGMA, YFIT,
     &        1.E20, -1.E20, RMS, NREJ )
C     
C     Evaluate the fit and reject large outliers
C     
      ELSE
         CALL REJECT( NDATA, YDATA, YSIGMA, YFIT,
     &        THRHI, THRLO, RMS, NREJ )
      END IF
C     
C     Report results of this reject
C     
      WRITE(*,*) ' Cycle', ICYCLE, '  RMS =', RMS,
     &     ' (sigma)  Rejects =', NREJ
C     
C     Next fit-reject cycle
C     
      IF( ICYCLE.LT.NCYCLE .AND. NREJ.NE.LASTREJ ) GOTO 100
C     
C     normal return
C
      IFAIL = 0
      RETURN
C
C error return
C     
999   IFAIL = 1
      WRITE(*,*) 'SPLFITA aborted.'
      RETURN
      END

      SUBROUTINE SPLFITR( NDATA, XDATA, YDATA, YSIGMA, YFIT, IFAIL)
*
* Computes weighted least-squares spline fit to (x-y) data pairs.
* with reject cycles.  Interactively controlled.
*
*  Inputs:
*       NDATA   = NUMBER OF DATA VALUES
*       XDATA   = X DATA VALUES
*       YDATA   = Y DATA VALUES
*       YSIGMA  = Y UNCERTAINTIES (1-SIGMA) (NEGATIVE TO REJECT)
*  Output:
*       YFIT    = Y FIT VALUES
*       IFAIL   = 0 IF SUCCESSFUL, 1 IF FAILED.
*
* Jul 1984 Keith Horne @ IOA - Original version.
* Oct 1986 KDH @ STScI - SPLFITA separated from SPLFITR.
*
        REAL*4 XDATA(1), YDATA(1), YSIGMA(1), YFIT(1)
        CHARACTER*1 REPLY

* Default spline fit parameters

        PARAMETER (MAXSPLINE = 500)
        DATA NSPLINE/3/
        DATA NCYCLE/10/
        DATA THRHI/100./
        DATA THRLO/-2./

* Trap invalid inputs.

      IF(NDATA.LE.0) THEN
        PRINT *,'** No data.', NDATA
        GOTO 999
      END IF

        GOTO 100

* command node

   10   PRINT *,' '
        PRINT '(A)', '$SPLFITR > '
        READ(*,'(A)') REPLY
        CALL UPPER_CASE( REPLY )

        IF( REPLY.EQ.'Q') GOTO 1000
        IF( REPLY.EQ.'A') GOTO 999
        IF( REPLY.EQ.'C') GOTO 100
        IF( REPLY.EQ.'P') GOTO 200
        IF( REPLY.EQ.'R') GOTO 300

* command summary

        PRINT *,' '
        PRINT *,'Type P -- to PLOT the fit'
        PRINT *,'     C -- for more reject CYCLES'
        PRINT *,'     R -- to RESTORE rejected pixels and try again'
        PRINT *,'     A -- to ABORT'
        PRINT *,'     Q -- to adopt this fit and continue'
        GOTO 10

* Report spline fit parameters

  100   PRINT *,' '
        PRINT *,'    Number of data pairs =', NDATA
        PRINT *,'       Number of SPLINES =', NSPLINE
        PRINT *,' Number of reject CYCLES =', NCYCLE
        PRINT *,'HIGH rejection threshold =', THRHI, ' sigma.'
        PRINT *,' LOW rejection threshold =', THRLO, ' sigma.'
        PRINT *,' '
        PRINT *,'Type A -- to ABORT this fit'
        PRINT *,'     C -- to CHANGE spline parameters'
        PRINT *,'   <CR>-- to proceed with the fit'
        READ(*,'(A)') REPLY

        IF( REPLY.EQ.'A') GOTO 10
        IF( REPLY.EQ.'C') GOTO 150
        IF( REPLY.EQ.' ') GOTO 180
        GOTO 100

* get new spline fit parameters

  150   PRINT *,' '
        PRINT *,'Enter (1) Number of SPLINES'
        PRINT *,'      (2) Number of reject CYCLES'
        PRINT *,'      (3) HIGH (>0) reject threshold'
        PRINT *,'      (4) LOW (<0) reject threshold'
        PRINT *,'(Note: Enter 0 to keep a default value.)'
        READ(*,*,ERR=150) NEWSPLINE, NEWCYCLE, THRHINEW, THRLONEW

        IF( NEWSPLINE.GT.0) NSPLINE = MIN(MAXSPLINE, NEWSPLINE)
        IF( NEWSPLINE.GT.NSPLINE)
     *  PRINT *,'** MAXIMUM ALLOWED NUMBER OF SPLINES IS', MAXSPLINE
        IF( NEWCYCLE.GT.0) NCYCLE = NEWCYCLE
        IF( THRHINEW.GT.0.) THRHI = THRHINEW
        IF( THRLONEW.LT.0.) THRLO = THRLONEW
        GOTO 100

* fit the spline with automatic reject cycles

  180   CALL SPLFITA( NDATA, XDATA, YDATA, YSIGMA, YFIT,
     #  NSPLINE, NCYCLE, THRHI, THRLO, IFAIL)
        GOTO 10

* plot fit

  200   CALL PLOTFIT( NDATA, XDATA, YDATA, YSIGMA, YFIT, ' ', ' ',
     *  'Weighted Least-squares Spline Fit' )
        GOTO 10

* restore rejected data points

  300   IRESET = 0
      DO I=1,NDATA
        IF( YSIGMA(I).LE.0. ) IRESET = IRESET + 1
        YSIGMA(I) = ABS(YSIGMA(I))
      END DO
        PRINT *,IRESET, ' rejected data pairs restored.'
        GOTO 100

*  NORMAL RETURN

 1000   IFAIL = 0
        RETURN

*  ERROR RETURN

  999   IFAIL = 1
        PRINT *,'SPLFITR aborted.'
        RETURN

        END

C
C     Next routine offers more control to calling routines
C
      SUBROUTINE SPLFITB( ADATA, NDATA, NSPLINE, NORD, WORK, 
     &     MXWORK, XKNOT, YFIT, CSPLINE, IFAIL)
*
* Computes weighted least-squares cubic spline fit to data pairs and is
* designed to fit more with the calling sequence of LSQUAR
*
*  Input:
*      ADATA      = DATA in the form 
*      NDATA      = Number of data pairs
*      NSPLINE    = Number of splines requested
*      NORD       = Spline order cubic=4
*      WORK       = R*8 work space, dimension MXWORK 
*      MXWORK     = This will be checked, but should be 
*                   at least 3*NDATA+(NORD+1)*(2*NBKPT-NORD+4)+2*MAX(NDATA,NBKPT)+NORD**2
*                   where NBKPT=NSPLINE+2*NORD-1
*
*  Output:
*      XKNOT      = Position of knots used
*      YFIT       = fitted y values
*      CSPLINE    = Spline coefficients
*      IFAIL      = 0 if successful, 1 if failed.
*
* TRM Jun2009
*
      IMPLICIT NONE
      INTEGER NDATA, NSPLINE, MXWORK, NFIT, I, NORD, NBKPT
      INTEGER NOFF1, NOFF2, NOFF3, IFAIL, MDEOUT, NWKMIN
      DOUBLE PRECISION RANGE, XMIN, XMAX, CALC, RMS, YMEAN
      DOUBLE PRECISION ADATA(3,NDATA), YFIT(NDATA)
      DOUBLE PRECISION WORK(MXWORK), XKNOT(NSPLINE+2*NORD-1)
      DOUBLE PRECISION CSPLINE(NSPLINE+NORD-1)

      NBKPT = NSPLINE+2*NORD-1      
      NWKMIN= (NORD+1)*(2*NBKPT-NORD+4)+2*MAX(NDATA,NBKPT)+
     &     NBKPT+NORD**2
*
      IF(NDATA.LE.0) THEN
         WRITE(*,*) '** NO DATA IN ''SPLFITB'','
         GOTO 999
      END IF
      
      IF(MXWORK .LT. 3*NDATA+NWKMIN) THEN
         WRITE(*,*) '** WORK ARRAY NOT LARGE ENOUGH IN ''SPLFITB'','
         WRITE(*,*) '** MUST BE AT LEAST',3*NDATA+NWKMIN,
     &        'BUT IS ONLY',MXWORK
         GOTO 999
      END IF

      NOFF1 =   NDATA
      NOFF2 = 2*NOFF1
      NOFF3 = 3*NOFF1
 
*     Load data arrays for spline fit
 
      CALC = 0.D0
      YMEAN=0.0
      NFIT = 0
      XMIN = ADATA(1,1)
      XMAX = XMIN
      DO I= 1, NDATA
         IF( ADATA(3,I).GT.0.) THEN      
            NFIT = NFIT + 1
            WORK(NFIT)       = ADATA(1,I)
            WORK(NOFF1+NFIT) = ADATA(2,I)
            WORK(NOFF2+NFIT) = ADATA(3,I)
            YMEAN    = YMEAN + WORK(NFIT)
            CALC     = CALC  + WORK(NOFF2+NFIT)*WORK(NOFF2+NFIT)
            IF(XMIN.GT.WORK(NFIT)) XMIN = WORK(NFIT)
            IF(XMAX.LT.WORK(NFIT)) XMAX = WORK(NFIT)
         END IF
      END DO
      RANGE = XMAX - XMIN
      XMIN  = XMIN - 0.0001*RANGE
      XMAX  = XMAX + 0.0001*RANGE

      IF(NFIT.LT.2) THEN
         WRITE(*,*) '** TOO FEW DATA POINTS IN ''SPLFITB'','
         GOTO 999
      END IF
      
      RMS   = SQRT(CALC/REAL(NFIT))
      YMEAN = YMEAN/NFIT
      
*     Re-scale sigmas
      
      DO I=NOFF2+1,NOFF2+NFIT
         WORK(I) = WORK(I)/RMS
      END DO
 
*  Distribute spline knots ---------------------------------------------

C
C     Distribute interior knots
C     
      DO I = 0, NSPLINE
         XKNOT(I+NORD) = XMIN+I*(XMAX-XMIN)/NSPLINE
      END DO

C     
C     Initialize end knots (extra part to reduce chance of error)
C
      DO I=1,NORD-1
         XKNOT(I)              =  XMIN
         XKNOT(NBKPT-NORD+1+I) =  XMAX
      END DO

      IFAIL = 0
      CALL PDA_DEFC( NFIT, WORK, WORK(NOFF1+1), WORK(NOFF2+1), NORD,
     &     NBKPT, XKNOT, 1, MDEOUT, CSPLINE, NWKMIN, WORK(NOFF3+1), 
     &     IFAIL)
      IF( MDEOUT.NE.1 .OR. IFAIL.NE.0) THEN
         WRITE(*,*) '** SPLFITB. PDA_DEFC failed.  IFAIL=',IFAIL
         WRITE(*,*) '** SPLFITB. PDA_DEFC failed.  MDEOUT=',MDEOUT
         RETURN
      END IF
      
*     Evaluate spline at required points
      
      DO I= 1, NDATA
         WORK(I) = ADATA(1,I)
      END DO
      CALL SPLCALCB( NDATA, WORK, XKNOT, CSPLINE, NSPLINE, NORD, 
     &     YFIT, IFAIL)
*     
*     Normal RETURN
*     
      IFAIL = 0
      RETURN
*     
*     Error RETURN
*     
 999  IFAIL = 1
      RETURN
*     
      END
      
      SUBROUTINE SPLCALCB( NDATA, XDATA, XKNOT, CSPLINE, NSPLINE, NORD,
     +     YFIT, IFAIL )
*     
*     Evaluates spline fit. Beyond end knots it extrapolates.
*     The knots position are assumed to ascend.
*
*     Input:
*     NDATA      = NUMBER OF DATA PAIRS
*     XDATA      = DATA X VALUES
*     XKNOT      = KNOT X VALUES
*     CSPLINE    = SPLINE COEFFS
*     NSPLINE    = NUMBER OF SPLINES
*     NORD       = SPLINE ORDER (CUBIC=4)
*
*     Output:
*     YFIT       = CORRESPONDING Y VALUES OF SPLINE FIT
*     IFAIL      = 0 IF SUCCESSFUL, 1 IF FAILED.
*     
*     
      IMPLICIT NONE
      INTEGER NDATA, IFAIL, NSPLINE, NORD
      DOUBLE PRECISION XDATA(NDATA), YFIT(NDATA)
*
* WORK array long enough to take 3x maximum value of NORD
*
      INTEGER I, J, KNOT, INBV, NKNOT, NCOEFF
      DOUBLE PRECISION DERIV, WORK(60), PDA_DBVALU
      DOUBLE PRECISION XKNOT(NSPLINE+2*NORD-1), CSPLINE(NSPLINE+NORD-1)
      
      NCOEFF = NSPLINE+NORD-1
      NKNOT  = NCOEFF+NORD
      INBV   = 1
      IFAIL  = 0

C     Loop thru the data

      DO I=1,NDATA
         
C  Evaluate spline at desired x-value
         
         IF( XDATA(I).GE.XKNOT(1).AND. XDATA(I).LE.XKNOT(NKNOT) ) THEN

            YFIT(I)  = PDA_DBVALU(XKNOT,CSPLINE,NCOEFF,NORD,0,XDATA(I),
     &           INBV,WORK,IFAIL)
            IF( IFAIL.NE.0) THEN
               WRITE(*,*) '** SPLCALCB. PDA_DBVALU failed.(1) IFAIL=',
     &              IFAIL
               print *,' XDATA(',I,') = ',XDATA(I)
               print *,' XKNOT(1) = ', XKNOT(1)
               print *,' XKNOT(',NCOEFF,') = ', XKNOT(NCOEFF)
               RETURN  
            END IF
         ELSE
C     
C     Evaluate spline and first derivative at the end knot
C     
            IF (XDATA(I).GT.XKNOT(NCOEFF)) THEN
               KNOT = NCOEFF
            ELSE 
               KNOT = 1
            ENDIF
            
            YFIT(I) = PDA_DBVALU(XKNOT,CSPLINE,NCOEFF,NORD,0,
     &           XKNOT(KNOT),INBV,WORK,IFAIL)
            IF( IFAIL.NE.0) THEN
               WRITE(*,*) '** SPLCALCB. PDA_DBVALU Failed.(2) IFAIL=',
     &              IFAIL
               WRITE(*,*) 'XKNOT(',KNOT,') = ',XKNOT(KNOT)
               RETURN
            ENDIF
            
            DERIV = PDA_DBVALU(XKNOT,CSPLINE,NCOEFF,NORD,1,
     &           XKNOT(KNOT),INBV,WORK,IFAIL)
            IF( IFAIL.NE.0) THEN
               WRITE(*,*) '** SPLCALCB. PDA_DBVALU Failed.(3) IFAIL=',
     &              IFAIL
               RETURN
            ENDIF
C     
C     Linear extrapolation to desired point
C     
            YFIT(I) = YFIT(I) + DERIV * ( XDATA(I) - XKNOT(KNOT))
            
         END IF
      END DO
      RETURN
      END

