        SUBROUTINE POLYCALC( NDATA, XDATA, YFIT, IFAIL )
*
* CALCULATES POLY FIT FROM 'POLYFIT1'
* AT ALL POINTS OF SPECTRUM
*
*  Input:
*       NDATA   = NUMBER OF DATA PAIRS
*       XDATA   = DATA X VALUES
*  Output:
*       YFIT    = CORRESPONDING Y VALUES OF POLY FIT
*       IFAIL   = 0 IF SUCCESSFUL, 1 IF FAILED.
*
        REAL*4 XDATA(1), YFIT(1)

        REAL*8 PFT,POLY,XSTART,XEND,DUM(20),DSTART,DEND,START,END
        COMMON/PLYFIT0/NPFIT
        COMMON/PLYFIT1/XSTART,XEND,PFT(1)

        IFAIL = 1
        IF( NDATA.LE.0) RETURN

*  Loop thru the data

        DO I=1,NPFIT-1
          DUM(I) = PFT(I+1)*DBLE(I)
        END DO
* Evaluate derivatives at xstart,xend, and value of poly.
        START  = POLY(PFT,NPFIT  ,XSTART)
        END    = POLY(PFT,NPFIT  ,XEND  )
        IF(NPFIT.GT.1) THEN
          DSTART = POLY(DUM,NPFIT-1,XSTART)
          DEND   = POLY(DUM,NPFIT-1,XEND  )
        ELSE
          DSTART = 0.D0
          DEND   = 0.D0
        END IF
        DO I=1,NDATA

*  Evaluate poly at desired x-value
*  Extrapolate linearly outside range used for poly fit

          IF(DBLE(XDATA(I)).LT.XSTART) THEN
            YFIT(I) = SNGL(START - DSTART*(XSTART-DBLE(XDATA(I))))
          ELSE IF(DBLE(XDATA(I)).GT.XEND) THEN
            YFIT(I) = SNGL(END + DEND*(DBLE(XDATA(I))-XEND))
          ELSE
            YFIT(I) = SNGL(POLY(PFT,NPFIT,DBLE( XDATA(I) ) ) )
          END IF
        END DO
        IFAIL = 0
        RETURN
        END
        SUBROUTINE POLYFIT1( NDATA, XDATA, YDATA, YSIGMA,
     *          YFIT, NPOLY, IFAIL )
*
*  COMPUTES WEIGHTED LEAST-SQUARES POLY FIT TO DATA PAIRS
*
*  Input:
*       NDATA   = NUMBER OF DATA PAIRS
*       XDATA   = DATA X VALUES
*       YDATA   = DATA Y VALUES
*       YSIGMA  = UNCERTAINTY IN Y (1-SIGMA) (NEGATIVE TO REJECT)
*       NPOLY   = NUMBER OF POLY TERMS REQUESTED
*  Output:
*       YFIT    = FITTED Y VALUES
*       RMS     = RMS NORMALIZED RESIDUAL OF POINTS NOT REJECTED
*       IFAIL   = 0 IF SUCCESSFUL, 1 IF FAILED.
*
*       USES LSQUAR
*
        REAL*4 XDATA(NDATA), YDATA(NDATA), YSIGMA(NDATA), YFIT(NDATA)
*
        PARAMETER (MAXFIT = 5000)
        PARAMETER (MAXPOLY = 20)
        REAL*8 X(3,MAXFIT),PFT(MAXPOLY),XM(MAXPOLY,2*MAXPOLY+3)
        REAL*8 CHISQ, CALC, POLY, XSTART, XEND
        COMMON/PLYFIT0/NPFIT
        COMMON/PLYFIT1/XSTART,XEND,PFT
*
*  Check input data
*
      IF(NDATA.LE.0) THEN
        PRINT *,'** No data.', NDATA
        GOTO 999
      END IF
*
* Error return if NPOLY > NFIT
*
        NFIT=0
      DO I=1,NDATA
        IF(YSIGMA(I).GT.0.) NFIT = NFIT + 1
      END DO
      IF( NPOLY.GT.NFIT) THEN
        PRINT *,'** INSUFFICIENT DATA FOR POLYFIT. NPOLY=',
     *          NPOLY, 'NFIT=', NFIT
        GOTO 999
      END IF
*
*  Load data arrays for poly fit ---------------------------------------
*
      IF( NFIT.LE.MAXFIT ) THEN
*
        IPUT = 0
      DO IGET= 1,NDATA
        IF( YSIGMA(IGET).GT.0.) THEN  !Load only data with positive sigma
          IPUT = IPUT + 1
          X(1,IPUT) = XDATA(IGET)
          X(2,IPUT) = YDATA(IGET)
          X(3,IPUT) = YSIGMA(IGET)
        END IF
      END DO
        NFIT = IPUT
*
*  Too much data, some points must be skipped
*
      ELSE
*
        PRINT *,'** WARNING: TOO MUCH DATA FOR ''POLYFIT1''.',
     +          NFIT, MAXFIT
        PRINT *,'** INTERSPERSED DATA WILL BE SKIPPED.'
        IGET = 1
        IPUT = 0
        IGOT = 0
        DO I=1,MAXFIT
          PART = (I-1.)/(MAXFIT-1.)
          IWISH = NINT( 1.-PART + NFIT*PART )
   30     IF(IWISH .GT. IGOT ) THEN
   32       IF( YSIGMA(IGET).LE.0. ) THEN
              IGET = IGET + 1
              GOTO 32
            END IF
            IGOT = IGOT + 1
            GOTO 30
          END IF
          IPUT = IPUT + 1
          X(1,IPUT) = XDATA(IGET)
          X(2,IPUT) = YDATA(IGET)
          X(3,IPUT) = YSIGMA(IGET)
        END DO
        NFIT = IPUT
*
      END IF
*
*  Re-scale weights to their RMS value
*
        CALC = 0.D0
      DO I = 1,NFIT
        CALC = CALC + X(3,I)*X(3,I)
      END DO
      RMS = REAL(SQRT(CALC/NFIT))
      DO I=1,NFIT
        X(3,I) = X(3,I)/RMS
      END DO
*
*  Call LSQUAR routine to compute poly fit --------------------------------
*
        CALL LSQUAR(X,NFIT,NPOLY,PFT,CHISQ,XM,1)
*
*  Evaluate poly at required points
*
        DO I=1,NDATA
          YFIT(I) = SNGL( POLY( PFT, NPOLY, DBLE(XDATA(I)) ))
        END DO
        NPFIT = NPOLY
        XSTART = X(1,1)
        XEND   = XSTART
        DO I=1,NFIT
          XSTART = MIN(XSTART,X(1,I))
          XEND   = MAX(XEND, X(1,I))
        ENDDO
*
*  NORMAL RETURN
*
        IFAIL = 0
        RETURN
*
*  ERROR RETURN
*
  999   PRINT *,'** POLYFIT1 aborted.'
        IFAIL = 1
        RETURN
*
        END

        SUBROUTINE POLYFITA( NDATA, XDATA, YDATA, YSIGMA, YFIT,
     #  NPOLY, NCYCLE, THRHI, THRLO, IFAIL )
*
* Fits polynomial to data and performs reject cycles with
* no user interaction.
*
*  Inputs:
*       NDATA   = NUMBER OF DATA VALUES
*       XDATA   = X DATA VALUES
*       YDATA   = Y DATA VALUES
*       YSIGMA  = Y UNCERTAINTIES (1-SIGMA) (NEGATIVE TO REJECT)
*       NPOLY   = Number of terms in polynomial (0-maxpoly)
*       NCYCLE  = Max number of reject cycles
*       THRHI   = high threshold for sigma clipping
*       THRLO   = low threshold for sigma clipping
*
*  Output:
*       YFIT    = Y FIT VALUES
*       IFAIL   = 0 IF SUCCESSFUL, 1 IF FAILED.
*
* Oct 1986 KDH @ STScI
* 29/01/87 TRM @ RGO eliminated statement label
*
        REAL*4 XDATA(1), YDATA(1), YSIGMA(1), YFIT(1)

* Test inputs

      IF(NDATA.LE.0) THEN
        PRINT *,'** No data.', NDATA
        GOTO 999
      END IF
        LASTREJ = 1
        NREJ = 0        ! count rejected pixels
        ICYCLE = -1     ! count the fit-reject cycles
        DO WHILE(ICYCLE.LT.NCYCLE .AND. NREJ.NE.LASTREJ)
          ICYCLE = ICYCLE + 1
          LASTREJ = NREJ        ! stow reject count from previous fit

* Fit the polynomial

          CALL POLYFIT1( NDATA, XDATA, YDATA, YSIGMA, YFIT,
     &    NPOLY, IFAIL)
          IF( IFAIL.NE.0 ) GOTO 999

* Evaluate the fit
* Do not restore any points

          NREJ = -1

          IF( NCYCLE.EQ.0 ) THEN

            CALL REJECT( NDATA,YDATA,YSIGMA,YFIT,1.E20,-1.E20,RMS,NREJ)

* Evaluate the fit and reject large outliers

          ELSE

            CALL REJECT(NDATA,YDATA,YSIGMA,YFIT,THRHI,THRLO,RMS,NREJ)

          END IF

* Report results of this reject

          PRINT *, ' Cycle', ICYCLE, '  RMS =', RMS,
     *  ' (sigma)  Rejects =', NREJ

* Next fit-reject cycle
        END DO

* normal return

        IFAIL = 0
        RETURN

* error return

  999   IFAIL = 1
        PRINT *,'POLYFITA aborted.'
        RETURN

        END
        SUBROUTINE POLYFITR(  NDATA, XDATA, YDATA, YSIGMA, YFIT, IFAIL )
*
*  Computes weighted least-squares polynomial fit to (x-y) data pairs.
*  with reject cycles.  Interactively controlled.
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
*  SEPT 1984 BY KEITH HORNE AND TOM MARSH AT IOA.
* Oct 1986 KDH @ STScI -- Separate POLYFITR from POLYFITA.
*
        REAL*4 XDATA(1), YDATA(1), YSIGMA(1), YFIT(1)
        CHARACTER*1 REPLY

* default poly fit parameters

        PARAMETER (MAXPOLY = 20)
        DATA NPOLY/3/
        DATA NCYCLE/10/
        DATA THRHI/100./
        DATA THRLO/-2./

* Test inputs

        IFAIL = 0
      IF(NDATA.LE.0) THEN
        PRINT *,'** No data.'
        GOTO 999
      END IF

        GOTO 100

* Command node

   10   PRINT *,' '
        PRINT '(A)', '$POLYFITR > '
        READ(*,'(A)') REPLY

        IF( REPLY.EQ.'Q') GOTO 1000
        IF( REPLY.EQ.'A') GOTO 999
        IF( REPLY.EQ.'C') GOTO 100
        IF( REPLY.EQ.'P') GOTO 200
        IF( REPLY.EQ.'R') GOTO 300

* Command summary

        WRITE(*,*) ' '
        WRITE(*,*) 'Type P -- to PLOT the fit'
        WRITE(*,*) '     C -- CHANGE parameters '//
     &  'and run additional reject CYCLES'
        WRITE(*,*) '     R -- to RESTORE rejected'//
     &  ' pixels and try again'
        WRITE(*,*) '     A -- to ABORT'
        WRITE(*,*) '     Q -- to adopt this fit and continue'
        GOTO 10
C
C  REPORT POLY FIT PARAMETERS
C
  100   PRINT *,' '
        PRINT *,'   Number of data points =', NDATA
        PRINT *,' Number of polyfit TERMS =', NPOLY
        PRINT *,' Number of reject CYCLES =', NCYCLE
        PRINT *,'HIGH rejection threshold =', THRHI, ' sigma.'
        PRINT *,' LOW rejection threshold =', THRLO, ' sigma.'
        PRINT *,' '
        PRINT *,'Type A -- to ABORT this fit'
        PRINT *,'     C -- to CHANGE poly parameters'
        PRINT *,'   <CR>-- to proceed with the fit'
        READ(*,'(A)') REPLY
        CALL UPPER_CASE( REPLY )

        IF( REPLY.EQ.'A') GOTO 10
        IF( REPLY.EQ.'C') GOTO 150
        IF( REPLY.EQ.' ') GOTO 180
        GOTO 100

* Get new poly fit parameters

  150   PRINT *,' '
        PRINT *,'Enter (1) Number of polyfit TERMS'
        PRINT *,'      (2) Number of reject CYCLES'
        PRINT *,'      (3) HIGH (>0) reject threshold'
        PRINT *,'      (4) LOW (<0) reject threshold'
        PRINT *,'(Note: Enter 0 to keep a default value.)'
        READ(*,*,ERR=150) NEWPOLY, NEWCYCLE, THRHINEW, THRLONEW

        IF( NEWPOLY.GT.0) NPOLY = MIN(MAXPOLY, NEWPOLY)
      IF( NEWPOLY.GT.NPOLY) THEN
        PRINT *,CHAR(7),'** Maximum number of poly terms is', MAXPOLY
      END IF
        IF( NEWCYCLE.GT.0) NCYCLE = NEWCYCLE
        IF( THRHINEW.GT.0.) THRHI = THRHINEW
        IF( THRLONEW.LT.0.) THRLO = THRLONEW
        GOTO 100

* polyfit with automatic reject cycles

  180   CALL POLYFITA( NDATA, XDATA, YDATA, YSIGMA, YFIT,
     #  NPOLY, NCYCLE, THRHI, THRLO, IFAIL )
        GOTO 10

* plot the fit and normalized residuals

  200   CALL PLOTFIT( NDATA, XDATA, YDATA, YSIGMA, YFIT, ' ', ' ',
     *  'Weighted Least-squares Poly-Fit' )
        GOTO 10

* restore rejected data points

  300   IRESET = 0
      DO I=1,NDATA
        IF( YSIGMA(I).LE.0. ) IRESET = IRESET + 1
        YSIGMA(I) = ABS(YSIGMA(I))
      END DO
        PRINT *,IRESET, ' rejected data pairs restored.'
        GOTO 100

* normal return

 1000   IFAIL = 0
        RETURN

* error return

  999   IFAIL = 1
        PRINT *,'POLYFITR aborted.'
        RETURN

        END
