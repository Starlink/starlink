*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*   SUBROUTINE EWMEAS
*
*   Measures equivalent widths, and calculates errors if
*   appropriate data are available
*
*   MODE = 0      Assumes noise in line equal to noise in continuum
*                 (suitable for IUE-type spectra)
*   MODE = 1      Assumes Poisson statistics in line
*                 (suitable for IPCS-type data)
*
*   EXPORTS:
*       EW              (REAL) Equivalent width in mA
*
*   IMPORTS:
*       DEVTYP          (INTEGER)  Device type
*       ASIZE1          (INTEGER)  Size of WAVE, FLUX arrays
*       WAVE            (REAL)     Array of x values
*       FLUX            (REAL)     Array of y values
*       NPOINT          (INTEGER)  Number of points in WAVE & FLUX arrays
*       MAXBRK          (INTEGER)  Size of BREAKS array
*       BREAKS          (INTEGER)  Arrays of 'break' points
*       NBREAK          (INTEGER)  Number of 'breaks'
*       ICFLAG          (INTEGER)  Flag - is there are valid
*                       measurement of SIGCIH (ICFLAG=1) or not (0)?
*       IHFLAG          (INTEGER)  Flag - is there a valid set of
*                       systematic errors (IHFLAG=1) or not (0)?
*       ERRCIH          (REAL)     Fractional error on continuum
*       ERR0IH          (REAL)     Fractional error on background
*       WORV            (REAL)     Wavelength OR Velocity
*       MODE            (INTEGER)  IUE/IPCS switch (0/1)
*       SUBCHK          (LOGICAL)  TRUE on success
*
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE EWMEAS(EW,DEVTYP,ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,
     :                   BREAKS,NBREAK,ICFLAG,IHFLAG,NCIH,SIGCIH,ERRCIH,
     :                   ERR0IH,SIGEW,WORV,MODE,SUBCHK)
*
*
*
       IMPLICIT NONE
*
*
*
       INTEGER ASIZE1, NPOINT, MAXBRK, NBREAK, NCIH, MODE
       INTEGER DEVTYP, ICFLAG, IHFLAG

       INTEGER BREAKS(MAXBRK)
*
       REAL WAVE(ASIZE1), FLUX(ASIZE1)
       REAL WORV, SIGCIH, ERRCIH, ERR0IH
*
       LOGICAL SUBCHK
*
       LOGICAL LOGWRTE
       INTEGER EWUNIT
       COMMON /EWLOG/ LOGWRTE, EWUNIT
*
*
*   LOCAL VARIABLES
*
*
       INTEGER I, J, K, L, M, N
       INTEGER I1, I2, ITEMP
       INTEGER IBRK, ITMP
       INTEGER NINTEW, NLIH

       REAL EW
       REAL X1, X2, XNEW, XLAST, SLOPE
       REAL Y1, Y2, YNEW, YLAST, YCONT
       REAL SIGEW, SIG1, SIG2, SIG3, SIG4
       REAL FERR2, FERR3
       REAL D2, D3, DX
       REAL EWIH, BWIH
       REAL WSUM1, WSUM2, WSUM3
*
*
*
       SUBCHK = .TRUE.
       IF (WORV.EQ.0.0) WORV = 1.0
       SIG1 = 0.0
       SIG2 = 0.0
       SIG3 = 0.0
       SIG4 = 0.0
*
*
*
*
       CALL CPAIR(X1,Y1,X2,Y2)
       CALL FINDIT(WAVE,NPOINT,X1,I1,0)
       CALL FINDIT(WAVE,NPOINT,X2,I2,1)
  100  CONTINUE
       IF (I2.LE.I1) THEN
          ICFLAG = 0
       ELSE
          XLAST = X1
          YLAST = (FLUX(I1+1)-FLUX(I1))*(X1-WAVE(I1))
     :            /(WAVE(I1+1)-WAVE(I1)) + FLUX(I1)
          YLAST = 1.0 - YLAST/Y1
          SLOPE = (Y2-Y1)/(X2-X1)
          EW = 0.0
          SIG1 = 0.0
          WSUM1 = 0.0
          WSUM2 = 0.0
          WSUM3 = 0.0
          DO 150 I = I1 + 1, I2
             XNEW = WAVE(I)
             YCONT = Y1 + SLOPE*(XNEW-X1)
             YNEW = 1.0 - FLUX(I)/YCONT
             DX = XNEW - XLAST
             EW = EW + (YNEW+YLAST)*DX
             IF (MODE.EQ.1) THEN
                SIG1 = SIG1 + (DX/YCONT)**2*FLUX(I)
                WSUM1 = WSUM1 + XNEW*YNEW
                WSUM2 = WSUM2 + YNEW
                WSUM3 = WSUM3 + (XNEW/YCONT)**2*FLUX(I)
             ENDIF
             XLAST = XNEW
             YLAST = YNEW
  150     CONTINUE
          DX = WAVE(I2+1) - WAVE(I2)
          YNEW = (FLUX(I2+1)-FLUX(I2))*(X2-WAVE(I2))/DX + FLUX(I2)
          YNEW = 1.0 - YNEW/Y2
          EW = (EW+(YNEW+YLAST)*(X2-XLAST))*500.0*WORV
*
          ITMP = 0
          DO 200 I = 1, NBREAK
             IBRK = BREAKS(I)
             IF (IBRK.GE.I1 .AND. IBRK.LT.I2) ITMP = ITMP + 1
  200     CONTINUE
          NINTEW = NINT(EW)
          IF (X1.LT.1.0E+06 .AND. X2.LT.1.0E+06) THEN
             WRITE (*,'(''   (X1, X2:'',2F10.2,'')'')') X1, X2
             IF (LOGWRTE) WRITE (EWUNIT,
     :       '(''   (X1, X2:'',2F10.2,'')'')') X1, X2
          ELSE
             WRITE (*,'(''   (X1, X2:'',1P2E15.5,'')'')') X1, X2
             IF (LOGWRTE) WRITE (EWUNIT,
     :       '(''   (X1, X2:'',1P2E15.5,'')'')') X1, X2
          ENDIF
          IF (ABS(NINTEW).GE.1000) THEN
             WRITE (*,'(''   EW:'',F10.3,'' (Angstroms)'')') EW/1000.0
             IF (LOGWRTE) WRITE (EWUNIT,
     :       '(''   EW:'',F10.3,'' (Angstroms)'')') EW/1000.0
          ELSEIF (ABS(NINTEW).GE.10) THEN
             WRITE (*,'(''   EW:'',I9,'' (mA)'')') NINTEW
             IF (LOGWRTE) WRITE (EWUNIT,
     :       '(''   EW:'',I9,'' (mA)'')') NINTEW
          ELSE
             WRITE (*,'(''   EW:'',F10.3,'' (mA)'')') EW
             IF (LOGWRTE) WRITE (EWUNIT,
     :       '(''   EW:'',F10.3,'' (mA)'')') EW
          ENDIF
*
*
          EW = EW/1000.0
          IF (ITMP.NE.0) THEN
             WRITE (*,'(''   *WARNING* - linear interpolation used'')')
             IF (LOGWRTE) WRITE (EWUNIT,
     :       '(''   *WARNING* - linear interpolation used'')')
          ENDIF
*
* Calculate errors
*
          NLIH = I2 - I1 - 1
          EWIH = EW*1000.0
          BWIH = (WAVE(I2)-WAVE(I1))/REAL(NLIH)
*
* Statistical errors
* 1  -  line
*
          IF (ICFLAG.EQ.0) THEN
             SIGCIH = 0.0
          ENDIF
          IF (MODE.EQ.0) THEN
             SIG1 = SIGCIH*BWIH*SQRT(REAL(NLIH))*1000.0
          ELSEIF (MODE.EQ.1) THEN
             SIG1 = SQRT(SIG1)*1000.0
          ENDIF
*
* 2  -  continuum
*
          IF (SIGCIH.NE.0.0) THEN
             SIG2 = SIGCIH/SQRT(REAL(NCIH))
             D2 = EWIH*(1.0+SIG2)/(BWIH*NLIH) - SIG2
             FERR2 = SIG2*(1.0-D2)
             FERR2 = FERR2/(SIG2+D2)
             SIG2 = FERR2*EWIH
             SIG2 = ABS(SIG2)
          ENDIF
*
*  Systematic errors
*  1  -  continuum
*
          IF (ERRCIH.NE.0.0) THEN
             SIG3 = ERRCIH
             D3 = EWIH*(1.0+SIG3)/(BWIH*NLIH) - SIG3
             FERR3 = SIG3*(1.0-D3)
             FERR3 = FERR3/(SIG3+D3)
             SIG3 = FERR3*EWIH
             SIG3 = ABS(SIG3)
          ENDIF
*
*  2  -  Zero level
*
          IF (ERR0IH.NE.0) THEN
             SIG4 = EWIH*ERR0IH
             SIG4 = ABS(SIG4)
          ENDIF
*
*  Grand total
*
          SIGEW = SIG1**2 + SIG2**2 + SIG3**2 + SIG4**2
          SIGEW = SQRT(SIGEW)
          IF (SIGEW.NE.0.0) THEN
             IF (SIGCIH.EQ.0.0) THEN
                IF (MODE.EQ.0) THEN
                   WRITE (*,
     :             '(''   No continuum fit (or statistical error)'')')
                   IF (LOGWRTE) WRITE (EWUNIT,
     :             '(''   No continuum fit (or statistical error)'')')
                ELSEIF (MODE.EQ.1) THEN
                   WRITE (*,'(''   No continuum fit'')')
                   IF (LOGWRTE) WRITE (EWUNIT,
     :             '(''   No continuum fit'')')
                   IF (NINTEW.GE.1000) THEN
                      WRITE (*,
     :                '(''   Statistical error on line:'',F10.3)')
     :                SIG1/1000.0
                      IF (LOGWRTE) WRITE (EWUNIT,
     :                '(''   Statistical error on line:'',F10.3)')
     :                SIG1/1000.0
                   ELSEIF (NINTEW.GE.10) THEN
                      WRITE (*,'(''   Statistical error on line:'',I9)')
                      IF (LOGWRTE) WRITE (EWUNIT,
     :                '(''   Statistical error on line:'',I9)')
     :                NINT(SIG1)
                   ELSE
                      WRITE (*,
     :                '(''   Statistical error on line:'',F10.3)')
     :                SIG1
                      IF (LOGWRTE) WRITE (EWUNIT,
     :                '(''   Statistical error on line:'',F10.3)')
     :                SIG1
                   ENDIF
                ENDIF
             ELSEIF (NINTEW.GE.1000) THEN
                WRITE (*,
     :          '(''   Statistical error (line, cont.):   '',
     :          F10.3,'','',F10.3)') SIG1/1000.0, SIG2/1000.0
                IF (LOGWRTE) WRITE (EWUNIT,
     :          '(''   Statistical error (line, cont.):   '',
     :          F10.3,'','',F10.3)') SIG1/1000.0, SIG2/1000.0
             ELSEIF (NINTEW.GE.10) THEN
                WRITE (*,
     :          '(''   Statistical error (line, cont.):   '',2I9)')
     :          NINT(SIG1), NINT(SIG2)
                IF (LOGWRTE) WRITE (EWUNIT,
     :          '(''   Statistical error (line, cont.):   '',2I9)')
     :          NINT(SIG1), NINT(SIG2)
             ELSE
                WRITE (*,
     :          '(''   Statistical error (line, cont.):   '',
     :          F10.3,'','',F10.3)') SIG1, SIG2
                IF (LOGWRTE) WRITE (EWUNIT,
     :          '(''   Statistical error (line, cont.):   '',
     :          F10.3,'','',F10.3)') SIG1, SIG2
             ENDIF
             IF (IHFLAG.EQ.0) THEN
                WRITE (*,'(''   No systematic errors input'')')
                IF (LOGWRTE) WRITE (EWUNIT,
     :          '(''   No systematic errors input'')')
             ELSEIF (NINTEW.GE.1000) THEN
                WRITE (*,
     :          '(''    Systematic error (line, cont.):   '',
     :          F10.3,'','',F10.3)') SIG3/1000.0, SIG4/1000.0
                IF (LOGWRTE) WRITE (EWUNIT,
     :          '(''    Systematic error (line, cont.):   '',
     :          F10.3,'','',F10.3)') SIG3/1000.0, SIG4/1000.0
             ELSEIF (NINTEW.GE.10) THEN
                WRITE (*,
     :          '(''    Systematic error (line, cont.):   '',2I9)')
     :          NINT(SIG3), NINT(SIG4)
                IF (LOGWRTE) WRITE (EWUNIT,
     :          '(''    Systematic error (line, cont.):   '',2I9)')
     :          NINT(SIG3), NINT(SIG4)
             ELSE
                WRITE (*,
     :          '(''    Systematic error (line, cont.):   '',
     :          F10.3,'','',F10.3)') SIG3, SIG4
                IF (LOGWRTE) WRITE (EWUNIT,
     :          '(''    Systematic error (line, cont.):   '',
     :          F10.3,'','',F10.3)') SIG3, SIG4
             ENDIF
             IF (NINTEW.GE.1000) THEN
                WRITE (*,
     :          '(''   Total 2sigma error (A):    '',
     :          F10.3,F10.3)') SIGEW/500.0
                IF (LOGWRTE) WRITE (EWUNIT,
     :          '(''   Total 2sigma error (A):    '',
     :          F10.3,F10.3)') SIGEW/500.0
             ELSEIF (NINTEW.GE.10) THEN
                WRITE (*,
     :          '(''   Total 2sigma error (mA):   '',I9)')
     :          NINT(2.0*SIGEW)
                IF (LOGWRTE) WRITE (EWUNIT,
     :          '(''   Total 2sigma error (mA):   '',I9)')
     :          NINT(2.0*SIGEW)
             ELSE
                WRITE (*,
     :          '(''   Total 2sigma error (mA):   '',F10.3)')
     :          2.0*SIGEW
                IF (LOGWRTE) WRITE (EWUNIT,
     :          '(''   Total 2sigma error (mA):   '',F10.3)')
     :          2.0*SIGEW
             ENDIF
             IF (MODE.EQ.0) WRITE (*,'('' '')')
             IF (LOGWRTE .AND. (MODE.EQ.0)) WRITE (EWUNIT,'('' '')')
             SIGEW = SIGEW*2.0
          ENDIF
          IF (MODE.EQ.1) THEN
             WSUM1 = WSUM1/WSUM2
             WRITE (*,'(''   Mean wavelength:'',F10.2)') WSUM1
             IF (LOGWRTE) WRITE (EWUNIT,
     :       '(''   Mean wavelength:'',F10.2)') WSUM1
             WRITE (*,'('' '')')
             IF (LOGWRTE) WRITE (EWUNIT,
     :       '('' '')')
          ENDIF
          CALL CPAIR(X1,Y1,X2,Y2)
          CALL FINDIT(WAVE,NPOINT,X1,I1,0)
          CALL FINDIT(WAVE,NPOINT,X2,I2,1)
          GOTO 100
       ENDIF

       END
