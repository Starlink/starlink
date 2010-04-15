       SUBROUTINE NEBCONT(NPTS,XCOORD,YCOORD,TZ,HER,HEPR,EL,COLHP,HBETA)
*
*......ROUTINE CALLED BY NEBF FOR CALCULATING He,H CONTINUA
*
*
*......CONTINUUM EMISSION FROM H+, He+, He2+ COMPUTED IN YSAVE(I,5),
*...... UNITS ERG CM**-2 S**-1 ANGSTROM**-1 AT EARTH.
*
*......ADAPTED FROM PROGRAM WRITTEN BY J. P. HARRINGTON.
*
       INTEGER NPTS
       DIMENSION XCOORD(20000), YCOORD(20000), TZ(4)
       DIMENSION CON(2)
       DATA PI/3.14159265/
       DATA CON/10.96776, 43.890868/
       CBRT(X) = X**.33333333
*
*  SUMS REPLACED BY INTEGRALS FOR n.GT.M
*
       M = 10
*
*  INITIALIZATION
*
       T = TZ(1)
       TI = 1./T
       TSQRT = SQRT(T)
       T32 = TI/TSQRT
       T2 = TZ(2)
       TI2 = 1./T2
       TSQRT2 = SQRT(T2)
       T322 = TI2/TSQRT2
*
*  HBETA FLUX, UNITS ERGS CM**-2 S**-1 AT EARTH
*
*
*  START LOOP 20 ON WAVELENGTH POINTS
*     WITHIN LOOP ENERGIES CALCULATED PER UNIT FREQUENCY
*     IN UNITS 10**-25 ERG PER ION PER ELECTRON
*
       DO 100 NU = 1, NPTS
          FIMU = 10000./XCOORD(NU)
          QTP = 0.
*
*     H I, HE I, AND HE II  FREE-FREE CONTINUUM
*
          QH = (1.+.1728*(1.+1.39*T/FIMU)*CBRT(FIMU/10.96776))
     :    *6.84E-15*EXP(-1.438787*FIMU*TI)/TSQRT
          QHE = QH
          QHEP = (1.+.1728*(1.+1.39*T2/FIMU)*CBRT(FIMU/43.890868))
     :    *27.360E-15*EXP(-1.438787*FIMU*TI2)/TSQRT2
*
*     H & HE+ RECOMBINATIONS TO LEVELS 5 THROUGH INFINITY.
*     (APPROXIMATE GAUNT FACTORS.)
*     ALSO, HE RECOMBINATIONS, WHICH ARE ASSUMED HYDROGENIC.
*
          DO 50 IX = 1, 2
             TE = TZ(IX)
             TEI = 1./TE
             TER = SQRT(TE)
             FI = 1.438737*FIMU*TEI
             C0 = CON(IX)
             Z2 = IX*IX
             Z4 = Z2*Z2
             Y = FIMU/C0
             Y13 = CBRT(Y)
             Y13I = 1./Y13
             A = 1. + Y13*(.1728-.0496*Y13)
             B = Y13I*(.03307-.3456*Y13I)
             C = -.03307*Y13I/Y
             N0 = 5
             N1 = 1./SQRT(Y) + 1.
             IF (N1.GT.N0) N0 = N1
             N2 = N1 + M
             N3 = N2 - 1
             D = 0.
             X0 = 1.438787*C0*TEI
             DO 20 I = N0, N3
                EN = I
                EN = 1./EN
                EN2 = EN*EN
                X = X0*EN2
                D = D + (A+EN2*(B+EN2*C))*X*EN*EXP(X)
   20        CONTINUE
             EN = N2
             EN = 1./EN
             EN2 = EN*EN
             G = A + EN2*(B+EN2*C)
             X = X0*EN2
             EX = EXP(X)
             D = D + .5*G*X*EN*EX + .25*(A+G)*(EX-1.)
             D = D*149.97E-15*Z4*EXP(-FI)/(C0*TER)
             IF (IX.EQ.2) THEN
                QHEP = QHEP + D
             ELSE
                QH = QH + D
                QHE = QHE + D
             ENDIF
   50     CONTINUE
*
*......HE II 2-PHOTON CONTINUUM
*
          Y = .0303875*FIMU
          T1 = SQRT(4.-3.*Y)
          T2 = SQRT(1.+3.*Y)
          TD = 10. - 9.*Y + T1
          T1 = (8.+6.*Y-9.*Y*Y-4.*T1)/TD
          TD = 1. + 9.*Y + T2
          T2 = (5.+12.*Y-9.*Y*Y-4.*T2)/TD
          Z = (T1+T2-0.5)**2/(1.-Y)
          QTPHE2 = 7.427E-13*Z*T**(.627)
          IF (FIMU.LT.8.227) THEN
*
*     H I  2-PHOTON CONTINUUM  ( ENDS AT 1215.5A )
*
             Y = 0.12155*FIMU
             HII = EL/(1.+HER+2.*HEPR)
             T1 = SQRT(4.-3.*Y)
             T2 = SQRT(1.+3.*Y)
             TD = 10. - 9.*Y + T1
             T1 = (8.+6.*Y-9.*Y*Y-4.*T1)/TD
             TD = 1. + 9.*Y + T2
             T2 = (5.+12.*Y-9.*Y*Y-4.*T2)/TD
             Z = (T1+T2-0.5)**2/(1.-Y)
             Z = 1.0548E-13*Z*T**(-.7172)
             X = 1.0 + 5.7594E-5*HII*T**(-.151) + 6.926E-6*EL*T**(-.373)
             QTP = Z/X
          ENDIF
*
*......EXTRA STATEMENT
* COMMENTED OUT TEMPORARILY BY PJS 6/83
*     QTP=QTP*HII
          IF (FIMU.GE.0.685485) THEN
*
*     H I  BRACKETT CONTINUUM AT 14588A.
*     HELIUM ASSUMED HYDROGENIC
*
             W = FIMU/.685485
             X = SQRT(W-1.)
             Z = EXP(16.-16.*ATAN(X)/X)/(1.-EXP(-8.*PI/X))
             W2 = W*W
             W3 = W*W2
             Z = Z*(48.+104.*W+42.*W2+3.*W3)*(192.+272.*W+72.*W2+3.*W3)
     :       /106183.
             Z = 3.117709E-15*Z*EXP(-.986094*(W-1.)*TI)*T32/(W3*W3*W)
             QH = QH + Z
             QHE = QHE + Z
             IF (FIMU.GE.1.210134) THEN
*
*     HE I RECOMBINATIONS TO 3 SINGLET P AT 8264A.
*
*     FOR HE I QUANTUM DEFECT FORMULAE, THE QUANTITIES MU, CHI AND G ARE
*     FIT BY LINEAR INTERPOLATION BETWEEN THE VALUES AT EPS=0 AND EPS=.5
*
                EPS = 0.09113921*FIMU - .1102907
                X = 1.43879*FIMU - 1.741135
                X = EXP(-X*TI)*T32
                SMU = 0.1397 - 0.030*EPS
                SCHI = -.272 - .096*EPS
                COSS = COS(PI*(3.011138+SCHI+SMU))
                GS = 0.805 + 1.08*EPS
                S = (GS*COSS)**2
                DMU = .00217 + .00375*EPS
                DCHI = .077 + .482*EPS
                COSD = COS(PI*(3.011138+DCHI+DMU))
                GD = 1.633 + 3.514*EPS
                D = (GD*COSD)**2
                Z = 0.136306E-15*X*(S+2.*D)
                QHE = QHE + Z
                IF (FIMU.GE.1.21864) THEN
*
*     H I  PASCHEN CONTINUUM  AT 8206A.
*
                   W = FIMU/1.21864
                   X = SQRT(W-1.)
                   Z = EXP(12.-12.*ATAN(X)/X)/(1.-EXP(-6.*PI/X))
                   Z = Z*(16.+20.*W+3.*W*W)*(16.+12.*W+W*W)/1131.
                   Z = 7.25239E-15*Z*EXP(-1.753056*(W-1.)*TI)*T32/W**5
                   QH = QH + Z
                   IF (FIMU.GE.1.220576) THEN
*
*     HE I RECOMBINATIONS TO 3 SINGLET D AT 8193A.
*
                      EPS = .09113921*FIMU - 0.1112423
                      X = 1.43879*FIMU - 1.756142
                      X = EXP(-X*TI)*T32
                      PMU = -.0122 - 0.010*EPS
                      PCHI = -.333 - .166*EPS
                      COSP = COS(PI*(2.99823+PCHI+PMU))
                      GP = 0.472 + 0.266*EPS
                      P = (GP*COSP)**2
                      FCHI = 0.222 + 0.390*EPS
                      COSF = COS(PI*(2.99823+FCHI))
                      GF = 1.851 + 1.726*EPS
                      F = (GF*COSF)**2
                      Z = 0.69037E-15*X*(0.4*P+0.6*F)
                      QHE = QHE + Z
                      IF (FIMU.GE.1.220918) THEN
*
*     HE I RECOMBINATIONS TO 3 TRIPLET D AT 8191A.
*
                         EPS = 0.09113921*FIMU - 0.1112734
                         X = 1.43879*FIMU - 1.756634
                         X = EXP(-X*TI)*T32
                         PMU = 0.0685 + 0.022*EPS
                         PCHI = -0.333 - 0.166*EPS
                         COSP = COS(PI*(2.99781+PCHI+PMU))
                         GP = 0.472 + 0.266*EPS
                         P = (GP*COSP)**2
                         FCHI = 0.222 + 0.390*EPS
                         COSF = COS(PI*(2.99781+FCHI))
                         GF = 1.851 + 1.726*EPS
                         F = (GF*COSF)**2
                         Z = 2.07198E-15*X*(0.4*P+0.6*F)
                         QHE = QHE + Z
                         IF (FIMU.GE.1.274616) THEN
*
*     HE I RECOMBINATIONS TO 3 TRIPLET P AT 7846A.
*
                            EPS = 0.09113921*FIMU - 0.1161675
                            X = 1.43879*FIMU - 1.833893
                            X = EXP(-X*TI)*T32
                            SMU = 0.2967 - 0.0403*EPS
                            SCHI = -.2740 - .09266*EPS
                            COSS = COS(PI*(2.933984+SMU+SCHI))
                            GS = 0.8116 + 1.0582*EPS
                            S = (GS*COSS)**2
                            DMU = 0.00295 + 0.007*EPS
                            DCHI = 0.0829 + 0.4827*EPS
                            COSD = COS(PI*(2.933984+DMU+DCHI))
                            GD = 1.6726 + 3.7002*EPS
                            D = (GD*COSD)**2
                            Z = 0.44203E-15*X*(S+2.*D)
                            QHE = QHE + Z
                            IF (FIMU.GE.1.344587) THEN
*
*     HE I RECOMBINATIONS TO 3 SINGLET S AT 7440A.
*
                               EPS = .09113921*FIMU - 0.1225446
                               X = 1.43879*FIMU - 1.934566
                               X = EXP(-X*TI)*T32
                               PMU = -0.0122 - 0.010*EPS
                               PCHI = -.0666 + 0.3692*EPS
                               COSP = COS(PI*(2.856623+PCHI+PMU))
                               GP = 1.3877 + 2.7518*EPS
                               P = (GP*COSP)**2
                               Z = 0.159642E-15*X*P
                               QHE = QHE + Z
                               IF (FIMU.GE.1.507393) THEN
*
*     HE I RECOMBINATIONS TO 3 TRIPLET S AT 6636A.
*
                                  EPS = .09113921*FIMU - 0.1373825
                                  X = 1.43879*FIMU - 2.168807
                                  X = EXP(-X*TI)*T32
                                  PMU = 0.0685 + 0.022*EPS
                                  PCHI = -.0640 + 0.364*EPS
                                  COSP = COS(PI*(2.6979523+PCHI+PMU))
                                  GP = 1.4514 + 2.6708*EPS
                                  P = (GP*COSP)**2
                                  Z = 0.57106E-15*X*P
                                  QHE = QHE + Z
                                  IF (FIMU.GE.2.717583) THEN
*
*     HE I RECOMBINATION TO 2 SINGLET P AT 3680A.
*
                                     EPS = .09113921*FIMU - .2476782
                                     X = 1.43879*FIMU - 3.910004
                                     X = EXP(-X*TI)*T32
                                     DMU = .00217 + .00375*EPS
                                     DCHI = .1592 + .3528*EPS
                                     COSD = COS(PI*(2.009352+DMU+DCHI))
                                     GD = 2.4116 + 2.502*EPS
                                     D = (GD*COSD)**2
                                     SMU = .1397 - .030*EPS
                                     SCHI = -.2967 - .0546*EPS
                                     COSS = COS(PI*(2.009352+SMU+SCHI))
                                     GS = .8925 + .688*EPS
                                     S = (GS*COSS)**2
                                     Z = 0.4587E-15*X*(S+2.*D)
                                     QHE = QHE + Z
                                     IF (FIMU.GE.2.741939) THEN
*
*     H I  BALMER CONTINUUM AT 3647A.
*
                                        W = FIMU/2.741939
                                        X = SQRT(W-1.)
                                        Z = EXP(8.-8.*ATAN(X)/X)
     :                                  /(1.-EXP(-4.*PI/X))
                                        Z = Z*(W+2.)*(W+4.)/15.
                                        Z = 23.632E-15*Z*EXP
     :                                  (-3.944375*(W-1.)*TI)
     :                                  *T32/W**3
                                        QH = QH + Z
                            IF (FIMU.GE.2.7431792) THEN
*
*     HE II  BRACKETT CONTINUUM AT 3645A.
*
                             W = FIMU/2.7431792
                             X = SQRT(W-1.)
                             Z = EXP(16.-16*ATAN(X)/X)
     :                       /(1.-EXP(-8.*PI/X))
                             W2 = W*W
                             W3 = W*W2
                             Z = Z*(48.+104.*W+42.*W2+3.*W3)
     :                       *(192.+272.*W+72.*W2+
     :                       3.*W3)/106183.
                             Z = 49.883344E-15*Z*EXP
     :                       (-3.9460438*(W-1.)*TI2)
     :                       *T322/(W3*W3*W)
                             QHEP = QHEP + Z
                               IF (FIMU.GE.2.92240) THEN
*
*     HE I RECOMBINATION TO 2 TRIPLET P AT 3422A.
*
                               EPS = .09113921*FIMU - .2663452
                               X = 1.43879*FIMU - 4.204693
                               X = EXP(-X*TI)*T32
                               SMU = 0.2967 - .0403*EPS
                               SCHI = -.2992 - .0528*EPS
                               COSS = COS
     :                         (PI*(1.93766+SMU+SCHI))
                               GS = 0.8927 + 0.983*EPS -
     :                         0.688*EPS**2
                               S = (GS*COSS)**2
                               DMU = 0.00295 + 0.007*EPS
                               DCHI = 0.1734 + 0.3392*EPS
                               COSD = COS
     :                         (PI*(1.93766+DMU+DCHI))
                               GD = 2.5644 + 2.7748*EPS
                               D = (GD*COSD)**2
                               Z = 1.5345E-15*X*(S+2.*D)
                               QHE = QHE + Z
                                  IF (FIMU.GE.3.203352) THEN
*
*     HE I RECOMBINATION TO 2 SINGLET S AT 3122A.
*
                                  EPS = .09113921*FIMU - .2919508
                                  X = 1.43879*FIMU - 4.608920
                                  X = EXP(-X*TI)*T32
                                  PMU = -.0122 - .010*EPS
                                  PCHI = -.0323 + .314*EPS
                                  COSP = COS
     :                            (PI*(1.850739+PMU+PCHI))
                                  GP = 1.9715 + 2.0448*EPS
                                  Z = 0.5960E-15*X*(GP*COSP)**2
                                  QHE = QHE + Z
                                     IF (FIMU.GE.3.845459) THEN
*
*     HE I RECOMBINATION TO 2 TRIPLET S AT 2600A.
*
                                     EPS = .09113921*FIMU - .3504720
                                     X = 1.43879*FIMU - 5.532771
                                     X = EXP(-X*TI)*T32
                                     PMU = .0685 + .022*EPS
                                     PCHI = -.0250 + .3186*EPS
                                     COSP = COS
     :                               (PI*(1.68917+PMU+PCHI))
                                     GP = 2.1235 + 1.8342*EPS
                                     Z = 2.3492E-15*X*(GP*COSP)**2
                                     QHE = QHE + Z
                                        IF (FIMU.GE.4.87673) THEN
*
*     HE II   PASCHEN CONTINUUM AT 2051A.
*
                                        W = FIMU/4.87673
                                        X = SQRT(W-1.)
                                        Z = EXP(12.-12.*ATAN(X)/X)
     :                                  /(1.-EXP(-6.*PI/X))
                                        Z = Z*(16.+20.*W+3.*W*W)
     :                                  *(16.+12.*W+W*W)/1131.
                                        Z = 116.03822E-15*Z*EXP
     :                                  (-7.015189*(W-1.)*TI2)
     :                                  *T322/W**5
                                        QHEP = QHEP + Z
                                        ENDIF
                                     ENDIF
                                  ENDIF
                               ENDIF
                            ENDIF
                                     ENDIF
                                  ENDIF
                               ENDIF
                            ENDIF
                         ENDIF
                      ENDIF
                   ENDIF
                ENDIF
             ENDIF
          ENDIF
*
*  SUM CONTRIBUTIONS
*
          Q = (QH+QTP+QHE*HER+(QHEP+QTPHE2)*HEPR)*COLHP
          YCOORD(NU) = Q*FIMU*FIMU*2.9979E-15
  100  CONTINUE
       END
