*     This program demonstrates animation and 3D geometry in PGPLOT. It 
*     requires a fast, interactive display, e.g., /XWIN. Do not
*     specify a hardcopy device. The speed of the animation is limited by
*     the cpu speed of the host computer.
*     
*     Thanks to Dr Martin Weisser:
*     Date: Sun, 18 May 1997 16:14:01 CET
*     From: weisser@chclu.chemie.uni-konstanz.de


      PROGRAM PGDEM17
C-----------------------------------------------------------------------
C     Demonstration program for PGPLOT.
C-----------------------------------------------------------------------
      INTEGER PGOPEN
C
      WRITE (*,*) 'PGPLOT: Demonstration of animation and 3D geometry'
      WRITE (*,*) 'Select a fast, interactive device, e.g., /XWINDOW'
      IF (PGOPEN('?') .LE. 0) STOP
      CALL POLY3D
      CALL PGCLOS
C-----------------------------------------------------------------------
      END

      SUBROUTINE POLY3D
C     
      INTEGER NFRAMS
C     
      INTEGER NTOT, NLIN, IPOS, IFIRST          
      REAL T, T1, T2, T3, PI, W, W1, TET, TET1, ROT, ROT1
C     
      PARAMETER (NTOT=34)
      PARAMETER (T=1.618)
      PARAMETER (T1=1.0+T)
      PARAMETER (T2=-T)
      PARAMETER (T3=-T1)
      PARAMETER (W=0.60*T)
      PARAMETER (W1=-W)
      PARAMETER (TET=0.37)
      PARAMETER (TET1=-TET)
      PARAMETER (ROT=0.13)
      PARAMETER (ROT1=-ROT)
      PARAMETER (NLIN=49)
C     
      INTEGER I, J, L, III, ILINE, NTOTM6
      INTEGER ICDFOR, ICCFOR, ICTFOR, ICLFOR
      INTEGER ICDBCK, ICCBCK, ICTBCK, ICLBCK
      INTEGER ITYPE(NTOT), IARRAY(NLIN), JARRAY(NLIN), LITYPE(NLIN)
      REAL RQ, ZZ
      REAL THAXI1, PHAXI1, ALFA1, THAXI2, PHAXI2, ALFA2
      REAL THAXI3, PHAXI3, ALFA3, THAXI4, PHAXI4, ALFA4 
      REAL XOFF, YOFF, ZOFF
      REAL XARRAY(NTOT), YARRAY(NTOT), ZARRAY(NTOT), DISTAN(NLIN)
      REAL POLYS(3,NTOT), X(2), Y(2), C(3), CROT(3), RPOL(3,3)
      PARAMETER (PI=3.14159265359)
C     
C     Cartesian coordinates of the polygons 
C     
      DATA POLYS/ T, T, T,       T, T,T2,
     D     T,T2, T,       T,T2,T2,
     D     T2, T, T,      T2, T,T2,
     D     T2,T2, T,      T2,T2,T2,
     D     T1,1.0,0.0,    T1,-1.0,0.0,
     D     T3,1.0,0.0,    T3,-1.0,0.0,
     D     0.0,T1,1.0,    0.0,T1,-1.0,
     D     0.0,T3,1.0,    0.0,T3,-1.0,
     D     1.0,0.0,T1,    -1.0,0.0,T1,
     D     1.0,0.0,T3,   -1.0,0.0,T3, 
     C     W,    W,    W,     W,    W,   W1,
     C     W,   W1,    W,     W,   W1,   W1,
     C     W1,    W,    W,    W1,   W1,    W,
     C     W1,    W,   W1,    W1,   W1,   W1,                
     T     TET,  TET,  TET, TET1, TET1,  TET,
     T     TET1,  TET, TET1,  TET, TET1, TET1, 
     L     ROT,  0.0,  0.0, ROT1,  0.0,  0.0/
C     
      DATA ITYPE/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     C     2,2,2,2,2,2,2,2,
     T     3,3,3,3,
     L     4,4/
C     
C     Initialize the plot (no labels).
C     
      CALL PGENV(-3.2,3.2,-3.2,3.2,1,-2)
C     
C     Switch from page to page without typing return.  
C     
      CALL PGASK(.FALSE.)
C     
C     Rotation axis of the polygons 
C     
      THAXI1 = PI/4.0
      PHAXI1 = PI/4.5
      ALFA1  = 0.0
      THAXI2 = PI/6.0
      PHAXI2 = 0.0
      ALFA2  = 0.02
      THAXI3 = PI/2.0
      PHAXI3 = -PI/3.0
      ALFA3  = 0.0
      THAXI4 = -0.03
      PHAXI4 = PI/7.0
      ALFA4  = 0.9
C     
      XOFF=0.0
      YOFF=0.0
      ZOFF=0.0
      NTOTM6=NTOT-6
C     
C     Colors  
C     
      ICDFOR = 3
      ICDBCK = 10
C     
      ICCFOR = 8
      ICCBCK = 2
C     
      ICTFOR = 5
      ICTBCK = 4
C     
      ICLFOR = 1
      ICLBCK = 7
C     
      IPOS=1
C     
      WRITE(*,*)' Rotation with increasing velocity'
C     
      NFRAMS=3500 
C     
      DO 12 I=1,NTOT
         XARRAY(I) = POLYS(1,I)
         YARRAY(I) = POLYS(2,I)
         ZARRAY(I) = POLYS(3,I)
 12   CONTINUE
C     
      DO 30 L=1,NFRAMS
C     
         CALL PGBBUF
         CALL PGERAS
C     
         CALL SORTPP(NTOT,ITYPE,ZARRAY,YARRAY,XARRAY)
C     
         IFIRST=0
         DO 13 I=1,NTOT
            IF((ZARRAY(I).GE.0.0).AND.(IFIRST.EQ.0)) THEN
               IFIRST = 1
               IPOS = I
            END IF
 13      CONTINUE
C     
         IF(L.EQ.2800) CALL OFFSET (XOFF,YOFF,ZOFF)
         IF (MOD(L,500).EQ.0) THEN 
            CALL CHNAX(THAXI3,PHAXI3,THAXI2,PHAXI2,THAXI1,PHAXI1)
         END IF
C     
         DO 33 I=1,IPOS-1
            IF (ITYPE(I).EQ.1) THEN 
               CALL PGSCI(ICDBCK)
               CALL PGSLW(18)
            ELSE IF (ITYPE(I).EQ.2) THEN 
               CALL PGSCI(ICCBCK)
               CALL PGSLW(17)
            ELSE IF (ITYPE(I).EQ.3) THEN
               CALL PGSCI(ICTBCK)
               CALL PGSLW(15)
            ELSE 
               CALL PGSCI(ICLBCK)
               CALL PGSLW(14)
            END IF
            ZZ = ZARRAY(I)
            CALL PGPT(1,XARRAY(I)+0.2*ZZ,YARRAY(I)+0.3*ZZ,9)
 33      CONTINUE
C     
         DO 44 I=IPOS,NTOT
            IF (ITYPE(I).EQ.1) THEN 
               CALL PGSCI(ICDFOR)
               CALL PGSLW(18)
            ELSE IF (ITYPE(I).EQ.2) THEN 
               CALL PGSCI(ICCFOR)
               CALL PGSLW(17)
            ELSE IF (ITYPE(I).EQ.3) THEN
               CALL PGSCI(ICTFOR)
               CALL PGSLW(15)
            ELSE 
               CALL PGSCI(ICLFOR)
               CALL PGSLW(14)
            END IF
            ZZ = ZARRAY(I)
            CALL PGPT(1,XARRAY(I)+0.2*ZZ,YARRAY(I)+0.3*ZZ,9)
 44      CONTINUE
C     
         ILINE=0
C     
         DO 2000 I=2,NTOT
            DO 1000 J=1,I-1
               IF (ITYPE(I).EQ.ITYPE(J)) THEN
                  RQ = 0.0
                  RQ = RQ + ( (XARRAY(I)-XARRAY(J))**2+
     #                 (YARRAY(I)-YARRAY(J))**2+
     #                 (ZARRAY(I)-ZARRAY(J))**2  )
C     
                  IF ( ((RQ-0.0676)  .LT.0.001).OR.
     #                 ((RQ-1.095199).LT.0.001).OR.          
     #                 ((RQ-3.769809).LT.0.001).OR.
     #                 ((RQ-4.000000).LT.0.001)     ) THEN 
                     ILINE = ILINE + 1
                     DISTAN(ILINE) = ZARRAY(I)+ZARRAY(J)
                     IF(DISTAN(ILINE).LT.0.0) THEN 
                        LITYPE(ILINE) = -ITYPE(I)
                     ELSE  
                        LITYPE(ILINE) =  ITYPE(I)
                     END IF 
                     IARRAY(ILINE) = I
                     JARRAY(ILINE) = J
                  END IF
               END IF
 1000       CONTINUE
 2000    CONTINUE
C     
         CALL SORTLI(ILINE,DISTAN,IARRAY,JARRAY,LITYPE)
C     
         DO 3000 III=1,ILINE
            I=IARRAY(III)
            J=JARRAY(III)               
            ZZ = ZARRAY(I)
            X(1) = XARRAY(I)+0.2*ZZ
            Y(1) = YARRAY(I)+0.3*ZZ
            ZZ = ZARRAY(J)
            X(2) = XARRAY(J)+0.2*ZZ
            Y(2) = YARRAY(J)+0.3*ZZ
            IF (LITYPE(III).GT.0) THEN
               IF(LITYPE(III).EQ.1) THEN  
                  CALL PGSLW(10)
                  CALL PGSCI(ICDFOR)
               ELSE IF (LITYPE(III).EQ.2) THEN          
                  CALL PGSLW(8)
                  CALL PGSCI(ICCFOR)
               ELSE IF (LITYPE(III).EQ.3) THEN
                  CALL PGSLW(6)
                  CALL PGSCI(ICTFOR)
               ELSE
                  CALL PGSLW(4)
                  CALL PGSCI(ICLFOR)
               END IF
            ELSE
               IF(LITYPE(III).EQ.-1) THEN  
                  CALL PGSLW(7)
                  CALL PGSCI(ICDBCK)
               ELSE IF (LITYPE(III).EQ.-2) THEN          
                  CALL PGSLW(4)
                  CALL PGSCI(ICCBCK)
               ELSE IF (LITYPE(III).EQ.-3) THEN
                  CALL PGSLW(3)
                  CALL PGSCI(ICTBCK)
               ELSE
                  CALL PGSLW(2)
                  CALL PGSCI(ICLBCK)
               END IF
            END IF
            CALL PGLINE(2,X,Y)
 3000    CONTINUE
C     
         DO 45 I=NTOTM6,NTOT
            IF (ITYPE(I).EQ.1) THEN 
               CALL PGSCI(ICDFOR)
               CALL PGSLW(19)
               ZZ = ZARRAY(I)
               CALL PGPT(1,XARRAY(I)+0.2*ZZ,YARRAY(I)+0.3*ZZ,9)
            END IF 
 45      CONTINUE          
C     
         DO 4000 III=1,NTOT
            IF (ITYPE(III).EQ.1) THEN 
               CALL POLMAT(RPOL,THAXI1,PHAXI1,ALFA1)
            ELSE IF (ITYPE(III).EQ.2) THEN 
               CALL POLMAT(RPOL,THAXI2,PHAXI2,ALFA2)
            ELSE IF (ITYPE(III).EQ.3) THEN 
               CALL POLMAT(RPOL,THAXI3,PHAXI3,ALFA3)
            ELSE
               CALL POLMAT(RPOL,THAXI4,PHAXI4,ALFA4)
            END IF
            C(1)=XARRAY(III)
            C(2)=YARRAY(III)
            C(3)=ZARRAY(III)
            CALL MMULT (C,RPOL,CROT)
            XARRAY(III)=CROT(1)+XOFF
            YARRAY(III)=CROT(2)+YOFF
            ZARRAY(III)=CROT(3)+ZOFF
 4000    CONTINUE
C     
         ALFA1 = ALFA1+1.5E-5*(1.0+2.0*L/4000.)
         ALFA2 = ALFA2-2.0E-5*(1.0+4.0*L/4000.)
         ALFA3 = ALFA3-4.0E-5*(1.0+3.0*L/4000.)
C     
         CALL PGEBUF
C     
 30   CONTINUE
C     
C-----------------------------------------------------------------------
      END

      SUBROUTINE MMULT (VECTOR,RMATRX,ROTVEC)
C     
C     Matrix multiplication 
C     
      REAL VECTOR(3)
      REAL ROTVEC(3)
      REAL RMATRX(3,3)
C     
      ROTVEC(1)=RMATRX(1,1)*VECTOR(1)+RMATRX(1,2)*VECTOR(2)+
     #          RMATRX(1,3)*VECTOR(3)
      ROTVEC(2)=RMATRX(2,1)*VECTOR(1)+RMATRX(2,2)*VECTOR(2)+
     #          RMATRX(2,3)*VECTOR(3)
      ROTVEC(3)=RMATRX(3,1)*VECTOR(1)+RMATRX(3,2)*VECTOR(2)+
     #          RMATRX(3,3)*VECTOR(3)
C     
      RETURN
      END        

      SUBROUTINE POLMAT(RPOL,THAXI,PHAXI,ALFA)
C     
      REAL THAXI,PHAXI,ALFA
      REAL RPOL(3,3)
      REAL SINT,SINTQ,SINP,SINPQ,SINA
      REAL COST,COSTQ,COSP,COSPQ,COSA,EMCOSA
C     
      SINT = SIN(THAXI)
      COST = COS(THAXI)
      SINP = SIN(PHAXI)
      COSP = COS(PHAXI)
      SINA = SIN(ALFA)
      COSA = COS(ALFA)
      EMCOSA = 1.0-COSA
C     
      SINTQ = SINT*SINT
      COSTQ = COST*COST
      SINPQ = SINP*SINP
      COSPQ = COSP*COSP
C     
      RPOL(1,1) =  COSA+COSPQ*SINTQ*EMCOSA
      RPOL(2,1) =  COST*SINA+SINP*COSP*SINTQ*EMCOSA
      RPOL(3,1) = -SINP*SINT*SINA+SINT*COST*COSP*EMCOSA
      RPOL(1,2) = -COST*SINA+SINP*COSP*SINTQ*EMCOSA
      RPOL(2,2) =  COSA+SINPQ*SINTQ*EMCOSA
      RPOL(2,3) = -COSP*SINT*SINA+SINP*SINT*COST*EMCOSA
      RPOL(1,3) =  SINP*SINT*SINA+SINT*COST*COSP*EMCOSA
      RPOL(3,2) =  COSP*SINT*SINA+COST*SINT*SINP*EMCOSA
      RPOL(3,3) =  COSA+COSTQ*EMCOSA
C     
      RETURN 
      END

      SUBROUTINE SORTPP(N,ITYPE,RA1,RA2,RA3)
C     
      REAL RA1, RA2, RA3, RRA1, RRA2, RRA3
      INTEGER ITYPE(*), L, N, IR, I, J, IRRA1
      DIMENSION RA1(*), RA2(*), RA3(*)
      L=N/2+1
      IR=N
 10   CONTINUE
      IF(L.GT.1)THEN
         L=L-1
         RRA1=RA1(L)
         IRRA1=ITYPE(L)
         RRA2=RA2(L)
         RRA3=RA3(L)
      ELSE
         RRA1=RA1(IR)
         IRRA1=ITYPE(IR)
         RRA2=RA2(IR)
         RRA3=RA3(IR)
         RA1(IR)=RA1(1)
         ITYPE(IR)=ITYPE(1)
         RA2(IR)=RA2(1)
         RA3(IR)=RA3(1)
         IR=IR-1
         IF(IR.EQ.1)THEN
            RA1(1)=RRA1
            ITYPE(1)=IRRA1
            RA2(1)=RRA2
            RA3(1)=RRA3
            RETURN
         ENDIF
      ENDIF
      I=L
      J=L+L
 20   IF(J.LE.IR)THEN
         IF(J.LT.IR)THEN
            IF(RA1(J).LT.RA1(J+1))J=J+1
         ENDIF
         IF(RRA1.LT.RA1(J))THEN
            RA1(I)=RA1(J)
            ITYPE(I)=ITYPE(J)
            RA2(I)=RA2(J)
            RA3(I)=RA3(J)
            I=J
            J=J+J
         ELSE
            J=IR+1
         ENDIF
         GO TO 20
      ENDIF
      RA1(I)=RRA1
      ITYPE(I)=IRRA1
      RA2(I)=RRA2
      RA3(I)=RRA3
      GO TO 10
      END
C     
      SUBROUTINE SORTLI(N,RA1,IA1,IA2,IA3)
C     
      REAL RA1, RRA1
      INTEGER L, N, IR, I, J, IRA1, IRA2, IRA3, IA1, IA2, IA3
      DIMENSION RA1(*), IA1(*), IA2(*) , IA3(*)
      L=N/2+1
      IR=N
 10   CONTINUE
      IF(L.GT.1)THEN
         L=L-1
         RRA1=RA1(L)
         IRA1=IA1(L)
         IRA2=IA2(L)
         IRA3=IA3(L)
      ELSE
         RRA1=RA1(IR)
         IRA1=IA1(IR)
         IRA2=IA2(IR)
         IRA3=IA3(IR)
         RA1(IR)=RA1(1)
         IA1(IR)=IA1(1)
         IA2(IR)=IA2(1)
         IA3(IR)=IA3(1)
         IR=IR-1
         IF(IR.EQ.1)THEN
            RA1(1)=RRA1
            IA1(1)=IRA1
            IA2(1)=IRA2
            IA3(1)=IRA3
            RETURN
         ENDIF
      ENDIF
      I=L
      J=L+L
 20   IF(J.LE.IR)THEN
         IF(J.LT.IR)THEN
            IF(RA1(J).LT.RA1(J+1))J=J+1
         ENDIF
         IF(RRA1.LT.RA1(J))THEN
            RA1(I)=RA1(J)
            IA1(I)=IA1(J)
            IA2(I)=IA2(J)
            IA3(I)=IA3(J)
            I=J
            J=J+J
         ELSE
            J=IR+1
         ENDIF
         GO TO 20
      ENDIF
      RA1(I)=RRA1
      IA1(I)=IRA1
      IA2(I)=IRA2
      IA3(I)=IRA3
      GO TO 10
      END

      SUBROUTINE OFFSET (XOFF,YOFF,ZOFF)
C
      REAL XOFF,YOFF,ZOFF
C
      WRITE(*,*)' Rotation with shifting'
      XOFF=-0.0002
      YOFF=+0.0004
      ZOFF=-0.0002
      RETURN
      END

      SUBROUTINE CHNAX
     #     (THAXI3,PHAXI3,THAXI2,PHAXI2,THAXI1,PHAXI1)
C     
      REAL THAXI1,PHAXI1,PHAXI2,THAXI2,PHAXI3,THAXI3,PI
      PARAMETER (PI=3.14159265359)
C     
      THAXI3 = THAXI3 - PI*0.32
      PHAXI3 = PHAXI3 + PI*0.28
      THAXI2 = THAXI2 + PI*0.18
      PHAXI2 = PHAXI2 - PI*0.14
      THAXI1 = THAXI1 - PI*0.12
      PHAXI1 = PHAXI1 + PI*0.08
C     
      RETURN
      END






