      SUBROUTINE TVELVC (IERROR)
C
C LATEST REVISION   JULY, 1984
C
C PURPOSE                TO PROVIDE A SIMPLE DEMONSTRATION OF
C                        SUBROUTINES VELVCT AND EZVEC.
C
C USAGE                  CALL TVELVC (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          AN INTEGER VARIABLE
C                          =0 IF THERE IS A NORMAL EXIT FROM THE
C                             ROUTINES VELVCT AND EZVEC
C                          =1 OTHERWISE
C
C I/O                    IF THERE IS A NORMAL EXIT FROM THE ROUTINES
C                        VELVCT AND EZVEC THE MESSAGE
C                          VELVCT TEST SUCCESSFUL . . . SEE PLOTS TO
C                          VERIFY PERFORMANCE
C                        IS PRINTED.
C
C PRECISION              SINGLE
C
C LANGUAGE               FORTRAN
C
C HISTORY                ORIGINALLY WRITTEN NOVEMBER 1976
C
C ALGORITHM              ROUTINE TVELVC CALLS ROUTINES EZVEC AND
C                        VELVCT ONCE.  EACH CALL PRODUCES A PLOT
C                        REPRESENTING A VECTOR FIELD.  THE VECTOR
C                        FIELD IS OBTAINED FROM THE FUNCTION
C                          Z(X,Y) = X + Y + 1./((X-.1)**2+Y**2+.09)
C                                   -1./((X+.1)**2+Y**2+.09),
C                        BY USING THE DIRECTION OF THE Z GRADIENT
C                        VECTORS AND THE LOGARITHM OF THE ABSOLUTE
C                        VALUE OF THE COMPONENTS.
C
C
C
C
      DIMENSION       U(21,25)   ,V(21,25)
C
C SPECIFY COORDS FOR PLOT TITLES
C
        DATA IX/94/,IY/1000/
C
C SPECIFY SOME OF THE ARGUMENTS IN VELVCT CALLING SEQUENCE
C
      DATA FLO/0./,HI/0./,NSET/0/,LENGTH/0/,ISPV/0/,SPV/0./
C
C INITIALIZE ERROR PARAMETER
C
      IERROR = 1
C
C SPECIFY VELOCITY FIELD FUNCTIONS U AND V
C
      M = 21
      N = 25
      DO  20 I=1,M
         X = .1*FLOAT(I-11)
         DO  10 J=1,N
            Y = .1*FLOAT(J-13)
            DZDX = 1.-2.*(X-.10)/((X-.10)**2+Y**2+.09)**2+
     1             2.*(X+.10)/((X+.10)**2+Y**2+.09)**2
            DZDY = 1.-2.*Y/((X-.10)**2+Y**2+.09)**2+
     1             2.*Y/((X+.10)**2+Y**2+.09)**2
            UVMAG = ALOG(SQRT(DZDX*DZDX+DZDY*DZDY))
            UVDIR = ATAN2(DZDY,DZDX)
            U(I,J) = UVMAG*COS(UVDIR)
            V(I,J) = UVMAG*SIN(UVDIR)
   10    CONTINUE
   20 CONTINUE
C
C CALL WTSTR FOR EZVEC PLOT TITLE
C
        CALL GQCNTN(IERR,ICN)
        CALL GSELNT(0)
        X = CPUX(IX)
        Y = CPUY(IY)
      CALL WTSTR (X,Y,'DEMONSTRATION PLOT FOR ENTRY EZVEC OF VELVCT',
     1           2,0,-1)
        CALL GSELNT(ICN)
C
C CALL EZVEC FOR VELOCITY FIELD PLOT
C
      CALL EZVEC (U,V,M,N)
C
C CALL VELVCT FOR VELOCITY FIELD PLOT
C
      CALL VELVCT (U,M,V,M,M,N,FLO,HI,NSET,LENGTH,ISPV,SPV)
C
C CALL WTSTR FOR VELVCT PLOT TITLE
C
        CALL GQCNTN(IERR,ICN)
        CALL GSELNT(0)
        X = CPUX(IX)
        Y = CPUY(IY)
      CALL WTSTR (X,Y,
     1           'DEMONSTRATION PLOT FOR ENTRY VELVCT OF VELVCT',2,
     2           0,-1)
        CALL GSELNT(ICN)
      CALL FRAME
C
      IERROR = 0
      WRITE (6,1001)
      RETURN
C
 1001 FORMAT ('     VELVCT TEST SUCCESSFUL',24X,
     1        'SEE PLOTS TO VERIFY PERFORMANCE')
C
      END
