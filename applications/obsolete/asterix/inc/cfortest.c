/* cfortest.c  3.9 */          /* anonymous ftp@zebra.desy.de */
/* Burkhard Burow  burow@desy.de                 1990 - 1997. */

#include <stdio.h>
#include <stdlib.h>    /* qsort EXIT_SUCCESS */
#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif
#include "cfortran.h"

#define EASY_SELECT     /* To see the various examples select one of: 
        EASY_SELECT,SUBT_SELECT,  SZ_SELECT,  FT_SELECT,  FZ_SELECT, SS1_SELECT,
         ABC_SELECT,  RR_SELECT, REV_SELECT, FCB_SELECT,  EQ_SELECT,  F0_SELECT,
          FA_SELECT,  FB_SELECT,  FC_SELECT,  FD_SELECT,  FE_SELECT,  FF_SELECT,
          FG_SELECT,  FH_SELECT,  FI_SELECT,  FJ_SELECT,  FK_SELECT,  FL_SELECT,
          FM_SELECT,  FN_SELECT,  VV_SELECT,  V7_SELECT,FAND_SELECT,FORR_SELECT,
      STRTOK_SELECT,USER_SELECT, FUN_SELECT, SUB_SELECT.   Q_SELECT,  E2_SELECT,
        FSTR_SELECT,CF14_SELECT, F20_SELECT, SZ1_SELECT,  PZ_SELECT.
*/

/* FORTRAN_REAL, instead of float, is only required for CRAY T3E.             */
/* DOUBLE_PRECISION, instead of double, is only required for CRAY (not T3E).  */

#ifdef NAGf90Fortran
/*
   Following is only a C main calling f90-compiled Fortran routines.
   Irrelevant when Fortran PROGRAM calls C routines.

   Advice for 'NAGWare f90 compiler Version 2.0a(264)'
   and presumably also for more recent versions:
    C main must call f90_init and f90_finish. See kludge below.
    Initialization and termination behavior of f90 is easily investigated, e.g.
           burow[9] cat f.f
                    end
           burow[10] f90 -S f.f
           burow[11] cat f.c
           #include <f90.h>
           int main(argc,argv) int argc; char *argv[];
           {
           f90_init(argc,argv);
           f90_finish(0);
           }

   Advice for earlier incarnations of NAGWare f90:
    NAG f90 library hijacks main() and the user's program starts with a call to
    void f90_main(void);                   
    No problem for cfortest.c, but woe is the C application which uses
    command line arguments for which NAG f90 provides no support.
 */

/* Assume Version 2.0a(264) or more recent. */
main(argc, argv) int argc; char *argv[];
{f90_init(argc,argv); f90_main(argc,argv); f90_finish(0); return EXIT_SUCCESS;}
#define main f90_main

#endif

#ifdef EASY_SELECT
                  PROTOCCALLSFSUB2(EASY,easy, PINT, INT)
#define EASY(A,B)      CCALLSFSUB2(EASY,easy, PINT, INT, A, B)

main() {
int a;
printf("\nEASY EXAMPLE\n");
EASY(a,7);
printf("The FORTRAN routine EASY(a,7) returns a = %d\n", a);
return EXIT_SUCCESS;
}
#endif

#ifdef SUBT_SELECT
               PROTOCCALLSFSUB3(SUBT,subt, PSTRINGV, STRINGV, FLOAT)
#define SUBT(A,B,C) CCALLSFSUB3(SUBT,subt, PSTRINGV, STRINGV, FLOAT, A, B, C)

int main() {
static char v[][5] = {"000 ", "1", "22", " "};
static char w[][9]  = {" ", "bb","ccc ","dddd"};
SUBT(v, w, 10.);
printf("main:v=%s,%s,%s,%s. PSTRINGV => Has had trailing blanks stripped.\n",
       v[0],v[1],v[2],v[3]);
printf("main:w=%s,%s,%s,%s. STRINGV => malloc'd copy for FORTRAN=> C intact.\n"
       ,w[0],w[1],w[2],w[3]);
return EXIT_SUCCESS;
}
#endif

#ifdef SZ_SELECT
#define sz_ELEMS_1   ZTRINGV_ARGS(3)
#define sz_ELEMLEN_1 ZTRINGV_NUM(6)
#define sz_ELEMS_2   ZTRINGV_NUM(4)
#define sz_ELEMLEN_2 ZTRINGV_NUM(8)
             PROTOCCALLSFSUB3(SZ,sz, PZTRINGV, ZTRINGV, INT)
#define SZ(A,B,C) CCALLSFSUB3(SZ,sz, PZTRINGV, ZTRINGV, INT, A, B, C)

int main() {
static char v[][7] = {"000 ", "1", "22", " "};
static char w[][9]  = {" ", "bb","ccc ","dddd"};
SZ(v, w, 4);
printf("main:v=%s,%s,%s,%s. PZTRINGV => Has had trailing blanks stripped.\n",
       v[0],v[1],v[2],v[3]);
printf("main:w=%s,%s,%s,%s. ZTRINGV => malloc'd copy for FORTRAN=> C intact.\n"
       ,w[0],w[1],w[2],w[3]);
return EXIT_SUCCESS;
}
#endif

#ifdef FT_SELECT
PROTOCCALLSFFUN3(STRING,FT,ft, PSTRINGV, STRINGV, FLOAT)
#define FT(A,B,C) CCALLSFFUN3(FT,ft, PSTRINGV, STRINGV, FLOAT, A, B, C)

main() {
static char v[][5] = {"000 ", "1", "22", " "};
static char w[][9]  = {" ", "bb","ccc ","dddd"};
FORTRAN_REAL a = 10.0;
printf("FT(v, w, a); returns:%s.\n",FT(v, w, a));
printf("main:v=%s,%s,%s,%s. PSTRINGV => Has had trailing blanks stripped.\n",
       v[0],v[1],v[2],v[3]);
printf("main:w=%s,%s,%s,%s. STRINGV => malloc'd copy for FORTRAN=> C intact.\n"
       ,w[0],w[1],w[2],w[3]);
return EXIT_SUCCESS;
}
#endif

#ifdef FZ_SELECT
#define fz_ELEMS_1   ZTRINGV_ARGF(3)
#define fz_ELEMLEN_1 ZTRINGV_NUM(6)
#define fz_ELEMS_2   ZTRINGV_NUM(4)
#define fz_ELEMLEN_2 ZTRINGV_NUM(8)
PROTOCCALLSFFUN3(STRING,FZ,fz, PZTRINGV, ZTRINGV, INT)
#define FZ(A,B,C) CCALLSFFUN3(FZ,fz, PZTRINGV, ZTRINGV, INT, A, B, C)

main() {
static char v[][7] = {"000 ", "1", "22", " "};
static char w[][9]  = {" ", "bb","ccc ","dddd"};
printf("FZ(v, w, a); returns:%s.\n",FZ(v, w, 4));
printf("main:v=%s,%s,%s,%s. PZTRINGV => Has had trailing blanks stripped.\n",
       v[0],v[1],v[2],v[3]);
printf("main:w=%s,%s,%s,%s. ZTRINGV => malloc'd copy for FORTRAN=> C intact.\n"
       ,w[0],w[1],w[2],w[3]);
return EXIT_SUCCESS;
}
#endif

#ifdef SS1_SELECT
                       PROTOCCALLSFSUB1(SS1,ss1, PSTRING)
#define SS1(A1)             CCALLSFSUB1(SS1,ss1, PSTRING, A1)
                       PROTOCCALLSFSUB1(FORSTR1,forstr1, PSTRING)
#define FORSTR1(A1)         CCALLSFSUB1(FORSTR1,forstr1, PSTRING, A1)

main() {
static char b[] = "abcdefghij", forb[13] = "abcdefghijkl";
SS1(b); FORSTR1(forb);
printf("SS1(b) returns b = %s; FORSTR1(forb) = returns forb = %s;\n", b, forb);
return EXIT_SUCCESS;
}
#endif

#ifdef ABC_SELECT
                 PROTOCCALLSFSUB3(ABC,abc, STRING, PSTRING, PSTRING)
#define ABC(A1,A2,A3) CCALLSFSUB3(ABC,abc, STRING, PSTRING, PSTRING, A1, A2, A3)

main() {
static char aa[] = "one  ", bb[] = "two  ", cc[] = "three"; int i; 
for (i=0; i<10; i++) {printf("%s;%s;%s;\n",aa,bb,cc); ABC(aa,bb,cc);}
return EXIT_SUCCESS;
}
#endif

#ifdef RR_SELECT
PROTOCCALLSFFUN1(FLOAT,RR,rr,INT)
#define RR(A1)               CCALLSFFUN1(RR,rr, INT, A1)
PROTOCCALLSFFUN0(STRING,FORSTR2,forstr2)
#define FORSTR2()           CCALLSFFUN0(FORSTR2,forstr2)
PROTOCCALLSFFUN1(STRING,FORSTR,forstr,STRING)
#define FORSTR(A1)          CCALLSFFUN1(FORSTR,forstr, STRING, A1)

main() {
static char aa[] = "one";
int rrr = 333;
printf("RR(rrr=%d) returns int arg. as float:%f\n",rrr,RR(rrr));
printf("FORSTR(aa=%s) returns the string arg. as:%s<-end here\n",aa,FORSTR(aa));
printf("FORSTR2() returns the string constant:%s<-end here\n",FORSTR2());
return EXIT_SUCCESS;
}
#endif

#ifdef REV_SELECT
                          PROTOCCALLSFFUN1(INT,FREV,frev, INTV)
#define FREV(A1)               CCALLSFFUN1(FREV,frev, INTV, A1)
/* K&R mode of SunOS and Ultrix C prepro. dissallow space before FREV,
 * since they then go into an infinite loop of FREV replacement.       */

                          PROTOCCALLSFSUB1(REV,rev, INTV)
#define REV(A1)                CCALLSFSUB1(REV,rev, INTV, A1)

main() {
static int a[] = {1,2};
printf("REV(a[0,1]=%d,%d) receives:",a[0],a[1]);
REV(a); printf("a[0,1]=%d,%d\n",a[0],a[1]);
printf("FREV(a[0,1]=%d,%d) receives:",a[0],a[1]);
printf("%d",FREV(a)); printf(" with a[0,1]=%d,%d\n",a[0],a[1]);
return EXIT_SUCCESS;
}
#endif

#ifdef FCB_SELECT
                          PROTOCCALLSFSUB0(FFCB,ffcb)
#define FFCB()                 CCALLSFSUB0(FFCB,ffcb)

typedef struct { char v[13],w[4][13],x[2][3][13]; } FCB_DEF;
#define Fcb COMMON_BLOCK(FCB,fcb)
COMMON_BLOCK_DEF(FCB_DEF,Fcb);
FCB_DEF Fcb;

main() {
char cv[14];
static char cw[4][14]    = {"C's w[0]", "C's w[1]", "C's w[2]", "C's w[3]"};
static char cx[2][3][14] = {{"C's x[0][0]", "C's x[0][1]", "C's x[0][2]"}, 
                            {"C's x[1][0]", "C's x[1][1]", "C's x[1][2]"}};
C2FCBSTR("C's V" ,Fcb.v,0);
C2FCBSTR(cw      ,Fcb.w,1);
C2FCBSTR(cx      ,Fcb.x,2);
FFCB();
FCB2CSTR(Fcb.v   ,cv   ,0);
FCB2CSTR(Fcb.w   ,cw   ,1);
FCB2CSTR(Fcb.x   ,cx   ,2);
printf("FFCB returns v = %s.\n",cv);
printf("FFCB returns w[1,2,3,4] = %s,%s,%s,%s.\n",cw[0],cw[1],cw[2],cw[3]);
printf("FFCB returns x[0,(1,2,3)] = %s,%s,%s.\n",cx[0][0],cx[0][1],cx[0][2]);
printf("FFCB returns x[1,(1,2,3)] = %s,%s,%s.\n",cx[1][0],cx[1][1],cx[1][2]);
return EXIT_SUCCESS;
}
#endif

#ifdef EQ_SELECT
                         PROTOCCALLSFSUB0(FEQ,feq)
#define FEQ()                 CCALLSFSUB0(FEQ,feq)

#define KWBANK 690
typedef struct {
  int nzebra;
  FORTRAN_REAL gversn,zversn;
  int ixstor,ixdiv,ixcons;
  FORTRAN_REAL fendq[16];
  union {
    struct {
      int Lmain,Lr1; 
      union {FORTRAN_REAL Ws[KWBANK]; int Iws[2];}u;
    }s;
    union {
      int Lq[80];
      struct {
        int dummy[8];
        union {FORTRAN_REAL Q[2]; int Iq[2];}u;
      }s;
    }u;
  }u;
} GCBANK_DEF;
#define lmain u.s.Lmain
#define lr1   u.s.Lr1
#define ws    u.s.u.Ws
#define iws   u.s.u.Iws
#define lq    u.u.Lq
#define q     u.u.s.u.Q
#define iq    u.u.s.u.Iq
#define GCbank COMMON_BLOCK(GCBANK,gcbank)
COMMON_BLOCK_DEF(GCBANK_DEF,GCbank);
GCBANK_DEF GCbank;

main() {
FEQ();
printf("GCbank.nzebra       = %d.\n", GCbank.nzebra);
printf("GCbank.gversn       = %f.\n", GCbank.gversn);
printf("GCbank.zversn       = %f.\n", GCbank.zversn);
printf("GCbank.ixstor       = %d.\n", GCbank.ixstor);
printf("GCbank.ixcons       = %d.\n", GCbank.ixcons);
printf("GCbank.fendq[15]    = %f.\n", GCbank.fendq[15]);
printf("GCbank.lmain        = %d.\n", GCbank.lmain);
printf("GCbank.lr1          = %d.\n", GCbank.lr1);
printf("GCbank.ws[KWBANK-1] = %f.\n", GCbank.ws[KWBANK-1]);
printf("GCbank.iq[0]        = %d.\n", GCbank.iq[0]);
return EXIT_SUCCESS;
}
#endif

/* The following functions, exist through cor, are called by FORTRAN functions,
   as shown by the remaining examples. */

#ifdef CF_SAME_NAMESPACE
/* 
   VAX/VMS
   HP-UX (without the f77 +ppu      option. Ignore the undesirable -U option.)
   IBMR2 (without the xlf -qextname option.)
   AbsoftUNIXFortran default.
have C and FORTRAN sharing the same name space. The name space is
case-insensitive for VAX/VMS. There are several ways, some are described in
cfortran.doc, to meet this constraint, which is only a difficulty for C
routines to be called by FORTRAN.

The conflict is explicitly avoided, as shown, for the routines: ca, cb, cc, cd.

For VAX/VMS we need to change the name, (changing the case is not enough since
VAX/VMS is case insensitive. This is done implicitly via the defines given 
below: 

For the IBM, HP and AbsoftUNIXFortran,
we have chosen to name the C routines using a Proper Case notation, i.e:
                   Exist, Ce, Ccff, Ccg, Cch, Ci, Cj, Ck, Cl, Cm, Cn, Cvv, Cv7,
                   Cand, Cor, Cadd, Cfun, Pstru, Pstr, Cf14.
instead of the usual C convention:
                   exist, ce, ccff, ccg, cch, ci, cj, ck, cl, cm, cn, cvv, cv7,
                   cand, cor, cadd, cfun, pstru, pstr, cf14.

IF 'Exist', ETC. ARE CHANGED TO LOWER CASE,
THIS DEMO WILL STILL RUN ON ALL MACHINES,
EXCEPT THE HP9000      (when not using f77 +ppu) 
AND    THE IBM RS/6000 (when not using f77 -qextname)
AND    THE AbsoftUNIXFortran.
i.e. Only these two machines, when their Fortran compilers aren't forced to
     append underscores, can require code to go against C naming norms.
*/

#ifdef vmsFortran
#define Exist EXIST_
/*#define ca    CA_*/    /* We don't do this since we've decided to call the
                            routine ca from FORTRAN  as CFORTRANCA.           */
/*#define cb    CB_*/    /* Similarly we call cb as CFCB.                     */
/*#define cc    CC_*/    /*               and cc as CFCC.                     */
/*#define cd    CD_*/    /*               and cd as   CDCFORT.                */
#define Ce    CE_
#define Ccff  CCFF_
#define Ccg   CCG_
#define Cch   CCH_
#define Ci    CI_
#define Cj    CJ_
#define Ck    CK_
#define Cl    CL_
#define Cm    CM_
#define Cn    CN_
#define Cvv   CVV_
#define Cv7   CV7_
#define Cand  CAND_
#define Cor   COR_
#define Cadd  CADD_
#define Cfun  CFUN_
#define Pstru PSTRU_
#define Pstr  PSTR_
#define Cf14  CF14_
#endif                      /* vmsFortran        */
#endif                      /* CF_SAME_NAMESPACE */

void Exist() {printf("exist: was called.\n");}
FCALLSCSUB0(Exist,EXIST,exist)

void ca(i) int i; {printf("ca: had integer argument:%d.\n",i);}
FCALLSCSUB1(ca,CFORTRANCA,cfortranca, INT)
/*           ^      ^-----------^---------FORTRAN name. 
             |__ C name.                                                      */


/* With the next 2 lines we tell cfortran.h that for the subsequent FCALLSCSUBn
   and FCALLSCSUBn declarations, FORTRAN entry points to C routines have the 
   C name prefaced with the characters 'CF', i.e. whereas the
   C name of the routine is 'cb', the routine is called from FORTRAN as 'CFCB'. 
   Similarly C's cc, is CFCC for FORTRAN. */
#undef  fcallsc
#define fcallsc(UN,LN) preface_fcallsc(CF,cf,UN,LN)

void cb(i) int *i; 
{printf("cb: had pointer argument to integer:%d.\n",*i); *i*=2;}
FCALLSCSUB1(cb,CB,cb, PINT)

void cc(s) char *s; {printf("cc: had string argument:%s.\n",s);}
FCALLSCSUB1(cc,CC,cc, STRING)

/* With the next 2 lines we tell cfortran.h that for the subsequent FCALLSCSUBn
   and FCALLSCSUBn declarations, FORTRAN entry points to C routines have the 
   C name appended with the characters 'CFORT', i.e. whereas the C name of the
   routine is 'cd', the routine is called from FORTRAN as 'CDCFORT'.          */
#undef  fcallsc
#define fcallsc(UN,LN) append_fcallsc(CFORT,cfort,UN,LN)

void cd(s) char *s;
{printf("cd: had string argument:%s.\n",s); strcpy(s,"to you 12345678");}
FCALLSCSUB1(cd,CD,cd, PSTRING)

#undef  fcallsc
#define fcallsc(UN,LN) orig_fcallsc(UN,LN)
/* The preceeding line returns FORTRAN names to being the original C names.   */

void Ce(v) char v[][5];
{printf("ce: had string vector argument:%s,%s,%s.\n",v[0],v[1],v[2]);}
#define ce_STRV_A1 TERM_CHARS(' ',1)
FCALLSCSUB1(Ce,CE,ce, STRINGV)

void Ccff(v, n) char v[][5]; int n;
{int i;
printf("ccff: had %d string vector argument:",n);
for (i=0; i<n-1; i++) printf("%s,",v[i]);
printf("%s.\n",v[i]);
}
#define ccff_STRV_A1 NUM_ELEM_ARG(2)
FCALLSCSUB2(Ccff,CCFF,ccff, STRINGV, INT)


int Ccg() {return 111;}
FCALLSCFUN0(INT,Ccg,CCG,ccg)

char *Cch() {return "hello";}
FCALLSCFUN0(STRING,Cch,CCH,cch)

char *Ci(v) char v[][5]; {return v[3];}
#define ci_STRV_A1 NUM_ELEMS(6)
FCALLSCFUN1(STRING,Ci,CI,ci, STRINGV)

char *Cj(v) int v; {printf("cj:v=%d\n",v);return "hello";}
FCALLSCFUN1(STRING,Cj,CJ,cj, INT)

FORTRAN_REAL Ck() {return 111.;}
FCALLSCFUN0(FLOAT,Ck,CK,ck)

DOUBLE_PRECISION Cl() {return 111.;}
FCALLSCFUN0(DOUBLE,Cl,CL,cl)

FORTRAN_REAL Cm(a) FORTRAN_REAL a; {return a;}
FCALLSCFUN1(FLOAT,Cm,CM,cm, FLOAT)

DOUBLE_PRECISION Cn(a,b) DOUBLE_PRECISION a; DOUBLE_PRECISION b; {return a+b;}
FCALLSCFUN2(DOUBLE,Cn,CN,cn, DOUBLE, DOUBLE)

void Cvv(d,f,i) DOUBLE_PRECISION d[2][2]; FORTRAN_REAL f[2][2]; int i[2][2];
{
int j,k; double t[2][2];
for (j=0; j<2; j++) for (k=0; k<2; k++) {
  t[j][k] = d[j][k];
  d[j][k] = f[j][k];
  f[j][k] = i[j][k];
  i[j][k] = t[j][k];
}
return;
}
FCALLSCSUB3(Cvv,CVV,cvv, DOUBLEVV, FLOATVV, INTVV)

DOUBLE_PRECISION Cv7(d) DOUBLE_PRECISION d[2][3][5][7][11][13][1];
{
DOUBLE_PRECISION t=0;
int i,j,k,l,m,n,o;
for (            i=0; i< 2; i++) 
  for (          j=0; j< 3; j++)
    for (        k=0; k< 5; k++) 
      for (      l=0; l< 7; l++) 
        for (    m=0; m<11; m++) 
          for (  n=0; n<13; n++) 
            for (o=0; o< 1; o++) t += d[i][j][k][l][m][n][o];
return t;
}
FCALLSCFUN1(DOUBLE,Cv7,CV7,cv7, DOUBLEVVVVVVV)

int Cand(a,b) int a; int b; {return a && b;}
FCALLSCFUN2(LOGICAL,Cand,CAND,cand, LOGICAL, LOGICAL)

int Cor(a,b) int *a; int *b; {int t; t= *a;*a= *b;*b=t; return *a || *b;}
FCALLSCFUN2(LOGICAL,Cor,COR,cor, PLOGICAL, PLOGICAL)

void Pstru(s) char *s; { strcpy(s,"new pstring"); return;}
FCALLSCSUB1(Pstru,PSTRU,pstru, PSTRING)

void Pstr(s) char *s;
{
static char *save=NULL;
char *temp;
int ls, lsave;
/* If required, reset, or prepare to reset the saved location. */
if (!s || !save) { save=s; return; }
ls    = strlen(s   );
lsave = strlen(save);
temp = malloc(ls>lsave?ls:lsave);
/* Switch contents of argument with contents of saved string. */
strcpy(temp,save);
strcpy(save,s   );
strcpy(s   ,temp);
free(temp);
return;
}
/* Provide 3 interfaces using the the 3 types of PSTRING. */
FCALLSCSUB1(Pstr,PSTR,pstr,   PSTRING)
FCALLSCSUB1(Pstr,PNSTR,pnstr, PNSTRING)
FCALLSCSUB1(Pstr,PPSTR,ppstr, PPSTRING)

void Cf14(a,b,c,d,e,f,g,h,i,j,k,l,m,n)
  int *a,*b,*c,*d,*e,*f,*g,*h,*i,*j,*k,*l,*m,*n;
  {  *a =  1; *b =  2; *c =  3; *d =  4; *e =  5; *f =  6; *g =  7;
     *h =  8; *i =  9; *j = 10; *k = 11; *l = 12; *m = 13; *n = 14;
     return;}
FCALLSCSUB14(Cf14,CF14,cf14, PINT,PINT,PINT,PINT,PINT,PINT,PINT,PINT,PINT,PINT,PINT,PINT,PINT,PINT)


#ifdef F0_SELECT
                          PROTOCCALLSFSUB0(FEXIST,fexist)
#define FEXIST()               CCALLSFSUB0(FEXIST,fexist)

main() {FEXIST(); return EXIT_SUCCESS;}
#endif

#ifdef FA_SELECT
                        PROTOCCALLSFSUB1(FA,fa, INT)
#define FA(A1)               CCALLSFSUB1(FA,fa, INT, A1)

main() {FA(1234); return EXIT_SUCCESS;}
#endif

#ifdef FB_SELECT
                        PROTOCCALLSFSUB1(FB,fb, PINT)
#define FB(A1)               CCALLSFSUB1(FB,fb, PINT, A1)

main() 
{int i,ii; i=ii=1234; 
 FB(ii); printf("MAIN: FB(i=%d) returns with i=%d.\n",i,ii);
 return EXIT_SUCCESS;
}
#endif

#ifdef FC_SELECT
                        PROTOCCALLSFSUB1(FC,fc, STRING)
#define FC(A1)               CCALLSFSUB1(FC,fc, STRING, A1)

main() {FC("hello"); return EXIT_SUCCESS;}
#endif

#ifdef FD_SELECT
                        PROTOCCALLSFSUB1(FD,fd, PSTRING)
#define FD(A1)               CCALLSFSUB1(FD,fd, PSTRING, A1)

main() 
{static char i[] = "happy     "; static char ii[] = "happy      "; 
 FD(ii); printf("MAIN: FD(i=%s) returns with i=%s.\n",i,ii);
 return EXIT_SUCCESS;
}
#endif

#ifdef FE_SELECT
                        PROTOCCALLSFSUB1(FE,fe, STRINGV)
#define FE(A1)               CCALLSFSUB1(FE,fe, STRINGV, A1)

main() 
{static char v[][5] = {"0000", "1", "22", ""}; FE(v); return EXIT_SUCCESS;}
#endif

#ifdef FF_SELECT
                        PROTOCCALLSFSUB2(FF,ff, STRINGV, INT)
#define FF(A1,A2)            CCALLSFSUB2(FF,ff, STRINGV, INT, A1, A2)

main() 
{static char v[][5] = {"0000", "1", "22", ""}; 
 FF(v,sizeof(v)/sizeof v[0]);
 return EXIT_SUCCESS;
}
#endif

#ifdef FG_SELECT
PROTOCCALLSFFUN0(INT,FG,fg)
#define FG()               CCALLSFFUN0(FG,fg)

main() 
{printf("FG() returns %d.\n",FG()); return EXIT_SUCCESS;}
#endif

#ifdef FH_SELECT
PROTOCCALLSFFUN0(STRING,FH,fh)
#define FH()               CCALLSFFUN0(FH,fh)

main() 
{printf("FH() returns %s.\n",FH()); return EXIT_SUCCESS;}
#endif

#ifdef FI_SELECT
PROTOCCALLSFFUN1(STRING,FI,fi,STRINGV)
#define FI(A1)               CCALLSFFUN1(FI,fi, STRINGV, A1)

main() 
{static char v[][5] = {"0000", "1", "22", "333", "8", "9"}; 
 printf("FI(v) returns %s.\n",FI(v));
 return EXIT_SUCCESS;
}
#endif

#ifdef FJ_SELECT
PROTOCCALLSFFUN1(STRING,FJ,fj,INT)
#define FJ(A1)               CCALLSFFUN1(FJ,fj, INT, A1)

main() 
{ printf("FJ(2) returns %s.\n",FJ(2)); return EXIT_SUCCESS;}
#endif

#ifdef FK_SELECT
PROTOCCALLSFFUN0(FLOAT,FK,fk)
#define FK()               CCALLSFFUN0(FK,fk)

main() 
{printf("FK() returns %f.\n",FK()); return EXIT_SUCCESS;}
#endif

#ifdef FL_SELECT
PROTOCCALLSFFUN0(DOUBLE,FL,fl)
#define FL()               CCALLSFFUN0(FL,fl)

main() 
{printf("FL() returns %f.\n",(double)FL()); return EXIT_SUCCESS;}
#endif                       /* ^- cast req.d for CRAY. */

#ifdef FM_SELECT
PROTOCCALLSFFUN1(FLOAT,FM,fm,FLOAT) 
#define FM(A)               CCALLSFFUN1(FM,fm, FLOAT, A)

main() 
{printf("FM(111.) returns %f.\n",FM(111.)); return EXIT_SUCCESS;}
#endif

#ifdef FN_SELECT
PROTOCCALLSFFUN2(DOUBLE,FN,fn,DOUBLE,DOUBLE)
#define FN(A,B)             CCALLSFFUN2(FN,fn, DOUBLE, DOUBLE, A, B)

main() 
{printf("FN(1./3, 2./3) returns %f.\n",(double)FN(1./3, 2./3));
 return EXIT_SUCCESS;
}
#endif                                /* ^- cast req.d for CRAY. */

#ifdef VV_SELECT
             PROTOCCALLSFSUB3(VV,vv, DOUBLEVV, FLOATVV, INTVV)
#define VV(D,F,I) CCALLSFSUB3(VV,vv, DOUBLEVV, FLOATVV, INTVV, D, F, I)

main()
{
DOUBLE_PRECISION d[2][2]; 
FORTRAN_REAL     f[2][2];
int              i[2][2];
int j,k;
for (j=0; j<2; j++) for (k=0; k<2; k++) {
  d[j][k] = 100+10*j+k;
  f[j][k] = 200+10*j+k;
  i[j][k] = 300+10*j+k;
}
VV(d,f,i);
                               /*  \/- cast req.d for CRAY. */
printf("%4.0f%4.0f%4.0f%4.0f\n",(double)d[0][0],(double)d[0][1],
                                (double)d[1][0],(double)d[1][1]);
printf("%4.0f%4.0f%4.0f%4.0f\n",f[0][0],f[0][1],f[1][0],f[1][1]);
printf("%4d%4d%4d%4d\n"        ,i[0][0],i[0][1],i[1][0],i[1][1]);
return EXIT_SUCCESS;
}
#endif

#ifdef V7_SELECT
PROTOCCALLSFFUN1(DOUBLE,V7,v7,DOUBLEVVVVVVV)
#define V7(D)               CCALLSFFUN1(V7,v7, DOUBLEVVVVVVV, D)

main()
{
/* Original d[2][3][5][7][11][13][17] died a SEGV on DECstation MIPS cc 2.10, 
   just like e.g.             main() {double d[2][3][5][7][11][13][17], t=0;} */

DOUBLE_PRECISION d[2][3][5][7][11][13][1], t=0, r=1, tf;
int i,j,k,l,m,n,o;
for (            i=0; i< 2; i++) 
  for (          j=0; j< 3; j++)
    for (        k=0; k< 5; k++) 
      for (      l=0; l< 7; l++) 
        for (    m=0; m<11; m++) 
          for (  n=0; n<13; n++) 
            for (o=0; o< 1; o++) {
              r /= 2;
              t += r;
              d[i][j][k][l][m][n][o] = r;
            }
tf=V7(d);                         
printf("main() filled array d with a total: %10.9f\n", (double)t );
printf("V7()   returned the value:          %10.9f\n", (double)tf);
return EXIT_SUCCESS;
}                                 /* cast req.d for CRAY -^ */
#endif

#ifdef FAND_SELECT
PROTOCCALLSFFUN2(LOGICAL,FAND,fand,LOGICAL,LOGICAL)
#define FAND(A,B)             CCALLSFFUN2(FAND,fand, LOGICAL, LOGICAL, A, B)

main() 
{printf("FAND(0, 1) returns %d.\n",FAND(0, 1)); return EXIT_SUCCESS;}
#endif

#ifdef FORR_SELECT
PROTOCCALLSFFUN2(LOGICAL,FORR,forr,PLOGICAL,PLOGICAL)
#define FORR(A,B)             CCALLSFFUN2(FORR,forr, PLOGICAL, PLOGICAL, A, B)

main() 
{int a=2, b=0; printf("Calling FORR(a=%d, b=%d).\n", a,b);
               printf("FORR() returned %d.\n", FORR(a, b));
               printf("With a=%d, b=%d.\n", a,b);
 return EXIT_SUCCESS;
}
#endif


#include <string.h>
FCALLSCFUN2(STRING,strtok,CSTRTOK,cstrtok, STRING, STRING)

#ifdef STRTOK_SELECT
                  PROTOCCALLSFSUB0(FSTRTOK,fstrtok)
#define FSTRTOK()      CCALLSFSUB0(FSTRTOK,fstrtok)

main() {FSTRTOK(); return EXIT_SUCCESS;}
#endif

#ifdef USER_SELECT
/* We define a new type USERINT. [Same functionality as PINT actually.] */

#ifdef OLD_VAXC        /* To avoid %CC-I-PARAMNOTUSED. */
#pragma nostandard
#endif

#define USERINT_cfV(  T,A,B,F)       SIMPLE_cfV(T,A,B,F)
#define USERINT_cfSEP(T,  B)         SIMPLE_cfSEP(T,B)
#define USERINT_cfINT(N,A,B,X,Y,Z)   SIMPLE_cfINT(N,A,B,X,Y,Z)
#define USERINT_cfSTR(N,T,A,B,C,D,E) SIMPLE_cfSTR(N,T,A,B,C,D,E)
#define USERINT_cfCC( T,A,B)         SIMPLE_cfCC(T,A,B)
#define USERINT_cfAA( T,A,B)         USERINT_cfB(T,A)
#define USERINT_cfU(  T,A)           USERINT_cfN(T,A)

#define USERINT_cfN(  T,A)  int *A
#define USERINT_cfB(  T,A)     &(A)

#ifdef OLD_VAXC        /* Have avoided %CC-I-PARAMNOTUSED. */
#pragma standard
#endif

                  PROTOCCALLSFSUB2(EASY,easy, USERINT, INT)
#define EASY(A,B)      CCALLSFSUB2(EASY,easy, USERINT, INT, A, B)

main() {
int a;
printf("\nUsing user defined USERINT argument type.\n");
EASY(a,7);
printf("The FORTRAN routine EASY(a,7) returns a = %d\n", a);
return EXIT_SUCCESS;
}
#endif

#ifdef FUN_SELECT /* Passing C or Fortran Functions to Fortran routines. */
PROTOCCALLSFFUN3(INT,FUNADD,funadd,ROUTINE,INT,INT)
#define FUNADD(F,A,B) CCALLSFFUN3(FUNADD,funadd, ROUTINE, INT, INT, F, A, B)

int Cadd(a,b) int a; int b; {return a+b;}
FCALLSCFUN2(INT,Cadd,CADD,cadd, INT, INT)

/* Want fadd to be prototyped, though don't need the wrapper that is created. */
PROTOCCALLSFFUN2(INT,FADD,fadd,INT,INT)

main() {

printf("\nFUNADD(CADD,1,2) returns %d\n", 
       FUNADD(      C_FUNCTION(CADD,cadd),1,2) );
printf("\nFUNADD(FADD,3,4) returns %d\n", 
       FUNADD(FORTRAN_FUNCTION(FADD,fadd),3,4) );
return EXIT_SUCCESS;
}
#endif

#ifdef SUB_SELECT /* Fortran passes routines to C. */
   PROTOCCALLSFSUB4(FUNARG,funarg, ROUTINE, INT, INT, PINT)
#define FUNARG(F,A,B,C) \
        CCALLSFSUB4(FUNARG,funarg, ROUTINE, INT, INT, PINT, F, A, B, C)

int Cfun(f,a,b) int (*f)(); int a; int b; {int c; f(&a,&b,&c); return c;}
#undef  ROUTINE_1
#define ROUTINE_1  (int (*)())
FCALLSCFUN3(INT,Cfun,CFUN,cfun, ROUTINE, INT, INT)

main() {
int c;
FUNARG(C_FUNCTION(CFUN,cfun),1,2,c);
printf("\nFUNARG(CFUN,1,2,c) returns with c=%d\n",c);
return EXIT_SUCCESS;
}
#endif

#undef  ROUTINE_4
#ifdef VISUAL_CPLUSPLUS
#define ROUTINE_4 (int (*)(const void *,const void *))
#else
#define ROUTINE_4 (int (*)())
#endif
FCALLSCSUB4(qsort,FQSORT,fqsort, PVOID, INT, INT, ROUTINE)
/* Note that we've assumed in the above that size_t == int */

#ifdef Q_SELECT
                        PROTOCCALLSFSUB1(FQSORTEX,fqsortex, INT)
#define FQSORTEX(SIZEOF_INT) CCALLSFSUB1(FQSORTEX,fqsortex, INT, SIZEOF_INT)

main() {
#ifdef PowerStationFortran
printf("\n\
        Apologies. As described in cfortran.doc, MSPS Fortran provides no\n\
        easy way to pass a Fortran routine as an argument to a C routine,\n\
        so this qsort() example crashes for MSPS Fortran.\n\
        \n\
        As a kludge, the example works on MSPS Fortran by either\n\
        - using MSPS Fortran language extensions\n\
        or\n\
        - by removing the 'integer function cmp(a,b)' routine from cfortex.f\n\
          and instead using the following C routine.\n\
            int CMP( int *a, int *b) { return *a-*b ; }\n\
        \n\
        It remains a mystery why the SUB_SELECT example works\n\
        for MSPS Fortran, since it should crash due to the same problem.\n\
        Presumably the faulty stack clearing is not fatal for SUB_SELECT.\n\
        \n");
#else
FQSORTEX(sizeof(int));
#endif
return EXIT_SUCCESS;
}
#endif

#ifdef E2_SELECT
/* Only to demo. that we can force a wrapper to be used for subroutines. */
PROTOCCALLSFFUN2(VOID,EASY,easy,PINT,INT)
#define EASY(A,B)      CCALLSFFUN2(EASY,easy, PINT, INT, A, B)

main() {
int a;
printf("\nEASY (2) EXAMPLE\n");
EASY(a,7);
printf("The FORTRAN routine EASY(a,7) returns a = %d\n", a);
return EXIT_SUCCESS;
}
#endif

#ifdef FSTR_SELECT
                          PROTOCCALLSFSUB0(FSTR,fstr)
#define FSTR()                 CCALLSFSUB0(FSTR,fstr)

main() { FSTR(); return EXIT_SUCCESS;}
#endif

#ifdef CF14_SELECT
   PROTOCCALLSFSUB14(F14,f14, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT)
#define F14(A,B,C,D,E,F,G,H,I,J,K,L,M,N) \
        CCALLSFSUB14(F14,f14, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, A, B, C, D, E, F, G, H, I, J, K, L, M, N)

main()  {
int a=0, b=0, c=0, d=0, e=0, f=0, g=0, h=0, i=0, j=0, k=0, l=0, m=0, n=0;
F14(   a,b,c,d,e,f,g,h,i,j,k,l,m,n);
printf("CF14: %3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d.\n",
       a,b,c,d,e,f,g,h,i,j,k,l,m,n);
return EXIT_SUCCESS;
}
#endif

#ifdef  F20_SELECT
#if MAX_PREPRO_ARGS>31 && !defined(CFSUBASFUN)
   PROTOCCALLSFSUB20(F20,f20, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT)
#define F20(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) \
        CCALLSFSUB20(F20,f20, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, PINT, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)

main()  {
int a=0, b=0, c=0, d=0, e=0, f=0, g=0, h=0, i=0, j=0, k=0, l=0, m=0, n=0,
    o=0, p=0, q=0, r=0, s=0, t=0;
F20(   a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t);
printf(" F20: %3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d.\n",
       a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t);
return EXIT_SUCCESS;
}
#else
main()  {
printf("Sorry 14 argument max. via cfortran.h on this C preprocessor.\n");
return EXIT_SUCCESS;
}
#endif
#endif


#ifdef SZ1_SELECT
#define sz1_ELEMS_3   ZTRINGV_ARGS(4)
#define sz1_ELEMLEN_3 ZTRINGV_NUM(8)
                  PROTOCCALLSFSUB4(SZ1,sz1, STRINGV,INT,ZTRINGV,INT)
#define SZ1(S,IS,Z,IZ) CCALLSFSUB4(SZ1,sz1, STRINGV,INT,ZTRINGV,INT, S,IS,Z,IZ)

int main() {
char *p;
static char s[][7]={"000 ", " "}     , os[][3]={"s"}, as[ ]="one element";
static char z[][9]={" ", "bb","ccc "}, oz[][9]={"z"}, az[6]="1234";

/*
  - z[][9] must match ZTRINGV_NUM(8), while az[6] does not have to
    since a single element argument may have the wrong length.
  - For arrays of strings, can pass a pointer for ZTRINGV, but not for STRINGV.
    i.e. Can't determine sizes for STRINGV, that's why we have ZTRINGV.
  - NEITHER STRINGV nor ZTRINGV can accept an array of pointers, e.g.
    NO: { char *p[3]; p[0]=z[0]; p[1]=z[1]; p[2]=z[2]; SZ1(s, 2, p, 3); }
 */

p = (char *)z;
SZ1(s   , 2, p     , 3);
SZ1(s[1], 1, z[1]  , 1);
SZ1(os  , 1, oz    , 1);
SZ1(as  , 1, az    , 1);
SZ1("hi", 1, "hoho", 1);

return EXIT_SUCCESS;
}
#endif


#ifdef PZ_SELECT
#define pz_ELEMS_3   ZTRINGV_ARGS(4)
#define pz_ELEMLEN_3 ZTRINGV_NUM(8)
                 PROTOCCALLSFSUB4(PZ,pz, PSTRINGV,INT, PZTRINGV,INT)
#define PZ(S,IS,Z,IZ) CCALLSFSUB4(PZ,pz, PSTRINGV,INT, PZTRINGV,INT, S,IS,Z,IZ)

int main() {
char *p;
static char s[][7]={"000 ", " "}     , as[]  ="hihi";
static char z[][9]={" ", "bb","ccc "}, az[99]="hoho";

/*
 - z[][9] must match ZTRINGV_NUM(8), while az[99] can match or be bigger,
   since 8 character will be copied back.
 - Comments in SZ1 example above for Z|STRINGV, also apply for PZ|STRINGV.
 */

p = (char *)z;
PZ(s,2,p,3);
PZ(s[1],1,z[2],1);

PZ(as,1,az,1);
PZ(as,1,az,1);

return EXIT_SUCCESS;
}
#endif

