/*
 * These ones are necessary to override the behaviour of
 * PINT_cfB, which puts the & on before getting to the
 * TYPE specific PCINT_cfPP...
 * The only way to do this is to introduce PCDOUBLE_cfINT,
 * which means we use PCINT for alot of the generic macros.
 */

#define PCINT_cfAA                PINT_cfAA
#define PCINT_cfN                 PINT_cfN
#define PCINT_cfV                 PINT_cfV
#define PCINT_cfZ(T,I,A)          (__cfztringv[I]= (int ) *A),
#define PCINT_cfSEP               INT_cfSEP
#define PCINT_cfCC                PINT_cfCC
#define PCINT_cfB(T,A)            _(T,_cfPP) A
#define PCINT_cfU                 PINT_cfU

/* These are the real TYPE specific ones, and will need to be
 * duplicated for FLOAT,...
 */
#define PCINT_cfINT                  PCDOUBLE_cfINT
#define PCINT_cfAAP(A, B)            A 
#define PCINT_cfSTR(N,T,A,B,C,D,E)   _(CFARGS,N)(T,DEFAULT,A,B,C,D,E)
#define PCINT_cfTYPE int
#define PCINT_cfVP(A,B)               int  B = (int) *A;   /* For ZSTRINGV_ARGS */
#define PCINT_cfPP
#define PCINT_cfCCC(A,B)              A

#define PCFLOAT_cfINT                 PCDOUBLE_cfINT
#define PCFLOAT_cfAAP(A, B)           A 
#define PCFLOAT_cfSTR(N,T,A,B,C,D,E)  _(CFARGS,N)(T,DEFAULT,A,B,C,D,E)
#define PCFLOAT_cfTYPE                float
#define PCFLOAT_cfVP                  PCINT_cfVP   /* For ZSTRINGV_ARGS */
#define PCFLOAT_cfPP
#define PCFLOAT_cfCCC(A,B)            A

#define PCDOUBLE_cfINT(N,A,B,X,Y,Z)   _(CFARGS,N)(A,PCINT,B,X,Y,Z,0)
#define PCDOUBLE_cfAAP(A, B)          A 
#define PCDOUBLE_cfSTR(N,T,A,B,C,D,E) _(CFARGS,N)(T,DEFAULT,A,B,C,D,E)
#define PCDOUBLE_cfTYPE               double
#define PCDOUBLE_cfVP                 PCINT_cfVP   /* For ZSTRINGV_ARGS */
#define PCDOUBLE_cfPP
#define PCDOUBLE_cfCCC(A,B)           A

#define PCLOGICAL_cfINT               PCDOUBLE_cfINT
#define PCLOGICAL_cfA(M,I,A,B)        *A=C2FLOGICAL(*A);
#define PCLOGICAL_cfAAP(A,B)          B = A
#define PCLOGICAL_cfC(A,B,C)          *A=C2FLOGICAL(*A); 
#define PCLOGICAL_cfH(S,U,B)
#define PCLOGICAL_cfJ(B)
#define PCLOGICAL_cfW(A,B)            PLOGICAL_cfW(A,B)
#define PCLOGICAL_cfS(M,I,A)
#define PCLOGICAL_cfSTR(N,T,A,B,C,D,E)  _(CFARGS,N)(T,PCLOGICAL,A,B,C,D,E) 
#define PCLOGICAL_cfTYPE              int
#define PCLOGICAL_cfVP                PLOGICAL_cfVP   /* For ZSTRINGV_ARGS */
#define PCLOGICAL_cfPP
#define PCLOGICAL_cfKK                PLOGICAL_cfKK
#define PCLOGICAL_cfCCC(A,B)          B = A

/* 
 * I can't find where the following three defines are used... 
 * So they may well be wrong. 
 */

#define PCLOGICAL_cfQ(B)
#define PCLOGICAL_cfR(A,B,D)          *A=C2FLOGICAL(*A);
#define PCLOGICAL_cfT(M,I,A,B,D)      ((*A=F2CLOGICAL(*A)),A)

/* This is to get PZTRINGS to work for dynamically allocated
 * Contiguous arrays...  The problem was that the array is massaged
 * coming in with the call:         c2fstrv( A[0], A[0],... )
 * and coming out with:             f2cstrv( (char *) A, (char *) A,... )
 *
 * If you dynamically allocate an array with the trick:
 *
 *     char ** A;
 *     A = (char **) malloc ( nelements * sizeof(char *) );
 *     A[0] = (char *) malloc (nelements * elemSize * sizeof (char) );
 *     for ( i = 1; i < nelements; i++) A[i] = A[0] + i * elemSize;
 * 
 * Then the coming in call will kill you if you pass in A, and the 
 * coming out call will kill you if you pass in A[0]...
 * So, I change the coming in call to (char *)A, and you must then
 * pass in A[0].
 * 
 */


#undef PZTRINGV_cfA
#define PZTRINGV_cfA(M,I,A,B) APAZTRINGV_cfA(M,I,A,B,             \
                    (_3(M,_ELEMS_,I))*(( _3(M,_ELEMLEN_,I))+1),      \
                              (_3(M,_ELEMS_,I)),(_3(M,_ELEMLEN_,I))+1) 
#ifdef vmsFortran
#define  AAZTRINGV_cfA(M,I,A,B, sA,filA,silA)                                   \
 initfstr(B,malloc((sA)-(filA)),(filA),(silA)-1),                              \          c2fstrv((char *) A,B.dsc$a_pointer,(silA),(sA));
#define APAZTRINGV_cfA(M,I,A,B, sA,filA,silA)                                   \
 initfstr(B,(char *) A,(filA),(silA)-1),c2fstrv((char *) A,(char *)A,(silA),(sA));
#else
#define  AAZTRINGV_cfA(M,I,A,B, sA,filA,silA)                 \
 (B.s=malloc((sA)-(filA)),B.fs=c2fstrv((char *)A,B.s,(B.flen=(silA)-1)+1,(sA)));
#define APAZTRINGV_cfA(M,I,A,B, sA,filA,silA)                         \
 B.fs=c2fstrv((char *) A,(char *) A,(B.flen=(silA)-1)+1,B.sizeofA=(sA));
#endif


/*
 * This allows for character arrays longer than an unsigned short...
 */

#ifndef vmsFortran
#undef   STRING_cfV
#undef   PSTRINGV_cfV
#define  STRING_cfV(T,A,B,F) struct {unsigned int clen, flen;} B;
#define  PSTRINGV_cfV(T,A,B,F) struct {char *fs; unsigned int sizeofA, flen;} B;
#endif

/* 
 * This is to introduce a PZTRING ( NO V ) type 
 */


#ifdef vmsFortran
#define  PZTRING_cfV(T,A,B,F) static fstring B={0,DSC$K_DTYPE_T,DSC$K_CLASS_S,NULL};
#define  APATRING_cfA(M,I,A,B,silA) \
        (B.dsc$w_length=strlen(A),B.dsc$a_pointer=A,       \
        B.dsc$w_length >= silA?0:(memset((A)+B.dsc$w_length,' ',silA-B.dsc$w_length-1), \
                             A[B.dsc$w_length=silA-1]='\0'));
#define  PZTRING_cfC(A,B,C) \
        (B.dsc$w_length=strlen(A),B.dsc$a_pointer=A,       \
        B.dsc$w_length >= C?0:(memset((A)+B.dsc$w_length,' ',C-B.dsc$w_length-1), \
                             A[B.dsc$w_length=C-1]='\0'));
#else
#define  PZTRING_cfV(T,A,B,F)  int     B;
#define APATRING_cfA(M,I,A,B,silA)   \
        (B=strlen(A),B >= silA?0:(memset((A)+B,' ',silA-B-1)),A[B = silA - 1]='\0');
#define  PZTRING_cfC(A,B,C) \
           (B=strlen(A),B > C?0:(memset((A)+B,' ',(C - 1)-B-1)),A[B = C - 1]='\0');
#endif

#define  PZTRING_cfSTR(N,T,A,B,C,D,E) _(CFARGS,N)(T,PZTRING,A,B,C,D,E)
#define  PZTRING_cfINT       PVOID_cfINT
#define  PZTRING_cfA(M,I,A,B) APATRING_cfA(M,I,A,B,(_3(M,_ELEMLEN_,I))+1) 
#define  PZTRING_cfAA        PSTRING_cfCC
#define  PZTRING_cfB         PSTRING_cfB

#define  PZTRING_cfCC        PSTRING_cfCC
#define  PZTRING_cfJ         PSTRING_cfJ
#define  PZTRING_cfH         STRING_cfH
#define  PZTRING_cfN(T,A)   STRING_cfN(T,A)   /* CRAY insists on arg.'s here. */
#define  PZTRING_cfS(M,I,A)  ,( _3(M,_ELEMLEN_,I) + 1 )
#define  PZTRING_cfU(T,A)    char  *A
#define  PZTRING_cfW(A,B)    kill_trailing(A,' ');
#define  PZTRING_cfZ(T,I,A)
#define  PZTRING_cfSEP       INT_cfSEP
#define  PZTRING_cfKK        STRING_cfKK
