/* Common Block Declarations */

struct klotz0_1_ {
    double buff[607];
    int ptr;
};

#define klotz0_1 (*(struct klotz0_1_ *) &klotz0_)
#define min(a,b) (a<b)?a:b

struct klotz1_1_ {
    double xbuff[1024];
    int first, xptr;
};

#define klotz1_1 (*(struct klotz1_1_ *) &klotz1_)

/* Initialized data */

struct {
    int fill_1[1214];
    int e_2;
    } klotz0_ = { {0}, 0 };

struct {
    double fill_1[1024];
    int e_2[2];
    double e_3;
    } klotz1_ = { {0}, 0, 0, 0. };


int zufall_(n, a)
int n;
double *a;
{
    int buffsz = 607;

    int left, aptr, bptr, aptr0, i, k, q;
    double t;
    int nn, vl, qq, k273, k607, kptr;


/* portable lagged Fibonacci series uniform random number */
/* generator with "lags" -273 und -607: */
/* W.P. Petersen, IPS, ETH Zuerich, 19 Mar. 92 */


    aptr = 0;
    nn = n;

L1:

    if (nn <= 0) {
       return 0;
    }

/* factor nn = q*607 + r */

    q = (nn - 1) / 607;
    left = buffsz - klotz0_1.ptr;

    if (q <= 1) {

/* only one or fewer full segments */

       if (nn < left) {
            kptr = klotz0_1.ptr;
           for (i = 0; i < nn; ++i) {
               a[i + aptr] = klotz0_1.buff[kptr + i];
           }
           klotz0_1.ptr += nn;
           return 0;
       } else {
            kptr = klotz0_1.ptr;
           for (i = 0; i < left; ++i) {
               a[i + aptr] = klotz0_1.buff[kptr + i];
           }
           klotz0_1.ptr = 0;
           aptr += left;
           nn -= left;
/*  buff -> buff case */
           vl = 273;
           k273 = 334;
           k607 = 0;
           for (k = 0; k < 3; ++k) {
               for (i = 0; i < vl; ++i) {
                  t = klotz0_1.buff[k273+i]+klotz0_1.buff[k607+i];
                  klotz0_1.buff[k607+i] = t - (double) ((int) t);
               }
               k607 += vl;
               k273 += vl;
               vl = 167;
               if (k == 0) {
                   k273 = 0;
               }
           }
           goto L1;
       }
    } else {

/* more than 1 full segment */

        kptr = klotz0_1.ptr;
       for (i = 0; i < left; ++i) {
           a[i + aptr] = klotz0_1.buff[kptr + i];
       }
       nn -= left;
       klotz0_1.ptr = 0;
       aptr += left;

/* buff -> a(aptr0) */

       vl = 273;
       k273 = 334;
       k607 = 0;
       for (k = 0; k < 3; ++k) {
           if (k == 0) {
               for (i = 0; i < vl; ++i) {
                   t = klotz0_1.buff[k273+i]+klotz0_1.buff[k607+i];
                   a[aptr + i] = t - (double) ((int) t);
               }
               k273 = aptr;
               k607 += vl;
               aptr += vl;
               vl = 167;
           } else {
               for (i = 0; i < vl; ++i) {
                   t = a[k273 + i] + klotz0_1.buff[k607 + i];
                   a[aptr + i] = t - (double) ((int) t);
               }
               k607 += vl;
               k273 += vl;
               aptr += vl;
           }
       }
       nn += -607;

/* a(aptr-607) -> a(aptr) for last of the q-1 segments */

       aptr0 = aptr - 607;
       vl = 607;

       for (qq = 0; qq < q-2; ++qq) {
           k273 = aptr0 + 334;
           for (i = 0; i < vl; ++i) {
               t = a[k273 + i] + a[aptr0 + i];
               a[aptr + i] = t - (double) ((int) t);
           }
           nn += -607;
           aptr += vl;
           aptr0 += vl;
       }

/* a(aptr0) -> buff, last segment before residual */

       vl = 273;
       k273 = aptr0 + 334;
       k607 = aptr0;
       bptr = 0;
       for (k = 0; k < 3; ++k) {
           if (k == 0) {
               for (i = 0; i < vl; ++i) {
                   t = a[k273 + i] + a[k607 + i];
                   klotz0_1.buff[bptr + i] = t - (double) ((int) t);
               }
               k273 = 0;
               k607 += vl;
               bptr += vl;
               vl = 167;
           } else {
               for (i = 0; i < vl; ++i) {
                   t = klotz0_1.buff[k273 + i] + a[k607 + i];
                   klotz0_1.buff[bptr + i] = t - (double) ((int) t);
               }
               k607 += vl;
               k273 += vl;
               bptr += vl;
           }
       }
       goto L1;
    }
} /* zufall_ */


int zufalli_(seed)
int seed;
{
    /* Initialized data */

    int kl = 9373;
    int ij = 1802;

    /* Local variables */
    int i, j, k, l, m;
    double s, t;
    int ii, jj;


/*  generates initial seed buffer by linear congruential */
/*  method. Taken from Marsaglia, FSU report FSU-SCRI-87-50 */
/*  variable seed should be 0 < seed <31328 */


    if (seed != 0) {
       ij = seed;
    }

    i = ij / 177 % 177 + 2;
    j = ij % 177 + 2;
    k = kl / 169 % 178 + 1;
    l = kl % 169;
    for (ii = 0; ii < 607; ++ii) {
       s = 0.;
       t = .5;
       for (jj = 1; jj <= 24; ++jj) {
           m = i * j % 179 * k % 179;
           i = j;
           j = k;
           k = m;
           l = (l * 53 + 1) % 169;
           if (l * m % 64 >= 32) {
               s += t;
           }
           t *= (double).5;
       }
       klotz0_1.buff[ii] = s;
    }
    return 0;
} /* zufalli_ */


int zufallsv_(svblk)
double *svblk;
{
    int i;


/*  saves common blocks klotz0, containing seeds and */
/*  pointer to position in seed block. IMPORTANT: svblk must be */
/*  dimensioned at least 608 in driver. The entire contents */
/*  of klotz0 (pointer in buff, and buff) must be saved. */


    /* Function Body */
    svblk[0] = (double) klotz0_1.ptr;
    for (i = 0; i < 607; ++i) {
       svblk[i + 1] = klotz0_1.buff[i];
    }

    return 0;
} /* zufallsv_ */

int zufallrs_(svblk)
double *svblk;
{
    int i;


/*  restores common block klotz0, containing seeds and pointer */
/*  to position in seed block. IMPORTANT: svblk must be */
/*  dimensioned at least 608 in driver. The entire contents */
/*  of klotz0 must be restored. */


    klotz0_1.ptr = (int) svblk[0];
    for (i = 0; i < 607; ++i) {
       klotz0_1.buff[i] = svblk[i + 1];
    }

    return 0;
} /* zufallrs_ */

int normalen_(n, x)
int n;
double *x;
{
    int buffsz = 1024;

    /* Local variables */
    int left, i, nn, ptr, kptr;
    extern int normal00_();
/* Box-Muller method for Gaussian random numbers */

    nn = n;
    if (nn <= 0) {
       return 0;
    }
    if (klotz1_1.first == 0) {
       normal00_();
       klotz1_1.first = 1;
    }
    ptr = 0;

L1:
    left = buffsz - klotz1_1.xptr;
    if (nn < left) {
        kptr = klotz1_1.xptr;
       for (i = 0; i < nn; ++i) {
           x[i + ptr] = klotz1_1.xbuff[kptr + i];
       }
       klotz1_1.xptr += nn;
       return 0;
    } else {
        kptr = klotz1_1.xptr;
       for (i = 0; i < left; ++i) {
           x[i + ptr] = klotz1_1.xbuff[kptr + i];
       }
       klotz1_1.xptr = 0;
       ptr += left;
       nn -= left;
       normal00_();
       goto L1;
    }
} /* normalen_ */

int normal00_()
{
    /* Builtin functions */
    double cos(), sin(), log(), sqrt();

    /* Local variables */
    int i;
    double twopi, r1, r2, t1, t2;
    extern int zufall_();

    twopi = 6.2831853071795862;
    zufall_(1024, klotz1_1.xbuff);
    for (i = 0; i < 1023; i += 2) {
       r1 = twopi * klotz1_1.xbuff[i];
       t1 = cos(r1);
       t2 = sin(r1);
       r2 = sqrt(-2.*(log(1. - klotz1_1.xbuff[i+1])));
       klotz1_1.xbuff[i]   = t1 * r2;
       klotz1_1.xbuff[i+1] = t2 * r2;
    }

    return 0;
} /* normal00_ */

int normalsv_(svbox)
double *svbox;
{
    extern int zufallsv_();
    int i, k;


/*  IMPORTANT: svbox must be dimensioned at */
/*  least 1634 in driver. The entire contents of blocks */
/*  klotz0 (via zufallsv) and klotz1 must be saved. */

    if (klotz1_1.first == 0) {
       printf("ERROR in normalsv, save of uninitialized block\n");
    }

/*  save zufall block klotz0 */

    zufallsv_(svbox);

    svbox[608] = (double) klotz1_1.first;
    svbox[609] = (double) klotz1_1.xptr;
    k = 610;
    for (i = 0; i < 1024; ++i) {
       svbox[i + k] = klotz1_1.xbuff[i];
    }

    return 0;
} /* normalsv_ */

int normalrs_(svbox)
double *svbox;
{
    /* Local variables */
    extern int zufallrs_();
    int i, k;

/*  IMPORTANT: svbox must be dimensioned at */
/*  least 1634 in driver. The entire contents */
/*  of klotz0 and klotz1 must be restored. */

/* restore zufall blocks klotz0 and klotz1 */

    zufallrs_(svbox);
    klotz1_1.first = (int) svbox[608];
    if (klotz1_1.first == 0) {
      printf("ERROR in normalrs, restoration of uninitialized block\n");
    }
    klotz1_1.xptr = (int) svbox[609];
    k = 610;
    for (i = 0; i < 1024; ++i) {
       klotz1_1.xbuff[i] = svbox[i + k];
    }

    return 0;
} /* normalrs_ */

int fische_(n, mu, p)
int n;
double mu;
int *p;
{

    /* Builtin functions */
    double exp();

    /* Local variables */
    int left, indx[1024], i, k;
    double q[1024], u[1024];
    int nsegs, p0;
    double q0;
    int ii, jj;
    extern /* Subroutine */ int zufall_();
    int nl0;
    double pmu;

/* Poisson generator for distribution function of p's: */

/*    q(mu,p) = exp(-mu) mu**p/p! */

/* initialize arrays, pointers */


    /* Function Body */
    if (n <= 0) {
       return 0;
    }

    pmu = exp(-mu);
    p0 = 0;

    nsegs = (n - 1) / 1024;
    left = n - (nsegs << 10);
    ++nsegs;
    nl0 = left;

    for (k = 0; k < nsegs; ++k) {

       for (i = 0; i < left; ++i) {
           indx[i] = i;
           p[p0 + i] = 0;
           q[i] = 1.;
       }

/* Begin iterative loop on segment of p's */

L1:

/* Get the needed uniforms */

       zufall_(left, u);

       jj = 0;

       for (i = 0; i < left; ++i) {
           ii = indx[i];
           q0 = q[ii] * u[i];
           q[ii] = q0;
           if (q0 > pmu) {
               indx[jj++] = ii;
               ++p[p0 + ii];
           }
       }

/* any left in this segment? */

       left = jj;
       if (left > 0) {
           goto L1;
       }

       p0  += nl0;
       nl0  = 1024;
       left = 1024;

    }

    return 0;
} /* fische_ */




float etime_( float *a ){

      return( (float) time( 0 ) );

}
