#include <stdio.h>

/* Intel and Alpha chips are little-endian. */
/* Sparc, Motorola 68k and PowerPC chips are big-endian. */
/* Compile with -DBIGENDIAN on the latter */
#ifndef BIGENDIAN
#define BIGENDIAN 0
#endif

/*
 * pbits: given an integer i, put the rightmost n bits of the integer 
 * expressed in binary, into the string s.
 */
void pbits (unsigned int i, unsigned int n, char *s)
{
    char *p;
    for (p=s+n-1; p>=s; p--, i>>=1)
        *p = ((i&1) ? '1' : '0');
    return;
}

/* Given a float, return a (static) string holding its bit pattern */
char *print_float (float f)
{
    static char b[35];
    union {
        float f;
        struct {
#if BIGENDIAN
            unsigned int s : 1;
            unsigned int e : 8;
            unsigned int m : 23;
#else
            unsigned int m : 23;
            unsigned int e : 8;
            unsigned int s : 1;
#endif
        } ieee;
    } ieeefloat;

    ieeefloat.f = f;

    pbits (ieeefloat.ieee.s, 1,  b);
    b[1] = ' ';
    pbits (ieeefloat.ieee.e, 8,  &b[2]);
    b[10] = ' ';
    pbits (ieeefloat.ieee.m, 23, &b[11]);
    b[34] = '\0';
    
    return b;
}


main ()
{
    float f, a, b, c;
    double fd, ad, bd, cd;

    printf ("\n  You lose accuracy adding small numbers to large ones...\n");
    /* 2^23+1 works OK... */
    f = 8388608.0;
    printf ("2^23 = %e = %s\n", f, print_float (f));
    f = f + 1.0;
    printf ("  +1 = %e = %s\n", f, print_float (f));
    /* ...but 2^24+1=2^24 - catastrophic loss of precision */
    f = 16777216.0;
    printf ("2^24 = %e = %s\n", f, print_float (f));
    f = f + 1.0;
    printf ("  +1 = %e = %s\n", f, print_float (f));

    printf ("\n\n  Display the dread effects of catastrophic cancellation\n\
  by calculating a*b-a*c, in several different ways.  These are equivalent\n\
  arithmetically, but not computationally.\n");
    /* Now show the effect of catastrophic cancellation by calculating
     * ab-ac, when a, b and c are such that the terms nearly cancel.
     * To generate numbers which best demonstrate the effect,
     * set a = (1+da/2^12), b=(1+db/2^12), etc.  Thus
     * a * b = (1 + db/2^12 + da/2^12 + da.db/2^24).
     * Put da=1.0.  Now pick db=1-, so that final term is just below 1/2^24, 
     * then dc=1+, so that final term is just above 1/2^24.
     * Thus a*b rounds down, and a*c rounds up.
     * Both are quite accurate, but large errors are revealed when
     * they're subtracted.  We can ameliorate this by rewriting the expression.
     */

    /* First, do the calculation in double, to get relative errors. */
    /* The cancellation is ignorable in double */
    ad = 1.0 + ((double)1/4096);
    bd = 1.0 + ((double)0.9/4096);
    cd = 1.0 + ((double)1.1/4096);
    fd = ad*bd-ad*cd;

    a = 1.0 + ((float)1/4096);  /* a=1.000244; */
    b = 1.0 + ((float)0.9/4096); /* b=1.000220; */
    c = 1.0 + ((float)1.1/4096); /* c=1.000269; */

    /* first method - naive */
    f = a*b-a*c;
    printf ("a=%e  b=%e  c=%e\n", a, b, c);
    printf ("a*b-a*c       = %e  (error=%e)\n", f, ((double)f/fd-1.0));

    /* pre-subtract the nearly-equal b and c */
    f = a*(b-c);
    printf ("a*(b-c)       = %e  (%e)\n", f, ((double)f/fd-1.0));

    /* rewrite the expression, to calculate a * ((b-1) - (c-1)).
       Thus b-1 and c-1 have full accuracy */
    b = ((float)0.9/4096);      /* = (above b) -1 */
    c = ((float)1.1/4096);
    f = a*(b-c);

    printf ("a((b-1)-(c-1))= %e  (%e)\n", f, ((double)f/fd-1.0));
    /* Can't do the same trick with a.  If we calculate (a-1)*() + 1*(),
       we don't get any improvement.  We're not carelessly discarding
       accuracy, now - we can't keep any more than this. */

    printf ("\n  ...and further illustrate what's happening by showing\n\
  b and b-1.  Note the extra accuracy in the latter.\n");
    /* Display b and (b-1).  Note extra accuracy in latter. */
    b = 1.0 + ((float)0.9/4096);/* = (above b) */
    printf ("1+1/10000 = %14.7e = %s\n", b, print_float (b));
    b = ((float)0.9/4096);      /* = (above b) -1 */
    printf ("  1/10000 = %14.7e = %s\n", b, print_float (b));;

    printf ("\n\n  Display Infinity and NaN\n");
    /* Display NaNs and Infinity */
    /* Don't just write a=1.0/0.0, since compiler can warn about this */
    a = 0.0;                    /* and log(0) */
    a = 1/a;
    b = 1/a;
    c = a*b;
    printf ("a = 1/0.0 = %14e = %s\n", a, print_float (a));
    printf ("b = 1/a   = %14e = %s\n", b, print_float (b));
    printf ("c = a*b   = %14e = %s\n", c, print_float (c));
}
    
