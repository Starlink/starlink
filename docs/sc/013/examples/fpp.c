/*
 * Explore IEEE single-precision floats.  Invoked with an argument, it reprints
 * that number as a float, hex and binary.  Without argument, it reads numbers
 * from stdin.  The argument is either a float or an integer (hex, with
 * leading '0x', eg 0x3f800000) as argument.
 *
 * $Id$
 */

#include <stdio.h>
#include <stdlib.h>

/* Intel and Alpha chips are little-endian. */
/* Sparc, Motorola 68k and PowerPC chips are big-endian. */
/* Compile with -DBIGENDIAN=1 on the latter */
#ifndef BIGENDIAN
#define BIGENDIAN 0
#endif

/*
 * pbits: given an integer i (assumed 32 bits),
 * return the rightmost n (<=32) bits of the integer expressed in binary.
 */
char *pbits (unsigned int i, unsigned int n)
{
    static char s[33];
    char *p;
    s[n] = '\0';                        /* terminate the string */
    for (p=s+n-1; p>=s; p--, i>>=1)
        *p = ((i&1) ? '1' : '0');
    return s;
}

/* convert string to number and display in various formats */
void reprint_number (char *s)
{
    union {
        float f;
        unsigned int i;
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

    if (s[0]=='0' && s[1]=='x')         /* it's an integer */
        /* sscanf with "%u" doesn't get eg 0xffffffff or 0x80000000 right */
        ieeefloat.i = strtoul (s, (char**)NULL, 0);
    else
        ieeefloat.f = strtod (s, (char**)NULL);
    
    printf (" float  %12.7e\n   hex  %08x\n", ieeefloat.f, ieeefloat.i);
    printf ("binary  %s ", pbits (ieeefloat.ieee.s, 1));
    printf ("%s ", pbits (ieeefloat.ieee.e, 8));
    printf ("%s\n", pbits (ieeefloat.ieee.m, 23));
    printf ("  type  ");
    
    /* now print out what type of number it is */
    if (ieeefloat.ieee.e == 0)
        if (ieeefloat.ieee.m == 0)
            printf ("%s zero\n",
                    (ieeefloat.ieee.s ? "Negative" : "Positive"));
        else
            printf ("Subnormal\n");
    else if (ieeefloat.ieee.e == 255)
        if (ieeefloat.ieee.m == 0)
            printf ("%s infinity\n",
                    (ieeefloat.ieee.s ? "Negative" : "Positive"));
        else
            printf ("%s NaN\n",
                    (ieeefloat.ieee.m & (1<<22) ? "Quiet" : "Signalling"));
    else
        printf ("Normal\n");

    return;
}

int main (int argc, char **argv)
{
    if (argc > 1)
        reprint_number (argv[1]);
    else
    {
        char line[40];
        while (fgets (line, sizeof(line), stdin) != NULL)
            reprint_number (line);
    }
    exit (0);
}
