#if HAVE_CONFIG_H
#  include <config.h>
#endif

/*
 * This test checks that the values which HDS determines for the
 * platform-specific bad, maximum, minimum (etc) numbers do match
 * those determined in component PRM.  These must match, but neither
 * can practically be derived from the other.
 *
 * The code below draws heavily on dat1_show_ndr, and on hdstools.c:hds_show.
 */

#include <stdio.h>

#include "hds1.h"
#include "rec.h"
#include "dat1.h"

#include <prm_par.h>

#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* Helper function */
typedef unsigned char byte;
const char* tohex(const char*, byte* p);

int main(int argc, char** argv)
{
    int nerrs = 0;
    int testi;
    float testf;
    double testd;

    dat1_init();
    if (!_ok(hds_gl_status)) {
        fprintf(stderr, "Can't initialise HDS\n");
        exit (1);
    }

#define stringize(x) # x
#define testval_prm(ifd, prefix, NUM, num, type)                             \
    if (prefix ## __ ## NUM ## type != dat_gl_ndr[DAT__ ## type].num.type) { \
        test ## ifd = prefix ## __ ## NUM ## type;                           \
        printf(stringize(prefix ## __ ## NUM ## type) ": %s",                \
               tohex(stringize(ifd), (byte*)&test ## ifd));                  \
        printf(" != %s\n",                                                   \
               tohex(stringize(ifd),                                         \
                     (byte*)&dat_gl_ndr[DAT__ ## type].num.type));           \
        nerrs++;                                                             \
    }
#define testval(ifd, prefix, NUM, num, type)                            \
    if (prefix ## __ ## NUM ## type != dat_gl_ndr[DAT__ ## type].num) { \
        test ## ifd = prefix ## __ ## NUM ## type;                      \
        printf(stringize(prefix ## __ ## NUM ## type) ": %s",           \
               tohex(stringize(ifd), (byte*)&test ## ifd));             \
        printf(" != %s\n",                                              \
               tohex(stringize(ifd),                                    \
                     (byte*)&dat_gl_ndr[DAT__ ## type].num));           \
        nerrs++;                                                        \
    }

    /* 
     * Compare values in prm_par.h against the values in the struct
     * NDR, as initialised by dat1_init_ndr.c.  Excerpts:
     *
     * union PRM
     * {
     *    _BYTE B;
     *    _DOUBLE D;
     *    _INTEGER I;
     *    _LOGICAL L;
     *    _REAL R;
     *    _UBYTE UB;
     *    _UWORD UW;
     *    _WORD W;
     *    _CHAR C;
     * };
     * struct NDR
     * {
     *    union PRM bad;          -- "Bad" data value
     *    union PRM max;          -- Maximum value
     *    union PRM min;          -- Minimum (most negative) value
     *    const char *name;       -- Pointer to data type name
     *    unsigned short int length; -- Size of data element
     *    unsigned char format;   -- Data format code
     *    unsigned char order;    -- Storage order code
     *    unsigned char digits;   -- No. decimal digits of precision
     *    unsigned char txtsize;  -- Characters required for formatting
     * };
     *
     * Some prm_par.h settings have no analogue in NDR:
     *     val__eps -- Machine precision
     *     val__max -- Maximum (most positive) non-bad value
     *     val__min -- Minimum (most negative) non-bad value
     *     val__sml -- Smallest positive value
     */

    /* Bad values, used for flagging undefined data. */
    testval_prm(i, VAL, BAD, bad, UB);
    testval_prm(i, VAL, BAD, bad, B);
    testval_prm(i, VAL, BAD, bad, UW);
    testval_prm(i, VAL, BAD, bad, W);
    testval_prm(i, VAL, BAD, bad, I);
    testval_prm(f, VAL, BAD, bad, R);
    testval_prm(d, VAL, BAD, bad, D);

    /* Maximum (most positive) number */
    testval_prm(i, NUM, MAX, max, UB);
    testval_prm(i, NUM, MAX, max, B);
    testval_prm(i, NUM, MAX, max, UW);
    testval_prm(i, NUM, MAX, max, W);
    testval_prm(i, NUM, MAX, max, I);
    testval_prm(f, NUM, MAX, max, R);
    testval_prm(d, NUM, MAX, max, D);

    /* Minimum (most negative) number */
    testval_prm(i, NUM, MIN, min, UB);
    testval_prm(i, NUM, MIN, min, B);
    testval_prm(i, NUM, MIN, min, UW);
    testval_prm(i, NUM, MIN, min, W);
    testval_prm(i, NUM, MIN, min, I);
    testval_prm(f, NUM, MIN, min, R);
    testval_prm(d, NUM, MIN, min, D);
    
    /* Number of basic machine units (bytes) used by a value. */
    testval(i, VAL, NB, length, UB);
    testval(i, VAL, NB, length, B);
    testval(i, VAL, NB, length, UW);
    testval(i, VAL, NB, length, W);
    testval(i, VAL, NB, length, I);
    testval(i, VAL, NB, length, R);
    testval(i, VAL, NB, length, D);
    
    /* Number of characters required to format value as decimal string. */
    testval(i, VAL, SZ, txtsize, UB);
    testval(i, VAL, SZ, txtsize, B);
    testval(i, VAL, SZ, txtsize, UW);
    testval(i, VAL, SZ, txtsize, W);
    testval(i, VAL, SZ, txtsize, I);
    testval(i, VAL, SZ, txtsize, R);
    testval(i, VAL, SZ, txtsize, D);

    exit(nerrs);
}

/*
 * Write out a number in hex.  Write out the 4 or 8 bytes pointed to by `bp'
 * as a hex string.  If `*typep' is 'd', it's 8 bytes, otherwise 4.
 *
 * Only used for reporting problems in error messages.
 */
const char* tohex(const char* typep, byte* bp)
{
    static char c[17];
    const char *hexdigits = "0123456789ABCDEF";
    const int nbytes = (*typep == 'd' ? 8 : 4);
    int i;

#if WORDS_BIGENDIAN
    for (i=0; i<2*nbytes; bp++) {
        c[i++] = hexdigits[(*bp & 0xf0) >> 4];
        c[i++] = hexdigits[*bp & 0x0f];
    }
    c[i] = '\0';
#else /* WORDS_BIGENDIAN */
    c[2*nbytes] = '\0';
    for (i=2*nbytes-1; i>=0; bp++) {
        c[i--] = hexdigits[*bp & 0x0f];
        c[i--] = hexdigits[(*bp & 0xf0) >> 4];
    }
#endif /* WORDS_BIGENDIAN */

    return c;
}
