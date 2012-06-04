#ifndef _BYTESWAP_INCLUDED_
#define _BYTESWAP_INCLUDED_

/*  Define the byteswap functions in C (from C++ Skycat define.h) */

#include <arpa/inet.h>
#include <netinet/in.h>
#if HAVE_CONFIG_H
#include "config.h"     /* Local config.h */
#endif

/* Byte swap defines. On little Endian machines we use the network-to-host
 * routines since they are faster (implemented in assembler code). */
#if WORDS_BIGENDIAN
#  define BIGENDIAN 1

/* from /usr/include/bits/byteswap.h: */
#  define SWAP16(x) \
    ((((x) >> 8) & 0xff) | (((x) & 0xff) << 8))
#  define SWAP32(x) \
    ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) | \
     (((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24))

#else

#  define BIGENDIAN 0
#  define SWAP16(x) (ntohs((unsigned short)x))
#  define SWAP32(x) (ntohl((unsigned int)x))
#endif


/* Float data: Prevent auto conversions.
 * No speed hits if this is compiled with gcc -O.
 * Need static functions for C99 compliance (previously just used from C++).
 */
static inline float SWAP_FLOAT( float x )
{
    union {float f; unsigned int i;} u;
    u.f = x;
    u.i = SWAP32(u.i);
    return u.f;
}

static inline double SWAP_DOUBLE( double x )
{
    union {double d; unsigned int l[2];} u;
    unsigned int tmp;

    u.d = x;
    tmp = u.l[0];
    u.l[0] = SWAP32(u.l[1]);
    u.l[1] = SWAP32(tmp);
    return u.d;
}

static inline INT64 SWAP_INT64( INT64 x )
{
    union {INT64 d; unsigned int l[2];} u;
    unsigned int tmp;

    u.d = x;
    tmp = u.l[0];
    u.l[0] = SWAP32(u.l[1]);
    u.l[1] = SWAP32(tmp);
    return u.d;
}

static inline int SWAP_INT(int x)
{
    union {int i; unsigned int ui;} u;
    u.i = x;
    u.ui = SWAP32(u.ui);
    return u.i;
}

static inline short SWAP_SHORT( short x )
{
    union {short i; unsigned short ui;} u;
    u.i = x;
    u.ui = SWAP16(u.ui);
    return u.i;
}

static inline unsigned short SWAP_USHORT( unsigned short x )
{
    return SWAP16( x );
}

#endif /* _BYTESWAP_INCLUDED_ */

