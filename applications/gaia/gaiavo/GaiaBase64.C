/*
 * Copyright (C), 2000-2007 by the monit project group.
 * Copyright (C), 2008 Science and Technology Facilities Council.
 * All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 *  Implementation of base64 encoding/decoding.
 *
 *  @author Jan-Henrik Haukeland, <hauk@tildeslash.com>
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gaiabase64.h"

namespace gaia {

    /* Private prototypes */
    static bool is_base64( char c );
    static char encode( char u );
    static char decode( char c );

    /**
     * Base64 encode and return size data in 'src'. The caller must delete the
     * returned string.
     *
     * size The size of the data in src
     * src The data to be base64 encode
     *
     * returns encoded string otherwise NULL
     */
    char *encode_base64( int size, char *src )
    {
        int i;
        char *out;
        char *p;

        if ( !src ) {
            return NULL;
        }

        if ( !size ) {
            size = strlen( (char *) src );
        }
        out = new char[size * 4 / 3 + 4 ];

        p = out;

        for ( i = 0; i < size; i += 3 ) {
            char b1 = 0;
            char b2 = 0;
            char b3 = 0;
            char b4 = 0;
            char b5 = 0;
            char b6 = 0;
            char b7 = 0;

            b1 = src[i];

            if ( i+1 < size ) {
                b2 = src[i+1];
            }

            if ( i+2 < size ) {
                b3 = src[i+2];
            }

            b4 = b1>>2;
            b5 = ( ( b1&0x3 )<<4 ) | ( b2>>4 );
            b6 = ( ( b2&0xf )<<2 ) | ( b3>>6 );
            b7 = b3&0x3f;

            *p++ = encode( b4 );
            *p++ = encode( b5 );

            if ( i+1 < size ) {
                *p++ = encode( b6 );
            }
            else {
                *p++ = '=';
            }

            if ( i+2 < size ) {
                *p++ = encode( b7 );
            }
            else {
                *p++ = '=';
            }
        }
        return out;
    }


    /**
     * Decode the base64 encoded string 'src' into the memory pointed to by
     * 'dest'. The dest buffer is not NULL terminated.
     *
     * dest Pointer to memory for holding the decoded string.
     *      Must be large enough to recieve the decoded string.
     * src  A base64 encoded string.
     * dl   Length of dest used.
     *
     * returns true if succeeded.
     */
    bool decode_base64( const char *src,  char *dest, size_t *dl )
    {
        if ( src && *src ) {
             char *p = dest;
            int k;
            int l = strlen( src ) + 1;
             char *buf = new char[l];

            /* Ignore non base64 chars as per the POSIX standard */
            for ( k = 0, l = 0; src[k]; k++ ) {
                if ( is_base64( src[k] ) ) {
                    buf[l++] = src[k];
                }
            }

            for ( k = 0; k < l; k += 4 ) {
                char c1='A';
                char c2='A';
                char c3='A';
                char c4='A';
                char b1=0;
                char b2=0;
                char b3=0;
                char b4=0;

                c1 = buf[k];

                if ( k + 1 < l ) {
                    c2 = buf[k+1];
                }

                if ( k + 2 < l ) {
                    c3 = buf[k+2];
                }

                if ( k + 3 < l ) {
                    c4 = buf[k+3];
                }

                b1 = decode( c1 );
                b2 = decode( c2 );
                b3 = decode( c3 );
                b4 = decode( c4 );

                *p++ = ( ( b1<<2 ) | ( b2>>4 ) );

                if ( c3 != '=' ) {
                    *p++ = ( ( ( b2&0xf )<<4 ) | ( b3>>2 ) );
                }

                if ( c4 != '=' ) {
                    *p++ = ( ( ( b3&0x3 )<<6 ) | b4 );
                }
            }
            delete buf;
            *dl = ( p - dest );
            return true;
        }
        return false;
    }


    /**
     * Base64 encode one byte
     */
    static char encode( char u )
    {
        if( u < 26 ) {
            return ( 'A' + u );
        }
        if( u < 52 )  {
            return ( 'a' + ( u - 26 ) );
        }
        if( u < 62 ) {
            return ( '0' + ( u - 52 ) );
        }
        if( u == 62 ) {
            return '+';
        }
        return '/';
    }


    /**
     * Decode a base64 character
     */
    static char decode( char c )
    {
        if ( c >= 'A' && c <= 'Z' ) {
            return ( c - 'A' );
        }
        if ( c >= 'a' && c <= 'z' ) {
            return ( c - 'a' + 26 );
        }
        if ( c >= '0' && c <= '9' ) {
            return ( c - '0' + 52 );
        }
        if ( c == '+' ) {
            return 62;
        }
        return 63;
    }

    /**
     * Return true if 'c' is a valid base64 character, otherwise FALSE
     */
    static bool is_base64( char c )
    {
        if ( ( c >= 'A' && c <= 'Z' ) || ( c >= 'a' && c <= 'z' ) ||
             ( c >= '0' && c <= '9' ) || ( c == '+' )             ||
             ( c == '/' )             || ( c == '=' ) ) {
            return true;
        }
        return false;
    }
}
