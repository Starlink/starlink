#include <f77.h>

F77_SUBROUTINE( ech_mean_median )(
     INTEGER( n_points ),
     REAL( array ),
     LOGICAL( use_median ),
     LOGICAL( use_mode ),
     REAL( result ),
     INTEGER( status )
     )
{
/*+
 *  Name:
 *     ECHOMOP - ech_mean_median

 *  Purpose:
 *     Calculates mean, median, most-common-value.

 *  Description:
 *     this routine calculates the mean or median of the input n_points
 *     element array, and returns it in result.

 *  Invocation:
 *     call ech_mean_median(
 *     :    n_points,
 *     :    array,
 *     :    use_median,
 *     :    use_mode,
 *     :    result,
 *     :    status
 *     :   )

 *  Arguments:
 *     n_points = integer (given)
 *        number of points in array.
 *     array = real (given)
 *        array to be mean/median'd.
 *     use_median = logical (given)
 *        true to select return of median, else mean.
 *     use_mode = logical (given)
 *        true to select return of mode, overrides mean or median.
 *     result = real (returned)
 *        resulting mean/median value.
 *     status = integer (given and returned)
 *        global status.

 *  Method:
 *     if using median, calculate it
 *        find limiting values of array
 *        fill histogram bins
 *        find median/most common index
 *        calculate corresponding data value
 *     else calculate the mean
 *     endif

 *  Authors:
 *     DMILLS: Dave mills (UCL)
 *     MJC: Martin Clayton (Starlink, UCL)
 *     {enter_new_authors_here}

 *  History:
 *     01-sep-1992 (DMILLS):
 *       Initial release.
 *     28-mar-1996 (MJC):
 *       Corrected errors in histogram bin loading.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 */

/*Arguments Given:
 */
GENPTR_INTEGER( n_points )
GENPTR_REAL( array )
GENPTR_LOGICAL( use_median )
GENPTR_LOGICAL( use_mode )
GENPTR_REAL( result )
GENPTR_INTEGER( status )

#define MAX_BINS ( 8192 )
#define ECH__NO_DATA ( 19 )
#ifndef min
#define min( A, B )    ( ( A ) > ( B ) ) ? B : A
#endif
#ifndef max
#define max( A, B )    ( ( A ) < ( B ) ) ? B : A
#endif

/*Local Variables:
 */
F77_REAL_TYPE rlist[MAX_BINS];
F77_REAL_TYPE *ptr;
F77_REAL_TYPE *ptr2;
F77_REAL_TYPE minval;
F77_REAL_TYPE maxval;
F77_REAL_TYPE scale;
F77_REAL_TYPE swp;

F77_INTEGER_TYPE hist[MAX_BINS];
F77_INTEGER_TYPE *iptr;
F77_INTEGER_TYPE median_point;
F77_INTEGER_TYPE most_common_point;
F77_INTEGER_TYPE most_common;
F77_INTEGER_TYPE i;
F77_INTEGER_TYPE j;
F77_INTEGER_TYPE k;
F77_INTEGER_TYPE count;
F77_INTEGER_TYPE use_points;
F77_INTEGER_TYPE good_points;

/*.
 */

/*Simple check for one value.
 */
    if ( *n_points == 1 ) {
        *result = *array;
        return;
    }

/*Find the median of the array of values.
 */
    if ( *use_median || *use_mode ) {


/*    Estimate the median for a large number of points.
 */
        if ( *n_points > MAX_BINS || *use_mode ) {
            use_points = min( MAX_BINS, *n_points );

/*        Find limiting values in supplied data.
 */
            minval = max( *array, 0.0 );
            maxval = *array;
            ptr = array;
            for ( i = *n_points; i > 0; i-- ) {
                if ( *ptr >= 0.0 )  {
                    if ( *ptr < minval ) minval = *ptr;
                    if ( *ptr > maxval ) maxval = *ptr;
                }
                ptr++;
            }

/*        Fill histogram bins.
 */
            iptr = hist;
            i = use_points >> 4;
            switch ( use_points % 16 ) {
                case(  0 ): do { *iptr++ = 0;
                case( 15 ): *iptr++ = 0;
                case( 14 ): *iptr++ = 0;
                case( 13 ): *iptr++ = 0;
                case( 12 ): *iptr++ = 0;
                case( 11 ): *iptr++ = 0;
                case( 10 ): *iptr++ = 0;
                case(  9 ): *iptr++ = 0;
                case(  8 ): *iptr++ = 0;
                case(  7 ): *iptr++ = 0;
                case(  6 ): *iptr++ = 0;
                case(  5 ): *iptr++ = 0;
                case(  4 ): *iptr++ = 0;
                case(  3 ): *iptr++ = 0;
                case(  2 ): *iptr++ = 0;
                case(  1 ): *iptr++ = 0;
                } while ( --i > 0 );
            }
            scale = ( maxval - minval ) / (float)( use_points );
            if ( scale == 0.0 || use_points < 2 ) {
                *result = maxval;

            } else {
                good_points = 0;
                ptr = array;
                for ( i = *n_points; i > 0; i-- ) {
                    if ( *ptr >= 0.0 )  {
                        good_points++;
                        hist[ (int)( ( *ptr - minval ) / scale ) + 1 ]++;
                    }
                    ptr++;
                }

/*            Find median/most common index.
 */
                most_common_point = 0;
                most_common = 0;
                median_point = 0;
                count = 0;
                iptr = hist;
                for ( i = 0; i < use_points; i++ ) {
                    if ( ( good_points >> 1 ) > count  ) {
                        count += *iptr;
                        median_point = i;
                    }
                    if ( *iptr > most_common ) {
                        most_common = *iptr;
                        most_common_point = i;
                    }
                    iptr++;
                }

/*            Calculate corresponding data value.
 */
                if ( *use_mode ) {
                    *result = minval + (float)( most_common_point ) * scale;

                } else {
                    *result = minval + (float)( median_point ) * scale;
                }
            }

/*    Exact median for small number of points.
 */
        } else {


/*        Copy input array.
 */
            ptr = array;
            ptr2 = rlist;
            i = *n_points >> 4;
            switch ( *n_points % 16 ) {
                case(  0 ): do { *ptr2++ = *ptr++;
                case( 15 ) : *ptr2++ = *ptr++;
                case( 14 ) : *ptr2++ = *ptr++;
                case( 13 ) : *ptr2++ = *ptr++;
                case( 12 ) : *ptr2++ = *ptr++;
                case( 11 ) : *ptr2++ = *ptr++;
                case( 10 ) : *ptr2++ = *ptr++;
                case(  9 ) : *ptr2++ = *ptr++;
                case(  8 ) : *ptr2++ = *ptr++;
                case(  7 ) : *ptr2++ = *ptr++;
                case(  6 ) : *ptr2++ = *ptr++;
                case(  5 ) : *ptr2++ = *ptr++;
                case(  4 ) : *ptr2++ = *ptr++;
                case(  3 ) : *ptr2++ = *ptr++;
                case(  2 ) : *ptr2++ = *ptr++;
                case(  1 ) : *ptr2++ = *ptr++;
                } while ( --i > 0 );
            }

/*        Select sorting method depending on number of points.
 */
            if ( *n_points > 10 ) {

/*            (Shell) sort the array.
 */
                i = *n_points >> 1;
                while ( i > 0 ) {
                    for ( j = i + 1; j <= *n_points; j++ ) {
                        k = j - i;
                        while ( k > 0 ) {
                            if ( rlist[k] <= rlist[k + i] ) {
                               break;
                            }
                            swp = rlist[k];
                            rlist[k] = rlist[k + i];
                            rlist[k + i] = swp;
                            k -= i;
                        }
                    }
                    i >>= 1;
                }

            } else {

/*            (Bubble) sort the array.
 */
                ptr = rlist;
                for ( i = *n_points; i > 0; i-- ) {
                    ptr2 = ptr;
                    for ( j = i - 1; j > 0; j-- ) {
                        ptr2++;
                        if ( *ptr2 < *ptr )  {
                            swp = *ptr;
                            *ptr = *ptr2;
                            *ptr2 = swp;
                        }
                    }
                    ptr++;
                }
            }

/*        Find median - use middle value for odd number of points.
 */
            if ( *n_points % 2 == 1 ) {
                *result = rlist[( *n_points + 2 ) >> 1];

/*        Use mean of two central values for even number of points.
 */
            } else {
                *result = ( rlist[*n_points >> 1] +
                          rlist[( *n_points >> 1 ) + 1] ) / 2.0;
            }
        }

/*Else calculate the mean.
 */
    } else {
        count = 0;
        *result = 0.0;
        i = *n_points >> 4;
        ptr = array;
        switch ( *n_points % 16 ) {
            case(  0 ): do {
                if ( *ptr >= 0.0 ) { *result += *ptr; count++; } ptr++;
            case( 15 ):
                if ( *ptr >= 0.0 ) { *result += *ptr; count++; } ptr++;
            case( 14 ):
                if ( *ptr >= 0.0 ) { *result += *ptr; count++; } ptr++;
            case( 13 ):
                if ( *ptr >= 0.0 ) { *result += *ptr; count++; } ptr++;
            case( 12 ):
                if ( *ptr >= 0.0 ) { *result += *ptr; count++; } ptr++;
            case( 11 ):
                if ( *ptr >= 0.0 ) { *result += *ptr; count++; } ptr++;
            case( 10 ):
                if ( *ptr >= 0.0 ) { *result += *ptr; count++; } ptr++;
            case(  9 ):
                if ( *ptr >= 0.0 ) { *result += *ptr; count++; } ptr++;
            case(  8 ):
                if ( *ptr >= 0.0 ) { *result += *ptr; count++; } ptr++;
            case(  7 ):
                if ( *ptr >= 0.0 ) { *result += *ptr; count++; } ptr++;
            case(  6 ):
                if ( *ptr >= 0.0 ) { *result += *ptr; count++; } ptr++;
            case(  5 ):
                if ( *ptr >= 0.0 ) { *result += *ptr; count++; } ptr++;
            case(  4 ):
                if ( *ptr >= 0.0 ) { *result += *ptr; count++; } ptr++;
            case(  3 ):
                if ( *ptr >= 0.0 ) { *result += *ptr; count++; } ptr++;
            case(  2 ):
                if ( *ptr >= 0.0 ) { *result += *ptr; count++; } ptr++;
            case(  1 ):
                if ( *ptr >= 0.0 ) { *result += *ptr; count++; } ptr++;
            } while ( --i > 0 );
        }
        if ( count > 0 ) {
            *result = *result / (float)( count );

        } else {
            *status = ECH__NO_DATA;
        }
    }

return;
}
