/*+
 * Name:
 *    gen_qfisort

 * Purpose:
 *    Indirect sort of float array.

 * Language:
 *    Starlink ANSI C

 * Type of Module:
 *    Fortran-callable C function.

 * Invocation (from Fortran):
 *    CALL GEN_QFISORT( DATA, N, PTRS )

 * Description:
 *    This routine indirectly sorts a real array using the
 *    Quicksort algorithm.

 * Arguments:
 *    DATA( N ) = REAL (Given)
 *       The array containing the values to be sorted.
 *    N = INTEGER (Given)
 *       The number of elements in data or ptrs.
 *    PTRS( N ) = INTEGER (Returned)
 *       Contains the pointers to the sorted array. I.e. the lowest values
 *       element is DATA(PTRS(1)), and the highest is DATA(PTRS(N)).

 * Return value:
 *    None.

 * Authors:
 *    ks:  Keith Shortridge (AAO)
 *    ckl: (CIT)
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    08 Mar 1985 (ks):
 *       Original version.
 *    21 Nov 1988 (ckl):
 *       Converted to C for Sun.
 *    08 Sep 1992 (hme):
 *       Change qqfs to qqfis to avoid name clash.
 *    22 Oct 1992 (hme):
 *       Re-typed according to ANSI. No longer declare qqfis inside the
 *       functions. Declare it as static in a prototype before (!) any
 *       routine.
 *    19 Mar 1993 (hme):
 *       Use F77 to make the interface to Fortran portable.
 *-
 */

#include "f77.h"

static void qqfis( int first, int last, float data[], int ptrs[] );

/*:
 */

/* void gen_qfisort_( float data[], int *n, int ptrs[] );
 */

F77_SUBROUTINE(gen_qfisort)( REAL_ARRAY(data), INTEGER(n),
                             INTEGER_ARRAY(ptrs) )
{
   GENPTR_REAL_ARRAY(data)
   GENPTR_INTEGER(n)
   GENPTR_INTEGER_ARRAY(ptrs)

   int indx;

/*.
 */

   for ( indx = 0; indx < *n; indx++ )
      ptrs[indx] = indx+1;

   (void) qqfis( 0, *n-1, data, ptrs );

/* Return.
 */
   return;
}

/*+
 * Name:
 *    qqfis

 * Purpose:
 *    Subsidiary routine to gen_qfisort.

 * Language:
 *    Starlink ANSI C

 * Type of Module:
 *    C function.

 * Invocation:
 *    (void) qqfis( first, last, data, ptrs );

 * Description:
 *    This routine sorts the partition data[first...last] by splitting it
 *    into two partitions, one with all values above the initial central
 *    value, on with all values below it, and then it calls itself to sort
 *    those partitions.

 * Arguments:
 *    first = int (Given)
 *       Begin of partition.
 *    last = int (Given)
 *       End of partition.
 *    data[] = float (Given)
 *       The array containig the values to be sorted.
 *    ptrs[] = int (Returned)
 *       Contains the pointers to the sorted array.

 * Return value:
 *    None.

 * Authors:
 *    ks:  Keith Shortridge (AAO)
 *    ckl: (CIT)
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    08 Mar 1985 (ks):
 *       Original version.
 *    21 Nov 1988 (ckl):
 *       Converted to C for Sun.
 *    08 Sep 1992 (hme):
 *       Change qqfs to qqfis to avoid name clash.
 *    22 Oct 1992 (hme):
 *       Re-typed according to ANSI. No longer declare qqfis inside the
 *       functions. Declare it as static in a prototype before (!) any
 *       routine.
 *-
 */

/*:
 */

void qqfis( int first, int last, float data[], int ptrs[] )

{  int   i, j, temp;
   float vmid;

/*.
 */

   i = first; j = last;
   vmid = data[ ptrs[ (i+j)/2 ] - 1 ];     /* C indices start at 0 */

   do
   {  while ( data[ ptrs[i] - 1 ] < vmid ) i++;
      while ( vmid < data[ ptrs[j] - 1 ] ) j--;
      if ( i <= j )
      {  temp = ptrs[i]; ptrs[i] = ptrs[j]; ptrs[j] = temp;
         i++; j--;
      }
   } while ( i <= j );

   if ( first < j )
      (void) qqfis( first, j, data, ptrs );
   if ( i < last )
      (void) qqfis( i, last, data, ptrs );

/* Return.
 */
   return;
}
