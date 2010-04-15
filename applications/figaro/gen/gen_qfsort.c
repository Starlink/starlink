/*+
 * Name:
 *    gen_qfsort

 * Purpose:
 *    Sorts a real array using the Quicksort algorithm.

 * Language:
 *    Starlink ANSI C

 * Type of Module:
 *    Fortran-callable C funtion.

 * Invocation (from Fortran):
 *    CALL GEN_QFSORT( ARRAY, N )

 * Description:
 *    Given an array of real numbers, GEN_QFSORT sorts them into
 *    ascending order. Uses the Quicksort algorithm.  See any book on
 *    sorting (e.g. Knuth, The Art of Computer Programming, vol 3,
 *    Addison-Wesley).

 * Arguments:
 *    ARRAY( N ) = REAL (Given and Returned)
 *       Passed containing the values to be sorted, returned sorted into
 *       ascending order.
 *    N = INTEGER (Given)
 *       Number of values in ARRAY.

 * Returned Value:
 *    none.

 * Authors:
 *    ks:  Keith Shortridge (AAO)
 *    ckl: ? (CIT)
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    01 May 1986 (ks):
 *       Original version.
 *    21 Nov 1988 (ckl):
 *       Converted to C for Sun.
 *    23 Oct 1992 (hme):
 *       Do not declare qqfs within functions or as static, but make it
 *       static by prototyping.
 *    18 Mar 1993 (hme):
 *       Use F77 to make it portable.
 *-
 */

#include "f77.h"

static void qqfs( int first, int last, float data[] );

/*:
 */

/* void gen_qfsort_( float data[], int *n )
 */

F77_SUBROUTINE(gen_qfsort)( REAL_ARRAY(data), INTEGER(n) )
{
   GENPTR_REAL_ARRAY(data)
   GENPTR_INTEGER(n)

/*.
 */

   (void) qqfs(0,*n-1,data);
}

/*+
 *-
 */

void qqfs( int first, int last, float data[] )

{  int   i,j;
   float temp,vmid;

/*.
 */

   i = first; j = last;
   vmid = data[ (i+j) / 2 ];
   do
   {  while ( data[i] < vmid ) i++;
      while ( vmid < data[j] ) j--;
      if ( i <= j )
      {  temp = data[i]; data[i] = data[j]; data[j] = temp;
         i++; j--;
      }
   } while ( i <= j );

   if ( first < j )
      (void) qqfs( first, j, data );
   if ( i < last )
      (void) qqfs( i, last, data );
}

/*
;       Appendix: The following PASCAL program is as close as one
;       can get to the Macro version.  It was written to make sure
;       that the workings of the algorithm were understood before it
;       was coded in Macro.  PASCAL was used rather than FORTRAN
;       because you can't use Fortran recursively.  It may make the
;       operation  of the Macro a little clearer..
;
;       PROCEDURE QFSORT
;          (VAR DATA : ARRAY[LOW_INDEX..HIGH_INDEX : INTEGER] OF REAL);
;
;       { Quicksort of the array DATA }
;
;       PROCEDURE QQFS(FIRST,LAST:INTEGER);
;
;         { Sorts the partition DATA[FIRST..LAST] by splitting it
;           into two partitions, one with all values above the initial
;           central value, one with all values below it, and then
;           calling itself to sort those partitions }
;
;         VAR
;            I,J       : INTEGER;
;            TEMP,VMID : REAL;
;
;         BEGIN
;            I:=FIRST;
;            J:=LAST;
;            VMID:=DATA[(I+J) DIV 2];
;            REPEAT
;               WHILE DATA[I]<VMID DO I:=I+1;
;               WHILE VMID<DATA[J] DO J:=J-1;
;               IF I<=J THEN BEGIN
;                  TEMP:=DATA[I]; DATA[I]:=DATA[J]; DATA[J]:=TEMP;
;                  I:=I+1; J:=J-1;
;               END;
;            UNTIL I>J;
;            IF FIRST<J THEN QQFS(FIRST,J);
;            IF I<LAST  THEN QQFS(I,LAST);
;          END;
;
;         { End of QQFS }
;
;         BEGIN
;            QQFS(LOW_INDEX,HIGH_INDEX)
;         END;
;
;         { End of QFSORT }
;
*/
