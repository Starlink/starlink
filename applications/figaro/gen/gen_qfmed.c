/*+
 * Name:
 *    gen_qfmed

 * Purpose:
 *    Finds the median value of a real data array.

 * Language:
 *    Starlink ANSI C

 * Type of Module:
 *    Fortran-callable C function.

 * Invocation (from Fortran):
 *    RESULT = GEN_QFMED( DATA, N )

 * Description:
 *    Finds the median value of the NELM elements of the array DATA.
 *    DATA is conceptually sorted into ascending order and the median
 *    value is then either the value in the central element (if NELM
 *    is odd) or the average of the two central elements (if NELM is
 *    even). DATA is not in fact fully sorted, but its elements are
 *    re-aranged as far as is needed to determine the median.
 *
 *    For the method see any book on sorting (eg Knuth, The Art of Computer
 *    Programming, vol 3,  Addison-Wesley).

 * Arguments:
 *    DATA( NELM ) = REAL (Given and returned)
 *       Passed containing the array values to be sorted, returned with
 *       them jumbled somewhat.
 *    NELM = INTEGER (Given)
 *       Number of elements in DATA.

 * Returned Value (in Fortran):
 *    GEN_QFMED = REAL (Returned)
 *       The median value required.

 * Notes:
 *    This is a modification of the quicksort routine GEN_QFSORT, which does not
 *    bother with a full sort. Based on the Forth assembler routine by WRS
 *    (KPNO). Converted to VAX/VMS assembler by KS (CIT).

 * Authors:
 *    ks:  Keith Shortridge (CIT)
 *    ckl: ? (CIT)
 *    sns: Sam Southard, Jr. (CIT)
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    30 Jan 1984 (ks):
 *       Original version.
 *    21 Nov 1988 (ckl):
 *       Converted to C for Sun.
 *    31 Jul 1989 (sns):
 *       Sun f77 bug fixed.
 *    23 Sep 1992 (hme):
 *       No conditional compilations: Always include <math.h>, always use
 *       "float".
 *    23 Oct 1992 (hme):
 *       Do not declare qqfmed within the functions. Do not declare qqfmed as
 *       static, but prototype it as such.
 *    18 Mar 1993 (hme):
 *       Use F77 to make the interface to Fortran portable.
 *-
 */

#include <math.h>
#include "f77.h"

static void qqfmed( int first, int last, float data[], int cent );

/*:
 */

/* float gen_qfmed_( float data[], int *nelm )
 */

F77_REAL_FUNCTION(gen_qfmed)( REAL_ARRAY(data), INTEGER(nelm) )
{
   GENPTR_REAL_ARRAY(data)
   GENPTR_INTEGER(nelm)

   int   cent,i;
   float median,temp;

/*.
 */

   cent = ( *nelm / 2 );                 /* C indices start at 0        */
   (void) qqfmed( 0, *nelm-1, data, cent );
   median = data[cent];
   if ( (*nelm % 2) == 0 )               /* if NELM is even, look for   */
   {  temp = data[cent-1];               /* highest value in the low    */
      for ( i = 0; i < cent-1; i++ )     /* partition, then use average */
         if ( data[i] > temp )
            temp = data[i];
      median = ( median + temp ) * 0.5;
   }
   return ( median );
}

/*+
 *-
 */

void qqfmed( int first, int last, float data[], int cent )

{  int   i,j;
   float temp,vmid;

/*.
 */

   i = first;
   j = last;
   vmid = data[ (i+j) / 2 ];
   do
   {   while ( data[i] < vmid ) i++;
       while ( vmid < data[j] ) j--;
       if ( i <= j )
       {  temp = data[i]; data[i] = data[j]; data[j] = temp;
          i++; j--;
       }
   } while ( i <= j );

   if ( (first < j) && (cent <= j) )
      (void) qqfmed( first, j, data, cent );
   if ( (i <= cent) && (i < last) )
      (void) qqfmed( i, last, data, cent );
}

/*
;       Appendix: The following PASCAL program is as close as one
;       can get to the Macro version.  It was written to make sure
;       that the workings of the algorithm were understood before it
;       was coded in Macro.  PASCAL was used rather than FORTRAN
;       because you can't use Fortran recursively.  It may make the
;       operation  of the Macro a little clearer..
;
;       MODULE QFMED(OUTPUT);
;
;       [GLOBAL] FUNCTION QFMED
;          (VAR DATA : ARRAY[LOW_INDEX..HIGH_INDEX : INTEGER] OF REAL;
;           VAR NELM : INTEGER): REAL;
;
;          { Finds the median value of the NELM elements of the array DATA }
;
;          VAR CENT,I: INTEGER;
;          VAR MEDIAN,TEMP: REAL;
;
;          PROCEDURE QQFMED(FIRST,LAST:INTEGER);
;
;             { Once QFMED has set CENT to give the center element of the
;               array DATA, it calls QQFMED which homes in on the value
;               that should be in that element.  It does that using an
;               algorithm that is a modified quicksort, splitting the
;               partition DATA[FIRST..LAST] into two partitions, one
;               with all values above the initial central value, one with
;               all values below it, and then calling itself to repeat the
;               operation on the partition that contains the central
;               element }
;
;            VAR
;               I,J       : INTEGER;
;               TEMP,VMID : REAL;
;
;            BEGIN
;               I:=FIRST;
;               J:=LAST;
;               VMID:=DATA[(I+J) DIV 2];
;               REPEAT
;                  WHILE DATA[I]<VMID DO I:=I+1;
;                  WHILE VMID<DATA[J] DO J:=J-1;
;                  IF I<=J THEN BEGIN
;                     TEMP:=DATA[I]; DATA[I]:=DATA[J]; DATA[J]:=TEMP;
;                     I:=I+1; J:=J-1;
;                  END;
;               UNTIL I>J;
;               IF (FIRST<J) AND (CENT<=J) THEN QQFMED(FIRST,J);
;               IF (I<=CENT) AND (I<LAST)  THEN QQFMED(I,LAST);
;             END;
;
;         { End of QQFMED }
;
;         BEGIN
;            CENT:=(NELM DIV 2)+1;
;            QQFMED(1,NELM);
;            MEDIAN:=DATA[CENT];               { if NELM is even, look for   }
;            IF (NELM MOD 2) = 0 THEN BEGIN    { highest value in the low    }
;               TEMP:=DATA[CENT-1];            { partition, then use average }
;               FOR I:= 1 TO CENT-2 DO
;                  IF DATA[I] > TEMP THEN TEMP:=DATA[I];
;               MEDIAN:=(MEDIAN+TEMP)*0.5;
;            END;
;            QFMED:=MEDIAN
;         END;
;
;         { End of QFMED }
;
;      END.
;
*/
