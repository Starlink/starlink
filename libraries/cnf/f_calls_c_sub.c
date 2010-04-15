#include <string.h>
#include "f77.h"

F77_SUBROUTINE(ti)( INTEGER_ARRAY(ia), INTEGER(ni), INTEGER(i) )

/* Add up the numbers stored in the array ia and put the results in i.	    */
{
   GENPTR_INTEGER_ARRAY(ia)	    /* The array of numbers		    */
   GENPTR_INTEGER(ni)		    /* The number of elements in the array  */
   GENPTR_INTEGER(i)		    /* The return value			    */

   int j;

   *i = 0;
   for( j = 0 ; j < *ni ; j++ )
      *i = *i + ia[j];
}

F77_SUBROUTINE(tr)( REAL_ARRAY(ra), INTEGER(nr), REAL(r) )

/* Calculate the mean of the numbers stored in the array ra and put the	    */
/* results in r.							    */
{
   GENPTR_REAL_ARRAY(ra)	    /* The array of numbers		    */
   GENPTR_INTEGER(nr)		    /* The number of elements in the array  */
   GENPTR_REAL(r)		    /* The return value			    */

   int j;

   *r = 0;
   for( j = 0 ; j < *nr ; j++ )
      *r = *r + ra[j];
   *r = *r / *nr;
}

F77_SUBROUTINE(td)( DOUBLE_ARRAY(da), INTEGER(nd), DOUBLE(d) )

/* Calculate the sum of the squares of the numbers stored in the array da   */
/* and put the results in d.						    */
{
   GENPTR_DOUBLE_ARRAY(da)	    /* The array of numbers		    */
   GENPTR_INTEGER(nd)		    /* The number of elements in the array  */
   GENPTR_DOUBLE(d)		    /* The return value			    */

   int j;

   *d = 0;
   for( j = 0 ; j < *nd ; j++ )
      *d = *d + da[j] * da[j];
}

F77_SUBROUTINE(tl)( LOGICAL_ARRAY(la), INTEGER(nl), LOGICAL(l) )

/* Find the number of true values in the array la and see if this is equal  */
/* to 5. Return the result in l.					    */
{
   GENPTR_LOGICAL_ARRAY(la)	    /* The array of numbers		    */
   GENPTR_INTEGER(nl)		    /* The number of elements in the array  */
   GENPTR_LOGICAL(l)		    /* The return value			    */

   int j, k=0;

   j = F77_ISFALSE( la[0] );  /* This line is only used	to test the macro   */

   for( j = 0 ; j < *nl ; j++ )
      if( F77_ISTRUE( la[j] ) )
         k++;
   if( k == 5 )
      *l = F77_TRUE;
   else
      *l = F77_FALSE;
}

F77_SUBROUTINE(tb)( BYTE_ARRAY(ba), INTEGER(nb), BYTE(b) )

/* Add up the numbers stored in the array ba and put the results in b.	    */
{
   GENPTR_BYTE_ARRAY(ba)	    /* The array of numbers		    */
   GENPTR_INTEGER(nb)		    /* The number of elements in the array  */
   GENPTR_BYTE(b)		    /* The return value			    */

   int j;

   *b = 0;
   for( j = 0 ; j < *nb ; j++ )
      *b = *b + ba[j];
}

F77_SUBROUTINE(tw)( WORD_ARRAY(wa), INTEGER(nw), WORD(w) )

/* Add up the numbers stored in the array wa and put the results in w.	    */
{
   GENPTR_WORD_ARRAY(wa)	    /* The array of numbers		    */
   GENPTR_INTEGER(nw)		    /* The number of elements in the array  */
   GENPTR_WORD(w)		    /* The return value			    */

   int j;

   *w = 0;
   for( j = 0 ; j < *nw ; j++ )
      *w = *w + wa[j];
}

F77_SUBROUTINE(tub)( UBYTE_ARRAY(uba), INTEGER(nub), UBYTE(ub) )

/* Add up the numbers stored in the array ba and put the results in ub.	    */
{
   GENPTR_UBYTE_ARRAY(uba)	    /* The array of numbers		    */
   GENPTR_INTEGER(nub)		    /* The number of elements in the array  */
   GENPTR_UBYTE(ub)		    /* The return value			    */

   int j;

   *ub = 0;
   for( j = 0 ; j < *nub ; j++ )
      *ub = *ub + uba[j];
}

F77_SUBROUTINE(tuw)( UWORD_ARRAY(uwa), INTEGER(nuw), UWORD(uw) )

/* Add up the numbers stored in the array wa and put the results in w.	    */
{
   GENPTR_UWORD_ARRAY(uwa)	    /* The array of numbers		    */
   GENPTR_INTEGER(nuw)		    /* The number of elements in the array  */
   GENPTR_UWORD(uw)		    /* The return value			    */

   int j;

   *uw = 0;
   for( j = 0 ; j < *nuw ; j++ )
      *uw = *uw + uwa[j];
}

F77_SUBROUTINE(tc1)( CHARACTER(c) TRAIL(c) )

/* Set the character variable to a string.				    */
{
   GENPTR_CHARACTER(c)		    /* The character variable		    */

   char string[10];
   strcpy( string, "Test C1" );

   cnf_exprt( string, c, c_length );
}

F77_SUBROUTINE(tc2)( CHARACTER_ARRAY(ca), INTEGER(nc), CHARACTER(line)
                     TRAIL(ca) TRAIL(line) )

/* Extract the strings from ca and put them together into string line       */
{
   GENPTR_CHARACTER_ARRAY(ca)	    /* The array of characters 		    */
   GENPTR_INTEGER(nc)		    /* The number of elements in the array  */
   GENPTR_CHARACTER(line)	    /* The return value			    */

   int j;
   char string1[80];
   char string2[80];
   char *ptr;

   ptr = ca;
   string2[0] = '\0';
   for( j = 0 ; j < *nc ; j++ )
   {
      cnf_imprt( ptr, ca_length, string1 );
      strcat( string2, string1 );
      ptr += ca_length;
   }
   cnf_exprt( string2, line, line_length );
}

/* Check the use of common blocks.					    */

extern struct
{
   F77_INTEGER_TYPE cbi;
   F77_REAL_TYPE cbr;
} F77_BLANK_COMMON;

extern struct
{
   F77_INTEGER_TYPE cni;
   F77_REAL_TYPE cnr;
} F77_NAMED_COMMON(comm1);

F77_SUBROUTINE(tcom)( )

{
   F77_NAMED_COMMON(comm1).cni = F77_BLANK_COMMON.cbi;
   F77_NAMED_COMMON(comm1).cnr = F77_BLANK_COMMON.cbr;
}

/* Define an INTEGER function.						    */

F77_INTEGER_FUNCTION(fi)( INTEGER(i) )
{
   GENPTR_INTEGER(i)

   return(42);
}

/* Define a REAL function.						    */

F77_REAL_FUNCTION(fr)( REAL(r) )
{
   GENPTR_REAL(r)

   return(3.14159);
}

/* Define a DOUBLE PRECISION function.						    */

F77_DOUBLE_FUNCTION(fd)( DOUBLE(d) )
{
   GENPTR_DOUBLE(d)

   return(1.23456789);
}

/* Define a LOGICAL function.						    */

F77_LOGICAL_FUNCTION(fl)( LOGICAL(l) )
{
   GENPTR_LOGICAL(l)

   if( F77_ISTRUE(*l) )
      return( F77_FALSE );
   else
      return( F77_TRUE );
}

/* Define a CHARACTER function.						    */

F77_CHARACTER_FUNCTION(fc)( CHARACTER_RETURN_VALUE(text), CHARACTER(c)
                                                          TRAIL(c) )
{
   GENPTR_CHARACTER(text)
   GENPTR_CHARACTER(c)

   static char local[41];

   cnf_imprt( c, c_length, local );
   strcat( local, " to you too" );
   cnf_exprt( local, text, text_length );
}

/* Define a BYTE function.						    */

F77_BYTE_FUNCTION(fb)( BYTE(b) )
{
   GENPTR_BYTE(b)

   return(*b);
}

/* Define a WORD function.						    */

F77_WORD_FUNCTION(fw)( WORD(w) )
{
   GENPTR_WORD(w)

   return(*w);
}

/* Define an UNSIGNED BYTE function.						    */

F77_UBYTE_FUNCTION(fub)( UBYTE(ub) )
{
   GENPTR_UBYTE(ub)

   return(*ub);
}

/* Define a UNSIGNED WORD function.						    */

F77_UWORD_FUNCTION(fuw)( UWORD(uw) )
{
   GENPTR_UWORD(uw)

   return(*uw);
}

/* Test the use of pointers.						    */

F77_SUBROUTINE(getmem)( POINTER(ptr), INTEGER(n) )
{
   GENPTR_POINTER(ptr)
   GENPTR_INTEGER(n)
   void * temp;

   temp = cnfMalloc( (size_t)(*n) );
   *ptr = cnfFptr( temp );
}

F77_SUBROUTINE(freemem)( POINTER(ptr) )
{
   GENPTR_POINTER(ptr)
   cnfFree( cnfCptr( *ptr ) );
}
