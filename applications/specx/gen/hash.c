/*
* Name hash.c
*    C functions replacing Fortran routines for handling the symbol HASH table
*/

#include <string.h>
#include <stdio.h>
#include "f77.h"
#include "symbols.h"

/*+
* Name: GEN_HASHINS
*
* Purpose:
*    A Fortran-callable function return the index within the symbol table of
*    the specified symbol
*
* Language:
*    C
*
* Invocation:
*    CALL GEN_HASHINS( FSYMBOL, M, FTABLE, VALUE )
*
* Arguments:
*    FSYMBOL = CHARACTER*(*) (Given)
*       The name of the symbol to be found
*    TYPE = CHARACTER*(*) (Given)
*       The type of the symbol L4/I4/R4/R8/Cnnn
*    M = INTEGER (Given)
*       The table length
*    ADDRESS = INTEGER (Given)
*       The address of table
*       This is here for compatibility with the Fortran routine.
*       The table used now is decalared static in header file symbols.h
*    VALUE = INTEGER (Returned)
*       The index within the symbol table
*-
*/

F77_INTEGER_FUNCTION(gen_hash)( CHARACTER(symbol), INTEGER(m) TRAIL(symbol) );

F77_INTEGER_FUNCTION(gen_hashins)( CHARACTER(fsymbol),
                                    INTEGER(m),
                                    INTEGER(ftable),
                                    INTEGER(value)
                                    TRAIL(fsymbol) ) {
DECLARE_CHARACTER_DYN(symbol);
int count;
int gen_hashins;
char empty[5]="####";

   count = 1 ;
   F77_CREATE_CHARACTER(symbol,fsymbol_length+1);
   cnfImprt( fsymbol, fsymbol_length, symbol );
   gen_hashins = F77_CALL(gen_hash)( CHARACTER_ARG(fsymbol), INTEGER_ARG(m)
                           TRAIL_ARG(fsymbol) );
   while ( strcmp( hashtab[gen_hashins].name, empty )
           && ( hashtab[gen_hashins].name[0] != '\0' )
           && ( count < *m ) ) {
      gen_hashins = ( gen_hashins + 1) % *m;
      count++;
   }
   if ( count < *m ) {
      strcpy( hashtab[gen_hashins].name, symbol );
      hashtab[gen_hashins].value = *value;
   } else {
      gen_hashins = -1;
      printf( "-- gen_hashins --\n" );
      printf( "Hash table full\n" );
   }

   F77_FREE_CHARACTER(symbol);
   return gen_hashins;
}
/*+
* Name: GEN_HASHSRCH
*
* Purpose:
*    A Fortran-callable function return the index within the symbol table of
*    the specified symbol
*
* Language:
*    C
*
* Invocation:
*    CALL GEN_HASHSRCH( FSYMBOL, M, FTABLE, VALUE )
*
* Arguments:
*    FSYMBOL = CHARACTER*(*) (Given)
*       The name of the symbol to be found
*    TYPE = CHARACTER*(*) (Given)
*       The type of the symbol L4/I4/R4/R8/Cnnn
*    M = INTEGER (Given)
*       The table length
*    ADDRESS = INTEGER (Given)
*       The address of table
*       This is here for compatibility with the Fortran routine.
*       The table used now is decalare static in header file symbols.h
*    VALUE = INTEGER (Returned)
*       The index within the symbol table
*-
*/

F77_INTEGER_FUNCTION(gen_hash)( CHARACTER(symbol), INTEGER(m) TRAIL(symbol) );

F77_INTEGER_FUNCTION(gen_hashsrch)( CHARACTER(fsymbol),
                                    INTEGER(m),
                                    INTEGER(ftable),
                                    INTEGER(value)
                                    TRAIL(fsymbol) ) {
DECLARE_CHARACTER_DYN(symbol);
int count;
int gen_hashsrch;

   count = 1 ;
   F77_CREATE_CHARACTER(symbol,fsymbol_length+1);
   cnfImprt( fsymbol, fsymbol_length, symbol );
   gen_hashsrch = F77_CALL(gen_hash)( CHARACTER_ARG(fsymbol), INTEGER_ARG(m)
                           TRAIL_ARG(fsymbol) );
   while ( strcmp( hashtab[gen_hashsrch].name, symbol )
           && ( hashtab[gen_hashsrch].name[0] != '\0' )
           && count < *m ) {
      gen_hashsrch = ( gen_hashsrch + 1) % *m;
      count++;
   }
   if ( !strcmp ( hashtab[gen_hashsrch].name, symbol ) )
      *value =  hashtab[gen_hashsrch].value;
   else
      gen_hashsrch = -1;

   F77_FREE_CHARACTER(symbol);
   return gen_hashsrch;
}
/*+
* Name: GEN_HASHNMSET
*
* Purpose:
*    A Fortran-callable subroutine to set the hash table entry name to
*    the specified value
*
* Language:
*    C
*
* Invocation:
*    CALL GEN_HASHDEL( ENTRY, VALUE )
*
* Arguments:
*    ENTRY = INTEGER (Given)
*       The hash table entry number
*    VALUE = CHARACTER*(*) (Given)
*       The name to be set
*-
*/

F77_SUBROUTINE(gen_hashnmset)( INTEGER(entry),
                               CHARACTER(fvalue) TRAIL(fvalue) ){
   cnfImprt( fvalue, fvalue_length, hashtab[*entry].name );

}
