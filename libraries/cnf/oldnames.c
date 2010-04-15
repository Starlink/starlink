/* oldnames.c
* This provides drop-through versions of the cnf functions with the old-style
* names (e.g. cnf_copyf instead of cnfCopyf). It allows existing compiled code
* and executables linked with shared libraries to continue working.
* On re-compilation, assuming f77.h is #included, the new-style names will be
* used even if the code has not been changed to use the new names.
*
* Authors:
*    AJC: A.J.Chipperfield (Starlink)
* History:
*    23-JUN-1999 (AJC):
*       Original version
*/
#define CNF_OLD_DEFINED          /* Prevent redefinition of old names */
#include "f77.h"                 /* CNF macros and prototypes */

void cnf_copyf( const char *source_f, int source_len, char *dest_f,
                int dest_len ) {
   cnfCopyf( source_f, source_len, dest_f, dest_len );
}

char *cnf_creat( int length ) {
   return cnfCreat( length );
}

F77_CHARACTER_ARG_TYPE *cnf_cref( int length ) {
   return cnfCref( length );
}

F77_CHARACTER_ARG_TYPE *cnf_crefa( int length, int ndims, const int *dims ) {
   return cnfCrefa( length, ndims, dims );
}

char *cnf_creib( const char *source_f, int source_len ) {
   return cnfCreib( source_f, source_len );
}

char *cnf_creim( const char *source_f, int source_len ) {
   return cnfCreim( source_f, source_len );
}

F77_LOGICAL_TYPE *cnf_crela( int ndims, const int *dims ) {
   return cnfCrela( ndims, dims );
}

void cnf_expch( const char *source_c, char *dest_f, int nchars ) {
   cnfExpch( source_c, dest_f, nchars );
}

void cnf_expla( const int *source_c, F77_LOGICAL_TYPE *dest_f, int ndims,
                const int *dims ) {
   cnfExpla( source_c, dest_f, ndims, dims );
}

void cnf_expn( const char *source_c, int max, char *dest_f, int dest_len ) {
   cnfExpn( source_c, max, dest_f, dest_len );
}

void cnf_exprt( const char *source_c, char *dest_f, int dest_len ) {
   cnfExprt( source_c, dest_f, dest_len );
}

void cnf_exprta( const char *source_c, int source_len, char *dest_f,
                 int dest_len, int ndims, const int *dims ) {
   cnfExprta( source_c, source_len, dest_f, dest_len, ndims, dims );
}

void cnf_exprtap( char *const *source_c, char *dest_f, int dest_len,
                  int ndims, const int *dims ) {
   cnfExprtap( source_c, dest_f, dest_len, ndims, dims );
}

void cnf_freef( F77_CHARACTER_ARG_TYPE *temp ) {
   cnfFreef( temp );
}

void cnf_impb( const char *source_f, int source_len, char *dest_c ) {
   cnfImpb( source_f, source_len, dest_c );
}

void cnf_impbn( const char *source_f, int source_len, int max, char *dest_c ) {
   cnfImpbn( source_f, source_len, max, dest_c );
}

void cnf_impch( const char *source_f, int nchars, char *dest_c ) {
   cnfImpch( source_f, nchars, dest_c );
}

void cnf_impla( const F77_LOGICAL_TYPE *source_f, int *dest_c,
                int ndims, const int *dims ) {
   cnfImpla( source_f, dest_c, ndims, dims );
}

void cnf_impn( const char *source_f, int source_len, int max, char *dest_c ) {
   cnfImpn( source_f, source_len, max, dest_c );
}

void cnf_imprt( const char *source_f, int source_len, char *dest_c ) {
   cnfImprt( source_f, source_len, dest_c );
}

void cnf_imprta( const char *source_f, int source_len, char *dest_c,
                 int dest_len, int ndims, const int *dims ) {
   cnfImprta( source_f, source_len, dest_c, dest_len, ndims, dims );
}

void cnf_imprtap( const char *source_f, int source_len, char *const *dest_c,
                  int dest_len, int ndims, const int *dims ) {
   cnfImprtap( source_f, source_len, dest_c, dest_len, ndims, dims );
}

int cnf_lenc( const char *source_c ) {
   return cnfLenc( source_c );
}

int cnf_lenf( const char *source_f, int source_len ) {
   return cnfLenf( source_f, source_len );
}

void *cnf_calloc( size_t nobj, size_t size ) {
   return cnfCalloc( nobj, size );
}

void *cnf_cptr( F77_POINTER_TYPE fpointer ) {
   return cnfCptr( fpointer );
}

F77_POINTER_TYPE cnf_fptr( void *cpointer ) {
   return cnfFptr( cpointer );
}

void cnf_free( void *pointer ) {
   cnfFree( pointer );
}

void *cnf_malloc( size_t size ) {
   return cnfMalloc( size );
}

int cnf_regp( void *cpointer ) {
   return cnfRegp( cpointer );
}

void cnf_uregp( void *cpointer ) {
   cnfUregp( cpointer );
}
