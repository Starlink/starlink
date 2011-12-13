
/*
 *+
 *  Purpose:
 *     This is the fortran wrapper to the C implementation of the HLP library

 *  Language:
 *     ANSI C

 *  Description:
 *     This file contains the fortran interface to the C implementation of the
 *     HLP library. It contains wrappers for the following routines:
 *       -  hlp_help
 *       -  hlp_creh
 *       -  hlp_errmes
 *     and the default callback routines:
 *       -  hlp_nametr
 *       -  hlp_insub
 *       -  hlp_outsub

 *  Authors:
 *     Tim Jenness (JAC, Hawaii)

 *  History:
 *     30-DEC-2005 (TIMJ):
 *       Original Version
 *     03-JAN-2006 (TIMJ):
 *       Add callbacks.

 *  Notes:
 *     - Only the public interface as specified in SUN/124 is included.
 *     - The hlp_insub and hlp_outsub routines are stub functions that
 *       should not be called directly. The wrapper for hlp_help determines
 *       whether the C default callback should be called directly without
 *       going through the fortran overhead.
 *     - hlp_nametr can be called directly.

 *  Copyright:
 *     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of
 *     the License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public
 *     License along with this program; if not, write to the Free
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA

 *-
*/

#include <string.h>
#include <stdio.h>

#include "f77.h"
#include "help.h"
#include "help1.h" /* Error constants */

/* Default Fortran STDIN unit */
#define F77UNIT 6

/* Global variables for the fortran function pointers that must be
   used from the C callbacks. This is not thread-safe as written.
   Mainly because Fortran itself is not thread-safe so the extra
   effort to make the fortran interface thread-safe is not warranted
   (and HLP is not thread-safe).
 */

static void (*gl_nametr)( INTEGER(kmd), CHARACTER(instr), CHARACTER(outstr),
		       INTEGER(jstat) TRAIL(instr) TRAIL(outstr) );
static F77_INTEGER_TYPE (*gl_outsub)( CHARACTER(outstr) TRAIL(outstr) );
static F77_INTEGER_TYPE (*gl_insub)( CHARACTER(string), CHARACTER(prompt),
				  INTEGER(l) TRAIL(string) TRAIL(prompt) );

/* These are the C default callbacks with a Fortran interface */

F77_SUBROUTINE(hlp_nametr)( INTEGER(kmd), CHARACTER(instr), CHARACTER(outstr),
			    INTEGER(jstat) TRAIL(instr) TRAIL(outstr) );
F77_INTEGER_FUNCTION(hlp_outsub)( CHARACTER(outstr) TRAIL(outstr) );
F77_INTEGER_FUNCTION(hlp_insub)( CHARACTER(string), CHARACTER(prompt),
				 INTEGER(l) TRAIL(string) TRAIL(prompt) );


/* These are the C callback routines that have to be registered with
   hlpHelp and hlpCreh. They translate the arguments into fortran
   style and call the fortran callbacks */

/* Prototypes */
int nametr_c ( int, char*, int, char * );
int outsub_c ( char * );
int insub_c ( char *, char *, int * );


/* Subroutine to Process the output data callback */
int
outsub_c( char * outstr ) {

  DECLARE_CHARACTER_DYN(OUTSTR);
  size_t lenstr;
  int jstat;

  if (*gl_outsub == NULL) return hlp_INTERNAL_ERROR;

  /* Calculate the length of the string */
  lenstr = strlen( outstr );

  /* Copy pointers and store the length - really need a F77_COPY_CHARACTER macro */
  OUTSTR = outstr;
  OUTSTR_length = lenstr;

  /* Call the fortran routine, include the length of the HLP */
  jstat = (*gl_outsub)( CHARACTER_ARG(OUTSTR) TRAIL_ARG(OUTSTR) );
  return jstat;
}

/* Subroutine to process the input data callback */
/* "string" will be at least 80 characters */
int
insub_c( char * string, char * prompt, int * l ) {

  char string_f[80];
  size_t lprompt;
  int jstat;

  if (*gl_insub == NULL) return hlp_INTERNAL_ERROR;

  /* Find the length of the prompt */
  lprompt = strlen( prompt );

  /* Call fortran version - assume trail args */
  jstat = (*gl_insub)( string_f, prompt, l, 80, lprompt );

  /* Terminate */
  if ( jstat == 1 ) {
    cnfImprt( string_f, 80, string );
  } else {
    *string = '\0';
  }

  return jstat;
}

/* Name translation routine */

int
nametr_c( int kmd, char * strin, int lstrout, char * strout ) {
  size_t lstrin;
  int jstat;

  if (*gl_nametr == NULL) return hlp_INTERNAL_ERROR;

  /* Length of string */
  lstrin = strlen( strin );

  /* Need to make sure we leave space for the null */
  (*gl_nametr)( &kmd, strin, strout, &jstat, lstrin, lstrout - 1 );

  /* Terminate the result */
  if ( jstat == 0 || jstat == hlp_STRING_OVERFLOW ) {
    cnfImprt( strout, lstrout - 1, strout );
  } else {
    *strout = '\0';
  }

  return jstat;
}


/*  P U B L I C    F O R T R A N  A P I */

/* Convert error codes to error messages */

F77_SUBROUTINE(hlp_errmes)( INTEGER(j), CHARACTER(mes) TRAIL(mes) )
{
  char * errmess;
  errmess = hlpErrmes( *j );
  cnfExprt( errmess, mes, mes_length );
}

/* Interactive help - takes a pointer for function callbacks *NOT* a
   "fortran pointer", which may be an INTEGER */

/* Note that "lu" is meaningless since we are doing unix i/o
   so a fortran unit number to use for OPEN is not required */

F77_INTEGER_FUNCTION(hlp_help)( int (*outsub) (  CHARACTER(outstr) TRAIL(outstr) ),
				INTEGER(lout),
				CHARACTER(inline_f), /* "inline" clashes with gcc */
				INTEGER(lu),
				CHARACTER(lib),
				INTEGER(jflags),
				F77_INTEGER_TYPE (*insub) ( CHARACTER(string), CHARACTER(prompt),
							 INTEGER(l) TRAIL(string) TRAIL(prompt) ),
				void (*nametr) ( INTEGER(kmd), CHARACTER(instr), CHARACTER(outstr),
					     INTEGER(jstat) TRAIL(instr) TRAIL(outstr) )
				TRAIL(inline_f) TRAIL(lib) )
{
  /* Local variables */
  char * inline_c;
  char * lib_c;
  int jstat;

  /* Local callback variables */
  int ( *lnametr_c ) ( int, char*, int, char* );
  int ( *loutsub_c ) ( char* );
  int ( *linsub_c )( char*, char*, int* );

  /* Import the strings and terminate them */
  inline_c = cnfCreim( inline_f, inline_f_length );
  lib_c = cnfCreim( lib, lib_length );

  /* Store the function pointers in the globals but check to see if we can
   just use the C callbacks directly */
  if ( nametr != &F77_EXTERNAL_NAME(hlp_nametr) ) {
    gl_nametr = nametr;
    lnametr_c = &nametr_c;
  } else {
    lnametr_c = &hlpNametr;
  }
  if ( outsub != &F77_EXTERNAL_NAME(hlp_outsub) ) {
    gl_outsub = outsub;
    loutsub_c = &outsub_c;
  } else {
    loutsub_c = &hlpOutsub;
  }
  if ( insub != &F77_EXTERNAL_NAME(hlp_insub) ) {
    gl_insub = insub;
    linsub_c = &insub_c;
  } else {
    linsub_c = &hlpInsub;
  }

  /* Call C version - always call same C callbacks */
  jstat = hlpHelp( loutsub_c, *lout, inline_c, lib_c, *jflags, linsub_c, lnametr_c );

  /* Free temp storage */
  cnfFree( inline_c );
  cnfFree( lib_c );

  /* Reset the function pointers */
  gl_nametr = NULL;
  gl_insub = NULL;
  gl_outsub = NULL;

  /* Status return */
  return (F77_INTEGER_TYPE)jstat;
}

/* hlp_creh - translate a help file */

F77_SUBROUTINE(hlp_creh)( void (*nametr) ( INTEGER(kmd), CHARACTER(instr), CHARACTER(outstr),
					   INTEGER(jstat) TRAIL(instr) TRAIL(outstr) ),
			  INTEGER(luin), /* unused */
			  CHARACTER(source),
			  INTEGER(luout), /* unused */
			  CHARACTER(lib),
			  INTEGER(luerr), /* unused - stderr */
			  CHARACTER(eos), /* unused */
			  INTEGER(jstat)
			  TRAIL(source)
			  TRAIL(lib)
			  TRAIL(eos) )
{

  /* Local variables */
  char * source_c;
  char * lib_c;

  /* Local callback variables */
  int ( *lnametr_c ) ( int, char*, int, char* );

  /* Import the strings */
  lib_c = cnfCreim( lib, lib_length );
  source_c = cnfCreim( source, source_length );

  /* Store the function pointers in the globals but check to see if we can
   just use the C callbacks directly */
  if ( nametr != &F77_EXTERNAL_NAME(hlp_nametr) ) {
    gl_nametr = nametr;
    lnametr_c = &nametr_c;
  } else {
    lnametr_c = &hlpNametr;
  }

  /* Call the C function */
  *jstat = (F77_INTEGER_TYPE)hlpCreh( lnametr_c, source_c, lib_c );

  /* Free temp storage */
  cnfFree( source_c );
  cnfFree( lib_c );

  /* Reset the function pointers */
  gl_nametr = NULL;

  return;
}




/* Need to include the "standard" callback functions *BUT* we only need to provide
   a function stub since things are more efficient all around if the standard C callbacks
   are called directly from C rather than through the fortran wrapper */

F77_INTEGER_FUNCTION(hlp_outsub)( CHARACTER(outstr) TRAIL(outstr) ) {
  fprintf(stderr,"Do NOT call hlp_outsub directly. It must only be called from hlp_help\n");
  return 0;
}

F77_INTEGER_FUNCTION(hlp_insub)( CHARACTER(string), CHARACTER(prompt),
				 INTEGER(l) TRAIL(string) TRAIL(prompt) ) {
  fprintf(stderr,"Do NOT call hlp_insub directly. It must only be called from hlp_help\n");
  return 0;
}

/* Unfortunately, hlp_nametr seems to be part of the public interface */

F77_SUBROUTINE(hlp_nametr)( INTEGER(kmd), CHARACTER(instr), CHARACTER(outstr),
			    INTEGER(jstat) TRAIL(instr) TRAIL(outstr) ) {

  char * instr_c;

  /* Import the string and terminate */
  instr_c = cnfCreim( instr, instr_length );
  printf("Calling nametr with %d and '%s'\n", *kmd, instr_c);

  /* Call local routine */
  *jstat = (F77_INTEGER_TYPE)hlpNametr( *kmd, instr_c, outstr_length, outstr );

  /* Remove termination */
  cnfExprt( outstr, outstr, outstr_length );

  /* Tidy */
  cnfFree( instr_c );

  return;
}
