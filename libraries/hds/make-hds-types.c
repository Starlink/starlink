/*
 *+
 *  Name:
 *     make-hds-types

 *  Type of module:
 *     C source

 *  Purpose:
 *     Generate HDS type header file

 *  Description:
 *     We need to have some types made available to the public
 *     interface that are only derivable by looking at private
 *     config.h. Therefore this program reads config.h and creates
 *     a public include file defining the standard types. It also
 *     creates an internal types header file for types that are related
 *     to the public type but are most easily defined by a program rather
 *     than by CPP. Creates hds_types.h and hds1_types.h

 *  Usage:
 *     ./make-hds-types

 *  Authors:
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     DSB: David S Berry (EAO)
 *     {enter_new_authors_here}

 *  History:
 *     2005-Oct-21 (TIMJ):
 *        Original version
 *     2005-Nov-09 (PWD):
 *        Added fix for Microsoft/MINGW's handling of "long long" printfs.
 *     2005-Nov-18 (TIMJ):
 *        Use HDSLoc*
 *     2005-Nov-22 (TIMJ):
 *        Add definition of size_t via stddef.h
 *     2005-Dec-31 (TIMJ):
 *        Some tidy up of types. Add int64_t checks
 *     2006-Jan-03 (TIMJ):
 *        Protect stdint.h inclusion.
 *        Have CPP define for large hds dim
 *     2006-Jan-04 (PWD):
 *        Use inttypes.h in preference to stdint.h.
 *     2006-Jul-25 (PWD):
 *        More fixes for MINGW handling of "long long" printfs.
 *     2014-10-24 (TIMJ):
 *        Add hdsbool_t to make it easy to spot logicals in the C API.
 *     2017-Sep-12 (DSB):
 *        Switch HDS dimensions to signed 64 bit integers. They need to be
 *        signed since they will also be used for ARY/NDF array bounds, which
 *        can be negative. Another reason for using signed values is that
 *        HDS does not provide a "UK" (unsigned 64 bit int) data type, and
 *        so there would be no way to store unsigned dimension values in an
 *        HDS data file.
 *     2017-Oct-3 (DSB):
 *        - HDS dimensions should be signed, since otherwise there is no way
 *        to store them in an HDS data file, since we do not currently
 *        provide either "UI" (unsigned 32 bit int) or "UK" (unsigned 64
 *        bit int) data types in HDS.
 *        - Add a macro (HDS_DIM_TYPE) that gives the HDS data type to use
 *        for storing HDS dimensions.
 *        - Add a macro (HDSDIM_TYPE) that appends HDS_DIM_TYPE to the
 *        end of a given function name.
 *     2017-Nov-16 (DSB):
 *        Change HDS(_)DIM_TYPE to HDS(_)DIM_CODE, and add a new macro
 *        HDS_DIM_TYPE that holds the full HDS name for the dimension type.
 *     2020-Dec-11 (DSB):
 *        - Add HDSDIM_LCODE and remove unnecessary junk.
 *        - Add HDS_DIM_SIZEOF

 *  Copyright:
 *     Copyright (C) 2017-2020 East Asian Observatory
 *     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
 *     All Rights Reserved.

 *  License:
 *    This program is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program (see SLA_CONDITIONS); if not, write to the
 *    Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 *    Boston, MA  02110-1301  USA

 *-
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#if HAVE_TIME_H
# include <time.h>
#endif

#include <limits.h>
#include <inttypes.h>

#include <stdlib.h>
#include <stdio.h>
#include "f77.h"

#define INCLUDE_FILE "hds_types.h"
#define PINCLUDE_FILE "hds1_types.h"

/* Work everything out here */

/* Define a "normal integer". Typically use the "int" type, but ensure it */
/* has at least 4 bytes of precision.                                     */

#if ( ( INT_MIN <= -2147483647L ) && ( INT_MAX >= 2147483647L ) )
# define  STD_INT "int"
# define  STD_INT_FMT "d"
#else
# define  STD_INT "long int"
# define  STD_INT_FMT "ld"
#endif

/* Definitions for 64-bits integers                                        */
/* Use the standard int64_t if available                                   */
/* Use 'long' if it is 8 bytes, else use 'long long'                       */

#if HAVE_INT64_T && HAVE_UINT64_T
#define INT_BIG "int64_t"
#define UINT_BIG "uint64_t"
#define INT_BIG_S PRId64
#define INT_BIG_U PRIu64
#elif SIZEOF_LONG == 8
#define INT_BIG "long int"
#define UINT_BIG "unsigned long int"
#define INT_BIG_S "ld"
#define INT_BIG_U "lu"
#elif SIZEOF_LONG_LONG == 8
#define INT_BIG  "long long int"
#define UINT_BIG "unsigned long long int"
#if __MINGW32__
#define INT_BIG_S "I64d"
#define INT_BIG_U "I64u"
#else
#define INT_BIG_S "lld"
#define INT_BIG_U "llu"
#endif
#else
error unable to find an 8 byte integer type
#endif

/* The internal size of array dimensions within HDS can be either 32 or 64 */
/* bits. Note that 64 bits WILL change the file format!!!  */

/* Can not derive the dim size so we just set it */
/* We also state whether this is unsigned so that we can compare with
   the fortran type and also define the size. The last bit is a bit of
   a kluge to prevent sizeof("uint64_t") coming up with  9. Note, since
   HDS does not currently support a "UK" data type (i.e unsigned long
   long int), it is a bad idea to use UINT_BIG as the dimensions type as
   there is currently no suitable HDS data type for storing HDS dimension
   values within an HDS data file. Instead, use "INT_BIG" (i.e. signed)
   since then HDS dimensions can be stored in HDS data files using the
   "K" data type. */
#define BIGDIM 1   /* set to 1 if testing 64 bit dims */
#if BIGDIM
#define DIM_TYPE INT_BIG
#define HDS_DIM_CODE "K"
#define HDS_DIM_LCODE "k"
#define HDS_DIM_TYPE "_INT64"
#define SIZEOF_DIM 8
#define DIM_FORMAT INT_BIG_S
#define DIM_IS_UNSIGNED 0
#else
#define DIM_TYPE STD_INT
#define HDS_DIM_CODE "I"
#define HDS_DIM_LCODE "i"
#define HDS_DIM_TYPE "_INTEGER"
#define SIZEOF_DIM 4
#define DIM_FORMAT STD_INT_FMT
#define DIM_IS_UNSIGNED 0
#endif

/* Good grief. All I want to do is put quotes around the CNF type
   for printing */
#define myxstr(s) mystr(s)
#define mystr(s) #s

/* Simply state the Fortran type. We could choose a 64bit type here
   if we suddenly switched to INTEGER*8. Fortran will normally always
   be signed */
#define FORTRAN_HDS_INDEX_TYPE  myxstr(F77_INTEGER_TYPE)
#define FORTRAN_DIM_IS_UNSIGNED 0

/* Internal prototypes */
const char* todaysdate(void);

/* Main routine */
int main (int argc, char ** argv ) {
  char * progname = argv[0];
  FILE * OutputFile;
  FILE * POutputFile;
  int copydims;

  /* Open the public output file */
  OutputFile = fopen( INCLUDE_FILE, "w" );
  if (!OutputFile) {
    fprintf(stderr, "%s: can't open file %s for output\n",
	    progname, INCLUDE_FILE );
    return EXIT_FAILURE;
  }

  /* Open the public output file */
  POutputFile = fopen( PINCLUDE_FILE, "w" );
  if (!POutputFile) {
    fprintf(stderr, "%s: can't open file %s for output\n",
	    progname, PINCLUDE_FILE );
    return EXIT_FAILURE;
  }

  /* Start writing the include file. Begin with the header */
  fprintf(OutputFile,
"#if !defined( HDS_TYPES_INCLUDED ) /* %s already included? */\n"
"#define HDS_TYPES_INCLUDED 1\n"
"/*\n"
"*+\n"
"*  Name:\n"
"*     %s\n"
"\n"
"*  Type of Module:\n"
"*     C include file.\n"
"\n"
"*  Purpose:\n"
"*     Define public HDS-specific data types.\n"
"\n"
"*  Description:\n"
"*     This file defines the public types that are used in the HDS\n"
"*     public API.\n"
"\n"
"*  Authors:\n"
"*     TIMJ: Tim Jenness (JAC, Hawaii)\n"
"*     %s program\n"
"\n"
"*  History:\n"
"*     21-Oct-2005 (TIMJ):\n"
"*        Original version of C program (via auto-generation).\n"
"*     %s (%s):\n"
"*        Generated\n"
"*     No further changes -- do not edit this file\n"
"\n"
"*-\n"
"*/"
"\n\n",
	  INCLUDE_FILE, INCLUDE_FILE, progname, todaysdate(), progname);

  /* Start writing the private include file. Begin with the header */
  fprintf(POutputFile,
"#if !defined( HDS1_TYPES_INCLUDED ) /* %s already included? */\n"
"#define HDS1_TYPES_INCLUDED 1\n"
"/*\n"
"*+\n"
"*  Name:\n"
"*     %s\n"
"\n"
"*  Type of Module:\n"
"*     C include file.\n"
"\n"
"*  Purpose:\n"
"*     Define private but derived HDS-specific data types.\n"
"\n"
"*  Description:\n"
"*     This file defines the private types that are used in the HDS\n"
"*     build but have no reason to be public.\n"
"\n"
"*  Authors:\n"
"*     TIMJ: Tim Jenness (JAC, Hawaii)\n"
"*     %s program\n"
"\n"
"*  History:\n"
"*     21-Oct-2005 (TIMJ):\n"
"*        Original version of C program (via auto-generation).\n"
"*     %s (%s):\n"
"*        Generated\n"
"*     No further changes -- do not edit this file\n"
"\n"
"*-\n"
"*/"
"\n\n",
	  PINCLUDE_FILE, PINCLUDE_FILE, progname, todaysdate(), progname);

  /* System defines */
#if HAVE_STDDEF_H
  fprintf(POutputFile, "#include <stddef.h>\n\n" );
  fprintf(OutputFile, "#include <stddef.h>\n\n" );
#endif
#if HAVE_INTTYPES_H
  fprintf(POutputFile, "#include <inttypes.h>\n\n" );
  fprintf(OutputFile, "#include <inttypes.h>\n\n" );
#else
# if HAVE_STDINT_H
  fprintf(POutputFile, "#include <stdint.h>\n\n" );
# endif
#endif


  /* We first need to decide what we are using for a normal hdsint */

  fprintf(POutputFile, "/* Standard HDS integer type. Only used internally */\n");
  fprintf(POutputFile, "typedef %s hdsi32_t;\n\n", STD_INT );

  /* 64 bit integer type. Also internal for now */
  fprintf(POutputFile, "/* Standard HDS 64 bit integer (internal) */\n");

  fprintf(POutputFile,
	  "typedef %s hdsi64_t;\n"
	  "typedef %s hdsu64_t;\n"
	  "#define HDS_INT_BIG_S \"%s\"\n"
	  "#define HDS_INT_BIG_U \"%s\"\n\n",
	  INT_BIG, UINT_BIG, INT_BIG_S, INT_BIG_U);

  /* Make sure that int64_t is defined to something */
#if !HAVE_INT64_T
  fprintf(OutputFile, "/* standardise 64-bit integer for API */\n");
  fprintf(OutputFile, "typedef %s int64_t\n\n", INT_BIG );
#endif

  /* Note that we do not make public a true struct LOC since that would require that
     struct LCP is also made public. We simply create a struct that has the same sized
     components as an LOC and cast between them internally in HDS */

  fprintf( OutputFile,
	   "/* Public type for dealing with HDS locators */\n"
	   "/* The contents of the struct are private to HDS. The only public */\n"
	   "/* part is the HDSLoc typedef. Never use 'struct LOC' directly.   */\n"
           "/* Do not redefine if an internal definition has been made.       */\n"
           "#ifndef HDS_USE_INTERNAL_STRUCT\n"
	   "  typedef struct LOC HDSLoc;\n"
           "#endif\n"
           "\n");

   /* HDS wild card matching needs a struct but we just create a stub for now
     until we know for sure how it's going to work. Has never worked in C anyhow */
  fprintf( OutputFile,
           "/* HDS Wild card matching structure */\n"
           "/* This is a stub structure. The C API for this routine is unstable */\n"
           "typedef struct HDSWild {\n"
           "   int wild;\n"
           "} HDSWild;\n"
           "\n");

  /* Dimensions */
  fprintf( OutputFile,
           "/* Public type for specifying HDS dimensions */\n"
           "typedef %s hdsdim;\n"
           "#define HDS_DIM_FORMAT \"%s\"\n"
           "#define HDS_DIM_TYPE \"%s\"\n"
           "#define HDS_DIM_SIZEOF %d\n"
           "#define HDS_DIM_CODE %s\n"
           "#define HDS_DIM_LCODE %s\n\n",
           DIM_TYPE, DIM_FORMAT, HDS_DIM_TYPE, SIZEOF_DIM, HDS_DIM_CODE, HDS_DIM_LCODE );

  fprintf( OutputFile,
           "/* Helper macros for HDS dimensions. For instance HDSDIM_CODE(datFred)\n"
           "   expands to datFredK or datFredI, as required. */\n"
           "#define HDS_GLUE_HELPER(a,b) a##b\n"
           "#define HDS_GLUE(a,b) HDS_GLUE_HELPER(a,b)\n"
           "#define HDSDIM_CODE(a) HDS_GLUE(a,HDS_DIM_CODE)\n\n");

  fprintf( OutputFile,
           "/* Lower case version of HDSDIM_CODE. For instance HDSDIM_LCODE(datFred)\n"
           "   expands to datFredk or datFredi, as required. */\n"
           "#define HDSDIM_LCODE(a) HDS_GLUE(a,HDS_DIM_LCODE)\n\n");

  fprintf( POutputFile,
	   "/* Private types and sizes relating to dimensions */\n"
	   "typedef %s FORTRAN_INDEX_TYPE;\n"
	   "#define SIZEOF_HDSDIM %ld\n"
	   "#define HDSDIM_IS_UNSIGNED %d\n\n",
	   FORTRAN_HDS_INDEX_TYPE,
	   (long)SIZEOF_DIM, DIM_IS_UNSIGNED);


  /* Need to decide whether fortran dims need to be copied to
     C dims. This is required if either the size of signedness
     differ */

  if ( (sizeof(DIM_TYPE) == sizeof(FORTRAN_HDS_INDEX_TYPE)) &&
      (FORTRAN_DIM_IS_UNSIGNED == DIM_IS_UNSIGNED)) {
    copydims = 0;
  } else {
    copydims = 1;
  }
  fprintf(POutputFile,
	  "/* Decide whether Fortran dims should be copied element by element */\n"
	  "#define HDS_COPY_FORTRAN_DIMS %d\n\n",
	  copydims);

  /* Logical type -- the C side does not need to be the same as the Fortran
     side. */
  fprintf( OutputFile,
          "/* Public type for Logical type */\n"
          "typedef %s hdsbool_t;\n"
          "#define HDS_BOOL_FORMAT \"%s\"\n\n",
          "int", "d");

  fprintf(OutputFile,
	  "#endif /* _INCLUDED */\n\n");
  fprintf(POutputFile,
	  "#endif /* _INCLUDED */\n\n");

  fclose( OutputFile );
  fclose( POutputFile );

  return EXIT_SUCCESS;
}



/* Stolen from make-prm-par.c */
const char *todaysdate(void)
{
#if HAVE_TIME_H
    static char s[12];
    time_t t = time(0);

    strftime(s, sizeof(s), "%d-%b-%Y", localtime(&t));
    return s;
#else
    return "TODAY";
#endif
}
