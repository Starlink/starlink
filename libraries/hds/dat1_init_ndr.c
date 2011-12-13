#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <limits.h>
#include <float.h>
#include <stdio.h>

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */
#include "ems.h"                 /* ems_ public definitions                 */
#include "ems_par.h"             /* ems__ constants                         */

   void dat1_init_ndr( int *status )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    dat1_init_ndr                                                         */

/* Purpose:                                                                 */
/*    Initialise the global native data representation structure.           */

/* Invocation:                                                              */
/*    dat1_init_ndr( status )                                               */

/* Description:                                                             */
/*    This function initialises the global native data representation       */
/*    structure which holds information about the way primitive values are  */
/*    stored on the current host machine.                                   */

/* Parameters:                                                              */
/*    int *status                                                           */
/*       The inherited global status.                                       */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Notes:                                                                   */
/*    -  This routine makes considerable use of macros because it must      */
/*    apply similar processing to a range of arithmetic data types whose    */
/*    characteristics are not known directly (typically they are themselves */
/*    parameterised via the HDS primitive type macros). The macros used     */
/*    here appear, and are commented, as if they formed part of the normal  */
/*    executable code.                                                      */
/*    -  Although this routine works correctly on a renge of known          */
/*    compilers with the maximum level of optimisation enabled, it is       */
/*    always possible that a particularly awkward compiler may give         */
/*    problems. If this routine inexplicably fails to recognise the machine */
/*    data representation, then a reduction in the level of compiler        */
/*    optimisation may help.                                                */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */
/*    Copyright (C) 2006 Particle Physics and Engineering Research Council  */

/*  Licence:                                                                */
/*     This program is free software; you can redistribute it and/or        */
/*     modify it under the terms of the GNU General Public License as       */
/*     published by the Free Software Foundation; either version 2 of       */
/*     the License, or (at your option) any later version.                  */

/*     This program is distributed in the hope that it will be              */
/*     useful, but WITHOUT ANY WARRANTY; without even the implied           */
/*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR              */
/*     PURPOSE. See the GNU General Public License for more details.        */

/*     You should have received a copy of the GNU General Public            */
/*     License along with this program; if not, write to the Free           */
/*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,       */
/*     MA 02110-1301, USA                                                   */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    3-SEP-1992 (RFWS):                                                    */
/*       Original version.                                                  */
/*    5-OCT-1992 (RFWS):                                                    */
/*       Modified to initialise the number of decimal digits of precision.  */
/*    22-DEC-1992 (RFWS):                                                   */
/*       Added the _specialbits macro to avoid problems with integer        */
/*       overflow reported by some compilers when setting up special bit    */
/*       patterns in signed integer data types. Also added dat1_decoy calls */
/*       to prevent unwanted compiler optimisation.                         */
/*    03-JAN-2005 (TIMJ):                                                   */
/*       Turn on 'long double' code via autoconf tests                      */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local constants:                                                         */
#define BUFSIZE 100              /* Size of text buffers for formatting     */

/* Local Data Types:                                                        */
      enum TYPID {               /* Code to identify arithmetic data types  */
         SCHARTYPE,
         UCHARTYPE,
         SHRTTYPE,
         USHRTTYPE,
         INTTYPE,
         UINTTYPE,
         LONGTYPE,
         ULONGTYPE,
         FLTTYPE,
         DBLTYPE,
         LDBLTYPE
      };
      union TYP {                /* Union for holding any C arithmetic type */
#if HAVE_SIGNED_CHAR
         signed char scharval;
#else
         char scharval;
#endif
         unsigned char ucharval;
         short int shrtval;
         unsigned short int ushrtval;
         int intval;
         unsigned int uintval;
         long int longval;
         unsigned long int ulongval;
         float fltval;
         double dblval;
#if HAVE_LONG_DOUBLE
         long double ldblval;
#endif
      };

/* Local Variables:                                                         */
      _DOUBLE eps_DOUBLE;        /* Smallest increment for _DOUBLE type     */
      _REAL eps_REAL;            /* Smallest increment for _REAL type       */
      int i;                     /* Loop counter for bytes                  */
      unsigned char *ptr;        /* Pointer to byte sequence                */
      unsigned char IEEE_D_dig;  /* No. digits for IEEE_D numbers           */
      unsigned char IEEE_S_dig;  /* No. digits for IEEE_S numbers           */
      unsigned char VAXD_dig;    /* No. digits for VAXD numbers             */
      unsigned char VAXF_dig;    /* No. digits for VAXF numbers             */
      unsigned char dbl_dig;     /* No. digits for double numbers           */
      unsigned char dbl_format;  /* Data format code for double values      */
#if HAVE_LONG_DOUBLE
      unsigned char ldbl_dig;    /* No. digits for long double numbers      */
      unsigned char ldbl_format; /* Data format code for long double values */
#endif
      unsigned char dig_DOUBLE;  /* No. digits for _DOUBLE numbers          */
      unsigned char dig_REAL;    /* No. digits for _REAL numbers            */
      unsigned char flt_dig;     /* No. digits for float numbers            */
      unsigned char flt_format;  /* Data format code for float values       */
      unsigned char format_DOUBLE; /* Data format code for _DOUBLE values   */
      unsigned char format_REAL; /* Data format code for _REAL values       */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( *status ) ) return;

/* Initialisations to stop optimized compiler warnings                      */
      format_REAL = 0;
      format_DOUBLE = 0;
      dig_REAL = 0;
      dig_DOUBLE = 0;
      eps_REAL = 0.0;
      eps_DOUBLE = 0.0;


/* Set up the name of each primitive data type.                             */
/* ===========================================                              */
      dat_gl_ndr[ DAT__C ].name =  "_CHAR";
      dat_gl_ndr[ DAT__L ].name =  "_LOGICAL";
      dat_gl_ndr[ DAT__B ].name =  "_BYTE";
      dat_gl_ndr[ DAT__UB ].name = "_UBYTE";
      dat_gl_ndr[ DAT__W ].name =  "_WORD";
      dat_gl_ndr[ DAT__UW ].name = "_UWORD";
      dat_gl_ndr[ DAT__I ].name =  "_INTEGER";
      dat_gl_ndr[ DAT__R ].name =  "_REAL";
      dat_gl_ndr[ DAT__D ].name =  "_DOUBLE";

/* Set up the length of each primitive data type.                           */
/* =============================================                            */
      dat_gl_ndr[ DAT__C ].length =  sizeof( _CHAR );
      dat_gl_ndr[ DAT__L ].length =  sizeof( _LOGICAL );
      dat_gl_ndr[ DAT__B ].length =  sizeof( _BYTE );
      dat_gl_ndr[ DAT__UB ].length = sizeof( _UBYTE );
      dat_gl_ndr[ DAT__W ].length =  sizeof( _WORD );
      dat_gl_ndr[ DAT__UW ].length = sizeof( _UWORD );
      dat_gl_ndr[ DAT__I ].length =  sizeof( _INTEGER );
      dat_gl_ndr[ DAT__R ].length =  sizeof( _REAL );
      dat_gl_ndr[ DAT__D ].length =  sizeof( _DOUBLE );

/* Identify the format used for each C floating-point type.                 */
/* =======================================================                  */
/* Use the ANSI C floating point description in <float.h> to distinguish    */
/* the important formats. Note that this must be done at run-time, because  */
/* ANSI does not guarantee that these values are suitable for use in        */
/* pre-processor directives.                                                */

/* Note that the objective here is simply to distinguish the currently      */
/* supported floating-point types. There is no guarantee that we can also   */
/* distinguish these from new types not yet encountered (although in        */
/* practice this may happen). If new formats are to be supported, then new  */
/* format codes must be assigned (in dat1.h), new tests installed here, and */
/* new format-conversion algorithms provided.                               */

/* Set the number of decimal digits of precision for each floating point    */
/* type.                                                                    */
      IEEE_S_dig = 9;            /* Value given in IEEE standard            */
      IEEE_D_dig = 17;           /* Value given in IEEE standard            */
      VAXF_dig = 9;
      VAXD_dig = 18;

/* Identify the float format:                                               */
      if ( ( FLT_RADIX == 2 ) &&
           ( FLT_MANT_DIG == 24 ) &&
           ( FLT_MIN_EXP == -127 ) &&
           ( FLT_MAX_EXP == 127 ) )
      {
         flt_format = DAT__VAXF; /* VAX F-floating (single-precision)       */
         flt_dig = VAXF_dig;
      }

      else if ( ( FLT_RADIX == 2 ) &&
                ( FLT_MANT_DIG == 56 ) &&
                ( FLT_MIN_EXP == -127 ) &&
                ( FLT_MAX_EXP == 127 ) )
      {
         flt_format = DAT__VAXD; /* VAX D-floating (double precision)       */
         flt_dig = VAXD_dig;
      }

      else if ( ( FLT_RADIX == 2 ) &&
                ( FLT_MANT_DIG == 24 ) &&
                ( FLT_MIN_EXP == -125 ) &&
                ( FLT_MAX_EXP == 128 ) )
      {
         flt_format = DAT__IEEE_S; /* IEEE standard single precision        */
         flt_dig = IEEE_S_dig;
      }

      else if ( ( FLT_RADIX == 2 ) &&
                ( FLT_MANT_DIG == 53 ) &&
                ( FLT_MIN_EXP == -1021 ) &&
                ( FLT_MAX_EXP == 1024 ) )
      {
         flt_format = DAT__IEEE_D; /* IEEE standard double precision        */
         flt_dig = IEEE_D_dig;
      }

/* If the format is not recognised, note this fact, but do not report an    */
/* error yet since it may not actually be used as a primitive HDS data      */
/* type.                                                                    */
      else
      {
         flt_format = DAT__UNKNOWN;
         flt_dig = DAT__UNKNOWN;
      }

/* Identify the double format:                                              */
      if ( ( FLT_RADIX == 2 ) &&
           ( DBL_MANT_DIG == 24 ) &&
           ( DBL_MIN_EXP == -127 ) &&
           ( DBL_MAX_EXP == 127 ) )
      {
         dbl_format = DAT__VAXF; /* VAX F-floating (single-precision)       */
         dbl_dig = VAXF_dig;
      }

      else if ( ( FLT_RADIX == 2 ) &&
                ( DBL_MANT_DIG == 56 ) &&
                ( DBL_MIN_EXP == -127 ) &&
                ( DBL_MAX_EXP == 127 ) )
      {
         dbl_format = DAT__VAXD; /* VAX D-floating (double precision)       */
         dbl_dig = VAXD_dig;
      }

      else if ( ( FLT_RADIX == 2 ) &&
                ( DBL_MANT_DIG == 24 ) &&
                ( DBL_MIN_EXP == -125 ) &&
                ( DBL_MAX_EXP == 128 ) )
      {
         dbl_format = DAT__IEEE_S; /* IEEE standard single precision        */
         dbl_dig = IEEE_S_dig;
      }

      else if ( ( FLT_RADIX == 2 ) &&
                ( DBL_MANT_DIG == 53 ) &&
                ( DBL_MIN_EXP == -1021 ) &&
                ( DBL_MAX_EXP == 1024 ) )
      {
         dbl_format = DAT__IEEE_D; /* IEEE standard double precision        */
         dbl_dig = IEEE_D_dig;
      }

/* If the format is not recognised, note this fact, but do not report an    */
/* error yet since it may not actually be used as a primitive HDS data      */
/* type.                                                                    */
      else
      {
         dbl_format = DAT__UNKNOWN;
         dbl_dig = DAT__UNKNOWN;
      }

#if HAVE_LONG_DOUBLE
/* Identify the long double format:                                         */
      if ( ( FLT_RADIX == 2 ) &&
           ( LDBL_MANT_DIG == 24 ) &&
           ( LDBL_MIN_EXP == -127 ) &&
           ( LDBL_MAX_EXP == 127 ) )
      {
         ldbl_format = DAT__VAXF; /* VAX F-floating (single-precision)      */
         ldbl_dig = VAXF_dig;
      }

      else if ( ( FLT_RADIX == 2 ) &&
                ( LDBL_MANT_DIG == 56 ) &&
                ( LDBL_MIN_EXP == -127 ) &&
                ( LDBL_MAX_EXP == 127 ) )
      {
         ldbl_format = DAT__VAXD; /* VAX D-floating (double precision)      */
         ldbl_dig = VAXD_dig;
      }

      else if ( ( FLT_RADIX == 2 ) &&
                ( LDBL_MANT_DIG == 24 ) &&
                ( LDBL_MIN_EXP == -125 ) &&
                ( LDBL_MAX_EXP == 128 ) )
      {
         ldbl_format = DAT__IEEE_S; /* IEEE standard single precision       */
         ldbl_dig = IEEE_S_dig;
      }

      else if ( ( FLT_RADIX == 2 ) &&
                ( LDBL_MANT_DIG == 53 ) &&
                ( LDBL_MIN_EXP == -1021 ) &&
                ( LDBL_MAX_EXP == 1024 ) )
      {
         ldbl_format = DAT__IEEE_D; /* IEEE standard double precision       */
         ldbl_dig = IEEE_D_dig;
      }

/* If the format is not recognised, note this fact, but do not report an    */
/* error yet since it may not actually be used as a primitive HDS data      */
/* type.                                                                    */
      else
      {
         ldbl_format = DAT__UNKNOWN;
         ldbl_dig = DAT__UNKNOWN;
      }
#endif

/* Set lower and upper limits for the primitive data types.                 */
/* =======================================================                  */
/* The _CHAR and _LOGICAL types do not have limits - just set them to zero. */
      dat_gl_ndr[ DAT__C ].min.C = (_CHAR) 0;
      dat_gl_ndr[ DAT__C ].max.C = (_CHAR) 0;
      dat_gl_ndr[ DAT__L ].min.L = (_LOGICAL) 0;
      dat_gl_ndr[ DAT__L ].max.L = (_LOGICAL) 0;

/* The remaining data types will have their limits specified in <limits.h>  */
/* or <float.h>, but we first have to identify which limits belong with     */
/* which HDS primitive type.                                                */

/* Define a macro to determine whether an integer type is signed or not,    */
/* returning 0 or 1.                                                        */
#define _signed( dtype ) ( ( (dtype) -1 ) < ( (dtype) 0 ) )

/* Define a macro to classify an integer type and to assign appropriate     */
/* limits.                                                                  */
#define _limitsi( dtype, min, max )\
      {\
         enum TYPID id;          /* Code to identify data type              */\
         union TYP hi;           /* Temporary storage for upper limit       */\
         union TYP lo;           /* Temporary storage for lower limit       */\
\
/* Match the size and signed-ness of the integer data type against each     */\
/* possibility.                                                             */\
         if ( _ok( *status ) )\
         {\
            if ( ( sizeof( dtype ) == sizeof( char ) ) &&\
                   _signed( dtype ) )\
            {\
\
/* When a match is found, set a code to identify it and obtain the lower    */\
/* and upper bounds for that C data type, setting these in the appropriate  */\
/* element of the unions lo and hi.                                         */\
               id = SCHARTYPE;\
               lo.scharval = SCHAR_MIN;\
               hi.scharval = SCHAR_MAX;\
            }\
            else if ( ( sizeof( dtype ) == sizeof( unsigned char ) ) &&\
                        !_signed( dtype ) )\
            {\
               id = UCHARTYPE;\
               lo.ucharval = (unsigned char) 0;\
               hi.ucharval = UCHAR_MAX;\
            }\
            else if ( ( sizeof( dtype ) == sizeof( short int ) ) &&\
                        _signed( dtype ) )\
            {\
               id = SHRTTYPE;\
               lo.shrtval = SHRT_MIN;\
               hi.shrtval = SHRT_MAX;\
            }\
            else if ( ( sizeof( dtype ) == sizeof( unsigned short int ) ) &&\
                        !_signed( dtype ) )\
            {\
               id = USHRTTYPE;\
               lo.ushrtval = (unsigned short int) 0;\
               hi.ushrtval = USHRT_MAX;\
            }\
            else if ( ( sizeof( dtype ) == sizeof( int ) ) &&\
                        _signed( dtype ) )\
            {\
               id = INTTYPE;\
               lo.intval = INT_MIN;\
               hi.intval = INT_MAX;\
            }\
            else if ( ( sizeof( dtype ) == sizeof( unsigned int ) ) &&\
                        !_signed( dtype ) )\
            {\
               id = UINTTYPE;\
               lo.uintval = (unsigned int) 0;\
               hi.uintval = UINT_MAX;\
            }\
            else if ( ( sizeof( dtype ) == sizeof( long int ) ) &&\
                        _signed( dtype ) )\
            {\
               id = LONGTYPE;\
               lo.longval = LONG_MIN;\
               hi.longval = LONG_MAX;\
            }\
            else if ( ( sizeof( dtype ) == sizeof( unsigned long int ) ) &&\
                        !_signed( dtype ) )\
            {\
               id = ULONGTYPE;\
               lo.ulongval = (unsigned long int) 0;\
               hi.ulongval = ULONG_MAX;\
            }\
\
/* If a primitive data type cannot be identified, then report an error.     */\
            else\
            {\
               *status = DAT__FATAL;\
               emsRep( "DAT1_INIT_NDR_1",\
                          "Unable to identify an integer data type; HDS may \
require modification for use on this machine.",\
                          status );\
            }\
         }\
\
/* If the data type has been identified, make a decoy call so defeat        */\
/* optimisation. Then test the type identification code and move the limits */\
/* obtained to the required locations. (Note this intermediate stage of     */\
/* storage is solely to defeat compilers which are too clever; without it   */\
/* they may try to assign the constant limit values at compile-time. This   */\
/* can cause overflow and prevent compilation.)                             */\
         if ( _ok( *status ) )\
         {\
            dat1_decoy( (int) id, (void *) &id );\
            switch( id )\
            {\
               case SCHARTYPE:\
               {\
                  (min) = (dtype) lo.scharval;\
                  (max) = (dtype) hi.scharval;\
                  break;\
               }\
               case UCHARTYPE:\
               {\
                  (min) = (dtype) lo.ucharval;\
                  (max) = (dtype) hi.ucharval;\
                  break;\
               }\
               case SHRTTYPE:\
               {\
                  (min) = (dtype) lo.shrtval;\
                  (max) = (dtype) hi.shrtval;\
                  break;\
               }\
               case USHRTTYPE:\
               {\
                  (min) = (dtype) lo.ushrtval;\
                  (max) = (dtype) hi.ushrtval;\
                  break;\
               }\
               case INTTYPE:\
               {\
                  (min) = (dtype) lo.intval;\
                  (max) = (dtype) hi.intval;\
                  break;\
               }\
               case UINTTYPE:\
               {\
                  (min) = (dtype) lo.uintval;\
                  (max) = (dtype) hi.uintval;\
                  break;\
               }\
               case LONGTYPE:\
               {\
                  (min) = (dtype) lo.longval;\
                  (max) = (dtype) hi.longval;\
                  break;\
               }\
               case ULONGTYPE:\
               {\
                  (min) = (dtype) lo.ulongval;\
                  (max) = (dtype) hi.ulongval;\
                  break;\
               }\
	    default:\
	      {\
               *status = DAT__FATAL;\
               emsRep( "DAT1_INIT_NDR_1b",\
                          "Fell off end of switch statement unexpectedly when determining integer data type",\
                          status );\
	      }\
            }\
         }\
      }

/* Define a similar macro to classify a floating-point type and to assign   */
/* appropriate limits. In this case, we also require values for the         */
/* smallest increment value, the number of decimal digits of precision and  */
/* the data format code.                                                    */
#if HAVE_LONG_DOUBLE
#define _limitsf( dtype, min, max, eps, dig, format )\
      {\
         enum TYPID id;          /* Code to identify data type              */\
         union TYP hi;           /* Temporary storage for upper limit       */\
         union TYP lo;           /* Temporary storage for lower limit       */\
         union TYP tiny;         /* Temporary storage for tinyest increment */\
\
/* Match the size of the floating-point data type against each possibility. */\
         if ( _ok( *status ) )\
         {\
            if ( sizeof( dtype ) == sizeof( float ) )\
            {\
\
/* When a match is found, set a code to identify it and obtain the lower    */\
/* and upper bounds for that C data type, setting these in the appropriate  */\
/* element of the unions lo and hi. Also set a value for the smallest       */\
/* increment in the tiny union.                                             */\
               id = FLTTYPE;\
               lo.fltval = - FLT_MAX;\
               hi.fltval = FLT_MAX;\
               tiny.fltval = FLT_EPSILON;\
            }\
            else if ( sizeof( dtype ) == sizeof( double ) )\
            {\
               id = DBLTYPE;\
               lo.dblval = - DBL_MAX;\
               hi.dblval = DBL_MAX;\
               tiny.dblval = DBL_EPSILON;\
            }\
            else if ( sizeof( dtype ) == sizeof( long double ) )\
            {\
               id = LDBLTYPE;\
               lo.ldblval = - LDBL_MAX;\
               hi.ldblval = LDBL_MAX;\
               tiny.ldblval = LDBL_EPSILON;\
            }\
\
/* If a primitive data type cannot be identified, then report an error.     */\
            else\
            {\
               *status = DAT__FATAL;\
               emsRep( "DAT1_INIT_NDR_2",\
                          "Unable to identify a floating-point data type; HDS \
may require modification for use on this machine.",\
                          status );\
            }\
         }\
\
/* If the data type has been identified, make a decoy call to defeat        */\
/* optimisation.  Then test the type identification code and move the       */\
/* limits obtained to the required locations. (Note this intermediate stage */\
/* of storage is solely to defeat compilers which are too clever; without   */\
/* it they may try to assign the constant limit values at compile-time.     */\
/* This can cause overflow and prevent compilation.)                        */\
         if ( _ok( *status ) )\
         {\
            dat1_decoy( (int) id, (void *) &id );\
            switch( id )\
            {\
               case FLTTYPE:\
               {\
                  (min) = (dtype) lo.fltval;\
                  (max) = (dtype) hi.fltval;\
                  (eps) = (dtype) tiny.fltval;\
\
/* Also set the number of decimal digits of precision and the data format   */\
/* code appropriate to this data type (as determined earlier).              */\
                  (dig) = flt_dig;\
                  (format) = flt_format;\
                  break;\
               }\
               case DBLTYPE:\
               {\
                  (min) = (dtype) lo.dblval;\
                  (max) = (dtype) hi.dblval;\
                  (eps) = (dtype) tiny.dblval;\
                  (dig) = dbl_dig;\
                  (format) = dbl_format;\
                  break;\
               }\
               case LDBLTYPE:\
               {\
                  (min) = (dtype) lo.ldblval;\
                  (max) = (dtype) hi.ldblval;\
                  (eps) = (dtype) tiny.ldblval;\
                  (dig) = ldbl_dig;\
                  (format) = ldbl_format;\
                  break;\
               }\
	    default:\
	      {\
               *status = DAT__FATAL;\
               emsRep( "DAT1_INIT_NDR_2b",\
                          "Fell off end of switch statement unexpectedly when determining float data type",\
                          status );\
	      }\
            }\
         }\
      }
#else
/*  Don't use long double if it is not supported by the compiler.           */
#define _limitsf( dtype, min, max, eps, dig, format )\
      {\
         enum TYPID id;          /* Code to identify data type              */\
         union TYP hi;           /* Temporary storage for upper limit       */\
         union TYP lo;           /* Temporary storage for lower limit       */\
         union TYP tiny;         /* Temporary storage for tinyest increment */\
         if ( _ok( *status ) )\
         {\
            if ( sizeof( dtype ) == sizeof( float ) )\
            {\
               id = FLTTYPE;\
               lo.fltval = - FLT_MAX;\
               hi.fltval = FLT_MAX;\
               tiny.fltval = FLT_EPSILON;\
            }\
            else if ( sizeof( dtype ) == sizeof( double ) )\
            {\
               id = DBLTYPE;\
               lo.dblval = - DBL_MAX;\
               hi.dblval = DBL_MAX;\
               tiny.dblval = DBL_EPSILON;\
            }\
            else\
            {\
               *status = DAT__FATAL;\
               emsRep( "DAT1_INIT_NDR_3",\
                          "Unable to identify a floating-point data type; HDS \
may require modification for use on this machine.",\
                          status );\
            }\
         }\
         if ( _ok( *status ) )\
         {\
            dat1_decoy( (int) id, (void *) &id );\
            switch( id )\
            {\
               case FLTTYPE:\
               {\
                  (min) = (dtype) lo.fltval;\
                  (max) = (dtype) hi.fltval;\
                  (eps) = (dtype) tiny.fltval;\
                  (dig) = flt_dig;\
                  (format) = flt_format;\
                  break;\
               }\
               case DBLTYPE:\
               {\
                  (min) = (dtype) lo.dblval;\
                  (max) = (dtype) hi.dblval;\
                  (eps) = (dtype) tiny.dblval;\
                  (dig) = dbl_dig;\
                  (format) = dbl_format;\
                  break;\
               }\
	    default:\
	      {\
               *status = DAT__FATAL;\
               emsRep( "DAT1_INIT_NDR_2b",\
                          "Fell off end of switch statement unexpectedly when determining float data type",\
                          status );\
	      }\
            }\
         }\
      }
#endif

/* Invoke these macros to assign limits to each of the remaining primitive  */
/* data types.                                                              */
      _limitsi( _BYTE,
                dat_gl_ndr[ DAT__B ].min.B,
                dat_gl_ndr[ DAT__B ].max.B )
      _limitsi( _UBYTE,
                dat_gl_ndr[ DAT__UB ].min.UB,
                dat_gl_ndr[ DAT__UB ].max.UB )
      _limitsi( _WORD,
                dat_gl_ndr[ DAT__W ].min.W,
                dat_gl_ndr[ DAT__W ].max.W )
      _limitsi( _UWORD,
                dat_gl_ndr[ DAT__UW ].min.UW,
                dat_gl_ndr[ DAT__UW ].max.UW )
      _limitsi( _INTEGER,
                dat_gl_ndr[ DAT__I ].min.I,
                dat_gl_ndr[ DAT__I ].max.I )
      _limitsf( _REAL,
                dat_gl_ndr[ DAT__R ].min.R,
                dat_gl_ndr[ DAT__R ].max.R,
                eps_REAL, dig_REAL, format_REAL )
      _limitsf( _DOUBLE,
                dat_gl_ndr[ DAT__D ].min.D,
                dat_gl_ndr[ DAT__D ].max.D,
                eps_DOUBLE, dig_DOUBLE, format_DOUBLE )

/* Assign the "bad" value for each data type.                               */
/* =========================================                                */
/* A '*' is always used for the bad _CHAR value.                            */
      if ( _ok( *status ) )
      {
         dat_gl_ndr[ DAT__C ].bad.C = (_CHAR) '*';

/* The bad _LOGICAL value is set to an alternating sequence of zero and one */
/* bits which is unlikely to occur by accident. It is also made             */
/* palindromic, so its value does not alter with byte reversal.             */
         ptr = (unsigned char *) &( dat_gl_ndr[ DAT__L ].bad.L  );
         for ( i = 0; i < ( ( sizeof( _LOGICAL ) + 1 ) / 2 ); i++ )
         {
            ptr[ i ] = (unsigned char) ( ( i % 2 ) ? 0x5aU : 0xa5U );
            ptr[ sizeof( _LOGICAL ) - i - 1 ] = ptr[ i ];
         }

/* The remaining bad values are set to the appropriate limit.               */
         dat_gl_ndr[ DAT__B ].bad.B =    dat_gl_ndr[ DAT__B ].min.B;
         dat_gl_ndr[ DAT__UB ].bad.UB =  dat_gl_ndr[ DAT__UB ].max.UB;
         dat_gl_ndr[ DAT__W ].bad.W =    dat_gl_ndr[ DAT__W ].min.W;
         dat_gl_ndr[ DAT__UW ].bad.UW =  dat_gl_ndr[ DAT__UW ].max.UW;
         dat_gl_ndr[ DAT__I ].bad.I =    dat_gl_ndr[ DAT__I ].min.I;
         dat_gl_ndr[ DAT__R ].bad.R =    dat_gl_ndr[ DAT__R ].min.R;
         dat_gl_ndr[ DAT__D ].bad.D =    dat_gl_ndr[ DAT__D ].min.D;
      }

/* Determine the byte storage order for each primitive data type.           */
/* =============================================================            */
/* This is done by comparing two values of each type which differ by the    */
/* smallest significant difference. The byte which has changed is then      */
/* identified and the storage order assigned depending on whether this      */
/* change occurs before or after the half-way point.                        */

/* Define a macro to perform the comparisons and to assign the result.      */
#define _order( dtype, value1, value2, order )\
      {\
         unsigned char *ptr1;    /* Pointer to byte sequence                */\
         unsigned char *ptr2;    /* Pointer to byte sequence                */\
         dtype var1;             /* Temporary variable of appropriate type  */\
         dtype var2;             /* Temporary variable of appropriate type  */\
\
/* DAT__MSB is always assigned if the size is one.                          */\
         if ( sizeof( dtype ) == 1 )\
         {\
            (order) = DAT__MSB;\
         }\
\
/* Otherwise, assign each value to a variable of the appropriate type and   */\
/* obtain pointers to the resulting byte sequences.                         */\
         else\
         {\
            (var1) = (dtype) (value1);\
            (var2) = (dtype) (value2);\
            ptr1 = (unsigned char *) &var1;\
            ptr2 = (unsigned char *) &var2;\
\
/* Make decoy calls to ensure the required values are actually evaluated.   */\
            dat1_decoy( (int) var1, (void *) &var1 );\
            dat1_decoy( (int) var2, (void *) &var2 );\
\
/* Find the first byte which differs and test if it is before the half-way  */\
/* point.                                                                   */\
            for ( i = 0; i < sizeof( dtype ); i++ )\
            {\
               if ( ptr1[ i ] != ptr2[ i ] ) break;\
            }\
            (order) = ( i < ( ( sizeof( dtype ) + 1 ) / 2 ) ) ?\
                      DAT__LSB : DAT__MSB;\
         }\
      }

/* Invoke this macro to determine the storage order for each primitive      */
/* type. Avoid using long double constants unless they are supported by the */
/* compiler.                                                                */
      if ( _ok( *status ) )
      {
         _order( _CHAR,    'A',  'B', dat_gl_ndr[ DAT__C ].order )
         _order( _LOGICAL, 0,    1,   dat_gl_ndr[ DAT__L ].order )
         _order( _BYTE,    0,    1,   dat_gl_ndr[ DAT__B ].order )
         _order( _UBYTE,   0,    1,   dat_gl_ndr[ DAT__UB ].order )
         _order( _WORD,    0,    1,   dat_gl_ndr[ DAT__W ].order )
         _order( _UWORD,   0,    1,   dat_gl_ndr[ DAT__UW ].order )
         _order( _INTEGER, 0,    1,   dat_gl_ndr[ DAT__I ].order )
#if HAVE_LONG_DOUBLE
         _order( _REAL,    1.0L, ( 1.0L + eps_REAL ),
                                      dat_gl_ndr[ DAT__R ].order )
         _order( _DOUBLE,  1.0L, ( 1.0L + eps_DOUBLE ),
                                      dat_gl_ndr[ DAT__D ].order )
#else
         _order( _REAL,    1.0, ( 1.0 + eps_REAL ),
                                      dat_gl_ndr[ DAT__R ].order )
         _order( _DOUBLE,  1.0, ( 1.0 + eps_DOUBLE ),
                                      dat_gl_ndr[ DAT__D ].order )
#endif
      }

/* Calculate the number of characters required to format each data type.    */
/* ====================================================================     */
/* The _CHAR and _LOGICAL types have fixed lengths.                         */
      if ( _ok( *status ) )
      {
         dat_gl_ndr[ DAT__C ].txtsize = 1;
         dat_gl_ndr[ DAT__L ].txtsize = 5; /* To accommodate "FALSE"        */

/* Set their number of decimal digits of precision to zero (not used).      */
         dat_gl_ndr[ DAT__C ].digits = 0;
         dat_gl_ndr[ DAT__L ].digits = 0;
      }

/* For the integer types, determine the number of characters required by    */
/* formatting the minimum and maximum values and seeing how many characters */
/* are required. Use the maximum number of characters for the "txtsize"     */
/* value and the number of characters required for the maximum value for    */
/* the "digits" value. Define a macro to do this:                           */
#define _txtsizei( dtype, min, max, txtsize, digits )\
      {\
         char buf[ BUFSIZE + 1 ];/* Temporary buffer for formatting         */\
         int len1;               /* No. characters for minimum              */\
         int len2;               /* No. characters for maximum              */\
\
/* If the values are signed, cast to 64-bit int type.                       */\
         if( _signed( dtype ) )\
         {\
            (void) sprintf( buf, "%-"INT_BIG_S"%n", (INT_BIG) (min), &len1 );\
            (void) sprintf( buf, "%-"INT_BIG_S"%n", (INT_BIG) (max), &len2 );\
         }\
\
/* If the values are unsigned, cast to unsigned long int                    */\
         else\
         {\
            (void) sprintf( buf, "%-"INT_BIG_U"%n", (UINT_BIG) (min), \
                           &len1 );\
            (void) sprintf( buf, "%-"INT_BIG_U"%n", (UINT_BIG) (max), \
                           &len2 );\
         }\
\
/* Assign the results.                                                      */\
         (txtsize) = ( len1 > len2 ) ? len1 : len2;\
         (digits) = len2;\
      }

/* Invoke this macro to determine the number of characters required to      */
/* format all the integer primitive types.                                  */
      if ( _ok( *status ) )
      {
         _txtsizei( _BYTE,
                    dat_gl_ndr[ DAT__B ].min.B,
                    dat_gl_ndr[ DAT__B ].max.B,
                    dat_gl_ndr[ DAT__B ].txtsize,
                    dat_gl_ndr[ DAT__B ].digits )
         _txtsizei( _UBYTE,
                    dat_gl_ndr[ DAT__UB ].min.UB,
                    dat_gl_ndr[ DAT__UB ].max.UB,
                    dat_gl_ndr[ DAT__UB ].txtsize,
                    dat_gl_ndr[ DAT__UB ].digits )
         _txtsizei( _WORD,
                    dat_gl_ndr[ DAT__W ].min.W,
                    dat_gl_ndr[ DAT__W ].max.W,
                    dat_gl_ndr[ DAT__W ].txtsize,
                    dat_gl_ndr[ DAT__W ].digits )
         _txtsizei( _UWORD,
                    dat_gl_ndr[ DAT__UW ].min.UW,
                    dat_gl_ndr[ DAT__UW ].max.UW,
                    dat_gl_ndr[ DAT__UW ].txtsize,
                    dat_gl_ndr[ DAT__UW ].digits )
         _txtsizei( _INTEGER,
                    dat_gl_ndr[ DAT__I ].min.I,
                    dat_gl_ndr[ DAT__I ].max.I,
                    dat_gl_ndr[ DAT__I ].txtsize,
                    dat_gl_ndr[ DAT__I ].digits )
     }

/* Define a macro to format the minimum floating point values using a       */
/* specified number of decimal digits of precision to determine how many    */
/* characters are required.                                                 */
#if HAVE_LONG_DOUBLE && ! __MINGW32__
#define _txtsizef( min, digits, txtsize )\
      {\
         char buf[ BUFSIZE + 1 ];/* Buffer for formatted value              */\
         int len;                /* Number of characters in formatted value */\
\
/* Format the minimum value (cast to long double) using the appropriate     */\
/* number of digits of precision and a minimum field width which is able to */\
/* accommodate the additional characters required (including one digit of   */\
/* the exponent). Record the actual number of characters produced. This     */\
/* will include additional exponent characters if needed.                   */\
         (void) sprintf( buf, "%*.*LE%n",\
                         (int) ( (digits) + 5 ), (int) ( (digits) - 1 ),\
                         (long double) (min), &len );\
\
/* Assign the result.                                                       */\
         (txtsize) = len;\
      }
#else
/* Don't use long double if it is not supported by the compiler.            */
#define _txtsizef( min, digits, txtsize )\
      {\
         char buf[ BUFSIZE + 1 ]; /* Buffer for formatted value             */\
         int len;                /* Number of characters in formatted value */\
         (void) sprintf( buf, "%*.*E%n",\
                         (int) ( (digits) + 5 ), (int) ( (digits) - 1 ),\
                         (double) (min), &len );\
         (txtsize) = len;\
      }
#endif

/* Assign the number of decimal digits as determined previously and invoke  */
/* the macro above to determine the formatted size of the _REAL and _DOUBLE */
/* data types.                                                              */
      if ( _ok( hds_gl_status ) )
      {
         dat_gl_ndr[ DAT__R ].digits = dig_REAL;
         dat_gl_ndr[ DAT__D ].digits = dig_DOUBLE;
         _txtsizef( dat_gl_ndr[ DAT__R ].min.R,
                    dat_gl_ndr[ DAT__R ].digits,
                    dat_gl_ndr[ DAT__R ].txtsize )
         _txtsizef( dat_gl_ndr[ DAT__D ].min.D,
                    dat_gl_ndr[ DAT__D ].digits,
                    dat_gl_ndr[ DAT__D ].txtsize )
      }

/* Determine the number format for each data type.                          */
/* ==============================================                           */
/* (Note we are only interested in distinguishing the currently supported   */
/* types.)                                                                  */

/* Identify ASCII character format by testing a few key characters.         */
      if ( _ok( *status ) )
      {
         if ( ( ( (_CHAR) 'A' ) == ( (_CHAR) 0x41 ) ) &&
              ( ( (_CHAR) '0' ) == ( (_CHAR) 0x30 ) ) )
         {
            dat_gl_ndr[ DAT__C ].format = DAT__ASCII;
         }

/* Note if the format cannot be identified.                                 */
         else
         {
            dat_gl_ndr[ DAT__C ].format = DAT__UNKNOWN;
         }
      }

/* The format of other non-floating point data types is determined by       */
/* setting up a series of special bit patterns in memory and determining    */
/* the values they represent. Define a macro which declares a series of     */
/* variables with a specified data type and initialises them to contain     */
/* these special bit patterns.                                              */
#define _specialbits( dtype, order )\
\
/*  Declare the variables.                                                  */\
      dtype allclear;            /* All bits clear                          */\
      dtype allset;              /* All bits set                            */\
      dtype lsbclear;            /* Set except for least significant bit    */\
      dtype lsbset;              /* Clear except for least significant bit  */\
      dtype msbclear;            /* Set except for most significant bit     */\
      dtype msbset;              /* Clear except for most significant bit   */\
\
/*  Declare local variables required for initialisation.                    */\
      {\
         int lsbyte;             /* Offset of least significant byte        */\
         int msbyte;             /* Offset of most significant byte         */\
         unsigned char *ptr1;    /* Pointer to bytes                        */\
         unsigned char *ptr2;    /* Pointer to bytes                        */\
\
/*  Initiallise the allclear and allset values, clearing and setting all    */\
/*  the bits in each of their bytes.                                        */\
         ptr1 = (unsigned char *) &allclear;\
         ptr2 = (unsigned char *) &allset;\
         for ( i = 0; i < (int) sizeof( dtype ); i++ )\
         {\
            ptr1[ i ] = (unsigned char) 0u;\
            ptr2[ i ] = UCHAR_MAX;\
         }\
\
/*  Determine the offsets of the least and most significant bytes.          */\
         lsbyte = ( (order) == DAT__LSB ) ? 0 : (int) ( sizeof( dtype ) - 1 );\
         msbyte = (int) sizeof( dtype ) - lsbyte - 1;\
\
/*  Set the lsbset value to all clear, then set the least significant bit   */\
/*  of the least significant byte.                                          */\
         lsbset = allclear;\
         ptr1 = (unsigned char *) &lsbset;\
         ptr1[ lsbyte ] = (unsigned char) 1u;\
\
/* Similarly, clear the least significant bit in the lsbclear value.        */\
         lsbclear = allset;\
         ptr1 = (unsigned char *) &lsbclear;\
         ptr1[ lsbyte ] = (unsigned char) ( UCHAR_MAX - 1u );\
\
/* Set the most significant bit in the msbset value.                        */\
         msbset = allclear;\
         ptr1 = (unsigned char *) &msbset;\
         ptr1[ msbyte ] = (unsigned char) ( 1u << ( CHAR_BIT - 1 ) );\
\
/* Clear the most significant bit in the msbclear value.                    */\
         msbclear = allset;\
         ptr1 = (unsigned char *) &msbclear;\
         ptr1[ msbyte ] = (unsigned char) ( UCHAR_MAX >> 1 );\
      }\
\
/*  Make decoy calls to prevent unwanted compiler optimisation.             */\
      dat1_decoy( (int) allclear, (void *) &allclear );\
      dat1_decoy( (int) allset, (void *) &allset );\
      dat1_decoy( (int) lsbclear, (void *) &lsbclear );\
      dat1_decoy( (int) lsbset, (void *) &lsbset );\
      dat1_decoy( (int) msbclear, (void *) &msbclear );\
      dat1_decoy( (int) msbset, (void *) &msbset );

/* Identify the _LOGICAL format by locally declaring _LOGICAL values        */
/* holding the above special bit patterns and performing tests on them      */
/* using the macro F77_ISTRUE defined in f77.h.                             */
      if ( _ok( hds_gl_status ) )
      {
         _specialbits( _LOGICAL, dat_gl_ndr[ DAT__L ].order )

/* Test for bit zero = 1 ==> TRUE, bit zero = 0 ==> FALSE.                  */
         if ( !F77_ISTRUE( allclear ) &&
               F77_ISTRUE( allset ) &&
              !F77_ISTRUE( lsbclear ) &&
               F77_ISTRUE( lsbset ) &&
               F77_ISTRUE( msbclear ) &&
              !F77_ISTRUE( msbset ) )
         {
            dat_gl_ndr[ DAT__L ].format = DAT__BIT0;
         }

/* Test for non-zero ==> TRUE, zero ==> FALSE.                              */
         else if ( !F77_ISTRUE( allclear ) &&
                    F77_ISTRUE( allset ) &&
                    F77_ISTRUE( lsbclear ) &&
                    F77_ISTRUE( lsbset ) &&
                    F77_ISTRUE( msbclear ) &&
                    F77_ISTRUE( msbset ) )
         {
            dat_gl_ndr[ DAT__L ].format = DAT__NZ;
         }

/* Note if the format cannot be identified.                                 */
         else
         {
            dat_gl_ndr[ DAT__L ].format = DAT__UNKNOWN;
         }
      }

/* Define a macro to identify the format of the integer types in a similar  */
/* way.                                                                     */
#define _format( dtype, order, min, max, format )\
      {\
\
/* Declare special bit patterns with the appropriate data type and byte     */\
/* storage order.                                                           */\
         _specialbits( dtype, order )\
\
/* The format is binary if it is unsigned, all bits zero gives the minimum  */\
/* value, all bits set gives the maximum value and the least significant    */\
/* bit set gives 1.                                                         */\
         if ( !_signed( dtype ) &&\
              ( allclear == (dtype) (min) ) &&\
              ( allset == (dtype) (max) ) &&\
              ( lsbset == (dtype) 1 ) )\
         {\
            (format) = DAT__BINARY;\
         }\
\
/* The format is 2's complement if it is signed, all bits set gives -1, all */\
/* except the least significant bit set gives -2, the most significant bit  */\
/* set gives the minimum value, and all except the most significant bit set */\
/* gives the maximum value.                                                 */\
         else if ( _signed( dtype ) &&\
                   ( allset == (dtype) -1 ) &&\
                   ( lsbclear == (dtype) -2 ) &&\
                   ( msbset == (dtype) (min) ) &&\
                   ( msbclear == (dtype) (max) ) )\
         {\
            (format) = DAT__2COMP;\
         }\
\
/* Note if the format cannot be identified.                                 */\
         else\
         {\
            (format) = DAT__UNKNOWN;\
         }\
      }

/* Invoke this macro to determine the data format of the integer primitive  */
/* data types.                                                              */
      if ( _ok( *status ) )
      {
         _format( _BYTE,
                  dat_gl_ndr[ DAT__B ].order,
                  dat_gl_ndr[ DAT__B ].min.B,
                  dat_gl_ndr[ DAT__B ].max.B,
                  dat_gl_ndr[ DAT__B ].format )
         _format( _UBYTE,
                  dat_gl_ndr[ DAT__UB ].order,
                  dat_gl_ndr[ DAT__UB ].min.UB,
                  dat_gl_ndr[ DAT__UB ].max.UB,
                  dat_gl_ndr[ DAT__UB ].format )
         _format( _WORD,
                  dat_gl_ndr[ DAT__W ].order,
                  dat_gl_ndr[ DAT__W ].min.W,
                  dat_gl_ndr[ DAT__W ].max.W,
                  dat_gl_ndr[ DAT__W ].format )
         _format( _UWORD,
                  dat_gl_ndr[ DAT__UW ].order,
                  dat_gl_ndr[ DAT__UW ].min.UW,
                  dat_gl_ndr[ DAT__UW ].max.UW,
                  dat_gl_ndr[ DAT__UW ].format )
         _format( _INTEGER,
                  dat_gl_ndr[ DAT__I ].order,
                  dat_gl_ndr[ DAT__I ].min.I,
                  dat_gl_ndr[ DAT__I ].max.I,
                  dat_gl_ndr[ DAT__I ].format )

/* Assign the floating-point format codes determined earlier.               */
         dat_gl_ndr[ DAT__R ].format = format_REAL;
         dat_gl_ndr[ DAT__D ].format = format_DOUBLE;
      }

/* Check that the format of all primitive data types has been identified.   */
/* Report an error if any has not.                                          */
      if ( _ok( *status ) )
      {
         for ( i = 0; i < DAT__MXPRM; i++ )
         {
            if ( dat_gl_ndr[ i ].format == DAT__UNKNOWN )
            {
               *status = DAT__FATAL;
               emsSetnc( "TYPE", dat_gl_ndr[ i ].name, EMS__SZTOK );
               emsRep( "DAT1_INIT_NDR_4",
                          "Unable to identify the native machine format used \
for the ^TYPE data type; HDS may require modification for use on this machine.",
                          status );
               break;
            }
         }
      }

/* Exit the routine.                                                        */
      return;
   }
