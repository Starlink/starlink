/*
*+
*  Name:
*     fintramap.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST IntraMap class.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the IntraMap class.

*  Routines Defined:
*     AST_INTRAMAP
*     AST_INTRAREG
*     AST_ISAINTRAMAP

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     18-MAR-1998 (RFWS):
*        Original version.
*     15-SEP-1999 (RFWS):
*        Added a THIS pointer to the external transformation function
*        used by an IntraMap.
*/

/* Define the astFORTRAN77 macro which prevents error messages from
   AST C functions from reporting the file and line number where the
   error occurred (since these would refer to this file, they would
   not be useful). */
#define astFORTRAN77

/* Header files. */
/* ============= */
#include "f77.h"                 /* FORTRAN <-> C interface macros (SUN/209) */
#include "c2f77.h"               /* F77 <-> C support functions/macros */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory handling facilities */
#include "intramap.h"            /* C interface to the IntraMap class */

#include <stddef.h>
#include <string.h>

/* Prototypes for private functions. */
/* ================================= */
static void TranWrap( void (*)( AstMapping *, int, int, const double *[], int, int, double *[] ), AstMapping *, int, int, const double *[], int, int, double *[], int * );

/* Transformation function interface. */
/* ================================== */
/* This is concerned with allowing FORTRAN implementations of
   transformation functions to be passed to the IntraMap class and
   invoked when necessary by C code in the main class
   implementation. All FORTRAN-specific aspects of this interface are
   encapsulated here. */
static void TranWrap( void (* tran)( AstMapping *, int, int, const double *[],
                                     int, int, double *[] ),
                      AstMapping *this, int npoint, int ncoord_in,
                      const double *ptr_in[], int forward, int ncoord_out,
                      double *ptr_out[], int *status ) {
/*
*  Name:
*     TranWrap

*  Purpose:
*     Wrapper function to invoke a FORTRAN transformation function.

*  Type:
*     Private function.

*  Synopsis:
*     void TranWrap( void (* tran)( AstMapping *, int, int, const double *[],
*                                   int, int, double *[] ),
*                    AstMapping *this, int npoint, int ncoord_in,
*                    const double *ptr_in[], int forward, int ncoord_out,
*                    double *ptr_out[], int *status )

*  Description:
*     This function invokes a FORTRAN implementation of a
*     transformation function (which resembles AST_TRANN from the
*     Mapping class FORTRAN interface) in order to make it callable by
*     C code which would prefer to call a C function that resembles
*     astTranP (from the Mapping class C interface).

*  Parameters:
*     tran
*        Pointer to the FORTRAN transformation function to be invoked.
*        This should result from a cast applied to a pointer to a
*        function that resembles AST_TRANN (but with the first
*        argument omitted).
*     this
*        An external Mapping ID associated with the internal (true C) pointer
*        for the IntraMap whose transformation is being evaluated.
*     npoint
*        The number of points to be transformed.
*     ncoord_in
*        The number of coordinates being supplied for each input point
*        (i.e. the number of dimensions of the space in which the
*        input points reside).
*     ptr_in
*        An array of pointers to double, with "ncoord_in"
*        elements. Element "ptr_in[coord]" should point at the first
*        element of an array of double (with "npoint" elements) which
*        contain the values of coordinate number "coord" for each
*        input (untransformed) point. The value of coordinate number
*        "coord" for input point number "point" is therefore given by
*        "ptr_in[coord][point]".
*     forward
*        A non-zero value indicates that the forward coordinate
*        transformation is to be applied, while a zero value indicates
*        that the inverse transformation should be used.
*     ncoord_out
*        The number of coordinates being generated for each output
*        point (i.e. the number of dimensions of the space in which
*        the output points reside). This need not be the same as
*        "ncoord_in".
*     ptr_out
*        An array of pointers to double, with "ncoord_out"
*        elements. Element "ptr_out[coord]" should point at the first
*        element of an array of double (with "npoint" elements) into
*        which the values of coordinate number "coord" for each output
*        (transformed) point will be written.  The value of coordinate
*        number "coord" for output point number "point" will therefore
*        be found in "ptr_out[coord][point]".
*     status
*        Pointer to the inherited status value.
*/

/* Local Variables; */
   DECLARE_INTEGER(INDIM);       /* First dimension size of input array */
   DECLARE_INTEGER(NCOORD_IN);   /* Number of input coordinates */
   DECLARE_INTEGER(NCOORD_OUT);  /* Number of output coordinates */
   DECLARE_INTEGER(NPOINT);      /* Number of points */
   DECLARE_INTEGER(OUTDIM);      /* First dimension size of output array */
   DECLARE_INTEGER(STATUS);      /* FORTRAN error status variable */
   DECLARE_INTEGER(THIS);        /* External ID for the IntraMap */
   DECLARE_LOGICAL(FORWARD);     /* Use forward transformation? */
   F77_DOUBLE_TYPE *IN;          /* Input coordinate array for FORTRAN */
   F77_DOUBLE_TYPE *OUT;         /* Output coordinate array for FORTRAN */
   int coord;                    /* Loop counter for coordinates */
   int i;                        /* Index into FORTRAN arrays */

/* Check the global error status. */
   if ( !astOK ) return;

/* Assign input values to the arguments for the FORTRAN transformation
   function. */
   THIS = astP2I( this );
   NPOINT = npoint;
   NCOORD_IN = ncoord_in;
   INDIM = npoint;
   FORWARD = forward ? F77_TRUE : F77_FALSE;
   NCOORD_OUT = ncoord_out;
   OUTDIM = npoint;

/* Since the input/output coordinate values may be stored in separate
   arrays, we must move them temporarily into new 2-dimensional
   arrays, as required by the FORTRAN transformation function
   interface. Allocate memory for these arrays. */
   IN = astMalloc( (size_t) ( npoint * ncoord_in ) *
                   sizeof( F77_DOUBLE_TYPE ) );
   OUT = astMalloc( (size_t) ( npoint * ncoord_out ) *
                    sizeof( F77_DOUBLE_TYPE ) );

/* If OK, fill the input array with coordinate values. Use "memcpy" to
   avoid numerical errors if the data contain junk - this allows the
   transformation function to produce the appropriate error instead of
   it happening here. */
   if ( astOK ) {
      i = 0;
      for ( coord = 0; coord < ncoord_in; coord++ ) {
         (void) memcpy( IN + i, ptr_in[ coord ],
                        (size_t) npoint * sizeof( F77_DOUBLE_TYPE ) );
         i += npoint;
      }
   }

/* Cast the transformation function pointer to a pointer to the
   FORTRAN subroutine and then invoke it. Transfer the AST error
   status to and from the subroutine's error status argument. */
   if ( astOK ) {
      STATUS = astStatus;
      ( *(void (*)()) tran )( INTEGER_ARG(&THIS),
                              INTEGER_ARG(&NPOINT),
                              INTEGER_ARG(&NCOORD_IN),
                              INTEGER_ARG(&INDIM),
                              DOUBLE_ARRAY_ARG(IN),
                              LOGICAL_ARG(&FORWARD),
                              INTEGER_ARG(&NCOORD_OUT),
                              INTEGER_ARG(&OUTDIM),
                              DOUBLE_ARRAY_ARG(OUT),
                              INTEGER_ARG(&STATUS) );
      astSetStatus( STATUS );
   }


/* If OK, transfer the transformed coordinate values to the output
   arrays. */
   if ( astOK ) {
      i = 0;
      for ( coord = 0; coord < ncoord_out; coord++ ) {
         (void) memcpy( ptr_out[ coord ], OUT + i,
                        (size_t) npoint * sizeof( F77_DOUBLE_TYPE ) );
         i += npoint;
      }
   }

/* Free the temporary arrays. */
   astFree( IN );
   astFree( OUT );
}

/* FORTRAN interface functions. */
/* ============================ */
/* These functions implement the remainder of the FORTRAN interface. */
F77_SUBROUTINE(ast_intrareg)( CHARACTER(NAME),
                              INTEGER(NIN),
                              INTEGER(NOUT),
                              void (* TRAN)(),
                              INTEGER(FLAGS),
                              CHARACTER(PURPOSE),
                              CHARACTER(AUTHOR),
                              CHARACTER(CONTACT),
                              INTEGER(STATUS)
                              TRAIL(NAME)
                              TRAIL(PURPOSE)
                              TRAIL(AUTHOR)
                              TRAIL(CONTACT) ) {
   GENPTR_CHARACTER(NAME)
   GENPTR_INTEGER(NIN)
   GENPTR_INTEGER(NOUT)
   GENPTR_INTEGER(FLAGS)
   GENPTR_CHARACTER(PURPOSE)
   GENPTR_CHARACTER(AUTHOR)
   GENPTR_CHARACTER(CONTACT)
   char *name;
   void (* tran)( AstMapping *, int, int, const double *[], int, int,
                  double *[] );
   char *purpose;
   char *author;
   char *contact;

   astAt( "AST_INTRAREG", NULL, 0 );
   astWatchSTATUS(
      name = astString( NAME, NAME_length );
      tran =
         (void (*)( AstMapping *, int, int, const double *[], int, int,
                    double *[] )) TRAN;
      purpose = astString( PURPOSE, PURPOSE_length );
      author = astString( AUTHOR, AUTHOR_length );
      contact = astString( CONTACT, CONTACT_length );
      astIntraRegFor( name, *NIN, *NOUT, tran, TranWrap, *FLAGS, purpose,
                      author, contact );
      astFree( name );
      astFree( purpose );
      astFree( author );
      astFree( contact );
   )
}

F77_INTEGER_FUNCTION(ast_intramap)( CHARACTER(NAME),
                                    INTEGER(NIN),
                                    INTEGER(NOUT),
                                    CHARACTER(OPTIONS),
                                    INTEGER(STATUS)
                                    TRAIL(NAME)
                                    TRAIL(OPTIONS) ) {
   GENPTR_CHARACTER(NAME)
   GENPTR_INTEGER(NIN)
   GENPTR_INTEGER(NOUT)
   GENPTR_CHARACTER(OPTIONS)
   F77_INTEGER_TYPE(RESULT);
   char *name;
   char *options;
   int i;

   astAt( "AST_INTRAMAP", NULL, 0 );
   astWatchSTATUS(
      name = astString( NAME, NAME_length );
      options = astString( OPTIONS, OPTIONS_length );

/* Truncate the options string to exlucde any trailing spaces. */
      astChrTrunc( options );

/* Change ',' to '\n' (see AST_SET in fobject.c for why). */
      if ( astOK ) {
         for ( i = 0; options[ i ]; i++ ) {
            if ( options[ i ] == ',' ) options[ i ] = '\n';
         }
      }
      RESULT = astP2I( astIntraMap( name, *NIN, *NOUT, "%s", options ) );
      astFree( name );
      astFree( options );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_isaintramap)( INTEGER(THIS),
                                       INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astAt( "AST_ISAINTRAMAP", NULL, 0 );
   astWatchSTATUS(
      RESULT = astIsAIntraMap( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}
