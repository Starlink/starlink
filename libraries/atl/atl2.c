/*
*  Name:
*    atl2.c

*  Purpose:
*    Providea a virtual in-memory file structure for use from F77

*  Description:
*    An F77 API for a structure that holds lines of text. Similar to GRP
*    library but allowing arbitrary length lines of text to be stored.
*    The structure is refered to as a "virtual file structure" (VFS) below.

*  Copyright:
*    Copyright (C) 2019 East Asian Observatory

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
*     DSB: David S. Berry (EAO)

*  History:
*     16-SEP-2019 (DSB):
*        Original version.
*     14-DEC-2019 (DSB):
*        - Improve error mesages in ATL2_GET.
*        - Prevent ATL2_RDFIL bug that caused input lines following a blank 
*          line to be ignored.
*/

/* Headers: */
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include "ast.h"
#include "mers.h"
#include "cnf.h"
#include "sae_par.h"
#include "f77.h"

/* Local data types: */
typedef struct Vfs {
   int nline;
   char **lines;
   size_t *lengths;
} Vfs;

/* Prototypes for local static functions */
static void atl2Append( Vfs *vfs, char *text, size_t len, int *status );


/* Main public functions */
F77_SUBROUTINE(atl2_appnd)( CHARACTER(LINE), POINTER(VFS),
                            INTEGER(STATUS) TRAIL(LINE) ){
/*
*+
*  Name:
*     ATL2_APPND

*  Purpose:
*     Append a line of text to a virtual file structure

*  Invocation:
*     CALL ATL2_APPND( LINE, VFS, STATUS )

*  Description:
*     Append a line of text to a virtual file structure

*  Arguments:
*     LINE = CHARACTER * ( * ) (Given)
*        The line of text.
*     VFS = INTEGER (Given)
*        Pointer to the VFS structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*-
*/

/* Arguments: */
   GENPTR_CHARACTER(LINE)
   GENPTR_POINTER(VFS)
   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   Vfs *vfs;
   int i;
   int *old_status;

/* Check inherited status. */
   if( *STATUS != SAI__OK ) return;

/* Import the pointer and check it is usable. */
   vfs = cnfCptr( *VFS );
   if( vfs ) {

/* Tell AST to use the supplied status variable. */
      old_status = astWatch( STATUS );

/* Append the string to the VFS, removing trailing spaces. */
      atl2Append( vfs, LINE, LINE_length, STATUS );

/* Re-instate original AST status variable. */
      astWatch( old_status );

/* Report an error if the supplied VFS pointer was unusable. */
   } else {
      *STATUS = SAI__ERROR;
      errRep( " ", "ATL2_APPND: Invalid VFS pointer supplied.", STATUS );
   }
}

F77_SUBROUTINE(atl2_delet)( POINTER(VFS), INTEGER(STATUS) ){
/*
*+
*  Name:
*     ATL2_DELET

*  Purpose:
*     Delete a virtual file structure

*  Invocation:
*     CALL ATL2_DELET( VFS, STATUS )

*  Description:
*     Delete the supplied virtual file structure, freeing its resources.

*  Arguments:
*     VFS = INTEGER (Given and Returned)
*        Pointer to the VFS structure. A value of zero is returned on exit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine attempts to execute even iof an error has already
*     occurred.
*-
*/

/* Arguments: */
   GENPTR_POINTER(VFS)
   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   Vfs *vfs;
   int i;

/* Start a new error reporting context */
   errBegin( STATUS );

/* Import the pointer and check it is usable. */
   vfs = cnfCptr( *VFS );
   if( vfs ) {

/* Free the strings. */
      for( i = 0; i < vfs->nline; i++ ) {
         vfs->lines[ i ] = astFree( vfs->lines[ i ] );
      }

/* Free the arrays. */
      vfs->lines = astFree( vfs->lines );
      vfs->lengths = astFree( vfs->lengths );

/* Free the structure itself. */
      cnfFree( vfs );
      vfs = NULL;

/* Return a null pointer. */
      *VFS = 0;

/* Report an error if the supplied VFS pointer was unusable. */
   } else {
      *STATUS = SAI__ERROR;
      errRep( " ", "ATL2_DELET: Invalid VFS pointer supplied.", STATUS );
   }

/* End the error reporting context */
   errEnd( STATUS );
}

F77_SUBROUTINE(atl2_empty)( POINTER(VFS), INTEGER(STATUS) ){
/*
*+
*  Name:
*     ATL2_EMPTY

*  Purpose:
*     Empty a virtual file structure

*  Invocation:
*     CALL ATL2_EMPTY( VFS, STATUS )

*  Description:
*     Delete all lines of text in the supplied virtual file structure.

*  Arguments:
*     VFS = INTEGER (Given)
*        Pointer to the VFS structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*-
*/

/* Arguments: */
   GENPTR_POINTER(VFS)
   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   Vfs *vfs;
   int i;

/* Check inherited status. */
   if( *STATUS != SAI__OK ) return;

/* Import the pointer and check it is usable. */
   vfs = cnfCptr( *VFS );
   if( vfs ) {

/* Free the strings. */
      for( i = 0; i < vfs->nline; i++ ) {
         vfs->lines[ i ] = astFree( vfs->lines[ i ] );
      }

/* Free the arrays. */
      vfs->lines = astFree( vfs->lines );
      vfs->lengths = astFree( vfs->lengths );

/* Store the new size. */
      vfs->nline = 0;

/* Report an error if the supplied VFS pointer was unusable. */
   } else {
      *STATUS = SAI__ERROR;
      errRep( " ", "ATL2_EMPTY: Invalid VFS pointer supplied.", STATUS );
   }
}

F77_SUBROUTINE(atl2_get)( POINTER(VFS), INTEGER(INDEX), INTEGER(START),
                          CHARACTER(LINE), LOGICAL(TRUNC), INTEGER(STATUS)
                          TRAIL(LINE) ){
/*
*+
*  Name:
*     ATL2_GET

*  Purpose:
*     Get a line of text from a virtual file structure

*  Invocation:
*     CALL ATL2_GET( VFS, INDEX, START, LINE, TRUNC, STATUS )

*  Description:
*     Get a line of text from a virtual file structure.

*  Arguments:
*     VFS = INTEGER (Given)
*        Pointer to the VFS structure.
*     INDEX = INTEGER (Given)
*        The index of the line to get, starting at one.
*     START = INTEGER (Given)
*        The index of the first character of the selected line to return,
*        starting at one.
*     LINE = CHARACTER * ( * ) (Returned)
*        The returned text.
*     TRUNC = LOGICAL (Returned)
*        Returned .TRUE. if the supplied buffer (LINE) was too short to
*        hold the requested text (i.e. the text from character START to
*        the end of the line). In this case the returned buffer will hold
*        the truncated text (no error is reported).
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*-
*/

/* Arguments: */
   GENPTR_POINTER(VFS)
   GENPTR_INTEGER(INDEX)
   GENPTR_INTEGER(START)
   GENPTR_CHARACTER(LINE)
   GENPTR_LOGICAL(TRUNC)
   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   Vfs *vfs;
   int iline;
   size_t length;
   size_t nc;
   size_t start;

/* Initialise */
   *TRUNC = F77_FALSE;
   nc = 0;

/* Check inherited status. */
   if( *STATUS != SAI__OK ) return;

/* Import the pointer and check it is usable. */
   vfs = cnfCptr( *VFS );
   if( vfs ) {

/* Check the arguments. */
      if( *INDEX < 1 || *INDEX > vfs->nline ) {
         *STATUS = SAI__ERROR;
         if( vfs->nline ) {
            errRepf( " ", "ATL2_GET: Invalid line number %d (must be between "
                     "1 and %d).", STATUS, *INDEX, vfs->nline );
         } else {
            errRepf( " ", "ATL2_GET: Cannot get line %d (the file is empty).",
                     STATUS, *INDEX );
         }
      }  else {
         iline = *INDEX - 1;
         length = vfs->lengths[ iline ];

         if( *START < 1 || ( *START > length && ( length > 0 || *START != 1 ))) {
            *STATUS = SAI__ERROR;
            if( length ) {
               errRepf( " ", "ATL2_GET: Invalid line start %d (must be "
                       "between 1 and %d).", STATUS, *START, length );
               errRepf( " ", "ATL2_GET: Invalid line start %d (line %d "
                        "has length %d).", STATUS, *START, *INDEX, length );
            } else {
               errRepf( " ", "ATL2_GET: Invalid line start %d (line %d "
                        "has zero length).", STATUS, *START, *INDEX );
            }
         } else {
            start = *START - 1;

/* Check that the number of characters requested will fit in the supplied
   buffer. Indicate truncation and reduce the number of characters to
   return if not. */
            nc = length - start;
            if( LINE_length < nc ) {
               *TRUNC = F77_TRUE;
               nc = LINE_length;
            }

/* Copy the characters to the buffer. */
            memcpy( LINE + start, vfs->lines[iline], nc );
         }
      }

/* Report an error if the supplied VFS pointer was unusable. */
   } else {
      *STATUS = SAI__ERROR;
      errRep( " ", "ATL2_GET: Invalid VFS pointer supplied.", STATUS );
   }

/* Pad with spaces */
   if( nc < LINE_length ) memset( LINE + nc, ' ', LINE_length - nc );

}

F77_SUBROUTINE(atl2_gtmxl)( POINTER(VFS), INTEGER(MXLEN), INTEGER(STATUS) ){
/*
*+
*  Name:
*     ATL2_GTMXL

*  Purpose:
*     Get the maximum line length in a virtual file structure

*  Invocation:
*     CALL ATL2_GTMXL( VFS, MXLEN, STATUS )

*  Description:
*     Get the length of the longest line in a virtual file structure.

*  Arguments:
*     VFS = INTEGER (Given)
*        Pointer to the VFS structure.
*     MXLEN = INTEGER (Returned)
*        The longest line length in the VFS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*-
*/

/* Arguments: */
   GENPTR_POINTER(VFS)
   GENPTR_INTEGER(MXLEN)
   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   Vfs *vfs;
   int i;

/* Initialise */
   *MXLEN = 0;

/* Check inherited status. */
   if( *STATUS != SAI__OK ) return;

/* Import the pointer and check it is usable. */
   vfs = cnfCptr( *VFS );
   if( vfs ) {

/* Loop round the lines, finding the largest line length. */
      for( i = 0; i < vfs->nline; i++ ) {
         if( vfs->lengths[ i ] > *MXLEN ) *MXLEN = vfs->lengths[ i ];
      }

/* Report an error if the supplied VFS pointer was unusable. */
   } else {
      *STATUS = SAI__ERROR;
      errRep( " ", "ATL2_GTMXL: Invalid VFS pointer supplied.", STATUS );
   }
}

F77_SUBROUTINE(atl2_gtsiz)( POINTER(VFS), INTEGER(SIZE), INTEGER(STATUS) ){
/*
*+
*  Name:
*     ATL2_GTSIZ

*  Purpose:
*     Get the number of lines of text in a virtual file structure

*  Invocation:
*     CALL ATL2_GTSIZ( VFS, SIZE, STATUS )

*  Description:
*     Get the number of lines of text in a virtual file structure.

*  Arguments:
*     VFS = INTEGER (Given)
*        Pointer to the VFS structure.
*     SIZE = INTEGER (Returned)
*        The number of lines of text currently in the VFS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*-
*/

/* Arguments: */
   GENPTR_POINTER(VFS)
   GENPTR_INTEGER(SIZE)
   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   Vfs *vfs;
   int i;

/* Initialise */
   *SIZE = 0;

/* Check inherited status. */
   if( *STATUS != SAI__OK ) return;

/* Import the pointer and check it is usable. */
   vfs = cnfCptr( *VFS );
   if( vfs ) {

/* Return the number of lines in the VFS. */
      *SIZE = vfs->nline;

/* Report an error if the supplied VFS pointer was unusable. */
   } else {
      *STATUS = SAI__ERROR;
      errRep( " ", "ATL2_GTSIZ: Invalid VFS pointer supplied.", STATUS );
   }
}

F77_SUBROUTINE(atl2_init)( POINTER(VFS), INTEGER(STATUS) ){
/*
*+
*  Name:
*     ATL2_INIT

*  Purpose:
*     Initialise a virtual file structure

*  Invocation:
*     CALL ATL2_INIT( VFS, STATUS )

*  Description:
*     A new empty virtual file structure is created. It should be
*     destroyed using ATL2_DELET when no longer needed.

*  Arguments:
*     VFS = INTEGER (Returned)
*        Pointer to the new structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*-
*/

/* Arguments: */
   GENPTR_POINTER(VFS)
   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   Vfs *vfs;

/* Initialise returned values. */
   *VFS = 0;

/* Check inherited status. */
   if( *STATUS != SAI__OK ) return;

/* Allocate and initialise an empty VFS structure. */
   vfs = cnfMalloc( sizeof( *vfs ) );
   if( !vfs ) {
      *STATUS = SAI__ERROR;
      errRepf( " ", "ATL2_INIT: Failed to allocate %zu bytes.",
                     STATUS, sizeof( *vfs ) );
   } else {
      vfs->nline = 0;
      vfs->lines = NULL;
      vfs->lengths = NULL;

/* Export the returned pointer. */
      *VFS = cnfFptr( vfs );
   }
}

F77_SUBROUTINE(atl2_rdfil)( CHARACTER(FILNAM), POINTER(VFS),
                            INTEGER(STATUS) TRAIL(FILNAM) ){
/*
*  Name:
*     ATL2_RDFIL

*  Purpose:
*     Read the contents of a text file into a virtual file structure

*  Invocation:
*     CALL ATL2_RDFIL( FILNAM, VFS, STATUS )

*  Description:
*     The contents of the specified text file are read and appended to
*     the current contents of the VFS. Note, the carriage return/line-feed
*     characters marking the end of each line are not stored in the VFS.

*  Arguments:
*     FILNAM = CHARACTER * ( * ) (Given)
*        The file name.
*     VFS = INTEGER (Returned)
*        Pointer to the VFS structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*/

/* Arguments: */
   GENPTR_CHARACTER(FILNAM)
   GENPTR_POINTER(VFS)
   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   FILE *fd;
   Vfs *vfs;
   char *file;
   char *line;
   int c;
   int i;
   int *old_status;
   size_t nch;
   size_t szline;

/* Check inherited status. */
   if( *STATUS != SAI__OK ) return;

/* Import the pointer and check it is usable. */
   vfs = cnfCptr( *VFS );
   if( vfs ) {

/* Tell AST to use the supplied status variable. */
      old_status = astWatch( STATUS );

/* Create a C null-terminated string form the supplied Fortran space
   padded string. */
      file = astString( FILNAM, FILNAM_length );
      astChrRemoveBlanks( file );

/* Open the file for reading. Report an error if it cannot be opened. */
      fd = fopen( file, "r" );
      if( !fd ) {
         *STATUS = SAI__ERROR;
         errRepf( " ", "ATL2_RDFIL: Error opening text file '%s'.", STATUS,
                  file );
         errRepf( " ", "%s", STATUS, strerror(errno) );

/* Otherwise, read the file character by character, looking for newline
   (\n) characters. Note, the strings stored in the VFS are *NOT*
   null-terminated. */
      } else {
         szline = 20;
         line = astMalloc( szline );
         nch = 0;
         while( *STATUS == SAI__OK && ( (c = fgetc( fd )) != EOF ) ) {

/* If the current character is a newline, the line has ended. Store it in
   the VFS, expanding the VFS to make room for it. */
            if( c == '\n' ) {
               atl2Append( vfs, line, nch, STATUS );

/* Reset things for the next line. Retain the memory since it may
   well be appropriate for the next line. */
               nch = 0;

/* If the current character is not a carriage return, store it, expanding the
   line memory if required. */
            } else if( c != '\r' ) {
               if( nch == szline ) {
                  szline = 3*szline/2;
                  line = astRealloc( line, szline );
               }
               line[ nch++ ] = c;
            }
         }
      }

/* Append any remaining characters (i.e. a final line that does not end
   with a NEWLINE CHARACTER). */
      if( nch > 0 ) {
         line = astRealloc( line, nch );
         atl2Append( vfs, line, nch, STATUS );
      }

/* Free local resources. */
      file = astFree( file );
      line = astFree( line );

/* Re-instate original AST status variable. */
      astWatch( old_status );

/* Report an error if the supplied VFS pointer was unusable. */
   } else {
      *STATUS = SAI__ERROR;
      errRep( " ", "ATL2_RDFIL: Invalid VFS pointer supplied.", STATUS );
   }
}


/* Append an F77 space-padded string to the VFS, removing trailing
   spaces. */
static void atl2Append( Vfs *vfs, char *text, size_t len, int *status ){

/* Local Variables: */
   size_t l;
   char *p;
   int i;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Find the length of the supplied text, excluding trailing spaces. */
   l = len;
   p = text + len - 1;
   while( l > 0 && isspace( *p ) ) {
      p--;
      l--;
   }

/* Expand the arrays in the VFS. */
   i = (vfs->nline)++;
   vfs->lines = astGrow( vfs->lines, vfs->nline, sizeof(*(vfs->lines)) );
   vfs->lengths = astGrow( vfs->lengths, vfs->nline, sizeof(*(vfs->lengths)) );

/* If OK, store a copy of the supplied Fortran string (excluding trailing
   spaces) in dynamic memory and store the pointer in it in the VFS. */
   if( vfs->lengths ) {
      vfs->lines[ i ] = astStore( NULL, text, l );
      vfs->lengths[ i ] = l;
   }
}

