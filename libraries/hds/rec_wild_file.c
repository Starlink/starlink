#if HAVE_CONFIG_H
#   include <config.h>           /* Autoconf definitions                    */
#endif

/* C include files:                                                         */
/* ===============                                                          */
#include <ctype.h>

/* VMS version include files:                                               */
/* =========================                                                */
#if defined( vms )
#include <descrip.h>             /* Data descriptor definitions (VMS)       */
#include <stsdef.h>              /* System status codes (VMS)               */
#include <rms.h>                 /* RMS definitions (VMS)                   */

/* Portable version include files:                                          */
/* ==============================                                           */
#else
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <sys/types.h>
#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <unistd.h>
#endif

/* Other include files:                                                     */
/* ===================                                                      */
#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS__ public constants                  */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Private rec_ definitions                */
#include "dat_err.h"             /* DAT__ error code definitions            */

   void rec_wild_file( const char *fspec, INT fspec_len, struct WLD **context,
                       int *alldone, char **fname, INT *fname_len )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_wild_file                                                         */

/* Purpose:                                                                 */
/*    Return successive file names matching a wild-card file specification. */

/* Invocation:                                                              */
/*    rec_wild_file( fspec, fspec_len, context, alldone, fname, fname_len ) */

/* Description:                                                             */
/*    On the first invocation (when *context is NULL), the routine searches */
/*    the host file system for files matching the supplied wild-card file   */
/*    specification and allocates a struct WLD structure to contain the     */
/*    resulting list of file names. A pointer to the allocated structure, a */
/*    pointer to the first file name and the associated file name length    */
/*    are returned. On subsequent invocations (when a pointer to a          */
/*    previously-allocated struct WLD structure is supplied in *context),   */
/*    the routine simply returns a pointer to the next file name (together  */
/*    with its length) as stored within the structure. The *alldone flag is */
/*    set if no more file names remain.                                     */

/* Parameters:                                                              */
/*    const char *fspec                                                     */
/*       Pointer to a char array containing the wild-card file              */
/*       specification (not null terminated). The syntax depends on the     */
/*       host operating system.  Leading and trailing white space is        */
/*       ignored.                                                           */
/*    INT fspec_len                                                         */
/*       Length of the array holding the wild-card file specification.      */
/*    struct WLD **context                                                  */
/*       If *context is NULL on entry, then a new search is initiated and a */
/*       pointer to the newly-allocated search context structure is         */
/*       returned in *context. Otherwise, *context is used to supply a      */
/*       pointer to a previously-allocated search contect structure from    */
/*       which the next file name will be returned. The context structure   */
/*       supplied will be validated to ensure that it was created by a      */
/*       previous invocation of this routine.                               */
/*    int *alldone                                                          */
/*       If no more file names remain to be returned, then *alldone will be */
/*       set to 1, otherwise it will be set to 0.                           */
/*    char **fname                                                          */
/*       If the routine returns successfully with *alldone is set to zero,  */
/*       then *fname will return a pointer to a null-terminated character   */
/*       string containing the name of the next file matching the wild-card */
/*       file specification. This will be a pointer into memory managed by  */
/*       the search context structure and should not be modified or         */
/*       deallocated by the caller of this routine.                         */
/*    INT *fname_len                                                        */
/*       Pointer to an integer to receive the length of the file name       */
/*       pointed at by *fname (excluding the terminating null).             */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Notes:                                                                   */
/*    -  The files identified by this routine are checked to ensure that    */
/*    they exist at the time of the initial invocation (when *context is    */
/*    supplied as NULL on entry). It is not guaranteed that they will also  */
/*    be accessible (i.e. readable or writable), nor that they will still   */
/*    exist at the time their names are returned from a previously-created  */
/*    context structure by subsequent invocations of the routine.           */
/*    -  All memory allocated by this routine is managed via information    */
/*    stored in the search context structure pointed at by *context. It     */
/*    should only be deallocated via a call to the routine rec_end_wild,    */
/*    which should always be used to terminate a wild-card search when it   */
/*    is complete.                                                          */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */
/*    Copyright (C) 2005 Particle Physics and Astronomy Research Council    */

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
/*    RFWS: R.F. Warren-Smith (STARLINK, RAL)                               */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    17-NOV-1992 (RFWS):                                                   */
/*       Original version.                                                  */
/*    9-DEC-1992 (RFWS):                                                    */
/*       Report an error if the file specification is completely blank.     */
/*    28-DEC-2005 (TIMJ):                                                   */
/*       Use DAT__FLEXT rather than hard-coded ".SDF"                       */
/*     4-MAR-2006 (TIMJ):                                                   */
/*       Use emsSetp to create pointer value error token                    */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Macros:                                                            */
#if defined( vms )               /* VMS version local macros:               */
#define SZFIL 512                /* Maximum size of a file name             */
#endif

/* Local Constants:                                                         */
      const INT mxlist0 = 64;    /* Initial size of file name list          */

#if defined( vms )               /* VMS version local constants:            */
                                 /* None required                           */

#else                            /* Portable version local constants:       */
#if defined( PATH_MAX )
      const INT mxbuf0 = PATH_MAX + 1; /* Initial size of file name buffer  */
#else
      const INT mxbuf0 = _POSIX_PATH_MAX + 1;
#endif
#endif

/* Local Variables:                                                         */
#if defined( vms )               /* VMS version local variables:            */
      INT lname;                 /* Number of characters in file name       */
      INT nfile;                 /* Number of file names found              */
      char *mem;                 /* Pointer to allocated memory             */
      char buffer[ SZFIL ];      /* Buffer for returned file name           */
      struct dsc$descriptor def; /* Descriptor for default file spec        */
      struct dsc$descriptor filespec; /* Descriptor for file specification  */
      struct dsc$descriptor result; /* Descriptor for resultant file name   */
      unsigned int ictx;         /* LIB$FIND_FILE context value             */
      unsigned int systat;       /* System status code                      */

#else                            /* Portable version local variables:       */
      FILE *stream;              /* Stream for reading file names           */
      INT lcwd;                  /* Length of working directory string      */
      INT lname;                 /* Length of full file name                */
      INT mxbuf=0;               /* Allocated size of buffer                */
      INT nfile;                 /* Number of file names found              */
      char *buffer;              /* Pointer to buffer for reading file name */
      char *cwd;                 /* Pointer to working directory string     */
      char *mem;                 /* Pointer to allocated memory             */
      char c;                    /* Character read from pipe                */
      int absolute;              /* Absolute path name obtained?            */
      int lbase;                 /* Number of characters in base file name  */
      int stat_val;              /* Shell process status information        */
      pid_t pid;                 /* ID of shell process                     */
#endif

      INT current;               /* Current file name index                 */
      INT start;                 /* Array offset of first non-blank char    */
      int again;                 /* Loop again?                             */
      int valid;                 /* Wild-card context ID valid?             */
      struct WLD *next;          /* Next wild-card context in queue         */
      struct WLD *qpntr;         /* Pointer into wild-card context queue    */

/* External References:                                                     */
#if defined( vms )               /* VMS version system routines:            */
      unsigned int LIB$FIND_FILE /* Perform wild-card file search           */
         ( struct dsc$descriptor *filespec,
           struct dsc$descriptor *result,
           unsigned int *context,
           struct dsc$descriptor *defspec,
           struct dsc$descriptor *relspec,
           unsigned int *rmsstat,
           unsigned int *flags );
      unsigned int LIB$FIND_FILE_END /* End wild-card file search           */
         ( unsigned int *context );
#endif

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return;

/* Validate an existing context.                                            */
/* ============================                                             */
/* If the wild-card search context supplied was not null, then loop to      */
/* validate it by comparing it with each context currently on the wild-card */
/* context queue.                                                           */
      if ( *context != NULL )
      {
         valid = 0;
         again = 1;
         for( qpntr = rec_gl_wldque; again; qpntr = next )
         {

/* Quit testing if a match is found.                                        */
            if ( qpntr == *context )
            {
               valid = 1;
               break;
            }

/* Otherwise return to test against the next queue element, so long as we   */
/* have not yet returned to the head of the queue.                          */
            next = qpntr->flink;
            again = ( next != rec_gl_wldque );
         }

/* If no match was found, then report an error.                             */
         if ( !valid )
         {
            hds_gl_status = DAT__WLDIN;
            emsSetp( "IWLD", *context );
            emsRep( "REC_WILD_FILE_1",
                       "Wild-card search context identifier is invalid; value \
is ^IWLD (possible programming error).",
                       &hds_gl_status );
         }
      }

/* Validate the file specification.                                         */
/* ===============================                                          */
/* If the context pointer supplied is null, modify the file specification   */
/* length to omit any trailing white space.                                 */
      else
      {

         for ( ; fspec_len > 0; fspec_len-- )
         {
            if ( !isspace( fspec[ fspec_len - 1 ] ) ) break;
         }

/* Also strip white space from the start of the file specification.         */
         for ( start = 0; start < fspec_len; start++ )
         {
            if ( !isspace( fspec[ start ] ) ) break;
         }

/* If the file specification is completely blank, then report an error.     */
         if ( start == fspec_len )
         {
            hds_gl_status = DAT__FILNF;
            emsRep( "REC_WILD_FILE_2",
                       "Invalid blank file specification given.",
                       &hds_gl_status );
         }

/* Allocate a new context.                                                  */
/* ======================                                                   */
/* Allocate space for a new wild-card search context.                       */
         rec_alloc_mem( sizeof( struct WLD ), (void **) context );
         if ( _ok( hds_gl_status ) )
         {

/* Allocate initial space within the context for the list of file name      */
/* structures.                                                              */
            rec_alloc_mem( mxlist0 * sizeof( struct WLD_FILE ),
                           (void **) &(*context)->list );

/* Initialise the context structure.                                        */
            (*context)->mxlist = mxlist0;
            (*context)->nfile = 0;
            (*context)->current = 0;

/* Tidy up if an error occurs.                                              */
            if ( !_ok( hds_gl_status ) )
            {
               rec_deall_mem( mxlist0 * sizeof( struct WLD_FILE ),
                              (void **) &(*context)->list );
            }
         }
         if ( !_ok( hds_gl_status ) )
         {
            rec_deall_mem( sizeof( struct WLD ), (void **) context );
         }

/* If OK, enter the context into the global wild-card context queue.        */
         else
         {
            _insque( *context, rec_gl_wldque )


/* Enter file names.                                                        */
/* ================                                                         */
/* Proceed to fill the new context structure with a list of file names.     */

/* VMS version:                                                             */
/* ===========                                                              */
#if defined( vms )
/* Construct a descriptor for the file specification.                       */
            filespec.dsc$w_length =
                                   (unsigned short int) ( fspec_len - start );
            filespec.dsc$b_dtype = DSC$K_DTYPE_T;
            filespec.dsc$b_class = DSC$K_CLASS_S;
            filespec.dsc$a_pointer = fspec + start;

/* Construct a descriptor for the default file name.                        */
            def.dsc$w_length = (unsigned short int) 7;
            def.dsc$b_dtype = DSC$K_DTYPE_T;
            def.dsc$b_class = DSC$K_CLASS_S;
            def.dsc$a_pointer = "*" DAT__FLEXT ";0";

/* Construct a descriptor to place the returned file name into a local      */
/* buffer.                                                                  */
            result.dsc$w_length = (unsigned short int) SZFIL;
            result.dsc$b_dtype = DSC$K_DTYPE_T;
            result.dsc$b_class = DSC$K_CLASS_S;
            result.dsc$a_pointer = buffer;

/* Loop to obtain file names matching the file specification.               */
            ictx = (unsigned int) 0;
            nfile = 0;
            while ( _ok( hds_gl_status ) )
            {

/* Obtain the next file name.                                               */
               systat = LIB$FIND_FILE( &filespec, &result, &ictx, &def,
                                       (struct dsc$descriptor *) 0,
                                       (unsigned int *) 0,
                                       (unsigned int) 0 );

/* If obtained successfully, check if the file name list in the context     */
/* structure has room to store the name.                                    */
               if ( systat & STS$M_SUCCESS )
               {
                  if ( nfile >= (*context)->mxlist )
                  {

/* If more space is needed, then allocate it by doubling the amount already */
/* allocated and update the record of space allocated.                      */
                     rec_reall_mem( (*context)->mxlist * 2
                                    * (INT) sizeof( struct WLD_FILE ),
                                    (void **) &(*context)->list );
                     if ( _ok( hds_gl_status ) )
                     {
                        (*context)->mxlist *= 2;
                     }
                  }

/* Determine the length of the file name (up to the first blank) and        */
/* allocate space to store it along with a terminating null.                */
                  if ( _ok( hds_gl_status ) )
                  {
                     for ( lname = 0; lname < SZFIL; lname++ )
                     {
                        if ( buffer[ lname ] == ' ' ) break;
                     }
                     rec_alloc_mem( lname + 1, (void **) &mem );
                  }

/* Copy the file name into the allocated memory, terminate it with a null   */
/* and store the pointer in the file name list. Also store the file name    */
/* length in the list and increment the context's file name count.          */
                  if ( _ok( hds_gl_status ) )
                  {
                     (void) memcpy( (void *) mem, (const void *) buffer,
                                    (size_t) lname );
                     mem[ lname ] = '\0';
                     (*context)->list[ nfile ].name = mem;
                     (*context)->list[ nfile ].len = lname;
                     (*context)->nfile = ++nfile;
                  }
               }

/* If all the file names have been obtained, then check that at least one   */
/* was found successfully. If not, then report an error.                    */
               else if ( ( systat == RMS$_NMF ) || ( systat == RMS$_FNF ) )
               {
                  if ( nfile <= 0 )
                  {
                     hds_gl_status = DAT__FILNF;
                     emsSetnc( "FSPEC", fspec + start, fspec_len - start );
                     emsRep( "REC_WILD_FILE_3",
                                "No files found matching the file \
specification \'^FSPEC\'.",
                                &hds_gl_status );
                  }
                  break;
               }

/* If an error occurs while finding files, then make an error report.       */
               else
               {
                  hds_gl_status = DAT__FILNF;
                  emsSetnc( "FSPEC", fspec + start, fspec_len - start );
                  emsSyser( "MESSAGE", systat );
                  emsRep( "REC_WILD_FILE_4",
                             "Error searching for files matching the file \
specification \'^FSPEC\' - ^MESSAGE.",
                             &hds_gl_status );
               }
            }

/* End the LIB$FIND_FILE search context, checking for any errors. Perform   */
/* this in a new error reporting environment, since we may be cleaning up   */
/* after a previous error.                                                  */
            emsBegin( &hds_gl_status );
            systat = LIB$FIND_FILE_END( &ictx );
            if ( !( systat & STS$M_SUCCESS ) )
            {
               hds_gl_status = DAT__FATAL;
               emsSyser( "MESSAGE", systat );
               emsRep( "REC_WILD_FILE_5",
                          "Error ending a VMS wild-card file search context - \
^MESSAGE (internal programming error).",
                          &hds_gl_status );
            }
            emsEnd( &hds_gl_status );

/* Portable version:                                                        */
/* ================                                                         */
#else
/* Create a shell process to search for the required files.                 */
            rec1_find_file( fspec + start, fspec_len - start, &pid, &stream );
            if ( _ok( hds_gl_status ) )
            {

/* Allocate initial space for a buffer to hold the names of the files       */
/* found.                                                                   */
               rec_alloc_mem( mxbuf0, (void **) &buffer );
               if ( _ok( hds_gl_status ) ) mxbuf = mxbuf0;

/* Initialise the current working directory string and loop to read the     */
/* name of each file from the stream connected to the shell process.        */
               cwd = NULL;
               lcwd = 0;
               nfile = 0;
               while ( _ok( hds_gl_status ) )
               {

/* Loop to read characters one at a time and append them to the current     */
/* file name until an error or end of file occurs, or a null character is   */
/* read.                                                                    */
                  lbase = 0;
                  while ( _ok( hds_gl_status ) )
                  {
                     (void) fread( (void *) &c, sizeof( char ), (size_t) 1,
                                   stream );

/* If an error occurred, then report it and quit reading.                   */
                     if ( ferror( stream ) )
                     {
                        hds_gl_status = DAT__FATAL;
                        emsSyser( "MESSAGE", errno );
                        emsRep( "REC_WILD_FILE_6",
                                   "Error reading file names from stream \
connected to shell process - ^MESSAGE",
                                   &hds_gl_status );
                        break;
                     }

/* If an end of file is encountered, then there are no more file names, but */
/* we still have to process the current one. Pretend we read a blank        */
/* character.                                                               */
                     else if ( feof( stream ) )
                     {
                        c = ' ';
                     }

/* The character just read must now be appended to the file name.  Check    */
/* that the file name buffer is large enough to hold it. If not, then       */
/* extend it by doubling its length and record its new size.                */
                     if ( lbase >= mxbuf )
                     {
                        rec_reall_mem( mxbuf * 2, (void **) &buffer );
                        if ( _ok( hds_gl_status ) )
                        {
                           mxbuf *= 2;
                        }
                     }

/* If OK, and the character just read is not a blank, then append it to the */
/* file name.                                                               */
                     if ( _ok( hds_gl_status ) )
                     {
                        if ( !isspace( c ) )
                        {
                           buffer[ lbase++ ] = c;
                        }

/* If it is a blank, then we have reached the end of the current file name, */
/* so terminate it with a null and quit reading.                            */
                        else
                        {
                           buffer[ lbase ] = '\0';
                           break;
                        }
                     }
                  }

/* If there are no characters in the name (apart from the terminating       */
/* null), then we have read all the file names. Check that the name of at   */
/* least one existing file has been obtained and report an error if it has  */
/* not.                                                                     */
                  if ( _ok( hds_gl_status ) )
                  {
                     if ( lbase == 0 )
                     {
                        if ( nfile <= 0 )
                        {
                           hds_gl_status = DAT__FILNF;
                           emsSetnc( "FSPEC", fspec + start,
                                       fspec_len - start );
                           emsRep( "REC_WILD_FILE_7",
                                      "No files found matching the file \
specification \'^FSPEC\'.",
                                      &hds_gl_status );
                        }
                        break;
                     }

/* Check if the file exists (it is possible to produce the names of files   */
/* which do not exist if the original file specification contains no        */
/* pattern matching characters, or patterns which do not match, for         */
/* instance). Simply omit the names of any which do not exist.              */
                     if ( access( (const char *) buffer, F_OK ) == 0 )
                     {

/* Determine if the file name is an absolute path name (i.e. whether it     */
/* starts with '/').  If not, and the current working directory name is not */
/* yet known, then obtain a path name for the current working directory.    */
                        absolute = ( buffer[ 0 ] == '/' );
                        if ( !absolute && ( cwd == NULL ) )
                        {
                           rec1_getcwd( &cwd, &lcwd );
                        }

/* Calculate the full file name length (with the current working directory  */
/* path name added in front, if necessary) and allocate space to store the  */
/* full file name.                                                          */
                        lname = ( absolute ? 0 : lcwd + 1 ) + lbase;
                        rec_alloc_mem( lname + 1, (void **) &mem );

/* Copy the current working directory prefix (if needed) and the file name  */
/* into the allocated memory. Terminate with a null.                        */
                        if ( _ok( hds_gl_status ) )
                        {
                           if ( !absolute )
                           {
                              (void) strcpy( mem, (const char *) cwd );
                              mem[ lcwd ] = '/';
                           }
                           (void) strcpy( mem + ( absolute ? 0 : lcwd + 1 ),
                                          (const char *) buffer );

/* Check if the file name list in the context structure has room to store   */
/* the resulting name.  If more space is needed, then allocate it by        */
/* doubling the amount already allocated and update the record of space     */
/* allocated.                                                               */
                           if ( nfile >= (*context)->mxlist )
                           {
                              rec_reall_mem( (*context)->mxlist * 2
                                             * (INT) sizeof( struct WLD_FILE ),
                                             (void **) &(*context)->list );
                              if ( _ok( hds_gl_status ) )
                              {
                                 (*context)->mxlist *= 2;
                              }
                           }
                        }

/* If OK, then store a pointer to the file name in the search context       */
/* structure along with the file name length.  Increment the context        */
/* structure's file name count.                                             */
                        if ( _ok( hds_gl_status ) )
                        {
                           (*context)->list[ nfile ].name = mem;
                           (*context)->list[ nfile ].len = lname;
                           (*context)->nfile = ++nfile;
                        }

/* If an error occurred, then deallocate the memory reserved to hold the    */
/* full file name.                                                          */
                        else
                        {
                           rec_deall_mem( lname + 1, (void **) &mem );
                        }
                     }
                  }
               }

/* Deallocate the buffer used for reading file names.                       */
               rec_deall_mem( mxbuf, (void **) &buffer );

/* Close the stream connected to the shell process and check for errors. Do */
/* this inside a new error reporting environment, since we may be cleaning  */
/* up after a previous error.                                               */
               emsBegin( &hds_gl_status );
               if ( fclose ( stream ) != 0 )
               {
                  hds_gl_status = DAT__FATAL;
                  emsSyser( "MESSAGE", errno );
                  emsRep( "REC_WILD_FILE_8",
                             "Error closing stream used to read file names \
from a shell process - ^MESSAGE",
                             &hds_gl_status );
               }
               emsEnd( &hds_gl_status );

/* Kill the shell process (just to be sure) and wait for it to terminate.   */
#if HAVE_SYS_WAIT_H
               (void) kill( pid, SIGKILL );
               (void) waitpid( pid, &stat_val, 0 );
#endif
            }
#endif

/* If an error occurred while filling the context structure with file       */
/* names, then annul the search context, deallocating the structure and its */
/* contents.                                                                */
            if ( !_ok( hds_gl_status ) )
            {
               rec_end_wild( context );
            }
         }
      }

/* Extract the next file name.                                              */
/* ==========================                                               */
/* If a context structure has been successfully obtained (either supplied   */
/* by the caller or generated above), then locate the next file name within */
/* it.                                                                      */
      if ( _ok( hds_gl_status ) )
      {

/* First check to see if the file name list has been exhausted.             */
         *alldone = (*context)->current >= (*context)->nfile;

/* If not, then increment the current file counter and return a pointer to  */
/* the next file name along with its length.                                */
         if ( !*alldone )
         {
            current = (*context)->current++;
            *fname = (*context)->list[ current ].name;
            *fname_len = (*context)->list[ current ].len;
         }
      }

/* Exit the routine.                                                        */
      return;
   }
