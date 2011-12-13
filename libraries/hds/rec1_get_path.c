#if HAVE_CONFIG_H
#  include <config.h>
#endif

#if defined( vms )
void rec1_get_path( void ){};    /* This routine not used on VMS systems    */
#else

/* C include files:                                                         */
/* ===============                                                          */
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

/* Posix include files:                                                     */
/* ===================                                                      */
#include <signal.h>
#include <sys/types.h>
#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <unistd.h>

/* Other include files:                                                     */
/* ===================                                                      */
#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS__ public constants                  */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Private rec_ definitions                */
#include "dat1.h"                /* Private DAT_ definitions                */
#include "dat_err.h"             /* DAT__ error code definitions            */

   void rec1_get_path( const char *fname, INT fname_len, char **path,
                       INT *path_len )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_get_path                                                         */

/* Purpose:                                                                 */
/*    Obtain a full path name for a file (UNIX & POSIX only).               */

/* Invocation:                                                              */
/*    rec1_get_path( fname, fname_len, path, path_len )                     */

/* Description:                                                             */
/*    The routine translates a file name, which may be absolute or relative */
/*    and which may contain shell meta-characters, into a full path name. A */
/*    default file type extension of ".sdf" is added if appropriate.        */
/*    Leading and trailing white space is ignored.                          */

/* Parameters:                                                              */
/*    const char *fname                                                     */
/*       Pointer to a char array containing the original file name (not     */
/*       null terminated). The file need not necessarily exist, although    */
/*       the file system will be searched to identify it if shell           */
/*       pattern-matching characters are included.                          */
/*    INT fname_len                                                         */
/*       Number of characters in the original file name.                    */
/*    char **path                                                           */
/*       The value of *path will be set by this routine to point at a       */
/*       null-terminated character string containing the fully-expanded     */
/*       path name for the file. This path name occupies space dynamically  */
/*       allocated by rec_alloc_mem. It should be deallocated by the caller */
/*       (using rec_deall_mem) when no longer required. The amount of space */
/*       allocated is equal to (*path_len + 1) bytes.                       */
/*    INT *path_len                                                         */
/*       Pointer to an integer which will be set to the number of           */
/*       characters in the expanded path name, excluding the terminating    */
/*       null.                                                              */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Notes:                                                                   */
/*    -  If the file specification contains pattern matching characters and */
/*    matches more than one file, then the first match will be used.        */
/*    -  A value of NULL will be returned in *path and a value of zero in   */
/*    *path_len if this routine is called with the global status set, or if */
/*    it should fail for any reason.                                        */
/*    -  This routine is only implemented for UNIX & POSIX systems.         */

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
/*    PWD: Peter W. Draper (STARLINK, Durham University)                    */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    25-NOV-1992 (RFWS):                                                   */
/*       Original version.                                                  */
/*    9-DEC-1992 (RFWS):                                                    */
/*       Report an error if the file name is completely blank.              */
/*    21-JUL-2004 (PWD):                                                    */
/*       Add changes to support MinGW under Windows (no process control,    */
/*       plus Windows filename conventions), so that we can build           */
/*       shareable libraries for JNIHDS.                                    */
/*    28-DEC-2005 (TIMJ):                                                   */
/*       Use DAT__FLEXT rather than hard-coded ".sdf"                       */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Constants:                                                         */
#if defined( PATH_MAX )
      const INT mxbuf0 = PATH_MAX; /* Initial size of file name buffer      */
#else
      const INT mxbuf0 = _POSIX_PATH_MAX;
#endif

/* Local Variables:                                                         */
      FILE *stream;              /* Stream for reading file name            */
      INT i;                     /* Loop counter for file name characters   */
      INT idot=0;                /* Character position of last dot          */
      INT islash=0;              /* Character position of last slash        */
      INT lcwd;                  /* Length of working directory string      */
      INT mxbuf=0;               /* Allocated size of buffer                */
      INT start;                 /* Array index of first non-blank char     */
      char *buffer;              /* Pointer to buffer for reading file name */
      char *cwd;                 /* Pointer to working directory string     */
      char c;                    /* File name character read from stream    */
      const char *basename=NULL; /* Pointer to base file name               */
      int absolute;              /* Absolute path name?                     */
      int extn;                  /* File type extension present?            */
      int lbase=0;               /* Number of characters in basename string */
      int special=0;             /* Special characters in file name?        */
      int stat_val;              /* Shell process status information        */
      pid_t pid;                 /* ID of shell process                     */

#if __MINGW32__
      /* Use Windows separator */
#define SLASH  '\\'
#else
#define SLASH  '/'
#endif

/*.                                                                         */

/* Set initial null values for the returned results.                        */
      *path = NULL;
      *path_len = 0;

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return;

/* Initialise.                                                              */
      buffer = NULL;
      pid = (pid_t) -1;

/* Modify the file name length to omit any trailing white space.            */
      for ( ; fname_len > 0; fname_len-- )
      {
         if ( !isspace( fname[ fname_len - 1 ] ) ) break;
      }

/* Also strip white space from the start of the file specification.         */
      for ( start = 0; start < fname_len; start++ )
      {
         if ( !isspace( fname[ start ] ) ) break;
      }

/* If the file name is completely blank, then report an error.              */
      if ( start == fname_len )
      {
         hds_gl_status = DAT__FILNF;
         emsRep( "REC_GET_PATH_1",
                    "Invalid blank file name given.",
                    &hds_gl_status );
      }

/* Scan the file name, classifying the characters that appear in it.        */
      else
      {
         idot = islash = special = 0;
         for ( i = start; i < fname_len; i++ )
         {
            switch ( fname[ i ] )
            {

/* Note where the last dot '.' occurs.                                      */
               case '.':
                  idot = i + 1;
                  break;

/* Note where the last slash '/' occurs.                                    */
               case SLASH:
                  islash = i + 1;
                  break;

/* Underscore '_' and hyphen '-' are portable file name characters, so take */
/* no action on these.                                                      */
               case '_':
               case '-':
                  break;

/* If any other characters which are not in the POSIX.1 portable filename   */
/* character set are encountered, then note that the file name contains     */
/* special characters.                                                      */
               default:
                  if ( !isalnum( fname[ i ] ) )
                  {
                     special = 1;
                  }
                  break;
            }
         }
      }

/* Ignore the possible presence of special characters if the HDS__SHELL     */
/* tuning parameter specifies that shell expansion of such characters is    */
/* not to occur.                                                            */
#if __MINGW32__
      /* MinGW doesn't offer a shell facility so make sure it is disabled */
      hds_gl_shell = HDS__NOSHELL;
      special = 0;
#else
      if ( hds_gl_shell == HDS__NOSHELL ) special = 0;
#endif

/* If there are no speciaal characters present, then the file name can be   */
/* used as the basis of the full path name, without further translation.    */
      if ( !special )
      {
         basename = fname + start;

/* Note if a file type extension is present, as indicated by a dot '.' in   */
/* the final field of the file name (i.e. after the last slash '/').  Find  */
/* how much of the file name must be used after omitting exactly one final  */
/* dot '.', if present (it is a general rule in HDS that this operation is  */
/* performed on file names before use).                                     */
         extn = ( idot > islash );
         lbase  = fname_len - start - ( idot == fname_len );
      }

/* If special characters are present in the file name, then we must use a   */
/* shell process to interpret them. We only do this if it is really         */
/* necessary, since it is slower. Note that file extension information will */
/* be handled and start a shell process for finding files.                  */
      else
      {
         extn = 1;
         rec1_find_file( fname + start, fname_len - start, &pid, &stream );
         if ( _ok( hds_gl_status ) )
         {

/*  Allocate initial space for a buffer to hold the expanded file name.     */
            rec_alloc_mem( mxbuf0, (void **) &buffer );
            if ( _ok( hds_gl_status ) ) mxbuf = mxbuf0;

/* Read the name of the first file found. Loop to read characters one at a  */
/* time and append them to the file name until an error or end of file      */
/* occurs, or a blank character is read.                                    */
            lbase = 0;
            while ( _ok( hds_gl_status ) )
            {
               (void) fread( (void *) &c, sizeof( char ), (size_t) 1,
                             stream );

/* If an error occurs, then report it and quit reading.                     */
               if ( ferror( stream ) )
               {
                  hds_gl_status = DAT__FATAL;
                  emsSyser( "MESSAGE", errno );
                  emsRep( "REC_GET_PATH_2",
                             "Error reading file names from stream attached \
to shell process - ^MESSAGE",
                             &hds_gl_status );
                  break;
               }

/* If an end of file occurs, or a blank character is read, then we have     */
/* reached the end of the file name, so quit reading.                       */
               else if ( feof( stream ) || isspace( c ) )
               {
                  break;
               }

/* The character just read must now be appended to the file name. Check     */
/* that the file name buffer is large enough to hold it. If not, then       */
/* extend the buffer by doubling its length and record its new size.        */
               if ( lbase >= mxbuf )
               {
                  rec_reall_mem( mxbuf * 2, (void **) &buffer );
                  if ( _ok( hds_gl_status ) )
                  {
                     mxbuf *= 2;
                  }
               }

/* If OK, append the character to the file name.                            */
               if ( _ok( hds_gl_status ) )
               {
                  buffer[ lbase++ ] = c;
               }
            }

/* If no file name characters were read, then there was no file name match, */
/* so report an error.                                                      */
            if ( lbase == 0 )
            {
               hds_gl_status = DAT__FILNF;
               emsSetnc( "FILE", fname + start, fname_len - start );
               emsRep( "REC_GET_PATH_3",
                          "No files found matching the file specification \
\'^FILE\'.",
                          &hds_gl_status );
            }

/* Close the stream and check for errors.  Do this inside a new error       */
/* reporting environment, since we may be cleaning up after a previous      */
/* error.                                                                   */
            emsBegin( &hds_gl_status );
            if ( fclose ( stream ) != 0 )
            {
               hds_gl_status = DAT__FATAL;
               emsSyser( "MESSAGE", errno );
               emsRep( "REC_GET_PATH_4",
                          "Error closing stream used to read file names from \
a shell process - ^MESSAGE",
                          &hds_gl_status );
            }
            emsEnd( &hds_gl_status );

/* Kill the shell process (just to be sure).                                */
#if HAVE_SYS_WAIT_H
            (void) kill( pid, SIGKILL );
#endif

/* Use the expanded file name as the base name for forming the full path    */
/* name (it may not yet have a directory prefix).                           */
            basename = buffer;
         }
      }

/* Determine if the file base name is absolute (i.e. whether it starts with */
/* a slash '/').  If not, then obtain a path name for the current working   */
/* directory.                                                               */
      if ( _ok( hds_gl_status ) )
      {
#if __MINGW32__
         /* Windows has absolute names starting with "x:" where x
          * is the drive letter */
         absolute = ( basename[ 1 ] == ':' );
#else
         absolute = ( basename[ 0 ] == SLASH );
#endif
         if ( !absolute )
         {
            rec1_getcwd( &cwd, &lcwd );
         }

/* Calculate the full path name length (with the current working directory  */
/* path name added in front and a file type extension appended, if          */
/* necessary) and allocate space to store the full path name.               */
         *path_len = ( absolute ? 0 : lcwd + 1 ) + lbase + ( extn ? 0 : 4 );
         rec_alloc_mem( *path_len + 1, (void **) path );

/* Copy the current working directory prefix (if needed), the file base     */
/* name and the file type extension (if needed) into the allocated space.   */
/* Terminate with a null.                                                   */
         if ( _ok( hds_gl_status ) )
         {
            if ( !absolute )
            {
               (void) strcpy( *path, (const char *) cwd );
               (*path)[ lcwd ] = SLASH;
            }
            (void) strncpy( (*path) + ( absolute ? 0 : lcwd + 1 ), basename,
                            (size_t) lbase );
            if ( !extn )
            {
               (void) strcpy( (*path) + ( *path_len - DAT__SZFLX ),
			      DAT__FLEXT );
            }
            (*path)[ *path_len ] = '\0';
         }
      }

/* If necessary, deallocate the buffer used for reading the file base name  */
/* from the shell process and wait for the process to terminate.            */
      if ( special )
      {
         rec_deall_mem( mxbuf, (void **) &buffer );
#if HAVE_SYS_WAIT_H
         if ( pid != (pid_t) -1 ) (void) waitpid( pid, &stat_val, 0 );
#endif
      }

/* If an error occurred, then deallocate any space allocated for the path   */
/* name and return null results.                                            */
      if ( !_ok( hds_gl_status ) )
      {
         rec_deall_mem( *path_len + 1, (void **) path );
         *path_len = 0;
      }

/* Exit the routine.                                                        */
      return;
   }
#endif
