#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* C include files:                                                         */
/* ===============                                                          */
#include <stddef.h>
#include <ctype.h>

/* VMS version include files:                                               */
/* =========================                                                */
#if defined( vms )
#include <rms.h>                 /* RMS definitions (VMS)                   */
#include <stsdef.h>              /* System status codes (VMS)               */

/* Portable version include files:                                          */
/* ==============================                                           */
#else
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#endif

/* Other include files:                                                     */
/* ===================                                                      */
#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS__ public constants                  */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   void rec1_open_file( int expand, const char *file, INT file_len, char mode,
                        INT *slot, int *newslot )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_open_file                                                        */

/* Purpose:                                                                 */
/*    Open an existing file.                                                */

/* Invocation:                                                              */
/*    rec1_open_file( expand, file, file_len, mode, slot, newslot )         */

/* Description:                                                             */
/*    This function opens an existing container file for reading or writing */
/*    and allocates a new File Control Vector slot (if necessary) to refer  */
/*    to the file. Any new FCV slot is initialised and its number is        */
/*    returned. The file's reference count is left unchanged if it is       */
/*    already in use, or is set to zero if it is being opened for the first */
/*    time.                                                                 */

/* Parameters:                                                              */
/*    int expand                                                            */
/*       If expand is non-zero, then the file name supplied will be         */
/*       regarded as an abbreviated form of the full name of the file and   */
/*       will be expanded (according to the underlying operating system's   */
/*       rules) before use. Otherwise, the file name supplied is regarded   */
/*       as already fully expanded and will be used literally.  This        */
/*       mechanism is provided to allow previously-expanded file names to   */
/*       be given, while allowing for the fact that expanding a file name   */
/*       twice may cause the wrong file to be identified (if the underlying */
/*       file system has changed and/or the expanded file name contains     */
/*       special characters, for instance).                                 */
/*    const char *file                                                      */
/*       Pointer to a char array containing the host file-system name of    */
/*       the container file to be opened. It should not be null terminated. */
/*       If expand is non-zero, then leading and trailing white space will  */
/*       be ignored. If expand is zero, then the file name must be          */
/*       fully-expanded and white space may be significant.                 */
/*    INT file_len                                                          */
/*       Number of characters in the file name (excluding any terminating   */
/*       null, if present).                                                 */
/*    char mode                                                             */
/*       A character specifying the required file access mode: 'R' for      */
/*       read-only access or 'W' for write (or update) access.              */
/*    INT *slot                                                             */
/*       Pointer to an integer in which the File Control Vector slot number */
/*       allocated to the file will be returned.                            */
/*    int *newslot                                                          */
/*       Returns 1 if the FCV slot is a new one, otherwise 0 if the slot    */
/*       was already in use (i.e. the file was already open).               */

/* Returned Value:                                                          */
/*    void                                                                  */

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
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    28-MAR-1991 (RFWS):                                                   */
/*       Added prologue.                                                    */
/*    3-APR-1991 (RFWS):                                                    */
/*       Fixed bug in passing of file name for error message.               */
/*    3-MAY-1991 (RFWS):                                                    */
/*       Re-structured to return an FCV slot number and to initialise the   */
/*       slot if necessary.                                                 */
/*    7-MAY-1991 (RFWS):                                                    */
/*       Added a portable implementation.                                   */
/*    21-MAY-1991 (RFWS):                                                   */
/*       Remove trailing blanks from file names (portable version).         */
/*    22-MAY-1991 (RFWS):                                                   */
/*       Added defaulting of ".sdf" file extension in portable version.     */
/*    12-JUN-1991 (RFWS):                                                   */
/*       Fixed bug in testing of access mode.                               */
/*    28-JUN-1991 (RFWS):                                                   */
/*       Removed initialisation of the VMS-specific FCV lid field (not      */
/*       necessary). Added function prototypes for VMS system calls.        */
/*    11-SEP-1992 (RFWS):                                                   */
/*       Do not increment the file reference count. This is now the         */
/*       caller's responsibility.                                           */
/*    14-OCT-1992 (RFWS):                                                   */
/*       Changed to a void function and to use separate string pointer and  */
/*       length arguments.                                                  */
/*    24-NOV-1992 (RFWS):                                                   */
/*       Fixed error in assigning access mode for error message.            */
/*    25-NOV-1992 (RFWS):                                                   */
/*       Changed to extend the File Control Vector when necessary.          */
/*    26-NOV-1992 (RFWS):                                                   */
/*       Enhanced file name handling by using rec1_get_path.                */
/*    1-DEC-1992 (RFWS):                                                    */
/*       Added the expand parameter.                                        */
/*    28-DEC-2005 (TIMJ):                                                   */
/*       Use DAT__FLEXT rather than hard-coded ".SDF"                       */
/*    02-FEB-2006 (TIMJ):                                                   */
/*       Free malloced memory if the slot is reused.                        */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
#if defined (vms )               /* VMS version local variables:            */
      char esabuf[ NAM$C_MAXRSS ]; /* Expanded file name string buffer      */
      char rsabuf[ NAM$C_MAXRSS ]; /* Resultant file name string buffer     */
      struct FAB fab;            /* RMS file access block                   */
      struct NAM nam;            /* RMS NAM block                           */
      unsigned int systat;       /* System status code                      */
      unsigned short int iochan; /* File I/O channel                        */

#else                            /* Portable version local variables:       */
      FILE *iochan=NULL;         /* File I/O stream                         */
#endif

      INT i;                     /* Loop counter for FCV slots              */
      INT lfns;                  /* Length of File Name String              */
      INT start;                 /* Array offset of first non-blank char    */
      char *fns;                 /* Pointer to file name string             */
      int mustopen=0;            /* File must be opened?                    */
      struct FCV *fcv;           /* Pointer to File Control Vector element  */
      struct FID *fid;           /* Pointer to File ID                      */

/* External References:                                                     */
#if defined( vms )               /* VMS version system calls:               */
      unsigned int SYS$OPEN( struct FAB *fab );
      unsigned int SYS$PARSE( struct FAB *fab );
      unsigned int SYS$SEARCH( struct FAB *fab );
#endif

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return;

/* Initialise.                                                              */
      fns = NULL;
      fid = NULL;

/* If necessary, modify the file name length to omit any trailing white     */
/* space.                                                                   */
      start = 0;
      if ( expand )
      {
         for ( ; file_len > 0; file_len-- )
         {
            if ( !isspace( file[ file_len - 1 ] ) ) break;
         }

/* Also strip white space from the start of the file name (but leave at     */
/* least one character, even if the string is completely blank).            */
         for ( start = 0; start < ( file_len - 1 ); start++ )
         {
            if ( !isspace( file[ start ] ) ) break;
         }
      }

/* VMS version:                                                             */
/* ===========                                                              */
#if defined( vms )

/* Initialise the file FAB and NAM blocks.                                  */
      fab = cc$rms_fab;
      fab.fab$l_dna = DAT__FLEXT;
      fab.fab$b_dns = DAT__SZFLX;
      fab.fab$l_fna = file + start;
      fab.fab$b_fns = file_len - start;
      fab.fab$l_nam = &nam;

      nam = cc$rms_nam;
      nam.nam$l_esa = esabuf;
      nam.nam$b_ess = NAM$C_MAXRSS;
      nam.nam$l_rsa = rsabuf;
      nam.nam$b_rss = NAM$C_MAXRSS;

/* Parse the file name, reporting any errors.                               */
      systat = SYS$PARSE( &fab );
      if ( !( systat & STS$M_SUCCESS ) )
      {
         hds_gl_status = ( systat == RMS$_PRV ) ? DAT__FILPR : DAT__FILNF;
         emsSetnc( "FILE", file + start, file_len - start );
         emsSyser( "MESSAGE", systat );
         emsRep( "REC1_OPEN_FILE_1",
                    "Error in file name \'^FILE\' - ^MESSAGE.",
                    &hds_gl_status );
      }

/*  Search for the file, again reporting errors.                            */
      if ( _ok( hds_gl_status ) )
      {
         systat = SYS$SEARCH( &fab );
         if ( !( systat & STS$M_SUCCESS ) )
         {
            hds_gl_status = ( systat == RMS$_PRV ) ? DAT__FILPR : DAT__FILNF;
            emsSetnc( "FILE", esabuf, nam.nam$b_esl );
            emsSyser( "MESSAGE", systat );
            emsRep( "REC1_OPEN_FILE_2",
                       "Error searching for file ^FILE - ^MESSAGE.",
                       &hds_gl_status );
         }

/* If the file was found successfully, then allocate memory to hold the     */
/* File Name String and the File ID and copy the relevant information from  */
/* the NAM block into this memory (adding a terminating null to the file    */
/* name).                                                                   */
         else
         {
            lfns = nam.nam$b_rsl;
            rec_alloc_mem( lfns + 1, (void **) &fns );
            rec_alloc_mem( sizeof( struct FID ), (void **) &fid );
            if ( _ok( hds_gl_status ) )
            {
               (void) memcpy( (void *) fns, (const void *) nam.nam$l_rsa,
                              (size_t) lfns );
               fns[ lfns ] = '\0';
               (void) memcpy( (void *) fid, (const void *) nam.nam$t_dvi,
                              sizeof( struct FID ) );
            }
         }
      }

/* Portable version:                                                        */
/* ================                                                         */
#else
/* If required, obtain the full path name of the file.                      */
      if ( expand )
      {
         rec1_get_path( file + start, file_len - start, &fns, &lfns );
      }

/* Otherwise, allocate space and copy the file name for use directly.       */
      else
      {
         lfns = file_len - start;
         rec_alloc_mem( lfns + 1, (void **) &fns );
         if ( _ok( hds_gl_status ) )
         {
            (void) memcpy( (void *) fns, (const void *) ( file + start ),
                           (size_t) lfns );
            fns[ lfns ] = '\0';
         }
      }

/* Allocate memory to hold the File ID and store file identification        */
/* information in it.                                                       */
      rec_alloc_mem( sizeof( struct FID ), (void **) &fid );
      rec1_get_fid( fns, fid );
#endif

/* Loop to search the File Control Vector for any slot which is currently   */
/* open and associated with the same file.                                  */
      if ( _ok( hds_gl_status ) )
      {
         *slot = rec_gl_endslot;
         *newslot = 1;
         for ( i = 0; i < rec_gl_endslot; i++ )
         {

/* Remember the number of the last slot which is not being used.            */
            if ( !rec_ga_fcv[ i ].open )
            {
               *slot = i;
            }

/* If a slot is open and the identification matches, then note that a new   */
/* slot is not needed and quit searching                                    */
            else if ( !memcmp( (const void *) rec_ga_fcv[ i ].fid,
                               (const void *) fid, sizeof( struct FID ) ) )
            {
               *slot = i;
               *newslot = 0;
               break;
            }
         }

/* If no File ID match or unused FCV slot was found, then a new slot must   */
/* be used.                                                                 */
         if ( *slot == rec_gl_endslot )
         {

/* If there is insufficient space for another slot in the File Control      */
/* Vector, then extend the FCV by doubling its size. If successful,         */
/* initialise the new region to zero and record the new size.               */
            if ( *slot >= rec_gl_mxslot )
            {
               rec_reall_mem( rec_gl_mxslot * 2 * sizeof( struct FCV ),
                              (void **) &rec_ga_fcv );
               if ( _ok( hds_gl_status ) )
               {
                  (void) memset( (void *) ( rec_ga_fcv + rec_gl_mxslot ), 0,
                                 sizeof( struct FCV ) *
                                 (size_t) rec_gl_mxslot );
                  rec_gl_mxslot *= 2;
               }
            }

/* If OK, increment the count of FCV slots used.                            */
            if ( _ok( hds_gl_status ) )
            {
               rec_gl_endslot++;
            }
         }
      }

/* See if the file needs opening. This will be necessary if a new FCV slot  */
/* is being used or if the file is currently open for read-only access and  */
/* write access is now required.                                            */
      if ( _ok( hds_gl_status ) )
      {
         mustopen = *newslot ||
                    ( ( mode != 'R' ) &&
                      ( rec_ga_fcv[ *slot ].write == REC__NOIOCHAN ) );

/* If the file is to be opened...                                           */
         if ( mustopen )
         {

/* VMS version:                                                             */
/* ===========                                                              */
#if defined( vms )

/* Initialise the FAB block.                                                */
            fab.fab$l_fop = FAB$M_UFO | FAB$M_NAM;
            fab.fab$b_shr = FAB$M_SHRPUT | FAB$M_SHRGET | FAB$M_UPI;
            fab.fab$b_fac = ( mode == 'R' ) ?
                              FAB$M_GET : ( FAB$M_GET | FAB$M_PUT );

/* Open the file, reporting any errors.                                     */
            systat = SYS$OPEN( &fab );
            if ( !( systat & STS$M_SUCCESS ) )
            {
               hds_gl_status = ( systat == RMS$_PRV ) ?
                               DAT__FILPR : DAT__FILNF;
               emsSetnc( "FILE", rsabuf, nam.nam$b_rsl );
               emsSetnc( "ACCESS", ( mode == 'R' ) ?
                                     "reading" : "writing", EMS__SZTOK );
               emsSyser( "MESSAGE", systat );
               emsRep( "REC1_OPEN_FILE_3",
                          "Unable to open file ^FILE for ^ACCESS - ^MESSAGE.",
                          &hds_gl_status );
            }

/* If the file was opened successfully, extract its I/O channel from the    */
/* FAB block.                                                               */
            else
            {
               iochan = (int) fab.fab$l_stv;
            }

/* Portable version:                                                        */
/* ================                                                         */
#else
/* Open the file, checking for errors.                                      */
            iochan = fopen( (const char *) fns,
                            ( mode == 'R' ) ? "rb" : "r+b");
            if ( iochan == NULL )
            {

/* Categorise the possible error conditions, setting the appropriate status */
/* value.                                                                   */
               switch ( errno )
               {
                  case EACCES:
                     hds_gl_status = DAT__FILPR; /* Access denied           */
                     break;
                  case EISDIR:
                     hds_gl_status = DAT__FILIN; /* File is a directory     */
                     break;
                  case EROFS:
                     hds_gl_status = DAT__FILPR; /* Read-only file system   */
                     break;
                  default:                       /* All other errors ==>    */
                     hds_gl_status = DAT__FILNF; /* File not found          */
                     break;
               }

/* Report the error.                                                        */
               emsSyser( "MESSAGE", errno );
               emsSetnc( "FILE", fns, EMS__SZTOK );
               emsSetnc( "ACCESS", ( mode == 'R' ) ? "read" : "read/write",
                           EMS__SZTOK );
               emsRep( "REC1_OPEN_FILE_4",
                          "Error opening file ^FILE for ^ACCESS access - \
^MESSAGE",
                          &hds_gl_status );
            }
#endif
         }
      }

/* If the file has been opened successfully but an old slot has been used,  */
/* then simply store the new I/O channel in the slot.                       */
      if ( _ok( hds_gl_status ) )
      {
         if ( mustopen )
         {
            if ( !*newslot )
            {
               rec_ga_fcv[ *slot ].write = iochan;
            }

/* If a new slot is being used, fill in the File Control Vector fields,     */
/* marking the slot as open.                                                */
            else
            {
               fcv = &rec_ga_fcv[ *slot ];
               fcv->name = fns;
               fcv->fid = fid;
               fcv->read = ( mode == 'R' ) ? iochan : REC__NOIOCHAN;
               fcv->write = ( mode == 'R' ) ? REC__NOIOCHAN : iochan;
               fcv->count = 0;
               fcv->dele = 0;
               fcv->open = 1;
               fcv->locked = 0;
               fcv->hcb = NULL;
               fcv->hcbmodify = 0;
            }
         }
      }

/* If an error occurred, then deallocate any memory allocated for the File  */
/* Name String and File ID.                                                 */
/* Also free the memory if we are reusing a slot (since the name is already */
/* stored and the copy is not used.                                         */
      if ( !_ok( hds_gl_status ) || !*newslot )
      {
         rec_deall_mem( lfns + 1, (void **) &fns );
         rec_deall_mem( sizeof( struct FID ), (void **) &fid );
      }

/* Exit the routine.                                                        */
      return;
   }
