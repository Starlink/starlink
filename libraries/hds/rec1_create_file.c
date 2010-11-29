#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* C include files:							    */
/* ===============							    */
#include <stddef.h>
#include <ctype.h>
#include <unistd.h>

/* VMS version include files:						    */
/* =========================						    */
#if defined( vms )
#include <rms.h>		 /* RMS definitions (VMS)		    */
#include <stsdef.h>		 /* System status codes (VMS)		    */

/* Portable version include files:					    */
/* ==============================					    */
#else
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif

/* Other include files:							    */
/* ===================							    */
#include "ems.h"		 /* EMS error reporting routines	    */
#include "ems_par.h"		 /* EMS__ public constants		    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */
#include "win_fixups.h"          /* Windows special functions               */

   void rec1_create_file( int expand, const char *file, INT file_len,
			  INT size, INT *slot, INT *alq )
   {
/*+									    */
/* Name:								    */
/*    rec1_create_file							    */

/* Purpose:								    */
/*    Create a new container file.					    */

/* Invocation:								    */
/*    rec1_create_file( expand, file, file_len, size, slot, alq )	    */

/* Description:								    */
/*    This function creates a new file for use as an HDS container file.    */
/*    The file is left opened and associated with a File Control Vector	    */
/*    slot, which is initialised to refer to the file. The file reference   */
/*    count is initialised to zero.					    */

/* Parameters:								    */
/*    int expand							    */
/*	 If expand is non-zero, then the file name supplied will be	    */
/*	 regarded as an abbreviated form of the full name of the file and   */
/*	 will be expanded (according to the underlying operating system's   */
/*	 rules) before use. Otherwise, the file name supplied is regarded   */
/*	 as already fully expanded and will be used literally.  This	    */
/*	 mechanism is provided to allow previously-expanded file names to   */
/*	 be given, while allowing for the fact that expanding a file name   */
/*	 twice may cause the wrong file to be identified (if the underlying */
/*	 file system has changed and/or the expanded file name contains	    */
/*	 special characters, for instance).				    */
/*    const char *file							    */
/*	 Pointer to a char array containing the host file-system name of    */
/*	 the container file to be created. It should not be null	    */
/*	 terminated.  If expand is non-zero, then leading and trailing	    */
/*	 white space will be ignored. If expand is zero, then the file name */
/*	 must be fully-expanded and white space may be significant.	    */
/*    INT file_len							    */
/*       Number of characters in the file name.				    */
/*    INT size								    */
/*	 The minimum size of file to be created, in blocks.		    */
/*    INT *slot								    */
/*       Pointer to an integer in which the File Control Vector slot number */
/*	 allocated to the file will be returned.			    */
/*    INT *alq								    */
/*	 Pointer to an integer in which the actual size of file created, in */
/*	 blocks, will be returned. This may be larger then the size	    */
/*	 requested, but should not be smaller.				    */

/* Returned Value:							    */
/*    void								    */

/* Copyright:								    */
/*    Copyright (C) 1992 Science & Engineering Research Council		    */
/*    Copyright (C) 2005 Particle Physics and Astronomy Research Council    */
/*    Copyright (C) 2007,2009 Science and Technology Facilities Council     */

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
/*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,       */
/*     MA 02111-1307, USA                                                   */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    PWD: Peter W. Draper (JAC, Durham University)                         */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    27-MAR-1991 (RFWS):						    */
/*	 Added prologue.						    */
/*    2-MAY-1991 (RFWS):						    */
/*       Changed to leave the file in an open state and to return an FCV    */
/*	 slot number.							    */
/*    8-MAY-1991 (RFWS):						    */
/*       Added a portable implementation.				    */
/*    21-MAY-1991 (RFWS):						    */
/*       Remove trailing blanks from file names (portable version).	    */
/*    22-MAY-1991 (RFWS):						    */
/*       Added defaulting of ".sdf" file extension in portable version.	    */
/*    17-JUL-1991 (RFWS):						    */
/*       Removed reference to FCV lid field from non-VMS version.	    */
/*    11-SEP-1992 (RFWS):						    */
/*	 Changed to initialise the file reference count to zero instead of  */
/*	 one. Incrementing this is now the caller's responsibility.	    */
/*    14-OCT-1992 (RFWS);						    */
/*	 Changed to a void function and to use separate string pointer and  */
/*	 length arguments.						    */
/*    25-NOV-1992 (RFWS):						    */
/*       Changed to extend the File Control Vector when necessary.	    */
/*    26-NOV-1992 (RFWS):						    */
/*	 Enhanced portable implementation by using rec1_get_path to expand  */
/*	 the file name and checking that a file is not already in use by    */
/*	 HDS before using the same name to create a new file.		    */
/*    1-DEC-1992 (RFWS):						    */
/*	 Improved error reporting in portable implementation by		    */
/*	 categorising possible error conditions. Also added initial file    */
/*	 extension to portable version. Added expand parameter.		    */
/*    6-MAY-2004 (BKM):                                                     */
/*       Set appropriate FCV bit for 64-bit files                           */
/*    28-DEC-2005 (TIMJ):                                                   */
/*       Use DAT__FLEXT rather than hard-coded ".SDF"                       */
/*    29-AUG-2007 (PWD):                                                    */
/*       Unlink an existing file before overwriting. This leaves the file   */
/*       available to other processes that may have it opened.              */
/*    31-AUG-2009 (TIMJ):                                                   */
/*       Open the file read/write before calling unlink() since unlink()    */
/*       will delete the file even if it is write protected.                */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
#if defined( vms )		 /* VMS version local variables:	    */
      char buf[ REC__SZBLK ];	 /* Buffer for block I/O operation	    */
      char esabuf[ NAM$C_MAXRSS ]; /* Expanded file name string buffer	    */
      char rsabuf[ NAM$C_MAXRSS ]; /* Resultant file name string buffer	    */
      int iochan;		 /* File I/O channel			    */
      int systat;		 /* System status code			    */
      struct FAB fab;		 /* RMS file access block		    */
      struct NAM nam;		 /* RMS NAM block			    */
      struct RAB rab;		 /* RMS RAB block			    */

#else				 /* Portable version local variables:	    */
      FILE *iochan=NULL;	 /* File I/O stream			    */
      struct stat statbuf;	 /* Buffer for file status information	    */
#endif

      INT i;			 /* Loop counter for FCV slots		    */
      INT lfns;			 /* Length of file name string		    */
      INT start;		 /* Array index of first non-blank char	    */
      char *fns;		 /* Pointer to file name string		    */
      struct FCV *fcv;		 /* Pointer to File Control Vector element  */
      struct FID *fid;		 /* Pointer to File ID			    */
      INT_BIG alql;              /* Temporary variable to return file size  */
      int exists = 0;            /* Set to true when file already exists    */
/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return;

/* Initiallise.								    */
      fns = NULL;
      fid = NULL;

/* If necessary, modify the file name length to omit any trailing white	    */
/* space.								    */
      start = 0;
      if ( expand )
      {
         for ( ; file_len > 0; file_len-- )
         {
	    if ( !isspace( file[ file_len - 1 ] ) ) break;
         }

/* Also strip white space from the start of the file name (but leave at	    */
/* least one character, even if the string is completely blank).	    */
         for ( start = 0; start < ( file_len - 1 ); start++ )
         {
            if ( !isspace( file[ start ] ) ) break;
         }
      }

/* Search for a File Control Vector slot which is not currently associated  */
/* with an open file.							    */
      *slot = rec_gl_endslot;
      for ( i = 0; i < rec_gl_endslot; i++ )
      {
         if ( !rec_ga_fcv[ i ].open )
	 {
	    *slot = i;
	    break;
	 }
      }

/* If no such slot was found, then a new one must be used.		    */
      if ( *slot == rec_gl_endslot )
      {

/* If there is insufficient space for another slot in the File Control	    */
/* Vector, then extend the FCV by doubling its size. If successful,	    */
/* initialise the new region to zero and record the new size.		    */
         if ( *slot >= rec_gl_mxslot )
	 {
	    rec_reall_mem( rec_gl_mxslot * 2 * sizeof( struct FCV ),
			   (void **) &rec_ga_fcv );
	    if ( _ok( hds_gl_status ) )
	    {
	       (void) memset( (void *) ( rec_ga_fcv + rec_gl_mxslot ), 0,
			      sizeof( struct FCV ) * (size_t) rec_gl_mxslot );
	       rec_gl_mxslot *= 2;
	    }
	 }

/* If OK, increment the count of FCV slots used.			    */
	 if ( _ok( hds_gl_status ) )
         {
            rec_gl_endslot++;
	 }
      }

/* VMS version:								    */
/* ===========								    */
#if defined( vms )

/* Initialise the file FAB and NAM blocks.				    */
      if ( _ok( hds_gl_status ) )
      {
         fab = cc$rms_fab;
         fab.fab$l_dna = DAT__FLEXT;
         fab.fab$b_dns = DAT__SZFLX;
         fab.fab$l_fna = file + start;
         fab.fab$b_fns = file_len - start;
         fab.fab$l_nam = &nam;
         fab.fab$l_alq = size;
         fab.fab$l_fop = FAB$M_CBT;
         fab.fab$b_fac = FAB$M_BIO;
         fab.fab$b_rfm = FAB$C_FIX;
         fab.fab$w_mrs = REC__SZBLK;

         nam = cc$rms_nam;
         nam.nam$l_esa = esabuf;
         nam.nam$b_ess = NAM$C_MAXRSS;
         nam.nam$l_rsa = rsabuf;
         nam.nam$b_rss = NAM$C_MAXRSS;

/* Parse the file name, reporting any errors.				    */
         systat = SYS$PARSE( &fab );
         if ( !( systat & STS$M_SUCCESS ) )
         {
            hds_gl_status = ( systat == RMS$_PRV ) ? DAT__FILPR : DAT__FILNF;
            emsSetnc( "FILE", file + start, file_len - start );
            emsSyser( "MESSAGE", systat );
            emsRep( "REC1_CREATE_FILE_1",
                       "Unable to parse file name \'^FILE\' - ^MESSAGE.",
                       &hds_gl_status );
         }
      }

/* Create the file, leaving it open. Report any errors.			    */
      if ( _ok( hds_gl_status ) )
      {
         systat = SYS$CREATE( &fab );
         if ( !( systat & STS$M_SUCCESS) )
         {
            hds_gl_status = ( systat == RMS$_PRV ) ? DAT__FILPR : DAT__FILCR;
            emsSetnc( "FILE", esabuf, nam.nam$b_esl );
            emsSyser( "MESSAGE", systat );
            emsRep( "REC1_CREATE_FILE_2",
                       "Unable to create file ^FILE - ^MESSAGE.",
                       &hds_gl_status );
         }

/* If OK, return the allocated file size.				    */
         else
	 {
            *alq = fab.fab$l_alq;
	 }
      }

/* Connect a record stream to the file for performing block I/O.	    */
      if ( _ok( hds_gl_status ) )
      {
         rab = cc$rms_rab;
         rab.rab$l_fab = &fab;
         rab.rab$l_bkt = fab.fab$l_alq;
         rab.rab$w_rsz = REC__SZBLK;
         rab.rab$l_rbf = buf;
         rab.rab$l_rop = RAB$M_BIO;

         systat = SYS$CONNECT( &rab );

/* Perform a block I/O operation on the last block in the file and then	    */
/* close it (this causes the end-of-file to be updated in the file header). */
         if ( systat & STS$M_SUCCESS ) systat = SYS$WRITE( &rab );
         if ( systat & STS$M_SUCCESS ) systat = SYS$CLOSE( &fab );

/*  Report any errors which occur.					    */
         if ( !( systat & STS$M_SUCCESS ) )
         {
            hds_gl_status = ( systat == RMS$_PRV ) ? DAT__FILPR : DAT__FILWR;
            emsSetnc( "FILE", rsabuf, nam.nam$b_rsl );
            emsSyser( "MESSAGE", systat );
            emsRep( "REC1_CREATE_FILE_3",
                       "Unable to update end-of-file block for file ^FILE - \
^MESSAGE.",
                       &hds_gl_status );
         }
      }

/* Initialise the FAB block for re-opening the file for shared read/write   */
/* access using the "user file open" option.				    */
      if ( _ok( hds_gl_status ) )
      {
         fab.fab$l_fop = FAB$M_UFO | FAB$M_NAM;
         fab.fab$b_shr = FAB$M_SHRPUT | FAB$M_SHRGET | FAB$M_UPI;
         fab.fab$b_fac = FAB$M_GET | FAB$M_PUT;

/* Open the file, reporting any errors.					    */
         systat = SYS$OPEN( &fab );
         if ( !( systat & STS$M_SUCCESS ) )
         {
            hds_gl_status = ( systat == RMS$_PRV ) ? DAT__FILPR : DAT__FILNF;
            emsSetnc( "FILE", rsabuf, nam.nam$b_rsl );
            emsSyser( "MESSAGE", systat );
            emsRep( "REC1_CREATE_FILE_4",
                       "Unable to open file ^FILE for writing - ^MESSAGE.",
                       &hds_gl_status );
         }

/* If the file was opened successfully, extract its I/O channel from the    */
/* FAB block.								    */
	 else
	 {
	    iochan = (int) fab.fab$l_stv;

/* Allocate memory to hold the File Name String and the File ID and copy    */
/* the relevant information from the NAM block into this memory (adding a   */
/* terminating null to the file name).					    */
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

/* Portable version:							    */
/* ================							    */
#else
/* If required, obtain the full path name of the file.			    */
      if ( expand )
      {
         rec1_get_path( file + start, file_len - start, &fns, &lfns );
      }

/* Otherwise, allocate space and copy the file name for use directly.	    */
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

/* Obtain file status information. This will fail if the file does not yet  */
/* exist (normally the case, we expect).				    */
      if ( _ok( hds_gl_status ) )
      {
         if ( stat( fns, &statbuf ) == 0 )
         {
#if __MINGW32__ || __CYGWIN__
             /* Need windows pseudo inode data */
            win_get_inodes( fns, &statbuf.st_ino, &statbuf.st_rdev );
#endif

/* If the file appears to exist already, then loop to search the File	    */
/* Control Vector for any slot which is currently open and associated with  */
/* the same file.							    */
            exists = 1;
            for ( i = 0; i < rec_gl_endslot; i++ )
            {

/* If a slot is open and the file identification matches, then we cannot    */
/* use this file name to create a new file, since it will over-write the    */
/* existing one. Report an error.					    */
               if ( rec_ga_fcv[ i ].open &&
	            ( statbuf.st_ino == rec_ga_fcv[ i ].fid->st_ino ) &&
#if __MINGW32__ || __CYGWIN__
	            ( statbuf.st_rdev == rec_ga_fcv[ i ].fid->st_rdev ) &&
#endif
		    ( statbuf.st_dev == rec_ga_fcv[ i ].fid->st_dev ) )
               {
		  hds_gl_status = DAT__FILIN;
	          emsSetnc( "FILE", fns, EMS__SZTOK );
	          emsRep( "REC1_CREATE_FILE_4",
	                     "The file ^FILE is already in use by HDS; this \
name cannot be used to create a new container file.",
		             &hds_gl_status );
	          break;
               }
	    }
	 }
      }

/* If OK, create the file. To be careful we unlink any existing file so */
/* that it can continue to be used by any processes that have it open   */
/* and then create our new file, which will have a new inode.           */
/* Check for errors first                                               */
      if ( _ok( hds_gl_status ) )
      {
         if ( exists )
         {
/*  unlink() allows us to delete a write-protected file if it is owned  */
/*  by us. To work around this we first try to open the file for read/  */
/*  write (without truncating it) and if we can do that we close and    */
/*  do the unlink. We could also attempt to use the access() or stat()  */
/*  system calls but we want to open the file eventually.               */
           iochan = fopen( fns, "a+b" );

/*  Note unlink may fail because of permissions or because this is a     */
/*  directory. Handle that later.                                        */
           if (iochan) {
             fclose( iochan );
             iochan = NULL;
             unlink( fns );
           }
         }
         iochan = fopen( fns, "w+b" );
	 if ( iochan == NULL )
	 {

/* Categorise the possible error conditions, setting the appropriate status */
/* value.								    */
	    switch ( errno )
	    {
	       case EACCES:
	          hds_gl_status = DAT__FILPR; /* Access denied		    */
		  break;
	       case EISDIR:
		  hds_gl_status = DAT__FILIN; /* File is a directory	    */
		  break;
	       case EROFS:
		  hds_gl_status = DAT__FILPR; /* Read-only file system	    */
		  break;
	       default:			      /* All other errors ==>	    */
		  hds_gl_status = DAT__FILCR; /* File creation error	    */
		  break;
	    }

/* Report the error.							    */
	    emsSyser( "MESSAGE", errno );
	    emsSetnc( "FILE", fns, EMS__SZTOK );
	    emsRep( "REC1_CREATE_FILE_5",
	               "Unable to create file ^FILE - ^MESSAGE",
		       &hds_gl_status );
	 }

/* If OK, allocate memory to hold the File ID and store file identification */
/* information in this memory.						    */
	 else
	 {
	    rec_alloc_mem( sizeof( struct FID ), (void **) &fid );
            rec1_get_fid( fns, fid );
	 }
      }
#endif

/* If an error occurred, then deallocate any memory allocated for the FCV   */
/* slot.								    */
      if ( !_ok( hds_gl_status ) )
      {
	 rec_deall_mem( lfns + 1, (void **) &fns );
	 rec_deall_mem( sizeof( struct FID ), (void **) &fid );
      }

/* Otherwise, fill in the File Control Vector fields, marking the FCV slot  */
/* as open.								    */
      else
      {
         fcv = &rec_ga_fcv[ *slot ];
	 fcv->name = fns;
	 fcv->fid = fid;
	 fcv->read = REC__NOIOCHAN;
         fcv->write = iochan;
	 fcv->count = 0;
	 fcv->dele = 0;
	 fcv->open = 1;
	 fcv->locked = 0;
	 fcv->hcb = NULL;
	 fcv->hcbmodify = 0;
         fcv->hds_version = ( hds_gl_64bit ? REC__VERSION4 : REC__VERSION3 );

/* Extend the file to the required size.				    */

/* VMS version:								    */
/* ===========								    */
#if defined( vms )

/* Not required on VMS systems as the file size is specified when it is	    */
/* created.								    */

/* Portable version:							    */
/* ================							    */
#else
         rec1_extend_file( *slot, (INT_BIG) size, &alql);
         *alq = alql;
#endif
      }

/* Exit the routine.							    */
      return;
   }
