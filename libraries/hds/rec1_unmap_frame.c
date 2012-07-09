#if HAVE_CONFIG_H
#   include <config.h>
#endif

/* VMS version include files:						    */
/* =========================						    */
#if defined( vms )
#include <stddef.h>
#include <stsdef.h>		 /* System status codes (VMS)		    */
#else

/* Include files for version using mmap:				    */
/* ====================================					    */
#if defined( _mmap) || HAVE_MMAP
#include <sys/types.h>           /* OS X requires this */
#include <sys/mman.h>		 /* Definitions for memory management	    */

#if __APPLE__
#undef MS_ASYNC            /* 10.3 always returns an error for MS_ASYNC     */
#define MS_ASYNC MS_SYNC
#endif
#endif

/* Portable version include files:					    */
/* ==============================					    */
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#endif

/* Other include files:							    */
/* ===================							    */
#include "ems.h"		 /* EMS error reporting routines	    */
#include "ems_par.h"		 /* EMS__ public constants		    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */
#include "f77.h"                 /* Fortran <--> C interface facilities     */

   int rec1_unmap_frame( int slot, INT_BIG bloc, INT_BIG length,
                         INT_BIG offset, char mode,
			 unsigned char **pntr )
   {
/*+									    */
/* Name:								    */
/*    rec1_unmap_frame							    */

/* Purpose:								    */
/*    Unmap a frame of blocks from a container file.			    */

/* Invocation:								    */
/*    rec1_unmap_frame( slot, bloc, length, offset, mode, pntr )	    */

/* Description:								    */
/*    This function unmaps all or part of a sequence of blocks from a	    */
/*    container file which have previously been mapped for access using	    */
/*    rec1_map_frame.							    */

/* Parameters:								    */
/*    int slot								    */
/*       Slot number of the container file in the File Control Vector.	    */
/*    int bloc								    */
/*	 Number of the first container file block in the mapped frame (the  */
/*	 first block in the file is no.1).				    */
/*    int length							    */
/*	 Length of the mapped frame in unsigned chars.			    */
/*    int offset							    */
/*       Offset into the first block at which the mapped access starts	    */
/*	 (zero based, in unsigned chars).				    */
/*    char mode								    */
/*       Symbol giving the access mode with which the frame was originally  */
/*	 accessed: 'R' for read access, 'U' for update access, 'W' for	    */
/*	 write access and 'Z' for demand zero access.			    */
/*    unsigned char **pntr						    */
/*	 Pointer to a pointer to the first mapped unsigned char in memory.  */
/*	 A null pointer value is returned.				    */

/* Returned Value:							    */
/*    int rec1_unmap_frame						    */
/*	 The global status value current on exit.			    */

/* Notes:								    */
/*    -  This routine attempts to execute even if the HDS global status is  */
/*    set on entry, although no further error report will be made if it	    */
/*    subsequently fails under these circumstances.			    */
/*    -  Care should be taken that the pointer supplied is valid, since an  */
/*    invalid pointer value may not be detected by this routine. The	    */
/*    routine returns without action if *pntr is null.			    */
/*    -  The value of the global switch hds_gl_map when this routine is	    */
/*    called should be the same as its value when the corresponding call to */
/*    rec1_map_frame was made. This is to ensure that the same method of    */
/*    frame access (I/O or file mapping) is used in both cases.		    */

/*  Copyright:                                                              */
/*    Copyright (C) 1991 Science and Engineering Research Council.          */
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

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    PWD: Peter W. Draper (STARLINK, Durham University)                    */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    25-MAR-1991 (RFWS):						    */
/*	 Original version, re-write of earlier routine.			    */
/*    3-MAY-1991 (RFWS):						    */
/*	 Removed the assumption of page-aligned data by adding the bloc and */
/*	 offset arguments and passing the length argument in unsigned chars */
/*	 instead of blocks.						    */
/*    3-MAY-1991 (RFWS):						    */
/*       Added the mode argument.					    */
/*    8-MAY-1991 (RFWS):						    */
/*	 Added a portable implementation based on file I/O.		    */
/*    10-MAY-1991 (RFWS):						    */
/*	 Changed to cope with "demand zero" access mode.		    */
/*    28-JUN-1991 (RFWS):						    */
/*       Added function prototype for VMS system call.			    */
/*    3-JUL-1991 (RFWS):						    */
/*	 Added VMS I/O based version.					    */
/*    4-JUL-1991 (RFWS):						    */
/*       Added no action if a null pointer value is supplied.		    */
/*    19-DEC-1991 (RFWS):						    */
/*       Changed VMS implementation to save freed virtual addresses for	    */
/*	 later re-use.							    */
/*    24-AUG-1992 (RFWS):						    */
/*       Added sun4 version using file mapping.				    */
/*    7-JUL-1993 (RFWS):						    */
/*       Extended memory mapping for use on all suitable machines.	    */
/*    16-FEB-1999 (RFWS):                                                   */
/*       Deallocate exportable memory.                                      */
/*    08-MAR-2005 (PWD):                                                    */
/*       Call msync(MS_ASYNC) before unmapping memory. This should ensure   */
/*       that it is marked as dirty and requires commit to disk. To be sure */
/*       that data is returned to disk we also need to fsync() when closing */
/*       the file. These changes seem to be needed by the clarifications of */
/*       the single UNIX standard (version 3).                              */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
#if defined( vms )		 /* VMS version local variables:	    */
      int bloc1;		 /* First complete block		    */
      int bloc2;		 /* Last complete block			    */
      int brf;			 /* First block broken?			    */
      int brl;			 /* Last block broken?			    */
      int nbloc;		 /* Number of file blocks mapped	    */
      int tail;			 /* Data mapped after last complete block?  */
      unsigned char *sect;	 /* Pointer to section of allocated memory  */
      unsigned char buffer[ REC__SZBLK ]; /* Block I/O buffer		    */
      unsigned int actrng[ 2 ];  /* Actual address range deleted	    */
      unsigned int range[ 2 ];	 /* Virtual address range to delete	    */
      unsigned int systat;	 /* System status code			    */
#else

#if defined( _mmap) || HAVE_MMAP
                         	 /* Local variables for version using mmap: */
      size_t len;		 /* Length of data to unmap		    */
      unsigned long int ipntr;	 /* Pointer value cast to an integer	    */
      unsigned long int pagesize; /* System page size			    */
      void *addr;		 /* Address of start of mapped section	    */
#endif
				 /* Portable version local variables:	    */
      FILE *iochan;		 /* File I/O stream			    */
      INT_BIG offs;		 /* File offset to start of data	    */
      int writeok;		 /* Write operation completed successfully? */
#endif

/* External References:							    */
#if defined( vms )		 /* VMS version system calls:		    */
      unsigned int SYS$DELTVA	 /* Delete a range of virtual addresses	    */
         ( unsigned int *inadr,
	   unsigned int *retadr,
	   int acmode );	 /* Not used				    */

                                 /* System calls for version using mmap:    */
#elif ( defined( _mmap) || HAVE_MMAP ) && \
      !defined( _POSIX_MAPPED_FILES ) /* Use POSIX.4 mapped files           */
      int munmap		 /* Unmap pages of memory		    */
         ( void *addr,
           size_t len );
#endif

/*.									    */

/* Check that the pointer supplied is not null. There is nothing to do if   */
/* it is.								    */
      if ( *pntr != NULL )
      {

/* Start a new error reporting environment.				    */
         emsBegin( &hds_gl_status );

/* VMS version:								    */
/* ===========								    */
#if defined( vms )

/* Use file mapping if required.					    */
/* ============================						    */
         if ( hds_gl_map )
         {

/* Calculate where the start and end of the frame lie and delete this range */
/* of address spsce.							    */
            range[ 0 ] = (unsigned int) *pntr;
            range[ 1 ] = (unsigned int) ( *pntr + length - 1 );
            systat = SYS$DELTVA( range, actrng, 0 );

/* If an error occurred, then set the global status and report a message.   */
            if ( !( systat & STS$M_SUCCESS ) )
            {
               hds_gl_status = DAT__FILMP;
	       rec1_fmsg( "FILE", slot );
               emsSyser( "MESSAGE", systat );
               emsRep( "REC1_UNMAP_FRAME_1",
	                  "Error unmapping blocks in the file ^FILE - \
^MESSAGE.",
                          &hds_gl_status );
            }

/* Note the range of virtual addresses used for the mapping is now free for */
/* re-use.								    */
            else
	    {
	       rec1_put_addr( (unsigned char *) actrng[ 0 ],
			      (unsigned char *) actrng[ 1 ], &hds_gl_status );
	    }

/* Reset the frame pointer.						    */
            *pntr = NULL;
         }

/* Use I/O if required (still VMS).					    */
/* ===================							    */
         else
         {

/* If the access mode was not "read", then calculate how many disk blocks   */
/* may have to be written.						    */
            if ( mode != 'R' )
            {
               nbloc = 1 + ( offset + length - 1 ) / REC__SZBLK;

/* Calculate the amount by which the mapped data segment extends past the   */
/* last complete block which must be written.				    */
	       tail = ( offset + length ) % REC__SZBLK;

/* See if the first and last blocks to be written are broken (i.e. their    */
/* entire contents have not been mapped).				    */
	       brf = ( offset != 0 );
	       brl = ( tail != 0 );

/* If a subset of only a single block has been mapped, then read that block */
/* and insert the modified region before writing it back to the file.	    */
	       if ( ( nbloc == 1 ) && brf && brl )
	       {
	          rec1_read_file( slot, bloc, 1, buffer );
	          (void) memcpy( (void *) ( buffer + offset ), (void *) *pntr,
				 length );
	          rec1_write_file( slot, 1, buffer, bloc );
	       }

/* Otherwise, handle the data transfer in sections...			    */
	       else
	       {
	          sect = *pntr;

/* If the first block is broken, then read it and insert the modified	    */
/* section before writing it back.					    */
	          if ( brf )
	          {
	             rec1_read_file( slot, bloc, 1, buffer );
		     (void) memcpy( (void *) ( buffer + offset ), (void *) sect,
				    REC__SZBLK - offset );
		     rec1_write_file( slot, 1, buffer, bloc );
		     sect += ( REC__SZBLK - offset );
	          }

/* Calculate the numbers of the first and last blocks to be written in	    */
/* their entirety. If such blocks exist, then write them (there is no need  */
/* to read them first).							    */
	          bloc1 = bloc + ( brf ? 1 : 0 );
	          bloc2 = bloc + nbloc - 1 - ( brl ? 1 : 0 );
	          if ( bloc2 >= bloc1 )
	          {
	             rec1_write_file( slot, bloc2 - bloc1 + 1, sect, bloc1 );
		     sect += ( REC__SZBLK * ( bloc2 - bloc1 + 1 ) );
	          }

/* If the last block is broken, then read it and insert the modified	    */
/* section before writing it back.					    */
	          if ( brl )
	          {
	             rec1_read_file( slot, bloc + nbloc - 1, 1, buffer );
		     (void) memcpy( (void *) buffer, (void *) sect, tail );
	             rec1_write_file( slot, 1, buffer, bloc + nbloc - 1 );
	          }
	       }
            }

/* Deallocate the exportable memory allocated for the mapping.		    */
            rec_deall_xmem( length, (void **) pntr );
         }
#else

/* Version using file mapping (mmap).					    */
/* =================================					    */
#if defined( _mmap) || HAVE_MMAP
	 if ( hds_gl_map )
	 {

/* Determine the system page size.					    */
            pagesize = (unsigned long int) sysconf( _SC_PAGESIZE );

/* Obtain the address of the start of the mapped section of the file	    */
/* (allowing for the system page alignment performed during the mapping	    */
/* operation) along with the length of data to unmap.			    */
	    ipntr = (unsigned long int) *pntr;
	    addr = (void *) ( *pntr - ( ipntr % pagesize ) );
	    len = (size_t) length + (size_t) ( ipntr % pagesize );

/* Unmap the address, reporting any errors.                                  */
/* Also msync the region asynchonously to make sure that the associated      */
/* pages are marked dirty and queued for writing to disk. This write may or  */
/* not actually start after this call (but it will not block, which is why   */
/* we use it in preference to a synchronous call), that seems to depend on   */
/* the operating system. To be sure of writing we must call fsync() when the */
/* file is closed.                                                           */
/*	    printf("Unmapping %ld bytes from address %p (actually %p)\n", len, addr, *pntr );*/

/* Note: under Cygwin/Mingw the msync calls always returns bad, shouldn't    */
/* be critical, so we ignore that.                                           */
#if __MINGW32__ || __CYGWIN__
            msync( addr, len, MS_ASYNC );
            if ( munmap( addr, len ) != 0 )
#else
            if ( ( msync( addr, len, MS_ASYNC ) != 0 ) ||
                 ( munmap( addr, len ) != 0 ) )
#endif
            {
	       hds_gl_status = DAT__FILMP;
	       emsSyser( "MESSAGE", errno );
	       rec1_fmsg( "FILE", slot );
	       emsRepf( "REC1_UNMAP_FRAME_2",
	                "Error unmapping %zu bytes of data in the file ^FILE - ^MESSAGE",
		        &hds_gl_status, len );
	    }

/* Unregister the pointer to the exportable mapped memory and reset the     */
/* pointer value.                                                           */
            cnfUregp( *pntr );
            *pntr = NULL;
	 }
	 else
#endif

/* Portable version:							    */
/* ================							    */
/* If the access mode was not "read", then calculate the file offset at	    */
/* which the data values start.						    */
	 {
            if ( mode != 'R' )
            {
	       offs = ( bloc - 1 ) * REC__SZBLK + offset;

/* Seek to the required file offset and write the mapped values back to the */
/* file.								    */
               iochan = rec_ga_fcv[ slot ].write;
#if HAVE_FSEEKO
	       if ( (writeok = !fseeko( iochan, offs, SEEK_SET )) )
#else
	       if ( (writeok = !fseek( iochan, offs, SEEK_SET )) )
#endif
	       {
	          fwrite( *pntr, 1, length, iochan );

/* Note if an error occurred and clear the error indicator.		    */
	          if ( ferror( iochan ) )
	          {
	             writeok = 0;
	             clearerr( iochan );
	          }
               }

/* Report any errors.							    */
	       if ( !writeok )
	       {
	          hds_gl_status = DAT__FILWR;
	          emsSyser( "MESSAGE", errno );
	          dat1emsSetBigi( "FIRST", offs + 1 );
	          dat1emsSetBigi( "LAST", offs + length );
	          rec1_fmsg( "FILE", slot );
	          emsRep( "REC1_UNMAP_FRAME_3",
	                     "Error writing bytes ^FIRST:^LAST to file ^FILE - \
^MESSAGE",
		             &hds_gl_status );
	       }
            }

/* Deallocate the exportable memory allocated for the mapping.		    */
            rec_deall_xmem( length, (void **) pntr );
	 }
#endif

/* End the error reporting environment.					    */
         emsEnd( &hds_gl_status );
      }

/* Return the global status value.					    */
      return hds_gl_status;
   }
