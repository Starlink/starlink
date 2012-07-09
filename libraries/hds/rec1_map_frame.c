#if HAVE_CONFIG_H
#   include <config.h>
#endif

/* C include files:							    */
/* ===============							    */
#include <stddef.h>

/* VMS version include files:						    */
/* =========================						    */
#if defined( vms )
#include <secdef.h>		 /* SEC$M... codes (VMS)		    */
#include <stsdef.h>		 /* System status codes (VMS)		    */
#else

/* Include files for version using mmap:				    */
/* ====================================					    */
#if defined( _mmap) || HAVE_MMAP
#include <sys/types.h>           /* OS X requires this */
#include <sys/mman.h>            /* Memory management definitions	    */
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

   int rec1_map_frame( int slot, INT_BIG bloc, INT_BIG length,
                       INT_BIG offset, char mode, unsigned char **pntr )
   {
/*+									    */
/* Name:								    */
/*    rec1_map_frame							    */

/* Purpose:								    */
/*    Map a frame of blocks from a container file.			    */

/* Invocation:								    */
/*    rec1_map_frame( slot, bloc, length, offset, mode, pntr )		    */

/* Description:								    */
/*    This function maps all or part of a sequence of blocks from a	    */
/*    container file into memory for access and returns an exportable       */
/*    pointer to the contents.				                    */

/* Parameters:								    */
/*    int slot								    */
/*       Slot number of the container file in the File Control Vector.	    */
/*    int bloc								    */
/*       Number of the first file block to be mapped (the first block in    */
/*	 the file is no. 1).						    */
/*    int length							    */
/*	 Number of contiguous unsigned chars (starting at the specified	    */
/*	 offset) which are to be mapped into memory.			    */
/*    int offset							    */
/*	 Offset into the first block at which access is to start	    */
/*	 (zero-based, in unsigned chars).				    */
/*    char mode								    */
/*       Access mode for mapping the file: 'R' for read, 'U' for update,    */
/*	 'W' for write and 'Z' for demand-zero.				    */
/*    unsigned char **pntr						    */
/*	 Returns a pointer to the first mapped unsigned char in memory.	    */

/* Returned Value:							    */
/*    int rec1_map_frame						    */
/*	 The global status value current on exit.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    25-MAR-1991 (RFWS):						    */
/*	 Original version, re-write of earlier routine.			    */
/*    3-MAY-1991 (RFWS):						    */
/*       Changed to remove the assumption of page-aligned data by passing   */
/*	 the length in unsigned chars instead of blocks and adding the	    */
/*	 offset argument.						    */
/*    8-MAY-1991 (RFWS):						    */
/*       Added a portable implementation based on file I/O.		    */
/*    28-JUN-1991 (RFWS):						    */
/*       Added function prototypes for VMS system calls.		    */
/*    4-JUL-1991 (RFWS):						    */
/*       Added VMS I/O based version.					    */
/*    19-AUG-1991 (RFWS):						    */
/*	 Fixed bug: if mode = 'Z' file contents were still being read in    */
/*	 VMS I/O based version.						    */
/*    19-DEC-1991 (RFWS):						    */
/*	 Changed VMS implementation to re-use free addresses for mapping    */
/*	 file sections, if available.					    */
/*    21-JAN-1992 (RFWS):						    */
/*       Modified VMS version to remove dependence of file mapping on the   */
/*	 process paging file quota.					    */
/*    11-AUG-1992 (RFWS):						    */
/*       Added file mapping version for sun4 (based on version written by   */
/*	 Sam Southard, Caltech).					    */
/*    7-JUL-1993 (RFWS):						    */
/*       Extended file mapping for use on any suitable machine.		    */
/*    16-FEB-1999 (RFWS):                                                   */
/*       Allocate exportable memory.                                        */
/*    14-DEC-2010 (TIMJ):                                                   */
/*       Fix cnfRegp error handling logic.                                  */
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
      INT_BIG nbloc;		 /* Number of file blocks to map	    */
      int tail;			 /* Data required after last complete block */
      static unsigned char *guard = NULL; /* Pointer to guard page	    */
      unsigned char *end;	 /* End of free address range		    */
      unsigned char *sect;	 /* Pointer to section of allocated memory  */
      unsigned char *start;	 /* Start of free address range		    */
      unsigned char buffer[ REC__SZBLK ]; /* Block I/O buffer		    */
      unsigned int actrng[ 2 ];	 /* Actual address range used for mapping   */
      unsigned int flags;	 /* Mapping flags			    */
      unsigned int grdrng[ 2 ];	 /* Address range of guard page		    */
      unsigned int oldgrd[ 2 ];	 /* Address range of old guard page	    */
      unsigned int range[ 2 ];	 /* Virtual address range to map into	    */
      unsigned int systat;	 /* System status code			    */
      unsigned short int iochan; /* File I/O channel			    */
#else

#if defined( _mmap) || HAVE_MMAP
                                 /* Local variables for version using mmap: */
      int fd;			 /* File descriptor			    */
      int flags;		 /* Mapping flags			    */
      int pagesize;		 /* System page size			    */
      int prot;			 /* Mapping protection mask		    */
      int reg;                   /* Returned value from cnfRegp            */
      off_t off;		 /* File offset rounded to page size	    */
      size_t len;		 /* No. bytes to map			    */
      unsigned char *start;      /* Address returned by mmap                */
      unsigned char *where;      /* Suggested address to use for mapping    */
#endif
				 /* Portable version local variables	    */
      FILE *iochan;		 /* File I/O stream			    */
      INT_BIG offs;		 /* File offset to start of data	    */
      int readok;		 /* Read operation completed successfully?  */
#endif

/* External References:							    */
#if defined( vms )		 /* VMS version system calls:		    */
      unsigned int LIB$GET_VM_PAGE /* Allocate pages from VM pool	    */
         ( int *npage,
	   unsigned int *base );
      unsigned int LIB$FREE_VM_PAGE /* Return pages to VM pool		    */
         ( int *npage,
	   unsigned int *base );
      unsigned int SYS$CRETVA	 /* Create address space		    */
         ( unsigned int *inadr,
	   int retadr,		 /* Not used				    */
	   int acmode );	 /* Not used				    */
      unsigned int SYS$CRMPSC    /* Create and map section		    */
         ( unsigned int *inadr,
	   unsigned int *retadr,
	   int acmode,		 /* Not used				    */
	   unsigned int flags,
	   int gdsnam,		 /* Not used				    */
	   int ident,		 /* Not used				    */
	   int relpag,		 /* Not used				    */
	   unsigned short int chan,
	   unsigned int pagcnt,
	   unsigned int vbn,
	   int prot,		 /* Not used				    */
	   unsigned int pfc );	 /* Not used				    */
      unsigned int SYS$DELTVA	 /* Delete address space		    */
         ( unsigned int *inadr,
	   int retadr,		 /* Not used				    */
	   int acmode );	 /* Not used				    */
      unsigned int SYS$EXPREG    /* Expand program region		    */
         ( unsigned int pagcnt,
	   unsigned int *retadr,
	   int acmode,		 /* Not used				    */
	   unsigned int region );

                                 /* System calls for version using mmap:    */
#elif ( defined( _mmap) || HAVE_MMAP ) && \
      !defined( _POSIX_MAPPED_FILES ) /* Use POSIX.4 mapped files           */
      void *mmap		 /* Map file contents to pages of memory    */
         ( void *addr,
	   size_t len,
	   int prot,
	   int flags,
	   int fildes,
	   off_t off );
      int munmap		 /* Unmap pages of memory		    */
         ( void *addr,
           size_t len );
#endif

/*.									    */

/* Set an initial null value for the returned pointer.			    */
      *pntr = NULL;

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Get the appropriate I/O channel for mapping the file.		    */
      iochan = rec_ga_fcv[ slot ].write;
      if ( iochan == REC__NOIOCHAN )
      {
         iochan = rec_ga_fcv[ slot ].read;
      }

/* VMS version:								    */
/* ===========								    */
#if defined( vms )

/* Use file mapping if required.					    */
/* ============================						    */
      if ( hds_gl_map )
      {

/* Set up the mapping flags, according to the access mode.		    */
         flags = 0;
         switch ( mode )
         {
            case 'R':
               flags = 0;
               break;
            case 'W':
	       flags = SEC$M_WRT;
	       break;
            case 'U':
	       flags = SEC$M_WRT;
	       break;
	    case 'Z':
	       flags = SEC$M_WRT | SEC$M_DZRO;
	       break;
         }

/* Calculate how many complete disk blocks will have to be mapped.	    */
         nbloc = 1 + ( offset + length - 1 ) / 512;

/* See if a suitable range of free addresses is already available for	    */
/* mapping the file (i.e. has been freed by a previous unmapping	    */
/* operation).								    */
         if ( rec1_get_addr( nbloc * 512, &start, &end ) )
	 {

/* If so, then map the required blocks of the file into this range of	    */
/* addresses.								    */
            range[ 0 ] = (unsigned int) start;
	    range[ 1 ] = (unsigned int) end;
            systat = SYS$CRMPSC( range, actrng, 0, flags, 0, 0, 0, iochan,
				 nbloc, bloc, 0, 0 );

/* If an error occurred, then set the global status and report an error	    */
/* message.								    */
            if ( !( systat & STS$M_SUCCESS ) )
            {
               hds_gl_status = DAT__FILMP;
               dat1emsSetBigi( "START", bloc );
	       dat1emsSetBigi( "END", bloc + nbloc - 1 );
	       emsSetnc( "ACCESS", &mode, 1 );
	       rec1_fmsg( "FILE", slot );
               emsSyser( "MESSAGE", systat );
               emsRep( "REC1_MAP_FRAME_1",
	                  "Error mapping blocks ^START:^END for \'^ACCESS\' \
access in file ^FILE - ^MESSAGE.",
                          &hds_gl_status );

/* Delete any address space which was mapped before the error and return    */
/* the allocated addresses for re-use. Do this inside a new error reporting */
/* environment.								    */
	       if ( ( actrng[ 0 ] != -1 ) || ( actrng[ 1 ] != -1 ) )
	       {
                  systat = SYS$DELTVA( actrng, 0, 0 );
	       }
	       emsBegin( &hds_gl_status );
	       rec1_put_addr( start, end, &hds_gl_status );
	       emsEnd( &hds_gl_status );
            }
	 }

/* If a suitable range of addresses is not already available, then map the  */
/* required blocks of the file specifying that the program region should be */
/* expanded to accommodate the new address space.			    */
         else
	 {
            range[ 0 ] = 0;
	    range[ 1 ] = 0;
            systat = SYS$CRMPSC( range, actrng, 0, flags | SEC$M_EXPREG, 0, 0,
	                         0, iochan, nbloc, bloc, 0, 0 );

/* If an error occurred, then set the global status and report an error	    */
/* message. (Note we defer error reporting before doing this, so that we    */
/* can attempt to recover from the error.)				    */
            if ( !( systat & STS$M_SUCCESS ) )
            {
	       emsMark( );
               hds_gl_status = DAT__FILMP;
               dat1emsSetBigi( "START", bloc );
	       dat1emsSetBigi( "END", bloc + nbloc - 1 );
	       emsSetnc( "ACCESS", &mode, 1 );
	       rec1_fmsg( "FILE", slot );
               emsSyser( "MESSAGE", systat );
               emsRep( "REC1_MAP_FRAME_2",
	                  "Error mapping blocks ^START:^END for \'^ACCESS\' \
access in file ^FILE - ^MESSAGE.",
                          &hds_gl_status );

/* Delete any mappings within the allocated address range. Do not save the  */
/* addresses for re-use, because the program region will re-contract if the */
/* deleted address space extends to the end of it (as here).		    */
	       if ( ( actrng[ 0 ] != -1 ) || ( actrng[ 1 ] != -1 ) )
	       {
                  systat = SYS$DELTVA( actrng, 0, 0 );
	       }

/* Try to allocate the required addresses from the global page pool managed */
/* by LIB$GET_VM_PAGE. There is still a chance there may be some free pages */
/* remaining in this pool due to released memory allocations.		    */
	       systat = LIB$GET_VM_PAGE( &nbloc, &range[ 0 ] );
	       range[ 1 ] = range[ 0 ] + 512 * nbloc - 1;

/* If this failed as well, then the address space is full or too fragmented */
/* to use, so we must give up. If it succeeded, then annul the earlier	    */
/* error and carry on.							    */
               if ( systat & STS$M_SUCCESS )
	       {
		  emsAnnul( &hds_gl_status );

/* Map the required blocks of the file into the allocated range of	    */
/* addresses. (Note this deletes the previous mapping of these addresses to */
/* the system paging file.)						    */
                  systat = SYS$CRMPSC( range, actrng, 0, flags, 0, 0, 0,
				       iochan, nbloc, bloc, 0, 0 );

/* If an error occurred, then set the global status and report an error	    */
/* message.								    */
                  if ( !( systat & STS$M_SUCCESS ) )
                  {
                     hds_gl_status = DAT__FILMP;
                     dat1emsSetBigi( "START", bloc );
	             dat1emsSetBigi( "END", bloc + nbloc - 1 );
	             emsSetnc( "ACCESS", &mode, 1 );
	             rec1_fmsg( "FILE", slot );
                     emsSyser( "MESSAGE", systat );
                     emsRep( "REC1_MAP_FRAME_3",
	                        "Error mapping blocks ^START:^END for \
\'^ACCESS\' access in file ^FILE - ^MESSAGE.",
                                &hds_gl_status );

/* Re-map the addresses to the paging file (pages allocated by		    */
/* LIB$GET_VM_PAGE are initially mapped to this file) and return them to    */
/* the global page pool.						    */
                     systat = SYS$CRETVA( range, 0, 0 );
		     systat = LIB$FREE_VM_PAGE( &nbloc, &range[ 0 ] );
	          }
	       }

/* End deferral of error messages.					    */
	       emsRlse( );
	    }

/* If the file has been mapped successfully to a range of addresses not	    */
/* previously used for file mapping, then we must ensure we also have a	    */
/* "guard page" allocated at a higher address. This page is kept mapped to  */
/* the paging file and acts to prevent the program region being contracted  */
/* again when the address space to which the file is mapped is later	    */
/* deleted. Without this, re-contraction will occur if the file section	    */
/* extends to the end of the program region.				    */
            if ( _ok( hds_gl_status ) )
	    {

/* We need a new guard page if there isn't one already, or if its starting  */
/* address is too low. Expand the program region to obtain a new one if	    */
/* necessary.								    */
	       if ( ( guard == NULL ) |
	            ( guard <= (unsigned char *) actrng[ 1 ] ) )
	       {
                  systat = SYS$EXPREG( (unsigned int) 1, grdrng, 0, 0 );

/* If allocation of a guard page failed, then we cannot continue (to do so  */
/* risks losing the allocated addresses when the file is unmapped and later */
/* finding them re-used for another purpose, with possible disastrous	    */
/* consequences if they have also been re-used to map a file). Set status   */
/* and report an error.							    */
                  if ( !( systat & STS$M_SUCCESS ) )
	          {
                     hds_gl_status = DAT__NOMEM;
	             rec1_fmsg( "FILE", slot );
                     emsSyser( "MESSAGE", systat );
                     emsRep( "REC1_MAP_FRAME_4",
	                        "Unable to allocate a \"guard\" page to \
protect address space while mapping the file ^FILE - ^MESSAGE.",
                                &hds_gl_status );

/* Delete any new address space already created.			    */
	             systat = SYS$DELTVA( actrng, 0, 0 );
	          }

/* If successful, and a guard page was previously allocated, then delete    */
/* its address space and save the addresses it occupied for possible	    */
/* re-use.								    */
		  else
		  {
                     if ( guard != NULL )
	             {
			oldgrd[ 1 ] = oldgrd[ 0 ] = (unsigned int) guard;
			systat = SYS$DELTVA( oldgrd, 0, 0 );
	                rec1_put_addr( guard, guard + 511, &hds_gl_status );
	             }

/* Save a pointer to the latest guard page.				    */
	             guard = (unsigned char *) grdrng[ 0 ];
	          }
	       }
	    }
	 }

/* If the file was mapped successfully, then return a pointer to the first  */
/* byte in memory.							    */
         if ( _ok( hds_gl_status ) )
	 {
	    *pntr = ( (unsigned char *) actrng[ 0 ] ) + offset;
         }
      }

/* Use I/O if required (still VMS).					    */
/* ===================							    */
      else
      {

/* Calculate how many disk blocks may have to be read.			    */
         nbloc = 1 + ( offset + length - 1 ) / REC__SZBLK;

/* Allocate memory for the mapped data.					    */
         rec_alloc_xmem( length, (void **) pntr );
         if ( _ok( hds_gl_status ) )
         {

/* If the access mode is "demand zero", then fill the allocated memory with */
/* zeros.								    */
	    if ( mode == 'Z' )
	    {
	       (void) memset( (void *) *pntr, 0, length );
	    }

/* Otherwise, if the access mode is not write, then read the data from the  */
/* file.								    */
	    else if ( mode != 'W' )
	    {

/* Calculate the amount by which the required data segment extends past the */
/* last complete block which must be read.				    */
	       tail = ( offset + length ) % REC__SZBLK;

/* See if the first and last blocks to be read are broken (i.e. their	    */
/* entire contents are not required).					    */
	       brf = ( offset != 0 );
	       brl = ( tail != 0 );

/* If a subset of only a single block is required, then read that block and */
/* extract the region required.						    */
	       if ( ( nbloc == 1 ) && brf && brl )
	       {
	          rec1_read_file( slot, bloc, 1, buffer );
	          (void) memcpy( (void *) *pntr, (void *) ( buffer + offset ),
		                 length );
	       }

/* Otherwise, handle the data transfer in sections...			    */
	       else
	       {
	          sect = *pntr;

/* If the first block is broken, then read it and extract the required	    */
/* region.								    */
	          if ( brf )
	          {
	             rec1_read_file( slot, bloc, 1, buffer );
		     (void) memcpy( (void *) sect, (void *) ( buffer + offset ),
		                    REC__SZBLK - offset );
		     sect += ( REC__SZBLK - offset );
	          }

/* Calculate the numbers of the first and last blocks to be read in their   */
/* entirety. If such blocks exist, then read them.			    */
	          bloc1 = bloc + ( brf ? 1 : 0 );
	          bloc2 = bloc + nbloc - 1 - ( brl ? 1 : 0 );
	          if ( bloc2 >= bloc1 )
	          {
	             rec1_read_file( slot, bloc1, bloc2 - bloc1 + 1, sect );
		     sect += ( REC__SZBLK * ( bloc2 - bloc1 + 1 ) );
	          }

/* If the last block is broken, then read it and append the required part   */
/* of it.								    */
	          if ( brl )
	          {
	             rec1_read_file( slot, bloc + nbloc - 1, 1, buffer );
		     (void) memcpy( (void *) sect, (void *) buffer, tail );
	          }
	       }
	    }
         }

/* If an error has occurred, then deallocate the memory, causing a null	    */
/* pointer to be returned.						    */
         if ( !_ok( hds_gl_status ) )
         {
	    rec_deall_xmem( length, (void **) pntr );
         }
      }
#else

/* Version using file mapping (mmap).					    */
/* =================================					    */
#if defined( _mmap) || HAVE_MMAP
      if ( hds_gl_map )
      {

/* Set up the mapping protection mask, as required by the access mode.	    */
	 prot = ( mode == 'R' ) ? PROT_READ : ( PROT_READ | PROT_WRITE );

/* Set flags to share mapped data (no private copy).			    */
         flags = MAP_SHARED;

/* Determine the system page size.					    */
         pagesize = (int) sysconf( _SC_PAGESIZE );

/* Calculate the starting offset into the file and round this down to a	    */
/* multiple of the system page size. Calculate the number of bytes to map,  */
/* allowing for this rounding.						    */
	 offs = ( bloc - 1 ) * REC__SZBLK + offset;
	 off = offs - ( offs % pagesize );
	 len = length + ( offs - off );

/* Obtain a file descriptor for the file, checking for errors.		    */
         fd = fileno( iochan );
	 if ( fd == -1 )
	 {
	    hds_gl_status = DAT__FILMP;
            emsSyser( "MESSAGE", errno );
	    rec1_fmsg( "FILE", slot );
	    emsRep( "REC1_MAP_FRAME_5",
                       "Unable to obtain a file descriptor for mapping the \
file ^FILE - ^MESSAGE",
	               &hds_gl_status );
	 }

/* If OK, loop until the required part of the file has been successfully    */
/* mapped or an error occurs. Initially, do not suggest the virtual address */
/* to which it should be mapped.                                            */
	 else
	 {
            where = (unsigned char *) 0;
            while ( 1 )
            {

/* Attempt to map the file.                                                 */
               start = (unsigned char *) mmap( (void *) where, len, prot,
                                               flags, fd, off );

/* If an error occurred, then report an error.				    */
               if ( start == (unsigned char *) -1 )
               {
                  hds_gl_status = DAT__FILMP;
                  emsSyser( "MESSAGE", errno );
                  dat1emsSetBigi( "FIRST", offs + 1 );
                  dat1emsSetBigi( "LAST", offs + length );
		  dat1emsSetBigi( "NB", length );
                  emsSetnc( "ACCESS", &mode, 1 );
                  rec1_fmsg( "FILE", slot );
                  emsRep( "REC1_MAP_FRAME_6",
                             "Error mapping bytes ^FIRST:^LAST (^NB bytes) for "
                             "\'^ACCESS\' access in file ^FILE - ^MESSAGE",
                             &hds_gl_status );
                  break;

/* Otherwise, adjust the returned pointer for the effect of rounding to the */
/* system page size.							    */
               }
               else
               {
                  *pntr = start + ( offs - off );

/* Attempt to register the resulting pointer so it may be exported and used */
/* from both C and Fortran.                                                 */
                  reg = cnfRegp( *pntr );

/* If any other type of error occurred while registering the pointer, then  */
/* report an error, unmap the file and abort.                               */

                  if ( reg == -1 )
                  {
                     hds_gl_status = DAT__FILMP;
                     rec1_fmsg( "FILE", slot );
                     emsRep( "REC1_MAP_FRAME_8",
                                "Error registering a pointer for mapped data "
                                "in the file ^FILE - internal CNF error",
                                &hds_gl_status );
                     (void) munmap( start, len );
                     break;

/* If registration failed because the pointer is not unique when converted  */
/* into Fortran format, then unmap the file and generate a suggested        */
/* virtual address at which the file should be mapped next time in order to */
/* produce a different pointer value. Check for any errors during           */
/* unmapping (report an error and abort if necessary).                      */
                  }
                  else if ( !reg )
                  {
                     if ( munmap( start, len ) != 0 )
                     {
                        hds_gl_status = DAT__FILMP;
                        emsSyser( "MESSAGE", errno );
                        rec1_fmsg( "FILE", slot );
                        emsRepf( "REC1_MAP_FRAME_7",
                                 "Error unmapping %zu bytes of unregistered data in the file ^FILE - "
                                 "^MESSAGE", &hds_gl_status, len );
                        break;
                     }
                     if ( !where ) where = start;
                     where += pagesize;

/* Otherwise, file mapping was successful. If the access mode is demand     */
/* zero, then fill the mapped addresses with zeros.			    */
                  }
                  else
                  {
                     if ( mode == 'Z' )
                     {
                        (void) memset( (void *) *pntr, 0, length );
                     }
                     break;
                  }
               }
            }

	    /* indicate success */
	    /*	    if (hds_gl_status == DAT__OK) {
	      printf("Mapping %llu bytes in file %d at offset %llu to pointer %p (actually %p)\n",
		     (uint64_t)len, fd, (uint64_t)off, *pntr, start );
	    } else {
	      printf("Failed to map %llu bytes to file %d with offset %llu\n",
		     (uint64_t)len, fd, (uint64_t)off );
		     }*/
	 }



/* If an error occurred during file mapping, then return a null pointer	    */
/* value.								    */
	 if ( !_ok( hds_gl_status ) )
	 {
	    *pntr = NULL;
	 }
      }
      else
#endif

/* Portable I/O version:						    */
/* ====================							    */
/* Allocate exportable memory for the mapped frame.			    */
      {
         rec_alloc_xmem( length, (void **) pntr );
         if ( _ok( hds_gl_status ) )
         {

/* If OK, and the access mode is "read" or "update", then calculate the	    */
/* offset into the file at which the required data values start.	    */
	    if ( ( mode == 'R' ) || ( mode == 'U' ) )
	    {
	       offs = ( bloc - 1 ) * REC__SZBLK + offset;

/* Seek to this file offset and read the values into the allocated memory.  */
#if HAVE_FSEEKO
	       if ( (readok = !fseeko( iochan, offs, SEEK_SET )) )
#else
	       if ( (readok = !fseek( iochan, offs, SEEK_SET )) )
#endif
               {
                  fread( (void *) *pntr, 1, length, iochan );

/* Note if an error occurred and clear the error indicator. (Note that	    */
/* end-of-file conditions are ignored because these may happen if the file  */
/* has not yet been physically extended to match its logical extent. In	    */
/* this case junk values should be read.)				    */
	          if ( ferror( iochan ) )
                  {
                     readok = 0;
		     clearerr( iochan );
	          }
               }

/* Report any errors.							    */
	       if ( !readok )
	       {
	          hds_gl_status = DAT__FILRD;
                  emsSyser( "MESSAGE", errno );
	          dat1emsSetBigi( "FIRST", offs + 1 );
	          dat1emsSetBigi( "LAST", offs + length );
	          rec1_fmsg( "FILE", slot );
	          emsRep( "REC1_MAP_FRAME_10",
		             "Error reading bytes ^FIRST:^LAST from file \
^FILE - ^MESSAGE",
			     &hds_gl_status );
	       }
	    }

/* If the access mode is "demand zero", then fill the allocated memory with */
/* zeros.								    */
	    else if ( mode == 'Z' )
	    {
	       (void) memset( (void *) *pntr, 0, length );
	    }
         }

/* If an error has occurred, then deallocate the memory, causing a null	    */
/* pointer to be returned.						    */
         if ( !_ok( hds_gl_status ) )
         {
	    rec_deall_xmem( length, (void **) pntr );
         }
      }
#endif

/*   Return the current global status value.				    */
      return hds_gl_status;
   }
