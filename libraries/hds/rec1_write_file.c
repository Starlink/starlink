#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */

/* VMS version include files:						    */
/* =========================						    */
#if defined( vms )
#include <iodef.h>		 /* I/O function codes (VMS)		    */
#include <stsdef.h>		 /* System status codes (VMS)		    */

/* Portable version include files:					    */
/* ==============================					    */
#else
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>		 /* Needed for gcc on SUN (shouldn't be)    */
#endif

/* Other include files:							    */
/* ===================							    */
#include "ems.h"		 /* EMS error reporting routines	    */
#include "ems_par.h"		 /* EMS__ public constants		    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   int rec1_write_file( int slot, int size, const unsigned char *buffer,
		        int bloc )
   {
/*+									    */
/* Name:								    */
/*    rec1_write_file							    */

/* Purpose:								    */
/*    Write a sequence of blocks to a container file.			    */

/* Invocation:								    */
/*    rec1_write_file( slot, size, buffer, bloc )			    */

/* Description:								    */
/*    This function writes a sequence of contiguous blocks to a container   */
/*    file from the buffer supplied.					    */

/* Parameters:								    */
/*    int slot								    */
/*       The slot number for the container file in the File Control Vector. */
/*    int size								    */
/*	 Number of blocks to be written.				    */
/*    const unsigned char *buffer					    */
/*	 Pointer to a buffer containing the blocks to be written to the	    */
/*	 file.								    */
/*    int bloc								    */
/*	 Number of the first block to be written to the file (the first	    */
/*	 block in the file is number 1).				    */

/* Returned Value:							    */
/*    int rec1_write_file						    */
/*	 The global status value current on exit.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    4-APR-1991 (RFWS):						    */
/*       Original version.						    */
/*    3-MAY-1991 (RFWS):						    */
/*	 Added the maxwrite variable.					    */
/*    7-MAY-1991 (RFWS):						    */
/*       Added a portable implementation.				    */
/*    28-JUN-1991 (RFWS):						    */
/*	 Added function prototype for VMS system call.			    */
/*    2-OCT-1991 (RFWS):						    */
/*       Added check that the container file is open.			    */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
#if defined( vms )		 /* VMS version local variables:	    */
      int bloc0;		 /* Initial starting block number	    */
      int maxwrite;		 /* Max. number of blocks to write at once  */
      int nwrite;		 /* Number of blocks to write at once	    */
      int size0;		 /* Initial number of blocks to write	    */
      unsigned int systat;	 /* System status code			    */
      unsigned short int iochan; /* File I/O channel			    */
      unsigned short int iosb[ 4 ]; /* I/O status block			    */

#else				 /* Portable version local variables:	    */
      FILE *iochan;		 /* File I/O stream			    */
      int writeok;		 /* Write completed successfully?	    */
#endif

/* External References:							    */
#if defined( vms )		 /* VMS version system calls:		    */
      unsigned int SYS$QIOW
         ( int efn,		 /* Not used				    */
	   unsigned short int chan,
	   unsigned int func,
	   unsigned short int *iosb,
	   int astadr,		 /* Not used				    */
	   int astprm,	         /* Not used				    */
	   unsigned char *p1,
	   unsigned int p2,
	   unsigned int p3,
	   int p4,		 /* Not used				    */
	   int p5,		 /* Not used				    */
	   int p6 );		 /* Not used				    */
	   
#endif

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Check that the container file is open and report an error if it is not.  */
      if ( !rec_ga_fcv[ slot ].open )
      {
         hds_gl_status = DAT__FILWR;
         ems_seti_c( "FIRST", bloc );
         ems_seti_c( "LAST", bloc + size - 1 );
         ems_seti_c( "SLOT", slot );
	 ems_rep_c( "REC1_WRITE_FILE_1",
	            "Unable to write blocks ^FIRST:^LAST to file on HDS \
internal slot ^SLOT; container file is not open (internal programming error).",
                    &hds_gl_status );
      }
      else
      {

/* VMS version:								    */
/* ===========								    */
#if defined( vms )

/* Calculate the maximum number of blocks that can be written at one time   */
/* (we are limited by the available number range for the $QIOW byte count). */
         maxwrite = 32767 / REC__SZBLK;

/* Remember the initial block counts (for reporting error messages).	    */
         bloc0 = bloc;
         size0 = size;

/* Obtain the I/O channel to use for writing and loop to write the blocks   */
/* to the file.								    */
         iochan = rec_ga_fcv[ slot ].write;
         while ( ( size > 0 ) && _ok( hds_gl_status ) )
         {

/* Calculate how many blocks to write and transfer them to the file.	    */
            nwrite = size > maxwrite ? maxwrite : size;
            systat = SYS$QIOW( 0, iochan, IO$_WRITEVBLK, iosb, 0, 0,
			       buffer, REC__SZBLK * nwrite, bloc, 0, 0, 0 );

/* Check for errors, making an error report if necessary.		    */
            if ( systat & STS$M_SUCCESS ) systat = iosb[ 0 ];
            if ( !( systat & STS$M_SUCCESS ) )
            {
               hds_gl_status = DAT__FILRD;
               ems_seti_c( "FIRST", bloc0 );
               ems_seti_c( "LAST", bloc0 + size0 - 1 );
	       rec1_fmsg( "FILE", slot );
               ems_syser_c( "MESSAGE", systat );
               ems_rep_c( "REC1_WRITE_FILE_1",
                          "Unable to write blocks ^FIRST:^LAST to file ^FILE - \
^MESSAGE.",
		          &hds_gl_status );
            }

/* Increment/decrement counters for writing the next sequence of blocks	    */
/* from the appropriate part of the input buffer.			    */
            buffer += REC__SZBLK * nwrite;
            bloc += nwrite;
            size -= nwrite;
         }

/* Portable version:							    */
/* ================							    */
#else

/* Seek to the required file offset and write the required number of	    */
/* blocks.								    */
         iochan = rec_ga_fcv[ slot ].write;
         if ( writeok = !fseek( iochan, ( bloc - 1 ) * REC__SZBLK, SEEK_SET ) )
         {
            fwrite( buffer, 1, size * REC__SZBLK, iochan );

/* Note of an error occurred and clear the error indicator.		    */
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
	    ems_setc_c( "MESSAGE", strerror( errno ), EMS__SZMSG );
            ems_seti_c( "FIRST", ( bloc - 1 ) * REC__SZBLK + 1 );
            ems_seti_c( "LAST", ( bloc + size - 1 ) * REC__SZBLK );
	    rec1_fmsg( "FILE", slot );
            ems_rep_c( "REC1_WRITE_FILE_2",
                       "Unable to write bytes ^FIRST:^LAST to the file ^FILE - \
^MESSAGE",
		       &hds_gl_status );
         }
#endif
      }

/* Return the current global status value.				    */
      return hds_gl_status;
   }
