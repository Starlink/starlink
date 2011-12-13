#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* VMS version include files:                                               */
/* =========================                                                */
#if defined( vms )
#include <iodef.h>               /* I/O function codes (VMS)                */
#include <stsdef.h>              /* System status codes (VMS)               */

/* Portable version include files:                                          */
/* ==============================                                           */
#else
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>              /* Needed for gcc on SUN (shouldn't be)    */
#endif

/* Other include files:                                                     */
/* ===================                                                      */
#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS__ public constants                  */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec1_read_file( int slot, INT_BIG bloc, int size,
                       unsigned char *buffer )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_read_file                                                        */

/* Purpose:                                                                 */
/*    Read a sequence of blocks from a container file.                      */

/* Invocation:                                                              */
/*    rec1_read_file( slot, bloc, size, buffer )                            */

/* Description:                                                             */
/*    This function reads a sequence of contiguous blocks from a container  */
/*    file into the buffer supplied.                                        */

/* Parameters:                                                              */
/*    int slot                                                              */
/*       The slot number for the container file in the File Control Vector. */
/*    int bloc                                                              */
/*       Number of the first block to be read from the file (the first      */
/*       block in the file is no. 1).                                       */
/*    int size                                                              */
/*       Number of blocks to read.                                          */
/*    unsigned char *buffer                                                 */
/*       Pointer to a buffer to receive the blocks read from the file.      */

/* Returned Value:                                                          */
/*    int rec1_read_file                                                    */
/*       The global status value current on exit.                           */

/* Copyright:                                                               */
/*    Copyright (C) 1991 Science & Engineering Research Council             */
/*    Copyright (C) 2006 Particle Physics and Astronomy Research Council    */

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
/*    4-APR-1991 (RFWS):                                                    */
/*       Changed to be able to read a large number of blocks.               */
/*    3-MAY-1991 (RFWS):                                                    */
/*       Added the maxread variable.                                        */
/*    7-MAY-1991 (RFWS):                                                    */
/*       Added a portable implementation.                                   */
/*    2-OCT-1991 (RFWS):                                                    */
/*       Check that the container file is open and report an error if it is */
/*       not.                                                               */
/*    10-MAY-2006 (TIMJ):                                                   */
/*       Trap requests with zero size read or zero block. Fix empty if.     */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
#if defined( vms )               /* VMS version local variables:            */
      int bloc0;                 /* Initial starting block number           */
      int iochan;                /* File I/O channel                        */
      int maxread;               /* Max. number of blocks to read at once   */
      int nread;                 /* Number of blocks to read at once        */
      int size0;                 /* Initial size value                      */
      int systat;                /* System status code                      */
      short int iosb[4];         /* I/O status block                        */

#else                            /* Portable version local variables:       */
      FILE *iochan;              /* File I/O stream                         */
      int readok;                /* Read completed successfully?            */
#endif

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Check that the container file is open and report an error if it is not.  */
      if ( !rec_ga_fcv[ slot ].open )
      {
         hds_gl_status = DAT__FILRD;
         dat1emsSetBigi( "FIRST", bloc );
         dat1emsSetBigi( "LAST", bloc + size - 1 );
         emsSeti( "SLOT", slot );
         emsRep( "REC1_READ_FILE_1",
                    "Unable to read blocks ^FIRST:^LAST from file on HDS \
internal slot ^SLOT; container file is not open (internal programming error).",
                    &hds_gl_status );
      }
      else
      {
/*  Check the size request                                                  */
	if (size <= 0) {
	  hds_gl_status = DAT__FILRD;
	  dat1emsSetBigi( "SIZE", size );
	  rec1_fmsg( "FILE", slot );
	  emsRep( "REC1_READ_FILE_1a",
                    "Routine rec1_read_file called with an invalid size \
argument of ^SIZE for file ^FILE (internal programming error).",
		  &hds_gl_status );
	  return hds_gl_status;
	}
	if (bloc <= 0) {
	  hds_gl_status = DAT__FILRD;
	  dat1emsSetBigi( "BLOC", bloc );
	  rec1_fmsg( "FILE", slot );
	  emsRep( "REC1_READ_FILE_1b",
                    "Routine rec1_read_file called with an invalid bloc \
argument of ^BLOC for file ^FILE (internal programming error).",
		  &hds_gl_status );
	  return hds_gl_status;
	}

/* Obtain the appropriate I/O channel/descriptor.                           */
         iochan = rec_ga_fcv[ slot ].write;
         if ( iochan == REC__NOIOCHAN )
         {
            iochan = rec_ga_fcv[ slot ].read;
         }

         if (iochan == NULL) {
           hds_gl_status = DAT__FILRD;
           rec1_fmsg( "FILE", slot );
           emsRep( "REC1_READ_FILE_1b2",
                   "iochan could not be obtained for file ^FILE "
                   "(internal programming error).",
                   &hds_gl_status );
           return hds_gl_status;
         }

/* VMS version:                                                             */
/* ===========                                                              */
#if defined( vms )

/* Calculate the maximum number of blocks that can be read at one time (we  */
/* are limited by the available number range for the $QIO byte count).      */
         maxread = 32767 / REC__SZBLK;

/* Remember the initial block number and size (for reporting errors).       */
         bloc0 = bloc;
         size0 = size;

/* Loop to read the blocks from the file.                                   */
         while ( ( size > 0 ) && _ok( hds_gl_status ) )
         {

/* Calculate how many blocks to read.                                       */
            nread = size > maxread ? maxread : size;
            systat = SYS$QIOW( 0, (short int) iochan, IO$_READVBLK, iosb, 0, 0,
                               buffer, REC__SZBLK * nread, bloc, 0, 0, 0 );

/* Check for errors, making an error report if necessary.                   */
            if ( systat & STS$M_SUCCESS ) systat = iosb[0];
            if ( !( systat & STS$M_SUCCESS ) )
            {
               hds_gl_status = DAT__FILRD;
               emsSeti( "FIRST", bloc0 );
               emsSeti( "LAST", bloc0 + size0 - 1 );
               rec1_fmsg( "FILE", slot );
               emsSyser( "MESSAGE", systat );
               emsRep( "REC1_READ_FILE_2",
               "Unable to read blocks ^FIRST:^LAST from file ^FILE - ^MESSAGE.",
                          &hds_gl_status );
            }

/* Increment/decrement counters for reading the next sequence of blocks     */
/* into the appropriate part of the output buffer.                          */
            buffer += REC__SZBLK * nread;
            bloc += nread;
            size -= nread;
         }

/* Portable version:                                                        */
/* ================                                                         */
#else

/* Seek to the required starting position in the file and read the required */
/* number of blocks.                                                        */
#if HAVE_FSEEKO
	 /* make sure the bloc number is non-zero before this line */
         if ( (readok = !fseeko( iochan, ( bloc - 1 )*REC__SZBLK, SEEK_SET ) ) )
#else
	 if ( (readok = !fseek( iochan, ( bloc - 1 )*REC__SZBLK, SEEK_SET ) ) )
#endif
         {
	   fread( buffer, 1, (size_t)(size * REC__SZBLK), iochan );

/* Note if an error occurred and clear the error indicator. (Note that      */
/* end-of-file conditions are ignored because these may happen if the file  */
/* has not yet been physically extended to match its logical extent. In     */
/* this case junk values should be read.)                                   */
            if ( ferror( iochan ) )
            {
               readok = 0;
               clearerr( iochan );
            }
         }

/* Report any errors.                                                       */
         if ( !readok )
         {
            hds_gl_status = DAT__FILRD;
            emsSyser( "MESSAGE", errno );
            dat1emsSetBigi( "FIRST", bloc );
            dat1emsSetBigi( "LAST", bloc + size - 1 );
            rec1_fmsg( "FILE", slot );
            emsRep( "REC1_READ_FILE_3",
                       "Unable to read blocks ^FIRST:^LAST from file ^FILE - \
^MESSAGE.",
                       &hds_gl_status );
         }
#endif
      }

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
