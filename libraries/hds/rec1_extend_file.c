#if HAVE_CONFIG_H
#   include <config.h>
#endif

/* VMS version include files:                                               */
/* =========================                                                */
#if defined( vms )
#include <rms.h>                 /* RMS definitions (VMS)                   */
#include <stsdef.h>              /* System status codes (VMS)               */

/* Portable version include files:                                          */
/* ==============================                                           */
#else
#include <errno.h>
#include <string.h>
#include <stdio.h>
#if defined( sun4 )
#include <unistd.h>              /* For SEEK_SET                            */
#endif
#endif

/* Other include files:                                                     */
/* ===================                                                      */
#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS__ public constants                  */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   INT rec1_extend_file( INT slot, INT_BIG size, INT_BIG *actsize )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_extend_file                                                      */

/* Purpose:                                                                 */
/*    Extend a container file.                                              */

/* Invocation:                                                              */
/*    rec1_extend_file( slot, size, actsize )                               */

/* Description:                                                             */
/*    This function extends an existing (open) container file to make more  */
/*    blocks available at the end of the file.                              */

/* Parameters:                                                              */
/*    INT slot                                                              */
/*       Slot number of the container file in the File Control Vector.      */
/*    INT_BIG size                                                          */
/*       The new size required for the file in blocks.                      */
/*    INT_BIG *actsize                                                      */
/*       Pointer to an integer in which the actual new size of the file (in */
/*       blocks) will be returned. This may be larger than was actually     */
/*       requested, but should not be smaller.                              */

/* Returned Value:                                                          */
/*    INT rec1_extend_file                                                  */
/*       The global status value current on exit.                           */

/* Notes:                                                                   */
/*    The end-of-file block count in the HCB free space stack for the file  */
/*    is not updated by this routine.                                       */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */
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
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    27-MAR-1991 (RFWS):                                                   */
/*       Added prologue.                                                    */
/*    26-APR-1991 (RFWS):                                                   */
/*       Changed to use the file slot number as an argument to specify the  */
/*       file.                                                              */
/*    7-MAY-1991 (RFWS):                                                    */
/*       Added a portable implementation.                                   */
/*    22-MAY-1991 (RFWS):                                                   */
/*       Changed to work in absolute file sizes, rather then incremental    */
/*       sizes.                                                             */
/*    22-MAY-1991 (RFWS):                                                   */
/*       Added an implementation using ftruncate.                           */
/*    23-MAY-1991 (RFWS):                                                   */
/*       Changed the portable implementation to actually extend the file,   */
/*       rather than leaving this to later write operations.                */
/*    28-JUN-1991 (RFWS):                                                   */
/*       Added function prototypes for the VMS system calls, removed a      */
/*       return statement from the middle of the routine and changed        */
/*       non-VMS error messages to report file sizes in bytes.              */
/*    5-DEC-1991 (RFWS):                                                    */
/*       Made file extension in the VMS implementation conditional on the   */
/*       file's required size being greater than its initial size.          */
/*    12-OCT-1992 (RFWS):                                                   */
/*       Added more information to error reports.                           */
/*    13-OCT-1992 (RFWS):                                                   */
/*       Added substantially more error reporting to VMS implementation to  */
/*       obtain information about obscure modes of failure. Fixed bug which */
/*       omitted to update the EOF block if the file didn't need extending  */
/*       (this is actually still needed).                                   */
/*    7-JUL-1993 (RFWS):                                                    */
/*       Removed use of ftruncate.                                          */
/*    22-JUN-2000 (BKM):                                                    */
/*       Revise for extended format (64-bit) HDS files.                     */
/*     4-MAR-2006 (TIMJ):                                                   */
/*       Fix empty-if error. Use dat1emsSetBigi.                            */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
#if defined( vms )               /* VMS version local variables:            */
      char buf[ REC__SZBLK ];    /* Buffer for block I/O operation          */
      int extend;                /* File needs extending?                   */
      int opened;                /* File opened successfully?               */
      struct FAB fab;            /* RMS file access block                   */
      struct NAM nam;            /* RMS NAM block                           */
      struct RAB rab;            /* RMS RAB block                           */
      unsigned int systat;       /* System status code                      */

#else                            /* Portable version local variables:       */
      FILE *iochan;              /* File I/O stream                         */
      int doneok;                /* File extended OK?                       */
      unsigned char charbuf[ 1 ]; /* Character buffer                       */
#endif

/* External References:                                                     */
#if defined( vms )               /* VMS version system calls:               */
      unsigned int SYS$CLOSE( struct FAB *fab );
      unsigned int SYS$CONNECT( struct RAB *rab );
      unsigned int SYS$EXTEND( struct FAB *fab );
      unsigned int SYS$OPEN( struct FAB *fab );
      unsigned int SYS$WRITE( struct RAB *rab );
#endif

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* VMS version:                                                             */
/* ===========                                                              */
#if defined( vms )

/* Because the file will currently be open with the "User File Open"        */
/* option, this routine must open it on a separate stream to enable the use */
/* of the RMS $EXTEND service. Initialise the FAB and NAM blocks.           */
      fab = cc$rms_fab;
      fab.fab$l_nam = &nam;
      fab.fab$l_fop = FAB$M_NAM | FAB$M_CBT;
      fab.fab$b_shr = FAB$M_SHRPUT | FAB$M_SHRGET | FAB$M_UPI;
      fab.fab$b_fac = FAB$M_BIO | FAB$M_PUT;
      nam = cc$rms_nam;

/* Copy the file ID and open the file.                                      */
      (void) memcpy( (void *) nam.nam$t_dvi,
                     (const void *) rec_ga_fcv[ slot ].fid,
                     sizeof( struct FID ) );
      systat = SYS$OPEN( &fab );

/*  Report any errors and note if the file was opened successfully.         */
      opened = 1;
      if ( !( systat & STS$M_SUCCESS ) )
      {
         hds_gl_status = DAT__FILNX;
         rec1_fmsg( "FILE", slot );
         emsSeti( "BLOCKS", size );
         emsSeti( "SIZE", size * REC__SZBLK );
         emsSyser( "MESSAGE", systat );
         emsSeti( "STV", fab.fab$l_stv );
         emsRep( "REC1_EXTEND_FILE_1",
                    "Unable to open the file ^FILE for extension to a size of \
^BLOCKS blocks (^SIZE bytes) - ^MESSAGE (FAB status = ^STV).",
                    &hds_gl_status );
         opened = 0;
      }

/* If OK, save the current file size. If the file needs extending, attempt  */
/* to do so. Note we must check first, because external events may already  */
/* have extended the file without HDS knowing about it (a copy to a disk    */
/* with a different cluster size, for instance).                            */
      if ( _ok( hds_gl_status ) )
      {
         *actsize = fab.fab$l_alq;
         extend = ( size > *actsize );
         if ( extend )
         {
            fab.fab$l_alq = size - *actsize;
            systat = SYS$EXTEND( &fab );
         }

/*  Report any errors.                                                      */
         if ( !( systat & STS$M_SUCCESS ) )
         {
            hds_gl_status = DAT__FILNX;
            rec1_fmsg( "FILE", slot );
            emsSeti( "BLOCKS", size );
            emsSeti( "SIZE", size * REC__SZBLK );
            emsSyser( "MESSAGE", systat );
            emsSeti( "STV", fab.fab$l_stv );
            emsRep( "REC1_EXTEND_FILE_2",
                       "Unable to extend the file ^FILE to a size of ^BLOCKS \
blocks (^SIZE bytes) - ^MESSAGE (FAB status = ^STV).",
                       &hds_gl_status );
         }
      }

/* If the file has been extended, return its new size.                      */
      if ( _ok( hds_gl_status ) )
      {
         if ( extend )
         {
            *actsize += fab.fab$l_alq;
         }

/* A block-I/O write operation must be performed on the last physical block */
/* to ensure that the logical EOF is adjusted to the true end of the file.  */
/* Note that this must be done even if we did not need to extend the file,  */
/* because any external event that may have extended the file will not have */
/* adjusted the EOF position to match. Initialise a RAB block.              */
         rab = cc$rms_rab;
         rab.rab$l_fab = &fab;
         rab.rab$l_bkt = *actsize;
         rab.rab$w_rsz = REC__SZBLK;
         rab.rab$l_rbf = buf;
         rab.rab$l_rop = RAB$M_BIO;

/* Connect the RAB to the file, checking for errors.                        */
         systat = SYS$CONNECT( &rab );
         if ( !( systat & STS$M_SUCCESS ) )
         {
            hds_gl_status = DAT__FILNX;
            rec1_fmsg( "FILE", slot );
            emsSeti( "BLOCKS", size );
            emsSeti( "SIZE", size * REC__SZBLK );
            emsSyser( "MESSAGE", systat );
            emsSeti( "STV", rab.rab$l_stv );
            emsRep( "REC1_EXTEND_FILE_3",
                       "Unable to attach a record stream to the file ^FILE to \
adjust the EOF position after extending to a size of ^BLOCKS blocks (^SIZE \
bytes) - ^MESSAGE (RAB status = ^STV).",
                       &hds_gl_status );
         }
      }

/* Update the EOF position by writing to the final block, checking for      */
/* errors.                                                                  */
      if ( _ok( hds_gl_status ) )
      {
         systat = SYS$WRITE( &rab );
         if ( !( systat & STS$M_SUCCESS ) )
         {
            hds_gl_status = DAT__FILNX;
            rec1_fmsg( "FILE", slot );
            emsSeti( "BLOCKS", size );
            emsSeti( "SIZE", size * REC__SZBLK );
            emsSyser( "MESSAGE", systat );
            emsSeti( "STV", rab.rab$l_stv );
            emsRep( "REC1_EXTEND_FILE_4",
                       "Failed to write to the file ^FILE to adjust the EOF \
position after extending to a size of ^BLOCKS blocks (^SIZE bytes) - ^MESSAGE \
(RAB status = ^STV).",
                       &hds_gl_status );
         }
      }

/* The file must now be closed (if it was originally opened successfully),  */
/* forcing the EOF to be updated in the file header. If this is not done,   */
/* then any subsequent attempt to access the extension will fail. This      */
/* process also disconnects the record access stream. Perform this          */
/* operation within a new error reporting environment, since we may be      */
/* cleaning up after a previous error.                                      */
      if ( opened )
      {
         emsBegin( &hds_gl_status );
         systat = SYS$CLOSE( &fab );

/* Check for errors.                                                        */
         if ( !( systat & STS$M_SUCCESS ) )
         {
            hds_gl_status = DAT__FILNX;
            rec1_fmsg( "FILE", slot );
            emsSeti( "BLOCKS", size );
            emsSeti( "SIZE", size * REC__SZBLK );
            emsSyser( "MESSAGE", systat );
            emsSeti( "STV", fab.fab$l_stv );
            emsRep( "REC1_EXTEND_FILE_5",
                       "Failed to close the file ^FILE after adjusting the EOF \
to give a size of ^BLOCKS blocks (^SIZE bytes) - ^MESSAGE (FAB status = ^STV).",
                       &hds_gl_status );
         }
         emsEnd( &hds_gl_status );
      }

/* Portable version:                                                        */
/* ================                                                         */
#else

/* Obtain the I/O stream to use and seek to the last character in the       */
/* (extended) file. Attempt to read it.                                     */
      iochan = rec_ga_fcv[ slot ].write;
#if HAVE_FSEEKO
      if ( (doneok = !fseeko( iochan, size * REC__SZBLK - 1, SEEK_SET )) )
#else
      if ( (doneok = !fseek( iochan, size * REC__SZBLK - 1, SEEK_SET )) )
#endif
      {
         fread( charbuf, 1, 1, iochan );

/* If an error occurred, then note this fact and clear the error indicator. */
         if ( ferror( iochan ) )
         {
            doneok = 0;
            clearerr( iochan );
         }

/* If an end-of-file condition occurred, then the file needs extending.     */
/* Clear the error indicator and re-position at the character just read.    */
/* Write a value of zero to it (note we must have attempted to read the     */
/* character first, or an existing value may be over-written).              */
         else if ( feof( iochan ) )
         {
            clearerr( iochan );
#if HAVE_FSEEKO
            if ( (doneok = !fseeko( iochan, size * REC__SZBLK - 1, SEEK_SET )))
#else
            if ( (doneok = !fseek( iochan, size * REC__SZBLK - 1, SEEK_SET )))
#endif
            {
               charbuf[ 0 ] = '\0';
               fwrite( charbuf, 1, 1, iochan );

/* If an error occurred then note this fact and clear the error indicator.  */
               if ( ferror( iochan ) )
               {
                  doneok = 0;
                  clearerr( iochan );
               }

/* If OK, then seek to the end of the file (this should probably not be     */
/* necessary, but seems to be so when using the mmap function on SUN        */
/* systems - without it, mapping the extended file generates a SIGBUS       */
/* signal). It should be harmless on other systems.                         */
               else
               {
#if HAVE_FSEEKO
                  doneok = !fseeko( iochan, (off_t) 0, SEEK_END );
#else
                  doneok = !fseek( iochan, (long int) 0, SEEK_END );
#endif
               }
            }
         }
      }

/* If the file was not extended successfully, then report an error.         */
      if ( !doneok )
      {
         hds_gl_status = DAT__FILNX;
         emsSyser( "MESSAGE", errno );
         rec1_fmsg( "FILE", slot );
         dat1emsSetBigi( "BLOCKS", size );
         dat1emsSetBigi( "SIZE", size * REC__SZBLK );
         emsRep( "REC1_EXTEND_FILE_6",
                    "Unable to extend the file ^FILE to a size of ^BLOCKS \
blocks (^SIZE bytes) - ^MESSAGE",
                    &hds_gl_status );
      }

/* If successful, return the new file size.                                 */
      else
      {
         *actsize = size;
      }
#endif

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
