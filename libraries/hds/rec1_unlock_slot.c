#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* VMS version include files:                                               */
/* =========================                                                */
#if defined( vms )
#include <stsdef.h>              /* System status codes (VMS)               */

/* POSIX version include files:                                             */
/* ===========================                                              */
#else
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#endif

/* Other include files:                                                     */
/* ===================                                                      */
#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS__ public constants                  */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec1_unlock_slot( int slot )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_unlock_slot                                                      */

/* Purpose:                                                                 */
/*    Unlock a slot in the File Control Vector.                             */

/* Invocation:                                                              */
/*    rec1_unlock_slot( slot )                                              */

/* Description:                                                             */
/*    This function unlocks a slot in the File Control Vector, thereby      */
/*    allowing other users to write to the associated file. Before the file */
/*    is actually unlocked, any modified Header Control Block information   */
/*    is written back to it, and associated blocks are removed from the     */
/*    Working Page List, with modified blocks being written back to the     */
/*    file. This flushing of cached data occurs even if the slot is not     */
/*    initially locked.                                                     */

/* Parameters:                                                              */
/*    int slot                                                              */
/*       Container file slot number in the File Control Vector.             */

/* Returned Value:                                                          */
/*    int rec1_unlock_slot                                                  */
/*       The global status value current on exit.                           */

/* Notes:                                                                   */
/*    -  This routine attempts to execute even if the HDS global status is  */
/*    set on entry, although no further error report will be made if it     */
/*    subsequently fails under these circumstances.                         */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    26-MAR-1991 (RFWS):                                                   */
/*       Made into a separate module and added prologue.                    */
/*    3-APR-1991 (RFWS):                                                    */
/*       Converted to attempt to execute under error conditions.            */
/*    4-APR-1991 (RFWS):                                                    */
/*       Changed sys_write to rec1_write_file.                              */
/*    16-APR-1991 (RFWS):                                                   */
/*       Changed to deallocate the memory used for holding HCB information. */
/*    17-APR-1991 (RFWS):                                                   */
/*       Only release the lock if all associated data were flushed          */
/*       successfully.                                                      */
/*    22-APR-1991 (RFWS):                                                   */
/*       Report an error if the unlocking operation fails.                  */
/*    23-MAY-1991 (RFWS):                                                   */
/*       Installed a POSIX implementation.                                  */
/*    5-JUN-1991 (RFWS):                                                    */
/*       Changed to take no action if the slot is not initially locked.     */
/*    19-JUN-1991 (RFWS):                                                   */
/*       Further changed to allow flushing of cached information even if    */
/*       the file is not locked.                                            */
/*    28-JUN-1991 (RFWS):                                                   */
/*       Added function prototype for VMS system call.                      */
/*    24-SEP-1991 (RFWS):                                                   */
/*       Fixed bug. Modified blocks were deliberately not being written to  */
/*       their files if the file was marked for deletion. This was unsafe   */
/*       because the block might subsequently need to be re-read before the */
/*       file was actually deleted. All modified blocks are now written     */
/*       back.                                                              */
/*    24-AUG-1992 (RFWS):                                                   */
/*       Removed illegal casts, replaced with (void **) cast.               */
/*    8-NOV-1993 (RFWS):                                                    */
/*       Added flushing of output I/O streams in POSIX version.             */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
#if defined( vms )               /* VMS version local variables:            */
      unsigned int systat;       /* System error code                       */

#elif HAVE_FCNTL		 /* POSIX version local variables:	    */
      int fd;                    /* File descriptor                         */
      struct flock lockbuf;      /* Lock structure for fcntl                */
#endif

      int i;                     /* Loop counter for cached blocks          */
      int wplsize;               /* Size of working page list               */
      struct BCP *bcp;           /* Pointer to Block Control Packet         */
      struct BCP *flink;         /* Pointer to next Block Control Packet    */
      struct BID bid;            /* Block ID                                */
      unsigned char buf[ REC__SZBLK ]; /* Buffer for packed HCB information */

/* External References:                                                     */
#if defined( vms )               /* VMS version system calls:               */
      unsigned int SYS$DEQ
         ( unsigned int lkid,
           int valblk,           /* Not used                                */
           int acmode,           /* Not used                                */
           int flags );          /* Not used                                */
#endif

/*.                                                                         */

/* Begin a new error reporting context.                                     */
      ems_begin_c( &hds_gl_status );

/* See if the file's Header Control Block information is cached.            */
      if ( rec_ga_fcv[ slot ].hcb != NULL )
      {

/* If so, and it has been modified, then pack the HCB information into a    */
/* buffer and write it back to the first block in the file.                 */
         if ( rec_ga_fcv[ slot ].hcbmodify )
         {
            rec1_pack_hcb( rec_ga_fcv[ slot ].hcb, buf );
            rec1_write_file( slot, 1, buf, 1 );
         }

/* Reset the HCB modified flag and deallocate the memory used to hold the   */
/* HCB information.                                                         */
         if ( _ok( hds_gl_status ) )
         {
            rec_ga_fcv[ slot ].hcbmodify = 0;
            rec_deall_mem( sizeof( struct HCB ),
                           (void **) &rec_ga_fcv[ slot ].hcb );
         }
      }

/* Scan through the Working Page List.                                      */
      wplsize = rec_gl_wplsize;
      bcp = rec_ga_wpl;
      for ( i = 0; i < wplsize; i++ )
      {
         flink = bcp->flink;

/* Write any modified blocks back to the container file.                    */
         bid = bcp->bid;
         if ( bid.slot == slot )
         {
            rec1_flush_block( bcp );

/* Deallocate the memory used to hold each block and return its Block       */
/* Control Packet to the Free Page List.                                    */
            rec_deall_mem( REC__SZBLK, (void **) &bcp->bloc );
            (bcp->bid).slot = 0;
            (bcp->bid).bloc = 0;
            bcp->count = 0;
            _remque( bcp, rec_ga_wpl );
            _insque( bcp, rec_ga_fpl );
            rec_gl_wplsize--;
         }
         bcp = flink;
      }

/* POSIX version:                                                           */
/* =============                                                            */
#if !defined( vms )              /* Not required on VMS                     */

/* If the slot is open for writing, then we must flush the I/O stream to    */
/* hand off the file handle (POSIX terminology), since the file might next  */
/* be accessed by another process.                                          */
      if ( _ok( hds_gl_status ) &&
           ( rec_ga_fcv[ slot ].write != REC__NOIOCHAN ) )
      {

/* Flush the stream, checking for errors.                                   */
         if ( fflush( rec_ga_fcv[ slot ].write ) )
         {
            hds_gl_status = DAT__FILWR;
            rec1_fmsg( "FILE", slot );
            ems_setc_c( "MESSAGE", strerror( errno ), EMS__SZMSG );
            ems_rep_c( "REC1_UNLOCK_SLOT_1",
                       "Unable to flush written data to the file ^FILE - \
^MESSAGE",
                       &hds_gl_status );
         }
      }
#endif

/* If there has been no error, consider unlocking the file.                 */
      if ( _ok( hds_gl_status ) )
      {

/* Check that the file is locked. There is nothing more to do if it is not. */
         if ( rec_ga_fcv[ slot ].locked )
         {

/* VMS version:                                                             */
/* ===========                                                              */
#if defined( vms )

/* Release the lock on the container file. If successful, note the file is  */
/* unlocked.                                                                */
            systat = SYS$DEQ( rec_ga_fcv[ slot ].lid, 0, 0, 0 );
            if ( systat & STS$M_SUCCESS )
            {
               rec_ga_fcv[ slot ].locked = 0;
            }

/* Report an error if the unlocking operation failed.                       */
            else
            {
               hds_gl_status = DAT__FILCK;
               rec1_fmsg( "FILE", slot );
               ems_syser_c( "MESSAGE", systat );
               ems_rep_c( "REC1_UNLOCK_SLOT_2",
                          "Unable to unlock the file ^FILE - ^MESSAGE.",
                          &hds_gl_status );
            }

/* POSIX version:                                                           */
/* =============                                                            */
#else
#if HAVE_FCNTL
#if 0                            /* Disabled pending fix for NFS locking    */

/* Set up an flock structure to release locks on the whole file.            */
            lockbuf.l_type = F_UNLCK;
            lockbuf.l_whence = SEEK_SET;
            lockbuf.l_start = 0;
            lockbuf.l_len = 0;

/* Obtain a file descriptor for the file, checking for errors.              */
            fd = fileno( rec_ga_fcv[ slot ].write );
            if ( fd == -1 )
            {
               hds_gl_status = DAT__FILCK;
               ems_setc_c( "MESSAGE", strerror( errno ), EMS__SZMSG );
               rec1_fmsg( "FILE", slot );
               ems_rep_c( "REC1_UNLOCK_SLOT_3",
                          "Unable to obtain a file descriptor for unlocking \
the file ^FILE - ^MESSAGE",
                          &hds_gl_status );
            }

/* Release the lock, checking for errors.                                   */
            else if ( fcntl( fd, F_SETLK, &lockbuf ) == -1 )
            {
               hds_gl_status = DAT__FILCK;
               ems_setc_c( "MESSAGE", strerror( errno ), EMS__SZMSG );
               rec1_fmsg( "FILE", slot );
               ems_rep_c( "REC1_UNLOCK_SLOT_4",
                          "Unable to unlock the file ^FILE - ^MESSAGE",
                          &hds_gl_status );
            }

/* If successful, note the slot is no longer locked.                        */
            else
#endif
#endif
            {
               rec_ga_fcv[ slot ].locked = 0;
            }
#endif
         }
      }

/* End the error reporting context and return the current global status     */
/* value.                                                                   */
      ems_end_c( &hds_gl_status );
      return hds_gl_status;
   }
