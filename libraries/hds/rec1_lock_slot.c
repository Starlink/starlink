#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* VMS version include files:                                               */
/* =========================                                                */
#if defined( vms )
#include <descrip.h>             /* Data descriptor definitions (VMS)       */
#include <lckdef.h>              /* Lock manager definitions (VMS)          */
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
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec1_lock_slot( int slot )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_lock_slot                                                        */

/* Purpose:                                                                 */
/*    Lock a slot in the File Control Vector.                               */

/* Invocation:                                                              */
/*    rec1_lock_slot( slot )                                                */

/* Description:                                                             */
/*    This function locks a slot in the File Control Vector. This results   */
/*    in exclusive write access being granted to the current process for    */
/*    the associated file (if not currently granted to another process).    */

/* Parameters:                                                              */
/*    int slot                                                              */
/*       Slot number of the container file in the File Control Vector.      */

/* Returned Value:                                                          */
/*    int rec1_lock_slot                                                    */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    26-MAR-1991 (RFWS):                                                   */
/*       Made into a separate module and added prologue.                    */
/*    4-APR-1991 (RFWS):                                                    */
/*       Added error reporting and fixed bug where _strinit was used        */
/*       instead of _dscinit.                                               */
/*    25-APR-1991 (RFWS):                                                   */
/*       Installed call to rec1_fmsg.                                       */
/*    23-MAY-1991 (RFWS):                                                   */
/*       Installed a POSIX implementation.                                  */
/*    28-JUN-1991 (RFWS):                                                   */
/*       Added function prototype for VMS system call, added explicit use   */
/*       of a VMS descriptor for the lock resource name and improved the    */
/*       status checking after calling SYS$ENQW.                            */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
#if defined( vms )               /* VMS version local variables:            */
      struct dsc$descriptor locres; /* Descriptor for lock resource name    */
      unsigned int flags;        /* SYS$ENQW flags argument                 */
      unsigned int lksb[ 2 ];    /* Lock status block                       */
      unsigned int systat;       /* System status code                      */

#elif HAVE_FCNTL		 /* POSIX version local variables:	    */
      int fd;                    /* File descriptor                         */
      struct flock lockbuf;      /* Lock structure for fcntl                */
#endif

/* External References:                                                     */
#if defined( vms )               /* VMS version system calls:               */
      unsigned int SYS$ENQW
         ( int efn,              /* Not used                                */
           unsigned int lkmode,
           unsigned int *lksb,
           unsigned int flags,
           struct dsc$descriptor *resnam,
           int parid,            /* Not used                                */
           int astadr,           /* Not used                                */
           int astprm,           /* Not used                                */
           int blkast,           /* Not used                                */
           int acmode,           /* Not used                                */
           int nullarg );        /* Not used                                */

#endif

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Check that the slot is not already locked. There is nothing to do if it  */
/* is.                                                                      */
      if ( !rec_ga_fcv[ slot ].locked )
      {

/* VMS version:                                                             */
/* ===========                                                              */
#if defined( vms )

/* Set the system-wide $ENQW flag if necessary.                             */
         flags = hds_gl_syslck ? LCK$M_SYSTEM : 0;

/* Determine if the process is to wait if the file is currently locked by   */
/* another user.                                                            */
         if ( !hds_gl_wait ) flags |= LCK$M_NOQUEUE;

/* Use the File ID as the lock resource name. Construct a VMS descriptor    */
/* for the name and attempt to set the lock.                                */
         locres.dsc$w_length = (unsigned short int) sizeof( struct FID );
         locres.dsc$b_dtype = DSC$K_DTYPE_T;
         locres.dsc$b_class = DSC$K_CLASS_S;
         locres.dsc$a_pointer = rec_ga_fcv[ slot ].fid;
         systat = SYS$ENQW( 0, LCK$K_PWMODE, lksb, flags, &locres,
                            0, 0, 0, 0, 0, 0 );

/* Report an error if the lock cannot be granted.                           */
         if ( systat & STS$M_SUCCESS ) systat = lksb[ 0 ] & 0xffff;
         if ( !( systat & STS$M_SUCCESS ) )
         {
            hds_gl_status = DAT__FILCK;
            rec1_fmsg( "FILE", slot );
            emsSyser( "MESSAGE", systat );
            emsRep( "REC1_LOCK_SLOT_1",
                       "Unable to lock the file ^FILE for exclusive write \
access - ^MESSAGE.",
                       &hds_gl_status );
         }

/* Extract the Lock ID from the Lock Status Block.                          */
         else
         {
            rec_ga_fcv[ slot ].lid = lksb[ 1 ];
            rec_ga_fcv[ slot ].locked = 1;
         }

/* POSIX version:                                                           */
/* =============                                                            */
#else
#if HAVE_FCNTL
#if 0                            /* Disabled pending fix for NFS locking    */
/* Set up an flock structure to request an exclusive write lock on the      */
/* whole file.                                                              */
         lockbuf.l_type = F_WRLCK;
         lockbuf.l_whence = SEEK_SET;
         lockbuf.l_start = 0;
         lockbuf.l_len = 0;

/* Obtain a file descriptor for the file, checking for errors.              */
         fd = fileno( rec_ga_fcv[ slot ].write );
         if ( fd == -1 )
         {
            hds_gl_status = DAT__FILCK;
            emsSyser( "MESSAGE", errno );
            rec1_fmsg( "FILE", slot );
            emsRep( "REC1_LOCK_SLOT_2",
                       "Unable to obtain a file descriptor for locking the \
file ^FILE - ^MESSAGE",
                       &hds_gl_status );
         }

/* Request the lock, waiting if required. Check for errors.                 */
         else if ( fcntl( fd, hds_gl_wait ? F_SETLKW : F_SETLK,
                          &lockbuf ) == -1 )
         {
            hds_gl_status = DAT__FILCK;
            emsSyser( "MESSAGE", errno );
            rec1_fmsg( "FILE", slot );
            emsRep( "REC1_LOCK_SLOT_3",
                       "Unable to lock the file ^FILE for exclusive write \
access - ^MESSAGE",
                       &hds_gl_status );
         }

/* If successful, note the slot is locked.                                  */
         else
#endif
#endif
         {
            rec_ga_fcv[ slot ].locked = 1;
         }
#endif
      }

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
