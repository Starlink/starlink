#if HAVE_CONFIG_H
#   include <config.h>
#endif
#include <unistd.h>

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
#endif

/* Other include files:                                                     */
/* ===================                                                      */
#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS__ public constants                  */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec1_close_file( int slot, char mode )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_close_file                                                       */

/* Purpose:                                                                 */
/*    Close a container file.                                               */

/* Invocation:                                                              */
/*    rec1_close_file( slot, mode )                                         */

/* Description:                                                             */
/*    This function closes an I/O channel to a container file.              */

/* Parameters:                                                              */
/*    int slot                                                              */
/*       Slot number of the container file in the File Control Vector.      */
/*    char mode                                                             */
/*       A symbol indicating which I/O channel is to be closed: 'R' for the */
/*       read-only channel, 'W' for the read/write channel.                 */

/* Returned Value:                                                          */
/*    int rec1_close_file                                                   */
/*       The global status value current on exit.                           */

/* Notes:                                                                   */
/*   -  This routine attempts to execute even if the HDS global status is   */
/*   set on entry, although no further error report will be made if it      */
/*   subsequently fails under these circumstances.                          */
/*   -  No action is taken if the specified I/O channel is not initially    */
/*   open.                                                                  */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    PWD: Peter W. Draper (STARLINK, Durham University)                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    26-MAR-1991 (RFWS):                                                   */
/*       Added prologue.                                                    */
/*    26-APR-1991 (RFWS):                                                   */
/*       Changed to cater for a null-terminated file name string.           */
/*    26-APR-1991 (RFWS):                                                   */
/*       Changed to use the slot number as an argument.                     */
/*    7-MAY-1991 (RFWS):                                                    */
/*       Added a portable implementation.                                   */
/*    28-JUN-1991 (RFWS):                                                   */
/*       Added function prototype for VMS system call.                      */
/*    08-MAR-2005 (PWD):                                                    */
/*       Added fsync() call to make sure unmapped data is written to disk.  */
/*       This change seems to be required by the single UNIX standard       */
/*       version 3.                                                         */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
#if defined( vms )               /* VMS version local variables:            */
      unsigned int systat;       /* System status code                      */
      unsigned short int iochan; /* File I/O channel                        */

#else                            /* Portable version local variables:       */
      FILE *iochan;              /* File I/O stream                         */
      int fd;                    /* File identifier                         */
#endif

/* External References:                                                     */
#if defined( vms )               /* VMS version system routines:            */
      unsigned int SYS$DASSGN( unsigned short int iochan );
#endif

/*.                                                                         */

/* Begin a new error reporting context.                                     */
      ems_begin_c( &hds_gl_status );

/* Obtain the I/O channel (or stream) to be closed.                         */
      iochan = ( mode == 'R' ) ? rec_ga_fcv[ slot ].read :
                                 rec_ga_fcv[ slot ].write;

/* If the I/O channel is not null...                                        */
      if ( iochan != REC__NOIOCHAN )
      {

/* VMS version:                                                             */
/* ===========                                                              */
#if defined( vms )

/* Close the file by deassigning the channel.                               */
         systat = SYS$DASSGN( iochan );

/* Report any errors.                                                       */
         if ( !( systat & STS$M_SUCCESS ) )
         {
            hds_gl_status = DAT__FILCL;
            rec1_fmsg( "FILE", slot );
            ems_seti_c( "IOCHAN", iochan );
            ems_syser_c( "MESSAGE", systat );
            ems_rep_c( "REC1_CLOSE_FILE_1",
                       "Unable to close file ^FILE on I/O channel ^IOCHAN - \
^MESSAGE.",
                       &hds_gl_status );
         }

/* Portable version:                                                        */
/* ================                                                         */
#else

#if defined( HAVE_FSYNC ) && ( defined( _mmap ) || defined( HAVE_MMAP ) )
/* To make sure that a mapped file is correctly written to disk we need to  */
/* perform an fsync, this should have also been preceded by calls to        */
/* msync(MS_ASYNC) when the data was unmapped. Ignore any errors.           */
         if ( mode != 'R' ) 
         {
             fd = fileno( iochan );
             if ( fd != -1 )
             {
                 fsync( fd );
             }
         }
#endif

/* Close the file, reporting any errors.                                    */
         if ( fclose( iochan ) )
         {
            hds_gl_status = DAT__FILCL;
            ems_setc_c( "MESSAGE", strerror( errno ), EMS__SZMSG );
            rec1_fmsg( "FILE", slot );
            ems_rep_c( "REC1_CLOSE_FILE_2",
                       "Unable to close file ^FILE - ^MESSAGE",
                       &hds_gl_status );
         }
#endif

/* If successful, reset the I/O channel value in the File Control Vector    */
/* slot.                                                                    */
         else
         {
            if ( mode == 'R' )
            {
               rec_ga_fcv[ slot ].read = REC__NOIOCHAN;
            }
            else
            {
               rec_ga_fcv[ slot ].write = REC__NOIOCHAN;
            }
         }
      }

/* End the error reporting context and return the current global status     */
/* value.                                                                   */
      ems_end_c( &hds_gl_status );
      return hds_gl_status;
   }
