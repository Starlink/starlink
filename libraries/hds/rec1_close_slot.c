#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS__ public constants                  */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec1_close_slot( int slot )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_close_slot                                                       */

/* Purpose:                                                                 */
/*    Close down a slot in the File Control Vector.                         */

/* Invocation:                                                              */
/*    rec1_close_slot( slot )                                               */

/* Description:                                                             */
/*    This function closes down a slot in the File Control Vector. If the   */
/*    associated file is open, it is closed (and deleted if marked for      */
/*    deletion) and all other associated resources are released.            */

/* Parameters:                                                              */
/*    int slot                                                              */
/*       The slot in the File Control Vector to be closed.                  */

/* Returned Value:                                                          */
/*    int rec1_close_slot                                                   */
/*       The global status value current on exit.                           */

/* Notes:                                                                   */
/*    This function attempts to execute even if the HDS global status is    */
/*    set on entry, although no further error report will be made if it     */
/*    subsequently fails under these circumstances.                         */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    3-APR-1991 (RFWS):                                                    */
/*       Changed name, added error reporting and prologue and made portable */
/*       by means of the C "remove" function for file deletion.             */
/*    26-APR-1991 (RFWS):                                                   */
/*       Changed to cater for a null-terminated file name string.           */
/*    5-JUN-1991 (RFWS):                                                    */
/*       Clear the slot open flag when the slot is successfully closed.     */
/*    28-JUN-1991 (RFWS):                                                   */
/*       Improved the error handling and eliminated the need to fill the    */
/*       closed FCV slot with zeros.                                        */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      int lfns;                  /* Length of file name string              */

/*.                                                                         */

/* Check that the FCV slot is open. There is nothing to do if it is not.    */
      if ( rec_ga_fcv[ slot ].open )
      {

/* Begin a new error reporting context.                                     */
         ems_begin_c( &hds_gl_status );

/* Unlock the slot.                                                         */
         rec1_unlock_slot( slot );

/* Close both the read and write I/O channels to the file (if open).        */
         rec1_close_file( slot, 'R' );
         rec1_close_file( slot, 'W' );

/* Check if the file is marked for deletion.                                */
         if ( _ok( hds_gl_status ) )
         {
            if ( rec_ga_fcv[ slot ].dele )
            {

/* If so, then delete it, reporting any errors.                             */
               if ( remove( rec_ga_fcv[ slot ].name ) )
               {
                  hds_gl_status = DAT__FILND;
                  ems_setc_c( "MESSAGE", strerror( errno ), EMS__SZTOK );
                  rec1_fmsg( "FILE", slot );
                  ems_rep_c( "REC1_CLOSE_SLOT_1",
                             "Error deleting the file ^FILE - ^MESSAGE",
                             &hds_gl_status );
               }
            }

/* Deallocate the memory used for the File Name String and the File ID.     */
            lfns = strlen( rec_ga_fcv[ slot ].name );
            rec_deall_mem( lfns + 1, (void **) &rec_ga_fcv[ slot ].name );
            rec_deall_mem( sizeof( struct FID ),
                           (void **) &rec_ga_fcv[ slot ].fid );

/* Mark the FCV slot as closed.                                             */
            rec_ga_fcv[ slot ].open = 0;
         }

/* End the error reporting context.                                         */
         ems_end_c( &hds_gl_status );
      }

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
