#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stddef.h>
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec1.h"                /* Internal rec_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int rec1_locate_hcb( int slot, char mode, struct HCB **hcb )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_locate_hcb                                                       */

/* Purpose:                                                                 */
/*    Obtain a pointer to a container file's HCB information.               */

/* Invocation:                                                              */
/*    rec1_locate_hcb( slot, mode, hcb )                                    */

/* Description:                                                             */
/*    This function returns a pointer to the (unpacked) information         */
/*    contained in the Header Control Block of a container file.            */

/* Parameters:                                                              */
/*    int slot                                                              */
/*       Slot number of the container file in the File Control Vector.      */
/*    char mode                                                             */
/*       A symbol indicating the required mode of access to the HCB         */
/*       information: 'R' for read, 'U' for update and 'W' for write.       */
/*    struct HCB **hcb                                                      */
/*       Pointer to a struct HCB pointer which will be set to point at the  */
/*       HCB information. A null pointer value will be returned under error */
/*       conditions.                                                        */

/* Returned Value:                                                          */
/*    int rec1_locate_hcb                                                   */
/*       The global status value current on exit.                           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    25-MAR-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    3-APR-1991 (RFWS):                                                    */
/*       Added the mode argument.                                           */
/*    16-APR-1991 (RFWS):                                                   */
/*       Changed to handle the case where HCB information is not available  */
/*       by reading and unpacking the container file's Header Control       */
/*       Block. Changed to return a null pointer under error conditions.    */
/*    5-JUN-1991 (RFWS):                                                    */
/*       Added file locking to protect the integrity of cached HCB          */
/*       information.                                                       */
/*    11-JUN-1991 (RFWS):                                                   */
/*       Fixed bug causing the file to be read when mode is 'W'.            */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      unsigned char buf[ REC__SZBLK ]; /* Buffer for Header Control Block   */

/*.                                                                         */

/* Set an initial null value for the returned pointer.                      */
      *hcb = NULL;

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* If the File Control Vector slot does not currently have HCB information  */
/* associated with it, then lock the File Control Vector slot unless access */
/* is read-only.                                                            */
      if ( rec_ga_fcv[ slot ].hcb == NULL )
      {
         if ( mode != 'R' ) rec1_lock_slot( slot );

/* Allocate memory to hold the HCB information.                             */
         rec_alloc_mem( sizeof( struct HCB ),
                        (void **) &rec_ga_fcv[ slot ].hcb );

/* If the access mode is not write, then read the Header Control Block (the */
/* first block) from the container file and unpack it.                      */
         if ( mode != 'W' )
         {
            rec1_read_file( slot, 1, 1, buf );
            rec1_unpack_hcb( buf, rec_ga_fcv[ slot ].hcb );
         }

/* If successful, then initialise the HCB modified flag.                    */
         if ( _ok( hds_gl_status ) )
         {
            rec_ga_fcv[ slot ].hcbmodify = 0;
         }

/* Otherwise, deallocate the memory.                                        */
         else
         {
            rec_deall_mem( sizeof( struct HCB ),
                           (void **) &rec_ga_fcv[ slot ].hcb );
         }
      }

/* Obtain a pointer to the HCB information and update the HCB modified      */
/* flag.                                                                    */
      if ( _ok( hds_gl_status ) )
      {
         *hcb = rec_ga_fcv[ slot ].hcb;
         rec_ga_fcv[ slot ].hcbmodify = rec_ga_fcv[ slot ].hcbmodify ||
                                        ( mode != 'R' );
      }

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
