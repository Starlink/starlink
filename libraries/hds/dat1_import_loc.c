#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int dat1_import_loc( const struct LOC * loc,
                         struct LCP **lcp )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    dat1_import_loc                                                       */

/* Purpose:                                                                 */
/*    Validate a locator and find its Locator Control Packet.               */

/* Invocation:                                                              */
/*    dat1_import_loc( loc, lcp )                                           */

/* Description:                                                             */
/*    This routine validates a locator, as supplied by a caller of HDS, and */
/*    identifies the Locator Control Packet associated with it.             */

/* Parameters:                                                              */
/*    struct LOC *loc                                                       */
/*       Pointer to the locator.                                            */
/*    struct LCP **lcp                                                      */
/*       Pointer to a pointer which will be set to point at the Locator     */
/*       Control Packet associated with the locator supplied (if valid). If */
/*       the locator is not valid, or if any other error occurs, then a     */
/*       null pointer will be returned in *lcp.                             */

/* Returned Value:                                                          */
/*    int dat1_import_loc                                                   */
/*       The global status value current on exit.                           */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */
/*    Copyright (C) 2005 Particle Physics and Astronomy Research Council    */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    TIMJ: T.   Jenness      (JAC, Hawaii)                                 */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    25-SEP-1992 (RFWS):                                                   */
/*       Converted from an earlier version. Explicit locator length         */
/*       argument added instead of passing pointer and length in a combined */
/*       structure.                                                         */
/*    11-MAY-2004 (BKM):                                                    */
/*       Change HDS 32/64-bit flag appropriately for locator                */
/*    15-NOV-2005 (TIMJ):                                                   */
/*       Change API to use the struct LOC explcitly                         */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      int valid;                 /* Locator valid?                          */
      struct RCL rcl;            /* Record Control Label                    */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( _ok( hds_gl_status ) )
      {

/* See if HDS has been initialised. The locator cannot be valid if it has   */
/* not, as no locators will have been issued.                               */
         valid = hds_gl_active;

/* Validate the locator NULL-ness.                                          */
         if ( valid )
         {
 	    valid = ( loc != NULL );
         }

/* Validate the locator check field.                                        */
         if ( valid )
         {
            valid = ( loc->check == DAT__LOCCHECK );
         }

/* If OK, then identify the associated LCP and check that the locator       */
/* sequence number tallies with the LCP sequence number.                    */
         if ( valid )
         {
            *lcp = loc->lcp;
            valid = ( loc->seqno == (*lcp)->seqno );
         }

/* If OK, then check that the associated LCP is valid.                      */
         if ( valid )
         {
            valid = (*lcp)->data.valid;
         }

/* If still OK, then read the associated Record Control Label and check     */
/* that the Record ID of the record's parent, as stored in the LCP, matches */
/* the RID stored in the actual record.                                     */
         if ( valid )
         {
            rec_get_rcl( &(*lcp)->data.han, &rcl );
            if ( _ok( hds_gl_status ) )
            {

/* If the RIDs do not match, then report an error.                          */
               if ( ( rcl.parent.bloc != (*lcp)->data.parent.bloc ) ||
                    ( rcl.parent.chip != (*lcp)->data.parent.chip ) )
               {
                  hds_gl_status = DAT__INCHK;
                  ems_setc_c( "NAME", (*lcp)->data.name, DAT__SZNAM );
                  ems_rep_c( "DAT1_IMPORT_LOC_1",
                             "Locator refers to an object \'^NAME\' which no \
longer exists (possible programming error or corrupted HDS container file).",
                             &hds_gl_status );
               }
            }
         }

/* If the locator is not valid, but no other error has occurred, then       */
/* report an error.                                                         */
         if ( !valid && _ok( hds_gl_status ) )
         {
            hds_gl_status = DAT__LOCIN;
	    if (loc == NULL) {
	      emsSetc( "VALUE", "NULL" );
	    } else {
	      ems_setc_c( "VALUE", (char*)loc, sizeof(struct LOC) );
	    }
            ems_seti_c( "LENGTH", sizeof(struct LOC) );
            ems_rep_c( "DAT1_IMPORT_LOC_2",
                       "HDS locator invalid for import: value=\'^VALUE\', length=^LENGTH \
(possible programming error).",
                       &hds_gl_status );
         }
      }

/* If an error has occurred, then return a null pointer.                    */
      if ( !_ok( hds_gl_status ) )
      {
         *lcp = NULL;
      }
/* otherwise set the HDS 64-bit mode according to the "extended" bit in the */
/* RCL.                                                                     */
     else
     {
        hds_gl_64bit = rcl.extended;
     }

/* Exit the routine.                                                        */
      return hds_gl_status;
   }
