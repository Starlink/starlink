#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "ems.h"		 /* EMS error reporting routines	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   void dat1_import_loc( const char *loc, const int loc_length,
                         struct LCP **lcp )
   {
/*+									    */
/* Name:								    */
/*    dat1_import_loc							    */

/* Purpose:								    */
/*    Validate a locator and find its Locator Control Packet.		    */

/* Invocation:								    */
/*    dat1_import_loc( loc, loc_length, lcp )				    */

/* Description:								    */
/*    This routine validates a locator, as supplied by a caller of HDS, and */
/*    identifies the Locator Control Packet associated with it.		    */

/* Parameters:								    */
/*    const char *loc							    */
/*       Pointer to the locator string.					    */
/*    const int loc_length						    */
/*       Number of chars in the locator string.				    */
/*    struct LCP **lcp							    */
/*	 Pointer to a pointer which will be set to point at the Locator	    */
/*	 Control Packet associated with the locator supplied (if valid). If */
/*	 the locator is not valid, or if any other error occurs, then a	    */
/*	 null pointer will be returned in *lcp.				    */

/* Returned Value:							    */
/*    void								    */

/* Copyright:								    */
/*    Copyright (C) 1992 Science & Engineering Research Council		    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    25-SEP-1992 (RFWS):						    */
/*	 Converted from an earlier version. Explicit locator length	    */
/*	 argument added instead of passing pointer and length in a combined */
/*	 structure.							    */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      int valid;		 /* Locator valid?			    */
      struct LOC locdata;	 /* Copy of locator data		    */
      struct RCL rcl;		 /* Record Control Label		    */

/*.									    */

/* Check the inherited global status.					    */
      if ( _ok( hds_gl_status ) )
      {

/* See if HDS has been initialised. The locator cannot be valid if it has   */
/* not, as no locators will have been issued.				    */
         valid = hds_gl_active;

/* Validate the locator length.						    */
         if ( valid )
         {
            valid = ( loc_length == DAT__SZLOC );
         }

/* If OK, then extract the information from the locator string (necessary   */
/* to ensure that data alignment is correct, as the string will normally be */
/* stored externally in a Fortran CHARACTER variable).			    */
         if ( valid )
         {
	    (void) memcpy( (void *) &locdata, (const void *) loc,
			   sizeof( struct LOC ) );

/* Validate the locator check field.					    */
            valid = ( locdata.check == DAT__LOCCHECK );
         }

/* If OK, then identify the associated LCP and check that the locator	    */
/* sequence number tallies with the LCP sequence number.		    */
         if ( valid )
         {
            *lcp = locdata.lcp;
            valid = ( locdata.seqno == (*lcp)->seqno );
         }

/* If OK, then check that the associated LCP is valid.			    */
         if ( valid )
         {
            valid = (*lcp)->data.valid;
         }

/* If still OK, then read the associated Record Control Label and check	    */
/* that the Record ID of the record's parent, as stored in the LCP, matches */
/* the RID stored in the actual record.					    */
         if ( valid )
         {
            rec_get_rcl( &(*lcp)->data.han, &rcl );
            if ( _ok( hds_gl_status ) )
            {

/* If the RIDs do not match, then report an error.			    */
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

/* If the locator is not valid, but no other error has occurred, then	    */
/* report an error.							    */
         if ( !valid && _ok( hds_gl_status ) )
         {
            hds_gl_status = DAT__LOCIN;
            ems_setc_c( "VALUE", loc, loc_length );
            ems_seti_c( "LENGTH", loc_length );
            ems_rep_c( "DAT1_IMPORT_LOC_2",
                       "HDS locator invalid: value=\'^VALUE\', length=^LENGTH \
(possible programming error).",
                       &hds_gl_status );
         }
      }

/* If an error has occurred, then return a null pointer.		    */
      if ( !_ok( hds_gl_status ) )
      {
         *lcp = NULL;
      }

/* Exit the routine.							    */
      return;
   }
