#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include "ems.h"		 /* EMS error reporting routines	    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "dat1.h"		 /* Internal dat_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   void dat1_check_mode( const char *mode, INT mode_len, char *modechar,
			 INT *status )
   {
/*+									    */
/* Name:								    */
/*    dat1_check_mode							    */

/* Purpose:								    */
/*    Check an access mode string for validity.				    */

/* Invocation:								    */
/*    dat1_check_mode( mode, mode_len, modechar, status )		    */

/* Description:								    */
/*    This routine validates the syntax of an access mode specification.    */
/*    If valid, it returns one of the characters 'R', 'U' or 'W' depending  */
/*    on the first significant (non-blank) character encountered. If the    */
/*    access mode string is not valid, then an error is reported.	    */

/* Parameters:								    */
/*    const char *mode							    */
/*	 Pointer to the access mode string to be checked. It need not be    */
/*	 null-terminated. Case in not significant and only the first	    */
/*	 non-blank character is actually used (this is a historical	    */
/*	 feature).							    */
/*    INT mode_len							    */
/*       Number of characters in the mode string.			    */
/*    char *modechar							    */
/*	 Pointer to a character to receive the validated access mode	    */
/*	 abbreviation ('R', 'U' or 'W').				    */
/*    INT *status							    */
/*       The inherited global status.					    */

/* Returned Value:							    */
/*    void								    */

/* Copyright:								    */
/*    Copyright (C) 1992 Science & Engineering Research Council		    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    14-OCT-1992 (RFWS):						    */
/*	 Tidied up, renamed and changed to accept separate string pointer   */
/*	 and length arguments.						    */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      INT i;			 /* Loop counter for characters		    */
      int ok;			 /* Mode string valid?			    */

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( *status ) ) return;

/* Initially assume the mode string is invalid and scan through until the   */
/* first non-blank character is found.					    */
      ok = 0;
      for ( i = 0; i < mode_len; i++ )
      {
         if ( mode[ i ] != ' ' )
         {

/* Check for READ access.						    */
            if ( mode[ i ] == 'R' || mode[ i ] == 'r' )
            {
               *modechar = 'R';
	       ok = 1;
	       break;
            }

/* Check for WRITE access.						    */
            else if ( mode[ i ] == 'W' || mode[ i ] == 'w' )
            {
               *modechar = 'W';
	       ok = 1;
	       break;
            }

/* Check for UPDATE access.						    */
            else if ( mode[ i ] == 'U' || mode[ i ] == 'u' )
            {
               *modechar = 'U';
	       ok = 1;
	       break;
            }

/* If the first non-blank character is not valid, then quit the loop.	    */
            else
            {
               break;
            }
         }
      }

/* If the mode string is not valid, then report an error.		    */
      if ( !ok )
      {
         *status = DAT__MODIN;
         ems_setc_c( "MODE", mode, mode_len );
         ems_rep_c( "DAT1_CHECK_MODE_1",
                    "Invalid access mode \'^MODE\' specified (possible \
programming error).", status);
      }

/* Exit the routine.							    */
      return;
   }
