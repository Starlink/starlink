#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "rec1.h"		 /* Internal rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   int rec_count_files( int *count )
   {
/*+									    */
/* Name:								    */
/*    rec_count_files							    */

/* Purpose:								    */
/*    Count number of open files.					    */

/* Invocation:								    */
/*    rec_count_files( int * num)	       		       		    */

/* Description:								    */
/*    This function counts the number of currently open container	    */
/*    files. Used with hdsInfoI.        	 			    */

/* Parameters:								    */
/*    num = int * (Returned)				       		    */
/*      Number of open files                                                */

/* Returned Value:							    */
/*    int rec_list_files						    */
/*	 The global status value current on exit.			    */

/* Copyright:                                                               */
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

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    BKM:  B.K. McIlwrath    (STARLINK)                                    */
/*    TIMJ: Tim Jenness       (JAC, Hawaii)                                 */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    14-APR-1991 (RFWS):						    */
/*       Added prologue and error handling and tidied.			    */
/*    26-APR-1991 (RFWS):						    */
/*       Changed to cater for a null-terminate file name.		    */
/*    12-MAY-2004 (BKM):                                                    */
/*       Add 64-bit item to report                                          */
/*    25-JAN-2006 (TIMJ):                                                   */
/*       Copy from rec_list_files.c                                         */
/*    {@enter_further_changes_here@}					    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      int slot;			 /* Loop counter for File Control Vector    */

/*.									    */

/* Check the inherited global status.					    */
      *count = 0;
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Scan through the File Control Vector looking for open slots.		    */
      for ( slot = rec_gl_endslot - 1; slot >= 0; slot-- )
      {
         if ( rec_ga_fcv[ slot ].open )
         {
	   (*count)++;
         }
      }

/* Return the current global status value.				    */
      return hds_gl_status;
   }
