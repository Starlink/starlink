#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */
#include <stdlib.h>		 /* Utility functions			    */
#include <string.h>		 /* String built-ins			    */
#include <errno.h>		 /* Error numbers			    */

#include "ems.h"		 /* EMS error reporting routines	    */
#include "ems_par.h"		 /* EMS__ public constants		    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "dat_err.h"		 /* DAT__ error codes			    */

   int rec_reall_mem( int size, void **pntr )
   {
/*+									    */
/* Name:								    */
/*    rec_reall_mem							    */

/* Purpose:								    */
/*    Re-allocate memory.						    */

/* Invocation:								    */
/*    rec_reall_mem( size, pntr )					    */

/* Description:								    */
/*    This function re-allocates memory for use as workspace.		    */

/* Parameters:								    */
/*    int size								    */
/*	 The new amount of memory required, in bytes.			    */
/*    void **pntr							    */
/*	 Address of a pointer to workspace already allocated. A new pointer */
/*	 may be returned. The existing pointer is returned unchanged if an  */
/*	 error occurs.							    */

/* Returned Value:							    */
/*    int rec_reall_mem							    */
/*	 The global status value current on exit.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    12-DEC-1991 (RFWS):						    */
/*	 Original version.						    */
/*    {@enter_changes_here@}						    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      void *newptr;		 /* Pointer to re-allocated memory	    */

/*.									    */

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Re-allocate the required memory.					    */
      newptr = realloc( *pntr, (size_t) size );

/* If re-allocation failed, then report an error.			    */
      if ( newptr == NULL )
      {
         hds_gl_status = DAT__NOMEM;
         ems_setc_c( "MESSAGE", strerror( errno ), EMS__SZTOK );
         ems_seti_c( "NBYTES", size );
         ems_rep_c( "REC_REALL_MEM_1",
                    "Unable to obtain a block of ^NBYTES bytes of memory - \
^MESSAGE",
                    &hds_gl_status );
      }

/* If successful, return the new pointer.				    */
      else
      {
         *pntr = newptr;
      }

/* Return the current global status value.				    */
      return hds_gl_status;
   }
