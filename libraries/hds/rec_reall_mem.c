#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>              /* Utility functions                       */
#include <string.h>              /* String built-ins                        */
#include <errno.h>               /* Error numbers                           */

#include "star/mem.h"
#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS__ public constants                  */
#include "hds1.h"                /* Global definitions for HDS              */
#include "dat_err.h"             /* DAT__ error codes                       */
#include "rec.h"

   int rec_reall_mem( size_t size, void **pntr )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec_reall_mem                                                         */

/* Purpose:                                                                 */
/*    Re-allocate memory.                                                   */

/* Invocation:                                                              */
/*    rec_reall_mem( size, pntr )                                           */

/* Description:                                                             */
/*    This function re-allocates memory for use as workspace.               */

/* Parameters:                                                              */
/*    size_t size                                                           */
/*       The new amount of memory required, in bytes.                       */
/*    void **pntr                                                           */
/*       Address of a pointer to workspace already allocated. A new pointer */
/*       may be returned. The existing pointer is returned unchanged if an  */
/*       error occurs.                                                      */

/* Returned Value:                                                          */
/*    int rec_reall_mem                                                     */
/*       The global status value current on exit.                           */

/* Copyright:                                                               */
/*    Copyright (C) 1991 Science & Engineering Research Council             */
/*    Copyright (C) 2006 Particle Physics and Astronomy Research Council    */
/*    Copyright (C) 2010 Science & Technology Facilities Council.           */
/*    All Rights Reserved                                                   */

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
/*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,       */
/*     MA 02111-1307, USA                                                   */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    12-DEC-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    23-FEB-2006 (TIMJ):                                                   */
/*       use starmem                                                        */
/*    29-NOV-2010 (TIMJ):                                                   */
/*       Use size_t rather than int                                         */
/*    {@enter_changes_here@}                                                */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      void *newptr;              /* Pointer to re-allocated memory          */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* Re-allocate the required memory.                                         */
      newptr = MEM_REALLOC( *pntr, size );

/* If re-allocation failed, then report an error.                           */
      if ( newptr == NULL )
      {
         hds_gl_status = DAT__NOMEM;
         emsSyser( "MESSAGE", errno );
         emsRepf( "REC_REALL_MEM_1",
                  "Unable to obtain a block of %zu bytes of memory - "
                  "^MESSAGE", &hds_gl_status, size );
      }

/* If successful, return the new pointer.                                   */
      else
      {
         *pntr = newptr;
      }

/* Return the current global status value.                                  */
      return hds_gl_status;
   }
