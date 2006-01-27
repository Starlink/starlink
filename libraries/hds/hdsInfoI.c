
#include <stdlib.h>

#include "dat_err.h"
#include "hds1.h"
#include "ems.h"
#include "rec.h"
#include "str.h"
#include "dat1.h"
#include "hds.h"

/*
*+
*  Name:
*     hdsInfoI

*  Purpose:
*     Retrieve internal state from HDS as integer.

*  Invocation:
*     hdsInfoI(const HDSLoc* loc, const char * topic, int *result,
*              int * status);

*  Description :
*     Retrieves integer information associated with the current state
*     of the HDS internals.

*  Parameters :
*     loc = const HDSLoc* (Given)
*        HDS locator, if required by the particular topic. Will be
*        ignored for FILES and LOCATORS topics and can be NULL pointer.
*     topic = const char * (Given)
*        Topic on which information is to be obtained. Allowed values are:
*        - LOCATORS : Return the number of active locators
*        - FILES : Return the number of open files
*     result = int* (Returned)
*        Answer to the question.
*     status = int* (Given & Returned)
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Return Value:
*     Returns global status on exit.

*  See Also:
*     hdsShow

*  Notes:
*     - Can be used to help debug locator leaks.

*  Authors
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History :
*     25-JAN-2006 (TIMJ):
*        Create from hdsShow.
*     26-JAN-2006 (TIMJ):
*        Move into separte file.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}

*-
*/

int
hdsInfoI(const HDSLoc* loc, const char *topic_str, int *result,
        int  *status)
{        
/*===============================*/
/* HDS_INFOI - Retrieve HDS statistic */
/*===============================*/

#undef context_name
#undef context_message
#define context_name "HDS_INFOI_ERR"
#define context_message\
        "HDS_INFOI: Error retrieving HDS statistics."

   struct DSC      topic; 

   struct LCP      *lcp;
   struct LCP_DATA *data;
   char            name[DAT__SZNAM];
   struct LOC      locator;
   struct STR      path;
   struct STR      file;
   int             i;

/* These buffers are only used when using VMS descriptors. */

#if defined( vms )
   char            pbuf[STR_K_LENGTH];
   char            fbuf[STR_K_LENGTH];
#endif

/* Enter routine        */

   *result = 0;
   if (!_ok(*status))
      return *status;
   hds_gl_status = DAT__OK;

/* Import the name of the statistic to be shown.        */

   _strcsimp(&topic,topic_str);

/* Initialise strings.  */

   _strinit(&path, STR_K_LENGTH, pbuf);
   _strinit(&file, STR_K_LENGTH, fbuf);

/* Ensure that HDS has been initialised.                                    */
   if ( !hds_gl_active )
   {
      dat1_init( );
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;
   }


/* Format the topic name and show the appropriate statistic.    */
   dau_check_name(&topic, name);

   /* Number of open files */
   if (_cheql(4,name, "FILE"))
      rec_count_files( result );
   if (_cheql(4,name, "LOCA"))
   {
     /* Count number of valid locators */
      lcp          = dat_ga_wlq;
      locator.check    = DAT__LOCCHECK;
      *result = 0;
      for (i=0; i<dat_gl_wlqsize; i++)
      {
         data = &lcp->data;
	 if (data->valid) (*result)++;
	 lcp = lcp->flink;
      }
   }
   return hds_gl_status;
}
