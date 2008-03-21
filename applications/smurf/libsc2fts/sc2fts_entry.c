/*
*+
*  Name:
*     sc2fts_entry.c

*  Purpose:
*     Main Entry to the implementation of FTS-2 data reduction

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2fts_entry ( int *status )

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*
*

*  Authors:
*     B.Zhang (UoL)

*  History :
*     2008-03-18 (BZ):
*        Create a implementation for FTS-2

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
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
/* standard C includes */
#include <string.h>
#include <stddef.h>
#include <stdlib.h>

/* STARLINK includes */
#include "par.h"


/* SC2FTS includes */
#include "sc2fts_par.h"
#include "sc2fts_funs.h"
#include "sc2fts_common.h"

/* the main entry to FTS-2 data reduction operation */
void sc2fts_entry ( int *status )         /* status: global status (given and returned) */
{
   /* local variables */
   int i;
   int indf;                              /* file ID of a NDF file */
   char opslist[SC2FTS__OPSLISTSIZE];     /* store the operation list */
   char parslist[SC2FTS__PARSLISTSIZE];   /* store the corresponding parameter list */
   char pars[SC2FTS__PARSLISTSIZE];
   char *pars_ptr = parslist, *in_pars;

   ndfBegin();  
   parGet0c("OPSLIST", opslist, SC2FTS__OPSLISTSIZE, status);
   parGet0c("PARSLIST", parslist, SC2FTS__PARSLISTSIZE, status);

      
   for(i=0; i<sizeof(ops_sc2fts)/sizeof(ops_sc2fts[0]); i++)
   {
     if(issupported(ops_sc2fts[i], opslist) == 0)
     {
       if(getpars(pars_sc2fts[i], parslist, pars) ==0) 
       {
         printf("%si:%s:%s\n", ops_sc2fts[i], pars_sc2fts[i], pars);
         (*sc2fts_op[i])(indf, pars, status);
       }
     } 
   }

//   parGet0c("OUT", ncfile, 40, status);
//   ndfAssoc ( "IN", "UPDATE", &indf, status );
   printf("Implementation is under construction! \n");
   ndfEnd( status );
}
