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
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/kaplibs.h"


/* SC2FTS includes */
#include "sc2fts_par.h"
#include "sc2fts_funs.h"

#define FUNC_NAME "sc2fts_entry"

/* the main entry to FTS-2 data reduction operation */
void sc2fts_entry ( int *status )         /* status: global status (given and returned) */
{
  /* local variables */
  int i;
  int indf;                            /* NDF identifier of input file */
  Grp *igrp = NULL;                    /* Group of input files */
  Grp *ogrp = NULL;                    /* Group of output files */
  Grp *parsgrp = NULL;                 /* Group containing parameters for each operation */
  int flag;                            /* Flag */
  AstKeyMap *parsKeymap = NULL;        /* KeyMap of PARSLIST */
  AstKeyMap *subParsKeymap = NULL;     /* KeyMap for each operation */
  int ksize = 0;                       /* Number of items in a group */
  smfData *idata = NULL;               /* Pointer to SCUBA2 data struct */
  /* Main routine */
  ndfBegin();

  /* Get group of input files */
  ndgAssoc ( "IN", 1, &igrp, &ksize, &flag, status );

  /* Get group of input files */
  ndgCreat( "OUT", igrp, &ogrp, &ksize, &flag, status );

  /* Get the value for PARSLIST */ 
  kpg1Gtgrp( "PARSLIST", &parsgrp, &ksize, status );

  /* convert the value from Grp into Keymap */
  kpg1Kymap( parsgrp, &parsKeymap, status );

  /* delete parsgrp */
  if( parsgrp ) grpDelet( &parsgrp, status );

  /* Calibration Operations for FTS-2 */
  if(astMapHasKey(parsKeymap, "GROUPCOADD") == 0) /* other operations but GROUPCOADD */
  {
    smf_open_file(igrp, 1, "UPDATE", SMF__NOCREATE_DATA, &idata, status);
    for(i=0; i<sizeof(ops_sc2fts)/sizeof(ops_sc2fts[0]); i++)
    {
      /* the key/value pair in parsKeymap: op.key=value 
       * astMapGet0A will get a Keymap for an operation 
       */
      if(astMapHasKey(parsKeymap, ops_sc2fts[i]) !=0)
      {
        if(astMapType(parsKeymap, ops_sc2fts[i]) == AST__OBJECTTYPE) /* use user-defined values for parameters */
        {
          if(astMapGet0A(parsKeymap, ops_sc2fts[i], &subParsKeymap) != 0)
          {
            (*sc2fts_op[i])( NULL, subParsKeymap, status );
          }
        }
        else /* use default values for parameters */
        {
          (*sc2fts_op[i])( NULL, NULL, status );
        }
      }
    }   
  }
  else       /* GROUPCOADD operation */
  {
    if(astMapGet0A(parsKeymap, "GROUPCOADD", &subParsKeymap) != 0)
    {
      sc2fts_groupcoadd ( igrp, ogrp, subParsKeymap, status );
    }
  }
   
  if( igrp != NULL ) grpDelet( &igrp, status);
  if( ogrp != NULL ) grpDelet( &ogrp, status);
  printf("Implementation is under construction! \n");
  ndfEnd( status );
}
