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
#include "sc2fts_entry.h"

#define FUNC_NAME "sc2fts_entry"

/* define all fts-2-related calibration operations */
struct sc2fts_fun sc2fts_cal[] =
{
  { "IFGMFLATFIELD", sc2fts_ifgmflatfield },  /* keywords:  */
  { "ADDWCS",        sc2fts_addwcs },         /* keywords: none */
  { "FREQCORR",      sc2fts_freqcorr },       /* keywords: theta */
  { "PORTIMBALANCE", sc2fts_portimbalance },  /* keywords:  */
  { "TRANSCORR",     sc2fts_transcorr },      /* keywords: tau, am, pwv */
  { "SPECFLATFIELD", sc2fts_specflatfield },  /* keywords:  */
  { "GROUPCOADD",    sc2fts_groupcoadd }      /* keywords:  */
};

/* the main entry to FTS-2 data reduction operation */
void sc2fts_entry ( int *status )         /* status: global status (given and returned) */
{
  /* local variables */
  int i;
  int indf;                            /* NDF identifier of input file */
  int ondf;                            /* NDF identifier of output file */
  Grp *igrp = NULL;                    /* Group of input files */
  Grp *ogrp = NULL;                    /* Group of output files */
  Grp *parsgrp = NULL;                 /* Group containing parameters for each operation */
  AstKeyMap *parsKeymap = NULL;        /* KeyMap of PARSLIST */
  AstKeyMap *subParsKeymap = NULL;     /* KeyMap for each operation */
  int ksize = 0;                       /* Number of items in a group */
  char iname[GRP__SZNAM+1];           /* name of input file */
  char oname[GRP__SZNAM+1];           /* name of output file */
  char *pname = NULL;                  /* pointer to file name */

  /* Get group of input files */
  kpg1Gtgrp ( "IN", &igrp, &ksize, status );

  /* Get group of output files */
  kpg1Gtgrp( "OUT",  &ogrp, &ksize, status );

  /* Get the value for PARSLIST */ 
  kpg1Gtgrp( "PARSLIST", &parsgrp, &ksize, status );

  /* convert the value from Grp into Keymap */
  kpg1Kymap( parsgrp, &parsKeymap, status );

  /* delete parsgrp */
  if( parsgrp ) grpDelet( &parsgrp, status );

  /* Calibration Operations for FTS-2 */
  if(astMapHasKey(parsKeymap, "GROUPCOADD") == 0) /* other operations but GROUPCOADD */
  {
    pname = iname;
    grpGet( igrp, 1, 1, &pname, GRP__SZNAM, status );
    pname = oname;
    grpGet( ogrp, 1, 1, &pname, GRP__SZNAM, status );

    if( strcmp(iname, oname)!=0 ) /* when input and output files are not the same, 
                                   * populate the output file with data from input file 
                                   */
    {
      /* start of NDF */
      ndfBegin();

      /* Open the input file solely to propagate it to the output file */
      ndgNdfas( igrp, 1, "READ", &indf, status );
      /* We want QUALITY too if it's available */
      ndgNdfpr( indf, "DATA,WCS,QUALITY", ogrp, 1, &ondf, status );
      ndfAnnul( &indf, status );

      /* Close output file */
      ndfAnnul( &ondf, status );
      
      /* end of NDF */
      ndfEnd( status );
    }
  }

  for( i=0; i<sizeof(sc2fts_cal)/sizeof(sc2fts_cal[0]); i++ )
  {
    /* the key/value pair in parsKeymap: op.key=value 
     * astMapGet0A will get a Keymap for an operation 
     */
    if( astMapHasKey( parsKeymap, sc2fts_cal[i].name ) !=0 )
    {
      if( astMapType( parsKeymap, sc2fts_cal[i].name ) == AST__OBJECTTYPE ) /* use user-defined values for parameters */
      {
        astMapGet0A( parsKeymap, sc2fts_cal[i].name, &subParsKeymap );
        (*(sc2fts_cal[i].op))( igrp, ogrp, subParsKeymap, status );
      }
      else /* use default values for parameters */
      {
        (*(sc2fts_cal[i].op))( igrp, ogrp, NULL, status );
      }
    }
  }   
   
  if( igrp != NULL ) grpDelet( &igrp, status);
  if( ogrp != NULL ) grpDelet( &ogrp, status);
  printf("Implementation is under construction! \n");
}
