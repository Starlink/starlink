/*
*+
*  Name:
*     sc2fts_addwcs.c

*  Purpose:
*     Add WCS information for a spectral cube

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2fts_addwcs ( smfData *idata, AstKeyMap* parKeymap, int *status )

*  Arguments:
*     idata = smfData* (Given)
*        Pointer to input SCUBA2 data struct
*     parKeymap = AstKeyMap* (Given)
*        the parameter Keymap for this operation
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*
*

*  Authors:
*     B.Zhang (UoL)

*  History :
*     2008-03-16 (BZ):
*        Create a test implementation for FTS-2
*     2008-03-31 (BZ):
*        Try to finalize ADDWCS operation for FTS-2

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

/* Standard includes */
#include <math.h>

/* STARLINK includes */
#include "ast.h"

/* SMURF includes */
#include "libsmf/smf_typ.h"
#include "smurf_par.h"

#include "sc2da/sc2ast.h"

void sc2fts_addwcs 
(
smfData*   idata,
AstKeyMap* parKeymap,
int   *status          /* global status (given and returned) */
)
{
  HDSLoc *fts2drloc = NULL;     /* HDSLoc to More.FTS2DR */
  HDSLoc *ftswnloc = NULL;      /* HDSLoc to More.FTS2DR.FTS_WN */
  
  AstFrameSet *gridfset;        /* FrameSet by sc2ast_createwcs */
  AstMapping *gridmapping;      /* mapping from sc2ast_createwcs */
  AstZoomMap *specmapping;      /* mapping between index number and real wavenumber */
  AstCmpMap  *speccubemapping;  /* combined mapping of gridmapping and specmapping */
  AstCmpFrame *basefrm;         /* Base Frame for 3-D spectrum cube */
  AstCmpFrame *currentfrm;      /* Current Frame for 3-D spectrum cube */
  AstFrameSet *speccubewcs;     /* WCS for 3-D spectrum cube */
  double ftswn_unit;            /* unit for FTS spectrum */
  char subname[SZFITSCARD+1];      /* Subarray name */
  int subnum;                      /* Subarray index number */

  /* get the locator to More.FTS2DR */
  fts2drloc = smf_get_xloc( idata, "FTS2DR", "EXT", "READ", 0, 0, status );
  /* get the locator to More.FTS2DR.FTS_WN */
  datFind( fts2drloc, "FTS_WN", &ftswnloc, status );
  /* get the spectrum unit of FTS */
  datGet( ftswnloc, "_DOUBLE", 0, 0, &ftswn_unit, status );

  
  /* create 2-D WCS from sc2ast_createwcs */
  smf_fits_getS( idata->hdr, "SUBARRAY", subname, SZFITSCARD+1, status );
  sc2ast_name2num ( subname, &subnum, status );
  sc2ast_createwcs( subnum, idata->hdr->state, idata->hdr->instap, idata->hdr->telpos, &gridfset, status );

  /* get the mapping of 2-D WCS */
  gridmapping = astGetMapping( gridfset, AST__BASE, AST__CURRENT );
  /* create the zoommap for 1-D spectrum */
  specmapping = astZoomMap( 1, ftswn_unit, "" );
  /* create 3-D mapping for spectrum cube */
  speccubemapping = astCmpMap( gridmapping, specmapping, 0, "" );
   
  /* create Base Frame for 3-D spectrum cube */
  basefrm = astCmpFrame( astGetFrame(gridfset, AST__BASE), astSpecFrame("Unit=AU"), "DOMAIN=GRID" );
  /* create Current Frame for 3-D spectrum cube */
  currentfrm = astCmpFrame( astGetFrame(gridfset, AST__CURRENT), astSpecFrame("Unit=1/cm"), "DOMAIN=GRID" );
 
  /* create WCS for 3-D spectrum cube */
  speccubewcs = astFrameSet( basefrm, "" );
  astAddFrame( speccubewcs, AST__BASE, speccubemapping, currentfrm );
  
  /* write WCS into the NDF file */
  ndfPtwcs( speccubewcs, idata->file->ndfid, status );
}
