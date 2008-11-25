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
*     sc2fts_addwcs ( Grp* igrp, Grp* ogrp, AstKeyMap* parKeymap, int *status )

*  Arguments:
*     igrp = Grp* (Given)
*        the group of input files
*     ogrp = Grp* (Given)
*        the group of output files
*     parKeymap = AstKeyMap* (Given)
*        the parameter Keymap for this operation
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     ADDWCS operation creates a WCS for 3-D spectrum cube and stores it
*     into a NDF file pointed by data (smfData*). The WCS includes a
*     2-D Grid frameset for the first two dimensions from sc2ast_createwcs
*     and 1-D WinMap frameset for the third dimension, which is the wavenumber.
*     The scaling factor of the wavenumber is obtained from 
*     MORE.FTS2DR.FTS_WN_FACTOR of the NDF file. The real wavenumber will be
*     i  x  scaling factor (i is the index number of the third dimension,
*     starting from 0).
*

*  Authors:
*     B.Zhang (UoL)

*  History :
*     2008-03-16 (BZ):
*        Create a test implementation for FTS-2
*     2008-03-31 (BZ):
*        Try to finalize ADDWCS operation for FTS-2
*     2008-04-01 (BZ):
*        With the advice from Tim (TJ) and David (DB), 
*        fix momery leak and do other minor modifications.    
*     2008-04-02 (BZ):
*        With the advice from Tim (TJ) and David (DB),
*        set attributes of WCS.
*     2008-07-18 (TIMJ):
*        Use smf_find_subarray.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2008 University of Lethbridge. All Rights Reserved.

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
#include "sae_par.h"
#include "star/hds.h"

/* SMURF includes */
#include "libsmf/smf_typ.h"
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#include "sc2da/sc2ast.h"
#include "sc2fts_entry.h"

#define FUNC_NAME "sc2fts_addwcs"

void sc2fts_addwcs 
(
Grp *igrp,
Grp* ogrp,
AstKeyMap* parKeymap,
int   *status          /* global status (given and returned) */
)
{
  /* Local Variables */
  smfData *data;                /* Pointer to input SCUBA2 data struct */
  HDSLoc *fts2drloc = NULL;     /* HDSLoc to More.FTS2DR */
  HDSLoc *ftswnloc = NULL;      /* HDSLoc to More.FTS2DR.FTS_WN */
  
  AstFrameSet *gridfset;        /* FrameSet by sc2ast_createwcs */
  AstMapping *gridmapping;      /* mapping from sc2ast_createwcs */
  AstZoomMap *specmapping;       /* mapping between index number and real wavenumber */
  AstCmpMap  *speccubemapping;  /* combined mapping of gridmapping and specmapping */
  AstFrame *basefrm3d;          /* Base Frame for 3-D spectrum cube */
  AstSpecFrame *specframe;      /* Current Frame of 1-D spectrum */
  AstCmpFrame *currentfrm3d;    /* Current Frame for 3-D spectrum cube */
  AstSkyFrame *gridframe;       /* Current Frame for 2-D sky */
  AstFrameSet *speccubewcs;     /* WCS for 3-D spectrum cube */
  double ftswn_factor;          /* Factor for FTS spectrum */
  int subnum;                   /* Subarray index number */
  double refra, refdec;         /* Attributes: RefRA, RefDec */

  char oname[GRP__SZNAM];
  char *pname = oname;
  grpGet( ogrp, 1, 1, &pname, GRP__SZNAM, status );

  /* main routine */
  ndfBegin();

  /* open ogrp for further processing */
  smf_open_file( ogrp, 1, "UPDATE", SMF__NOCREATE_DATA, &data, status );

  /* get the locator to More.FTS2DR */
  fts2drloc = smf_get_xloc( data, "FTS2DR", "EXT", "READ", 0, 0, status );

  /* get the locator to More.FTS2DR.FTS_WN_FACTOR */
  datFind( fts2drloc, "FTS_WN_FACTOR", &ftswnloc, status );

  /* get the scaling factor of spectrum of FTS. The scaling factor is 
   * 1/L (L is the double-sided scan length with unit millimeter) 
  */
  datGet0D( ftswnloc, &ftswn_factor, status );
  /* Annual HDSLoc */
  datAnnul( &ftswnloc, status );
  datAnnul( &fts2drloc, status );
  if(*status != SAI__OK)
  {
	errRep(FUNC_NAME,	"Input file structure error", status);

	return;
  }
  /* AST begin */
  astBegin;

  /* create 2-D WCS from sc2ast_createwcs */
  smf_find_subarray( data->hdr, NULL, 0, &subnum, status );
  sc2ast_createwcs( subnum, data->hdr->state, data->hdr->instap, data->hdr->telpos, &gridfset, status );

  /* get frame of 2-D grid WCS */
  gridframe = astGetFrame( gridfset, AST__CURRENT );

  /* get mapping of 2-D grid WCS */
  gridmapping = astGetMapping( gridfset, AST__BASE, AST__CURRENT );

  /* create a 1-D spectrum frame */
  specframe = astSpecFrame( "System=wavenum,Unit=1/mm,StdOfRest=Topocentric" );

  /* if gridframe is SkyFrame, set attribute: RefRA, RefDec */
  if( astIsASkyFrame( gridframe ) )
  {
    if( astTest( gridframe, "SkyRef" ) )
    {
      refra = astGetD( gridframe, "SkyRef(1)" );
      refdec = astGetD( gridframe, "SkyRef(2)" );
      astSetRefPos( specframe, gridframe, refra, refdec );
    }
  }

  /* create ZoomMap for 1-D spectrum */
  specmapping = astZoomMap( 1, ftswn_factor, "" );

  /* combine 2-D grid mapping and 1-D spectrum mapping to 
   * create 3-D mapping for spectrum cube 
   */
  speccubemapping = astCmpMap( gridmapping, specmapping, 0, "" );
   
  /* create Base Frame for 3-D spectrum cube */
  basefrm3d = astFrame( 3, "DOMAIN=GRID");

  /* create Current Frame for 3-D spectrum cube */
  currentfrm3d = astCmpFrame( gridframe, specframe, "" );

  /* set attributes: Epoch, ObsLon, ObsLat */
  if( astTest( gridframe, "Epoch" ) )
    astSetD( currentfrm3d, "Epoch", astGetD(gridframe, "Epoch" ) );
  if( astTest( gridframe, "ObsLon" ) )
    astSetD( currentfrm3d, "ObsLon", astGetD(gridframe, "ObsLon" ) );
  if( astTest( gridframe, "ObsLat") )
    astSetD( currentfrm3d, "ObsLat", astGetD( gridframe, "ObsLat") );

  /* create WCS for 3-D spectrum cube */
  speccubewcs = astFrameSet( basefrm3d, "" );
  astAddFrame( speccubewcs, AST__BASE, speccubemapping, currentfrm3d );

  /* write WCS into the NDF file */
  ndfPtwcs( speccubewcs, data->file->ndfid, status );

  /* AST end */
  astEnd;

  /* close NDF file */
  smf_close_file(&data, status);

  /* end of NDF */
  ndfEnd( status );
}
