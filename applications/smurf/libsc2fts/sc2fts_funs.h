/*
*+
*  Name:
*     sc2fts_funs.h

*  Purpose:
*     Prototypes for the libsc2fts library

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Header File

*  Invocation:
*     #include "sc2fts_funs.h"

*  Description:
*     Prototypes used by the libsc2fts functions.

*  Authors:
*     B.Zhang (UoL)
*     {enter_new_authors_here}

*  History:
*     2008-03-14 (BZ):
*        first draft
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
*     All Rights Reserved.

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
/* STARLINK includes */
#include "ast.h"
#include "star/grp.h"

/* SMURF includes */
#include "libsmf/smf_typ.h"

/* the list of available data reduction operations for FTS-2 */
const char *ops_sc2fts[] = { "IFGMFLATFIELD",  /* parameters: X, Y */
                             "ADDWCS",         /* parameters: X, Y */
                             "FREQCORR",       /* parameters: X, Y */
                             "PORTIMBALANCE",  /* parameters: X, Y */
                             "TRANSCORR",      /* parameters: X, Y */
                             "SPECFLATFIELD",  /* parameters: X, Y */
                             "GROUPCOADD"      /* parameters: X, Y */
                           };
/* functions of FTS-2 calibration modules */
void sc2fts_ifgmflatfield ( smfData* idata, AstKeyMap* parKeymap, int *status );
void sc2fts_addwcs ( smfData* idata, AstKeyMap* parKeymap, int *status );
void sc2fts_freqcorr ( smfData* idata, AstKeyMap* parKeymap, int *status );
void sc2fts_portimbalance ( smfData* idata, AstKeyMap* parKeymap, int *status );
void sc2fts_transcorr ( smfData* idata, AstKeyMap* parKeymap, int *status );
void sc2fts_specflatfield ( smfData* idata, AstKeyMap* parKeymap, int *status );
void sc2fts_groupcoadd ( Grp *igrp, Grp *ogrp, AstKeyMap* parKeymap, int *status );

/* function pointers */
void (*sc2fts_op[])( smfData* idata, AstKeyMap* parKeymap, int *status ) = {
     sc2fts_ifgmflatfield,
     sc2fts_addwcs,
     sc2fts_freqcorr,
     sc2fts_portimbalance,
     sc2fts_transcorr,
     sc2fts_specflatfield,
};

