/*
*+
*  Name:
*     smurflib.h

*  Purpose:
*     Prototypes and constants for libsmurf functions

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Header File

*  Invocation:
*     #include "smurflib.h"

*  Description:
*     Prototypes and constants used by the libsmurf functions.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-09-27 (TIMJ):
*        Initial test version
*     2006-01-24 (TIMJ):
*        Add makemap
*     2006-02-16 (AGG):
*        Add remsky
*     2006-03-16 (AGG):
*        Add qlmakemap
*     2006-06-06 (AGG):
*        Add smurf_sim
*     2006-06-13 (AGG):
*        Add smurf_dreamsolve
*     2006-07-27 (TIMJ):
*        Add MAKECUBE
*     2006-09-13 (JB):
*        Add BADBOLOS
*     2006-09-15 (AGG):
*        Add DREAMWEIGHTS
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council, University of British Columbia.  All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/
#ifndef SMURF_LIB_DEFINED
#define SMURF_LIB_DEFINED

void smurf_extinction( int * );
void smurf_flatfield( int * );
void smurf_makemap( int * );
void smurf_qlmakemap( int * );
void smurf_makecube( int * );
void smurf_remsky( int * );
void smurf_scanfit( int * );
void smurf_sc2sim( int * );
void smurf_dreamsolve( int * );
void smurf_dreamweights( int * );
void smurf_badbolos( int * );
void smurf_impaztec( int * );
void smurf_skynoise( int * );

#endif /* SMURF_LIB_DEFINED */
