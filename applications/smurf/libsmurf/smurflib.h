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
*     Edward Chapin (UBC)
*     COBA: Coskun Oba (UoL)
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
*     2006-10-26 (AGG):
*        Add STARECALC
*     2006-11-01 (TIMJ):
*        Add SMURFHELP
*     2007-11-08 (DSB):
*        Add TIMESORT.
*     2008-02-12 (AGG):
*        Add UNMAKECUBE, RAWUNPRESS
*     2008-03-27 (EC):
*        Add SC2CLEAN
*     2008-04-21 (JB):
*        Add GSDSHOW.
*     2008-06-10 (TIMJ):
*        Add SC2FTS
*     2008-07-22 (EC):
*        Add SC2FFT
*     2008-08-14 (TIMJ):
*        Add smurfcopy
*     2008-08-26 (TIMJ):
*        Add calcflat / calcdark
*     2009-09-27 (TIMJ):
*        Add calcresp
*     2009-05-19 (TIMJ):
*        Add RAWFIXMETA
*     2010-07-19 (COBA):
#         Add FTS2_FLATFIELD, FTS2_FREQCORR, FTS2_PORTIMBAL,
#             FTS2_REMOVEBSE, FTS2_SPATIALWCS, FTS2_TRANSCORR
*     2010-08-27 (COBA):
#         Remove smurf_sc2fts( int * )
*     2010-09-17 (COBA):
#         Add FTS2_PHASECORR
*     2010-09-30 (COBA):
#         Add FTS2_EQSLICED
*     2010-11-03 (COBA):
#         Add smurf_fts2_specre
*     2010-11-24 (COBA):
#         Add smurf_fts2_init
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2009 Science and Technology Facilities Council.
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council, University of British Columbia.  All Rights Reserved.

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
#ifndef SMURF_LIB_DEFINED
#define SMURF_LIB_DEFINED

void smurf_badbolos( int * );
void smurf_calcdark( int * );
void smurf_calcflat( int * );
void smurf_calcnoise( int * );
void smurf_calcqu( int * );
void smurf_calcresp( int * );
void smurf_copyflat( int * );
void smurf_dreamsolve( int * );
void smurf_dreamweights( int * );
void smurf_dsutils( int * );
void smurf_extinction( int * );
void smurf_flatfield( int * );
void smurf_fixsteps( int * );
void smurf_fts2_deglitch( int * );
void smurf_fts2_eqsliced( int * );
void smurf_fts2_flatfield( int * );
void smurf_fts2_freqcorr( int * );
void smurf_fts2_init( int * );
void smurf_fts2_phasecorr( int * );
void smurf_fts2_portimbal( int * );
void smurf_fts2_removebse( int * );
void smurf_fts2_spatialwcs( int * );
void smurf_fts2_specre( int * );
void smurf_fts2_transcorr( int * );
void smurf_gsd2acsis( int * );
void smurf_gsdshow( int * );
void smurf_impaztec( int * );
void smurf_makecube( int * );
void smurf_makemap( int * );
void smurf_qlmakemap( int * );
void smurf_rawfixmeta( int * );
void smurf_rawpress( int * );
void smurf_rawunpress( int * );
void smurf_remsky( int * );
void smurf_sc2clean( int * );
void smurf_sc2concat( int * );
void smurf_sc2expandmodel( int * );
void smurf_sc2fft( int * );
void smurf_sc2sim( int * );
void smurf_sc2threadtest( int * );
void smurf_skynoise( int * );
void smurf_smurfcopy( int * );
void smurf_smurfhelp( int * );
void smurf_stackframes( int * );
void smurf_starecalc( int * );
void smurf_timesort( int * );
void smurf_unmakecube( int * );

#endif /* SMURF_LIB_DEFINED */
