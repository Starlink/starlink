#if !defined( PAL2AST_INCLUDED )  /* Include this file only once */
#define PAL2AST_INCLUDED
/*
*  Name:
*     pal2ast.h

*  Type:
*     C include file.

*  Purpose:
*     Defines new names for symbols exported by the PAL library.

*  Invocation:
*     #include "pal2ast.h"

*  Description:
*     This include file defines a new name for each public function
*     defined by the pal library. The names defined by PAL itself are
*     of the form "palXxx" (e.g. palPvobs) - this include file defines
*     a macro that translates each such name to the form "astPalXxx"
*     (e.g. astPalPvobs). This is done so that the names do not clash
*     with any external PAL library with which the application is linked.
*
*     It should be included at the start of any AST source file that refers
*     to PAL functions using the standard names (e.g. palPvobs).

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     DSB: D.S. Berry (Starlink)

*  History:
*     16-FEB-2012 (DSB):
*        Original version.
*/

/* Rename all PAL functions */
#define palAddet   astPalAddet
#define palAmpqk   astPalAmpqk
#define palCaldj   astPalCaldj
#define palDat	   astPalDat
#define palDe2h	   astPalDe2h
#define palDeuler  astPalDeuler
#define palDh2e	   astPalDh2e
#define palDjcal   astPalDjcal
#define palDmat	   astPalDmat
#define palDs2tp   astPalDs2tp
#define palDtp2s   astPalDtp2s
#define palDtps2c  astPalDtps2c
#define palDtt	   astPalDtt
#define palEcmat   astPalEcmat
#define palEqgal   astPalEqgal
#define palEtrms   astPalEtrms
#define palEvp	   astPalEvp
#define palFk45z   astPalFk45z
#define palFk524   astPalFk524
#define palFk54z   astPalFk54z
#define palGaleq   astPalGaleq
#define palGalsup  astPalGalsup
#define palMappa   astPalMappa
#define palMapqkz  astPalMapqkz
#define palPrebn   astPalPrebn
#define palPrec	   astPalPrec
#define palPrenut  astPalPrenut
#define palPvobs   astPalPvobs
#define palRvgalc  astPalRvgalc
#define palRvlg	   astPalRvlg
#define palRvlsrd  astPalRvlsrd
#define palRvlsrk  astPalRvlsrk
#define palSubet   astPalSubet
#define palSupgal  astPalSupgal
#define palCldj    astPalCldj
#define palDaf2r   astPalDaf2r
#define palDav2m   astPalDav2m
#define palDbear   astPalDbear
#define palDcc2s   astPalDcc2s
#define palDcs2c   astPalDcs2c
#define palDd2tf   astPalDd2tf
#define palDimxv   astPalDimxv
#define palDjcl	   astPalDjcl
#define palDm2av   astPalDm2av
#define palDmxm	   astPalDmxm
#define palDmxv	   astPalDmxv
#define palDpav	   astPalDpav
#define palDrange  astPalDrange
#define palDranrm  astPalDranrm
#define palDsep	   astPalDsep
#define palDsepv   astPalDsepv
#define palDtf2d   astPalDtf2d
#define palDtf2r   astPalDtf2r
#define palDvdv	   astPalDvdv
#define palDvn	   astPalDvn
#define palDvxv	   astPalDvxv
#define palEpb	   astPalEpb
#define palEpb2d   astPalEpb2d
#define palEpj	   astPalEpj
#define palEpj2d   astPalEpj2d
#define palEqeqx   astPalEqeqx
#define palFk5hz   astPalFk5hz
#define palGeoc	   astPalGeoc
#define palGmst	   astPalGmst
#define palHfk5z   astPalHfk5z

/* Rename all PAL global variables */
#define PAL__DPI   AST__PALDPI
#define PAL__D2PI  AST__PALD2PI
#define PAL__DD2R  AST__PALDD2R
#define PAL__DR2AS AST__PALDR2AS
#define PAL__DAS2R AST__PALDAS2R
#define PAL__MJD0  AST__PALMJD0
#define PAL__CR    AST__PALCR
#define PAL__VF    AST__PALVF
#define PAL__SR    AST__PALSR


#endif
