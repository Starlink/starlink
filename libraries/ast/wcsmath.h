/*=============================================================================
*
*   WCSLIB - an implementation of the FITS WCS proposal.
*   Copyright (C) 1995-2002, Mark Calabretta
*
*   This library is free software; you can redistribute it and/or modify it
*   under the terms of the GNU Library General Public License as published
*   by the Free Software Foundation; either version 2 of the License, or (at
*   your option) any later version.
*
*   This library is distributed in the hope that it will be useful, but
*   WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library
*   General Public License for more details.
*
*   You should have received a copy of the GNU Library General Public License
*   along with this library; if not, write to the Free Software Foundation,
*   Inc., 51 Franklin Street,Fifth Floor, Boston, MA 02110-1301, USA
*
*   Correspondence concerning WCSLIB may be directed to:
*      Internet email: mcalabre@atnf.csiro.au
*      Postal address: Dr. Mark Calabretta,
*                      Australia Telescope National Facility,
*                      P.O. Box 76,
*                      Epping, NSW, 2121,
*                      AUSTRALIA
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   $Id$
*=============================================================================
*
*  This version of wcstrig.h is based on the version in wcslib-2.9, but has
*  been modified in the following ways by the Starlink project (e-mail:
*  ussc@star.rl.ac.uk):
*     -  Changed the name of the WCSLIB_MATH macro to WCSLIB_MATH_INCLUDED
*===========================================================================*/

#ifndef WCSLIB_MATH_INCLUDED
#define WCSLIB_MATH_INCLUDED

#ifdef PI
#undef PI
#endif

#ifdef D2R
#undef D2R
#endif

#ifdef R2D
#undef R2D
#endif

#ifdef SQRT2
#undef SQRT2
#endif

#ifdef SQRT2INV
#undef SQRT2INV
#endif

#define PI 3.141592653589793238462643
#define D2R PI/180.0
#define R2D 180.0/PI
#define SQRT2 1.4142135623730950488
#define SQRT2INV 1.0/SQRT2

#endif /* WCSLIB_MATH_INCLUDED */
