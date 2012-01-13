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
*     -  Support for non-ANSI C prototypes removed
*     -  Changed the name of the WCSLIB_TRIG macro to WCSLIB_TRIG_INCLUDED
*     -  Changed names of degrees trig functions to avoid clashes with
*        wcslib.
*===========================================================================*/

#ifndef WCSLIB_TRIG_INCLUDED
#define WCSLIB_TRIG_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

double astCosd(const double);
double astSind(const double);
double astTand(const double);
double astACosd(const double);
double astASind(const double);
double astATand(const double);
double astATan2d(const double, const double);

/* Domain tolerance for asin and acos functions. */
#define WCSTRIG_TOL 1e-10

#ifdef __cplusplus
};
#endif

#endif /* WCSLIB_TRIG_INCLUDED */
