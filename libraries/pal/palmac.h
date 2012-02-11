#ifndef PALMACDEF
#define PALMACDEF

/*
*+
*  Name:
*     palmac.h

*  Purpose:
*     Macros used by the PAL library

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Include file

*  Description:
*     A collection of useful macros provided and used by the PAL library

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*

*  History:
*     2012-02-08 (TIMJ):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
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
*    You should have received a copy of the GNU General Public License
*    along with this program; if not, write to the Free Software
*    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
*    USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Pi */
#define PAL__DPI (3.141592653589793238462643)

/* 2Pi */
#define PAL__D2PI (6.283185307179586476925287)

/* pi/180:  degrees to radians */
#define PAL__DD2R 0.017453292519943295769236907684886127134428718885417

/* Start of SLA modified Julian date epoch */
#define PAL__MJD0 2400000.5

#endif
