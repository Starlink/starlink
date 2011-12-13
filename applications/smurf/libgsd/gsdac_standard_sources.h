/*
 *+
 *  Name:
 *     gsdac_standards.h

 *  Purpose:
 *     Standard source table for gsd2acsis

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     C include file

 *  Description:
 *     Lookup table of standard source objects.


 *  Authors:
 *     J. Balfour (j.balfour@jach.hawaii.edu)

 *  History:
 *     2008-04-02 (JB):
 *        Original.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

 *-
*/

#ifndef GSDAC_STANDARDS_DEFINED
#define GSDAC_STANDARDS_DEFINED

/* Spectral standard sources. */
static const char* standards[] = {
  "W3(OH)",
  "L1551-IRS5",
  "CRL618",
  "OMC1",
  "N2071IR",
  "OH231.8",
  "IRC+10216",
  "16293-2422",
  "NGC6334I",
  "G34.3",
  "W75N",
  "CRL2688",
  "NGC7027",
  "N7538IRS1",
  ""
};

#endif /* GSDAC_STANDARDS_DEFINED */
