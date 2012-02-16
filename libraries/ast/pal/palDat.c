/*
*+
*  Name:
*     palDtt

*  Purpose:
*     Return offset between UTC and TT

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     dat = palDat( double utc );

*  Arguments:
*     utc = double (Given)
*        UTC date as a modified JD (JD-2400000.5)

*  Returned Value:
*     dat = double
*        TAI-UTC in seconds

*  Description:
*     Increment to be applied to Coordinated Universal Time UTC to give
*     International Atomic Time (TAI).

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - This routine converts the MJD argument to calendar date before calling
*       the SOFA iauDat function.
*     - This routine matches the slaDat interface which differs from the iauDat
*       interface. Consider coding directly to the SOFA interface.
*     - See iauDat for a description of error conditions when calling this function
*       with a time outside of the UTC range.
*     - The status argument from iauDat is ignored. This is reasonable since the
*       error codes are mainly related to incorrect calendar dates when calculating
*       the JD internally.

*  History:
*     2012-02-08 (TIMJ):
*        Initial version
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

#include "pal.h"
#include "palmac.h"

#include "sofa.h"

double palDat ( double dju ) {
  int iy;
  int im;
  int id;
  int status;
  double fd;
  double deltat;

  iauJd2cal( PAL__MJD0, dju,
             &iy, &im, &id, &fd );

  status = iauDat( iy, im, id, fd, &deltat );
  return deltat;
}
