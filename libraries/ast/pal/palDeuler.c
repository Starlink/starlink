/*
*+
*  Name:
*     palDeuler

*  Purpose:
*     Form a rotation matrix from the Euler angles

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palDeuler ( const char *order, double phi, double theta, double psi,
*                      double rmat[3][3] );

*  Arguments:
*     order = const char[] (Given)
*        Specifies about which axes the rotation occurs
*     phi = double (Given)
*        1st rotation (radians)
*     theta = double (Given)
*        2nd rotation (radians)
*     psi = double (Given)
*        3rd rotation (radians)
*     rmat = double[3][3] (Given & Returned)
*        Rotation matrix

*  Description:
*     A rotation is positive when the reference frame rotates
*     anticlockwise as seen looking towards the origin from the
*     positive region of the specified axis.
*
*     The characters of ORDER define which axes the three successive
*     rotations are about.  A typical value is 'ZXZ', indicating that
*     RMAT is to become the direction cosine matrix corresponding to
*     rotations of the reference frame through PHI radians about the
*     old Z-axis, followed by THETA radians about the resulting X-axis,
*     then PSI radians about the resulting Z-axis.
*
*     The axis names can be any of the following, in any order or
*     combination:  X, Y, Z, uppercase or lowercase, 1, 2, 3.  Normal
*     axis labelling/numbering conventions apply;  the xyz (=123)
*     triad is right-handed.  Thus, the 'ZXZ' example given above
*     could be written 'zxz' or '313' (or even 'ZxZ' or '3xZ').  ORDER
*     is terminated by length or by the first unrecognized character.
*
*     Fewer than three rotations are acceptable, in which case the later
*     angle arguments are ignored.  If all rotations are zero, the
*     identity matrix is produced.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-02-08 (TIMJ):
*        Initial version with documentation taken from Fortran SLA
*        Adapted with permission from the Fortran SLALIB library.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 1997 Rutherford Appleton Laboratory
*     Copyright (C) 2012 Science and Technology Facilities Council.
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

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"
#include "pal1sofa.h"

void
palDeuler( const char *order, double phi, double theta, double psi,
                 double rmat[3][3] ) {
  int i = 0;
  double rotations[3];

  /* Initialise rmat */
  eraIr( rmat );

  /* copy the rotations into an array */
  rotations[0] = phi;
  rotations[1] = theta;
  rotations[2] = psi;

  /* maximum three rotations */
  while (i < 3 && order[i] != '\0') {

    switch (order[i]) {
    case 'X':
    case 'x':
    case '1':
      eraRx( rotations[i], rmat );
      break;

    case 'Y':
    case 'y':
    case '2':
      eraRy( rotations[i], rmat );
      break;

    case 'Z':
    case 'z':
    case '3':
      eraRz( rotations[i], rmat );
      break;

    default:
      /* break out the loop if we do not recognize something */
      i = 3;

    }

    /* Go to the next position */
    i++;
  }

  return;
}
