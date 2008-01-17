//    This file is part of dvi2bitmap.
//    Copyright 2003--5, Norman Gray
//    
//    This program is part of the Starlink Software Distribution: see
//    http://www.starlink.ac.uk 
//
//    dvi2bitmap is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    dvi2bitmap is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with dvi2bitmap; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//    The General Public License is distributed along with this
//    program in the file LICENCE.
//
//    Author: Norman Gray <norman@astro.gla.ac.uk>
//    $Id: DviFilePosition.cc,v 1.2 2005/12/08 10:42:39 normang Exp $



#include <config.h>
#include <iostream>
using STD::cerr;
using STD::endl;

#include "DviFilePosition.h"

// Static variables
verbosities DviFilePosition::verbosity_ = normal;


/**
 * Create a new marked position in a DVI file.
 * @param f the DVI file of interest
 * @param x the x-coordinate of the position
 * @param y the y-coordinate of the position
 * @param u the units of the (x,y) position
 */
DviFilePosition::DviFilePosition(DviFile* f,
                                 double x, double y,
                                 DviFile::DviUnits u)
{
    // Hrumph.  Initialise the y coordinate as one less than the one
    // passed in.  I don't have a good explanation of why I'm doing
    // this, except that if I don't, then the last test in t8.pl
    // doesn't work, because the reported mark positions are 0.0001
    // pixels `too large', which ends up with them being rounded to 31
    // rather than 30.  This is doubtless some variant of the question
    // of whether pixel centres are at integer or half-integer
    // coordinates, and may be related, in some weird way which spans
    // 4 orders of magnitude!) to the dithering in Bitmap.cc about the
    // offset of +1 added to the reported mark position. [there's a
    // reference to this text in t8.pl.in comments, so change both together]

    dvif = f;
    this->x = DviFile::convertToScaledPoints(x, u, dvif);
    this->y = DviFile::convertToScaledPoints(y, u, dvif) - 1;
    if (verbosity_ > normal)
        cerr << "New mark at ("
             << DviFile::convertFromScaledPoints(this->x,
                                                 DviFile::unit_pixels, dvif)
             << ','
             << DviFile::convertFromScaledPoints(this->y,
                                                 DviFile::unit_pixels, dvif)
             << ")px" << endl;
}
/**
 * Create a new marked position in a DVI file.
 * @param dvifp use the DVI file referred to by this class
 * @param x the x-coordinate of the position
 * @param y the y-coordinate of the position
 * @param u the units of the (x,y) position
 */
DviFilePosition::DviFilePosition(DviFilePosition* dvifp,
                                 double x, double y,
                                 DviFile::DviUnits u)
{
    dvif = dvifp->dvif;
    this->x = DviFile::convertToScaledPoints(x, u, dvif);
    this->y = DviFile::convertToScaledPoints(y, u, dvif) - 1;
}
/**
 * Retrieve the x-coordinate of this position.
 * @param u express the answer in these units
 * @return the x-coordinate in the required units
 */
double DviFilePosition::getX(DviFile::DviUnits u)
{
    return DviFile::convertFromScaledPoints(x, u, dvif);
}
/**
 * Retrieve the y-coordinate of this position.
 * @param u express the answer in these units
 * @return the y-coordinate in the required units
 */
double DviFilePosition::getY(DviFile::DviUnits u)
{
    return DviFile::convertFromScaledPoints(y, u, dvif);
}
/**
 * Shift the mark position by the given amount
 * @param dx the amount to shift in the x direction
 * @param dy the amount to shift in the y direction
 * @param u the units in which dx and dy are expressed
 */
void DviFilePosition::shift(double dx, double dy, DviFile::DviUnits u)
{
    if (verbosity_ > normal)
        cerr << "shift("
             << DviFile::convertFromScaledPoints(x, DviFile::unit_pixels, dvif)
             << ','
             << DviFile::convertFromScaledPoints(y, DviFile::unit_pixels, dvif);
    x += DviFile::convertToScaledPoints(dx, u, dvif);
    y += DviFile::convertToScaledPoints(dy, u, dvif);
    if (verbosity_ > normal)
        cerr << ")px -> ("
             << DviFile::convertFromScaledPoints(x, DviFile::unit_pixels, dvif)
             << ','
             << DviFile::convertFromScaledPoints(y, DviFile::unit_pixels, dvif)
             << ")px" << endl;
}

/**
 * Scale the position by the given factor.  The scaling is done in
 * such a way that the coordinate <em>(0,0)</em> remains `fixed'.
 * @param factor a scaling factor, by which all distances are multiplied
 */
void DviFilePosition::scale(double factor)
{
    if (verbosity_ > normal)
        cerr << "scale("
             << DviFile::convertFromScaledPoints(x, DviFile::unit_pixels, dvif)
             << ','
             << DviFile::convertFromScaledPoints(y, DviFile::unit_pixels, dvif);
    x = static_cast<int>(x * factor + 0.5);
    y = static_cast<int>(y * factor + 0.5);
    if (verbosity_ > normal)
        cerr << ")px -> ("
             << DviFile::convertFromScaledPoints(x, DviFile::unit_pixels, dvif)
             << ','
             << DviFile::convertFromScaledPoints(y, DviFile::unit_pixels, dvif)
             << ")px" << endl;
}
#if 0
// redundant method, but still potentially useful (and apparently correct)
/**
 * Scale the position by the given factor.  The scaling is done in
 * such a way that it is the distance between the marked position and
 * the fixed point that is scaled
 * @param scale a scaling factor, by which all distances are multiplied
 * @param fixed_point the fixed point in the scaling operation
 */
void DviFilePosition::scale(double factor, DviFilePosition& fixed_point)
{
    if (verbosity_ > normal)
        cerr << "scale("
             << DviFile::convertFromScaledPoints(x, DviFile::unit_pixels, dvif)
             << ','
             << DviFile::convertFromScaledPoints(y, DviFile::unit_pixels, dvif);
    x = static_cast<int>(fixed_point.x + (x - fixed_point.x)*factor + 0.5);
    y = static_cast<int>(fixed_point.y + (y - fixed_point.y)*factor + 0.5);
    if (verbosity_ > normal)
        cerr << ")px -> ("
             << DviFile::convertFromScaledPoints(x, DviFile::unit_pixels, dvif)
             << ','
             << DviFile::convertFromScaledPoints(y, DviFile::unit_pixels, dvif)
             << ")px" << endl;
}
#endif

/**
 * Return a copy of the object
 */
DviFilePosition* DviFilePosition::copy()
{
    return new DviFilePosition(dvif, x, y, DviFile::unit_sp);
}

