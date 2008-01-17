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
//    $Id: DviFilePosition.h,v 1.1 2005/12/08 09:58:00 normang Exp $

#ifndef DVIFILEPOSITION_HEADER_READ
#define DVIFILEPOSITION_HEADER_READ 1

#include "DviFile.h"
#include "verbosity.h"

/**
 * Represents a location within the DviFile.  Coordinates
 * <code>x</code> and <code>y</code> are relative to the top-left
 * corner of the `page', with the pixel in the top-left corner
 * having coordinates (0,0).  Note that this is different from DVI
 * coordinates, which have their origin at a point one inch in and one
 * inch down from the top-left of the `paper'.
 *
 */
class DviFilePosition {
 private:
    int x, y;                   /* position in scaled points */
    DviFile* dvif;
    static enum verbosities verbosity_;
 public:
    DviFilePosition(DviFile*, double x, double y, DviFile::DviUnits);
    DviFilePosition(DviFilePosition*, double x, double y, DviFile::DviUnits);
    double getX(const DviFile::DviUnits u);
    double getY(const DviFile::DviUnits u);
    void shift(double dx, double dy, DviFile::DviUnits u);
    void scale(double factor);
    //void scale(double factor, DviFilePosition& fixed_point);
    // make a static copy
    DviFilePosition* copy();
    static verbosities verbosity(const verbosities level) {
        enum verbosities oldv = verbosity_;
        verbosity_ = level;
        return oldv;
    }
};

#endif /* DVIFILEPOSITION_HEADER_READ */
