//    This file is part of dvi2bitmap.
//    Copyright 1999--2002, Council for the Central Laboratory of the Research Councils
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
//    $Id$

#ifndef DVIERROR_HEADER_READ
#define DVIERROR_HEADER_READ 1

#include <string>
using std::string;

class DviError {
 public:
    DviError(const string s) : problem_(s) { }
    DviError(const char *fmt, ...);
    virtual ~DviError() { }
    void print() const;
    string problem() const { return problem_; }
 protected:
    DviError() { };
    string problem_;
};
class DviBug : public DviError {
 public:
    DviBug(const string s) : DviError(s) { }
    DviBug(const char *fmt, ...);
    void print() const;
};

#endif /* DVIERROR_HEADER_READ */
