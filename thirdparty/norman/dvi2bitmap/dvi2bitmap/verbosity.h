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
//    $Id: verbosity.h,v 1.4 2003/06/15 17:55:47 norman Exp $


#ifndef VERBOSITY_HEADER_READ
#define VERBOSITY_HEADER_READ 1

/**
 * Expresses a verbosity level.  Several classes report a different
 * level of detail depending on the value of some internal verbosity
 * variable, usually set with a function such as {@link
 * DviFile#verbosity}.
 *
 * <p>The initial value of this is always <code>normal</code>.  The
 * value <code>everything</code> is the highest level (but not
 * <em>necessarily</em> higher than <code>debug</code>; the value
 * <code>silent</code> causes the class to produce no output at all.
 *
 * <p>The values are ordered, so that 
 * <pre>
 *   enum verbosities verbosity_;
 *   ...
 *   if (verbosity_ > normal) {
 *     // chatter
 *   }
 * </pre>
 * is a good way of testing whether the verbosity level is above normal.
 */
enum verbosities { silent, quiet, normal, debug, everything };
// leave `everything' as the last in this enum

#endif
