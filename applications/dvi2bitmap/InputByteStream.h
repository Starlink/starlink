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


#ifndef INPUT_BYTE_STREAM_HEADER_READ
#define INPUT_BYTE_STREAM_HEADER_READ 1

#include <string>
#include "Byte.h"
#include "DviError.h"
#include "verbosity.h"

class InputByteStreamError : public DviError {
 public:
    InputByteStreamError(string s) : DviError(s) { }
};

/**
 * Abstracts a file as a stream of bytes.
 */
class InputByteStream {
 public:
    InputByteStream (string& s, bool preload=false, string tryext="");
    ~InputByteStream();
    bool eof();
    Byte getByte(int n=1);
    signed int getSIU(int);
    signed int getSIS(int);
    unsigned int getUIU(int);
    // static getUIU reads from an array, rather than the file (ie,
    // it's really nothing to do with InputByteStream, but it's here
    // for consistency.
    static unsigned int getUIU(int, const Byte *);
    // retrieve a block from the file.  pos<0 means from end of file.
    const Byte *getBlock (int pos, unsigned int length);
    void seek (unsigned int);
    int pos ();
    void skip (unsigned int);
    /**
     * Sets the verbosity of this module.
     * @param level the required verbosity
     */
    static void verbosity (const verbosities level) { verbosity_ = level; }
 private:
    int fd_;
    string fname_;
    size_t filesize_;
    size_t buflen_;
    Byte *buf_;			// buffer
    Byte *p_;			// current position in buffer 
    Byte *eob_;			// end of buffer
    bool eof_;
    const bool preloaded_;
    void read_buf_(void);

    static verbosities verbosity_;
};
#endif // #ifndef INPUT_BYTE_STREAM_HEADER_READ
