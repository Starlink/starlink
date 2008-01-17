/*
 *    This file is part of dvi2bitmap.
 *    Copyright 1999--2002, Council for the Central Laboratory of the Research Councils
 *    
 *    This program is part of the Starlink Software Distribution: see
 *    http://www.starlink.ac.uk 
 *
 *    dvi2bitmap is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    dvi2bitmap is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with dvi2bitmap; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *    The General Public License is distributed along with this
 *    program in the file LICENCE.
 *
 *    Author: Norman Gray <norman@astro.gla.ac.uk>
 *    $Id: FileByteStream.h,v 1.2 2003/08/22 14:05:43 norman Exp $
 */


#ifndef FILE_BYTE_STREAM_HEADER_READ
#define FILE_BYTE_STREAM_HEADER_READ 1

#include <InputByteStream.h>

class FileByteStream : public InputByteStream {

 public:
    FileByteStream(string& filename,
		   string tryext="",
		   bool preload=false)
	    throw (InputByteStreamError);
    ~FileByteStream();
    void seek(int pos)
	    throw (InputByteStreamError);    

 private:
    int filesize_;
};

#endif	/* FILE_BYTE_STREAM_HEADER_READ */
