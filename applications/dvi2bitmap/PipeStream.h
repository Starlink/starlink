/*
 *    This file is part of dvi2bitmap.
 *    Copyright 2003, Council for the Central Laboratory of the Research Councils
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
 *    $Id$
 */

#ifndef PIPE_STREAM_HEADER_READ
#define PIPE_STREAM_HEADER_READ 1

#include <InputByteStream.h>

class PipeStream : public InputByteStream {
 public:
    PipeStream (string cmd, string envs="")
	    throw (InputByteStreamError);
    string getResult(void)
	    throw (InputByteStreamError);
    /**
     * Return the status of the command at the end of the pipe.  This
     * is only available after the command has run to completion.
     *
     * @return the exit status of the process, or negative if no
     * status is yet available
     */
    int getStatus(void) const { return pipe_status_; };
    int getPid(void) const { return pid_; };
 protected:
    void closedFD(void);
 private:
    int pipe_status_;
    int pid_;
    const string orig_command_;
    /* void read_status_(void); */
};


#endif /* PIPE_STREAM_HEADER_READ */
