//  Avoid inclusion into files more than once.
#ifndef _GaiaRtdRemote_
#define _GaiaRtdRemote_

/*+
 * Name:
 *    GaiaRtdRemote

 * Purpose:
 *    Define GaiaRtdRemote class to extend RtdRemote so that we can
 *    control ~/.rtd-remote and keep its contents valid.

 * Authors:
 *    P.W. Draper (PWD)

 *  Copyright:
 *     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 * History:
 *    01-FEB-2000 (PWD):
 *       Original version.
 *-
 */

#include "RtdImage.h"
#include "RtdRemote.h"

class GaiaRtdRemote : public RtdRemote
{
 protected:

    //  Reference to rtdimage. Used for calling its methods.
    RtdImage *rtdimage_;

    //  Static members to store connection information (port numbers).
    enum {MAXPORTS = 64};
    static int zeroports_;
    static int savedports_[];

    //  Slot where the port number of this object is stored.
    int portslot_;

    //  Members to save, remove and restore ports.
    void saveConnection();
    void removeConnection();
    void restoreConnection();

    //  Create an additional status file with pid/port info.
    int makeExtraStatusFile();

    //  Get name of one of the status files.
    int getStatusName( int extra, char *filename );

 public:

    //  Constructor
    GaiaRtdRemote( RtdImage *rtdimage, int port );

    //  Destructor
    ~GaiaRtdRemote();

    //  Call an rtdimage command method by name
    int call( const char *name, int len, int argc, char *argv[] ) {
        return rtdimage_->call( name, len, argc, argv );
    }
};

#endif // GaiaRtdRemote
