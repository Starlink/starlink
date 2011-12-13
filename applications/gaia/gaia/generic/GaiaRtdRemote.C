/*+
 *  Name:
 *     GaiaRtdRemote

 *  Language:
 *     C++

 *  Purpose:
 *     Defines the members of the GaiaRtdRemote class.

 *  Authors:
 *     P.W. Draper (PWD)

 *  Copyright:
 *     Copyright (C) 2000 Central Laboratory of the Research Councils
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

 *  History:
 *     01-FEB-2000 (PWD):
 *        Original version.
 *     26-MAY-2000 (PWD):
 *        Added maintenance of addition rtd-remote file. This allows
 *        multiple GAIA instances to co-exist using the same home
 *        directory (and the remote control interface).
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

//  Include files.
#include <unistd.h>
#include "GaiaRtdRemote.h"
#include "error.h"
#include "define.h"
#include "tcl.h"

//  Initialise static members.
int GaiaRtdRemote::zeroports_ = 1;
int GaiaRtdRemote::savedports_[MAXPORTS];

//+
//   Constructor.
//-
GaiaRtdRemote::GaiaRtdRemote( RtdImage *rtdimage, int port )
   : RtdRemote( rtdimage->interp(), port, rtdimage->verbose() ),
     rtdimage_( rtdimage ),
     portslot_( -1 )
{
   //  If not done initialise the store of ports.
   if ( zeroports_ ) {
      for ( int i = 0; i < MAXPORTS; i++ ) {
         savedports_[i] = 0;
      }
      zeroports_ = 0;
   }

   //  Add details of bound socket to the list of possible places to
   //  connect too.
   saveConnection();

   //  Create additional rtd-remote file, if needed. Note we cannot
   //  override makeStatusFile as it is called in the RtdRemote
   //  constructor, so the compromise here is to maintain (up to) two
   //  versions of rtd-remote.
   if ( ( status_ = makeExtraStatusFile() ) != 0 ) {
      return;  // This is an error.
   }
}

//+
//   Destructor.
//-
GaiaRtdRemote::~GaiaRtdRemote()
{
   //  Restore a previous connection, if available.
   removeConnection();
   restoreConnection();
}

//+
//   GaiaRtdRemote::saveConnection
//
//   Purpose:
//      Save the current connection values so that they can be
//      restored when needed.
//-
void GaiaRtdRemote::saveConnection()
{

   //  Save a copy of the port number. The other information can be
   //  recovered (hostname and process id). Look for a free slot.
   portslot_ = -1;
   for ( int i = 0; i < MAXPORTS; i++ ) {
      if ( savedports_[i] == 0 ) {
         portslot_ = i;
         break;
      }
   }
   if ( portslot_ == -1 ) {
      // Run out of slots, just hope that the MAXPORT other windows do
      // not all get deleted this time before this one (so one of them
      // will be restored).
      return;
   }
   savedports_[portslot_] = port_;
   return;
}

//+
//   GaiaRtdRemote::removeConnection
//
//   Purpose:
//      Remove the current connection values so that they are no
//      longer used.
//-
void GaiaRtdRemote::removeConnection()
{
   if ( portslot_ != -1 ) {
      savedports_[portslot_] = 0;
      portslot_ = -1;
   }
}

//+
//   GaiaRtdRemote::restoreConnection
//
//   Purpose:
//      Restore another connection (port) from the savedports_ list.
//      This keeps both status files up to date.
//-
void GaiaRtdRemote::restoreConnection()
{
   int slot = -1;
   for ( int i = 0; i < MAXPORTS; i++ ) {
      if ( savedports_[i] != 0 ) {
         slot = i;
         break;
      }
   }
   if ( slot != -1 ) {
      int port = savedports_[slot];
      for ( int i = 0; i < 2; i++ ) {
         char filename[1024];
         if ( getStatusName( i, filename ) ) {
            FILE *f = fopen( filename, "w+" );
            if ( f ) {
               char hostname[80];
               if (gethostname( hostname, sizeof(hostname) ) != 0) {
                  strcpy( hostname, "localhost" );
               }
               fprintf( f, "%u %s %u\n", getpid(), hostname, port );
               fclose( f );
            }
         }
      }
   }
   return;
}

//+
//   GaiaRtdRemote::makeExtraStatusFile
//
//   Purpose:
//     Possibily create an additional.rtd-remote file so that it can
//     be placed in different directories. This is so that multiple
//     GAIA processes can share the same home directory and still be
//     controlled through the remote interface. The position of the
//     extra file is controlled by the RTD_REMOTE_DIR environment
//     variable.
//-
int GaiaRtdRemote::makeExtraStatusFile()
{
   char filename[1024];
   if ( getStatusName( 1, filename ) ) {
      FILE *f = fopen( filename, "w+" );
      if ( !f ) {
         return sys_error( filename );
      }
      char hostname[80];
      if ( gethostname( hostname, sizeof(hostname) ) != 0 ) {
         strcpy(hostname, "localhost");
      }
      fprintf( f, "%u %s %u\n", getpid(), hostname, port_ );
      fclose( f );
   }
   return 0;
}

//+
//   GaiaRtdRemote::getStatusName
//
//   Purpose:
//      Get the name of one of the status files. This is either the
//      standard one found in $(HOME) or /tmp, or the additional file
//      whose position is controlled by $(RTD_REMOTE_DIR). If no name
//      can be produced 0 is returned, otherwise 1 is returned.  The
//      filename pointer must point to enough space to contain the
//      name.
//-
int GaiaRtdRemote::getStatusName( int extra, char *filename )
{
   if ( extra ) {

      //  Want name of addition status file.
      char *rtdremote = getenv( "RTD_REMOTE_DIR" );
      if ( rtdremote ) {
         sprintf( filename, "%s/.rtd-remote", rtdremote );
         return 1;
      }
   } else {
      char* home = getenv("HOME");
      sprintf( filename, "%s/.rtd-remote", (home ? home : "/tmp") );
      return 1;
   }
   return 0;
}
