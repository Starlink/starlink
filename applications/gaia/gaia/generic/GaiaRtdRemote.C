//+
//  Name:
//     GaiaRtdRemote
//
//  Language:
//     C++
//
//  Purpose:
//     Defines the members of the GaiaRtdRemote class.
//
//  Authors:
//     P.W. Draper (PWD)
//
//  Copyright:
//     Copyright (C) 2000 Central Laboratory of the Research Councils
//
//  History:
//     01-FEB-2000 (PWD):
//        Original version.
//-

//  Include files.
#include <unistd.h>
#include "GaiaRtdRemote.h"
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
   portslot_( -1 ),
   rtdimage_( rtdimage )
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
      char filename[1024];
      char* home = getenv( "HOME" );
      sprintf( filename, "%s/.rtd-remote", (home ? home : "/tmp") );
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
   return;
}
