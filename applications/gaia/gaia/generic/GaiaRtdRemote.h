//  Avoid inclusion into files more than once.
#ifndef _GaiaRtdRemote_
#define _GaiaRtdRemote_

//
//+
// Name:
//    GaiaRtdRemote
//
// Purpose:
//    Define GaiaRtdRemote class to extend RtdRemote so that we can
//    control ~/.rtd-remote and keep its contents valid.
//
// Authors:
//    P.W. Draper (PWD)
//
// Copyright:
//    Copyright (C) 2000 Central Laboratory of the Research Councils
//
// History:
//    01-FEB-2000 (PWD):
//       Original version.
//-

#include "RtdImage.h"
#include "RtdRemote.h"

class GaiaRtdRemote : public RtdRemote {

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
