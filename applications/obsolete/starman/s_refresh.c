/********************************************************
  s_refresh -- Refreshes  Starman X window when it is revealed

  pat morris                      Leeds             1993 Jan
*/


/*
**
**  INCLUDE FILES
**
*/

#include <stdio.h>
#include <stdlib.h>

#if defined(vms)
#include "starman_x11_xlib"
#include "starman_x11_xutil"
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif

int main ( int argc, char *argv[] )

{
   Display *vd_id;
   Window  wd_id;
   Pixmap pixmap;
   GC gc_id;
   XGCValues xgcvl;
   XEvent event;
   Bool found, loop;
   unsigned int kx, ky;
   char display_name[100];

#if defined(vms)
      sscanf ( argv[0], "%d", &wd_id );
      sscanf ( argv[1], "%d", &pixmap );
#else
      sscanf ( argv[1], "%lu", &wd_id );
      sscanf ( argv[2], "%lu", &pixmap );
#endif

      sscanf ( argv[3], "%s", display_name );
      vd_id = XOpenDisplay ( display_name );
      XSelectInput ( vd_id, wd_id, StructureNotifyMask | SubstructureNotifyMask
                        | ExposureMask );
      gc_id = XCreateGC ( vd_id, wd_id, None, &xgcvl );
      loop = True;
      while ( loop )
      {

         XNextEvent ( vd_id, &event );
         kx = event.xexpose.width ;
         ky = event.xexpose.height ;
         switch ( event.type ) {
            case Expose:
               if ( kx > 0 && ky > 0 && event.xexpose.window==wd_id ) {
                XCopyArea ( vd_id, pixmap, wd_id, gc_id,
                      event.xexpose.x, event.xexpose.y, kx, ky,
                      event.xexpose.x, event.xexpose.y ) ;
                XFlush (vd_id);
               }
               break;
            case DestroyNotify :
                loop=False;
                break;
         }
       }
       XCloseDisplay (vd_id);
}
