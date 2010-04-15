/*
   starX_test.c

   Noddy X test program for the starX X11 linking facility.
   Borrowed 31-Jan-1995 from Dave Terret (Starlink, RAL), was xprobe.c
*/

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>

int main()
{
    Display *display;
    XVisualInfo visinfo, *vis;
    int i, n;
    char *classnames[6];

    for (i = 0; i < 6; i++)
    {
	switch(i)
	{
	case PseudoColor: classnames[i] = "PseudoColor"; break;
	case TrueColor: classnames[i] = "TrueColor"; break;
	case DirectColor: classnames[i] = "DirectColor"; break;
	case StaticColor: classnames[i] = "StaticColor"; break;
	case GrayScale: classnames[i] = "GrayScale"; break;
	case StaticGray: classnames[i] = "StaticGray"; break;
	}
    }

    if (display = XOpenDisplay(NULL))
    {
	printf("Default visual type is %s\n",classnames[DefaultVisual(display,
		DefaultScreen(display))->class]);
	printf("Number of planes is %d\n\n", DisplayPlanes(display,
						    DefaultScreen(display)));
        for( i = 0; i < 6; i++)
	{
	    visinfo.class = i;
	    vis = XGetVisualInfo(display, VisualClassMask, &visinfo, &n);
	    if (vis) printf("%s supported, %d planes\n",classnames[vis->class],
		vis->depth);
	    XFree((char*)vis);
	}

	XCloseDisplay(display);
    }
    else
    {
	printf("Unable to open display\n");
    }
exit(0);
}
