// -*-c++-*-
#ifndef _TkWidget_H_
#define _TkWidget_H_
/* 
 * TkWidget.h - base class for Tk widgets implemented in C++.
 * 
 * This class adds some features to the TclCommand class for 
 * implementing Tk widgets.
 * 
 * See the man page for a complete description.
 *
 * -----------------------------------------------------------------------------
 * "@(#) $Id: TkWidget.h,v 1.2 2005/02/02 01:43:02 brighton Exp $" 
 */

#include <tk.h>
#include "TclCommand.h"

/* 
 * A struct derived from this one is used to hold the values
 * modified by the configure widget command
 */
class TkWidgetOptions {};


/* 
 * This is the base class for classes defining tk widgets from
 * C++ classes
 */
class TkWidget : public TclCommand {
protected:
    Tk_Window tkwin_;			// widget's Tk window
    Display *display_;	                // X token for the window's display

    char* pname_;			// name of parent window
    char* wclass_;                      // this widget's class

    Tk_ConfigSpec* configSpecsPtr_;     // ptr to widget's config struct
    TkWidgetOptions* optionsPtr_;       // ptr to struct holding option values

    int redraw_pending_;                // flag: true if widget needs to be redrawn


    // do first time widget configuration 
    virtual int initWidget(int argc, char* argv[]);

    // configure the widget with cmd line options.
    virtual int configureWidget(int argc, char* argv[], int flags = 0);
    
    // If not already scheduled, schedule the widget to be redrawn
    virtual int eventually_redraw();

    // this method may be redefined in a derived class to redraw
    // the widget.
    virtual void redraw();

    // called for DestroyNotify events in the widget 
    virtual void destroyNotify(XEvent *eventPtr);

    // called for ConfigureNotify events (resize)
    virtual void configureNotify(XEvent *eventPtr);

public:

    // constructor: pclass is the expected parent window class type,
    // the specs and options args are used to process the command line
    // args and for the configure widget command.
    TkWidget(Tcl_Interp*, const char* pclass, 
	     Tk_ConfigSpec* specs, TkWidgetOptions& options,
	     int argc, char* argv[]);

    // destructor
    virtual ~TkWidget();

    // call a member function by name
    virtual int call(const char* name, int len, int argc, char* argv[]);

    virtual int configureCmd(int argc, char* argv[]);

    // called for the cget widget command
    virtual int cgetCmd(int argc, char* argv[]);

    // Redraw the widget by calling the virtual redraw_ method
    // which may be defined in a derived class.
    static void redrawWidget(ClientData);

    // called for StructureNotify events in the widget
    static void structureNotify(ClientData clientData, XEvent *eventPtr);
 
    // invoked by Tk_EventuallyFree to clean up widget 
    static void destroyProc(char *clientData);
};


#endif /* _TkWidget_H_ */


