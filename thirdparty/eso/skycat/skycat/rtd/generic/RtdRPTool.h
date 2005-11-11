#ifndef RTDRPTOOL_H
#define RTDRPTOOL_H

/*
 * E.S.O. - VLT project / ESO Archive
 *
 * RtdRPTool.h - class definitions for class RtdRPTool, RtdRecorder,
 *	and RtdPlayback.
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * D.Hopkinson	   20/02/97  Created
 * D.Hopkinson	   17/04/97  Changed name to RtdRPTool and added
 *			     inheritance from this.
 * pbiereic        28/05/01  Included Allan's changes for tcl8.3.3
 */

#include <cstdlib>
#include <cstring>
#include <sys/shm.h>

#include "TclCommand.h"
#include "Mem.h"
#include "rtdImageEvent.h"
#include "tk.h"
#include "FitsIO.h"
#include "Compress.h"
#include "rtdSem.h"
#include "config.h"
#include "RtdRPFile.h"

#define MAXRECORDS	60	// Maximum number of records in file.
#define MAXFILENAMELEN	1024	// Maximum length of file names. (allan: changed from 128)
#define MAXFILESIZE	5	// Maximum size of data file in Mb.
#define SLOWSPEED	4000.	// Time between images when playing slow (in msec)
#define FASTSPEED	200.	// Time between sends when playing fast (in msec)

/*
 * This is an abstract class that simply acts as the parent of the recorder
 * and playback objects. It contains the various common properties, and also
 * carries the definitions for the more abstract Tcl commands executed from
 * the recorder tool.
 */
class RtdRPTool : public TclCommand {

protected:
    char fileName[MAXFILENAMELEN];	// Filename to load images into.
    rtdIMAGE_EVT_HNDL *eventHndl_;	// Event handle for server connection.
    Display *display;			// Server connection.
    Tk_ImageMaster master_;		// Tk master.
    Tk_Window tkwin_;			// Tk window.
    int status_;			// Object status
    int cycleMode_;			// Flag: cycle round to start of file.
    RtdRPFile *fileHandler;		// Pointer to file handler object.

    // Constructor.
    RtdRPTool(Tcl_Interp* interp, char* instname, int argc, char** argv, 
	      Tk_ImageMaster master);

    // Cleanup routine
    void cleanup();

public:
    // Destructor.
    ~RtdRPTool();

    // Overridden subcommand calling method.
    virtual int call(const char *name, int len, int argc, char *argv[]);

    // Subcommand definition methods.
    int close(int argc, char *argv[]);
    int init();
    virtual int filename(int argc, char *argv[]);
    int cycle(int argc, char *argv[]);
    int status(int argc, char *argv[]) {return status_;}
    
};

/*
 * Class definition for the recorder object. This is derived from the general
 * properties of the RtdRPTool object.
 */
class RtdRecorder : public RtdRPTool {

protected:
    char camera_[32];			// Camera to attach to.
    double fileSize_;			// Maximum allowed size of saved file.
    int attached_;			// Flag: currently attached to camera.
    enum fileFormat {
	COMP_FITS,		// Compressed FITS - not implemented
	FITS_CUBE		// FITS cube
    } fileFormat_;			// Format to use in file save.
    int subImage_;			// Flag true if using subimage
    int x0_, y0_;			// Bottom left coords of subimage
    int width_, height_;		// Dimensions of subimage

    // Function called when image event received by recorder.
    static void fileEventProc(ClientData clientData, int mask);

    // Routine to process the incoming image event.
    int processFileEvent();

public:
    // Constructor
    RtdRecorder(Tcl_Interp *interp, char *instname, int argc, char **argv,
		Tk_ImageMaster master);

    // Destructor
    ~RtdRecorder() {}		// currently empty

    // Action routines defined in the Tk_ImageType structure. Most of these
    // are no-ops.
    static int CreateImage(Tcl_Interp*, char *name, int argc, 
#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
                           Tcl_Obj *CONST objv[],
#else
                           char **argv,
#endif
                           Tk_ImageType*, Tk_ImageMaster, ClientData*);

    static ClientData GetImage(Tk_Window, ClientData) {return 0;}
    static void DisplayImage(ClientData, Display*, Drawable,
			     int imageX, int imageY, int width, int height,
			     int drawableX, int drawableY) {}
    static void FreeImage(ClientData, Display*) {}
    static void DeleteImage(ClientData) {}

    // Subcommand calling method.
    int call(const char *name, int len, int argc, char *argv[]);

    // Subcommand definitions
    int record(int argc, char *argv[]);
    int camera(int argc, char *argv[]);
    int file(int argc, char *argv[]);
    int stop(int argc, char *argv[]);
    int subimage(int argc, char *argv[]);
};

/*
 * Class definition for the playback object. This is derived from the general
 * properties of the RtdRPTool object.
 */
class RtdPlayback : public RtdRPTool {

protected:
    int direction_;		// Flag: true if playing forwards
    enum playSpeed {
	SPEED_SLOW,
	SPEED_FAST,
	SPEED_RT
    } playSpeed_;		// Playback speed
    Tcl_TimerToken timer_;	// Timer handler for managing the timer callback
    int spool_;			// Flag: we are doing a fast-forward/rewind.

    // Called from timer callback.
    static void sendEventProc(ClientData clientData);

    // Retrieve data from file and send to rtdServer daemon.
    int sendImage(int reinvoke);

    // Add a timeout to send the next image.
    void makeTimeOut();

    // Method to make a file handler object instance from an existing file
    int makeFileHandler(char *err);

    // Utility to provide a short time interval for the spool increments
    void spoolPause();

public:
    // Constructor
    RtdPlayback(Tcl_Interp *interp, char *instname, int argc, char **argv,
		Tk_ImageMaster master);

    // Destructor
    ~RtdPlayback() {}		// currently empty

    // Action routines defined in the Tk_ImageType structure. Most of these
    // are no-ops at the moment.
    static int CreateImage(Tcl_Interp*, char *name, int argc, 
#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
                           Tcl_Obj *CONST objv[],
#else
                           char **argv,
#endif
                           Tk_ImageType*, Tk_ImageMaster, ClientData*);

    static ClientData GetImage(Tk_Window, ClientData) {return 0;}
    static void DisplayImage(ClientData, Display*, Drawable,
			     int imageX, int imageY, int width, int height,
			     int drawableX, int drawableY) {}
    static void FreeImage(ClientData, Display*) {}
    static void DeleteImage(ClientData) {}

    // Subcommand calling method.
    int call(const char *name, int len, int argc, char *argv[]);

    // Subcommand definitions
    int close(int argc, char *argv[]);
    int filename(int argc, char *argv[]);
    int gotoimage(int argc, char *argv[]);
    int hastime(int argc, char *argv[]);
    int play(int argc, char *argv[]);
    int reset(int argc, char *argv[]);
    int spool(int argc, char *argv[]);
    int props(int argc, char *argv[]);
    int step(int argc, char *argv[]);
    int stop(int argc, char *argv[]);
};

// cleanup shared mem
void Mem_RPTcleanup();

#endif
