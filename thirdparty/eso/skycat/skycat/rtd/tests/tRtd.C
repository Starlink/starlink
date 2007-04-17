/*
 * E.S.O. - VLT project 
 * $Id: tRtd.C,v 1.1.1.1 2006/01/12 16:38:02 abrighto Exp $
 *
 * tRtd.C - test RTD real-time updates by sending image and rapid frame
 *          
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * pbiereic        05/02/03   Complete new version which supports all
 *                            data types implemented in RTD.
 */

/************************************************************************
 *
 *  DESCRIPTION
 *    tRtd is used as a "test camera" for testing RTD image events.
 *    The options for tRtd can be set either via command line options
 *    or the RTD widget (tRtd.tcl) which is shown when RTD is started with the
 *    "-debug 1" option.
 *    tRtd generates image events at a defined speed (option -t). By default,
 *    it uses semaphore locking to avoid "image jitters" and other (nasty)
 *    side effects which can be seen when the option "-l 0" is set.
 *    tRtd generates the images in a sort of "movie" style, i.e. images continuously
 *    change so that image updates can be seen in the RTD image. A reference
 *    point at the position REF_PIXEL is generated which is used for analysis.
 *    Another point moves continuously across the image.
 *
 *    All data types which are implemented in RTD are supported (option -D).
 *    Byte swapped images can be generated with option (-E); this allows to
 *    test eg. images which were produced on a Linux-PC and which are transferred
 *    to a HP/Sun machine for display via RTD.
 *    
 *    Three different images can be generated with tRtd:
 *      o a continuously changing image pattern   
 *      o a Fits image (eg. ngc1275.fits) with a moving area at the star position
 *        (XS, YS). This image is used for tests with "pick object"
 *      o a rapid frame. The rapid frame is displayed at a higher frequency
 *        than the main image (option "-t" value / 5). 
 *    
 *
 * The sequence to generate images is:
 *   - Attach to rtdServer.
 *   - Create shared memory area(s).
 *   - Install signal handlers which cleanup the shared memory area(s).
 *   - Create an object which handles data of type dataType.
 *   - Loop start -
 *   - Generate data for the image.
 *   - Wait until the next shared memory buffer is unlocked.
 *   - Copy data to shared memory and lock it.
 *   - Fill the image event info structure.
 *   - Fill the image information fields with semaphore/shm info.
 *   - Send the image information to rtdServer.
 *   - Delay before next event.
 *   - Cycle shm index and goto Loop
 *
 *
 * For the implementation tRtd uses classes since there are two image frames
 * (main and rapid) and the images can be of any data type supported by RTD.
 * Class tRtdEvt handles one image event cycle and loads Fits images when
 * required.
 * Class tRtdEvtData generates the data type specific classes via tRtdEvtTemplate.icc
 * and creates an data handling object required for the image.
 * The Template contains the code for all data type dependent classes, oa. byte
 * swap simulation (native and non-native) for all data types.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <math.h>

#include "error.h"
#include "rtdSem.h"

#include "tRtd.h"
#include "tRtdEvt.h"

extern char *optarg;

// structures for multibuffering/semaphore convenience routines
static rtdShm shmMain;
static rtdShm shmRapid;

// the "big" data buffer
static char data[MAX_NX * MAX_NY * 4];

/* 
 * Main:
 */
main(int argc, char** argv) 
{
    rtdIMAGE_EVT_HNDL eventHndl;	// image event handle
    struct opts       opt;		// structure holding the options 
    tRtdEvt           *mainObj;		// object which handles the main frame
    tRtdEvt           *rapidObj;	// object which handles the rapid frame

    ZERO(eventHndl);

    parseInput(argc, argv, &opt);	// parse the user's input
    if (opt.verbose && ! opt.useFits)	// print arguments
	usage(&opt);

    set_error_handler(&errprint); 	// error handler (see rtd/tclutil/util/src/error.C)

    
    if (rtdInitImageEvt(opt.rtd_camera, &eventHndl, NULL) != 0) {
	errexit("Could not initialize image event: check if rtdServer is running");
    }

    // clean up shared memory on exit
    signal(SIGINT,  cleanup);
    signal(SIGTERM, cleanup);
    signal(SIGHUP,  cleanup);

    // create the object which handles the main image
    mainObj = new tRtdEvt((char *)"Main", &shmMain, (char *)&data, &opt, 
			  opt.main_width, opt.main_height, 0);

    // create the object which handles the rapid frame
    if (opt.rapid_id != 0 && ! opt.useFits)
	rapidObj = new tRtdEvt((char *)"Rapid", &shmRapid, (char *)&data, &opt, 
			       opt.rapid_width, opt.rapid_height, opt.rapid_id);

    // Loop until tRtdEvt gets aborted by the user
    while ( 1 ) {
        rtdSleep(opt.delay);
 	mainObj->start(&eventHndl);

        if (opt.rapid_id == 0 || opt.useFits)
            continue;
	
        for (int i = 0; i < RAPIDS; i++) {
	    rapidObj->start(&eventHndl);
	    rtdSleep(opt.delay / RAPIDS);
	}

    }  // -- end while( 1 )
    return 0;
}


/*
 * cleanup and exit
 */
void cleanup(int i)
{
    rtdShmDelete(&shmMain);
    rtdShmDelete(&shmRapid);
    exit(i);
}

/*
 * error message handler
 */
void errprint(const char* buf)
{
#ifdef DEBUG
    printf("errprint: %s\n", buf);
#endif
}

/*
 * print error message and exit
 */
void errexit(const char* msg1, const char* msg2)
{
    printf("%s %s\n", msg1, msg2);
    cleanup(1);
}

/*
 * print usage
 */
void usage(opts *opt)
{
    printf("tRtdEvt \n"
	   "\t -v Verbose flag..................(%d)\n"
	   "\t -c Camera name...................(%s)\n"
	   "\t -t Update interval in msecs......(%d)\n"
	   "\t -W Main frame width..............(%d)\n"
	   "\t -H Main frame height.............(%d)\n"
	   "\t -w Rapid frame width.............(%d)\n"
	   "\t -h Rapid frame height............(%d)\n"
	   "\t -x Rapid frame start x...........(%d)\n"
	   "\t -y Rapid frame start y...........(%d)\n"
	   "\t -f Rapid frame Id................(%d)\n"
	   "\t -l Use semaphore locking.........(%d)\n"
	   "\t -b # of shared memory buffers....(%d)\n"
	   "\n"
	   "\t -I FITS image pathname...........(%s)\n"
	   "\t -0 FITS image star center x......(%d)\n"
	   "\t -1 FITS image star center y......(%d)\n"
	   "\t -2 FITS image star bbox..........(%d)\n"
	   "\t -3 FITS image star max. jitter...(%d)\n"
	   "\n"
	   "\t -E Endian flag...................(%d)\n"
	   "\t -D Data type (16 short, -16 ushort, 32 int, -32 float, else byte...(%d)\n",
	   opt->verbose, opt->rtd_camera, opt->delay, opt->main_width, opt->main_height,
	   opt->rapid_width, opt->rapid_height, opt->rapid_x, opt->rapid_y, opt->rapid_id,
	   opt->lock, opt->numShm, opt->fitsFile, opt->starx, opt->stary, opt->starbbox, 
	   opt->starjitter, opt->shmEndian, opt->dataType);
}

/*
 * parse input
 */    
void parseInput(int argc, char** argv, opts *opt)
{
    /*
     * set defaults
     */
    opt->rapid_x = opt->rapid_y = opt->verbose = opt->rapid_id = opt->useFits = 0 ;
    opt->main_width = opt->main_height = opt->rapid_width = opt->rapid_height = 255;
    opt->shmEndian = -1;
    opt->delay = 500;
    opt->rtd_camera = getenv("RTD_CAMERA");
    opt->fitsFile = "../images/ngc1275.fits";
    opt->dataType = 16;
    opt->lock = 1;
    opt->numShm = 2;
    opt->starx = 252; opt->stary = 171; opt->starbbox = 50; opt->starjitter = 3;


    int c;
    while ((c = getopt(argc, argv, "x:y:w:h:W:H:f:v:c:t:b:l:I:E:D:0:1:2:3:")) != -1) {
	switch(c) {
	case 'x': 
	    opt->rapid_x = 	atoi(optarg); break;
	case 'y': 
	    opt->rapid_y = 	atoi(optarg); break;
	case 'W': 
	    opt->main_width = 	atoi(optarg); break;
	case 'H': 
	    opt->main_height = 	atoi(optarg); break;
	case 'w': 
	    opt->rapid_width = 	atoi(optarg); break;
	case 'h': 
	    opt->rapid_height = atoi(optarg); break;
	case 'f': 
	    opt->rapid_id = 	atoi(optarg); break;
	case 'v': 
	    opt->verbose = 	atoi(optarg); break;
	case 'c': 
	    opt->rtd_camera = 	optarg;  break;
	case 't': 
	    opt->delay = 	atoi(optarg); break;
	case 'b': 
	    opt->numShm = 	atoi(optarg); break;
	case 'l': 
	    opt->lock = 	atoi(optarg); break;
	case 'I': 
	    opt->fitsFile = 	optarg; opt->useFits = 1; break;
	case 'E': 
	    opt->shmEndian = 	atoi(optarg); break;
	case 'D': 
	    opt->dataType = 	atoi(optarg); break;
	case '0': 
	    opt->starx = 	atoi(optarg); break;
	case '1': 
	    opt->stary = 	atoi(optarg); break;
	case '2': 
	    opt->starbbox = 	atoi(optarg); break;
	case '3': 
	    opt->starjitter = 	atoi(optarg); break;
	default:
	    usage(opt);
	    exit(1);
	}
    }

    // check arguments
    int dt = opt->dataType;
    if (dt != 8 && dt != 16 && dt != -16 && dt != 32 && dt != -32)
	errexit("wrong data type");
    if (opt->rapid_x < 0 || opt->rapid_y < 0 || opt->rapid_width < 0 || opt->rapid_height < 0 || 
	opt->main_width <= 0 || opt->main_height <= 0 || opt->rapid_id < 0 || opt->delay < 0 || 
	opt->numShm  <= 0)
	errexit("wrong argument");

    // limit ranges
    opt->main_width   = min(opt->main_width,   MAX_NX);
    opt->main_height  = min(opt->main_height,  MAX_NY);
    opt->rapid_width  = min(opt->rapid_width,  MAX_NX);
    opt->rapid_height = min(opt->rapid_height, MAX_NY);
    opt->rapid_x      = min(opt->rapid_x,      MAX_NX);
    opt->rapid_y      = min(opt->rapid_y,      MAX_NY);

    if (! opt->rtd_camera) 
	errexit("please use '... -c camera' or 'setenv RTD_CAMERA ...' first");
}


