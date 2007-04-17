/*
 * E.S.O. - VLT project 
 * $Id: tImageEvent.C,v 1.1.1.1 2006/01/12 16:38:00 abrighto Exp $
 *
 * tImageEvent.C - simulate a moving star within the ngc1275 image
 *
 * tImageEvent is an auxiliary tool to test RtdImagePick with
 * image events. The image data are read from file ../images/ngc1275.fits
 * and a rectangular around a star is shifted randomly while image events
 * are being sent.
 *
 * For using it:
 *  o start rtdServer in background
 *  o start rtd
 *  o attach rtd to camera (defined by RTD_CAMERA)
 *  o pick object at 230, 250
 *  o run tImageEvent and enjoy ...
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * pbiereic        14/12/99   Created
 */

// window size of the rectangular which contains the star
#define WINSIZE 50
// start point of the rectangular which contains the star
#define XS 200
#define YS 230
// max. shift of the rectangular (+/- MAXSHIFT)
#define MAXSHIFT 5

// parameters for ngc1275
#define DFILE   "../images/ngc1275.fits"
#define TYPE    short
#define XSIZE   353
#define YSIZE   353
#define NUMBYTE 2
#define DTYPE   16
#define OFFSET  11520

// update rate in msec
#define DELAY 2000

#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include "rtdSem.h"
#include "rtdImageEvent.h"

extern "C" {
    void *shmat(int shmid, void *shmaddr, int shmflg);
    int shmdt(void *shmaddr);
}


static rtdShm main1;

/*
 * cleanup and exit
 */
static void cleanup(int i=0)
{
    i=1;
    sleep(1);
    rtdShmDelete(&main1);
    exit(1);
}

void errexit(char *msg1, char *msg2)
{
    printf("%s %s\n", msg1, msg2);
    cleanup();
}

void copyArrayToImage(TYPE *array, TYPE *image, int xs, int ys)
{
    TYPE *s1, *s2;
    for (int idx = 0; idx < WINSIZE; idx++) {
	s1 = array + idx * WINSIZE;
	s2 = image + (ys - 1 + idx) * XSIZE + xs - 1;
	memcpy(s2, s1, WINSIZE * NUMBYTE);
    }
}


void copyImageToArray(TYPE *image, TYPE *array, int xs, int ys)
{
    TYPE *s1, *s2;
    for (int idx = 0; idx < WINSIZE; idx++) {
	s1 = array + idx * WINSIZE;
	s2 = image + (ys - 1 + idx) * XSIZE + xs - 1;
	memcpy(s1, s2, WINSIZE * NUMBYTE);
    }
}


/*
 * sleep for some msec.
 */
void msecSleep(int msec)
{
    timeval t;  
    t.tv_sec  = msec / 1000;
    msec     -= t.tv_sec * 1000;
    t.tv_usec = msec * 1000;
    select(0, NULL, NULL, NULL, &t);
}


/*
 * get a random shift with a size of -MAXSHIFT to +MAXSHIFT
 */
int getRandomShift()
{
    int random = rand();
    double shift, r;
    r = (double)(RAND_MAX / 2.);
    shift = ((random - r) / r) * MAXSHIFT;
    return (int)shift;
}

/* 
 * Main:
 */
main(int argc, char** argv) 
{
    rtdIMAGE_EVT_HNDL eventHndl;
    rtdIMAGE_INFO info;

    // name of camera
    char* rtd_camera = getenv("RTD_CAMERA");
    if (! rtd_camera) 
	errexit("please set the environment RTD_CAMERA first", "");

    memset(&info, '\0', sizeof(rtdIMAGE_INFO));
    if (rtdInitImageEvt(rtd_camera, &eventHndl, NULL) != 0) 
	errexit("Could not initialize image event: check if rtdServer is running", "");
    
    // clean up shared memory on exit
    signal(SIGINT,  cleanup);
    signal(SIGTERM, cleanup);
    signal(SIGHUP,  cleanup);

    // create shared memory area
    if (rtdShmCreate(1, &main1, XSIZE, YSIZE, DTYPE) == -1)
        errexit("error creating shared memory", "");

/*
 * fill image event info
 */    
    info.shmId          = main1.shmId[0];
    info.frameId        = 0;
    info.dataType       = DTYPE;
    info.bytePerPixel   = NUMBYTE;
    info.xPixels        = XSIZE;
    info.yPixels        = YSIZE;
     
    // attach to shm
    TYPE *ptrInfo = (TYPE *)shmat(info.shmId, NULL, 0);
    if (ptrInfo <= NULL)
        errexit("Unable to attach to shared memory", "");

    // get data from FITS file
    int fd = open(DFILE, O_RDONLY);
    if (fd < 0)
	errexit("error opening file", DFILE);

    if ((read(fd, ptrInfo, OFFSET)) < 0)
	errexit("error reading file", DFILE);

    if ((read(fd, ptrInfo, XSIZE * YSIZE * NUMBYTE)) < 0)
	errexit("error reading file", DFILE);

    close(fd);

    // allocate buffers for saving and copying data
    int arraySize = WINSIZE * WINSIZE * NUMBYTE;
    TYPE *array  = (TYPE *)malloc(arraySize);
    TYPE *array2 = (TYPE *)malloc(arraySize);

    // save the rectangular around the star into array
    copyImageToArray(ptrInfo, array, XS, YS);

    int xs = XS, ys = YS;
    for (;;) {
	ys = YS + getRandomShift();
	xs = XS + getRandomShift();

	// copy array starting at xs,ys to the area starting at XS, YS
	copyImageToArray(ptrInfo, array2, xs, ys);
	copyArrayToImage(array2, ptrInfo, XS, YS);

	// send image event to rtdServer
	if (rtdSendImageInfo(&eventHndl, &info, NULL) != 0)
	    errexit("error from rtdSendImageInfo", "");

	msecSleep(DELAY);

	// restore the area starting at XS, YS
	copyArrayToImage(array, ptrInfo, XS, YS);
    }  // ... and loop

    exit(0);
}

