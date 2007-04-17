#ifndef tRtdEvt_h
#define tRtdEvt_h
/*
 * E.S.O. - VLT project 
 * "@(#) $Id: tRtdEvt.h,v 1.1.1.1 2006/01/12 16:38:02 abrighto Exp $" 
 *
 * tRtdEvt.h - definitions for tRtdEvt
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * pbiereic        05/02/03  Created
 */

#include "Fits_IO.h"
#include "rtdSem.h"
#include "tRtd.h"
#include "rtdSem.h"
#include "rtdImageEvent.h"
#include "tRtdEvtData.h"

#define LOCK_DELAY 50	// delay when all shm buffers are locked (in msec)
#define RAPIDS 5	// number of rapid frames displayed before main image

#define ZERO(x) memset((void *)(&x), '\0', sizeof(x))

class tRtdEvt {

public:
    // public member functions
    tRtdEvt(char *id, rtdShm *rtdShm, char *data, opts *opt, int width, int height, int frameId);
    ~tRtdEvt();
    void start(rtdIMAGE_EVT_HNDL *eventHndl);

private:
    char *id_;			// id, e.g "Main" or "rapid"
    rtdShm *rtdShm_;		// rtdShm struct
    char *data_;		// ptr to static ("big") data buffer
    opts *opt_;			// tRtd options struct
    int width_;			// image width
    int height_;		// image height
    int frameId_;		// frame Id (eg. 0=main, 1=rapid)

    int count_;			// counter
    int countM_;		// counter
    int countMSign_;		// counter sign

    void fillImageInfo();
    void printImageInfo(int count);

    rtdIMAGE_INFO imageInfo_;	// image info structure

    tRtdEvtData DataObj_;	// data object
    tRtdEvtData *dataObj_;	// data handling object


    int index_;			// index for shared memory buffer

    FitsIO *fits_;		// FITS I/O object
    int dataType_;		// data type
    int shmEndian_;		// shmEndian
};

#endif /* tRtdEvt_h */






