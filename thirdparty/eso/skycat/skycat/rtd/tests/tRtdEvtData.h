#ifndef tRtdEvtData_h
#define tRtdEvtData_h
/*
 * E.S.O. - VLT project 
 * "@(#) $Id: tRtdEvtData.h,v 1.1.1.1 2006/01/12 16:38:00 abrighto Exp $" 
 *
 * tRtdEvt.h - definitions for tRtdEvtData
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * pbiereic        05/02/03  Created
 */

#include <math.h>
#include "rtdSem.h"

#define COUNT_INC 3	// count increment after an image was displayed

class tRtdEvtData {

public:
    // public member functions
    tRtdEvtData();

    int getRandomShift(int starjitter);
    tRtdEvtData* makeImage(int width, int height, rtdShm *shm, int dataType, int shmEndian);

    virtual void genRef(char *p, int width, int height, int ref) {};
    virtual void genImage(char *p, int width, int height, int count) {};
    virtual void copyArrayToImage(char *array, char *image, int xs, int ys, int width) {};
    virtual void copyImageToArray(char *image, char *array, int xs, int ys, int width) {};

    virtual void initArea(char *ptr, int xs, int ys, int winsize, int width, int height) {};
    virtual void jitterArea(int starjitter) {};
    virtual void restoreArea() {};

    int ref_pixel() {return ref_pixel_;};	// center of reference pixel
    int ref_size()  {return ref_size_;};	// size of reference area (must be an odd number)

protected:
    int ref_pixel_;
    int ref_size_;
};

#endif /* tRtdEvtData_h */






