/*
 * E.S.O. - VLT project 
 * $Id: tRtdEvt.C,v 1.1.1.1 2006/01/12 16:38:02 abrighto Exp $
 *
 * tRtdEvt.C - test RTD real-time updates by sending image and rapid frame
 *          
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * pbiereic        05/02/03   Created
 */

/************************************************************************
 *
 *  DESCRIPTION
 *    tRtdEvt is class for generating image events (RTD main
 *    or rapid frame)
 *
 */

#include <string.h>
#include <math.h>
#include "tRtdEvt.h"

/*
 * constructor
 */
tRtdEvt::tRtdEvt(char *id, rtdShm *rtdShm, char *data, opts *opt, int width, int height, int frameId)
{
    id_         = id;
    rtdShm_     = rtdShm;
    data_       = data;
    opt_        = opt;
    width_      = width;
    height_     = height;
    frameId_    = frameId;
    count_      = COUNT_INC+1;
    countM_     = 0;
    countMSign_ = 1;
    index_      = 0;

    dataType_  = opt->dataType;
    shmEndian_ = opt->shmEndian;
    int nbytes;

    // check if Fits file is specified
    if (opt->useFits) {
	fits_ = FitsIO::read(opt->fitsFile);
	if (fits_ == NULL)
	    errexit("cannot open file");
	shmEndian_ = 0;

	// get parameters from image
	dataType_ = fits_->bitpix();
	width_    = fits_->width();
	height_   = fits_->height();

	nbytes = width_ * height_ * abs(dataType_ / 8);
	if (nbytes > MAX_NX * MAX_NY * 4)
	    errexit("not enough memory allocated");

	if (width_ < 3 ||  height_ < 3)
	    errexit("image too small (HDU?)");

	memcpy(data_, (char *)fits_->data().ptr(), nbytes);

	delete fits_;
    }

    // create main shared memory area
    if (rtdShmCreate(opt->numShm, rtdShm, width_, height_, dataType_) == -1)
	errexit("error creating shared memory");

    // create an object of type dataType
    dataObj_ = DataObj_.makeImage(width_, height_, rtdShm, dataType_, shmEndian_);

    if (opt->useFits)
	dataObj_->initArea(data_, opt->starx - opt->starbbox/2, opt->stary - opt->starbbox/2,
			   opt->starbbox, width_, height_);
}

tRtdEvt::~tRtdEvt() {};

void tRtdEvt::start(rtdIMAGE_EVT_HNDL *eventHndl)
{
    if (! opt_->useFits) {
	dataObj_->genImage(data_, width_, height_, count_);
    }
    else {
	dataObj_->jitterArea(opt_->starjitter);
    }

    while (rtdShmFill(index_, data_, rtdShm_, 0) == -1)
	rtdSleep((int)LOCK_DELAY);

    int cnt = count_ + countMSign_ * COUNT_INC;
    if (cnt >= width_ || cnt <= COUNT_INC)
	countMSign_ = -countMSign_;
    count_ = count_ + countMSign_ * COUNT_INC;

    fillImageInfo();

    if (rtdShmStruct(index_, &imageInfo_, rtdShm_) == -1)
	errexit("rtdShmStruct failed");

    if ( ! opt_->lock)
	rtdSemReset(imageInfo_.semId, index_);

    printImageInfo(++countM_);

    if (rtdSendImageInfo(eventHndl, &imageInfo_, NULL) == RTD_ERROR)
	errexit("rtdSendImageInfo failed");

    if (opt_->useFits)
	dataObj_->restoreArea();

    index_ = (index_ + 1) % opt_->numShm;
}

/*
 * print image event info
 */    
void tRtdEvt::printImageInfo(int count)
{
    if (! opt_->verbose)
	return;

    printf("update #%d %s frame: frameId=%d, %dx%d+%d+%d, semId=%d, buffer %d(%d)\n", 
	   count, id_, imageInfo_.frameId,
	   imageInfo_.xPixels, imageInfo_.yPixels, imageInfo_.frameX, imageInfo_.frameY,
	   imageInfo_.semId, imageInfo_.shmNum + 1, opt_->numShm);
}

/*
 * fill image event info structure
 */    
void tRtdEvt::fillImageInfo()
{
    // clear info in rtdIMAGE_INFO
    ZERO(imageInfo_);

    imageInfo_.dataType  = dataType_;
    imageInfo_.frameX    = 0;
    imageInfo_.frameY    = 0;
    imageInfo_.xPixels   = width_;
    imageInfo_.yPixels   = height_;
    imageInfo_.frameId   = frameId_;
    imageInfo_.shmEndian = shmEndian_;

    // add some dummy world coordinates for testing
    imageInfo_.ra        = 49.951;
    imageInfo_.dec       =  41.5117;
    imageInfo_.secpix    = 1.70127;
    imageInfo_.xrefpix   = width_/2.0;
    imageInfo_.yrefpix   = height_/2.0 ;
    imageInfo_.rotate    = double(count_ % 360);
    imageInfo_.equinox   = 2000;
    imageInfo_.epoch     = 1957.97;
    strcpy(imageInfo_.proj, "-TAN");

#ifdef DEBUG
    // add some dummy detector information
    if (frameId == 0) {
	imageInfo_.startX    = count_;
	imageInfo_.startY    = count_;
	imageInfo_.binningX  = 2;
	imageInfo_.binningY  = 2;
	imageInfo_.lowCut    = 0;
	imageInfo_.highCut   = 100;
    }
#endif
}










