/*
 * E.S.O. - VLT project 
 * $Id: tRtdEvtData.C,v 1.1.1.1 2006/01/12 16:38:00 abrighto Exp $
 *
 * tRtdEvtData.C - data class for tRtd
 *          
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * pbiereic        05/02/03   Created
 */

/************************************************************************
 *
 *  DESCRIPTION
 *    class tRtdEvtData contains methods for handling classes of different
 *    data types.
 *
 */

#include "tRtdEvt.h"
#include "tRtdEvtData.h"
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "define.h"

/*
 * -----------------------------------------------------------
 * Include the Template code for each supported data type
 * -----------------------------------------------------------
 */

/*
 * data classes which need byte swap.
 */

#define DATA_TYPE unsigned char
#define CLASS_NAME ByteSubs
#define SWAP(x) x
#include "tRtdEvtTemplate.icc"
#undef DATA_TYPE
#undef CLASS_NAME
#undef SWAP

#define DATA_TYPE short
#define CLASS_NAME ShortSubs
#define SWAP(x) SWAP16(x)
#include "tRtdEvtTemplate.icc"
#undef DATA_TYPE
#undef CLASS_NAME
#undef SWAP

#define DATA_TYPE unsigned short
#define CLASS_NAME UShortSubs
#define SWAP(x) SWAP16(x)
#include "tRtdEvtTemplate.icc"
#undef DATA_TYPE
#undef CLASS_NAME
#undef SWAP

#define DATA_TYPE int
#define CLASS_NAME IntSubs
#define SWAP(x) SWAP32(x)
#include "tRtdEvtTemplate.icc"
#undef DATA_TYPE
#undef CLASS_NAME
#undef SWAP

#define DATA_TYPE float
#define CLASS_NAME FloatSubs
#define SWAP(x) SWAP_FLOAT(x)
#include "tRtdEvtTemplate.icc"
#undef DATA_TYPE
#undef CLASS_NAME
#undef SWAP

/*
 * data classes which do not need byte swap.
 */
#define SWAP(x) x

#define DATA_TYPE short
#define CLASS_NAME NativeShortSubs
#include "tRtdEvtTemplate.icc"
#undef DATA_TYPE
#undef CLASS_NAME

#define DATA_TYPE unsigned short
#define CLASS_NAME NativeUShortSubs
#include "tRtdEvtTemplate.icc"
#undef DATA_TYPE
#undef CLASS_NAME

#define DATA_TYPE int
#define CLASS_NAME NativeIntSubs
#include "tRtdEvtTemplate.icc"
#undef DATA_TYPE
#undef CLASS_NAME

#define DATA_TYPE float
#define CLASS_NAME NativeFloatSubs
#include "tRtdEvtTemplate.icc"
#undef DATA_TYPE
#undef CLASS_NAME

#undef SWAP

/*
 * -----------------------------------------------------------
 * Class tRtdEvtData
 * -----------------------------------------------------------
 */

/*
 * constructor
 */
tRtdEvtData::tRtdEvtData() :
    ref_pixel_(11),
    ref_size_(3)
{};

/*
 * create and initialize the objects which handle data of type dataType
 */
tRtdEvtData* tRtdEvtData::makeImage(int width, int height, rtdShm *shm, int dataType, int shmEndian)
{
    tRtdEvtData *obj;	// data type object
    /*
     * make the image which handles the data of type dataType.
     * BIGENDIAN = 1 for Hp/Sun..., 0 for Linux-PC...
     * shmEndian = 0: produce big endian data, 1: little endian data, -1 native data
     */
    int native = ((BIGENDIAN == ! shmEndian) || (shmEndian == -1));

    if (native) {
	if (dataType == 16)
	    obj = new NativeShortSubs(-10000, 10000);
	else if (dataType == -16)
	    obj = new NativeUShortSubs(0, 30000);
	else if (dataType == 32)
	    obj = new NativeIntSubs(-100000, 100000);
	else if (dataType == -32)
	    obj = new NativeFloatSubs(-1.E7, 1.E7);
	else
	    obj = new ByteSubs(0, 255);
    }
    else {
	if (dataType == 16)
	    obj = new ShortSubs(-10000, 10000);
	else if (dataType == -16)
	    obj = new UShortSubs(0, 30000);
	else if (dataType == 32)
	    obj = new IntSubs(-100000, 100000);
	else if (dataType == -32)
	    obj = new FloatSubs(-1.E7, 1.E7);
	else
	    obj = new ByteSubs(0, 255);
    }
    return obj;
}

/*
 * get a random shift with a size of +/- starjitter
 */
int tRtdEvtData::getRandomShift(int starjitter)
{
    int random = rand();
    double shift, r;
    r = (double)(RAND_MAX / 2.);
    shift = ((random - r) / r) * starjitter;
    return (int)shift;
}





