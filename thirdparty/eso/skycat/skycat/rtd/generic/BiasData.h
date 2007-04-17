#ifndef _BiasData_h_
#define _BiasData_h_
/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: BiasData.h,v 1.1.1.1 2006/01/12 16:38:53 abrighto Exp $" 
 *
 * BiasData.h - class definitions for managing bias subtraction
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * P. Biereichel   22/03/99  Created
 */

#include <sys/types.h>     
#include <sys/stat.h>
#include "ImageData.h"
#include "ImageIO.h"
#include "Fits_IO.h"

// max. number of bias frames
#define MAXBIAS 5
#define BIASNAME "Bias"

class BiasData
{
private:
    ImageData* biasImage_;           // current bias image
    int idxBias_;                    // index to biasImages_[]
    ImageData* biasImages_[MAXBIAS];
    biasINFO biasinfo_;
    char files_[MAXBIAS][1024];
 
public:
    // constructor, destructor
    BiasData();
    ~BiasData();

    int    on();
    int    off();
    void   clear(int nr);
    int    status();
    char*  file(int nr);
    int    file(char *file, int nr);
    int    copy(ImageData* image, char *filename, int nr);
    int    select(int nr);
    int    select();

    ImageData* image()    {return biasImage_;}
    biasINFO*  biasInfo() {return &biasinfo_;}

protected:
};

#endif /* _BiasData_h_ */




