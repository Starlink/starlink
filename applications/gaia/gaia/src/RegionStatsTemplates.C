//+
//  Define members of RegionStats.C that are data type dependent.
//
//  The data type used by these overloaded members is controlled by
//  the macro "DATA_TYPE". Define this and #include this file as many
//  times as required for the data types that you want to support.
//
//
//  This code is really Min Tan's "ImageTemplates::Statistic_box"
//  extracted from RTD.
//-

//
//   Calculate stats for a sub region of image data.
//   Native format image data version.
//
//   Arguments:
//      image = pointer to the image data.
//      nx, ny = size of image data.
//      bscale, bzero = image data scale and zero point (FITS only).
//      x0, y0, x1, y1 = array indices of region to process.
//
void RegionStats::calcNative( const DATA_TYPE *image, const int nx,
                              const int ny, const double bscale,
                              const double bzero, const int x0,
                              const int y0, const int x1, const int y1 )
{
    double stotal = 0.0;
    double value;

    max_ = -DBL_MAX;
    mean_ = -DBL_MAX;
    min_ = DBL_MAX;
    pixels_ = 0;
    std_ = -DBL_MAX;
    total_ = 0.0;

    for ( int iy = y0; iy <= y1; iy++ ) {
        for ( int ix = x0; ix <= x1; ix++ ) {

            //  Skip bad value pixels.
            if ( ! badpix( image, nx, ix, iy ) ) {
                value = arrayVal( image, nx, ix, iy );
                if ( value > max_ ) {
                    max_ = value;
                }
                if ( value < min_ ) {
                    min_ = value;
                }
                total_ = total_ + value;
                stotal = stotal + ( value * value );
                pixels_++;
            }
        }
    }

    //  Calculate the required statistics.
    if ( pixels_ > 0 ) {
        mean_ = total_ / pixels_;
        if ( pixels_ > 1 ) {
            std_ = stotal - ( total_ * total_ / pixels_ );
            std_ = sqrt( std_ / ( pixels_ - 1 ) );
        }
        else {
            //  Not defined for one pixel.
            std_ = -DBL_MAX;
        }
    }
}

//
//   Calculate stats for a sub region of image data.
//   Swapped format image data version.
//
//   Arguments:
//      image = pointer to the image data.
//      nx, ny = size of image data.
//      bscale, bzero = image data scale and zero point (FITS only).
//      x0, y0, x1, y1 = array indices of region to process.
//
void RegionStats::calcSwap( const DATA_TYPE *image, const int nx,
                            const int ny, const double bscale,
                            const double bzero, const int x0,
                            const int y0, const int x1, const int y1 )
{
    double stotal = 0.0;
    double value;

    max_ = -DBL_MAX;
    mean_ = -DBL_MAX;
    min_ = DBL_MAX;
    pixels_ = 0;
    std_ = -DBL_MAX;
    total_ = 0.0;

    for ( int iy = y0; iy <= y1; iy++ ) {
        for ( int ix = x0; ix <= x1; ix++ ) {

            //  Skip bad value pixels.
            if ( ! swapBadpix( image, nx, ix, iy ) ) {
                value = swapArrayVal( image, nx, ix, iy );
                if ( value > max_ ) {
                    max_ = value;
                }
                if ( value < min_ ) {
                    min_ = value;
                }
                total_ = total_ + value;
                stotal = stotal + ( value * value );
                pixels_++;
            }
        }
    }

    //  Calculate the required statistics.
    if ( pixels_ > 0 ) {
        mean_ = total_ / pixels_;
        if ( pixels_ > 1 ) {
            std_ = stotal - ( total_ * total_ / pixels_ );
            std_ = sqrt( std_ / ( pixels_ - 1 ) );
        }
        else {
            //  Not defined for one pixel.
            std_ = -DBL_MAX;
        }
    }
}

