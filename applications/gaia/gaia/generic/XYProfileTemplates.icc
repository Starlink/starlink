//+
//  Define members of XYProfile.C that are data type dependent.
//
//  The types used by these overloaded members is controlled by the macro
//  "DATA_TYPE".
//
//-

//
//   Detect and draw a single contour. Returns the number of pixel
//   used in the contour. Use unswapped data.
//
void XYProfile::extractNativeImage( const DATA_TYPE *image, const int nx,
                                    const int ny, const double bscale,
                                    const double bzero, const int x0,
                                    const int y0, const int x1,
                                    const int y1, double *xVector,
                                    double *yVector, int numValues[2] )
{
    cout << "Using native values " << endl;
    DATA_TYPE value;
    int i;
    int j;
    int ix;
    int iy;
    int w = x1 - x0;
    int h = y1 - y0;

    /*  Allocate storage for the X and Y sums and counts */
    double *xsum = new double[w];
    int *xcount = new int[w];
    double *ysum = new double[h];
    int *ycount = new int[h];
    for ( i = 0; i < w; i++ ) {
        xsum[i] = 0.0;
        xcount[i] = 0;
    }
    for ( i = 0; i < h; i++ ) {
        ysum[i] = 0.0;
        ycount[i] = 0;
    }

    /*  Scan image forming sums and counts */
    for ( j = y0, iy = 0; j < y1; j++, iy++ ) {
        for ( i = x0, ix = 0; i < x1; i++, ix++ ) {
            if ( ! badpix( image, nx, i, j ) ) {
                value = arrayVal( image, nx, i, j );
                xsum[ix] += value;
                ysum[iy] += value;
                xcount[ix]++;
                ycount[iy]++;
            }
        }
    }

    /*  Initialise vectors back to zero */
    for ( i = 0; i < w*2; i++) {
        xVector[i] = 0.0;
    }
    for ( i = 0; i < h*2; i++ ) {
        yVector[i] = 0.0;
    }

    /*  Now create averages. */
    int n = 0;
    for ( i = 0, ix = 0; i < w; i++, ix += 2 ) {
        if ( xcount[i] > 0 ) {
            xVector[ix] = (double) i + x0;
            xVector[ix+1] = bscale * ( xsum[i] / (double)xcount[i] ) + bzero;
            n++;
        }
    }
    numValues[0] = n;
    n = 0;
    for ( i = 0, iy = 0; i < w; i++, iy += 2 ) {
        if ( ycount[i] > 0 ) {
            yVector[iy] = (double) i + y0;
            yVector[iy+1] = bscale * ( ysum[i] / (double)ycount[i] ) + bzero;
            n++;
        }
    }
    numValues[1] = n;

    /*  Release all locally allocated space */
    delete[] xsum;
    delete[] xcount;
    delete[] ysum;
    delete[] ycount;
}

//
//   Detect and draw a single contour. Returns the number of pixel
//   used in the contour. Use swapped data.
//
void XYProfile::extractSwapImage( const DATA_TYPE *image, const int nx,
                                  const int ny, const double bscale,
                                  const double bzero, const int x0,
                                  const int y0, const int x1,
                                  const int y1, double *xVector,
                                  double *yVector, int numValues[2] )
{
    cout << "Using swapped values " << endl;
    DATA_TYPE value;
    int i;
    int j;
    int ix;
    int iy;

    /*  Allocate storage for the X and Y sums and counts */
    double *xsum = new double[nx];
    int *xcount = new int[nx];
    double *ysum = new double[ny];
    int *ycount = new int[ny];
    for ( i = 0; i < nx; i++ ) {
        xsum[i] = 0.0;
        xcount[i] = 0;
    }
    for ( i = 0; i < ny; i++ ) {
        ysum[i] = 0.0;
        ycount[i] = 0;
    }

    /*  Scan image forming sums and counts */
    for ( j = y0; j < y1; j++ ) {
        for ( i = x0; i < x1; i++ ) {
            if ( ! swapBadpix( image, nx, i, j ) ) {
                value = swapArrayVal( image, nx, i, j );
                xsum[i] += value;
                ysum[j] += value;
                xcount[i]++;
                ycount[j]++;
            }
        }
    }

    /*  Now create averages. */
    int n = 0;
    for ( i = x0, ix = 0; i < x1; i++, ix += 2 ) {
        if ( xcount[i] > 0 ) {
            xVector[ix] = (double) i;
            xVector[ix+1] = bscale * ( xsum[i] / (double)xcount[i] ) + bzero;
            n++;
        }
    }
    numValues[0] = n;
    n = 0;
    for ( i = y0, iy = 0; i < y1; i++, iy += 2 ) {
        if ( ycount[i] > 0 ) {
            yVector[iy] = (double) i;
            yVector[iy+1] = bscale * ( ysum[i] / (double)ycount[i] ) + bzero;
            n++;
        }
    }
    numValues[1] = n;

    /*  Release all locally allocated space */
    delete[] xsum;
    delete[] xcount;
    delete[] ysum;
    delete[] ycount;
}
