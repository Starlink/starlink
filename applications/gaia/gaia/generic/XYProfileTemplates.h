//
//  Declare data type dependent members of XYProfile class.
//
void extractNativeImage( const DATA_TYPE *image, const int nx, const int ny,
                         const double bscale, const double bzero,
                         const int x0, const int y0, const int x1,
                         const int y1, double *xCoords, double *xVector,
                         double *yCoords, double *yVector, int numValues[2] );

void extractSwapImage( const DATA_TYPE *image, const int nx, const int ny,
                       const double bscale, const double bzero,
                       const int x0, const int y0, const int x1,
                       const int y1, double *xCoords, double *xVector,
                       double *yCoords, double *yVector, int numValues[2] );

