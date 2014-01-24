//
//  Declare data type dependent members of XYHistogram class.
//
void extractNativeImage( const DATA_TYPE *image, const int nx, const int ny,
                         const double bscale, const double bzero,
                         const double low, const double high,
                         const int x0, const int y0, const int x1,
                         const int y1, Histogram *histogram );

void extractSwapImage( const DATA_TYPE *image, const int nx, const int ny,
                       const double bscale, const double bzero,
                       const double low, const double high,
                       const int x0, const int y0, const int x1,
                       const int y1, Histogram *histogram );

