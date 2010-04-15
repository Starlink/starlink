//
//  Declare data type dependent members of RegionStats class.
//
void calcNative( const DATA_TYPE *image, const int nx, const int ny,
                 const double bscale, const double bzero,
                 const int x0, const int y0, const int x1, const int y1 );

void calcSwap( const DATA_TYPE *image, const int nx, const int ny,
               const double bscale, const double bzero,
               const int x0, const int y0, const int x1, const int y1 );

