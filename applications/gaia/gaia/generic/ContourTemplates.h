//
//  Declare data type dependent members of Contour class.
//
int scanNativeImage( const DATA_TYPE *image, const int nx, const int ny, 
                     const AstPlot *plot, const double cval, const int xlower, 
                     const int ylower, const int xsize, const int ysize, 
                     char *done );

int scanSwapImage( const DATA_TYPE *image, const int nx, const int ny, 
                   const AstPlot *plot, const double cval, const int xlower, 
                   const int ylower, const int xsize, const int ysize, 
                   char *done );
