//
//  Declare data type dependent members of Contour class.
//
int JOIN_STRINGS(scanNativeImage,DATA_FORMAT)
    ( const DATA_TYPE *image, const int nx, const int ny,
      const AstPlot *plot, const double cval, const int xlower,
      const int ylower, const int xsize, const int ysize,
      char *done );

int JOIN_STRINGS(scanSwapImage,DATA_FORMAT)
    ( const DATA_TYPE *image, const int nx, const int ny,
      const AstPlot *plot, const double cval, const int xlower,
      const int ylower, const int xsize, const int ysize,
      char *done );
