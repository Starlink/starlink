// -*-c++-*-
#ifndef _Skycat_h_
#define _Skycat_h_

/*
 * E.S.O. - VLT project / ESO Archive
 * "@(#) $Id: Skycat.h,v 1.7 1999/03/19 20:10:17 abrighto Exp $"
 *
 * Skycat.h - class definitions for class Skycat, which extends the
 *            RtdImage class to add methods for drawing symbols in
 *            an image based in world or image coordinates.
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  06/02/98  Created
 *
 *                 10/03/98  Added optional args to constructor to allow derived
 *                           class to specify its own configuration options.
 * Peter W. Draper 28/04/98  Made plotting methods virtual to allow
 *                           these to be redefined in derived classes.
 */


#include "RtdImage.h"
#include "FitsIO.h"

/*
 * Class Skycat
 * 
 * This class extends the RtdImage class to add some new Tcl subcommands for
 * drawing symbols based on given coordinates and units.
 */
class Skycat : public RtdImage {
private:
     
    // copy constructor: not defined
    Skycat(const Skycat&);
    
protected:
    // these are part of the implementation of the hdu subcommand (see hduCmd())
    int hduCmdHeadings(int argc, char** argv, FitsIO* fits);
    int hduCmdGet(int argc, char** argv, FitsIO* fits);
    int hduCmdCreate(int argc, char** argv, FitsIO* fits);
    int hduCmdDelete(int argc, char** argv, FitsIO* fits);
    int hduCmdList(int argc, char** argv, FitsIO* fits);
    int hduCmdSet(int argc, char** argv, FitsIO* fits);
    int hduCmdType(int argc, char** argv, FitsIO* fits);

    // Write the contents of the current HDU (FITS table) to a catalog file
    // or return it as a Tcl list, if filename is NULL.
    int getHDU(FitsIO* fits, const char* filename, const char* entry);

    // Return the column headings for the current FITS table
    int getHDUHeadings(FitsIO* fits);

public:
    // Create a new skycat image object
    Skycat(Tcl_Interp*, const char* instname, int argc, char** argv, 
	     Tk_ImageMaster master, const char* imageType,
	     Tk_ConfigSpec* specs = (Tk_ConfigSpec*)NULL, 
	     RtdImageOptions* options = (RtdImageOptions*)NULL);
    
    // destructor - free any allocated resources
    virtual ~Skycat() {}
    
    // call a member function by name
    virtual int call(const char* name, int len, int argc, char* argv[]);

    // entry point from tcl to create a image
    static int CreateImage(Tcl_Interp*, char *name, int argc, char **argv, 
		    Tk_ImageType*, Tk_ImageMaster, ClientData*);

    // return a pointer to the Skycat class object for the given tcl rtdimage
    // instance name, or NULL if the name is not an rtdimage.
    static Skycat* getInstance(char* name);


    // Return the canvas coordinates of the 3 points: center, north and east
    virtual int get_compass(double x, double y, const char* xy_units, 
                            double radius, const char* radius_units, 
                            double ratio, double angle,
                            double& cx, double& cy, double& nx, double& ny, 
                            double& ex, double& ey);
   
    // rotate the point x,y around the center point cx,cy by the given angle in deg.
    virtual int rotate_point(double& x, double& y, double cx, double cy, double angle);

    // Write a Tcl canvas command to the given stream to add a label
    int make_label(ostream& os, const char* label, double x, double y, 
		   const char* tags, const char* color,
		   const char* font = "-*-courier-medium-r-*-*-*-120-*-*-*-*-*-*");

    // draw a symbol with the given shape, etc., in the image
    virtual int draw_symbol(const char* shape, 
                            double x, double y, const char* xy_units, 
                            double radius, const char* radius_units, 
                            const char* bg, const char* fg, 
                            const char* symbol_tags, 
                            double ratio = 1., double angle = 0.,
                            const char* label = NULL, const char* label_tags = NULL);
  
    // draw a symbol at the give coordinates, units, etc...
    virtual int draw_circle(double x, double y, const char* xy_units, 
                            double radius, const char* radius_units, 
                            const char* bg, const char* fg, 
                            const char* symbol_tags, 
                            double ratio = 1., double angle = 0.,
                            const char* label = NULL, const char* label_tags = NULL);
  
    virtual int draw_square(double x, double y, const char* xy_units, 
                            double radius, const char* radius_units, 
                            const char* bg, const char* fg, 
                            const char* symbol_tags, 
                            double ratio = 1., double angle = 0.,
                            const char* label = NULL, const char* label_tags = NULL);

    virtual int draw_plus(double x, double y, const char* xy_units, 
                          double radius, const char* radius_units, 
                          const char* bg, const char* fg, 
                          const char* symbol_tags, 
                          double ratio = 1., double angle = 0.,
                          const char* label = NULL, const char* label_tags = NULL);
  
    virtual int draw_cross(double x, double y, const char* xy_units, 
                           double radius, const char* radius_units, 
                           const char* bg, const char* fg, 
                           const char* symbol_tags, 
                           double ratio = 1., double angle = 0.,
                           const char* label = NULL, const char* label_tags = NULL);
  
    virtual int draw_triangle(double x, double y, const char* xy_units, 
                              double radius, const char* radius_units, 
                              const char* bg, const char* fg, 
                              const char* symbol_tags, 
                              double ratio = 1., double angle = 0.,
                              const char* label = NULL, const char* label_tags = NULL);
  
    virtual int draw_diamond(double x, double y, const char* xy_units, 
                             double radius, const char* radius_units, 
                             const char* bg, const char* fg, 
                             const char* symbol_tags, 
                             double ratio = 1., double angle = 0.,
                             const char* label = NULL, const char* label_tags = NULL);
  
    virtual int draw_ellipse(double x, double y, const char* xy_units, 
                             double radius, const char* radius_units, 
                             const char* bg, const char* fg, 
                             const char* symbol_tags, 
                             double ratio = 1., double angle = 0.,
                             const char* label = NULL, const char* label_tags = NULL);

    virtual int draw_compass(double x, double y, const char* xy_units, 
                             double radius, const char* radius_units, 
                             const char* bg, const char* fg, 
                             const char* symbol_tags, 
                             double ratio = 1., double angle = 0.,
                             const char* label = NULL, const char* label_tags = NULL);
  
    virtual int draw_line(double x, double y, const char* xy_units, 
                          double radius, const char* radius_units, 
                          const char* bg, const char* fg, 
                          const char* symbol_tags, 
                          double ratio = 1., double angle = 0.,
                          const char* label = NULL, const char* label_tags = NULL);

    virtual int draw_arrow(double x, double y, const char* xy_units, 
                           double radius, const char* radius_units, 
                           const char* bg, const char* fg, 
                           const char* symbol_tags, 
                           double ratio = 1., double angle = 0.,
                           const char* label = NULL, const char* label_tags = NULL);
  
    // -- skycat image subcommand methods --
    
    int hduCmd(int argc, char* argv[]);
    int symbolCmd(int argc, char* argv[]);

};

#endif /* _Skycat_h_ */
