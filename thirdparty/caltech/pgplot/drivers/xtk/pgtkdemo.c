#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include <tk.h>

#include "tkpgplot.h"
#include "cpgplot.h"

/* Set the default image size */

enum {IMAGE_SIZE=129};

/* Set the number of points plotted per slice */

enum {SLICE_SIZE=100};

/*
 * The demo supports several 2D functions that are displayed in
 * its image window. For each supported function-type there is a 
 * C function of the following declaration, that returns the
 * value of the function at a given x,y position.
 */
#define IMAGE_FN(fn) float (fn)(float x, float y)

/*
 * List the prototypes of the available 2D-function functions.
 */
static IMAGE_FN(sinc_fn);
static IMAGE_FN(gaus_fn);
static IMAGE_FN(ring_fn);
static IMAGE_FN(sin_angle_fn);
static IMAGE_FN(cos_radius_fn);
static IMAGE_FN(star_fn);
/*
 * List the association between image function name and the functions
 * that evaluate them.
 */
static struct {
  char *name;               /* The TCL name for the function */
  IMAGE_FN(*fn);            /* The C function that evaluates the function */
} image_functions[] = {
  {"cos(R)sin(A)",             ring_fn},
  {"sinc(R)",                  sinc_fn},
  {"exp(-R^2/20.0)",           gaus_fn},
  {"sin(A)",                   sin_angle_fn},
  {"cos(R)",                   cos_radius_fn},
  {"(1+sin(6A))exp(-R^2/100)", star_fn}
};

/*
 * Declare a type to hold a single X,Y coordinate.
 */
typedef struct {
  double x, y;       /* World coordinates */
} Vertex;

/*
 * Declare the object type that is used to record the state of a
 * given demo instance command.
 */
typedef struct {
  Tcl_Interp *interp; /* The TCL interpreter of the demo */
  int image_id;       /* The PGPLOT id of the image widget */
  int slice_id;       /* The PGPLOT id of the slice widget */
  float *image;       /* The gray-scale image array */
  float *slice;       /* The slice compilation array */
  float scale;        /* Coversion factor pixels -> coords */
  int image_size;     /* The number of pixels along each side of the image */
  int slice_size;     /* The length of the slice array */
  int xa,xb;          /* Min and max X pixel coordinates */
  int ya,yb;          /* Min and max Y pixel coordinates */
  float datamin;      /* The minimum data value in image[] */
  float datamax;      /* The maximum data value in image[] */
  IMAGE_FN(*fn);      /* The function to be displayed */
  Vertex va;          /* The start of the latest slice line */
  Vertex vb;          /* The end of the latest slice line */
  int have_slice;     /* This true when va and vb contain valid slice limits */
  int monochrome;     /* True if the image colormap only contains two colors */
} Pgdemo;

static Pgdemo *new_Pgdemo(Tcl_Interp *interp, char *caller, char *cmd,
			  char *image_device, char *slice_device);
static Pgdemo *del_Pgdemo(Pgdemo *demo);
static void Pgdemo_DeleteProc(ClientData data);
static int pgdemo_instance_command(ClientData data, Tcl_Interp *interp,
			       int argc, char *argv[]);
static int pgdemo_save_command(Pgdemo *demo, Tcl_Interp *interp, int argc,
			       char *argv[]);
static int pgdemo_function_command(Pgdemo *demo, Tcl_Interp *interp, int argc,
				   char *argv[]);
static int pgdemo_slice_command(Pgdemo *demo, Tcl_Interp *interp, int argc,
				   char *argv[]);
static int pgdemo_redraw_slice_command(Pgdemo *demo, Tcl_Interp *interp,
				       int argc, char *argv[]);
static int pgdemo_recolor_image_command(Pgdemo *demo, Tcl_Interp *interp,
					int argc, char *argv[]);
static int demo_display_fn(Pgdemo *demo, Tcl_Interp *interp, IMAGE_FN(*fn));
static int demo_display_image(Pgdemo *demo, int id);
static int demo_display_slice(Pgdemo *demo, Vertex *va, Vertex *vb);
static void demo_display_help(Pgdemo *demo);
static void demo_display_busy(Pgdemo *demo);

static void Pgdemo_DeleteProc(ClientData data);
static int create_pgdemo(ClientData data, Tcl_Interp *interp, int argc,
			 char *argv[]);


static int valid_demo_script(char *name);
static int Demo_AppInit(Tcl_Interp *interp);

/*
 * Define some color tables.
 */

/*
 * Define single-color ramp functions.
 */
static float grey_l[]  = {0.0,1.0};
static float grey_c[]  = {0.0,1.0};

/*
 * Define a rainbow color table.
 */
static float rain_l[] = {-0.5, 0.0, 0.17, 0.33, 0.50, 0.67, 0.83, 1.0, 1.7};
static float rain_r[] = { 0.0, 0.0,  0.0,  0.0,  0.6,  1.0,  1.0, 1.0, 1.0};
static float rain_g[] = { 0.0, 0.0,  0.0,  1.0,  1.0,  1.0,  0.6, 0.0, 1.0};
static float rain_b[] = { 0.0, 0.3,  0.8,  1.0,  0.3,  0.0,  0.0, 0.0, 1.0};

/*
 * Iraf "heat" color table.
 */
static float heat_l[] = {0.0, 0.2, 0.4, 0.6, 1.0};
static float heat_r[] = {0.0, 0.5, 1.0, 1.0, 1.0};
static float heat_g[] = {0.0, 0.0, 0.5, 1.0, 1.0};
static float heat_b[] = {0.0, 0.0, 0.0, 0.3, 1.0};

/*
 * AIPS tvfiddle discrete rainbow color table.
 */
static float aips_l[] = {0.0, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.4, 0.5,
			 0.5, 0.6, 0.6, 0.7, 0.7, 0.8, 0.8, 0.9, 0.9, 1.0};
static float aips_r[] = {0.0, 0.0, 0.3, 0.3, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0,
			 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0};
static float aips_g[] = {0.0, 0.0, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0, 0.8, 0.8,
			 0.6, 0.6, 1.0, 1.0, 1.0, 1.0, 0.8, 0.8, 0.0, 0.0};
static float aips_b[] = {0.0, 0.0, 0.3, 0.3, 0.7, 0.7, 0.7, 0.7, 0.9, 0.9,
			 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

/*
 * Define a macro that returns the number of elements in a static array.
 */
#ifdef COUNT
#undef COUNT
#endif
#define COUNT(lev) sizeof(lev)/sizeof(lev[0])

/*
 * List the supported color tables.
 */
typedef struct {
  char *name;    /* The name of the color table */
  int n;         /* The number of nodes in the color table */
  float *l;      /* The normalized color-table positions of the n nodes */
  float *r;      /* The n red normalized intensities */
  float *g;      /* The n green normalized intensities */
  float *b;      /* The n blue normalized intensities */
} Cmap;

static Cmap std_cmaps[] = {
  {"grey",    COUNT(grey_l),   grey_l,  grey_c,  grey_c,  grey_c},
  {"rainbow", COUNT(rain_l),   rain_l,  rain_r,  rain_g,  rain_b},
  {"heat",    COUNT(heat_l),   heat_l,  heat_r,  heat_g,  heat_b},
  {"aips",    COUNT(aips_l),   aips_l,  aips_r,  aips_g,  aips_b},
};
static int n_std_cmap = COUNT(std_cmaps);

/*.......................................................................
 * After presenting a warning if the first argument is not the name
 * of the demo Tcl script, main() simply calls the standard Tk_Main()
 * to initialize Tcl/Tk and the demo package.
 * Input:
 *  argc     int    The number of command line arguments.
 *  argv    char*[] The array of command-line argument strings.
 * Output:
 *  return   int    0 - OK.
 *                  1 - Error.
 */
int main(int argc, char *argv[])
{
  char *usage = "Usage: pgtkdemo pgtkdemo.tcl [tk-options].\n";
/*
 * Check whether the first argument names a valid pgtkdemo
 * script file.
 */
  if(argc < 2 || *argv[1] == '-' || !valid_demo_script(argv[1])) {
    fprintf(stderr, usage);
    return 1;
  };
/*
 * Start the application.
 */
  Tk_Main(argc, argv, Demo_AppInit);
  return 0;
}

/*.......................................................................
 * This dummy fortran main allows pgtkdemo to be linked with the
 * f2c-compiled pgplot library.
 */
int MAIN__(void)
{
}

/*.......................................................................
 * This is the application initialization file that is called by Tk_Main().
 */
static int Demo_AppInit(Tcl_Interp *interp)
{
/*
 * Create the standard Tcl and Tk packages, plus the TkPgplot package.
 */
  if(Tcl_Init(interp)    == TCL_ERROR ||
     Tk_Init(interp)     == TCL_ERROR ||
     Tkpgplot_Init(interp) == TCL_ERROR)
    return 1;
/*
 * Create the TCL command used to initialization the demo.
 */
  Tcl_CreateCommand(interp, "create_pgdemo", create_pgdemo,
		    (ClientData) Tk_MainWindow(interp), 0);
  return 0;
}

/*.......................................................................
 * This function provides the TCL command that creates a pgdemo
 * manipulation command. This opens the two given PGPLOT widgets to
 * PGPLOT, establishes a cursor handler and records the state of the
 * demo in a dynamically allocated container.
 *
 * Input:
 *  data      ClientData    The main window cast to ClientData.
 *  interp    Tcl_Interp *  The TCL intrepreter of the demo.
 *  argc             int    The number of command arguments.
 *  argv            char ** The array of 'argc' command arguments.
 *                          argv[0] = "create_pgdemo"
 *                          argv[1] = The name to give the new command.
 *                          argv[2] = The name of the image widget.
 *                          argv[3] = The name of the slice widget.
 * Output:
 *  return           int    TCL_OK    - Success.
 *                          TCL_ERROR - Failure.
 */
static int create_pgdemo(ClientData data, Tcl_Interp *interp, int argc,
			 char *argv[])
{
  Pgdemo *demo;      /* The new widget instance object */
/*
 * Check that the right number of arguments was provided.
 */
  if(argc != 4) {
    Tcl_AppendResult(interp,
	     argv[0], ": Wrong number of arguments - should be \'",
	     argv[0], " new_command_name image_widget slice_widget\'", NULL);
    return TCL_ERROR;
  };
/*
 * Allocate a context object for the command.
 */
  demo = new_Pgdemo(interp, argv[0], argv[1], argv[2], argv[3]);
  if(!demo)
    return TCL_ERROR;
  return TCL_OK;
}

/*.......................................................................
 * Create a new PGPLOT demo instance command and its associated context
 * object.
 *
 * Input:
 *  interp   Tcl_Interp *  The TCL interpreter object.
 *  caller         char *  The name of the calling TCL command.
 *  cmd            char *  The name to give the new demo-instance command.
 *  image_device   char *  The PGPLOT device specification to use to open
 *                         the image-display device.
 *  slice_device   char *  The PGPLOT device specification to use to open
 *                         the slice-display device.
 * Output:
 *  return       Pgdemo *  The new demo object, or NULL on error.
 *                         If NULL is returned then the context of the
 *                         error will have been recorded in the result
 *                         field of the interpreter.
 */
static Pgdemo *new_Pgdemo(Tcl_Interp *interp, char *caller, char *cmd,
			  char *image_device, char *slice_device)
{
  Pgdemo *demo;        /* The new widget object */
  int minind, maxind;  /* The min/max color indexes available for images */
  int i;
/*
 * Allocate the container.
 */
  demo = (Pgdemo *) malloc(sizeof(Pgdemo));
  if(!demo) {
    Tcl_AppendResult(interp, "Insufficient memory to create ", cmd, NULL);
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize the container
 * at least up to the point at which it can safely be passed to
 * del_Pgdemo().
 */
  demo->interp = interp;
  demo->image_id = -1;
  demo->slice_id = -1;
  demo->image = NULL;
  demo->slice = NULL;
  demo->image_size = IMAGE_SIZE;
  demo->slice_size = SLICE_SIZE;
  demo->scale = 40.0f/demo->image_size;
  demo->xa = -(int)demo->image_size/2;
  demo->xb = demo->image_size/2;
  demo->ya = -(int)demo->image_size/2;
  demo->yb = demo->image_size/2;
  demo->fn = sin_angle_fn;
  demo->have_slice = 0;
  demo->monochrome = 0;
/*
 * Attempt to open the image and slice widgets.
 */
  if((demo->image_id = cpgopen(image_device)) <= 0 ||
     (demo->slice_id = cpgopen(slice_device)) <= 0) {
    Tcl_AppendResult(interp, "Unable to open widgets: ", image_device, ", ",
		     slice_device, NULL);
    return del_Pgdemo(demo);
  };
/*
 * Now allocate the 2D image array as a 1D array to be indexed in
 * as a FORTRAN array.
 */
  demo->image = (float *) malloc(sizeof(float) * demo->image_size * demo->image_size);
  if(!demo->image) {
    Tcl_AppendResult(interp, "new_Pgdemo: Insufficient memory.", NULL);
    return del_Pgdemo(demo);
  };
/*
 * Initialize the image array.
 */
  for(i=0; i<demo->image_size*demo->image_size; i++)
    demo->image[i] = 0.0f;
/*
 * Allocate an array to be used when constructing slices through the
 * displayed image.
 */
  demo->slice = (float *) malloc(sizeof(float) * demo->slice_size);
  if(!demo->slice) {
    Tcl_AppendResult(interp, "new_Pgdemo: Insufficient memory.", NULL);
    return del_Pgdemo(demo);
  };
/*
 * Initialize the slice array.
 */
  for(i=0; i<demo->slice_size; i++)
    demo->slice[i] = 0.0f;
/*
 * If there are fewer than 2 colors available for plotting images,
 * mark the image as monochrome so that pggray can be asked to
 * produce a stipple version of the image.
 */
  cpgslct(demo->image_id);
  cpgqcir(&minind, &maxind);
  demo->monochrome = maxind-minind+1 <= 2;
/*
 * Create the instance command.
 */
  Tcl_CreateCommand(interp, cmd, pgdemo_instance_command,
		    (ClientData)demo, Pgdemo_DeleteProc);
/*
 * Return the command name.
 */
  Tcl_AppendResult(interp, cmd, NULL);
  return demo;
}

/*.......................................................................
 * Delete the context of a Pgdemo instance command.
 *
 * Input:
 *  demo     Pgdemo *   The widget to be deleted.
 * Output:
 *  return  Pgdemo *   Always NULL.
 */
static Pgdemo *del_Pgdemo(Pgdemo *demo)
{
  if(demo) {
    demo->interp = NULL;
/*
 * Close the PGPLOT widgets.
 */
    if(demo->image_id > 0) {
      cpgslct(demo->image_id);
      cpgclos();
      demo->image_id = -1;
    };
    if(demo->slice_id > 0) {
      cpgslct(demo->slice_id);
      cpgclos();
      demo->slice_id = -1;
    };
/*
 * Delete the container.
 */
    free(demo);
  };
  return NULL;
}

/*.......................................................................
 * This is a wrapper around del_Pgdemo() suitable to be registered as
 * a DeleteProc callback for Tcl_CreateCommand().
 *
 * Input:
 *  data  ClientData   The (Pgdemo *) object cast to ClientData.
 */
static void Pgdemo_DeleteProc(ClientData data)
{
  (void) del_Pgdemo((Pgdemo *) data);
}

/*.......................................................................
 * This function implements a given Tcl PGPLOT demo instance command.
 *
 * Input:
 *  data      ClientData    The demo context object cast to (ClientData).
 *  interp    Tcl_Interp *  The TCL intrepreter.
 *  argc             int    The number of command arguments.
 *  argv            char ** The array of 'argc' command arguments.
 *                          argv[0] - the name of the demo command.
 *                          argv[1..] - One of:
 *                           save device_spec
 *                           function image_function
 *                           slice x1 y1 x2 y2
 * Output:
 *  return           int    TCL_OK    - Success.
 *                          TCL_ERROR - Failure.
 */
static int pgdemo_instance_command(ClientData data, Tcl_Interp *interp,
				   int argc, char *argv[])
{
  Pgdemo *demo = (Pgdemo *) data;
  char *command;    /* The name of the command */
/*
 * We must have at least one command argument.
 */
  if(argc < 2) {
    Tcl_SetResult(interp, "Wrong number of arguments.", TCL_STATIC);
    return TCL_ERROR;
  };
/*
 * Get the command-name argument.
 */
  command = argv[1];
  if(strcmp(command, "save") == 0)
    return pgdemo_save_command(demo, interp, argc - 2, argv + 2);
  else if(strcmp(command, "function") == 0)
    return pgdemo_function_command(demo, interp, argc - 2, argv + 2);
  else if(strcmp(command, "slice") == 0)
    return pgdemo_slice_command(demo, interp, argc - 2, argv + 2);
  else if(strcmp(command, "redraw_slice") == 0)
    return pgdemo_redraw_slice_command(demo, interp, argc - 2, argv + 2);
  else if(strcmp(command, "recolor_image") == 0)
    return pgdemo_recolor_image_command(demo, interp, argc - 2, argv + 2);
/*
 * Unknown command name.
 */
  Tcl_AppendResult(interp, argv[0], ": Unknown demo command \"",
		   argv[1], "\"", NULL);
  return TCL_ERROR;
}

/*.......................................................................
 * Implement the demo "save" command. This takes a PGPLOT device
 * specification as its argument.
 *
 * Input:
 *  demo       Pgdemo *   The demo being serviced.
 *  interp Tcl_Interp *   The TCL interpreter of the demo.
 *  argc          int     The number of TCL arguments in argv[].
 *  argv         char **  An array of 'argc' TCL arguments.
 * Output:
 *  return        int     TCL_OK    - Normal completion.
 *                        TCL_ERROR - The interpreter result will contain
 *                                    the error message.
 */
static int pgdemo_save_command(Pgdemo *demo, Tcl_Interp *interp, int argc,
			       char *argv[])
{
  char *device;  /* The PGPLOT device specification */
  int device_id; /* The PGPLOT id of the new device */
/*
 * There should only be a single argument.
 */
  if(argc != 1) {
    Tcl_AppendResult(interp, "Missing PGPLOT device specification.\n",
		     NULL);
    return TCL_ERROR;
  };
/*
 * Get the device specification.
 */
  device = argv[0];
/*
 * Open the new PGPLOT device.
 */
  device_id = cpgopen(device);
/*
 * If the device was successfully opened, plot the current image
 * within it and close the device.
 */
  if(device_id > 0) {
    demo_display_image(demo, device_id);
    cpgclos();
  } else {
    Tcl_AppendResult(interp, "cpgopen(\"", device, "\") failed.", NULL);
    return TCL_ERROR;
  };
  return TCL_OK;
}

/*.......................................................................
 * Implement the demo "function" command. This takes one of a set of
 * supported function-designations and displays it in the image window.
 *
 * Input:
 *  demo       Pgdemo *   The demo being serviced.
 *  interp Tcl_Interp *   The TCL interpreter of the demo.
 *  argc          int     The number of TCL arguments in argv[].
 *  argv         char **  An array of 'argc' TCL arguments.
 *                        argv[0] - A function designation chosen from:
 *                                   "cos(R)sin(A)"
 *                                   "sinc(R)"
 *                                   "exp(-R^2/20.0)"
 *                                   "sin(A)"
 *                                   "cos(A)"
 *                                   "(1+sin(6A))exp(-R^2/100)"
 * Output:
 *  return        int     TCL_OK    - Normal completion.
 *                        TCL_ERROR - The interpreter result will contain
 *                                    the error message.
 */
static int pgdemo_function_command(Pgdemo *demo, Tcl_Interp *interp, int argc,
				   char *argv[])
{
  char *function; /* The name of the display function */
  int i;
/*
 * There should only be a single argument.
 */
  if(argc != 1) {
    Tcl_AppendResult(interp, "Missing image function name.\n",
		     NULL);
    return TCL_ERROR;
  };
/*
 * Get the function specification.
 */
  function = argv[0];
/*
 * Look up the function in the table that associates function names
 * with the C functions that implement them.
 */
  for(i=0; i<sizeof(image_functions)/sizeof(image_functions[0]); i++) {
    if(strcmp(image_functions[i].name, function) == 0)
      return demo_display_fn(demo, interp, image_functions[i].fn);
  };
  Tcl_AppendResult(interp, "Unknown function name \"", function, "\"",
		   NULL);
  return TCL_ERROR;
}

/*.......................................................................
 * Implement the demo "slice" command. This takes two pairs of image
 * world coordinates and plots a 1D representation of the currently
 * displayed function in the slice window.
 *
 * Input:
 *  demo       Pgdemo *   The demo being serviced.
 *  interp Tcl_Interp *   The TCL interpreter of the demo.
 *  argc          int     The number of TCL arguments in argv[].
 *  argv         char **  An array of 'argc' TCL arguments. There
 *                        should be 4 arguments, "x1 y1 x2 y2" where
 *                        x1,y1 and x2,y2 are the two end points of the
 *                        slice line.
 *                        
 * Output:
 *  return        int     TCL_OK    - Normal completion.
 *                        TCL_ERROR - The interpreter result will contain
 *                                    the error message.
 */
static int pgdemo_slice_command(Pgdemo *demo, Tcl_Interp *interp, int argc,
				char *argv[])
{
  Vertex va;  /* The coordinates of one end of the slice */
  Vertex vb;  /* The coordinates of the other end of the slice */
/*
 * There should be four arguments.
 */
  if(argc != 4) {
    Tcl_AppendResult(interp,
		     "Wrong number of arguments to the slice command.\n",
		     "Should be x1 y1 x2 y2.", NULL);
    return TCL_ERROR;
  };
/*
 * Read the four coordinate values.
 */
  if(Tcl_GetDouble(interp, argv[0], &va.x) == TCL_ERROR ||
     Tcl_GetDouble(interp, argv[1], &va.y) == TCL_ERROR ||
     Tcl_GetDouble(interp, argv[2], &vb.x) == TCL_ERROR ||
     Tcl_GetDouble(interp, argv[3], &vb.y) == TCL_ERROR)
    return TCL_ERROR;
/*
 * Record the slice vertices so that the slice can be redrawn
 * when the widget is resized.
 */
  demo->va = va;
  demo->vb = vb;
  demo->have_slice = 1;
/*
 * Plot the new slice.
 */
  return demo_display_slice(demo, &va, &vb);
}

/*.......................................................................
 * Implement the demo "redraw_slice" command.
 *
 * Input:
 *  demo       Pgdemo *   The demo being serviced.
 *  interp Tcl_Interp *   The TCL interpreter of the demo.
 *  argc          int     The number of TCL arguments in argv[].
 *  argv         char **  An array of 'argc' TCL arguments. No arguments
 *                        are expected.
 * Output:
 *  return        int     TCL_OK    - Normal completion.
 *                        TCL_ERROR - The interpreter result will contain
 *                                    the error message.
 */
static int pgdemo_redraw_slice_command(Pgdemo *demo, Tcl_Interp *interp,
				       int argc, char *argv[])
{
  if(argc > 0) {
    Tcl_AppendResult(interp, "'pgdemo redraw_slice' takes no arguments.", NULL);
    return TCL_ERROR;
  };
  if(demo->have_slice)
    demo_display_slice(demo, &demo->va, &demo->vb);
  else
    demo_display_help(demo);
  return TCL_OK;
}

/*.......................................................................
 * Implement the demo "recolor_image" command. This takes one of a set of
 * supported color-table names and redisplays the current image with the
 * specified color table.
 *
 * Input:
 *  demo       Pgdemo *   The demo being serviced.
 *  interp Tcl_Interp *   The TCL interpreter of the demo.
 *  argc          int     The number of TCL arguments in argv[].
 *  argv         char **  An array of 'argc' TCL arguments.
 *                        argv[0] - A color table name chosen from:
 *                                   "aips"    -  AIPS tvfiddle color table.
 *                                   "blue"    -  A blue color table.
 *                                   "green"   -  A green color table.
 *                                   "grey"    -  A grey-scale color table.
 *                                   "heat"    -  The IRAF "heat" color table.
 *                                   "rainbow" -  A red color table.
 *                                   "red"     -  A red color table.
 * Output:
 *  return        int     TCL_OK    - Normal completion.
 *                        TCL_ERROR - The interpreter result will contain
 *                                    the error message.
 */
static int pgdemo_recolor_image_command(Pgdemo *demo, Tcl_Interp *interp,
					int argc, char *argv[])
{
  char *name;   /* The name of the desired color table */
  int i;
/*
 * There should only be a single argument.
 */
  if(argc != 1) {
    Tcl_AppendResult(interp, "Missing color-table name.\n", NULL);
    return TCL_ERROR;
  };
/*
 * Get the color-table name.
 */
  name = argv[0];
/*
 * Look up the name in our list of supported color tables.
 */
  for(i=0; i<n_std_cmap; i++) {
    Cmap *cmap = std_cmaps + i;
/*
 * If the color table is found, install it and return.
 */
    if(strcmp(cmap->name, name) == 0) {
      cpgslct(demo->image_id);
      cpgctab(cmap->l, cmap->r, cmap->g, cmap->b, cmap->n, 1.0, 0.5);
      return TCL_OK;
    };
  };
  Tcl_AppendResult(interp, "Unknown color map name \"", name, "\"", NULL);
  return TCL_ERROR;
}

/*.......................................................................
 * A sinc(radius) function.
 *
 * Input:
 *  x,y     float   The coordinates to evaluate the function at.
 * Output:
 *  return  float   The function value at the specified coordinates.
 */
static IMAGE_FN(sinc_fn)
{
  const float tiny = 1.0e-6f;
  float radius = sqrt(x*x + y*y);
  return (fabs(radius) < tiny) ? 1.0f : sin(radius)/radius;
}

/*.......................................................................
 * A exp(-(x^2+y^2)/20) function.
 *
 * Input:
 *  x,y     float   The coordinates to evaluate the function at.
 * Output:
 *  return  float   The function value at the specified coordinates.
 */
static IMAGE_FN(gaus_fn)
{
  return exp(-((x*x)+(y*y))/20.0f);
}

/*.......................................................................
 * A cos(radius)*sin(angle) function.
 *
 * Input:
 *  x,y     float   The coordinates to evaluate the function at.
 * Output:
 *  return  float   The function value at the specified coordinates.
 */
static IMAGE_FN(ring_fn)
{
  return cos(sqrt(x*x + y*y)) * sin(x==0.0f && y==0.0f ? 0.0f : atan2(x,y));
}

/*.......................................................................
 * A sin(angle) function.
 *
 * Input:
 *  x,y     float   The coordinates to evaluate the function at.
 * Output:
 *  return  float   The function value at the specified coordinates.
 */
static IMAGE_FN(sin_angle_fn)
{
  return sin(x==0.0f && y==0.0f ? 0.0f : atan2(x,y));
}

/*.......................................................................
 * A cos(radius) function.
 *
 * Input:
 *  x,y     float   The coordinates to evaluate the function at.
 * Output:
 *  return  float   The function value at the specified coordinates.
 */
static IMAGE_FN(cos_radius_fn)
{
  return cos(sqrt(x*x + y*y));
}

/*.......................................................................
 * A (1+sin(6*angle))*exp(-radius^2 / 100)function.
 *
 * Input:
 *  x,y     float   The coordinates to evaluate the function at.
 * Output:
 *  return  float   The function value at the specified coordinates.
 */
static IMAGE_FN(star_fn)
{
  return (1.0 + sin(x==0.0f && y==0.0f ? 0.0f : 6.0*atan2(x,y)))
    * exp(-((x*x)+(y*y))/100.0f);
}

/*.......................................................................
 * Display a new function in the image window.
 *
 * Input:
 *  demo       Pgdemo *   The demo instance object.
 *  interp Tcl_Interp *   The TCL interpreter of the demo.
 *  fn       IMAGE_FN(*)  The function to be displayed.
 * Output:
 *  return        int     TCL_OK    - Normal completion.
 *                        TCL_ERROR - The interpreter result will contain
 *                                    the error message.
 */
static int demo_display_fn(Pgdemo *demo, Tcl_Interp *interp, IMAGE_FN(*fn))
{
  int ix, iy;  /* The pixel coordinates being assigned */
  float vmin;  /* The minimum pixel value in the image */
  float vmax;  /* The maximum pixel value in the image */
  float *pixel;/* A pointer to pixel (ix,iy) in demo->image */
/*
 * Check arguments.
 */
  if(!fn) {
    Tcl_AppendResult(interp, "demo_display_fn: NULL function.", NULL);
    return TCL_ERROR;
  };
/*
 * Install the new function.
 */
  demo->fn = fn;
/*
 * Display a "please wait" message in the slice window.
 */
  demo_display_busy(demo);
/*
 * Fill the image array via the current display function.
 */
  pixel = demo->image;
  vmin = vmax = demo->fn(demo->xa * demo->scale, demo->ya * demo->scale);
  for(iy = demo->ya; iy <= demo->yb; iy++) {
    for(ix = demo->xa; ix <= demo->xb; ix++) {
      float value = demo->fn(ix * demo->scale, iy * demo->scale);
      *pixel++ = value;
      if(value < vmin)
	vmin = value;
      if(value > vmax)
	vmax = value;
    };
  };
/*
 * Record the min and max values of the data array.
 */
  demo->datamin = vmin;
  demo->datamax = vmax;
/*
 * Display the new image.
 */
  demo_display_image(demo, demo->image_id);
/*
 * Display instructions in the slice window.
 */
  demo_display_help(demo);
/*
 * No slice has been selected yet.
 */
  demo->have_slice = 0;
  return TCL_OK;
}

/*.......................................................................
 * Display the current image function in a specified PGPLOT device.
 *
 *
 * Input:
 *  demo  Pgdemo *   The demo instance object.
 *  id       int     The id of the PGPLOT device to display.
 * Output:
 *  return   int     TCL_OK    - Normal completion.
 *                   TCL_ERROR - The interpreter result will contain
 *                               the error message.
 */
static int demo_display_image(Pgdemo *demo, int id)
{
/*
 * Select the specified PGPLOT device and display the image array.
 */
  cpgslct(id);
  cpgask(0);
  cpgpage();
  cpgsch(1.0f);
  cpgvstd();
  cpgwnad(demo->xa * demo->scale, demo->xb * demo->scale,
	  demo->ya * demo->scale, demo->yb * demo->scale);
  {
    float tr[6];  /* Coordinate definition matrix */
    tr[0] = (demo->xa - 1) * demo->scale;
    tr[1] = demo->scale;
    tr[2] = 0.0f;
    tr[3] = (demo->ya - 1) * demo->scale;
    tr[4] = 0.0f;
    tr[5] = demo->scale;
    if(demo->monochrome) {
      cpggray(demo->image, demo->image_size, demo->image_size,
	      1, demo->image_size, 1, demo->image_size, demo->datamax,
	      demo->datamin, tr);
    } else {
      cpgimag(demo->image, demo->image_size, demo->image_size,
	      1, demo->image_size, 1, demo->image_size, demo->datamin,
	      demo->datamax, tr);
    };
  };
  cpgsci(1);
  cpgbox("BCNST", 0.0f, 0, "BCNST", 0.0f, 0);
  cpglab("X", "Y", "Image display demo");
  return TCL_OK;
}

/*.......................................................................
 * Display a new slice in the slice window.
 *
 * Input:
 *  demo  Pgdemo *   The demo instance object.
 *  va    Vertex *   The vertex of one end of the slice line.
 *  vb    Vertex *   The vertex of the opposite end of the slice line.
 * Output:
 *  return   int     TCL_OK    - Normal completion.
 *                   TCL_ERROR - The interpreter result will contain
 *                               the error message.
 */
static int demo_display_slice(Pgdemo *demo, Vertex *va, Vertex *vb)
{
  float xa;  /* The start X value of the slice */
  float dx;  /* The X-axis world-coordinate width of one slice pixel */
  float ya;  /* The start Y value of the slice */
  float dy;  /* The Y-axis world-coordinate width of one slice pixel */
  float smin;/* The minimum slice value */
  float smax;/* The maximum slice value */
  float slice_length;  /* The world-coordinate length of the slice */
  float ymargin;       /* The Y axis margin within the plot */
  int i;
/*
 * Determine the slice pixel assignments.
 */
  xa = va->x;
  dx = (vb->x - va->x) / demo->slice_size;
  ya = va->y;
  dy = (vb->y - va->y) / demo->slice_size;
/*
 * Make sure that the slice has a finite length by setting a
 * minimum size of one pixel.
 */
  {
    float min_delta = demo->scale / demo->slice_size;
    if(fabs(dx) < min_delta && fabs(dy) < min_delta)
      dx = min_delta;
  };
/*
 * Construct the slice in demo->slice[] and keep a tally of the
 * range of slice values seen.
 */
  for(i=0; i<demo->slice_size; i++) {
    float value = demo->fn(xa + i * dx, ya + i * dy);
    demo->slice[i] = value;
    if(i==0) {
      smin = smax = value;
    } else if(value < smin) {
      smin = value;
    } else if(value > smax) {
      smax = value;
    };
  };
/*
 * Determine the length of the slice.
 */
  {
    float xlen = dx * demo->slice_size;
    float ylen = dy * demo->slice_size;
    slice_length = sqrt(xlen * xlen + ylen * ylen);
  };
/*
 * Determine the extra length to add to the Y axis to prevent the
 * slice plot hitting the top and bottom of the plot.
 */
  ymargin = 0.05 * (demo->datamax - demo->datamin);
/*
 * Set up the slice axes.
 */
  cpgslct(demo->slice_id);
  cpgask(0);
  cpgpage();
  cpgbbuf();
  cpgsch(2.0f);
  cpgvstd();
  cpgswin(0.0f, slice_length, demo->datamin - ymargin, demo->datamax + ymargin);
  cpgbox("BCNST", 0.0f, 0, "BCNST", 0.0f, 0);
  cpglab("Radius", "Image value", "A 1D slice through the image");
/*
 * Draw the slice.
 */
  for(i=0; i<demo->slice_size; i++) {
    if(i==0)
      cpgmove(0.0f, demo->slice[0]);
    else
      cpgdraw(slice_length * (float)i / (float)demo->slice_size, demo->slice[i]);
  };
  cpgebuf();
  return TCL_OK;
}

/*.......................................................................
 * Display usage instructions in the slice window.
 *
 * Input:
 *  demo     Pgdemo *   The demo instance object.
 */
static void demo_display_help(Pgdemo *demo)
{
/*
 * Clear the slice plot and replace it with instructional text.
 */
  cpgslct(demo->slice_id);
  cpgask(0);
  cpgpage();
  cpgsch(3.0f);
  cpgsvp(0.0, 1.0, 0.0, 1.0);
  cpgswin(0.0, 1.0, 0.0, 1.0);
  cpgmtxt("T", -2.0, 0.5, 0.5, "See the help menu for instructions.");
}

/*.......................................................................
 * Display a "Please wait" message in the slice window.
 *
 * Input:
 *  demo     Pgdemo *   The demo instance object.
 */
static void demo_display_busy(Pgdemo *demo)
{
/*
 * Clear the slice plot and replace it with instructional text.
 */
  cpgslct(demo->slice_id);
  cpgask(0);
  cpgpage();
  cpgsch(3.5f);
  cpgsvp(0.0, 1.0, 0.0, 1.0);
  cpgswin(0.0, 1.0, 0.0, 1.0);
  cpgmtxt("T", -2.0, 0.5, 0.5, "Please wait.");
}

/*.......................................................................
 * Check that the specified command-line argument names a pgtkdemo
 * script file. A pgtkdemo script file is defined as being a readable
 * text file that contains the string "#!pgtkdemo.tcl" at its start.
 *
 * Input:
 *  name    char *   The command-line argument to be checked.
 * Output:
 *  return   int     0 - Not valid.
 *                   1 - Valid.
 */
static int valid_demo_script(char *name)
{
#define REQUIRED_HEADER "#!pgtkdemo"
  char header[sizeof(REQUIRED_HEADER)];
/*
 * Attempt to open the file for reading.
 */
  FILE *fp = fopen(name, "r");
  if(!fp) {
    fprintf(stderr, "Unable to open file: %s\n", name);
    return 0;
  };
/*
 * Read the first line and compare it to the required header.
 */
  if(fgets(header, sizeof(header), fp) == NULL ||
     strcmp(header, REQUIRED_HEADER)!=0 || getc(fp) != '\n') {
    fprintf(stderr, "File '%s' is not a pgtkdemo Tcl script.\n", name);
    fclose(fp);
    return 0;
  };
  fclose(fp);
  return 1;
}
