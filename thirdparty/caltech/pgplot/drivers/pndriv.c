/*
  This is the PNG (Portable Network Graphics) driver for PGPLOT.
  For more information on the PNG standard, and to get the
  necessary libraries, see http://www.cdrom.com/pub/png/

  This driver is intended to be used in the same ways as one would
  use PGPLOT's  GIF driver, and as such uses many of the same conventions.

  The default plotting dimensions are 850x680, and can be
  manipulated via the PGPLOT_PNG_WIDTH and PGPLOT_PNG_HEIGHT
  environment variables.

  The driver can be opened as many times as the caller likes (i.e., more
  than one device number available). Associated with each device
  is a single filename, and after each page advance the filename is
  modified to have a trailing "_X", where "X" is the current
  page number. This does not apply to the first page output,
  however.

  For compilation, both libpng and zlib must be installed. These
  libraries are Free Software, and can be obtained at the following
  URLs:
        libpng: http://www.cdrom.com/pub/png/
        zlib:   http://www.cdrom.com/pub/infozip/zlib/


  March, 1999
  Pete Ratzlaff <pratzlaff@cfa.harvard.edu>

*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <png.h>

#ifdef VMS
#include <descrip.h>
#include <ssdef.h>
#endif

/* make the driver callable from FORTRAN */
#ifdef PG_PPU
#define PNDRIV pndriv_
#else
#define PNDRIV pndriv
#endif

/*
  Flags passed by grexec(). We may need others in the future. Set in
  drivers.list.
*/
#define TRANS_OFF 1
#define TRANS_ON  2

/* miscellaneous constants */
#define DEFAULT_WIDTH 850
#define DEFAULT_HEIGHT 680
#define NCOLORS 256
#define DEVICE_CAPABILITIES "HNNNNRPNYN"
#define DEFAULT_FILENAME "pgplot.png"

#define boolean unsigned char
#define true 1
#define false 0


#define PNG_IDENT_BASIC "PGPLOT /png" /* used in warning messages */
#define PNG_IDENT_TRANS "PGPLOT /tpng" /* used in warning messages */
static char *png_ident; /* should be set each time pndriv() is entered */

/* use for opcode = 1 */
#define DEVICE_NAME_BASIC "PNG (Portable Network Graphics file)"
#define DEVICE_NAME_TRANS "TPNG (Portable Network Graphics file - transparent background)"

/* simple way of specifiying the current device structure pointer */
#define ACTIVE_DEVICE  (all_devices.devices[all_devices.active])

typedef unsigned char ColorComponent; /* red, green, or blue component of a colortable entry */
typedef unsigned char ColorIndex;  /* index into a color table */

/* taken from the GIF driver */
static ColorComponent base_colors[] = {
  0,   0,   0,
  255, 255, 255,
  255, 0, 0,
  0, 255, 0,
  0, 0, 255,
  0, 255, 255,
  255, 0, 255,
  255, 255, 0,
  255, 128, 0,
  128, 255, 0,
  0, 255, 128,
  0, 128, 255,
  128, 0, 255,
  255, 0, 128,
  85, 85, 85,
  170, 170, 170,
};

/* each new device initially copies its colortable from here */
static ColorComponent default_colortable[NCOLORS * 3];

/* data for a single open device */
typedef struct _DeviceData DeviceData, *DeviceDataPtr;
struct _DeviceData {
  int w, h;
  long npix; /* w*h */
  boolean trans; /* transparent background flag */
  boolean error; /* if true, we can plot no more on this device */
  ColorIndex *pixmap; /* image consisting of array of color indicies */
  int npages; /* running total of plot pages */
  char *filename;
  ColorComponent ctable[NCOLORS * 3];
  ColorIndex cindex; /* current plotting color index */
  int devnum; /* this device's identifier */
};

/* global data holding all devices */
typedef struct _Devices Devices;
struct _Devices {
  DeviceDataPtr *devices;
  int nallocated;
  int active;
};
static Devices all_devices;

/* number of DeviceData structures to allocate at a time */
#define devices_ALLOC_INCREMENT 128

/* copy the default colortable to a newly-opened device's ctable entry */
static void initialize_device_ctable(DeviceData *dev) {
  memcpy(dev->ctable, default_colortable, 3 * NCOLORS * sizeof(ColorComponent));
}

/* use to set the RGB components of a colortable entry */
static void set_color_rep(DeviceData *dev, int index, ColorComponent r, ColorComponent g, ColorComponent b) {
  dev->ctable[index*3+0] = r;
  dev->ctable[index*3+1] = g;
  dev->ctable[index*3+2] = b;
}

static void get_color_rep(DeviceData *dev, int index, ColorComponent *r, ColorComponent *g, ColorComponent *b) {
  *r = dev->ctable[index*3+0];
  *g = dev->ctable[index*3+1];
  *b = dev->ctable[index*3+2];
}

/* If one were to port this driver to a new image format, then this is the
 * only routine that would need to be rewritten.
 */
static void write_image_file(DeviceData *dev) {

  int i;
  char *filename;
  FILE *fp;

  png_structp    png_ptr;
  png_infop      info_ptr;
  png_color      colors[NCOLORS];
  ColorComponent r, g, b;

  if (dev->error == true)
	return;

  /* fill the color table for libpng */
  for (i=0; i<NCOLORS; i++) {
	get_color_rep(dev, i, &r, &g, &b);
	colors[i].red = r;
	colors[i].green = g;
	colors[i].blue = b;
  }

  /*
	For multiple pages, we make a new file for each page.
	We name them sequentially, based on the original filename
	specified.
  */
#define EXTRA_CHARS 16 /* allow 10^15 image files per device */

  filename = malloc(strlen(dev->filename)+EXTRA_CHARS);
  if (!filename) {
	fprintf(stderr,"%s: out of memory, plotting disabled\n", png_ident);
	dev->error = true;
	return;
  }
  strcpy(filename,dev->filename);
  if (strcmp("-",filename) != 0 && dev->npages > 1) {
	sprintf(filename,"%s_%d",dev->filename,dev->npages);
	fprintf(stderr,"%s: writing new file as %s\n",png_ident,filename);
  }

  /* open the file */
  if (strcmp("-",filename) != 0) {
	if (! (fp = fopen(filename,"wb"))) {
	  fprintf(stderr,"%s: could not open file %s for writing, plotting disabled\n",png_ident,filename);
	  dev->error = true;
	  free(filename);
	  return;
	}
  } else {
	fp = stdout;
  }

  png_ptr = png_create_write_struct(
									PNG_LIBPNG_VER_STRING,
									NULL,
									NULL,
									NULL
									);

  info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr) {
	fprintf(stderr,"%s: error in libpng while writing file %s, plotting disabled\n", png_ident, filename);
	png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
	dev->error = true;
	if (fp != stdout)
	  fclose(fp);
	free(filename);
	return;
  }

  if (setjmp(png_jmpbuf(png_ptr))) { /* not really sure what I'm doing here... */
	fprintf(stderr,"%s: error in libpng while writing file %s, plotting disabled\n", png_ident, filename);
	png_destroy_write_struct(&png_ptr,&info_ptr);
	dev->error = true;
	if (fp != stdout)
	  fclose(fp);
	free(filename);
	return;
  }

  png_init_io(png_ptr, fp);

  png_set_IHDR(png_ptr, info_ptr,
			   dev->w, dev->h, 8, 
			   PNG_COLOR_TYPE_PALETTE,
			   PNG_INTERLACE_NONE,
			   PNG_COMPRESSION_TYPE_DEFAULT,
			   PNG_FILTER_TYPE_DEFAULT
			   );
  png_set_PLTE(png_ptr, info_ptr, colors, NCOLORS);

  /* text to go in PNG file */
  {
	png_text text[1];

	text[0].key = "Software";
	text[0].text = "PGPLOT Graphics Subroutine Library";
	text[0].text_length = strlen(text[0].text);
	text[0].compression = PNG_TEXT_COMPRESSION_NONE;

	png_set_text(png_ptr, info_ptr, text, 1);
  };

  /* set bKGD chunk */
  /* not sure I know what image readers will do with this, so it's commented out
  {
	png_color_16 background;
	background.index = 0;
	png_set_bKGD(png_ptr,info_ptr,&background);
  }
  */

  /* set transparency */
  if (dev->trans == true) {
	png_byte i = 0;
	png_set_tRNS(png_ptr, info_ptr, &i, 1, NULL);
  };

  png_write_info(png_ptr, info_ptr);
  for (i=dev->h-1; i>=0; i--)
	png_write_row(png_ptr,&dev->pixmap[i * dev->w]);
  png_write_end(png_ptr, info_ptr);

  png_destroy_write_struct(&png_ptr, &info_ptr);
  if (fp != stdout)
	fclose(fp);
  free(filename);

}

static void swap_coords(int *x1, int *y1, int *x2, int *y2) {
  int tmp;

  tmp = *x1;
  *x1 = *x2;
  *x2 = tmp;

  tmp = *y1;
  *y1 = *y2;
  *y2 = tmp;

}

static void fill_rectangle( DeviceData *dev, int x1, int y1, int x2, int y2, ColorIndex index ) {

  int y;
  int npix = (x2 - x1 + 1); /* number of pixels to fill on a single line */

  if (dev->error == true)
	return;

  /* ensure coords are lower left and upper right */
  if (x2<x1 && y2<y1)
	swap_coords(&x1, &y1, &x2, &y2);
  else if (y2 < y1) {
	y = y1;
	y1 = y2;
	y2 = y;
  }
  else if (x2 < x1) {
	y = x1;
	x1 = x2;
	x2 = y;
	npix = (x2 - x1 + 1);
  }

  for (y=y1; y<=y2; y++)
	memset( &dev->pixmap[ y * dev->w + x1 ], index, npix * sizeof(ColorIndex) );

}

/*
  Begins a new plot page. Allocates memory for the pixmap, which
  should be freed after the page has been written to file
*/
static void start_plot(DeviceData *dev, int w, int h) {
  dev->w = w;
  dev->h = h;
  dev->npix = dev->w * dev->h;
  dev->pixmap = malloc( dev->npix * sizeof(ColorIndex) );
  if (!dev->pixmap) {
	fprintf(stderr,"%s: out of memory, plotting disabled\n",png_ident);
	dev->error = true;
  }
  dev->npages++;
  fill_rectangle(dev, 0, 0, dev->w-1, dev->h-1, 0);
  return;
}

/*
  Called when page is done. Should write file and free
  memory allocated in start_plot() for pixmap.
*/
static void end_plot(DeviceData *dev) {
  if (dev->error == true)
	return;
  write_image_file(dev);
  free(dev->pixmap);
}

static void make_device_active(float devnum) {
  all_devices.active = devnum;
  if (ACTIVE_DEVICE == NULL)
	fprintf(stderr,"%s: one SIGSEGV coming right up! ACTIVE_DEVICE == NULL\n",png_ident);
}

/*
  Used first time through PNDRIV()
*/
static void initialize_default_colortable(void) {
  int i;
  ColorComponent half_colors[] = { 128, 128, 128 };

  memcpy(default_colortable, base_colors, 3 * 16 * sizeof(ColorComponent) );

  for (i=16; i<NCOLORS; i++)
	memcpy( default_colortable + (i * 3), half_colors, 3 * sizeof(ColorComponent) );
}

static void get_default_dimensions( int *width, int *height) {

  char *width_string = NULL;
  char *height_string = NULL;

  if (! (width_string = getenv("PGPLOT_PNG_WIDTH")))
	width_string = "DEFAULT_WIDTH";
  if (! (height_string = getenv("PGPLOT_PNG_HEIGHT")))
	height_string = "DEFAULT_HEIGHT";

  *width = atoi(width_string);
  *height = atoi(height_string);

  if (!(*width > 0) || !(*height>0)) {
	*width = DEFAULT_WIDTH;
	*height = DEFAULT_HEIGHT;
  }

  return;
}

static void draw_line(DeviceData *dev, int x1, int y1, int x2, int y2, ColorIndex index) {

  int x, y;
  float rate;

  if (dev->error == true)
	return;

  if (x1 == x2 || y1 == y2) { /* rate of change calculation below doesn't like this case */
	fill_rectangle(dev,x1,y1,x2,y2,index);
	return;
  }

  if (abs(y2-y1) > abs(x2-x1)) {

	if (y1 > y2)
	  swap_coords(&x1, &y1, &x2, &y2);

	rate = (x2 - x1) / (float)(y2 - y1);

	for (y=y1; y<y2; y++) {
	  x = x1 + (y - y1) * rate;
	  dev->pixmap[ y * dev->w + x ] = index;
	}
  } else {

	if (x1 > x2)
	  swap_coords(&x1, &y1, &x2, &y2);

	rate = (float)(y2 - y1) / (float)(x2 - x1);

	for (x=x1; x<x2; x++) {
	  y = y1 + (x - x1) * rate;
	  dev->pixmap[ y * dev->w + x ] = index;
	}

  }
	 
}

/* set a single pixel's color */
static void fill_pixel(DeviceData *dev, int x, int y, ColorIndex index) {
  if (dev->error == true)
	return;
  dev->pixmap[ y * dev->w + x ] = index;
}


static void initialize_all_devices(void) {

  all_devices.devices = NULL;
  all_devices.nallocated = 0;
  all_devices.active = -1;

}

static void open_new_device(char *file, int length, float *id, float *err, int mode) {

  DeviceDataPtr *tmp;

  int i;
  int devnum = -1;

  /* find an empty slot */
  for (i=0; i<all_devices.nallocated; i++) {
	if (all_devices.devices[i] == NULL) { /* got one */
	  devnum = i;
	  break;
	}
  }
  if (devnum < 0) /* didn't find one */
	devnum = all_devices.nallocated;

  *err = 0.0;

  /* allocate more device slots, if necessary */
  if (devnum >= all_devices.nallocated) {
	tmp = realloc(all_devices.devices, sizeof(DeviceDataPtr) * (all_devices.nallocated + devices_ALLOC_INCREMENT));

	/* didn't get the memory needed */
	if (!tmp) {
	  fprintf(stderr,"%s: out of memory\n", png_ident);
	  return;
	}
	else {
	  all_devices.devices = tmp;
	  for (i=all_devices.nallocated; i<all_devices.nallocated+devices_ALLOC_INCREMENT; i++)
		all_devices.devices[i] = NULL;
	  all_devices.nallocated += devices_ALLOC_INCREMENT;
	}
  }

  if (!(all_devices.devices[devnum] = malloc(sizeof(DeviceData)))) {
	fprintf(stderr,"%s: out of memory\n", png_ident);
	return;
  }

  all_devices.devices[devnum]->filename = malloc(length+1);
  if (! all_devices.devices[devnum]->filename) {
	fprintf(stderr,"%s: out of memory\n",png_ident);
	free(all_devices.devices[devnum]);
	all_devices.devices[devnum] = NULL;
	return;
  }

  make_device_active(devnum);

  ACTIVE_DEVICE->filename[length] = '\0';
  strncpy(ACTIVE_DEVICE->filename,file,length);

  initialize_device_ctable(ACTIVE_DEVICE);
  ACTIVE_DEVICE->devnum = devnum;
  ACTIVE_DEVICE->npages = 0;

  if (mode & TRANS_ON)
	ACTIVE_DEVICE->trans = true;
  else
	ACTIVE_DEVICE->trans = false;

  *id = (float)devnum;
  *err = 1.0;

  return;
}

static void close_device( DeviceData *dev ) {
  int devnum = dev->devnum;

  if (dev->filename)
	free(dev->filename);
  free(all_devices.devices[devnum]);
  all_devices.devices[devnum] = NULL;
  if (all_devices.active == devnum)
	all_devices.active = -1;
}

#ifdef VMS
void pndriv(int *opcode, float *rbuf, int *nbuf, struct dsc$descriptor_s *chrdsc, int *lchr, int *mode) {
  int len = chrdsc->dsc$w_length;
  char *chr = chrdsc->dsc$a_pointer;
#else
void PNDRIV(int *opcode, float *rbuf, int *nbuf, char *chr, int *lchr, int *mode, int len) {
#endif

  static int firsttime = 1;

  /* text used in warning messages */
  if (*mode & TRANS_ON)
	png_ident = PNG_IDENT_TRANS;
  else
	png_ident = PNG_IDENT_BASIC;

  if (firsttime) {
	initialize_default_colortable();
	initialize_all_devices();
	firsttime = 0;
  }

  switch (*opcode) {

	/* Return device name */
  case 1:
	{
	  int i;
	  char *name;
	  if (*mode & TRANS_ON)
		name = DEVICE_NAME_TRANS;
	  else
		name = DEVICE_NAME_BASIC;

	  strncpy(chr,name,len);
	  *lchr = strlen(name);
	  for (i=*lchr; i<len; i++)
		chr[i] = ' ';
	};
	break;

	/* min and max dimensions of plot device, color indicies */
  case 2:
	rbuf[0] = 0.0;
	rbuf[1] = -1.0;
	rbuf[2] = 0.0;
	rbuf[3] = -1.0;
	rbuf[4] = 0.0;
	rbuf[5] = 255.0;
	*nbuf = 6;
	break;

	/* return device scale */
  case 3:
	rbuf[0] = 85.0; /* same as used in GIF drivers */
	rbuf[1] = 85.0;
	rbuf[2] = 1.0;
	*nbuf = 3;
	break;

	/* return device capabilities */
  case 4:
	*lchr = strlen(DEVICE_CAPABILITIES);
	strncpy(chr,DEVICE_CAPABILITIES,*lchr);
	break;

	/* return default device filename */
  case 5:
	*lchr = strlen(DEFAULT_FILENAME);
	strncpy(chr,DEFAULT_FILENAME,*lchr);
	break;

	/* default edge coordinates of view surface */
  case 6:
	{
	  int width, height;

	  get_default_dimensions(&width, &height);

	  rbuf[0] = 0.0;
	  rbuf[1] = width - 1.0;
	  rbuf[2] = 0.0;
	  rbuf[3] = height - 1.0;
	  *nbuf = 4;
	};
	break;

	/* scale factor of obsolete character set */
  case 7:
	rbuf[0] = 1.0;
	*nbuf = 1;
	break;

	/* select active device */
  case 8:
	make_device_active(rbuf[1]);
	break;

	/* open device */
  case 9:
	open_new_device(chr,*lchr,&rbuf[0],&rbuf[1],*mode);
	break;

	/* close device */
  case 10:
	close_device(ACTIVE_DEVICE);
	break;

	/* begin picture */
  case 11:
	start_plot(ACTIVE_DEVICE, (int)rbuf[0] + 1, (int)rbuf[1] + 1);
	break;

	/* draw a line */
  case 12:
	draw_line(
			  ACTIVE_DEVICE,
			  (int)rbuf[0],
			  (int)rbuf[1],
			  (int)rbuf[2],
			  (int)rbuf[3],
			  ACTIVE_DEVICE->cindex
			  );
	break;

	/* fill dot */
  case 13:
	fill_pixel(ACTIVE_DEVICE, (int)rbuf[0], (int)rbuf[1], ACTIVE_DEVICE->cindex);
	break;

	/* end picture */
  case 14:
	end_plot(ACTIVE_DEVICE);
	break;

	/* set current color index */
  case 15:
	ACTIVE_DEVICE->cindex = (ColorIndex)rbuf[0];

	/* flush buffer */
  case 16:
	break;

	/* erase alpha (text) screen */
  case 18:
	break;

	/* set color representation */
  case 21:
	set_color_rep(
				  ACTIVE_DEVICE,
				  (ColorIndex)rbuf[0],
				  (ColorComponent)(rbuf[1]*255.0),
				  (ColorComponent)(rbuf[2]*255.0),
				  (ColorComponent)(rbuf[3]*255.0)
				  );
	break;

	/* escape function */
  case 23:
	break;

	/* rectangle fill */
  case 24:
	fill_rectangle(
				   ACTIVE_DEVICE,
				   (int)rbuf[0],
				   (int)rbuf[1],
				   (int)rbuf[2],
				   (int)rbuf[3],
				   ACTIVE_DEVICE->cindex
				   );
	break;

	/* fill line with data */
  case 26:
	{
	  int x = rbuf[0];
	  int y = rbuf[1];
	  int base = ACTIVE_DEVICE->w * y + x;
	  int i;

	  for (i = 0; i<(int)*nbuf-2; i++)
		ACTIVE_DEVICE->pixmap[base+i] = (ColorIndex)rbuf[i+2];
	}
	break;

	/* query color representation */
  case 29:
	{
	  ColorComponent r, g, b;
	  get_color_rep( ACTIVE_DEVICE, (ColorIndex)rbuf[0], &r, &g, &b );

	  rbuf[1] = r / 255.0;
	  rbuf[2] = g / 255.0;
	  rbuf[3] = b / 255.0;
	  *nbuf = 4;
	};
	break;

  default:
	fprintf(stderr,"%s: unhandled opcode = %d (please notify Pete Ratzlaff: pratzlaff@cfa.harvard.edu)\n",png_ident, *opcode);

  }
}
