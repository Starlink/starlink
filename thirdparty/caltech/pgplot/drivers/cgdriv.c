/*
 * PGPLOT CGM (Computer Graphics Metafile) Driver
 * Version: 1.0 - 17/12/97
 * 
 * Author: Robin Sergeant, Rutherford Appleton Laboratory (Oxfordshire, UK)
 * Email: rsergeant@clara.net
 * WWW: http://www.isis.rl.ac.uk/computing/cgmdriv.htm
 *
 * Output conforms to version 1 of the CGM specification
 *
 * NB Scaling information is ignored by some products (eg MS Word).
 *    However it has been included as some software will take advantage 
 *    of this.
 *
 */

#define DPI 1000            /* set the resoloution to 1000 dpi */
#define PAGE_WIDTH 7.8      /* set the page width to 7.8 inches */
#define PAGE_HEIGHT 10.5    /* set the page height to 10.5 inches */

typedef unsigned short WORD;
typedef unsigned char BYTE;

typedef struct rgb
{
    BYTE r;
    BYTE g;
    BYTE b;
    BYTE pad;		    /* So structure is aligned */
} COLOUR;

static int words_bigendian = 0;     /* Machine type (1 = Bigendian) */ 

/*
 * Certain symbols in fcntl.h may not get defined
 * unless the _POSIX_SOURCE feature-test macro is set.
 */
#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif

#ifdef _WIN32
#define CGDRIV CGDRIV
#elif defined(PG_PPU)
#define CGDRIV cgdriv_
#else
#define CGDRIV cgdriv
#endif

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#ifndef convex
#include <string.h>
#endif

/*
 * VAX VMS includes etc..
 */
#ifdef VMS
#include <signal.h>  /* sleep() is prototyped here */
#include <unixio.h>  /* access() is prototyped here */
#include <descrip.h>
#include <ssdef.h>
#include <clidef.h>
#include <libclidef.h>
#include <lib$routines.h>

typedef struct dsc$descriptor_s VMS_string;

#define VMS_STRING(dsc, string) \
  dsc.dsc$w_length = strlen(string); \
  dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
  dsc.dsc$b_class = DSC$K_CLASS_S; \
  dsc.dsc$a_pointer = string;

static int vms_define_command(char *file, char *command);
static int vms_spawn_nowait(char *command);
#endif

#ifndef VMS
#include <fcntl.h>

#if defined (HAVE_UNISTD_H)
#  include <unistd.h>
#endif /* HAVE_UNISTD_H */

#endif

static void pg_colour_setup(COLOUR *c)
{
    int count;
    c[0].r=255;c[0].g=255;c[0].b=255;       /* White (background) */
    c[1].r=0;c[1].g=0;c[1].b=0;             /* Black (default) */
    c[2].r=255;c[2].g=0;c[2].b=0;           /* Red */
    c[3].r=0;c[3].g=255;c[3].b=0;           /* Green */
    c[4].r=0;c[4].g=0;c[4].b=255;           /* Blue */
    c[5].r=0;c[5].g=255;c[5].b=255;         /* Cyan (Green + Blue) */
    c[6].r=255;c[6].g=0;c[6].b=255;         /* Magenta (Red + Blue) */
    c[7].r=255;c[7].g=255;c[7].b=0;         /* Yellow (Red + Green) */
    c[8].r=255;c[8].g=128;c[8].b=0;         /* Red + Yellow (Orange) */
    c[9].r=128;c[9].g=255;c[9].b=0;         /* Green + Yellow */
    c[10].r=0;c[10].g=255;c[10].b=128;      /* Green + Cyan */
    c[11].r=0;c[11].g=128;c[11].b=255;      /* Blue + Cyan */
    c[12].r=128;c[12].g=0;c[12].b=255;      /* Blue + Magenta */
    c[13].r=255;c[13].g=0;c[13].b=128;      /* Red + Magenta */
    c[14].r=84;c[14].g=84;c[14].b=84;       /* Dark Gray */
    c[15].r=168;c[15].g=168;c[15].b=168;    /* Light Gray */
    for (count=16;count<256;count++) {
        c[count].r=0;c[count].g=0;c[count].b=0; /* make the rest Black */
    }
    return;
}

static int write_byte(FILE *pt,BYTE b)
{
    int error = 0; /* clear error flag */
    if (putc(b,pt) == EOF)
        error = 1; /* set error flag */
    return error;
}

static int write_word(FILE *pt,WORD w)
{
    BYTE *cp;
    int error;
    cp=(BYTE*)&w;
    if (words_bigendian) {
        write_byte(pt,cp[0]);   /* write in big-endian format */
        error = write_byte(pt,cp[1]);
    } else {
        write_byte(pt,cp[1]);   /* write in big-endian format */
        error = write_byte(pt,cp[0]);
    }
    return error;
}

static void write_float(FILE *pt,float f)
{
    BYTE *cp;
    cp=(BYTE*)&f;
    if (words_bigendian) {
        write_byte(pt,cp[0]);   /* write in big-endian format */
        write_byte(pt,cp[1]);
        write_byte(pt,cp[2]);
        write_byte(pt,cp[3]);
    } else {        
        write_byte(pt,cp[3]);   /* write in big-endian format */
        write_byte(pt,cp[2]);
        write_byte(pt,cp[1]);
        write_byte(pt,cp[0]);
    }
    return;
}

static FILE* begin_metafile(const char *filename,const char *name)
{
    int c,length;
    FILE *pt;
    length=strlen(name);
    pt=fopen(filename,"wb");
    if (pt == NULL)
        return pt;
    if (length < 30)    /* use short format if possible */
        write_word(pt,(WORD)(0x0020+length+1));
    else {
        write_word(pt,0x003f);
        write_word(pt,(WORD)(length+1));
    }
    write_byte(pt,(BYTE)length);
    for (c=0;c<length;c++) {
        write_byte(pt,name[c]);
    }
    if (length%2 == 0)  /* pad with trailing null if necessary */
        write_byte(pt,0);
    return pt;
}

static void metafile_version(FILE *pt)
{
    write_word(pt,0x1022);
    write_word(pt,0x0001);
    return;
}

static void metafile_description(FILE *pt,const char *name)
{
    int c,length;
    length=strlen(name);
    if (length < 30)    /* use short format if possible */
        write_word(pt,(WORD)(0x1040+length+1));
    else {
        write_word(pt,0x105f);
        write_word(pt,(WORD)(length+1));
    }
    write_byte(pt,(BYTE)length);
    for (c=0;c<length;c++) {
        write_byte(pt,name[c]);
    }
    if (length%2 == 0)  /* pad with trailing null if necessary */
        write_byte(pt,0);
    return;
}

static void metafile_element_list(FILE *pt)
{
    write_word(pt,0x1166);
    write_word(pt,0x0001);
    write_word(pt,0xffff);
    write_word(pt,0x0001);
    return;
}

static void begin_picture(FILE *pt,char *name)
{
    int c,length;
    length = strlen(name);
    if (length < 30)    /* use short format if possible */
        write_word(pt,(WORD)(0x0060+length+1));
    else {
        write_word(pt,0x007f);
        write_word(pt,(WORD)(length+1));
    }
    write_byte(pt,(BYTE)length);
    for (c=0;c<length;c++) {
        write_byte(pt,name[c]);
    }
    if (length%2 == 0)  /* pad with trailing null if necessary */
        write_byte(pt,0);
    return;
}

static void colour_selection_mode(FILE *pt,int mode)
{
    write_word(pt,0x2042);
    write_word(pt,(WORD)mode);
    return;
}

static void colour_table(FILE *pt,BYTE index,BYTE r,BYTE g,BYTE b)
{
    write_word(pt,0x5444);
    write_byte(pt,index);
    write_byte(pt,r);
    write_byte(pt,g);
    write_byte(pt,b);
    return;
}

static void line_width_specification_mode(FILE *pt,WORD mode)
{
    write_word(pt,0x2062);
    write_word(pt,mode);
    return;
}

static void vdc_extent(FILE *pt,WORD x1,WORD y1,WORD x2,WORD y2)
{
    write_word(pt,0x20c8);
    write_word(pt,x1);
    write_word(pt,y1);
    write_word(pt,x2);
    write_word(pt,y2);
    return;
}

static void begin_picture_body(FILE *pt)
{
    write_word(pt,0x0080);
    return;
}

static void line_colour(FILE *pt,int colourMode,BYTE index,COLOUR colours[])
{
    if (colourMode == 0) {      /* write in indexed colour format */
        write_word(pt,0x5081);
        write_byte(pt,index);
    } else {                    /* write in direct colour format */
        write_word(pt,0x5083);
        write_byte(pt,colours[index].r);
        write_byte(pt,colours[index].g);
        write_byte(pt,colours[index].b);
    }
    write_byte(pt,0);
    return;
}

static void fill_colour(FILE *pt,int colourMode,BYTE index,COLOUR colours[])
{
    if (colourMode == 0) {      /* write in indexed colour format */
        write_word(pt,0x52e1);
        write_byte(pt,index);
    } else {                    /* write in direct colour format */
        write_word(pt,0x52e3);
        write_byte(pt,colours[index].r);
        write_byte(pt,colours[index].g);
        write_byte(pt,colours[index].b);
    }
    write_byte(pt,0);
    return;
}

static void line_width(FILE *pt,WORD width)
{
    write_word(pt,0x5062);
    write_word(pt,width);
    return;
}

static void interior_style(FILE *pt,WORD style)
{
    write_word(pt,0x52c2);
    write_word(pt,style);
    return;
}

static void polyline(FILE *pt,WORD points[],int length)
{
    int c;
    if (length < 16)    /* use short format if possible */
        write_word(pt,(WORD)(0x4020+length*2));
    else {
        write_word(pt,0x403f);
        write_word(pt,(WORD)(length*2));
    }
    for (c=0;c<length;c++) {
        write_word(pt,points[c]);
    }
    return;
}

static void line(FILE *pt,WORD x1,WORD y1,WORD x2,WORD y2)
{
    WORD points[4];
    points[0]=x1;
    points[1]=y1;
    points[2]=x2;
    points[3]=y2;
    polyline(pt,points,4);
    return;
}

static void polygon(FILE *pt,WORD points[],int length)
{
    int c;
    if (length < 16)    /* use short format if possible */
        write_word(pt,(WORD)(0x40e0+length*2));
    else {
        write_word(pt,0x40ff);
        write_word(pt,(WORD)(length*2));
    }
    for (c=0;c<length;c++) {
        write_word(pt,points[c]);
    }
    return;
}

static void rectangle(FILE *pt,WORD x1,WORD y1,WORD x2,WORD y2)
{
    write_word(pt,0x4168);
    write_word(pt,x1);
    write_word(pt,y1);
    write_word(pt,x2);
    write_word(pt,y2);
    return;
}

static void circle(FILE *pt,WORD centreX,WORD centreY,WORD radius)
{
    write_word(pt,0x4186);
    write_word(pt,centreX);
    write_word(pt,centreY);
    write_word(pt,radius);
}

static void end_picture(FILE *pt)
{
    write_word(pt,0x00a0);
    return;
}

static void end_metafile(FILE *pt)
{
    int error;
    error = write_word(pt,0x0040);
    if (error == 1)
        printf("CGMDRIV:Error writing bytes, file is incomplete\n");
    fclose(pt);
    return;
}

static void real_precision(FILE *pt,WORD p1,WORD p2,WORD p3)
{
    write_word(pt,0x10a6);
    write_word(pt,p1);
    write_word(pt,p2);
    write_word(pt,p3);
    return;
}

static void scaling_mode(FILE *pt,float scale)
{
    write_word(pt,0x2026);
    write_word(pt,1);
    write_float(pt,scale);
    return;
}

#ifdef _WIN32
void __stdcall CGDRIV(ifunc, rbuf, nbuf, chr, len, lchr, mode)
    int   *ifunc, *nbuf, *lchr, *mode;
    unsigned len;
    float rbuf[];
    char  *chr;
{
#elif defined(VMS)
void CGDRIV(ifunc, rbuf, nbuf, chrdsc, lchr, mode)
     int *ifunc;
     float rbuf[];
     int *nbuf;
     struct dsc$descriptor_s *chrdsc; /* VMS FORTRAN string descriptor */
     int *lchr;
     int *mode;
{
    int len = chrdsc->dsc$w_length;
    char *chr = chrdsc->dsc$a_pointer;
#else
void CGDRIV(ifunc, rbuf, nbuf, chr, lchr, mode, len)
    int *ifunc, *nbuf, *lchr, *mode;
    int len;
    float rbuf[];
    char *chr;
{
#endif

    static FILE *pt;
    static COLOUR colours[256];
    static int width = 1;
    static int state = 0;       /* Device state (1 = open) */
    static int picture;         /* Picture number */
    static int colourMode;      /* 0 = indexed, 1 = direct */
    static int status = 0;      /* Driver status (1 = called) */

    colourMode = *mode - 1;
    if (!status) {              /* Check machine type on first call */
        WORD test = 0x100;
        BYTE* cp;
        cp = (BYTE*)&test;
        if (cp[0] == 1) {
            words_bigendian = 1;
        }
        status = 1;
    }
    switch(*ifunc) {

/*--- IFUNC=1, Return device name ---------------------------------------*/

    case 1:
    {
        const char *dev_name;
        int c;
        if (*mode == 1)
            dev_name = "CGM (CGM file, indexed colour selection mode)";
        else
            dev_name = "CGMD (CGM file, direct colour selection mode)";
        *lchr = strlen(dev_name);
        strncpy(chr, dev_name, len);
        for (c = *lchr;c<(int)len;c++)
            chr[c] = ' ';
    };
    break;

/*--- IFUNC=2, Return physical min and max for plot device, and range
               of color indices -----------------------------------------*/
    case 2:
        rbuf[0] = 0.0;
        rbuf[1] = 32767.0;
        rbuf[2] = 0.0;
        rbuf[3] = 32767.0;
        rbuf[4] = 0.0;
        rbuf[5] = 255.0;
        *nbuf = 6;
        break;

/*--- IFUNC=3, Return device resolution ---------------------------------*/

    case 3:
        rbuf[0] = DPI;   /* device coordinates per inch */
        rbuf[1] = DPI;
        rbuf[2] = 1.0;		/* Device coordinates per pixel */
        *nbuf = 3;
        break;

/*--- IFUNC=4, Return misc device info ----------------------------------*/

    case 4:
        chr[0] = 'H'; /* Hardcopy device */
        chr[1] = 'N'; /* Cursor is not available */
        chr[2] = 'N'; /* No dashed lines */
        chr[3] = 'A'; /* Area fill available */
        chr[4] = 'T'; /* Thick lines available*/
        chr[5] = 'R'; /* Rectangle fill available */
        chr[6] = 'P'; /* Line of pixels available */
        chr[7] = 'N'; /* Do not prompt on close */
        chr[8] = 'Y'; /* Can return color representation */
        chr[9] = 'N'; /* Not used */
        chr[10] = 'N'; /* Not used */
        *lchr = 11;
        break;

/*--- IFUNC=5, Return default file name ---------------------------------*/

    case 5:
    {
        const char *file_name;
        int c;
        file_name = "pgplot.cgm";
        *lchr = strlen(file_name);
        strncpy(chr, file_name, len);
        for (c = *lchr;c<(int)len;c++)
            chr[c] = ' ';
    }
    break;

/*--- IFUNC=6, Return default physical size of plot ---------------------*/

    case 6:
	    rbuf[0] = 0.0;
	    rbuf[1] = PAGE_WIDTH * DPI;
	    rbuf[2] = 0.0;
	    rbuf[3] = PAGE_HEIGHT * DPI;
        *nbuf = 4;
        break;

/*--- IFUNC=7, Return misc defaults -------------------------------------*/

    case 7:
        rbuf[0] = 1.0;
        *nbuf = 1;
        break;

/*--- IFUNC=8, Select plot ----------------------------------------------*/

    case 8:
        break;

/*--- IFUNC=9, Open workstation -----------------------------------------*/

    case 9:
    {
        const char *name = "PGPLOT CGM File";
        const char *desc = "$Revision$";
        char *filename;
        if (state == 1) {       /* only allow one device */
            printf("CGMDRIV:Error a CGM file is already open\n");
            rbuf[1] = 0.0;  /* error while opening device */
            return;
        }
        filename = (char*)malloc(*lchr * sizeof(char) + 1);
        strncpy(filename, chr, *lchr);
	    filename[*lchr] = '\0';
        pt = begin_metafile(filename, name);
        free(filename);
        if (pt != NULL) {
            state = 1;
            picture = 0;
            pg_colour_setup(colours);
            metafile_version(pt);
            metafile_description(pt,desc);
            real_precision(pt,0,9,23);  /* set to 32-bit floating point */
            metafile_element_list(pt);        
            rbuf[0] = 1.0;
            rbuf[1] = 1.0;  /* no error */
        } else
            rbuf[1] = 0.0;  /* error while opening device */
        *nbuf = 2;
    }
    break;

/*--- IFUNC=10, Close workstation ---------------------------------------*/

    case 10:
        state = 0;
        end_metafile(pt);    
        break;

/*--- IFUNC=11, Begin picture -------------------------------------------*/

    case 11:
    {
        int c;
        char name[60];
        picture++;
        sprintf(name,"Picture %d",picture);
        begin_picture(pt,name);
        colour_selection_mode(pt,colourMode);
        line_width_specification_mode(pt,0);
        vdc_extent(pt,0x0000,0x0000,(WORD)(rbuf[0]+0.5),(WORD)(rbuf[1]+0.5));
        scaling_mode(pt,0.0254F); /* 1 VDC pixel = 0.0254mm or 1/1000" */
        begin_picture_body(pt);
        interior_style(pt,1);
        if (colourMode == 0)    /* add colour table entries if needed */
        {
            for (c=0;c<16;c++)
                colour_table(pt,(BYTE)c,colours[c].r,colours[c].g,colours[c].b);
        }
    }    
    break;

/*--- IFUNC=12, Draw line -----------------------------------------------*/

    case 12:
        line(pt,(WORD)(rbuf[0]+0.5),(WORD)(rbuf[1]+0.5),(WORD)(rbuf[2]+0.5),(WORD)(rbuf[3]+0.5));
        break;

/*--- IFUNC=13, Draw dot ------------------------------------------------*/

    case 13:
        circle(pt,(WORD)(rbuf[0]+0.5),(WORD)(rbuf[1]+0.5),(WORD)width);
        break;

/*--- IFUNC=14, End picture ---------------------------------------------*/

    case 14:
        end_picture(pt);
        break;

/*--- IFUNC=15, Select color index --------------------------------------*/

    case 15:
    {
        int i = (int)(rbuf[0]+0.5);
        line_colour(pt,colourMode,(BYTE)i,colours);
        fill_colour(pt,colourMode,(BYTE)i,colours);
    }  
    break;

/*--- IFUNC=16, Flush buffer. -------------------------------------------*/

    case 16:
        break;

/*--- IFUNC=17, Read cursor. --------------------------------------------*/

    case 17:
        *nbuf = -1;
        break;

/*--- IFUNC=18, Erase alpha screen. -------------------------------------*/
 
    case 18:
        break;

/*--- IFUNC=19, Set line style. -----------------------------------------*/

    case 19:
        *nbuf = -1;
        break;

/*--- IFUNC=20, Polygon fill. -------------------------------------------*/

    case 20:
    {
        static int n = 0;   /* set no. of points to 0 (no polygon) */
        static int c;
        static WORD *points;
        if (n==0) {     /* first call for this polygon? */
            n = (int)(rbuf[0]+0.5)*2; /* if so, set n and allocate memory */
            c = 0;  /* point counter */
            points = (WORD*)malloc(sizeof(WORD)*n);
        } else {
            points[c] = (WORD)(rbuf[0]+0.5);     /* add points to array */
            points[c+1] = (WORD)(rbuf[1]+0.5);
            if (c==n-2) {   /* final set of points? */
                polygon(pt,points,n);   /* if so, create polygon */
                n = 0;
                free(points);
            } else
                c = c+2;
        }    
    }
    break;

/*--- IFUNC=21, Set color representation. -------------------------------*/

    case 21:
    {
        int i = (int)(rbuf[0]+0.5);
        colours[i].r = (int)(255*rbuf[1]+0.5);  /* convert from rgb scales to rgb values */
        colours[i].g = (int)(255*rbuf[2]+0.5);
        colours[i].b = (int)(255*rbuf[3]+0.5);
        if (colourMode == 0)    /* add colour table entry if needed */
            colour_table(pt,(BYTE)i,colours[i].r,colours[i].g,colours[i].b);
    }
    break;

/*--- IFUNC=22, Set line width. -----------------------------------------*/

    case 22:
        width = (int)(rbuf[0]*0.005*DPI+0.5);
        if (width == 0)
            width = 1;
        line_width(pt,(WORD)width);
        break;

/*--- IFUNC=23, Escape --------------------------------------------------*/

    case 23:
        break;

/*--- IFUNC=24, Rectangle Fill. -----------------------------------------*/

    case 24:
        rectangle(pt,(WORD)(rbuf[0]+0.5),(WORD)(rbuf[1]+0.5),(WORD)(rbuf[2]+0.5),(WORD)(rbuf[3]+0.5));
        break;

/*--- IFUNC=25, ---------------------------------------------------------*/

    case 25:
        break;

/*--- IFUNC=26, Line of pixels ------------------------------------------*/

    case 26:
    {
        int x,y,c,i,oldi,x1;
        if (width > 1)      /* make sure the width is 1 */
            line_width(pt,1);
        x = (int)(rbuf[0]+0.5);   /* start co-ordinates */
        y = (int)(rbuf[1]+0.5);
        x1 = 0;     /* set line offset to 0 */
        oldi = (int)(rbuf[2]+0.5);    /* set old colour index to the first */
        for (c=0;c<*nbuf-2;c++)
        {
            i = (int)(rbuf[c+2]+0.5);
            if (i != oldi) {    /* if colour changed then draw line */
                line_colour(pt,colourMode,(BYTE)oldi,colours);
                line(pt,(WORD)(x+x1),(WORD)y,(WORD)(x+c),(WORD)y);
                x1 = c;     /* reset line offset */
            }
            oldi = i;
        }
        line_colour(pt,colourMode,(BYTE)oldi,colours);
        line(pt,(WORD)(x+x1),(WORD)y,(WORD)(x+c),(WORD)y);    /* add final line */
        if (width > 1) 
            line_width(pt,(WORD)width);   /* reset width if changed above */ 
    }
    break;

/*--- IFUNC=29, Query color representation ------------------------------*/
    case 29:
    {
        int i = (int)(rbuf[0]+0.5);
        rbuf[1] = (float)colours[i].r/255;  /* convert from rgb values to rgb scales */
        rbuf[2] = (float)colours[i].g/255;
        rbuf[3] = (float)colours[i].b/255;
        *nbuf = 4;
    }
    break;
  };
  return;
}
