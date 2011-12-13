/*
*+
*  Name:
*     idi_gbl

*  Purpose:
*     IDI Global variable definitions

*  Copyright:
*     Copyright (C) 2004 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     Brad Cavanagh (JAC, Hawaii)

*  History:
*     01-OCT-2004 (BEC):
*        Original version.

*-
*/

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "device.dep"
#include "idi.h"

/* External definitions */

struct      int_bar     /* intensity bar structure              */
    {
    int     created;    /* associated window creation flag      */
    int     wnd;        /* associated window                    */
    int     conf;       /* associated config                    */
    int     mem;        /* associated mem                       */
    int     itt;        /* associated ITT                       */
    int     vis;        /* visibility                           */
    };
typedef struct int_bar INT_BAR;

struct
   {
   int     id;          /* current LUT id                      */
   int     nalloc;      /* number of cells allocated from X    */
   int     off;         /* current LUT offset                  */
   int     len;         /* current LUT length                  */
   int     lutpix[256]; /* Color cells indices                 */
   int     lutr[256];   /* lookup Red data                     */
   int     lutg[256];   /* lookup Green data                   */
   int     lutb[256];   /* lookup Blue data                    */
   }
   curlut;

struct      dev_data    /* device data structure   */
    {
    int     unique;     /* display is unique                   */
    int     opened;     /* display is opened                   */
    int     vd_id;      /* display identifier	               */
    int     wd_id;      /* window identifier                   */
    int     inwin;      /* input window identifier             */
    int     gcima_id;   /* GC identifier                       */
    int     gcdraw_id;  /* GC identifier                       */
    int     gccurs_id;  /* GC identifier                       */
    unsigned long pm_mask;
                        /* GC plane mask                       */
    int     kb_id;      /* keyboard identifier                 */
    int     bitmap;     /* display bitmap                      */
    int     overlay;    /* flag to indicate overlay presence   */
    int     pm_mem;     /* base memory associated with pixmap  */
    int     pm_memov;   /* overlay associated with pixmap      */
    char    devtyp [DEVLEN + WINDLEN + 2];
                        /* display type                        */
    char    devnam [DEVLEN + 1];
                        /* display name                        */
    int     dev_xsiz;   /* display x_size                      */
    int     dev_ysiz;   /* display y_size                      */
    int     depth;      /* display depth                       */
    int     zoom_min;   /* min zoom factor                     */
    int     zoom_max;   /* max zoom factor                     */
    int     n_curs;     /* number of cursors                   */
    struct  curs_data *cursor[MAX_CURS];
    int     curs_flag;  /* cursor flag                         */
    int     n_roi;      /* number of ROIs                      */
    struct  roi_data  *roi[MAX_ROI];
    int     roi_flag;   /* ROI flag                            */
    int     n_lut;      /* number of lookup tables             */
    struct  lut_data *lookup[MAX_LUT];
                        /* lookup structure                    */
    int     n_conf;     /* number of configurations            */
    int     confid;     /* current configuration               */
    int     dynconfid;  /* current dyn configuration           */
    struct  conf_data *config[MAX_CONFIG+1];
                        /* config. structure                   */
    int     n_max_inter;/* max no. of enabled                  */
                        /* interactions                        */
    int     n_inter;    /* no. of enabled interactions         */
    struct  inter_data *inter[MAX_INTER];
                        /* interaction structure               */
    float   x_scroll;   /* display X scroll                    */
    float   y_scroll;   /* display Y scroll                    */
    int     zoom;       /* display zoom factor                 */
    struct  int_bar  bar;
                        /* intensity bar structure             */
    FILE    *feb;       /* external bitmap file desriptor      */
    int     extbm_xsize;/* external bitmap X size              */
    int     extbm_ysize;/* external bitmap Y size              */
    int     eblut;      /* external bitmap LUT pointer         */
    int     ebdepth;    /* external bitmap depth               */
    int     ebpackf;    /* external bitmap packing factor      */
    }
    device [MAX_DEV];

struct int_struct
   {
   int     opened;      /* interactors definition flag         */
   int     n_int_dev;   /* no. of interactors                  */
   struct  int_dev_data *int_dev[MAX_INT_DEV];
                        /* interactors structure description   */
   }
   int_struct;

int idi_diag = 0;       /* diagnostic flag                     */

Colormap                cmap;
Cursor                  cur_def, cur_def_int;
Display                 *display_id;
GC                      gcima, gcdraw, gccurs, gclut, gcalph, gcpix;
Pixmap                  pixmap_id;
Visual                  *visual;
Window                  w_id;
XColor                  defs[256], xcol;
XEvent                  evw_data;
XGCValues               values_ima, values_draw, values_curs;
XFontStruct             *font_info;
XImage                  *ima, *zima[MAX_MEM], *lutima;
XPoint                  vlist[8192];
XSegment                curso0[4];

int                     screen = 0;
unsigned long int       black = 0;
unsigned long int       white = 0;
unsigned int            cur_id = 0;

int                     sli_ind = 0;
int                     sli_r = 0;
int                     sli_g = 0;
int                     sli_b = 0;
int                     user = 0;
int                     inter_active = 0;
int                     roi_last_active = 0;
int                     roi_reset = 0;

int                     user_reset[MAX_DEV];
int                     LOC_X[MAX_LOC], LOC_Y[MAX_LOC];
int                     int_scroll_reset[MAX_DEV];
int                     inter_mask[MAX_DEV];
