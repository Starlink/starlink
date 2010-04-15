/***********************************************************************
*                                                                      *
*   file IDISTRUCT.H                                                   *
*                                                                      *
*   IDI  Data Structure definition                                     *
*                                                                      *
*                                                                      *
************************************************************************
*   V 2.0    881026                                                    *
*   Author : P. Santin  - Trieste Astronomical Observatory             *
*   Update : Nick Eaton  910424  - Added curlut.nalloc                 *
*   Update : Nick Eaton  910712  - Added device.devnam .pm_id .pm_mem  *
*   Update : Nick Eaton  910718  - Added roi.corner                    *
*   Update : Nick Eaton  910730  - Moved exit trigger to inter_data    *
*   Update : Nick Eaton  911127  - Added pm_mask                       *
*   Update : Nick Eaton  920213  - Added ittinv                        *
*   Update : Nick Eaton  920414  - Added pixmap identifier in mem_data *
*   Update : Nick Eaton  920514  - Added lutpix array to lut structure *
*                                  and overlay flag to device          *
*   Update : Nick Eaton  921102  - Added cursor initialisation flag    *
*   Update : Nick Eaton  921201  - Make x_v_off and y_v_off reals      *
*   Update : Nick Eaton  921215  - Make scroll factors reals           *
*   Update : Nick Eaton  930104  - Add curs_flag and roi_flag          *
*   Update : D Terrett   930415  - Add input window id                 *
************************************************************************


!!  all coordinates are referred to the (0,0) bottom left origin
!!  and are internally inverted for X Graphic Primitives
   ---------------------------------------------------------    */

#if defined(VMS)
#define NOSHARE extern noshare
#else
#define NOSHARE extern
#endif

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

struct      g_list      /* graphic display list element         */
    {
    int     geln;       /* element sequential no.               */
    struct g_list  *next_gel;
                        /* next element pointer                 */
    int     color;      /* draw color                           */
    int     style;      /* draw style                           */
    int     np;         /* no. of points                        */
    int     *xl;        /* coordinates pointers                 */
    int     *yl;
    };
typedef struct g_list G_LIST;

struct      t_list      /* text display list element            */
    {
    int     teln;       /* element sequential no.               */
    struct t_list  *next_tel;
                        /* next element pointer                 */
    int     x0;         /* X position                           */
    int     y0;         /* Y position                           */
    int     path;       /* path                                 */
    int     orient;     /* orientation                          */
    int     color;      /* color                                */
    int     size;       /* size                                 */
    char    text[80];   /* text                                 */
    };
typedef struct t_list T_LIST;

struct      lut_data    /* lookup data structure                */
    {
    int     lut_free;   /* lookup busy flag                     */
    int     lut_len;    /* lookup length                        */
    int     lutpix[256];/* indices into curlut cells            */
    int     lutr[256];  /* lookup Red data                      */
    int     lutg[256];  /* lookup Green data                    */
    int     lutb[256];  /* lookup Blue data                     */
    };
typedef struct lut_data LUT_DATA;

struct      itt_data    /* ITT data structure                   */
    {
    int     itt_def;    /* ITT defined flag                     */
    int     itt_len;    /* ITT length                           */
    int     ittlev[256];/* itt data                             */
    int     ittinv[256];/* inverse itt data                     */
	};
typedef struct itt_data ITT_DATA;

struct      curs_data  /* cursors data structure                */
    {
    int     cur_memid; /* associated memory                     */
    int     cur_sh;    /* cursor shape                          */
    int     cur_col;   /* cursor color                          */
    int     vis;       /* cursor visibility                     */
    int     x_pos;     /* X cursor position                     */
    int     y_pos;     /* Y cursor position                     */
    int     init;      /* initialisation flag                   */
	};
typedef struct curs_data CURS_DATA;

struct      roi_data   /* cursors data structure                */
    {
    int     memid;     /* associated memory                     */
    int     col;       /* roi color                             */
    int     sh;        /* roi shape [0 = rect , 1 = circle]     */
    int     vis;       /* roi visibility                        */
    int     corner;    /* roi active corner                     */
    int     x_min;     /* X min                                 */
    int     y_min;     /* Y min                                 */
    int     x_max;     /* X max                                 */
    int     y_max;     /* Y max                                 */
	};
typedef struct roi_data ROI_DATA;

struct      inter_data /* interactions data structure           */
    {
    int     int_type;    /* interactor type                     */
    int     int_id;      /* interactor id                       */
    int     obj_type;    /* object type                         */
    int     obj_id;      /* object id                           */
    int     oper;        /* interactive operation               */
    int     trigger;     /* exit trigger                        */
	};
typedef struct inter_data INTER_DATA;

struct	    mem_data    /* memory data structure                */
	{
    int     mem_free;   /* memory busy flag                     */
                        /* -1 = free memory                     */
                        /* +1 = busy & cleared memory           */
                        /*  0 = busy memory                     */
    unsigned char *  mmbm;       /* main memory bitmap                   */
    int     attbm;      /* attribute descriptor                 */
    FILE    *feb;       /* external bitmap file descriptor      */
    int     ebdepth;    /* external bitmap depth                */
    int     ebpackf;    /* external bitmap packing factor       */
    int     visibility; /* memory visibility flag               */
    int     x_size;     /* memory x size                        */
    int     y_size;     /* memory y size                        */
    int     x_v_size;   /* memory x visible size                */
    int     y_v_size;   /* memory y visible size                */
    float   x_v_off;    /* memory x visible offset              */
    float   y_v_off;    /* memory y visible offset              */
    int     depth;      /* memory depth                         */
    int     type;       /* memory type                          */
    int     x_woff;     /* image data offset                    */
    int     y_woff;     /*        "                             */
    int     x_wdim;     /* image data dimensions                */
    int     y_wdim;     /*        "                             */
    int     load_dir;   /* transfer window load direction       */
                        /* 0 = bottom->top / 1 = top->bottom    */
    int     lut_id;     /* current lookup                       */
    int     itt_id;     /* current ITT                          */
    struct g_list  *el_glist;
                       /* graphic display list pointer         */
    int     n_gel;      /* total no. of graphic elements        */
    struct g_list  *g_last_el;
                        /* last graphic element pointer         */
    struct t_list  *el_tlist;
                        /* text display list pointer            */
    int     n_tel;      /* total no. of text elements           */
    struct t_list  *t_last_el;
                        /* last text element pointer            */
    float   x_scroll;   /* x scroll position                    */
    float   y_scroll;   /* y scroll position                    */
    int     zoom;       /* zoom factor  effective               */
    int     zoom_new;   /* zoom factor  new                     */
                        /* X,Y fictitious scroll of enlarged    */
                        /* image due to zoom operation          */
                        /* effective                            */
    float   zoom_xsc;
    float   zoom_ysc;
                        /* new                                  */
    float   zoom_xsc_new;
    float   zoom_ysc_new;
    int     n_itt;      /* number of ITT per memory             */
    struct  itt_data *itt[MAX_ITT];
                        /* ITT structure                        */
    int     bck;        /* background value                     */
    int     pm_id;      /* pixmap identifier                    */
    unsigned long pm_mask;
                        /* GC plane mask                        */
    };
typedef struct mem_data MEM_DATA;

struct     conf_data    /* config. data structure               */
    {
    int     dyn;        /* dynamic configurability              */
    int     n_mem;      /* number of memories                   */
    int     memid;      /* current memory id                    */
    struct  mem_data *memory[MAX_MEM];
                        /* memory structure                     */
    };
typedef struct conf_data CONF_DATA;

NOSHARE struct      dev_data    /* device data structure   */
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

NOSHARE struct
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


/* ----------------------------------------------------------- */

struct loc_data
   {
   int      x_min;      /* Locator X range                     */
   int      x_max;
   int      y_min;      /* Locator Y range                     */
   int      y_max;
   int      left_ls;    /* Loc left movement low speed def     */
   int      left_hs;    /*  "   "      "     high  "    "      */
   int      right_ls;   /* Loc right movement low speed def    */
   int      right_hs;   /*  "   "       "     high  "   "      */
   int      up_ls;      /* Loc up movement low speed def       */
   int      up_hs;      /*  "   "    "     high  "    "        */
   int      down_ls;    /* Loc down movement low speed def     */
   int      down_hs;    /*  "   "      "     high  "    "      */
   int      hs_fact;    /* movement high speed factor          */
   int      x_pos;      /* locator X & Y values                */
   int      y_pos;
   };
typedef struct loc_data LOC_DATA;

struct evl_data
   {
   int      type;       /* evaluator type                      */
                        /* EVLT / EVLI / EVLR / EVLS           */
   int      def[2];     /* keys def for Toggle type evaluator  */
   int      min;        /* evaluator range                     */
   int      max;
   int      ival;       /* evaluators value                    */
   float    rval;
   char     sval[32];
   };
typedef struct evl_data EVL_DATA;

struct trg_data
   {
   int     type;        /* trigger type [KEY / BUTT]           */
   int     def;         /* trigger definition                  */
   };
typedef struct trg_data TRG_DATA;

struct int_dev_data
   {
   int     descr;       /* interactor description              */
   int     n_loc;       /* no. of available locators           */
   struct loc_data *loc[MAX_LOC];
                        /* locators definition                 */
   int     n_evl;       /* no. of available evaluators         */
   struct evl_data *evl[MAX_EVL];
                        /* evaluators definition               */
   int     n_trg;       /* no. of available triggers           */
   struct trg_data *trg[MAX_TRG];
                        /* triggers definition                 */
   };
typedef struct int_dev_data INT_DEV_DATA;


NOSHARE struct int_struct
   {
   int     opened;      /* interactors definition flag         */
   int     n_int_dev;   /* no. of interactors                  */
   struct  int_dev_data *int_dev[MAX_INT_DEV];
                        /* interactors structure description   */
   }
   int_struct;


/* ----------------------------------------------------------- */

NOSHARE int        idi_diag;    /* diagnostic flag                     */

/* ----------------------------------------------------------- */

