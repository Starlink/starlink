/* idienv.c */

#define getdev idi__getdev
char *getdev ();
#define getdctfile idi__getdctfile
void getdctfile ( char fildct[] );
#define getdatfile idi__getdatfile
void getdatfile ( char fildat[] );
#define getfile idi__getfile
void getfile ( char *file, char *name );

/* idiextra.c */

#define attach_pixmap idi__attach_pixmap
void attach_pixmap ( int display, int memid );
#define create_pixmap idi__create_pixmap
void create_pixmap ( int display, int memid, int confn );
#define clear_pixmap idi__clear_pixmap
void clear_pixmap ( int display, int memid, int bck,
                    unsigned long plane_mask );
#define free_pixmap idi__free_pixmap
void free_pixmap ( int display, int confid, int memid );
#define int_mem_scroll idi__int_mem_scroll
void int_mem_scroll ( int display, int nint, short ev_type, short ev_data,
                      short pos[], int ew, int* err );
#define int_dis_scroll idi__int_dis_scroll
void int_dis_scroll ( int display, int nint, short ev_type, short ev_data,
                      short pos[], int ew, int* err );
#define imagrefr_p idi__imagrefr_p
void imagrefr_p ( int display, int memid );
#define imagrefr_z_p idi__imagrefr_z_p
void imagrefr_z_p ( int display, int memid, int zoom, float x0, float y0,
                    float xs0, float ys0, float x1, float y1, float xs1,
                    float ys1 );
#define imagrefr_uz_p idi__imagrefr_uz_p
void imagrefr_uz_p ( int display, int memid, int zoom, float x0, float y0,
                     float xs0, float ys0, float x1, float y1, float xs1,
                     float ys1 );
#define polyrefr_p idi__polyrefr_p
void polyrefr_p ( int display, int confn, int memid );
#define textrefr_p idi__textrefr_p
void textrefr_p ( int display, int confn, int memid );
#define polyline_p idi__polyline_p
void polyline_p ( int display, int memid, int col, int style, int xs[],
                  int ys[], int np, int inwin );
#define text_p idi__text_p
void text_p ( int display, int memid, int x0, int y0, int path, int orient,
              int col, int size, char txt[], int inwin );
#define refr_p idi__refr_p
void refr_p ( int display );
#define snap_pix idi__snap_pix
void snap_pix ( int display, int colmode, int npixel, int xoff, int yoff,
                int nlines, int dd, int depth, int packf, int data[] );
#define pack idi__pack
void pack ( unsigned char *inp, unsigned char *outp, int pfact, int width,
            int height );
#define update_memory idi__update_memory
void update_memory ( int display );
#define update_current_pixmap idi__update_current_pixmap
void update_current_pixmap ( int display, int memid );
#define define_memory idi__define_memory
int define_memory ( int display, int xdim, int ydim, int mdepth, int mtype,
                    int* memid );
#define update_keys idi__update_keys
void update_keys ( int display );

/* idilocal.c */
#define local_init idi__local_init
void local_init ( int display );
#define disp_init idi__disp_init
void disp_init ( int display, char wind[], int xdisp, int xwind, int* depth,
                 int* lutlen, int* idierr );
#define acq_disp idi__acq_disp
void acq_disp ( int display );
#define rel_disp idi__rel_disp
void rel_disp ( int display );
#define acq_lut idi__acq_lut
void acq_lut ( int display );
#define icol idi__icol
int icol ( double lutval );
#define icol1 idi__icol1
float icol1 ( int color );
#define cl_display idi__cl_display
void cl_display ( int display, int bck );
#define wr_lut idi__wr_lut
void wr_lut ( int display );
#define wr_lut_gwm idi__wr_lut_gwm
void wr_lut_gwm ( int display );
#define get_slicol idi__get_slicol
void get_slicol ( int* r, int* g, int* b );
#define smv idi__sml
void smv ( int display, int memid );
#define smv_z idi__sml_z
void smv_z ( int display, int memid, int zoom, int x0, int y0, int xs0,
             int ys0, int x1, int y1, int xs1, int ys1 );
#define smv__uz idi__sml_uz
void smv_uz ( int display, int memid, int zoom, int x0, int y0, int xs0,
              int ys0, int x1, int y1, int xs1, int ys1 );
#define int_enable idi__int_enable
void int_enable ( int display );
#define int_disable idi__int_disable
void int_disable ( int display );
#define exit_trg_enable idi__exit_trg_enable
void exit_trg_enable ( int display );
#define exit_trg_disable idi__exit_trg_disable
void exit_trg_disable ( int display );
#define wait_int idi__wait_int
void wait_int ( int display, int* ew, short* type, short* data,
                short position[2] );
#define test_loc idi__test_loc
void test_loc ( int display, int zf, short ev_type, short ev_data,
                short pos[2], int interactor_id, int loc_id, int* f0 );
#define test_evl idi__test_evl
void test_evl ( int display, short ev_type, short ev_data, int interactor_id,
                int evl_id, int* f0 );
#define test_trg idi__test_trg
void test_trg ( short ev_type, short ev_data, int interactor_id, int trg_id,
                int* f0 );
void idi__bar ( int display, int memid, int vis, int* idierr );
#define cross_hair idi__cross_hair
void cross_hair ( int display, int xcur, int ycur, int curcol );
#define cross idi__cross
void cross ( int display, int xcur, int ycur, int curcol );
#define rectange idi__rectangle
void rectangle ( int display, int x0, int y0, int x1, int y1, int roicol );
#define polyline_d idi__polyline_d
void polyline_d ( int display, int memid, int col, int style, int xs[],
                  int ys[], int np );
#define text_d idi__text_d
void text_d ( int display, int x0, int y0, int path, int orient, int col,
              int size, char txt[], int* idierr );

/* idiother.c */
#define roi_rectangle idi__roi_rectangle
void roi_rectangle ( int display, int corner, int x0, int y0, int x1, int y1,
                   int roicol );
#define roi_refresh idi__roi_refresh
void roi_refresh ( int display );
#define roi_switch idi__roi_switch
void roi_switch ( int display, int nint, short ev_type, short ev_data,
                  int* err );
#define lut_loc_rotate idi__lut_loc_rotate
void lut_loc_rotate ( int display, int nint, short ev_type, short ev_data,
                      short pos[], int ew, int* err );
#define dis_zoom idi__dis_zoom
void dis_zoom ( int display, int nint, short ev_type, short ev_data, int* err );
#define dis_unzoom idi__dis_unzoom
void dis_unzoom ( int display, int nint, short ev_type, short ev_data,
                  int* err );
#define dis_clzoom idi__dis_clzoom
void dis_clzoom ( int display, int nint, short ev_type, short ev_data,
                  int* err );
#define remove_pending idi__remove_pending
void remove_pending( int display, short ev_type );
#define is_motion_pending idi__is_motion_pending
int is_motion_pending( int display, int x_pos, int y_pos );
#define mem_screen_conv idi__mem_screen_conv
void mem_screen_conv ( int display, int memid, int xmem, int ymem, int* xscr,
                       int* yscr );
#define screen_mem_conv idi__screen_mem_conv
void screen_mem_conv ( int display, int memid, int xscr, int yscr, int* xmem,
                       int* ymem );
#define hard_cursor idi__hard_cursor
void hard_cursor ( int display, int func, int xin, int yin, int* xout,
                   int* yout );

/* idiutil.c */
int idi__min ( int ival[4], int n );
int idi__max ( int ival[4], int n );
#define refr idi__refr
void refr ( int display, int* iierr );
#define grefr idi__grefr
void grefr ( int display, int* iierr );
#define polyrefr idi__polyrefr
void polyrefr ( int display, int confn, int memid );
#define txtrefr idi__txtrefr
void txtrefr ( int display, int confn, int memid );
#define mem_dis_conv idi__mem_dis_conv
void mem_dis_conv ( int display, int memid, int xmem, int ymem, int* xdis,
                    int* ydis );
#define dis_mem_conv idi__dis_mem_conv
void dis_mem_conv ( int display, int memid, int xdis, int ydis, int* xmem,
                    int* ymem );
#define cl_bitmap idi__cl_bitmap
void cl_bitmap ( int display, int confn, int memid, int bck );
#define cl_dl idi__cl_dl
void cl_dl ( int display, int confn, int memid );
#define mmbm_all idi__mmbm_all
void mmbm_all ( int display, int confn, int memid, int* iierr );
#define mmbm_deall idi__mmbm_deall
void mmbm_deall ( int display, int confn, int memid );
#define wr_mem idi__wr_mem
void wr_mem ( int display, int memid, int x0, int y0, int packf, int iy,
              int dd, int depth, int npixel, int data[] );
#define rd_mem idi__rd_mem
void rd_mem ( int display, int memid, int x0, int y0, int packf, int npixel,
              int iy, int dd, int depth, int ittf, int data[] );
#define ebwmy idi__ebwmy
void ebwmy ( int display, int confn, int memid, int x0, int y0, int* iierr );
#define eb_openi idi__eb_openi
void eb_openi ( int display, char bmdscr[], int* xsize, int* ysize,
                int* packf, int* ebdepth, int* idierr );
#define loc_zero idi__loc_zero
void loc_zero ( int display );
#define test_user idi__test_user
void test_user ( int display, int nint, short ev_type, short ev_data,
                 short pos[], int ew, int* user_flag, int* trg_flag );
#define sync_loc idi__sync_loc
void sync_loc ( int display, int nint, short ev_type, short ev_data,
                short pos[], int* loc_flag );
#define sync_evl idi__sync_evl
void sync_evl ( int display, int nint, short ev_type, short ev_data,
                int* evl_flag );
#define sync_trg idi__sync_trg
void sync_trg ( int display, int nint, short ev_type, short ev_data,
                int* trg_flag );
#define get_loc idi__get_loc
void get_loc ( int loc0, int* interactor_id, int* loc_id );
#define get_evl idi__get_evl
void get_evl ( int evl0, int* interactor_id, int* evl_id );
#define get_trg idi__get_trg
void get_trg ( int trg0, int* interactor_id, int* trg_id );
#define cursor_move idi__cursor_move
void cursor_move ( int display, int nint, short ev_type, short ev_data,
                   short pos[], int ew, int* err );
#define roi_move idi__roi_move
void roi_move ( int display, int nint, short ev_type, short ev_data,
                short pos[], int ew, int* err );
#define roi_modify idi__roi_modify
void roi_modify ( int display, int nint, short ev_type, short ev_data,
                  short pos[], int ew, int* err );
#define lut_rotate idi__lut_rotate
void lut_rotate ( int display, int nint, short ev_type, short ev_data,
                  int* err );
#define lut_slice idi__lut_slice
void lut_slice ( int display, int nint, short ev_type, short ev_data,
                 int* err );
#define mem_scroll idi__mem_scroll
void mem_scroll ( int display, int nint, short ev_type, short ev_data,
                  short pos[], int ew, int* err );
#define mem_zoom idi__mem_zoom
void mem_zoom ( int display, int nint, short ev_type, short ev_data, int* err );
#define mem_unzoom idi__mem_unzoom
void mem_unzoom ( int display, int nint, short ev_type, short ev_data,
                  int* err );
#define mem_clzoom idi__mem_clzoom
void mem_clzoom ( int display, int nint, short ev_type, short ev_data,
                  int* err );
#define polyline_dl idi__polyline_dl
void polyline_dl ( int display, int memid, int color, int style, int xs[],
                   int ys[], int np );
#define text_dl idi__text_dl
void text_dl ( int display, int memid, char text[], int x0, int y0, int path,
               int orient, int color, int size );

/* iic.c */
int IICINC_C ( int display, int memid, int curn, int cursh, int curcol,
               int xcur, int ycur );
int IICSCV_C ( int display, int curn, int vis );
int IICRCP_C ( int display, int inmemid, int curn, int* xcur, int* ycur,
               int* outmemid );
int IICWCP_C ( int display, int memid, int curn, int xcur, int ycur );

/* iid.c */
int IIDOPN_C ( char display[], int* displayid );
int IIDCLO_C ( int display );
int IIDRST_C ( int display );
int IIDQDV_C ( int display, int* nconf, int* xdev, int* ydev, int* depthdev,
               int* maxlutn, int* maxittn, int* maxcurn );
int IIDQCI_C ( int display, int devcap, int size, int capdata[], int* ncap );
int IIDQCR_C ( int display, int devcap, int size, float capdata[], int* ncap );
int IIDQDC_C ( int display, int confn, int memtyp, int maxmem, int* confmode,
               int mlist[], int mxsize[], int mysize[], int mdepth[],
               int ittlen[], int*  nmem );
int IIDSEL_C ( int display, int confn );
int IIDENC_C ( int display );
int IIDSTC_C ( int display, int* confid );
int IIDAMY_C ( int display, int xdim, int ydim, int mdepth, int mtype,
               int* memid );
int IIDRLC_C ( int display, int confid );
int IIDUPD_C ( int display );
void IIDERR_C ( int errn, char errtxt[], int* txtlen );
int IIDIAG_C ( int display, int outf );
int IIDSDP_C ( int display, int memlist[], int nmem, int lutflag[],
               int ittflag[] );
int IIDSNP_C ( int display, int colmode, int npixel, int xoff, int yoff,
               int depth, int packf, int data[] );
int IIDSSS_C ( int display, int memid[], int xoff[], int yoff[], int splitf,
               int splitx, int splity );

/* iig.c */
int IIGPLY_C ( int display, int memid, int x[], int y[], int np, int color,
               int style );
int IIGTXT_C ( int display, int memid, char txt[], int x0, int y0, int path,
               int orient, int color, int txtsize );

/* iii.c */
int IIIENI_C ( int display, int intype, int intid, int objtype, int objid,
               int oper, int trigger );
int IIISTI_C ( int display );
int IIIEIW_C ( int display, int trgstatus[MAX_TRG] );
int IIIGIE_C ( int display, int evlid, int* evlival );
int IIIGRE_C ( int display, int evlid, float* evlrval );
int IIIGSE_C ( int display, int evlid, char evlsval[], int* svallen );
int IIIGLE_C ( int display, int evlid, int* evllval );
int IIIQID_C ( int display, int intype, int intn, char intdscr[],
               int* dscrlen );
int IIIGLD_C ( int display, int locn, int* xdis, int* ydis );

/* iil.c */
int IILWIT_C ( int display, int memid, int ittn, int ittstart, int ittlen,
               float ittdata[] );
int IILRIT_C ( int display, int memid, int ittn, int ittstart, int ittlen,
               float ittdata[] );
int IILWLT_C ( int display, int lutn, int lutstart, int lutlen,
               float lutdata[] );
int IILRLT_C ( int display, int lutn, int lutstart, int lutlen,
               float lutdata[] );
int IILSBV_C ( int display, int memid, int vis );

/* iim.c */
int IIMSTW_C ( int display, int memid, int loaddir, int xwdim, int ywdim,
               int depth, int xwoff, int ywoff );
int IIMWMY_C ( int display, int memid, int data[], int npixel, int depth,
               int packf, int x0, int y0 );
int IIMRMY_C ( int display, int memid, int npixel, int x0, int y0, int depth,
               int packf, int ittf, int data[] );
int IIMSMV_C ( int display, int memlist[], int nmem, int vis );
int IIMCMY_C ( int display, int memlist[], int nmem, int bck );
int IIMSLT_C ( int display, int memid, int lutn, int ittn );
int IIMBLM_C ( int display, int memlst[], int nmem, float period[] );
int IIMEBM_C ( int display, char bmdscr[], char bmtype, int* xdim, int* ydim );

/* iir.c */
int IIRINR_C ( int display, int memid, int roicol, int roixmin, int roiymin,
               int roixmax, int roiymax, int *roiid );
int IIRSRV_C ( int display, int roiid, int vis );
int IIRRRI_C ( int display, int inmemid, int roiid, int* roixmin,
               int* roiymin,int*  roixmax, int* roiymax, int* outmemid );
int IIRWRI_C ( int display, int memid, int roiid, int roixmin, int roiymin,
               int roixmax, int roiymax );

/* iiz.c */
int IIZWSC_C ( int display, int memlist[], int nmem, int xscr, int yscr );
int IIZWZM_C ( int display, int memlist[] , int nmem, int zoomf );
int IIZRSZ_C ( int display, int memid, int* xscr, int* yscr, int* zoom );
int IIZWZP_C ( int display, int xscr, int yscr, int zoomf );
int IIZRZP_C ( int display, int* xscr, int* yscr, int* zoom );

/* kwm.c */
void kwi_xtr ( char filkeyw[], char keywid[], int* nitem, int ikeyw[] );
void kwi_upd ( char filkeyw[], char keywid[], int* nitem, int ikeyw[] );
void kws_xtr ( char* filkeyw, char* keywid, int* nitem, char* skeyw[] );
void kwm_xtr ( char filkeyw[], char keywid[], int* nitem, int ikeyw[] );
void kwm_upd ( char filkeyw[], char keywid[], int* nitem, int ikeyw[] );

/* vdm.c */
int VDM_CRE ( char device[], int xoff, int yoff, int xdim, int ydim,
              char wtype, char display[] );
int VDM_DEL ( char display[] );
int VDM_INQ ( char display[], int* xoff, int* yoff, int* xdim, int* ydim );
int VDM_MOD ( char display[], int vis, int dxoff, int dyoff, int dxdim,
              int dydim );
int VDM_SAV ( char display[], char bmfile[] );
#define filwndcr vdm__filwndcr
void filwndcr ( char filwindow[], int* status );
#define getkwfile idi__getkwfile
void getkwfile ( char filwnd[] );

/* vdmdd.c */
void vdm_cr_a ( char device[], int xoff, int yoff, int xdim, int ydim,
                char wind[], int* status );
void vdm_cr_g ( char device[], int xoff, int yoff, int xdim, int ydim,
                char wind[], int* status );
void vdm_cr_i ( char device[], int xoff, int yoff, int xdim, int ydim,
                char wind[], int* status );
void vdm_del_x ( char device[], char wind[], int* status );
void vdm_inq_x ( char device[], char wind[], int* xoff, int* yoff, int* xdim,
                 int* ydim, int* status );
void vdm_mod_x ( char device[], char wind[], int vis, int dxoff, int dyoff,
                 int dxdim, int dydim, int* status );
void vdm_sav_x ( char device[], char wind[], char bmfile[], int* status );
