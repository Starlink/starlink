{
#define maximize_width 15
#define maximize_height 15
static UNSIGNED_CHAR maximize_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0xfc, 0x1f, 0x04, 0x10, 0x04, 0x70, 0x04, 0x70,
   0x04, 0x70, 0x04, 0x70, 0x04, 0x70, 0x04, 0x70, 0x04, 0x70, 0x04, 0x70,
   0xfc, 0x7f, 0xf0, 0x7f, 0xf0, 0x7f};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("maximize"), (char*)maximize_bits, maximize_width, maximize_height);
}
{
#define act_fold_width 16
#define act_fold_height 10
static UNSIGNED_CHAR act_fold_bits[] = {
   0xfc, 0x00, 0xaa, 0x0f, 0x55, 0x15, 0xeb, 0xff, 0x15, 0x80, 0x0b, 0x40,
   0x05, 0x20, 0x03, 0x10, 0x01, 0x08, 0xff, 0x07};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("act_fold"), (char*)act_fold_bits, act_fold_width, act_fold_height);
}
{
/* XPM */
static char * act_fold_xpm[] = {
/* width height num_colors chars_per_pixel */
"16 12 4 1",
/* colors */
" 	s None	c None",
".	c black",
"X	c yellow",
"o	c #5B5B57574646",
/* pixels */
"   ....         ",
"  .XXXX.        ",
" .XXXXXX.       ",
".............   ",
".oXoXoXoXoXo.   ",
".XoX............",
".oX.XXXXXXXXXXX.",
".Xo.XXXXXXXXXX. ",
".o.XXXXXXXXXXX. ",
".X.XXXXXXXXXXX. ",
"..XXXXXXXXXX..  ",
".............   "};
Tix_DefinePixmap(Et_Interp, Tk_GetUid("act_fold"), act_fold_xpm);
}
{
#define balarrow_width 6
#define balarrow_height 6
static UNSIGNED_CHAR balarrow_bits[] = {
   0x1f, 0x07, 0x07, 0x09, 0x11, 0x20};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("balarrow"), (char*)balarrow_bits, balarrow_width, balarrow_height);
}
{
#define cbxarrow_width 11
#define cbxarrow_height 14
static UNSIGNED_CHAR cbxarrow_bits[] = {
   0x00, 0x00, 0x70, 0x00, 0x70, 0x00, 0x70, 0x00, 0x70, 0x00, 0x70, 0x00,
   0xfe, 0x03, 0xfc, 0x01, 0xf8, 0x00, 0x70, 0x00, 0x20, 0x00, 0x00, 0x00,
   0xfe, 0x03, 0xfe, 0x03};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("cbxarrow"), (char*)cbxarrow_bits, cbxarrow_width, cbxarrow_height);
}
{
#define ck_def_width 13
#define ck_def_height 13
static UNSIGNED_CHAR ck_def_bits[] = {
   0xff, 0x1f, 0x01, 0x10, 0x55, 0x15, 0x01, 0x10, 0x55, 0x15, 0x01, 0x10,
   0x55, 0x15, 0x01, 0x10, 0x55, 0x15, 0x01, 0x10, 0x55, 0x15, 0x01, 0x10,
   0xff, 0x1f};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("ck_def"), (char*)ck_def_bits, ck_def_width, ck_def_height);
}
{
#define ck_off_width 13
#define ck_off_height 13
static UNSIGNED_CHAR ck_off_bits[] = {
   0xff, 0x1f, 0x01, 0x10, 0x01, 0x10, 0x01, 0x10, 0x01, 0x10, 0x01, 0x10,
   0x01, 0x10, 0x01, 0x10, 0x01, 0x10, 0x01, 0x10, 0x01, 0x10, 0x01, 0x10,
   0xff, 0x1f};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("ck_off"), (char*)ck_off_bits, ck_off_width, ck_off_height);
}
{
#define ck_on_width 13
#define ck_on_height 13
static UNSIGNED_CHAR ck_on_bits[] = {
   0xff, 0x1f, 0x01, 0x10, 0x01, 0x10, 0x01, 0x14, 0x01, 0x16, 0x01, 0x17,
   0x89, 0x13, 0xdd, 0x11, 0xf9, 0x10, 0x71, 0x10, 0x21, 0x10, 0x01, 0x10,
   0xff, 0x1f};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("ck_on"), (char*)ck_on_bits, ck_on_width, ck_on_height);
}
{
#define cross_width 14
#define cross_height 14
static  UNSIGNED_CHAR cross_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x06, 0x18, 0x0e, 0x1c, 0x1c, 0x0e, 0x38, 0x07,
   0xf0, 0x03, 0xe0, 0x01, 0xe0, 0x01, 0xf0, 0x03, 0x38, 0x07, 0x1c, 0x0e,
   0x0e, 0x1c, 0x06, 0x18};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("cross"), (char*)cross_bits, cross_width, cross_height);
}
{
#define decr_width 7
#define decr_height 4
static UNSIGNED_CHAR decr_bits[] = {
   0x7f, 0x3e, 0x1c, 0x08};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("decr"), (char*)decr_bits, decr_width, decr_height);
}
{
#define drop_width 16
#define drop_height 16
#define drop_x_hot 6
#define drop_y_hot 4
static UNSIGNED_CHAR drop_bits[] = {
   0x00, 0x00, 0xfe, 0x07, 0x02, 0x04, 0x02, 0x04, 0x42, 0x04, 0xc2, 0x04,
   0xc2, 0x05, 0xc2, 0x07, 0xc2, 0x07, 0xc2, 0x0f, 0xfe, 0x1f, 0xc0, 0x07,
   0xc0, 0x06, 0x00, 0x0c, 0x00, 0x1c, 0x00, 0x08};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("drop"), (char*)drop_bits, drop_width, drop_height);
}
{
#define file_width 12
#define file_height 12
static UNSIGNED_CHAR file_bits[] = {
   0xfe, 0x00, 0x02, 0x03, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02,
   0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0x02, 0xfe, 0x03};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("file"), (char*)file_bits, file_width, file_height);
}
{
/* XPM */
static char * file_xpm[] = {
"12 12 3 1",
" 	s None	c None",
".	c black",
"X	c #FFFFFFFFF3CE",
" ........   ",
" .XXXXXX.   ",
" .XXXXXX... ",
" .XXXXXXXX. ",
" .XXXXXXXX. ",
" .XXXXXXXX. ",
" .XXXXXXXX. ",
" .XXXXXXXX. ",
" .XXXXXXXX. ",
" .XXXXXXXX. ",
" .XXXXXXXX. ",
" .......... "};
Tix_DefinePixmap(Et_Interp, Tk_GetUid("file"), file_xpm);
}
{
#define folder_width 16
#define folder_height 10
static UNSIGNED_CHAR folder_bits[] = {
   0xfc, 0x00, 0x02, 0x07, 0x01, 0x08, 0x01, 0x08, 0x01, 0x08, 0x01, 0x08,
   0x01, 0x08, 0x01, 0x08, 0x01, 0x08, 0xff, 0x07};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("folder"), (char*)folder_bits, folder_width, folder_height);
}
{
/* XPM */
static char * folder_foo_xpm[] = {
/* width height num_colors chars_per_pixel */
"16 12 3 1",
/* colors */
" 	s None	c None",
".	c black",
"X	c #f0ff80",
/* pixels */
"   ....         ",
"  .XXXX.        ",
" .XXXXXX.       ",
".............   ",
".XXXXXXXXXXX.   ",
".XXXXXXXXXXX.   ",
".XXXXXXXXXXX.   ",
".XXXXXXXXXXX.   ",
".XXXXXXXXXXX.   ",
".XXXXXXXXXXX.   ",
".XXXXXXXXXXX.   ",
".............   "};
Tix_DefinePixmap(Et_Interp, Tk_GetUid("folder"), folder_foo_xpm);
}
{
#define harddisk_width 32
#define harddisk_height 32
static UNSIGNED_CHAR harddisk_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0xf8, 0xff, 0xff, 0x1f, 0x08, 0x00, 0x00, 0x18, 0xa8, 0xaa, 0xaa, 0x1a,
   0x48, 0x55, 0xd5, 0x1d, 0xa8, 0xaa, 0xaa, 0x1b, 0x48, 0x55, 0x55, 0x1d,
   0xa8, 0xfa, 0xaf, 0x1a, 0xc8, 0xff, 0xff, 0x1d, 0xa8, 0xfa, 0xaf, 0x1a,
   0x48, 0x55, 0x55, 0x1d, 0xa8, 0xaa, 0xaa, 0x1a, 0x48, 0x55, 0x55, 0x1d,
   0xa8, 0xaa, 0xaa, 0x1a, 0xf8, 0xff, 0xff, 0x1f, 0xf8, 0xff, 0xff, 0x1f,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("harddisk"), (char*)harddisk_bits, harddisk_width, harddisk_height);
}
{
#define hourglass_width 32
#define hourglas_height 32
#define hourglas_x_hot 16
#define hourglas_y_hot 15
static UNSIGNED_CHAR hourglas_bits[] = {
   0xfe, 0xff, 0xff, 0xff, 0xfe, 0xff, 0xff, 0xff, 0xfe, 0xff, 0xff, 0xff,
   0x7c, 0x00, 0x00, 0x7c, 0x7c, 0x00, 0x00, 0x7c, 0x7c, 0x00, 0x00, 0x7c,
   0xfc, 0x00, 0x00, 0x7e, 0xfc, 0x00, 0x00, 0x7e, 0xfc, 0x00, 0x00, 0x7e,
   0xbc, 0x01, 0x00, 0x7b, 0xbc, 0xfd, 0x7e, 0x7b, 0x3c, 0xfb, 0xbf, 0x79,
   0x3c, 0xe6, 0xcf, 0x78, 0x3c, 0xdc, 0x77, 0x78, 0x3c, 0x38, 0x39, 0x78,
   0x3c, 0x60, 0x0d, 0x78, 0x3c, 0x38, 0x38, 0x78, 0x3c, 0x1c, 0x71, 0x78,
   0x3c, 0x06, 0xc1, 0x78, 0x3c, 0x03, 0x80, 0x79, 0xbc, 0x01, 0x00, 0x7b,
   0xbc, 0x01, 0x00, 0x7b, 0xfc, 0x00, 0x01, 0x7e, 0xfc, 0x00, 0x01, 0x7e,
   0xfc, 0x80, 0x03, 0x7e, 0x7c, 0xc0, 0x07, 0x7c, 0x7c, 0xf0, 0x1f, 0x7c,
   0x7c, 0xfe, 0xff, 0x7c, 0xfe, 0xff, 0xff, 0x7f, 0xfe, 0xff, 0xff, 0xff,
   0xfe, 0xff, 0xff, 0xff, 0xfe, 0xff, 0xff, 0xff};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("hourglas"), (char*)hourglas_bits, hourglass_width, hourglas_height);
}
{
#define incr_width 7
#define incr_height 4
static UNSIGNED_CHAR incr_bits[] = {
   0x08, 0x1c, 0x3e, 0x7f};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("incr"), (char*)incr_bits, incr_width, incr_height);
}
{
/* XPM */
static char * info_xpm[] = {
"32 32 3 1",
" 	s None	c None",
".	c #000000000000",
"X	c white",
"                                ",
"           .........            ",
"        ...XXXXXXXXX...         ",
"       .XXXXXXXXXXXXXXX.        ",
"     ..XXXXXXXXXXXXXXXXX..      ",
"    .XXXXXXXXXXXXXXXXXXXXX.     ",
"   .XXXXXXXXXX...XXXXXXXXXX.    ",
"   .XXXXXXXXX.....XXXXXXXXX.    ",
"  .XXXXXXXXX.......XXXXXXXXX.   ",
" .XXXXXXXXXX.......XXXXXXXXXX.  ",
" .XXXXXXXXXX.......XXXXXXXXXX.  ",
" .XXXXXXXXXXX.....XXXXXXXXXXX.  ",
".XXXXXXXXXXXXX...XXXXXXXXXXXXX. ",
".XXXXXXXXXXXXXXXXXXXXXXXXXXXXX. ",
".XXXXXXXXXXXXXXXXXXXXXXXXXXXXX. ",
".XXXXXXXXXXX.......XXXXXXXXXXX. ",
".XXXXXXXXXXX.......XXXXXXXXXXX. ",
".XXXXXXXXXXX.......XXXXXXXXXXX. ",
".XXXXXXXXXXX.......XXXXXXXXXXX. ",
".XXXXXXXXXXX.......XXXXXXXXXXX. ",
".XXXXXXXXXXX.......XXXXXXXXXXX. ",
" .XXXXXXXXXX.......XXXXXXXXXX.  ",
" .XXXXXXXXXX.......XXXXXXXXXX.  ",
" .XXXXXXXXXX.......XXXXXXXXXX.  ",
"  .XXXXXXXXX.......XXXXXXXXX.   ",
"   .XXXXXXXX.......XXXXXXXX.    ",
"   .XXXXXXXX.......XXXXXXXX.    ",
"    .XXXXXXXXXXXXXXXXXXXXX.     ",
"     ..XXXXXXXXXXXXXXXXX..      ",
"       .XXXXXXXXXXXXXXX.        ",
"        ...XXXXXXXXX...         ",
"           .........            "};
Tix_DefinePixmap(Et_Interp, Tk_GetUid("info"), info_xpm);
}
{
#define minimize_width 15
#define minimize_height 15
static UNSIGNED_CHAR minimize_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0x01,
   0x20, 0x03, 0x20, 0x03, 0xe0, 0x03, 0xc0, 0x03, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("minimize"), (char*)minimize_bits, minimize_width, minimize_height);
}
{
#define minus_width 9
#define minus_height 9
static UNSIGNED_CHAR minus_bits[] = {
   0xff, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x7d, 0x01, 0x01, 0x01,
   0x01, 0x01, 0x01, 0x01, 0xff, 0x01};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("minus"), (char*)minus_bits, minus_width, minus_height);
}
{
/* XPM */
static char * minus_xpm[] = {
"9 9 2 1",
".	s None	c None",
" 	c black",
"         ",
" ....... ",
" ....... ",
" ....... ",
" .     . ",
" ....... ",
" ....... ",
" ....... ",
"         "};
Tix_DefinePixmap(Et_Interp, Tk_GetUid("minus"), minus_xpm);
}
{
#define minusarm_width 9
#define minusarm_height 9
static UNSIGNED_CHAR minusarm_bits[] = {
   0xff, 0x01, 0x01, 0x01, 0x7d, 0x01, 0x7d, 0x01, 0x01, 0x01, 0x7d, 0x01,
   0x7d, 0x01, 0x01, 0x01, 0xff, 0x01};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("minusarm"), (char*)minusarm_bits, minusarm_width, minusarm_height);
}
{
/* XPM */
static char * minusarm_xpm[] = {
"9 9 3 1",
" 	c black",
".	c yellow",
"X	c #808080808080",
"         ",
" ....... ",
" ....... ",
" .XXXXX. ",
" .X   X. ",
" .XXXXX. ",
" ....... ",
" ....... ",
"         "};
Tix_DefinePixmap(Et_Interp, Tk_GetUid("minusarm"), minusarm_xpm);
}
{
#define network_width 32
#define network_height 32
static UNSIGNED_CHAR network_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x7f, 0x00, 0x00, 0x02, 0x40,
   0x00, 0x00, 0xfa, 0x5f, 0x00, 0x00, 0x0a, 0x50, 0x00, 0x00, 0x0a, 0x52,
   0x00, 0x00, 0x0a, 0x52, 0x00, 0x00, 0x8a, 0x51, 0x00, 0x00, 0x0a, 0x50,
   0x00, 0x00, 0x4a, 0x50, 0x00, 0x00, 0x0a, 0x50, 0x00, 0x00, 0x0a, 0x50,
   0x00, 0x00, 0xfa, 0x5f, 0x00, 0x00, 0x02, 0x40, 0xfe, 0x7f, 0x52, 0x55,
   0x02, 0x40, 0xaa, 0x6a, 0xfa, 0x5f, 0xfe, 0x7f, 0x0a, 0x50, 0xfe, 0x7f,
   0x0a, 0x52, 0x80, 0x00, 0x0a, 0x52, 0x80, 0x00, 0x8a, 0x51, 0x80, 0x00,
   0x0a, 0x50, 0x80, 0x00, 0x4a, 0x50, 0x80, 0x00, 0x0a, 0x50, 0xe0, 0x03,
   0x0a, 0x50, 0x20, 0x02, 0xfa, 0xdf, 0x3f, 0x03, 0x02, 0x40, 0xa0, 0x02,
   0x52, 0x55, 0xe0, 0x03, 0xaa, 0x6a, 0x00, 0x00, 0xfe, 0x7f, 0x00, 0x00,
   0xfe, 0x7f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("network"), (char*)network_bits, network_width, network_height);
}
{
/* XPM */
static char * no_entry_xpm[] = {
"32 32 4 1",
" 	s None	c None",
".	c #000000000000",
"X	c red",
"o	c yellow",
"                                ",
"           .........            ",
"        ...XXXXXXXXX...         ",
"       .XXXXXXXXXXXXXXX.        ",
"     ..XXXXXXXXXXXXXXXXX..      ",
"    .XXXXXXXXXXXXXXXXXXXXX.     ",
"   .XXXXXXXXXXXXXXXXXXXXXXX.    ",
"   .XXXXXXXXXXXXXXXXXXXXXXX.    ",
"  .XXXXXXXXXXXXXXXXXXXXXXXXX.   ",
" .XXXXXXXXXXXXXXXXXXXXXXXXXXX.  ",
" .XXXXXXXXXXXXXXXXXXXXXXXXXXX.  ",
" .XXXXXXXXXXXXXXXXXXXXXXXXXXX.  ",
".XXXXXXXXXXXXXXXXXXXXXXXXXXXXX. ",
".XXX.......................XXX. ",
".XXX.ooooooooooooooooooooo.XXX. ",
".XXX.ooooooooooooooooooooo.XXX. ",
".XXX.ooooooooooooooooooooo.XXX. ",
".XXX.ooooooooooooooooooooo.XXX. ",
".XXX.ooooooooooooooooooooo.XXX. ",
".XXX.ooooooooooooooooooooo.XXX. ",
".XXX.......................XXX. ",
" .XXXXXXXXXXXXXXXXXXXXXXXXXXX.  ",
" .XXXXXXXXXXXXXXXXXXXXXXXXXXX.  ",
" .XXXXXXXXXXXXXXXXXXXXXXXXXXX.  ",
"  .XXXXXXXXXXXXXXXXXXXXXXXXX.   ",
"   .XXXXXXXXXXXXXXXXXXXXXXX.    ",
"   .XXXXXXXXXXXXXXXXXXXXXXX.    ",
"    .XXXXXXXXXXXXXXXXXXXXX.     ",
"     ..XXXXXXXXXXXXXXXXX..      ",
"       .XXXXXXXXXXXXXXX.        ",
"        ...XXXXXXXXX...         ",
"           .........            "};
Tix_DefinePixmap(Et_Interp, Tk_GetUid("no_entry"), no_entry_xpm);
}
{
#define openfile_width 16
#define openfile_height 10
static UNSIGNED_CHAR openfile_bits[] = {
   0xf8, 0x01, 0x04, 0x06, 0x02, 0x08, 0x02, 0x10, 0xe2, 0xff, 0x52, 0x55,
   0xaa, 0x2a, 0x56, 0x15, 0xaa, 0x0a, 0xfe, 0x07};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("openfile"), (char*)openfile_bits, openfile_width, openfile_height);
}
{
#define openfold_width 16
#define openfold_height 10
static UNSIGNED_CHAR openfold_bits[] = {
   0xfc, 0x00, 0x02, 0x07, 0x01, 0x08, 0xc1, 0xff, 0x21, 0x80, 0x11, 0x40,
   0x09, 0x20, 0x05, 0x10, 0x03, 0x08, 0xff, 0x07};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("openfold"), (char*)openfold_bits, openfold_width, openfold_height);
}
{
/* XPM */
static char * openfolder_xpm[] = {
/* width height num_colors chars_per_pixel */
"16 12 3 1",
/* colors */
" 	s None	c None",
".	c black",
"X	c #f0ff80",
/* pixels */
"   ....         ",
"  .XXXX.        ",
" .XXXXXX.       ",
".............   ",
".XXXXXXXXXXX.   ",
".XXX............",
".XX.XXXXXXXXXXX.",
".XX.XXXXXXXXXX. ",
".X.XXXXXXXXXXX. ",
".X.XXXXXXXXXXX. ",
"..XXXXXXXXXX..  ",
".............   "};
Tix_DefinePixmap(Et_Interp, Tk_GetUid("openfold"), openfolder_xpm);
}
{
#define plus_width 9
#define plus_height 9
static UNSIGNED_CHAR plus_bits[] = {
   0xff, 0x01, 0x01, 0x01, 0x11, 0x01, 0x11, 0x01, 0x7d, 0x01, 0x11, 0x01,
   0x11, 0x01, 0x01, 0x01, 0xff, 0x01};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("plus"), (char*)plus_bits, plus_width, plus_height);
}
{
/* XPM */
static char * plus_xpm[] = {
"9 9 2 1",
".	s None	c None",
" 	c black",
"         ",
" ....... ",
" ... ... ",
" ... ... ",
" .     . ",
" ... ... ",
" ... ... ",
" ....... ",
"         "};
Tix_DefinePixmap(Et_Interp, Tk_GetUid("plus"), plus_xpm);
}
{
#define plusarm_width 9
#define plusarm_height 9
static UNSIGNED_CHAR plusarm_bits[] = {
   0xff, 0x01, 0x01, 0x01, 0x6d, 0x01, 0x6d, 0x01, 0x01, 0x01, 0x6d, 0x01,
   0x6d, 0x01, 0x01, 0x01, 0xff, 0x01};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("plusarm"), (char*)plusarm_bits, plusarm_width, plusarm_height);
}
{
/* XPM */
static char * plusarm_xpm[] = {
"9 9 3 1",
" 	c black",
".	c yellow",
"X	c gray40",
"         ",
" ....... ",
" ... ... ",
" ..X X.. ",
" .  X  . ",
" ..X X.. ",
" ... ... ",
" ....... ",
"         "};
Tix_DefinePixmap(Et_Interp, Tk_GetUid("plusarm"), plusarm_xpm);
}
{
#define resize1_width 13
#define resize1_height 13
#define resize1_x_hot 6
#define resize1_y_hot 6
static UNSIGNED_CHAR resize1_bits[] = {
   0x7f, 0x00, 0x21, 0x00, 0x11, 0x00, 0x31, 0x00, 0x6d, 0x00, 0xdb, 0x00,
   0xb1, 0x11, 0x60, 0x1b, 0xc0, 0x16, 0x80, 0x11, 0x00, 0x11, 0x80, 0x10,
   0xc0, 0x1f};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("resize1"),(char*) resize1_bits, resize1_width, resize1_height);
}
{
#define resize2_width 13
#define resize2_height 13
#define resize2_x_hot 6
#define resize2_y_hot 6
static UNSIGNED_CHAR resize2_bits[] = {
   0xc0, 0x1f, 0x80, 0x10, 0x00, 0x11, 0x80, 0x11, 0xc0, 0x16, 0x60, 0x1b,
   0xb1, 0x11, 0xdb, 0x00, 0x6d, 0x00, 0x31, 0x00, 0x11, 0x00, 0x21, 0x00,
   0x7f, 0x00};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("resize2"), (char*)resize2_bits, resize2_width, resize2_height);
}
{
#define restore_width 15
#define restore_height 15
static UNSIGNED_CHAR restore_bits[] = {
   0x00, 0x00, 0x80, 0x00, 0xc0, 0x01, 0xe0, 0x03, 0xf0, 0x07, 0xf8, 0x0f,
   0xfc, 0x1f, 0x00, 0x00, 0xfc, 0x1f, 0xf8, 0x0f, 0xf0, 0x07, 0xe0, 0x03,
   0xc0, 0x01, 0x80, 0x00, 0x00, 0x00};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("restore"), (char*)restore_bits, restore_width, restore_height);
}
{
#define srcfile_width 12
#define srcfile_height 12
static UNSIGNED_CHAR srcfile_bits[] = {
   0xfe, 0x01, 0x02, 0x01, 0x02, 0x07, 0x02, 0x04, 0x72, 0x04, 0x8a, 0x04,
   0x0a, 0x04, 0x0a, 0x04, 0x8a, 0x04, 0x72, 0x04, 0x02, 0x04, 0xfe, 0x07};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("srcfile"), (char*)srcfile_bits, srcfile_width, srcfile_height);
}
{
/* XPM */
static char * srcfile_xpm[] = {
"12 12 3 1",
" 	s None	c None",
".	c black",
"X	c gray91",
" ........   ",
" .XXXXXX.   ",
" .XXXXXX... ",
" .XXXXXXXX. ",
" .XX...XXX. ",
" .X.XXX.XX. ",
" .X.XXXXXX. ",
" .X.XXXXXX. ",
" .X.XXX.XX. ",
" .XX...XXX. ",
" .XXXXXXXX. ",
" .......... "};
Tix_DefinePixmap(Et_Interp, Tk_GetUid("srcfile"), srcfile_xpm);
}
{
#define system_width 15
#define system_height 15
static UNSIGNED_CHAR system_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x3f,
   0x02, 0x20, 0x02, 0x20, 0xfe, 0x3f, 0xfe, 0x3f, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("system"), (char*)system_bits, system_width, system_height);
}
{
#define textfile_width 12
#define textfile_height 12
static UNSIGNED_CHAR textfile_bits[] = {
   0xfe, 0x01, 0x02, 0x01, 0x02, 0x07, 0x7a, 0x04, 0x02, 0x04, 0x3a, 0x04,
   0x02, 0x04, 0xfa, 0x04, 0x02, 0x04, 0xfa, 0x04, 0x02, 0x04, 0xfe, 0x07};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("textfile"), (char*)textfile_bits, textfile_width, textfile_height);
}
{
/* XPM */
static char * textfile_xpm[] = {
"12 12 3 1",
" 	s None	c None",
".	c black",
"X	c #FFFFFFFFF3CE",
" ........   ",
" .XXXXXX.   ",
" .XXXXXX... ",
" .X....XXX. ",
" .XXXXXXXX. ",
" .X...XXXX. ",
" .XXXXXXXX. ",
" .X.....XX. ",
" .XXXXXXXX. ",
" .X.....XX. ",
" .XXXXXXXX. ",
" .......... "};
Tix_DefinePixmap(Et_Interp, Tk_GetUid("textfile"), textfile_xpm);
}
{
#define tick_width 14
#define tick_height 14
static UNSIGNED_CHAR tick_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x38, 0x00, 0x1c, 0x00, 0x0e,
   0x00, 0x07, 0x80, 0x03, 0xc2, 0x01, 0xe7, 0x00, 0x7f, 0x00, 0x3e, 0x00,
   0x1c, 0x00, 0x08, 0x00};
Tk_DefineBitmap(Et_Interp, Tk_GetUid("tick"), (char*)tick_bits, tick_width, tick_height);
}
{
/* XPM */
static char * warning_xpm[] = {
"32 32 3 1",
" 	s None	c None",
".	c #000000000000",
"X	c yellow",
"                                ",
"           .........            ",
"        ...XXXXXXXXX...         ",
"       .XXXXXXXXXXXXXXX.        ",
"     ..XXXXXXXXXXXXXXXXX..      ",
"    .XXXXXXXXX...XXXXXXXXX.     ",
"   .XXXXXXXXX.....XXXXXXXXX.    ",
"   .XXXXXXXXX.....XXXXXXXXX.    ",
"  .XXXXXXXXX.......XXXXXXXXX.   ",
" .XXXXXXXXXX.......XXXXXXXXXX.  ",
" .XXXXXXXXXX.......XXXXXXXXXX.  ",
" .XXXXXXXXXX.......XXXXXXXXXX.  ",
".XXXXXXXXXXX.......XXXXXXXXXXX. ",
".XXXXXXXXXXX.......XXXXXXXXXXX. ",
".XXXXXXXXXXX.......XXXXXXXXXXX. ",
".XXXXXXXXXXX.......XXXXXXXXXXX. ",
".XXXXXXXXXXX.......XXXXXXXXXXX. ",
".XXXXXXXXXXXX.....XXXXXXXXXXXX. ",
".XXXXXXXXXXXX.....XXXXXXXXXXXX. ",
".XXXXXXXXXXXX.....XXXXXXXXXXXX. ",
".XXXXXXXXXXXXX...XXXXXXXXXXXXX. ",
" .XXXXXXXXXXXXXXXXXXXXXXXXXXX.  ",
" .XXXXXXXXXXXX...XXXXXXXXXXXX.  ",
" .XXXXXXXXXXX.....XXXXXXXXXXX.  ",
"  .XXXXXXXXX.......XXXXXXXXX.   ",
"   .XXXXXXXX.......XXXXXXXX.    ",
"   .XXXXXXXX.......XXXXXXXX.    ",
"    .XXXXXXXX.....XXXXXXXX.     ",
"     ..XXXXXXX...XXXXXXX..      ",
"       .XXXXXXXXXXXXXXX.        ",
"        ...XXXXXXXXX...         ",
"           .........            "};
Tix_DefinePixmap(Et_Interp, Tk_GetUid("warning"), warning_xpm);
}
