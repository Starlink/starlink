/* VT_GENC.C    Screen, image, virtual image transformations
-
  Includes:-
-
 VTC_TIS     Translate image to screen coords
 VTC_TSI     Translate screen to image coords
 VTC_TSV     Translate screen to virtual coords
 VTC_TVS     Translate virtual to screen coords
 VTC_TIV     Translate image to virtual coords
 VTC_TVI     Translate virtual to image coords
*/


#define Bool int
#include "f77.h"
#include "ds_gen.h"

extern int DSNXS, DSNYS ;
extern int DSCOMFX, DSCOMFY, DSIXS, DSIYS, DSSNX, DSSNY ;
extern int DSZM, DSZPX, DSZPY;

char TEXTAA[200];



/* ***************************************************************
  VTC_TIS -- Translate image cords to screen coords

    alan penny           ral                       1990-02-01
*/

vtc_tis ( kxi, kyi, kxo, kyo )

int    kxi;		/* i: Image X position */
int    kyi;		/* i: Image Y position */
int    *kxo;		/* o: Screen X position at b.l.h. of image pixel */
int    *kyo;		/* o: Screen Y position at b.l.h. of image pixel */
/* C-- */
{
      float x, y, xa, ya;
/* Cbegin */


      DSIXS = F77_NAMED_COMMON(ds_gen).dsixs;
      DSIYS = F77_NAMED_COMMON(ds_gen).dsiys;
      DSNXS = F77_NAMED_COMMON(ds_gen).dsnxs;
      DSNYS = F77_NAMED_COMMON(ds_gen).dsnys;
      DSZPX = F77_NAMED_COMMON(ds_gen).dszpx;
      DSZPY = F77_NAMED_COMMON(ds_gen).dszpy;
      DSCOMFX = F77_NAMED_COMMON(ds_gen).dscomfx;
      DSCOMFY = F77_NAMED_COMMON(ds_gen).dscomfy;
      DSZM = F77_NAMED_COMMON(ds_gen).dszm;

      x = (float) DSIXS + (float) (kxi-DSNXS) / (float) DSCOMFX;	/*Convert image to virtual */
      y = (float) DSIYS + (float) (kyi-DSNYS) / (float) DSCOMFY;

      xa = 1.0 + (float) DSZM * (x-(float) DSZPX);			/*Convert virtual to screen */
      ya = 1.0 + (float) DSZM * (y-(float) DSZPY);

      *kxo = (int) xa;
      *kyo = (int) ya;


/*
 (void) sprintf ( TEXTAA, " C TIS i: %d %d o: %d %d ", kxi, kyi, *kxo, *kyo );
 (void) c_printo ( TEXTAA );
*/


}


/* ***************************************************************
VTC_TSI -- Translate screen coords to image coords

    alan penny           ral                       1990-02-01
*/

vtc_tsi ( kxi, kyi, kxo, kyo )

int   kxi;		/* i: Screen X position */
int   kyi;		/* i: Screen Y position */
int   *kxo;		/* o: Image X position */
int   *kyo;		/* o: Image Y position */
/* C-- */
{
      float x, y, xa, ya;
/* Cbegin */


      DSIXS = F77_NAMED_COMMON(ds_gen).dsixs;
      DSIYS = F77_NAMED_COMMON(ds_gen).dsiys;
      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;
      DSNXS = F77_NAMED_COMMON(ds_gen).dsnxs;
      DSNYS = F77_NAMED_COMMON(ds_gen).dsnys;
      DSZPX = F77_NAMED_COMMON(ds_gen).dszpx;
      DSZPY = F77_NAMED_COMMON(ds_gen).dszpy;
      DSCOMFX = F77_NAMED_COMMON(ds_gen).dscomfx;
      DSCOMFY = F77_NAMED_COMMON(ds_gen).dscomfy;
      DSZM = F77_NAMED_COMMON(ds_gen).dszm;

      x = (float) DSZPX + (float) (kxi-1) / (float) DSZM ;				/* Screen to virtual */
      y = (float) DSZPY + (float) (kyi-1) / (float) DSZM ;

      xa = (float) DSNXS + ( x - (float) DSIXS ) * (float) DSCOMFX ;	/* Virtual to image */
      ya = (float) DSNYS + ( y - (float) DSIYS ) * (float) DSCOMFY ;

      *kxo = (int) xa;
      *kyo = (int) ya;


/*
 (void) sprintf ( TEXTAA, " C TSI i: %d %d o: %d %d ", kxi, kyi, *kxo, *kyo );
 (void) c_printo ( TEXTAA );
*/


}


/* ***************************************************************
   VTC_TSV -- Translate screen to virtual screen coords

    alan penny           ral                       1990-02-01
*/

vtc_tsv ( kxi, kyi, kxo, kyo )

int    kxi;		/* i: Screen X position */
int    kyi;		/* i: Screen Y position */
int    *kxo;		/* o: Virtual X position */
int    *kyo;		/* o: Virtual Y position */
/* C-- */
{
      float x, y;
/* Cbegin */


      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;
      DSZPX = F77_NAMED_COMMON(ds_gen).dszpx;
      DSZPY = F77_NAMED_COMMON(ds_gen).dszpy;
      DSZM = F77_NAMED_COMMON(ds_gen).dszm;

      x = (float) DSZPX + (float) (kxi-1) / (float) DSZM ;
      y = (float) DSZPY + (float) (kyi-1) / (float) DSZM ;

      *kxo = (int) x;
      *kyo = (int) y;

/*
 (void) sprintf ( TEXTAA, " C TSV i: %d %d o: %d %d ", kxi, kyi, *kxo, *kyo );
 (void) c_printo ( TEXTAA );
*/


}


/* ***************************************************************
   VTC_TVS -- Translate virtual to screen coords

    alan penny           ral                       1990-02-01
*/

vtc_tvs ( kxi, kyi, kxo, kyo )

int    kxi;		/* i: Virtual X position */
int    kyi;		/* i: Virtual Y position */
int    *kxo;		/* o: Screen X position at t.l.h. of image pixel */
int    *kyo;		/* o: Screen Y position at t.l.h. of image pixel */
/* C-- */
{
      float x, y;
/* Cbegin */


      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;
      DSZPX = F77_NAMED_COMMON(ds_gen).dszpx;
      DSZPY = F77_NAMED_COMMON(ds_gen).dszpy;
      DSZM = F77_NAMED_COMMON(ds_gen).dszm;

      x = 1.0 + (float) DSZM * (float) (kxi-DSZPX) ;
      y = 1.0 + (float) DSZM * (float) (kyi-DSZPY);

      *kxo = (int) x;
      *kyo = (int) y;

/*
 (void) sprintf ( TEXTAA, " C TVS i: %d %d o: %d %d ", kxi, kyi, *kxo, *kyo );
 (void) c_printo ( TEXTAA );
*/


}


/* ***************************************************************
   VTC_TVI -- Covert virtual coordinates to image ones

    alan penny           ral                       1993 March
*/

vtc_tvi ( kxi, kyi, kxo, kyo )

int    kxi;		/* i: Virtual X position */
int    kyi;		/* i: Virtual Y position */
int   *kxo;		/* o: Image X position */
int   *kyo;		/* o: Image Y position */
/* C-- */
{
  float x, y;
/* Cbegin */


      DSIXS = F77_NAMED_COMMON(ds_gen).dsixs;
      DSIYS = F77_NAMED_COMMON(ds_gen).dsiys;
      DSNXS = F77_NAMED_COMMON(ds_gen).dsnxs;
      DSNYS = F77_NAMED_COMMON(ds_gen).dsnys;
      DSCOMFX = F77_NAMED_COMMON(ds_gen).dscomfx;
      DSCOMFY = F77_NAMED_COMMON(ds_gen).dscomfy;

       x = (float) DSNXS + (float) ( kxi - DSIXS ) * (float) DSCOMFX ;
       y = (float) DSNYS + (float) ( kyi - DSIYS ) * (float) DSCOMFY ;

       *kxo = (int) x;
       *kyo = (int) y;

/*
 (void) sprintf ( TEXTAA, " C TVI i: %d %d o: %d %d ", kxi, kyi, *kxo, *kyo );
 (void) c_printo ( TEXTAA );
*/

}


/* ***************************************************************
   VTC_TIV -- Covert image coordinates to virtual ones

    alan penny           ral                       1993 March
*/

vtc_tiv ( kxi, kyi, kxo, kyo )

int    kxi;		/* i: Image X position */
int    kyi;		/* i: Image Y position */
int    *kxo;		/* o: Virtual X position */
int    *kyo;		/* o: Virtual Y position */
/* C-- */
{
float x, y;
/* Cbegin */


      DSIXS = F77_NAMED_COMMON(ds_gen).dsixs;
      DSIYS = F77_NAMED_COMMON(ds_gen).dsiys;
      DSNXS = F77_NAMED_COMMON(ds_gen).dsnxs;
      DSNYS = F77_NAMED_COMMON(ds_gen).dsnys;
      DSCOMFX = F77_NAMED_COMMON(ds_gen).dscomfx;
      DSCOMFY = F77_NAMED_COMMON(ds_gen).dscomfy;

       x = (float) DSIXS + (float) ( kxi - DSNXS ) / (float) DSCOMFX ;
       y = (float) DSIYS + (float) ( kyi - DSNYS ) / (float) DSCOMFY ;

       *kxo = (int) x;
       *kyo = (int) y;

/*
 (void) sprintf ( TEXTAA, " C TIV i: %d %d o: %d %d ", kxi, kyi, *kxo, *kyo );
 (void) c_printo ( TEXTAA );
*/

}
