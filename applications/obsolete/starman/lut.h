/*
 LUT.H

  Color tables copyright Smithsonian Astrophysical Observatory 1989 */

#define LVX NUMDCOL

      int	LUTC_STORED; 		/* Number of stored LUTs */
      int	LUTC_ENDS;		/* LUT end types (1=black/white;2=wh/bl;3=col/col;4=wrap */
      float     LUTC_VAL[150][3];	/* RGB arrays */
      int       LUTC_NUM;		/* Present LUT number */
      float     LUTC_SC; 		/* LUT scale */
      float     LUTC_ZE;		/* LUT zero */

      Bool      LUTC_FLIPPED;		/* Is LUT flipped? */
