/* copyright (c) Mark M Martin. RAL. 1986 */
#include "di.h"
/*
 * grey shade generator
 * This uses Crispin Goswell's PS algorithm for picking a grey texture
 * by using a matrix of values and then using the intensity as a threshold.
 * Code Copyright (c) Chris Crampton @ RAL, 1987
 * adopted and adapted for ww 17 mar 1988. mmm.
 * Caller should free bitmap when done with.
 */
/*
 * clever matrix of values: if the value at a given point is
 * <= to the desired intensity (0-64) then that bit in the (8x8 bit)
 * texture should be "on". The values are arranged to provide
 * smooth, uniform shades (hopefully).
 */
#define SHWIDTH		8	/* if >8 need to add to code below!!!! */
#define SHHEIGHT	8
#define SHADES		65	/* SHWIDTH*SHHEIGHT+1 */
#define SHBYTES		8	/* SHHEIGHT*(SHWIDTH+7)/8 */
static unsigned char halftone [SHHEIGHT][SHWIDTH] = {
	{43, 11, 35,  3, 41,  9, 33,  1},
	{27, 59, 19, 51, 25, 57, 17, 49},
	{39,  7, 47, 15, 37,  5, 45, 13},
	{23, 55, 31, 63, 21, 53, 29, 61},
	{42, 10, 34,  2, 44, 12, 36,  4},
	{26, 58, 18, 50, 28, 60, 20, 52},
	{38,  6, 46, 14, 40,  8, 48, 16},
	{22, 54, 30, 62, 24, 56, 32, 64}
};
PUBLIC bitmap *gk0xxxgrey(intensity,cache)unsigned int intensity,cache;{
	static unsigned char shade[SHADES][SHBYTES];
	static bitmap *bmcache[SHADES];
	static int used[SHADES];
	static int numcached = 0;
	static int latest = 0;
	unsigned int which;
	bitmap *bm;

	if(latest==0){	/* not already computed */
		register unsigned char *pat;
		unsigned char threshold;

		pat = &shade[0][0];
		for(threshold = 0;threshold<SHADES;threshold++){
			unsigned char *tonep;
			register int i;

			tonep = &halftone[0][0];
			for(i = 0;i<SHHEIGHT;i++){	/* each scan line */
				register int j;
				for(j = 0; j<SHWIDTH; j++)	/* each bit */
				if (*tonep++ <= threshold){
					*pat |= 1<<j;	/* switch the bit on */
					/* if(j%8==7 && j!=SHWIDTH-1)pat++; when SHWIDTH>8!!! */
				}
				pat++;
			}
		}
		latest = 1;
	}
	if(intensity>100)intensity = 100;
	which = (intensity*(SHADES-1))/100;
	if(bmcache[which]!=0){
		if(cache==0){
			bm = gk0xbmget(SHWIDTH,SHHEIGHT);
			gk0xbmxcopy(bmcache[which],bmcache[which]->bm_box,
				bm,bm->bm_box,WWCOPY);
		}else bm = bmcache[which];
		used[which] = ++latest;
	}else{
		if(cache!=0 && cache<=numcached){
			int min,mini,i;
			min = latest+1;
			mini = -1;
			for(i = 0;i<SHADES;i++)
			if(used[i]!=0 && used[i]<=min)min = used[i], mini = i;
			if(mini==-1)bm = gk0xbmget(SHWIDTH,SHHEIGHT);
			else{
				bm = bmcache[mini];
				bmcache[mini] = 0;
				used[mini] = 0;
				numcached--;
			}
		}else bm = gk0xbmget(SHWIDTH,SHHEIGHT);
		gk0xwwcharmap(bm,bm->bm_box,shade[which],WWMAPPUT);
		if(cache!=0){
			bmcache[which] = bm;
			used[which] = ++latest;
			numcached++;
		}
	}
	return(bm);
}
