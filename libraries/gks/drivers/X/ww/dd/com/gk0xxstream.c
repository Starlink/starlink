/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
/*
 * bit order. source lp style: left	01234567 89ABCDEF G...
 * on perq:				89ABCDEF 01234567 G...
 * on whitechapel			76543210 FEDCBA98 G...
 * on sun:				01234567 89ABCDEF G...
 */
/*
 * set or get 1 pixel encoded as a char ' ' or 'x'.
 * flush the line we are putting if the char is \n, but allow for other
 * sort of usage where each line is fully bitstreamed and auto newlines are taken.
 * return true if was last bit in raster line
 * be careful not to read ahead a byte beyond the end of raster.
 */
static int put,bitwidth,initialise;	/* set by streamraster */
LOCALPUBLIC int gk0xbitstream(charp)char *charp;{
	static unsigned char mask;
	static char byte;
	static int x;
	static char space = ' ';
	if(put){
		if(initialise){
			initialise = FALSE;
			x = 0;
			byte = 0;
			mask = 0200;
		}
		if(*charp=='\n'){
			while(x<bitwidth)gk0xbitstream(&space);
			x = 0;
		}else{
			if(x>=bitwidth)x = 0;	/* no newline at end of previous line */
			if(*charp!=' ')byte |= mask;
			mask >>= 1;
			if(++x>=bitwidth || mask==0){
				gk0xbytestream(&byte);
				byte = 0;
				mask = 0200;
			}
		}
	}else{
		if(initialise){
			initialise = FALSE;
			x = 0;
			gk0xbytestream(&byte);
			mask = 0200;
		}else if(x>=bitwidth || mask==0){
			gk0xbytestream(&byte);
			if(x>=bitwidth)x = 0;
			mask = 0200;
		}
		*charp = (byte&mask)?'x':' ';
		x++;
		mask >>= 1;
	}
	return(x==0);
}
/*
 * allow serial access to a raster bitmap for bmencode etc.
 * Initially call streamraster, saying TRUE if putting to raster bitmap,
 * then call bytestream for each byte sequentially,
 * or bitstream for each bit, either being in canonical ww ordering (lp left to right)
 */
static unsigned char *mapmem = 0,*mp,*mapend;
static box mapbox;
static bitmap *mapbm;
static int maxy;
LOCALPUBLIC void gk0xstreamraster(bm,toput)bitmap *bm;{
	int bytewidth;
	initialise = TRUE;
	mapbm = bm;
	bitwidth = WIDTH(bm->bm_box);	/* for bitstream() */
	maxy = bm->bm_box.b_bottom;
	bytewidth = (bitwidth+7)/8;
	put = toput;
	if(mapmem!=0)free((char*)mapmem);
	mapmem = (unsigned char *)malloc(bytewidth);
	if(mapmem==0)gk0xwwpanic("streamraster: no space");
	mp = mapmem;
	mapend = mapmem+bytewidth;
	mapbox = gk0xboxbuild(0,0,bitwidth-1,0);
}
LOCALPUBLIC int gk0xbytestream(charp)char *charp;{
	if(mapbox.b_top>maxy)gk0xwwpanic("streaming off end of bitmap");
	if(put)*mp++ = *charp;
	else{
		if(mp==mapmem)gk0xwwcharmap(mapbm,mapbox,mapmem,WWMAPGET);
		*charp = *mp++;
	}
	if(mp>=mapend){
		if(put)gk0xwwcharmap(mapbm,mapbox,mapmem,WWMAPPUT);
		mp = mapmem;
		++mapbox.b_top, ++mapbox.b_bottom;
	}
	return(mp==mapmem);
}
