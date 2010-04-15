/* copyright (c) Mark M Martin. RAL. 1987 */
#include "dd.h"
#include "map.h"
PRIVATE void gk0xdomap();
/*
 * move left until in range 0 to width-1. compensate right too.
 * dunno what modulo neg number is.
 */
PRIVATE void remod(l,r,w)int *l,*r;{
	*r -= *l;
	*l = (*l>=0)?(*l % w):(w-(-(*l) % w));
	*r += *l;
}
/*
 * copy (to or from) a box within a bitmap to a char array of memory,
 * ostensibly 2D (width(b)+7)/8 bytes wide.
 * b (inclusive) must be in bitmap or WWMAPTILE. can be any bit aligned.
 */
PUBLIC void gk0xwwcharmap(bm,b,map,flags)bitmap *bm;box b;unsigned char *map;int flags;{
	if(flags&~(WWMAPPUT|WWMAPGET|WWMAPTILE))gk0xwwpanic("wwcharmap: bad flags");
	if(flags&WWMAPTILE){
		if(flags&WWMAPPUT)gk0xwwpanic("wwcharmap: cannot PUT and TILE");
		/* move start within bitmap */
		remod(&b.b_left,&b.b_right,WIDTH(bm->bm_box));
		remod(&b.b_top,&b.b_bottom,HEIGHT(bm->bm_box));
	}else if(b.b_left<0 || b.b_right>bm->bm_box.b_right || b.b_top<0 || b.b_bottom>bm->bm_box.b_bottom)
		gk0xwwpanic("wwcharmap: box outside bitmap");
	gk0xdomap(bm,b,map,flags);
}
/*
 * copy a bitmap to a flat 2D char array.
 * take each map byte, slide into a register 8 bits at a time,
 * slide out 8 bits at a time to bitmap (*next).
 * Preload with destination so partial byte at start done ok,
 * fill last byte removed similarly.
 * Simultaneously, copy bytes the other way in another register, simpler
 * cos destination map guaranteed starts at bit 0.
 * If tiling, start is within bitmap.
 */
PRIVATE void gk0xdomap(bm,b,map,flags)bitmap *bm;box b;unsigned char *map;int flags;{
	int x,y,sl,pl,n,shift,bitwidth;
	unsigned char *line,*next;
	unsigned int putreg,getreg;
	struct layout l;

	l = *gk0xmaptomem(bm,b,flags);
	x = b.b_left, y = b.b_top;
	line = next = l.l_base +
		(y+l.l_yoffset)*l.l_bytestride+
		((x+l.l_xoffset)>>3);
	shift = (x+l.l_xoffset)&7;
	bitwidth = WIDTH(b);
	if((flags&(WWMAPGET|WWMAPTILE)) == (WWMAPGET|WWMAPTILE)){
		int gl,firstbits,lastbits;
		unsigned char *end;

		firstbits = 8-(l.l_xoffset&7);	/* valid bits in 1st byte scanline*/
		if(firstbits>l.l_width)firstbits = l.l_width;
		lastbits = (l.l_width+l.l_xoffset)&7;	/* valid bits last byte scanline*/
		/* invalid and not used if 1st byte==last byte */
		while(y<=b.b_bottom){
			line = l.l_base+
				(y % HEIGHT(bm->bm_box) + l.l_yoffset)*l.l_bytestride;
			end = line + ((l.l_width-1+l.l_xoffset)>>3);	/* last byte in scanline */
			next = line + ((x+l.l_xoffset)>>3);
			getreg = GETBYTE(next);
			gl = 8-shift;	/* valid bits in getreg ending least sig bit */
			if(gl>l.l_width-x){	/* bits to end of scanline */
				getreg >>= (gl-(l.l_width-x));
				gl = l.l_width-x;
			}
			for(sl = bitwidth;sl>0;sl -= 8){
				n = sl>8?8:sl;
				while(gl<n){
					if(next==end){
						next = line + (l.l_xoffset>>3);
						getreg = (getreg<<firstbits) | (GETBYTE(next)&(0377>>(8-firstbits)));
						gl += firstbits;
					}else{
						next++;
						getreg = (getreg<<8) | GETBYTE(next);
						if(next==end){
							getreg >>= (8-lastbits);
							gl += lastbits;
						}else gl += 8;
					}
				}
				if(gl>=8)*map++ = (getreg>>(gl-8))&(0377<<(8-n));
				else *map++ = getreg<<(8-gl);
			}
			y++;
		}
	}else if((flags==WWMAPPUT || flags==WWMAPGET) && bitwidth%8==0 && shift==0){
		/* really fast */
		while(y++<=b.b_bottom){
			for(sl = bitwidth/8;sl>0;sl--){
				if(flags&WWMAPGET)*map = GETBYTE(next);
				else DEREF(next) = BITREV(*map);
				next++, map++;
			}
			next = line += l.l_bytestride;
		}
	}else
	while(y++<=b.b_bottom){
		if(flags&WWMAPPUT){
			if(shift==0)putreg = pl = 0;
			else{
				putreg = GETBYTE(next)>>(8-shift);
				pl = shift;
			}
		}
		getreg = GETBYTE(next);
		for(sl = bitwidth;sl>0;sl -= 8){
			if(flags&WWMAPGET){
				getreg = (getreg<<8) | GETBYTE(next+1);
				*map = getreg>>(8-shift);
				if(sl<8)*map &= (0377<<(8-sl));	/* clear unasked for bits */
			}
			if(flags&WWMAPPUT){
				n = sl>8?8:sl;
				putreg = (putreg<<n) | (*map>>(8-n));
				pl += n;
				if(pl<8){	/* fill last byte */
					putreg = (putreg<<(8-pl)) | (GETBYTE(next)&(0377>>pl));
					pl = 8;
				}
				pl -= 8;
				DEREF(next) = BITREV(putreg>>pl);
			}
			next++, map++;
		}
		if((flags&WWMAPPUT) && pl>0){	/* unfinished last byte (pl is <8) */
			if(pl<8)putreg = (putreg<<(8-pl)) | (GETBYTE(next)&(0377>>pl));
			DEREF(next) = BITREV(putreg);
		}
		next = line += l.l_bytestride;
	}
	gk0xunmapfrommem();
	if(flags&WWMAPPUT)NOTESCREEN(bm,b);
}
