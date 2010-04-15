/* copyright (c) Mark M Martin. RAL. 1986 */
#include "di.h"
/*
 * convert unaligned bytes into an integer
 */
LOCALPUBLIC int gk0xbtoi(cp,len)char *cp;{
	int val;
	val = 0;
	while(len-->0)val = (val<<8)|chartoint(*cp++);
	return(val);
}
PUBLIC bitmap *gk0xbmdecode(pattern,style) char *pattern; int style;{
	bitmap *bm;
	if(style==ENWWSTYLE){
		int lines,wid;
#ifndef WWFORSUN
		char *end;
#endif WWFORSUN
		if(pattern==NULLPTR(char))return(NULLPTR(bitmap));
		lines = gk0xbtoi(pattern,2);
		wid = gk0xbtoi(pattern+2,2);
		bm = gk0xbmget(wid,lines);
		pattern += 4;
#ifdef WWFORSUN
		gk0xwwcharmap(bm,bm->bm_box,pattern,WWMAPPUT);
#else WWFORSUN
		end = pattern+(wid+7)/8*lines;
		gk0xstreamraster(bm,TRUE);	/* put to raster */
		while(pattern<end)
			gk0xbytestream(pattern++);
#endif WWFORSUN
		return(bm);
	}
	if(style==ENLPSTYLE){
		char *cp,*start;
		int lines,maxwid;
		if(pattern==NULLPTR(char))return(NULLPTR(bitmap));
		cp = pattern-1;
		start = pattern;
		lines = maxwid = 0;
		while(*++cp!='\0')
			if(*cp=='\n'){
				if(cp-start>maxwid)maxwid = cp-start;
				start = cp+1;
				lines++;
			}
		if(start!=cp){	/* doesnt end with newline */
			if(cp-start>maxwid)maxwid = cp-start;
			lines++;
		}
		bm = gk0xbmget(maxwid,lines);
		cp = pattern;
		gk0xstreamraster(bm,TRUE);	/* put to raster */
		while(*cp!='\0')
			gk0xbitstream(cp++);
		return(bm);
	}
	if(style==ENRUNSTYLE){
		int lines,wid,len;
		char *end;
		if(pattern==NULLPTR(char))return(NULLPTR(bitmap));
		lines = gk0xbtoi(pattern,2);
		wid = gk0xbtoi(pattern+2,2);
		len = gk0xbtoi(pattern+4,2);
		bm = gk0xbmget(wid,lines);
		pattern += 6;
		end = pattern+len;
		gk0xstreamraster(bm,TRUE);	/* put to raster */
		while(pattern<end){	/* invalid codes: 0, 0200, 0201 */
			len = chartoint(*pattern++);
			if(len & 0200){
				len &= 0177;
				while(len-->0)gk0xbytestream(pattern);
				pattern++;
			}else while(len-->0)gk0xbytestream(pattern++);
		}
		return(bm);
	}
	wwfail("unknown style to gk0xbmdecode",NULLPTR(bitmap));
}
#ifdef FORTINTER
FORTINTER int wbmdco_(pattern,length,style,width)char *pattern; int *length,*style,width;{	/* char *pattern; int length; */
	int bm;
	bm =(int)bmdecode(pattern,*style);
	return bm;
}
#ifdef OLDFORTRAN
FORTINTER int wbmdec_(pattern,length,width)char *pattern;int *length,width;{
 	char *p = pattern,*save = malloc((unsigned)*length);
 	int i,bm;

 	for(i = 0;i < *length;i++){
  		p += width;
		save[i] = p[-1];
 		p[-1] = '\n';
 	}
 	p[-1] = '\0';
 	bm =(int)bmdecode(pattern,ENLPSTYLE);
 	for(i = 0;i < *length;i++){
 	 	pattern += width;
 	 	pattern[-1] = save[i];
 	}
 	free(save);
 	return(bm);
}
#endif OLDFORTRAN
#endif FORTINTER
