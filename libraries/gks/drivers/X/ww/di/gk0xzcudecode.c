/* copyright (c) Mark M Martin. RAL. 1986 */
#include "di.h"
/*
 * convert cursor chars into structure.
 * lp style is 0 to 4 decimal numbers separated by spaces or commas
 * followed by newline then the usual null terminated lp pattern.
 */
PUBLIC cursor *gk0xcudecode(pattern,style) char *pattern; int style;{
	cursor *cp;
	cp = STRUCTMALLOC(cursor);
	if(style==ENWWSTYLE){
		cp->c_rop = *pattern++;
		cp->c_xhot = *pattern++;	/* signed */
		cp->c_yhot = *pattern++;	/* signed */
		cp->c_xoffset = *pattern++;	/* signed */
		cp->c_yoffset = *pattern++;	/* signed */
	}else if(style==ENLPSTYLE){
		int val[4];
		int start,stop,error,num,i;
		char *sign;	/* position of '-' sign */
		val[0] = val[1] = val[2] = val[3] = 0;
		error = stop = FALSE;
		i = 0;
		while(!stop){
			sign = 0;
			start = FALSE;
			while(!start && !stop){
				switch(*pattern){
				case ' ':
				case ',':
					break;
				case '-':if(sign)error++, stop++;
					sign = pattern;
					break;
				case '\n':stop++;
					break;
				case '\0':error++, stop++;
					break;
				default:if(*pattern<'0' || *pattern>'9')error++, stop++;
					start++;
					break;
				}
				if(!start)pattern++;
			}
			if(sign && sign!=pattern-1)error++, stop++;
			if(!stop)
			if(i==4)error++, stop++;
			else{
				num = 0;
				while(start){
					num = ((*pattern++-'0')&0377)+num*10;

					start = (*pattern>='0' && *pattern <='9');
				}
				val[i++] = sign?-num:num;
			}
		}
		if(error){
			gk0xcufree(cp);
			return(0);
		}
		cp->c_rop = WWXOR;
		cp->c_xhot = val[0];
		cp->c_yhot = val[1];
		cp->c_xoffset = val[2];
		cp->c_yoffset = val[3];
	}else gk0xwwpanic("unknown style to gk0xcudecode");
	cp->c_bm = gk0xbmdecode(pattern,style);
	if(cp->c_bm==0){
		gk0xcufree(cp);
		cp = 0;
	}
	return(cp);
}
#ifdef FORTINTER
FORTINTER int wcudec_(pattern,style,patlen)char *pattern; int *style,patlen;{
	 return(int)cudecode(pattern,*style);
}
#endif FORTINTER
