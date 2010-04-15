/* copyright (c) Mark M Martin. RAL. 1986 */
#include "dd.h"
#include <signal.h>
/*
 * do a malloc and clear the space
 */
LOCALPUBLIC char *gk0xclrmalloc(u)unsigned int u;{
	char *calloc();
	return(calloc(u,1));
}
/*
 * remember user's settings for shutting down.
 */
PRIVATE void gk0xsetcolour(colouring,number){
	static int *colourptr;
	number = abs(number)%256;
	if(colouring<4){
		*colourptr |= number<<(8*(colouring-1));
		return;
	}
	if(jdd->jd_defcolsize<number+1){
		int *cp;
		if(jdd->jd_defaultcolour==0)jdd->jd_defaultcolour =
			(int*)malloc((number+1)*sizeof(int));
		else jdd->jd_defaultcolour =
			(int*)realloc((char*)jdd->jd_defaultcolour,(number+1)*sizeof(int));
		cp = jdd->jd_defaultcolour+jdd->jd_defcolsize;	/* old end */
		jdd->jd_defcolsize = number+1;
		while(cp<jdd->jd_defaultcolour+jdd->jd_defcolsize)
			*cp++ = 0;	/* clear all up to new end */
	}
	colourptr = jdd->jd_defaultcolour+number;
	*colourptr = ENTRYSET;	/* show something there */
}
#define ARGISNUM	1
#define ANYARG		2
#define ARGINIT		(-1)
static char *argstr;	/* these two hold returned values from nextarg */
static int argval;
PRIVATE int getarg(init)char *init;{
	static char *cp;
	static char save;
	if(init){
		cp = init;
		argstr = 0;
		return 0;
	}
	if(argstr==cp)return FALSE;	/* end of input */
	if(argstr!=0)*cp = save;	/* restore what was under null */
	while(*cp==' ')cp++;
	argstr = cp;
	while(*cp!='\0' && *cp!=' ')cp++;	/* go to end of arg */
	if(argstr==cp)
		return FALSE;	/* end of input */
	save = *cp;
	*cp = '\0';
	return TRUE;
}
PRIVATE int nextarg(str,wanted)char *str;int wanted;{
	static char *option;
	static int numericargs;
	if(str!=0)
	if(wanted==ARGINIT){
		getarg(str);	/* init */
		return 0;
	}else{
		numericargs = wanted;
		option = str;
		return(strcmp(argstr,str)==0);
	}
	if(wanted==ARGISNUM){
		if(getarg(0)){
			char *dp;
			dp = argstr;
			if(*dp=='-')dp++;
			while(*dp>='0' && *dp<='9')dp++;
				/* ignore check for '-' alone */
			if(!(*dp!='\0' && *dp!=' ')){
				argval = atoi(argstr);
				return TRUE;
			}
		}{
			char msg[200];
			sprintf(msg,"WWOPTIONS=%s needs %d numbers",option,numericargs);
			wwpanic(msg);
		}
	}
	return(getarg(0));
}
DDPUBLIC void gk0xparsewwoptions(argstring)char *argstring;{
	extern char *getenv();
	int i;

	if(argstring==0)argstring = getenv("WWOPTIONS");
	nextarg(argstring,ARGINIT);
	if(argstring!=0)
	while(nextarg(0,ANYARG)){
		if(nextarg("-buf",0))
			dd->d_flags |= WWNOBUFFER;
		else if(nextarg("-noise",0))
			dd->d_flags |= WWNONOISE;
		else if(nextarg("-font",0) && nextarg(0,ANYARG)){
			jdd->jd_defaultfont = (char*)malloc(strlen(argstr)+1);
			strcpy(jdd->jd_defaultfont,argstr);
		}else if(nextarg("-colour",4))
			for(i = 4;i>0 && nextarg(0,ARGISNUM);)
				gk0xsetcolour(i--,argval);
		else if(nextarg("-forepix",1) && nextarg(0,ARGISNUM))
			dd->d_fore = argval;
		else if(nextarg("-backpix",1) && nextarg(0,ARGISNUM))
			dd->d_back = argval;
		else if(nextarg("-colourmap",0) && nextarg(0,ANYARG)){
			jdd->jd_mapname = (char*)malloc(strlen(argstr)+1);
			strcpy(jdd->jd_mapname,argstr);
		}else if(nextarg("-map",0) && nextarg(0,ANYARG))
			for(i = 0;i<3;)
			switch(*argstr++){
			case 'l':dd->d_buttonmap[i++] = ITEMBUTTON;
				break;
			case 'm':dd->d_buttonmap[i++] = MENUBUTTON;
				break;
			case 'r':dd->d_buttonmap[i++] = SHOWBUTTON;
				break;
			default:/*i = 9;*/
				wwpanic("WWOPTIONS='-map xxx' not a permutation of lmr");
			}
		else if(nextarg("-box",4)){
			jdd->jd_flags |= JDDEFAULTBOX;
			for(i = 4;i>0 && nextarg(0,ARGISNUM);)
			switch(i--){
			case 4:jdd->jd_defaultbox.b_left = argval;break;
			case 3:jdd->jd_defaultbox.b_top = argval;break;
			case 2:jdd->jd_defaultbox.b_right = argval;break;
			case 1:jdd->jd_defaultbox.b_bottom = argval;break;
			}
		}else{
			char msg[200];
			sprintf(msg,"unknown arg in WWOPTIONS='%s'",argstr);
			wwpanic(msg);
		}
	}
}
LOCALPUBLIC void gk0xworry(s)char *s;{
	if(dd->d_flags & PRINTERR)
		gk0xwwpanic(s);
	wwerror(s);
}
PUBLIC void gk0xwwnoise(){
	gk0xwwxnoise(ddwin);
}
