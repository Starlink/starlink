/*
 				fitscat_defs.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	The LDAC Tools
*
*	Author:		E.BERTIN, DeNIS/LDAC
*
*	Contents:	Simplified version of the LDACTools: internal defs
*
*	Last modify:	23/01/97
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*------------------------ what, who, when and where ------------------------*/

#define		BANNER		"LDACTools"
#define		VERSION		"1.1 (Jan 23 97)"
#define		COPYRIGHT	"Emmanuel BERTIN (bertin@iap.fr)"
#define		INSTITUTE	"IAP/Leiden"


/*----------------------------- Internal constants --------------------------*/

#define	OUTPUT		stderr	/* where all msgs are sent */
#define	DATA_BUFSIZE	(1024*1024)

#ifndef PI
#define	PI		3.14159265359	/* never met before? */
#endif

/* NOTES:
We must have:		MAXCHARS >= 16
			DATA_BUFSIZE >= 2 although DATA_BUFSIZE >= 100000
					  is better!!
*/

/*------------ Set defines according to machine's specificities -------------*/

#ifdef DEC_ALPHA
#define	BSWAP
#define	INT64
#endif

#ifdef HP_UX
#define	_HPUX_SOURCE
#endif

#ifdef PC_LINUX
#define	BSWAP
#endif

#ifdef SUN_ULTRASPARC
#define	INT64
#endif

/*--------------------- in case of missing constants ------------------------*/

#ifndef         SEEK_SET
#define         SEEK_SET        0
#endif
#ifndef         SEEK_CUR
#define         SEEK_CUR        1
#endif

#ifndef	EXIT_SUCCESS
#define	EXIT_SUCCESS		0
#endif
#ifndef	EXIT_FAILURE
#define	EXIT_FAILURE		-1
#endif

/*--------------------------------- typedefs --------------------------------*/
typedef	unsigned char	BYTE;			/* a byte */
typedef	int		LONG;			/* for DEC-Alpha... */
	
/*------------------------------- Other Macros -----------------------------*/

#define QFREAD(ptr, size, file, fname) \
		{if (fread(ptr, (size_t)(size), (size_t)1, file)!=1) \
		  error(EXIT_FAILURE, "*Error* while reading ", fname);;}

#define QFWRITE(ptr, size, file, fname) \
		{if (fwrite(ptr, (size_t)(size), (size_t)1, file)!=1) \
		  error(EXIT_FAILURE, "*Error* while writing ", fname);;}

#define	QFSEEK(file, offset, pos, fname) \
		{if (fseek(file, (long)(offset), pos)) \
		  error(EXIT_FAILURE,"*Error*: File positioning failed in ", \
			fname);;}

#define	QFTELL(pos, file, fname) \
		{if ((pos=ftell(file))==-1) \
		  error(EXIT_FAILURE,"*Error*: File position unknown in ", \
			fname);;}


#define	QFREE(x)	{free(x); x = NULL;}

#define	QCALLOC(ptr, typ, nel) \
		{if (!(ptr = (typ *)calloc((size_t)(nel),sizeof(typ)))) \
		  error(EXIT_FAILURE, "Not enough memory for ", \
			#ptr " (" #nel " elements) !");;}

#define	QMALLOC(ptr, typ, nel) \
		{if (!(ptr = (typ *)malloc((size_t)(nel)*sizeof(typ)))) \
		  error(EXIT_FAILURE, "Not enough memory for ", \
			#ptr " (" #nel " elements) !");;}

#define	QMEMCPY(ptrin, ptrout, typ, nel) \
		{if (!(ptrout = (typ *)malloc((size_t)(nel)*sizeof(typ)))) \
		  error(EXIT_FAILURE, "Not enough memory for ", \
			#ptrout " (" #nel " elements) !"); \
		memcpy(ptrout, ptrin, (size_t)(nel)*sizeof(typ));}

#define	QREALLOC(ptr, typ, nel) \
		{if (!(ptr = (typ *)realloc(ptr, (size_t)(nel)*sizeof(typ)))) \
		  error(EXIT_FAILURE, "Not enough memory for ", \
			#ptr " (" #nel " elements) !");;}

#define	RINT(x)	(int)(floor(x+0.5))


#define	QPRINTF		if (qflag) fprintf

#define	QFPRINTF(w,x)	{if (qflag) \
				fprintf(w, "\33[1M> %s\n\33[1A",x);;}


#define	QGETKEY(tab, key, keyname, dest) \
	{if (!(key = name_to_key(tab, keyname))) \
	  error(EXIT_FAILURE, "*Error*: No such parameter in catalog: ", \
			keyname); \
	 dest = key->ptr;}

