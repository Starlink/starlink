/*
 				fitscat.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	The LDAC Tools
*
*	Author:		E.BERTIN, DeNIS/LDAC
*
*	Contents:	Simplified versin of the LDACTools: main include file
*
*	Last modify:	07/02/2003
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

#ifndef _FITSCAT_H_
#define _FITSCAT_H_

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#define	MAXCHARS	256	/* max. number of characters */

/*---------------------------- return messages ------------------------------*/

#ifndef	RETURN_OK
#define	RETURN_OK		0
#endif
#ifndef	RETURN_ERROR
#define	RETURN_ERROR		(-1)
#endif
#ifndef	RETURN_FATAL_ERROR
#define	RETURN_FATAL_ERROR	(-2)
#endif

/*--------------------------- FITS BitPix coding ----------------------------*/

#define		BP_BYTE		8
#define		BP_SHORT	16
#define		BP_LONG		32
#define		BP_FLOAT	(-32)
#define		BP_DOUBLE	(-64)

/*-------------------------------- macros -----------------------------------*/

/* Standard FITS name suffix*/

#define		FITS_SUFFIX		".fits"	

/* size (in bytes) of one FITS block */

#define		FBSIZE		2880L	

/* FITS size after adding padding */

#define		PADTOTAL(x)	(((x-1)/FBSIZE+1)*FBSIZE)

/* extra size to add for padding */

#define		PADEXTRA(x)	((FBSIZE - (x%FBSIZE))% FBSIZE)

/*--------------------------------- typedefs --------------------------------*/

typedef enum            {H_INT, H_FLOAT, H_EXPO, H_BOOL, H_STRING, H_STRINGS,
			H_COMMENT, H_HCOMMENT, H_KEY}	h_type;
						/* type of FITS-header data */
typedef enum		{T_BYTE, T_SHORT, T_LONG, T_FLOAT, T_DOUBLE, T_STRING}
				t_type;		/* Type of data */
typedef enum		{WRITE_ONLY, READ_ONLY}
				access_type;	/* Type of access */
typedef enum		{SHOW_ASCII, SHOW_SKYCAT}
				output_type;    /* Type of output */

typedef	float		PIXTYPE;		/* Pixel type */

#ifdef	HAVE_UNSIGNED_LONG_LONG
typedef	unsigned long long	KINGSIZE_T;	/* for large sizes */
#else
typedef	size_t			KINGSIZE_T;	/* better than nothing */
#endif
#ifdef HAVE_LONG_LONG
typedef	long long		KINGLONG;	/* for large sizes */
#else
typedef	long			KINGLONG;	/* better than nothing */
#endif

#if _FILE_OFFSET_BITS
#define OFF_T	off_t
#else
#define OFF_T	KINGLONG
#endif

/*------------------------------- constants ---------------------------------*/

static int	t_size[] = {1, 2, 4, 4, 8, 1};	/* size in bytes per t_type */

/*---------------------------------- key ------------------------------------*/

typedef struct structkey
  {
  char		name[80];		/* name */
  char		comment[80];		/* a comment */
  void		*ptr;			/* pointer to the data */
  h_type	htype;			/* standard ``h_type'' (display) */
  t_type	ttype;			/* standard ``t_type'' (storage) */
  char		printf[80];		/* printing format (C Convention) */
  char		unit[80];		/* physical unit */
  int		naxis;			/* number of dimensions */
  int		*naxisn;		/* pointer to an array of dim. */
  int		nobj;			/* number of objects */
  int		nbytes;			/* number of bytes per element */
  long		pos;			/* position within file */
  struct structkey	*prevkey;	/* previous key within the chain */
  struct structkey	*nextkey;	/* next key within the chain */
  struct structtab	*tab;		/* (original) parent tab */
  int         allocflag;              /* true if ptr dynamically allocated */
  }		keystruct;

/*------------------------------- catalog  ---------------------------------*/

typedef struct structcat
  {
  char		filename[MAXCHARS];	/* file name */
  FILE		*file;			/* pointer to the file structure */
  struct structtab *tab;		/* pointer to the first table */
  int		ntab;			/* number of tables included */
  access_type	access_type;		/* READ_ONLY or WRITE_ONLY */
  }		catstruct;

/*-------------------------------- table  ----------------------------------*/

typedef struct structtab
  {
  int		bitpix;			/* bits per element */
  int		bytepix;		/* bytes per element */
  int		bitsgn;			/* = 0 if unsigned data */
  double	bscale;			/* data scale factor */
  double	bzero;			/* data offset parameter */
  enum {COMPRESS_NONE, COMPRESS_BASEBYTE, COMPRESS_PREVPIX}
		compress_type;		/* image compression type */
  char		*compress_buf;		/* de-compression buffer */
  char		*compress_bufptr;	/* present pixel in buffer */
  int		compress_curval;	/* current pixel or checksum value */
  int		compress_checkval;	/* foreseen pixel or checksum value */
  size_t	compress_npix;		/* remaining pixels in buffer */
  int		naxis;			/* number of dimensions */
  int		*naxisn;		/* array of dimensions */
  int		tfields;		/* number of fields */
  int		pcount, gcount;		/* alignment of the data */
  KINGSIZE_T	tabsize;		/* total table size (bytes) */
  char		xtension[82];		/* FITS extension type */
  char		extname[82];		/* FITS extension name */
  char		*headbuf;		/* buffer containing the header */
  int		headnblock;		/* number of FITS blocks */
  char		*bodybuf;		/* buffer containing the body */
  OFF_T		bodypos;		/* position of the body in the file */
  OFF_T		headpos;		/* position of the head in the file */
  struct structcat *cat;		/* (original) parent catalog */
  struct structtab *prevtab, *nexttab;	/* previous and next tab in chain */
  int		seg;			/* segment position */
  int		nseg;			/* number of tab segments */
  keystruct	*key;			/* pointer to keys */
  int		nkey;			/* number of keys */
  int		swapflag;		/* mapped to a swap file ? */
  char		swapname[MAXCHARS];	/* name of the swapfile */
  unsigned int	bodysum;		/* Checksum of the FITS body */
  }		tabstruct;


/*------------------------------- functions ---------------------------------*/

extern catstruct	*new_cat(int ncat),
			*read_cat(char *filename),
			*read_cats(char **filenames, int ncat);

extern tabstruct	*asc2bin_tab(catstruct *catin, char *tabinname, 
				catstruct *catout, char *taboutname),
			*init_readobj(tabstruct *tab),
			*name_to_tab(catstruct *cat, char *tabname, int seg),
			*new_tab(char *tabname),
			*pos_to_tab(catstruct *cat, int pos, int seg),
			*asc2bin_tab(catstruct *catin, char *tabinname,
				catstruct *catout, char *taboutname);

extern keystruct	*name_to_key(tabstruct *tab, char *keyname),
			*new_key(char *keyname),
			*pos_to_key(tabstruct *tab, int pos),
			*read_key(tabstruct *tab, char *keyname);

extern void	add_cleanupfilename(char *filename),
		cleanup_files(void),
		copy_tab_fromptr(tabstruct *tabin, catstruct *catout, int pos),
		encode_checksum(unsigned int sum, char *str),
		end_readobj(tabstruct *keytab, tabstruct *tab),
		end_writeobj(catstruct *cat, tabstruct *tab),
		error(int, char *, char *),
		fixexponent(char *s),
		free_body(tabstruct *tab),
		free_cat(catstruct **cat, int ncat),
		free_key(keystruct *key),
		free_tab(tabstruct *tab),
		init_writeobj(catstruct *cat, tabstruct *tab),
		install_cleanup(void (*func)(void)),
		print_obj(FILE *stream, tabstruct *tab),
		read_keys(tabstruct *tab, char **keynames, keystruct **keys,
			int nkeys, unsigned char *mask),
		read_basic(tabstruct *tab),
		read_body(tabstruct *tab, PIXTYPE *ptr, size_t size),
		readbasic_head(tabstruct *tab),
		remove_cleanupfilename(char *filename),
		save_cat(catstruct *cat, char *filename),
		save_tab(catstruct *cat, tabstruct *tab),
		show_keys(tabstruct *tab, char **keynames, keystruct **keys,
			int nkeys, unsigned char *mask, FILE *stream,
			int strflag,int banflag, int leadflag,
                        output_type o_type),
		swapbytes(void *, int, int),
		*ttypeconv(void *ptr, t_type ttypein, t_type ttypeout),
		warning(char *, char *),
		write_body(tabstruct *tab, PIXTYPE *ptr, size_t size),
		write_checksum(tabstruct *tab);

extern char	*tdisptoprintf(char *tdisp),
		*printftotdisp(char *cprintf),
		*fitsnfind(char *fitsbuf, char *str, int nblock),
		**tabs_list(catstruct *cat, int *n),
		**keys_list(tabstruct *tab, int *n);

extern unsigned int
		compute_blocksum(char *buf, unsigned int sum),
		compute_bodysum(tabstruct *tab, unsigned int sum),
		decode_checksum(char *str);

extern int	about_cat(catstruct *cat, FILE *stream),
		about_tab(catstruct *cat, char *tabname, FILE *stream),
		addhistoryto_cat(catstruct *cat, char *str),
		add_key(keystruct *key, tabstruct *tab, int pos),
		addkeyto_head(tabstruct *tab, keystruct *key),
		addkeywordto_head(tabstruct *tab, char *keyword,char *comment),
		add_tab(tabstruct *tab, catstruct *cat, int pos),
		blank_keys(tabstruct *tab),
		close_cat(catstruct *cat),
		copy_key(tabstruct *tabin, char *keyname, tabstruct *tabout,
			int pos),
		copy_tab(catstruct *catin, char *tabname, int seg,
			catstruct *catout, int pos),
		copy_tabs(catstruct *catin, catstruct *catout),
		copy_tabs_blind(catstruct *catin, catstruct *catout),
		ext_head(tabstruct *tab),
		findkey(char *, char *, int),
		findnkey(char *, char *, int, int),
		fitsadd(char *fitsbuf, char *keyword, char *comment),
		fitsfind(char *fitsbuf, char *keyword),
		fitspick(char *fitsbuf, char *keyword, void *ptr,
			h_type *htype, t_type *ttype, char *comment),
		fitsread(char *fitsbuf, char *keyword, void *ptr,
			h_type htype, t_type ttype),
		fitsremove(char *fitsbuf, char *keyword),
		fitswrite(char *fitsbuf, char *keyword, void *ptr,
			h_type htype, t_type ttype),
		get_head(tabstruct *tab),
		inherit_cat(catstruct *catin, catstruct *catout),
		init_cat(catstruct *cat),
		map_cat(catstruct *cat),
		open_cat(catstruct *cat, access_type at),
		prim_head(tabstruct *tab),
		readbintabparam_head(tabstruct *tab),
		read_field(tabstruct *tab, char **keynames, keystruct **keys,
			int nkeys, int field, tabstruct *ftab),
		read_obj(tabstruct *keytab, tabstruct *tab),
		read_obj_at(tabstruct *keytab, tabstruct *tab, long pos),
		remove_key(tabstruct *tab, char *keyname),
		remove_keys(tabstruct *tab),
		remove_tab(catstruct *cat, char *tabname, int seg),
		remove_tabs(catstruct *cat),
		set_maxram(size_t maxram),
		set_maxvram(size_t maxvram),
		set_swapdir(char *dirname),
		tab_row_len(char *, char *),
		tformof(char *str, t_type ttype, int n),
		tsizeof(char *str),
		update_head(tabstruct *tab),
		update_tab(tabstruct *tab),
		verify_checksum(tabstruct *tab),
		write_obj(tabstruct *tab),
		wstrncmp(char *, char *, int);

extern PIXTYPE	*alloc_body(tabstruct *tab,
			void (*func)(PIXTYPE *ptr, int npix));

extern t_type	ttypeof(char *str);

extern  void	error(int, char *, char *),
		swapbytes(void *ptr, int nb, int n),
		warning(char *msg1, char *msg2);


int		bswapflag;

#endif
