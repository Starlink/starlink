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
*	Last modify:	07/04/99
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

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

/* size (in bytes) of one FITS block */

#define		FBSIZE		2880L	

/* FITS size after adding padding */

#define		PADTOTAL(x)	(((x-1)/FBSIZE+1)*FBSIZE)

/* extra size to add for padding */

#define		PADEXTRA(x)	((FBSIZE - (x%FBSIZE))% FBSIZE)

/*--------------------------------- typedefs --------------------------------*/

typedef enum            {H_INT, H_FLOAT, H_EXPO, H_BOOL, H_STRING, H_COMMENT,
			H_HCOMMENT, H_KEY}	h_type;
						/* type of FITS-header data */
typedef enum		{T_BYTE, T_SHORT, T_LONG, T_FLOAT, T_DOUBLE, T_STRING}
				t_type;		/* Type of data */
typedef enum		{WRITE_ONLY, READ_ONLY}
				access_type;	/* Type of access */
typedef enum		{SHOW_ASCII, SHOW_SKYCAT}
				output_type;    /* Type of output */


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
  int		pos;			/* position within file */
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
  int		bitpix;			/* Bits per element */
  int		bytepix;		/* Bytes per element */
  int		naxis;			/* number of dimensions */
  int		*naxisn;		/* array of dimensions */
  int		tfields;		/* number of fields */
  int		pcount, gcount;		/* alignment of the data */
  int		tabsize;		/* total table size (bytes) */
  char		xtension[82];		/* FITS extension type */
  char		extname[82];		/* FITS extension name */
  char		*headbuf;		/* buffer containing the header */
  int		headnblock;		/* number of FITS blocks */
  char		*bodybuf;		/* buffer containing the body */
  int		bodypos;		/* position of the body in the file */
  struct structcat *cat;		/* (original) parent catalog */
  struct structtab *prevtab, *nexttab;	/* previous and next tab in chain */
  int		seg;			/* segment position */
  int		nseg;			/* number of tab segments */
  keystruct	*key;			/* pointer to keys */
  int		nkey;			/* number of keys */
  }		tabstruct;


/*------------------------------- functions ---------------------------------*/

extern catstruct	*new_cat(int ncat),
			*read_cat(char *filename),
			*read_cats(char **filenames, int ncat);

extern tabstruct	*init_readobj(tabstruct *tab),
			*name_to_tab(catstruct *cat, char *tabname, int seg),
			*new_tab(char *tabname),
			*pos_to_tab(catstruct *cat, int pos, int seg),
			*asc2bin_tab(catstruct *catin, char *tabinname,
				catstruct *catout, char *taboutname);

extern keystruct	*name_to_key(tabstruct *tab, char *keyname),
			*new_key(char *keyname),
			*pos_to_key(tabstruct *tab, int pos),
			*read_key(tabstruct *tab, char *keyname);

extern void	end_readobj(tabstruct *keytab, tabstruct *tab),
		end_writeobj(catstruct *cat, tabstruct *tab),
		error(int, char *, char *),
		fixexponent(char *s),
		free_cat(catstruct *cat, int ncat),
		free_key(keystruct *key),
		free_tab(tabstruct *tab),
		init_writeobj(catstruct *cat, tabstruct *tab),
		print_obj(FILE *stream, tabstruct *tab),
		read_keys(tabstruct *tab, char **keynames, keystruct **keys,
			int nkeys, unsigned char *mask),
		read_basic(tabstruct *tab),
		readbasic_head(tabstruct *tab),
		save_cat(catstruct *cat, char *filename),
		save_tab(catstruct *cat, tabstruct *tab),
		show_keys(tabstruct *tab, char **keynames, keystruct **keys,
			int nkeys, unsigned char *mask, FILE *stream,
			int strflag,int banflag, int leadflag,
                        output_type o_type),
		swapbytes(void *, int, int),
		*ttypeconv(void *ptr, t_type ttypein, t_type ttypeout),
		warning(char *, char *);

extern char	*tdisptoprintf(char *tdisp),
		*printftotdisp(char *cprintf),
		*fitsnfind(char *fitsbuf, char *str, int nblock),
		**tabs_list(catstruct *cat, int *n),
		**keys_list(tabstruct *tab, int *n);

extern int	about_cat(catstruct *cat, FILE *stream),
		about_tab(catstruct *cat, char *tabname, FILE *stream),
		addhistoryto_cat(catstruct *cat, char *str),
		add_key(keystruct *key, tabstruct *tab, int pos),
		addkeyto_head(tabstruct *tab, keystruct *key),
		add_tab(tabstruct *tab, catstruct *cat, int pos),
		blank_keys(tabstruct *tab),
		close_cat(catstruct *cat),
		copy_key(tabstruct *tabin, char *keyname, tabstruct *tabout,
			int pos),
		copy_tab(catstruct *catin, char *tabname, int seg,
			catstruct *catout, int pos),
		copy_tabs(catstruct *catin, catstruct *catout),
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
		readbintabparam_head(tabstruct *tab),
		read_field(tabstruct *tab, char **keynames, keystruct **keys,
			int nkeys, int field, tabstruct *ftab),
		read_obj(tabstruct *keytab, tabstruct *tab),
		read_obj_at(tabstruct *keytab, tabstruct *tab, long pos),
		remove_key(tabstruct *tab, char *keyname),
		remove_keys(tabstruct *tab),
		remove_tab(catstruct *cat, char *tabname, int seg),
		remove_tabs(catstruct *cat),
		tab_row_len(char *, char *),
		tformof(char *str, t_type ttype, int n),
		tsizeof(char *str),
		update_head(tabstruct *tab),
		update_tab(tabstruct *tab),
		write_obj(tabstruct *tab),
		wstrncmp(char *, char *, int);

extern t_type	ttypeof(char *str);
