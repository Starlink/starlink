/* copyright (c) Mark M Martin. RAL. 1988 */
/*
 * more junk info about a font
 */
typedef struct{
	int		jf_links;	/* number of uses. 0=> get rid of */
	int		jf_flags;	/* proportional font? */
	Font		jf_font;
	int		jf_offset;
}jfontinfo;

#define PROPORTIONAL	01	/* in jf_flags */
#define DONTFREE	02	/* dont free font even if user asks */
#define jft(x)	((jfontinfo *)(x->f_junk))
#define MAXDIMCURSOR 64
