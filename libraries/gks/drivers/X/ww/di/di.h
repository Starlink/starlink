/* copyright (c) Mark M Martin. RAL. 1986 */
/*
 * include files for all device independent routines that go
 * to build the ww library of routines. jan 88
 */
#include "wwinfo.h"
#include <stdio.h>

extern char *malloc(), *realloc(), *calloc();
extern char *sprintf();

#define LOCALPUBLIC
#define DDPUBLIC
#define PRIVATE	static
#define PUBLIC
#define UNSET   (-1)
#define BOXEQ(a,b)	(b.b_left==a.b_left && b.b_right==a.b_right && \
			b.b_top==a.b_top && b.b_bottom==a.b_bottom)
/*
 * define FORTINTER to get fortran interface code in library
 */
/*#define FORTINTER */

#ifdef FORTINTER
/*
 * define OLDFORTRAN to get upwards compatible version, ie old routines too
 * wbmdef wbmset wbmrop wbmexb wwwdef wwnexm wwnexb wftdef wftset wftexf
 * wftexm wtxexs wtxexb wlnrop wstate winkey wnwwin wmagic wmessg
 */
#define OLDFORTRAN
box to_box();

/* numbers must equal those in fwwinfo.h -1 (since arrays start at 0 in C) */
#define TOP	0
#define BOTTOM	1
#define LEFT	2
#define RIGHT	3

/* these numbers must match those in wwinfo.h and fwwinfo.h */
#define WXBM	1
#define WXROP	2
#define WXHOTX	3
#define WXHOTY	4
#define WXOFFX	5
#define WXOFFY	6

#endif FORTINTER

#define wwerror(s)	sprintf(dd->d_errmsg,s)
#define wwfail(s,r)	{ gk0xworry(s); return r; }
#define HEIGHT(b)	(b.b_bottom-b.b_top+1)
#define WIDTH(b)	(b.b_right-b.b_left+1)
#define MONOCHROME	2	/* number of colours in mono bitmap */
/*
 * flags for emu. in p_flags:
 */
#define PSTANDOUT	04	/* currently in standout mode */
#define PUNDERLINE	010	/* currently in underline mode */
#define PINVERTED	020	/* currently screen is inverted */
#define PFATFONT	040	/* current font same size as fat one */
#define PINSERT		0100	/* currently in insert mode */
#define PSTATUS		0200	/* status line is on display */
#define PINSTATUS	0400	/* currently on status line */
#define PSAVE		01000	/* saving text for selecting */
#define PMEMORY		02000	/* =>PSAVE. not too small. there is a p_length/text array */

/*
 * stacks of various things. Only width and top used externally.
 * space is not recovered when stacks reduce.
 * top points to the topmost element just stacked. null=> empty.
 * width is number of chars needed per entry, or better, the alignment needed
	static stack stackbm = sizeof(bitmap *);
 * ( end is pointer to end of got space. (last char+1)
 *   memory is pointer to start of got space. 0=>no memory. bottom not used. )
 * ---------------- Same struct is used for queues. ---------------
 * top->top of queue, next element to extract. null=> empty.
 * bottom->last item added. will == top if one item in queue.
 */
typedef struct{
	unsigned int	s_width;	/* must be first */
	char	*s_top;
	char	*s_bottom;
	char	*s_end;
	char	*s_memory;
}stack;
#define STACKPUSH(s,v)	gk0xstackpush(&s,(char *)&(v))
#define STACKPOP(s,v)	gk0xstackpop(&s,(char *)&(v))
#define STACKEMPTY(s)	((s).s_top==NULLPTR(char))
#define ENQUEUE(s,v)	enqueue(&s,(char *)&(v))
#define DEQUEUE(s,v)	dequeue(&s,(char *)&(v))
#define UNDOQUEUE(s,v)	undoqueue(&s,(char *)&(v))
#define QUEUEEMPTY(s)	((s).s_top==NULLPTR(char))
#define ENDQUEUE(s)	((s).s_top)	/* -> last item on q or 0 */
/*
 * keep a cache of visited directories, so we can
 * visit them again quickly. each item is a struct 'dirinfo'.
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>
typedef struct direct *dirptr;
typedef struct{
	char	*m_dir;		/* full pathname of this dir */
	int	 m_dirlen;	/* len of m_dir for quicker == checking */
	char	**m_dirnames;	/* array of sub-directory names */
	char	**m_filenames;	/* array of ordinary filenames */
	int	*m_dirlens;	/* lengths of each dir name */
	int	*m_filelens;	/* lengths of each filename */
	int	 m_rootindex;	/* index of root dir in m_namelist or -1 */
	int	 m_parentindex;	/* index of .. dir in m_namelist */
	int	 m_sonindex;	/* index of previous dir (we .. from) in m_namelist or -1 */
	ino_t	 m_inode;	/* this dir inode */
	dev_t	 m_device;	/* this dir device */
	dirptr	*m_namelist;	/* namelist returned from scandir() */
	int	 m_length;	/* length of namelist, for free */
}dirinfo;
#define NNULL (struct direct *)0
/*
 *
 */
#define STRUCTMALLOC(x)	(x *)gk0xclrmalloc(sizeof(x))
#define ARRAYMALLOC(length,type)	(type *)malloc((unsigned)(length)*sizeof(type))
#define chartoint(x)	((x)&0377)
/*
 * External Typing: rest of file generated automatically BEWARE
 */
int	 gk0xbitstream();
int	 gk0xbtoi();
int	 gk0xbytestream();
dirinfo	*cache();
char	*gk0xclrmalloc();
void	 dequeue();
void	 enqueue();
int	 findcwd();
void	 freecache();
dirinfo	*indot();
void	 itob();
void	 lineorbar();
dirinfo	*scan();
void	 gk0xsetscreensize();
void	 gk0xstackpop();
void	 gk0xstackpush();
void	 gk0xstreamraster();
cursor	*textcursor();
void	 textsee();
void	 textshowseln();
int	 tohittype();
void	 txbar();
void	 txlengthen();
int	 txposn();
void	 undoqueue();
void	 gk0xworry();
