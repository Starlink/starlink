typedef union {
    int    Node;        /* Index of Node */
    double dbl;         /* real value    */
    long   lng;         /* integer value */
    char   log;         /* logical value */
    char   str[256];    /* string value  */
} FFSTYPE;
#define	BOOLEAN	257
#define	LONG	258
#define	DOUBLE	259
#define	STRING	260
#define	BITSTR	261
#define	FUNCTION	262
#define	BFUNCTION	263
#define	GTIFILTER	264
#define	REGFILTER	265
#define	COLUMN	266
#define	BCOLUMN	267
#define	SCOLUMN	268
#define	BITCOL	269
#define	ROWREF	270
#define	NULLREF	271
#define	SNULLREF	272
#define	OR	273
#define	AND	274
#define	EQ	275
#define	NE	276
#define	GT	277
#define	LT	278
#define	LTE	279
#define	GTE	280
#define	POWER	281
#define	NOT	282
#define	INTCAST	283
#define	FLTCAST	284
#define	UMINUS	285


extern FFSTYPE fflval;
