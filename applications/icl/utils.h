/******************************************************************************
 *									      *
 *		U T I L S . H						      *
 * 									      *
 * Include file for ICL utilities functions. This file is included by icl.h   *
 *									      *
 *   History								      *
 *      Created: S.K. Robinson 7/11/91					      *
 *	Modified: B.K. McIlwrath 14/7/93				      *
 *		Add strip_path()				              *
 *	Tidied:   B.K. McIlwrath 15/11/93				      *
 *      Added restore_adamstring: A.J.Chipperfield 28/11/96                   *
 *									      *
 ******************************************************************************
 */
#define CHARNULL ((char *)0)
extern char *CHARNIL;

extern void pad(char *string, int stringlen, char padchar);
extern int  iclidentityequal(char *s, char *t, int n);
extern int  expand_name(char **absname, char *name);
extern char *strcopy(const char *s);
extern char *strconcat(char *s, char *t);
extern char *strtrim(char *s, int st_len);
extern char *uppercase(char *st);
extern char *lowercase(char *st);
extern char *which_exec(char *st);
extern char *restore_iclstring(char *iclstring);
extern char *restore_adamstring(char *iclstring);
extern char *remove_parens (char *name);
extern char *make_width(char *buf, int bufwidth, int width);
extern char *strip_path(char *str);
extern char *strip_all_zeros(char *buf);
extern char *strip_zeros(char *buf, int m);
extern int  non_lead_zero_count(char *buf);
extern char *bin(int num, int width, int sig);
