/*****************************************************************************
 *
 *		O U T P U T . H 	Interface for output.c
 *
 *		Created		S.K. Robinson 14/5/92
 *
 *
 *****************************************************************************
 */
extern void sendtoiosubsystem(int command, int info, char *message);
extern int stackandsetoutfp(value vfp);
extern int restoreoutfp(void);
extern value iclopenasoutfp (char *whofor, char *filename);
extern value iclcloseasoutfp(char *whofor, char *filename);
extern void systemfail(char *mess);
extern int outstring(char *mess);
extern int bufstring(char *mess);
extern int bufchar(char ch);
extern int bufnewline(void);
extern int flshbuf(void);
extern int outfpint (int i);
extern int outfpreal(double d);
extern int outfpstring(char *mess);
extern int outfpchar(char ch);
extern int outfpformatstring(char *format, char *mess);
extern void iclems_flush(void);
extern value init_output(void);
