/*
 * Routine prototypes for adam_ routines
 */
extern
void adam_stop(void);
extern
void adam_acknow(int path, int messid, int messtatus, int context,
                 char *name, char *svalue, int svlen, int *status);
extern void
adam_getreply(int path, int messid, int *context, char *name, char *svalue,
              int *slen, int *status);
extern void
adam_getreplyt(int time, int path, int messid, int *context, char *name,
	       char *svalue, int *svlen, int *status);
extern void
adam_path(char *taskname, int *path, int *status);
extern void
adam_prcname(char *name, int *length, int *status);
extern void
adam_receive(int *path, int *context, char *name, char *svalue, int *svlen,
             int *messid, int *status);
extern void
adam_reply(int path, int context, char *name, char *valu, int vlen,
           int messid, int *status);
extern void
adam_send(int path, int context, char *name, char *inval, int inlen,
          char outval[], int *outlen, int *messid, int *status);
extern void
adam_sendonly(int path, int context, char *name, char *inval, int inlen,
              int *messid, int *status);
extern void
adam_sendt(int time, int path, int context, char *name, char *inval,
           int inlen, char outval[], int *outlen, int *messid, int *status);
extern void
adam_trigger(int path, int context, char *name, char *valu, int messid,
             int *status);
extern void
adam_wait(int msecs, int *status);
