
#include <stdio.h>
#include <errno.h>
#include <strings.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <time.h>
#include <stdlib.h>

#define LINELENG 100

int
main(int argc, char *argv[]) {

   char *iam;                /* Name of this program */
   char *outfilename = NULL; /* File to append output to */
   char *label = NULL;       /* Label for output line */
   pid_t pid;                /* Child process ID     */
   char *c_cmd, **c_argv;    /* Child exec arguments */
   int ptr_stat;             /* Pointer to child process status info */
   int status = 0;           /* Status of returning child process */
   struct rusage usage;      /* Contains info about resource usage */
   struct timeval tvbuf;     /* Workspace of type struct timeval */
   int pagesize;             /* Number of bytes per page */
   int user_time,            /* User time of child in seconds */
       sys_time,             /* System time of child in seconds */
       elapse_time,          /* Wall clock time for child in seconds */
       max_rss,              /* Maximum RSS of child in Kbyte */
       max_size,             /* Maximum total size of child in Kbyte */
       cpu_percent;          /* Percentage of CPU time used */
   time_t t_start, t_stop;   /* For recording elapsed time */
   char linebuf[LINELENG];   /* Buffer for preparing output */
   int ret;                  /* Buffer for function return values */
   FILE *outfile;            /* Stream for output */

                       /* Get name (excluding path) of program */
   if ( (iam = rindex(argv[0], '/')) )
      iam++;
   else
      iam=argv[0];
   argv++;

                       /* Parse arguments */
   while (*argv && **argv == '-') {
      switch ( (*argv)[1] ) {

         case 'f':
                     /* Name of file to append to */
            argv++;
            outfilename = argv[0];
            argv++;
            if (!outfilename) goto usage_message;
            break;

         case 'l':
                     /* Label for line of output (e.g. name of cmd) */
            argv++;
            label = argv[0];
            argv++;
            if (!label) goto usage_message;
            break;

         case 'h':
         default:
                     /* Print usage */
            usage_message:
            printf (
               "Usage: %s [-f logfile] [-l label] cmd [args]   - time cmd\n",
               iam);
            printf (
               "       %s [-f logfile]                         - print title\n",
               iam);
            exit (0);
            break;
      }
   }


   if (*argv != NULL) { 
                          /* A command has been supplied, so time it */

                          /* Arguments for the exec */
         c_cmd  = *argv;
         c_argv = argv;

                          /* Determine label for output line */
                          /* If it's already been defined (using flags) *
                           * then leave it.                             * 
                           * Otherwise use the root name of the command */
      if (!label) {
        if ( (label = rindex(c_cmd, '/')) )
             label++;
          else
             label = c_cmd;
      }


                          /* Start timer */
      t_start = time(NULL);
      
      if ((pid = fork()) == (pid_t) -1) {
         perror(iam);
         exit (1);
      }

      if (!pid) {      /* Child  */
         execvp (c_cmd, c_argv);
                       /* Exec has failed if we arrive here */
         perror(iam);
         _exit (1);
      } 

      /* Parent */
                    /* Wait for child process to finish */
      wait (&ptr_stat);

                    /* Stop timer */
      t_stop = time(NULL);

                    /* Check for abnormal exit */
      if (!WIFEXITED(ptr_stat)) {
         printf ("%s terminated abnormally\n", c_cmd);
         exit (1);
      }
      if (status == WEXITSTATUS(ptr_stat))
         printf ("WARNING: %s exited with status %d\n", c_cmd, status);
   
      if (getrusage (RUSAGE_CHILDREN, &usage)) {
         perror (iam);
         exit (1);
      }

                 /* Prepare resource use values for output */
      tvbuf = usage.ru_utime;
      user_time = tvbuf.tv_sec;
   
      tvbuf = usage.ru_stime;
      sys_time = tvbuf.tv_sec;
   
      elapse_time = (int) (t_stop - t_start);
   
      cpu_percent = elapse_time ? (100 * (user_time + sys_time)) / elapse_time
                                : 0;
   
      pagesize = getpagesize();
      max_rss = (((int) usage.ru_maxrss) * pagesize) / 1024;
      printf ("%ld  %d  %d  \n", (long)usage.ru_maxrss, pagesize, max_rss);

      max_size = 0;
   
               /* Prepare line for output */
      ret = sprintf (linebuf, 
         "%15s: %6d %6d %7d     %3d  %7d  %7d\n", 
         label,
         user_time, sys_time, elapse_time, cpu_percent, max_rss, max_size);
      if (ret < 0) {
         printf ("%s: Output error\n", iam);
         exit (1);
      }

   } else {
                          /* No command supplied; just print out header line */
      ret = sprintf (linebuf, 
         "#                User/s  Sys/s Elaps/s Percent MaxRSS/K MaxSiz/K\n");
      if (ret < 0) {
         printf ("%s: Output error\n", iam);
         exit (1);
      }
   }

                      /* Open file for appending (or use stdout) */
   if (outfilename) {
      outfile = fopen (outfilename, "a");
      if (!outfile) {
         perror (iam);
         exit (1);
      }
   } else {
      outfile = stdout;
   }

                      /* Write the line to the file */
   ret = fprintf (outfile, "%s", linebuf);
   if (ret < 0) {
      printf ("%s: Output error\n", iam);
      exit (1);
   }

                      /* Close file */
   if (outfilename) {
      ret = fclose (outfile);
      if (ret) {
         perror (iam);
         exit (1);
      }
   }

                      /* Exit */
   exit (0);

}




   

