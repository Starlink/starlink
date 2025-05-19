
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define SLEN 81

int main()
{
  char line[SLEN], prompt[SLEN];

  int  llen;

  static int  itrm = 1;

  llen = get_aline_(line, "Hello World (over): ", &itrm, SLEN, 20);

  if (llen == 0)
    printf("....Nobody home I guess.\n");
  else
   {
     printf("World replied:\n");
     printf("'%s'\n",line);
     if (llen == -1)
       printf("....*choke* remainder truncated\n");
   }

  printf("\nTry history (up-arrow) and some editing next!\n");
  sprintf(prompt,"Hello Moon too (over): ");

  llen = get_aline_(line, prompt, &itrm, SLEN, strlen(prompt));

  if (llen == 0)
    printf("....Still a dusty and lonely place.\n");
  else
    {
      printf("Moon replied:\n");
      printf("'%s'\n",line);
      if (llen == -1)
	printf("....*choke* remainder truncated\n");
    }
  printf("\n");

  return EXIT_SUCCESS;

}
