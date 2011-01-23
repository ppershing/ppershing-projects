#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#define MAX 100

char data[MAX][MAX];
#define PAGE 6
#define FOR(q,n) for(int q=0;q<(int)n;q++)

int main(){
  int lines=0;
  srand(time(NULL));

  while (gets(data[lines]),data[lines++][0]!=0);

  FOR(q,lines)
    FOR(w,strlen(data[q]))
      if (data[q][w]=='.') data[q][w]=1+rand()%PAGE;
  

 FOR(p,PAGE){
   FOR(q,lines) {
     printf("|");
     if (p%2==0)  {
       FOR(w,strlen(data[q])) if (data[q][w]==p+1) printf("."); else printf(" ");
     } else {
       for(int w=strlen(data[q])-1;w>=0;w--) 
          if (data[q][w]==p+1) printf("."); else printf(" ");
     }

     printf("|\n");
   }


  printf("-----------------------------------\n");
 }


}
