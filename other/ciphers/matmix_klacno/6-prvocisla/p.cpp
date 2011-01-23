#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

int is_prime(int i){
if (i<2) return 0;
if (i==2) return 1;
for (int q=2;q<i;q++) if (i%q==0) return 0;
return 1;
}

char s[100];
int main(){
  srand(time(NULL));
  scanf("%s\n",s);
  int spos=0;
  int pos=1;
   while (spos<strlen(s)) {
     if (is_prime(pos)) {
       printf("%c",s[spos]);
       spos++;
     } else {
       printf("%c",rand()%26+96);

     }


     pos++;
   }
  


}
