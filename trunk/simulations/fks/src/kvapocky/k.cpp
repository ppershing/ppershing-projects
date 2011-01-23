#include <vector>
#include <utility>
#include "winbgim.h"
#include <stdio.h>
#include <cmath>
#include <time.h>
#include <stdlib.h>

#define RESX 1000
#define RESY 800

using namespace std;

#define FOR(q,n) for(int q=0;q<n;q++)
#define FORS(q,t) FOR((q),(int)(t).size())
#define fi first
#define se second
#define mp make_pair
#define pb push_back
#define K 0.00000000001

typedef double ll;
typedef pair<ll,ll> PLL;
int N;
int apage=0;

typedef struct{
PLL pos,v;
ll polomer,hmotnost;
} tkvapka;


vector<tkvapka> kvapky,tmp;

void zobraz(){
  apage^=1;
//  setactivepage(apage);
//cleardevice();
FORS(q,kvapky)
  putpixel((int)kvapky[q].pos.fi,(int)kvapky[q].pos.se,15);
//  setvisualpage(apage);

}

void init(){
 initwindow(RESX,RESY);
 FOR(q,200) {
   tkvapka k;
   k.hmotnost=64;
   k.polomer=4;
   k.pos=mp((ll)(rand()%RESX),(ll)(rand()%RESY));
   k.v=mp(0.0,0.0);
   kvapky.pb(k);
   


 }
}


ll sqr(ll x){
return x*x;
}

ll dist(int q,int w){
return (sqrt(sqr(kvapky[q].pos.fi-kvapky[w].pos.fi)+sqr(kvapky[q].pos.se-
        kvapky[w].pos.se)));
}

void zraz(int q,int w){
if (dist(q,w)>kvapky[q].polomer+kvapky[w].polomer) return;
tkvapka k;
k.hmotnost=kvapky[q].hmotnost+kvapky[w].hmotnost;
k.polomer=exp(log(k.hmotnost)/3);
k.pos.fi=(kvapky[q].hmotnost/k.hmotnost)*kvapky[q].pos.fi;
k.pos.se=(kvapky[q].hmotnost/k.hmotnost)*kvapky[q].pos.se;
k.pos.fi+=(kvapky[w].hmotnost/k.hmotnost)*kvapky[w].pos.fi;
k.pos.se+=(kvapky[w].hmotnost/k.hmotnost)*kvapky[w].pos.se;


k.v.fi=(kvapky[q].hmotnost/k.hmotnost)*kvapky[q].v.fi;
k.v.se=(kvapky[q].hmotnost/k.hmotnost)*kvapky[q].v.se;
k.v.fi+=(kvapky[w].hmotnost/k.hmotnost)*kvapky[w].v.fi;
k.v.se+=(kvapky[w].hmotnost/k.hmotnost)*kvapky[w].v.se;

kvapky[q]=k;
kvapky.erase(kvapky.begin()+w);
}

void simuluj(){
tmp=kvapky;
FORS(q,kvapky){
  PLL f=mp(0.0,0.0);
  
  FORS(w,tmp){
  f.fi+=kvapky[q].hmotnost*tmp[w].hmotnost*K*(tmp[w].pos.fi-kvapky[q].pos.fi);
  f.se+=kvapky[q].hmotnost*tmp[w].hmotnost*K*(tmp[w].pos.se-kvapky[q].pos.se);
  }
     
  kvapky[q].v.fi+=f.fi;
  kvapky[q].v.se+=f.se;
  kvapky[q].pos.fi+=kvapky[q].v.fi;
  kvapky[q].pos.se+=kvapky[q].v.se;
}
  

FORS(q,kvapky)
  FORS(w,kvapky)
  if (q!=w) zraz(q,w);

}

int main(){
  srand(time(NULL));
init();
zobraz();
while(true) {
  simuluj();
  zobraz();
}
  

}
