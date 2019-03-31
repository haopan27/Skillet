#include<BWAPI.h>
#include<fstream>
#define A vector
#define C X->self()
#define E UnitTypes::allUnitTypes()
#define F A<V>(E.begin(),E.end())
#define XE X->enemy()
#define H(u,z,R)u##fstream("bwapi-data/"#z"/"+XE->getName()).##z(R,1)
#define GB X->getBuildLocation
#define GC ->getClosestUnit
#define GR X->getUnitsInRadius
#define K u->getOrderTarget()
#define L(z)(z&&z->exists())
#define M else if
#define N void
#define P u->getTilePosition()
#define gt ->getType()
#define Q u gt
#define Y using
#define RT return
#define CT continue
#define j(z)(unsigned char)a[8*(string("2!~jfEIVakt#lMg").find(char(D.x%10*10+D.y%10+29)))+z]-32
#define B(z)Filter::Is##z
#define BE B(Enemy)
#define BM B(MineralField)
#define BO B(Owned)
#define BW B(Worker)
#define IU ->isUnderAttack()
#define IF ->isFlying()
#define ua u->attack
#define ud u->getDistance
#define ug u->gather
#define ut u->morph
#define um u->move
#define uu u->useTech
Y namespace std;Y namespace BWAPI;Y S=Position;Y T=TilePosition;Y U=Unit;Y V=UnitType;Y W=int;auto&X=Broodwar;
char*a = "s6n-n0uW)ˆ0‘0Ž'h";
W CC(W u) {RT C->completedUnitCount(F[u]);}W CI(W u) {RT C->incompleteUnitCount(F[u]);}W CL(W u) {RT CC(u)+CI(u)+(u==123?CC(124):0);}
A<W>b={24,34,78},c={123,134,133};A<T>h;map<T,U>i;map<U,U>z,q;map<U,W>y;W n,w,np,wp,R,O,cc,co;char G;T D,kn;U t;A<U>m;T k(W u) {RT T(j(u*2),j(u*2+1));}
N o(U u) {m.insert(m.begin(),u);}
N p(T u) {for (U z:GR(S(u),400,BM))o(z),o(z);}
N s(U u) {if (q[u])o(q[u]),q.erase(u);}
N I(U u,T z) {O%48==0?ua(S(z)):a;}
N BB(W u,T bl=D,T z=D) {if (O-cc>360) {cc=O;U br=X GC(S(z),BW&&(B(Idle)||B(GatheringMinerals))&&BO,400);L(br)?br->build(F[u],bl==D?GB(F[u],z,18):bl):a;}}
struct ExampleAIModule :AIModule {
 N onFrame() {
  O=X->getFrameCount();kn=k(0);if (!O) {D=C->getStartLocation();H(i,read,&G);for (T u:X->getStartLocations())u!=D?h.push_back(u):a;p(kn);p(D);}
  for (auto u=h.begin();u!=h.end();) u=X->isVisible(*u)&&X->getUnitsOnTile(*u,B(Building)&&BE).empty()?i[*u]=t,h.erase(u):u+1;
  for (U u:XE->getUnits())!i[P]&&Q.isBuilding()?i[P]=u,h.push_back(P):a;
  for (;R<3;R++) if (CL(c[R])<(c[R]==123?2:1)&&O>b[R]*99) {BB(c[R],k(R));break;}
  if (n>23&&CL(140)<(W)GR(S(D),400,B(ResourceContainer)&&!BM).size()) BB(140);
  M(CC(140)==1&&!GR(S(kn),400,BO&&BW&&B(Completed)).empty()) BB(140,kn);M(CL(133)) CL(123)<4?BB(123):(CC(135)||CC(137)?a:BB(135));
  wp=w;np=n;w=n=R=0;
 for (U u:C->getUnits()) {
  w+=Q.supplyProvided();n+=Q.supplyRequired();
  U Z=u GC(BE,200);Z&&!Z->isMoving()?y[Z]=O:w;u->isStartingAttack()?y[u]=O:w;
  if(Q.isBuilding()) {
   CL(124)?a:ut(F[124]);ut(F[137]);
   u->upgrade(UpgradeTypes::Zerg_Flyer_Attacks);
  }
  M(Q==F[34]) {
   if (n>17&&w-n<4) if (O-co>601) {co=O;ut(F[41]);}
   for (S ip:{S(D),S(kn)}) if(ud(ip)<128&&GR(ip,320,BO&&BW).size()<20) ut(F[40]);
   ut(F[42]);
   if (CC(36)<18) ut(F[36]);
  }
  M(Q==F[40]) {
   if (Z&&(O-y[Z])<99&&!Z IF)s(u),K!=Z?ua(Z):a;
   M(ud(S(kn))<320) {if (u->isIdle())if (U cm=u GC(BM)) {ug(cm);CT;}}
   M(m.size()&&!q[u])if (ug(m[0]))q[u]=m[0],m.erase(m.begin());M(1)um(S(k(2)));
   M(K&&K->getResources()&&K!=q[u])ug(q[u]);
  }
  M(O-y[u]<4);M(O%2500==0&&u IF)z[u]=u;M(h.empty())O%500==0?I(u,T(rand()%2*X->mapWidth(),rand()%2*128)):a;
  M(CC(42)>5&&z[u])I(u,h[0]);M(1)I(u,D);
  if (Q==F[36]||Q==F[42])if (U ZZ=u GC(BE,400))if (L(ZZ)&&ZZ gt!=F[67]) K!=ZZ?ua(ZZ):a;
 }
 }
 N onUnitComplete(U u) {Q==F[140]?o(u),o(u):a;}
 N onEnd(bool u) { H(o,write,u!=!G?"1":"\0");}
};
