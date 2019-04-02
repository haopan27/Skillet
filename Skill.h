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
#define GI X->getUnitsOnTile
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
#define j(z)(unsigned char)a[6*(string("2!~jfEIVakt#lMg").find(char(D.x%10*10+D.y%10+29)))+z]-32
#define B(z)Filter::Is##z
#define BE B(Enemy)
#define BF B(Flying)
#define BM B(MineralField)
#define BO B(Owned)
#define BW B(Worker)
#define IU ->isUnderAttack()
#define IF ->isFlying()
#define IM ->isMoving()
#define ua u->attack
#define ud u->getDistance
#define ug u->gather
#define ut u->morph
#define um u->move
#define up u->upgrade
#define uu u->useTech
#define HU C->getUpgradeLevel
#define UT UpgradeTypes::
#define AG UT Adrenal_Glands
#define PC UT Pneumatized_Carapace
#define VS UT Ventral_Sacs
#define RP T(rand()%2*X->mapWidth(),rand()%2*128)
#define SM h.empty()?um(S(RP)):um(S(h[0]))
#define NL u->getLoadedUnits().size()
Y namespace std;Y namespace BWAPI;Y S=Position;Y T=TilePosition;Y U=Unit;Y V=UnitType;Y W=int;auto&X=Broodwar;
char*a = "s6n-r0)ˆ0‘,Ž]xi“`–[FU*`&uJj[w_'z(‹&ŽD&2*&.u&Ž$•,s–Ž“–›F–+’'›H&,#(,”&u*‚%˜•‘u—~'—>”I˜$(+>%G";
W CC(W u) {RT C->completedUnitCount(F[u]);}W CI(W u) {RT C->incompleteUnitCount(F[u]);}W CL(W u) {RT CC(u)+CI(u)+(u==123?CC(124):0);}
A<W>b={22,32,74},c={123,134,133};A<T>h;map<T,U>i;map<U,U>z,q;map<U,W>y;W n,np,w,wp,x,xp,R,O,cc,co,cm;char G;T D,kn;U t;A<U>m;T k(W u) {RT T(j(u*2),j(u*2+1));}
N o(U u) {m.insert(m.begin(),u);}
N p(T u) {for (U z:GR(S(u),400,BM))o(z),o(z);}
N s(U u) {if (q[u])o(q[u]),q.erase(u);}
N I(U u,T z) {O%48==0?ua(S(z)):a;}
N BB(W u,T b0=D,T z=D) {if(O-cc>(b0==kn?600:240)) {cc=O;U br=X GC(S(z),BW&&(B(Idle)||B(GatheringMinerals))&&BO,400);L(br)?br->build(F[u],b0==D?GB(F[u],z,18):b0):a;}}
struct ExampleAIModule :AIModule {
 N onFrame() {
  O=X->getFrameCount();kn=k(0);if(X->mapHash()[0]==54)b[2]=80;if (!O) {D=C->getStartLocation();H(i,read,&G);for (T u:X->getStartLocations())u!=D?h.push_back(u):a;p(kn);p(D);}
  for (auto u=h.begin();u!=h.end();) u=X->isVisible(*u)&&GI(*u,B(Building)&&BE).empty()?i[*u]=t,h.erase(u):u+1;
  for (U u:XE->getUnits())!i[P]&&Q.isBuilding()?i[P]=u,h.push_back(P):a;
  for (;R<3;R++) if (CL(c[R])<(c[R]==123?2:1)&&O>b[R]*99) {BB(c[R],k(R));break;}
  if (n>23&&CL(140)<(W)GR(S(D),400,B(ResourceContainer)&&!BM).size()) BB(140);
  M(CC(140)==1&&!GR(S(kn),400,BO&&BW&&B(Completed)).empty()) BB(140,D,kn);M(CL(c[2])) CL(123)<5?BB(123):(CL(130)?a:BB(130));
  wp=w;np=n;xp=x;w=n=x=R=0;
 for (U u:C->getUnits()) {
  w+=Q.supplyProvided();n+=Q.supplyRequired();
  U Z=u GC(BE,200);Z&&!Z IM?y[Z]=O:w;u->isStartingAttack()?y[u]=O:w;
  if(Q.isBuilding()) {P==D?(!CL(124)?ut(F[124]):(!CL(125)?ut(F[125]):a)):a;up(AG);up(UT Metabolic_Boost);up(VS);up(PC);}
  M(Q==F[34]) {
   if (wp<400){if (CC(c[2])&&1.0*np/wp>0.8)O-co>200?co=O,ut(F[41]):a;M(np>17&&wp-np<4)O-co>601?co=O,ut(F[41]):a;}
   for (S ip:{S(D),S(kn)}) if(ud(ip)<128&&GR(ip,320,BO&&BW).size()<19&&CC(40)<38) O>b[0]*99&&C->minerals()<352&&GI(kn,BO&&B(Building)).empty()?a:ut(F[40]);
   CC(125)&&!HU(AG)&&!C->isUpgrading(AG)?a:C->gas()>151?ut(F[42]):a,ut(F[36]);
  }
  M(Q==F[40]) {
   if (Z&&(O-y[Z])<99&&!Z IF)s(u),K!=Z?ua(Z):a;
   M(ud(S(kn))<320&&CL(123)>1) {if (u->isIdle())if (U cm=u GC(BM)) {ug(cm);CT;}}
   M(m.size()&&!q[u])if (ug(m[0]))q[u]=m[0],m.erase(m.begin());M(1)um(S(k(0)));
   M(K&&K->getResources()&&K!=q[u])ug(q[u]);
  }
  M(Q==F[41]) {
if(HU(VS)&&HU(PC)&&u->getHitPoints()>159&&cm>2){
   if(!GR(S(P),160,BE&&!BF).empty()) {NL?u->unload(u->getLoadedUnits().begin()._Ptr->_Myval):um(S(D));}
   M(U cz=u GC(BO&&!BW&&!BF&&!B(Loaded)&&Filter::CanAttack,256))!GR(cz->getPosition(),99,BO&&B(ResourceDepot)).empty()?(NL<8?u->load(cz):(!u IM&&xp>2?(SM):a)):a;
   M(NL>3)x++,!u IM&&xp>2?SM:a;
   M(NL<4)um(S(D));}
GR(S(kn),250,BO).empty()&&!NL?um(S(kn)):a;
  }
  M(O-y[u]<4);M(O%2500==0&&u IF)z[u]=u;M(h.empty())O%500==0?I(u,RP):a;
  M(CC(42)>3&&z[u])I(u,h[0]);M(1)I(u,D);
  if (Q==F[36]||Q==F[42])if (U ZZ=u GC(Q==F[36]?BE&&!BF:BE,400)) L(ZZ)&&ZZ gt!=F[67]&&ZZ->isDetected()&&K!=ZZ?ua(ZZ):a;
 }}
 N onUnitComplete(U u) {Q==F[140]?o(u),o(u):(Q==F[42]?cm++:0);}
 N onEnd(bool u) { H(o,write,u!=!G?"1":"\0");}
};