#include<BWAPI.h>
#include<fstream>
#define A vector
#define C X->self()
#define E UnitTypes::allUnitTypes()
#define F A<V>(E.begin(),E.end())
#define CF TechTypes::Cloaking_Field
#define YG TechTypes::Yamato_Gun
#define XE X->enemy()
#define H(u,z,R)u##fstream("bwapi-data/"#z"/"+XE->getName()).##z(R,1)
#define GB X->getBuildLocation
#define GE u->getEnergy()
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
#define j(z)(unsigned char)a[18*(string("2!~jfEIVakt#lMg").find(char(D.x%10*10+D.y%10+29)))+z]-32
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
#define ut u->train
#define um u->move
#define uu u->useTech
#define HR C->hasResearched
#define ba u->buildAddon
Y namespace std;Y namespace BWAPI;Y S=Position;Y T=TilePosition;Y U=Unit;Y V=UnitType;Y W=int;auto&X = Broodwar;
char*a = "k-k/},k2q5}.w5z8uW1’1Ž “1‹+ˆ ‘%‰ ‡'hl—l’l™kŽeŒ_˜_Ž]‘]xR%R)R#R,X-a%^-b+[Fmbh[mXukuov[ooiouJ/‘2Š'Š4›8›'Œ>›>˜'z/12*/',>&>'* > ;D&“*Ž#Ž!Š,Š)“,Š&•.u&”™Œ’‘‘Š›Š˜”›Š•™Žs–(™.‘+‘0š0—(›(Œ\"ŒF–)*/\",\"1+1(),).#.H&s)s+s1s.y/€,€/Œ$VB•|‘t•~Œ}ŠzŒtŠw˜Œ}W;”;‘;Œ;ŽAŽG’HŒ-˜f|(E,=(G0F0C0=0@#0?g";
W CC(W u) {RT C->completedUnitCount(F[u]);}
W CI(W u) {RT C->incompleteUnitCount(F[u]);}
A<W>b={12,16,32,44,54,64,68,74},c={3,5,3,7,8,3,10,3};A<T>h;map<T,U>i,l;map<U,U>z,q;map<U,W>y;W d,n,w,R,O,e,cc,cd,cy;char G;T f,D,kn;U v,t;A<U>m;T k(W u) {RT T(j(u*2),j(u*2+1));}
N o(U u) {m.insert(m.begin(),u);}
N p(T u) {for (U z:GR(S(u),400,BM))o(z),o(z);}
N s(U u) {if (q[u])o(q[u]),q.erase(u);}
N g(W u, T Z=T()) {e=u;d=O;f=Z.y?d+=99,Z:GB(F[u],D,D.y==63?18:64);}
N I(U u,T z) {O%48==0?ua(S(z)):a;}
N BB(W u,T z=D) {if (O-cc>600&&!CI(u)) {cc=O;U br=X GC(S(z),BW&&(B(Idle)||B(GatheringMinerals))&&BO,400);if (L(br)) br->build(F[u],GB(F[u],z,18));}}
struct ExampleAIModule :AIModule {
 N onFrame() {
  O=X->getFrameCount();kn=k(8);G?c[6]=c[7]=8:0;if (!O) {D=C->getStartLocation();H(i,read,&G);for (T u:X->getStartLocations())u!=D?h.push_back(u):a;p(kn);p(D);}
  for (auto u=h.begin(); u!=h.end();) u=X->isVisible(*u)&&X->getUnitsOnTile(*u,B(Building)&&BE).empty()?i[*u]=t,h.erase(u):u+1;
  for (U u:XE->getUnits()) !i[P]&&Q.isBuilding()?i[P]=u,h.push_back(P):a;
  if (d&&L(l[f])||O-d>240) d=0;
  for (;R<8;R++) if (!d&&!L(l[k(R)])&&O>b[R]*99) g(c[R]+100,k(R));
  if (!d&&w<400&&n>87) 1.0*n/w>0.8?g(103):(CC(108)<3?g(108):a);
  if (n>21&&CC(104)<(int)GR(S(D),400,B(ResourceContainer)&&!BM).size()) BB(104);
  M(n>41&&CC(100)==1) BB(100); M(CC(104)==1&&!GR(S(kn),400,BO&&BW&&B(Completed)).empty()) BB(104,kn);
  w=n=R=0;
 for (U u:C->getUnits()) {
  if (Q==F[3]) {if (CC(15)+CC(0)==0) {um(S(D));CT;}M(U cf=X GC(S(P),BO&&B(Flyer)&&Filter::CanAttack)) {if (cf IU&&!cf->isDefenseMatrixed()&&GE>99&&O-cd>30) {cd=O;uu(TechTypes::Defensive_Matrix,cf);CT;} um(cf->getPosition());CT;}CT;}
  w+=Q.supplyProvided();n+=Q.supplyRequired();
  U Z=u GC(BE,200);Z&&!Z->isMoving()?y[Z]=O:w; u->isStartingAttack()?y[u]=O:w;
  if (d&&u==v)s(u),ud(S(f))<200?u->build(F[e],f):um(S(f));
  M(Q.isAddon()) G?u->research(CF):u->research(YG);
  M(Q.isBuilding()) {
  l[P] = u;
  if (!u->isTraining()) {
   if (!d||e%3!=0) {
    if ((P==D||P==kn)&&GR(S(P),320,BO&&BW).size()<21&&CC(13)<48) ut(F[13]);
    if (CC(2)<12)ut(F[2]);}
   if (!CC(3)&&!CI(3)) ut(F[3]);
  G?ut(F[0]):ut(F[15]);}
  if (Q==F[100]) {
   if (P!=D&&P!=kn)
    if (!u IF) {if (u->isTraining()) u->cancelTrain();u->lift();}
    M(ud(S(kn))>200) um(S(kn));
	M(u->isMoving()) {if (m.empty()) p(kn);u->land(kn);}
  }
  G?(CC(109)?a:ba(F[109])):ba(F[109]),ba(F[112]);
  }
  M(Q == F[13]) {
  !L(v) ? v = u : v;
  if (Z&&(O-y[Z])<99&&!Z IF)s(u),K!=Z?ua(Z):a;
  M(ud(S(kn))<400) {if (u->isIdle())if (U cm=u GC(BM)) {ug(cm);CT;}}
  M(m.size()&&!q[u])if (ug(m[0]))q[u]=m[0], m.erase(m.begin()); M(1)um(S(k(2)));
  M(K&&K->getResources()&&K!=q[u])ug(q[u]);}
  M(O-y[u]<4); M(O%2500==0&&u IF)z[u]=u; M(h.empty())O%500==0?I(u,T(rand()%2*X->mapWidth(),rand()%2*128)):a;
  M((G?CC(0)>5:CC(15)>2)&&z[u])I(u,h[0]); M(1)I(u,D);
  if(Q==F[0])if (HR(CF)&&!u->isCloaked()&&u IU) uu(CF);			
  if (Q==F[15])if (L(Z))if (Z gt.airWeapon()!=WeaponTypes::None&&Z->getHitPoints()>99||Z gt==F[66])if (HR(YG)&&GE>150&&O-cy>80) {cy=O;uu(YG, Z);CT;}
  if (Q==F[0]||Q==F[2]||Q==F[15])if (U ZZ=u GC(BE,400))if (L(ZZ)&&ZZ gt!=F[67]&&ZZ->isDetected()) K!=ZZ?ua(ZZ):a;
 }}
 N onUnitComplete(U u) {Q==F[104]?o(u),o(u):a;}
 N onEnd(bool u) {H(o,write,u!=!G?"1":"\0");}
};