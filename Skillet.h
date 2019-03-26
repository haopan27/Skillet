#include<BWAPI.h>
#include<fstream>
#define A vector
#define C X->self()
#define CF TechTypes::Cloaking_Field
#define YG TechTypes::Yamato_Gun
#define E UnitTypes::allUnitTypes()
#define F A<V>(E.begin(),E.end())
#define H(u,z,R)u##fstream("bwapi-data/"#z"/"+X->enemy()->getName()).##z(R,1)
#define GC(z) u->getClosestUnit(B(Enemy), z)
#define K u->getOrderTarget()
#define L(z)(z&&z->exists())
#define M else if
#define N void
#define P u->getTilePosition()
#define Q u->getType()
#define Y using
#define RT return
#define CT continue
#define j(z)(unsigned char)a[20*(string("2!~jfEIVakt#lMg").find(char(D.x%10*10+D.y%10+29)))+z]-32
#define B(z)Filter::Is##z
Y namespace std;Y namespace BWAPI;Y S = Position;Y T = TilePosition;Y U = Unit;Y V = UnitType;Y W = int;auto&X = Broodwar;
char*a = "k-k/k*},k2q5}.w5z8uW1’1Ž1” “1‹+ˆ ‘%‰ ‡'hl—l’l•l™kŽeŒ_˜_Ž]‘]xR%R)R'R#R,X-a%^-b+[Fmbh[mZmXukuov[ookouJ/‘2Š.‰'Š4›8›'Œ>›>˜'z/12*.)/',>&>'* > ;D&“*Ž#“\"Ž!Š,Š)“,Š&•.u&”™Œ’”‘‘‘Œ›Œ˜”›Œ•™Žs–(™.‘'‘+‘0š0—(›(Œ$ŒF–)*/\"(\",\"1+1(),).#.H&s)s+s's1s.y/€,€/Ž$VB•|‘t•t•~Œ}ŠzŒtŠw˜Ž}W;”;‘;–;Œ;ŽAŽG’HŒ-˜f|(E,='=(G0F0C0=0@$/?g";
A<W>b = {12,17,31,38,56,67,78,85,94},c = {3,5,4,3,7,8,3,10,14};A<T>h;
map<T, U>i, l;map<U, U>z, q;map<U, W>y;W d, n, w, R, O, e, cc, cd, cy;char G;T f, D, k9;U v, t;A<U>m;T k(W u) {RT T(j(u * 2), j(u * 2 + 1));}
N o(U u) {m.insert(m.begin(), u);}N p(T u) {for (U z : X->getUnitsInRadius(S(u), 400, B(MineralField)))o(z), o(z);}
N s(U u) {if (q[u])o(q[u]), q.erase(u);}N g(W u, T Z = T()) {e = u; d = O; f = Z.y ? d += 99, Z : X->getBuildLocation(F[u], D, D.y == 63 ? 18 : 64);}
N I(U u, T z) { O % 48 == 0 ? u->attack(S(z)) : a; }W CC(W u) { RT C->completedUnitCount(F[u]); }
N BB(W u, T z = D) {if (O - cc > 600 && C->incompleteUnitCount(F[u]) == 0) {cc = O; U br = X->getClosestUnit(S(z), B(Worker) && (B(Idle) || B(GatheringMinerals)) && B(Owned), 400);if (L(br)) br->build(F[u], X->getBuildLocation(F[u], z, 18));}}
struct ExampleAIModule :AIModule {
 N onFrame() {
  O = X->getFrameCount();k9 = k(9);if (!O) {D = C->getStartLocation();H(i, read, &G);for (T u : X->getStartLocations())u != D ? h.push_back(u) : a;p(k9);p(D);}
  for (auto u = h.begin(); u != h.end();) u = X->isVisible(*u) && X->getUnitsOnTile(*u, B(Building) && B(Enemy)).empty() ? i[*u] = t, h.erase(u) : u + 1;
  for (U u : X->enemy()->getUnits()) !i[P] && Q.isBuilding() ? i[P] = u, h.push_back(P) : a;
  if (d&&L(l[f]) || O - d > 240) d = 0;
  for (; R < 9; R++) if (!d && !L(l[k(R)]) && O > b[R] * 99) g(c[R] + 100, k(R));
  if (!d&&w < 400 && CC(110)) 1.0*n / w > 0.8 ? g(103) : (CC(108) < 3 ? g(108) : (CC(114) && CC(116) < 8 ? g(116) : a));
  if (CC(104) == 1 && X->getUnitsInRadius(S(D), 400, B(ResourceContainer) && !B(MineralField)).size() > 1) BB(104);
  M(n > 41 && CC(100) == 1) BB(100); M(CC(104) == 1 && !X->getUnitsInRadius(S(k9), 400, B(Owned) && B(Worker) && B(Completed)).empty()) BB(104, k9);
  w = n = R = 0;
  for (U u : C->getUnits()) {
   if (Q == F[3]) { if (CC(15)+CC(0)==0) {u->move(S(D));CT;}M(U cf = X->getClosestUnit(S(P), B(Owned) && B(Flyer) && BWAPI::Filter::CanAttack)) {if (cf->isUnderAttack() && !cf->isDefenseMatrixed() && u->getEnergy() > 100 && O - cd > 30) {cd = O;u->useTech(TechTypes::Defensive_Matrix, cf);CT;} u->move(cf->getPosition());CT;}CT;}
   w += Q.supplyProvided();n += Q.supplyRequired();
   U Z = GC(200);Z && !Z->isMoving() ? y[Z] = O : w; u->isStartingAttack() ? y[u] = O : w;
   if (d&&u == v)s(u), u->getDistance(S(f)) < 200 ? u->build(F[e], f) : u->move(S(f));
   M(Q.isAddon()) G ? u->research(CF) : u->research(YG);
   M(Q.isBuilding()) { l[P] = u;
	if (!u->isTraining()) {
	 if (!d || e % 3 != 0) {
	  if ((P == D || P == k9) && X->getUnitsInRadius(S(P), 320, B(Owned) && B(Worker)).size() < 21) u->train(F[13]);
	  if (CC(2) < 12)u->train(F[2]);
	 }
	 if (!CC(3) && !C->incompleteUnitCount(F[3])) u->train(F[3]);
	 G ? u->train(F[0]) : u->train(F[15]);
	}
	if (Q == F[100]) {
	 if (P != D && P != k9)
	  if (!u->isFlying()) {if (u->isTraining()) u->cancelTrain(); u->lift();}
	  M (u->getDistance(S(k9)) > 200) u->move(S(k9));
	  M(u->isMoving()) {if (m.empty()) p(k9);u->land(k9);}
	}
	u->buildAddon(F[109]); G ? a : u->buildAddon(F[112]);
   }
   M(Q == F[13]) {!L(v) ? v = u : v;
	if (Z && (O - y[Z]) < 99 && !Z->isFlying())s(u), K != Z ? u->attack(Z) : a;
	M(u->getDistance(S(k9)) < 400) {if (u->isIdle())if (U cm = u->getClosestUnit(B(MineralField))) { u->gather(cm);CT;}}
	M(m.size() && !q[u])if (u->gather(m[0]))q[u] = m[0], m.erase(m.begin()); M(1)u->move(S(k(2)));
	M(K&&K->getResources() && K != q[u])u->gather(q[u]);
   }
   M(O - y[u] < 4); M(O % 2500 == 0 && u->isFlying())z[u] = u; M(h.empty())O % 500 == 0 ? I(u, T(rand()%2*X->mapWidth(), rand()%2*128)) : a;
   M((G ? CC(0) > 7 : CC(15) > 4) && z[u])I(u, h[0]); M(1)I(u, D);
   if(Q == F[0])if (C->hasResearched(CF) && !u->isCloaked() && u->isUnderAttack()) u->useTech(CF);			
   if (Q == F[15])if (L(Z))if (Z->getType().airWeapon() != WeaponTypes::None && Z->getHitPoints() > 99 || Z->getType() == F[66])if (C->hasResearched(YG) && u->getEnergy() > 150 && O - cy > 80) {cy = O;u->useTech(YG, Z);CT;}
   if (Q == F[0] || Q == F[2] || Q == F[15])if (U ZZ = GC(400))if (L(ZZ)) K != ZZ ? u->attack(ZZ) : a;
  }
 }
 N onUnitComplete(U u) {Q == F[104] ? o(u), o(u) : a;}
 N onEnd(bool u) {H(o, write, u != !G ? "1" : "\0");}
};