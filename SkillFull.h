#include<BWAPI.h>
#include<fstream>
#include "BWEB/BWEB.h"
#include "BWEM/bwem.h"

#define A vector
#define C X->self()
#define NT UnitTypes::
#define E NT allUnitTypes()
#define F A<V>(E.begin(),E.end())
#define XE X->enemy()
#define GB X->getBuildLocation
#define GC ->getClosestUnit
#define GI X->getUnitsOnTile
#define GR X->getUnitsInRadius
#define K u->getOrderTarget()
#define mh X->mapHash()
#define M else if
#define N void
#define P u->getTilePosition()
#define gt ->getType()
#define gh ->getHitPoints()
#define gs ->getShields()
#define Q u gt
#define L(z)(z&&z->exists()&&z->isDetected())
#define Y using
#define RT return
#define CT continue
#define BK break
#define j(z)(unsigned char)a[si[string("h5J€n{7f r\",!m").find(char(mh[0]%10*10+mh[1]%10+32))]+z]-32
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
#define dm "\n"
#define R1 Races::Protoss
#define R2 Races::Terran
#define R3 Races::Zerg
#define ER XE->getRace()
#define NW WeaponTypes::None
#define XF X->getFrameCount()
Y namespace std; Y namespace BWAPI; Y S = Position; Y T = TilePosition; Y U = Unit; Y V = UnitType; Y W = int; auto&X = Broodwar;
Y namespace BWEB;
char*a = "s6n-r0)ˆ0‘,Ž]xi“`–[FU*`&uJj[w_'z(‹&ŽD&2*&.u&Ž$•,s–Ž“–›F–+’'›H&,#(,”&u*‚%˜•‘u—~'—>”I˜$(+>%G"; // encoded TilePosition
W CC(W u) { RT C->completedUnitCount(F[u]); } // count my completed units
W CI(W u) { RT C->incompleteUnitCount(F[u]); } // count my incompleted units (being trained, morphed, etc).
W CL(W u) { RT CC(u) + CI(u) + (u == 123 ? CC(124) : 0); } // count my total units
A<W>b = { 22,4,60 }, c = { 123,134,133 }; // build order timing and what to build
A<W>si = { 0,20,40,58,76,94,116,140,156,174,190,206,222,238,256 }; // starting index specific to map
A<T>h; // target queue (vector)
map<T, U>i; // target queue (map)
map<U, U>z, q; // map of units that are assigned to attack, map of worker gather assignments
map<U, W>y; // map of latest frame when an enemy unit either attacked or repaired or a friendly unit started an attack
W n, np, w, wp, x, xp, R, O, cc, co, cm; // current used supply, its prevVal, current max supply, its prevVal, current loaded overlords count, its prevVal, iterator for 'b' and 'c', frame count, guard frame used in 'BB', guard frame used in morphing overlords, my created mutalisk counter
W G; // build selector
A<W> GS(40, 0); // build stats
T D, kn; // my start location, my nat location
U t; A<U>m; // nullptr unit, queue of available gather targets (mineral patches and vespene geysers)
T k(W u) { RT T(j(u * 2), j(u * 2 + 1)); } // Get prescribed TilePosition
N o(U u) { m.insert(m.begin(), u); } // Add resources to gather
N p(T u) { for (U z : GR(S(u), 400, BM))o(z), o(z); } // add all mineral patches close to a position to the queue
N s(U u) { if (q[u])o(q[u]), q.erase(u); } // remove a worker from a gather assignment
N I(U u, T z) { O % 48 == 0 ? ua(S(z)) : a; } // attack the given target with the given unit unless it is already doing so
N BB(W u, T b0 = D, T z = D) { if (O - cc > (b0 == kn ? 600 : 240)) { cc = O; U br = X GC(S(z), BW && (B(Idle) || B(GatheringMinerals)) && BO, 400); L(br) ? br->build(F[u], b0 == D ? GB(F[u], z, 18) : b0) : a; } } // Morph into a building

template<bool withAdj>
W distSq(const S &pos1, const U &unit2) {
	if (withAdj) {
		W xDist = unit2->getLeft() - pos1.x;
		if (xDist < 0)
		{
			xDist = pos1.x - (unit2->getRight() + 1);
			if (xDist < 0) xDist = 0;
		}

		W yDist = unit2->getTop() - pos1.y;
		if (yDist < 0)
		{
			yDist = pos1.y - (unit2->getBottom() + 1);
			if (yDist < 0) yDist = 0;
		}
		RT xDist * xDist + yDist * yDist;
	}
	else {
		S pos2 = unit2->getPosition();
		RT(pos1.x - pos2.x) * (pos1.x - pos2.x) + (pos1.y - pos2.y) * (pos1.y - pos2.y);
	}
} // distSq()

int GetAttackPriority(U u) {
	if (Q == NT Zerg_Egg || Q == NT Zerg_Larva || Q == NT Protoss_Interceptor) RT - 1; // neglect
	if (Q.groundWeapon() != NW && !Q.isFlyer()) RT 11; 	// highest priority is something that can attack us or aid in combat
	M(Q.isSpellcaster() && !Q.isFlyer()) RT 10;
	M(Q.airWeapon() != NW && !Q.isFlyer()) RT 9;
	M(Q.isWorker()) RT 8;
	M(Q == NT Protoss_Photon_Cannon || Q == NT Zerg_Sunken_Colony) RT 7;	// Special buildings
	M(Q.groundWeapon() != NW && Q.isFlyer()) RT 6;
	M(Q.isRefinery()) RT 5;
	M(Q.isResourceDepot()) RT 4;
	M(Q.gasPrice() > 0) RT 3; // Buildings that cost gas
	M(Q.mineralPrice() > 0) RT 2;
	M(Q.isNeutral()) RT 1;
	else RT 1;
} // GetAttackPriority

U FindTarget(U attacker)
{
	S attackerPos = attacker->getPosition();
	V attackerType = attacker gt;
	W attackerRange = attackerType.sightRange();
	U target = NULL;
	U targetUnit = NULL;

	// if target is in attack range, attack it first
	if (targetUnit != NULL && distSq<true>(attackerPos, targetUnit) < attackerRange * attackerRange) target = targetUnit;
	else
	{
		BWAPI::Unitset nearbyEnemys = GR(attackerPos, attackerRange, BE); // Using minimum sight range as search radius; Working as intended

		int hightPriority = -99999;
		int lowHealth = 99999;

		for (auto u : nearbyEnemys)
		{
			if (!u->isDetected() && u->isVisible()) CT;
			if (Q == NT Zerg_Larva && u->isVisible()) CT;
			if (attackerType.airWeapon() == NW && Q.isFlyer()) CT;
			if (attackerType.groundWeapon() == NW && !Q.isFlyer()) CT;
			if (targetUnit != NULL && !Q.canAttack()) CT; //if has target, do not attack the building first

			int enemyPriority = GetAttackPriority(u);
			if (enemyPriority >= 0) {
				if (enemyPriority > hightPriority
					|| (enemyPriority == hightPriority && u->getHitPoints() < lowHealth)
					|| target == NULL)
				{
					hightPriority = enemyPriority;
					lowHealth = u->getHitPoints();
					target = u;
				}
			}
		}
	}
	RT target;
} // FindTarget()

N SmartMove(U attacker, S targetPosition)
{
	if (attacker->getLastCommandFrame() >= XF - 10) RT; // if we have issued a command to this unit already this frame, ignore this one
	BWAPI::UnitCommand currentCommand(attacker->getLastCommand()); 	// get the unit's current command
	if ((currentCommand.getType() == BWAPI::UnitCommandTypes::Move) // if we've already told this unit to attack this target, ignore this command
		&& (currentCommand.getTargetPosition() == targetPosition)
		&& (XF - attacker->getLastCommandFrame() < 10)
		&& (attacker->isMoving())) RT;

	// Special case: Zerg_Lurker
	if (attacker gt == NT Zerg_Lurker)
		if (GR(attacker->getPosition(), 6 * 32, BE && !B(Invincible) && !BF).empty())
			if (!attacker->isAttacking())
				if (attacker->isBurrowed()) {
					attacker->unburrow();
					RT;
				}

	attacker->move(targetPosition); // if nothing prevents it, move to the target
} // SmartMove

N SmartAttack(U attacker, U target)
{
	if (attacker == NULL || target == NULL) RT;
	if (attacker->isIrradiated() || attacker->isUnderStorm()) SmartMove(attacker, S(D));
	// if we have issued a command to this unit already this frame, ignore this one
	if (attacker->getLastCommandFrame() >= XF - 5) RT;

	int attackCD = 10;
	// Data from: https://docs.google.com/spreadsheets/d/1bsvPvFil-kpvEUfSG74U3E5PLSTC02JxSkiR8QdLMuw/edit#gid=0
	BWAPI::UnitType attackerType = attacker gt;
	if (attackerType == NT Zerg_Zergling) attackCD = 5;
	M (attackerType == NT Zerg_Hydralisk) attackCD = 3;
	M (attackerType == NT Zerg_Ultralisk) attackCD = 15;

	BWAPI::UnitCommand currentCommand(attacker->getLastCommand()); // get the unit's current command
	if (currentCommand.getType() == BWAPI::UnitCommandTypes::Attack_Unit &&	currentCommand.getTarget() == target
		&& (XF - attacker->getLastCommandFrame() <= attackCD)) RT;

	if (target->isFlying() && attacker->getAirWeaponCooldown() != 0 || !target->isFlying() && attacker->getGroundWeaponCooldown() != 0) RT;

	// Special case: Zerg_Lurker
	if (attacker gt == NT Zerg_Lurker)
		if (!attacker->isBurrowed()) {
			attacker->burrow();
			RT;
		}

	attacker->attack(target); // if nothing prevents it, attack the target
} // SmartAttack

bool me4Pool = false;

struct ExampleAIModule :AIModule {
	N onStart() {
		D = C->getStartLocation();
		X->setCommandOptimizationLevel(1);

		ifstream mf("bwapi-data/read/" + XE->getName() + ".txt");
		if (mf) {
			W td = -1; W lc; while (mf >> lc) { ++td; GS[td] = lc; } mf.close();

			A<W> feasibleBOs;
			switch (ER) {
			case R1: feasibleBOs = { 1,2,3,4,5 }; BK;
			case R2: feasibleBOs = { 6,7,8,9,10 }; BK;
			case R3: feasibleBOs = { 11,12,13,14,15 }; BK;
			default: feasibleBOs = { 11,12,13,14,15 }; BK;
			}
			G = feasibleBOs[0];

			int betaDistributionParamAlpha = 2;
			int betaDistributionParamBeta = betaDistributionParamAlpha;
			double winratePosteriorMeanBest = 0;
			std::unordered_map<int, double> boAndStats;

			for (int iBOstats : feasibleBOs)
			{
				double winratePosteriorMean = (betaDistributionParamAlpha + GS[iBOstats * 2 - 1]) /
					double(betaDistributionParamAlpha + betaDistributionParamBeta + GS[iBOstats * 2 - 2]);

				boAndStats[iBOstats] = winratePosteriorMean;
				if (winratePosteriorMean > winratePosteriorMeanBest) {
					G = iBOstats;
					winratePosteriorMeanBest = winratePosteriorMean;
				}
			}

			int buildOrderMngrIDCopy = G;
			if (rand() % 100 + 1 > int(winratePosteriorMeanBest * 100)) {
				double rewardPlusNoiseBest = 0;
				double expDistParamLambda = 1.0;

				for (int iBO : feasibleBOs)
				{
					double winRateReward = boAndStats[iBO];
					double expNoise = -log(rand() / double(RAND_MAX)) / expDistParamLambda;
					double rewardPlusNoise = winRateReward + expNoise - 0.1 * (iBO == buildOrderMngrIDCopy);
					if (rewardPlusNoise > rewardPlusNoiseBest) {
						G = iBO;
						rewardPlusNoiseBest = rewardPlusNoise;
					}
				}
			}
		}
		else {
			switch (ER) {
			case R1: G = 1; BK;
			case R2: G = 6; BK;
			case R3: G = 11; BK;
			default: G = 1; BK;
			}
		}

		me4Pool = G % 5 == 1;
		for (T u : X->getStartLocations())u != D ? h.push_back(u) : a; p(kn); p(D);

		BWEM::Map::Instance().Initialize();
		BWEB::Map::onStart();
	} // N onStart()
	
	N onFrame() {
		// Initializing...
		O = XF; //kn = k(0);

		kn = BWEB::Map::getNaturalTile();
		X << kn.x << ", " << kn.y << endl;

		// Updating target queue...
		for (auto u = h.begin(); u != h.end();) u = X->isVisible(*u) && GI(*u, B(Building) && BE).empty() ? i[*u] = t, h.erase(u) : u + 1;
		for (U u : XE->getUnits())!i[P] && Q.isBuilding() ? i[P] = u, h.push_back(P) : a;

		// Constructing my buildings...
		for (; R < 3; R++) if (CL(c[R]) < (c[R] == 123 ? 2 : 1) && n >= b[R] * 2) { BB(c[R], D); BK; }
		if (n > (me4Pool ? 45 : 23) && CL(140) < (W)GR(S(D), 400, B(ResourceContainer) && !BM).size()) BB(140); // Building extractor(s) near my main...
		M(CC(140) == 1 && !GR(S(kn), 400, BO&&BW&&B(Completed)).empty()) BB(140, D, kn); // Building extractor(s) near my nat...
		M(CL(c[2])) CL(123) < 5 ? BB(123) : (CL(130) ? a : BB(130)); // More hatches when tech is ready
		M(me4Pool && CL(123) < 2 && C->minerals() > 351) BB(123); // More hatches when possible

		// Resetting...
		wp = w; np = n; xp = x; w = n = x = R = 0; map<U, W>es;

		// Regularizing my units' behaviors...
		for (U u : C->getUnits()) {
			if (!u->exists() || !u->isCompleted() || u->isMaelstrommed() || u->isStasised() || u->isLoaded() || u->isStuck()) CT;

			// Updating supply info...
			w += Q.supplyProvided(); n += Q.supplyRequired();
			U Z = u GC(BE, 200); Z && !Z IM ? y[Z] = O : w; u->isStartingAttack() ? y[u] = O : w;

			// Upgrading/Researching/Morphing...
			if (Q.isBuilding()) { P == D ? (!CL(124) ? ut(F[124]) : (!CL(125) ? ut(F[125]) : a)) : a; up(AG); up(UT Metabolic_Boost); up(VS); up(PC); }

			// Managing larvae...
			M(Q == F[34]) {
				if (wp < 400) { if (CC(c[2]) && 1.0*np / wp > 0.8)O - co > 200 ? co = O, ut(F[41]) : a; M(np > 17 && wp - np < 4)O - co > 601 ? co = O, ut(F[41]) : a; }
				for (S ip : {S(D), S(kn)}) if (ud(ip) < 128 && (W)GR(ip, 320, BO&&BW).size() < (me4Pool ? 4 : 19) && CC(40) < 38) np > b[0] * 2 && C->minerals() < 352 && GI(kn, BO&&B(Building)).empty() ? a : ut(F[40]);
				CC(125) && !HU(AG) && !C->isUpgrading(AG) ? a : C->gas() > 151 ? ut(F[42]) : ut(F[36]);
			}

			// Managing drones...
			M(Q == F[40]) {
				if (Z && (O - y[Z]) < 99 && !Z IF)s(u), K != Z ? ua(Z) : a;
				M(ud(S(kn)) < 320 && CL(123) > 1) { if (u->isIdle())if (U cm = u GC(BM)) { ug(cm); CT; } }
				M(m.size() && !q[u])if (ug(m[0]))q[u] = m[0], m.erase(m.begin()); M(1)um(S(D));
				M(K&&K->getResources() && K != q[u])ug(q[u]);
			}

			// Managing overlords...
			M(Q == F[41]) {
				if (HU(VS) && HU(PC) && u gh > 159 && cm > 2) {
					if (!GR(S(P), 160, BE && !BF).empty()) { NL ? u->unload(u->getLoadedUnits().begin()._Ptr->_Myval) : um(S(D)); }
					M(U cz = u GC(BO && !BW && !BF && !B(Loaded) && Filter::CanAttack, 256))!GR(cz->getPosition(), 99, BO&&B(ResourceDepot)).empty() ? (NL < 8 ? u->load(cz) : (!u IM&&xp > 2 ? (SM) : a)) : a;
					M(NL > 3)x++, !u IM&&xp > 2 ? SM : a;
					M(NL < 4)um(S(D));
				}
				GR(S(kn), 250, BO).empty() && !NL ? um(S(kn)) : a;
			}

			// Prohibitting issuing commands too often
			M(O - y[u] < 4);

			// Nudging stuck units
			M(O % 2500 == 0 && u IF)z[u] = u; M(h.empty())O % 500 == 0 ? I(u, RP) : a;

			// Setting our attack condition
			M(me4Pool && CC(36) > 5 || (CC(42) > 3) && z[u])I(u, h[0]); M(1)I(u, D);

			// Managing lings and mutas
			if (Q == F[36] || Q == F[42])if (U ZZ = FindTarget(u)) L(ZZ) ? SmartAttack(u, ZZ) : a;

			// Managing scourges
			if (Q == F[46])if (U ZZ = u GC(BE&&BF, 400)) L(ZZ) && es[ZZ] <= (ZZ gh + ZZ gs) / 110 ? es[ZZ]++, ua(ZZ) : a;
		}
	}
	N onUnitComplete(U u) { Q == F[140] ? o(u), o(u) : (Q == F[42] || Q == F[46] ? cm++ : 0); }
	N onEnd(bool u) {
		std::ostringstream mfo;
		for (int ki = 0; ki < (W)GS.size(); ++ki) {
			if (ki % 2 == 0 && G == ki / 2 + 1 || ki % 2 && u && G == (ki + 1) / 2) { mfo << GS[ki] + 1 << dm; CT; }
			mfo << GS[ki] << dm;
		}

		ofstream mf("bwapi-data/write/" + XE->getName() + ".txt", std::ofstream::trunc);
		if (mf)
		{
			mf << mfo.str();
			mf.flush();
		}
		mf.close();
	}
};
