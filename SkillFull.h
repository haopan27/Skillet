#include<BWAPI.h>
#include<fstream>
#include "BWEM/bwem.h"
#include "BWEB/BWEB.h"
#include "BattleCommander.h"

#define A vector
#define C X->self()
#define E UnitTypes::allUnitTypes()
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
#define B(z)Filter::Is##z
#define BE B(Enemy)
#define BF B(Flying)
#define BM B(MineralField)
#define BO B(Owned)
#define BR B(ResourceDepot)
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
#define ur u->research
#define uu u->useTech
#define HU C->getUpgradeLevel
#define UT UpgradeTypes::
#define AG UT Adrenal_Glands
#define PC UT Pneumatized_Carapace
#define VS UT Ventral_Sacs
#define HR C->hasResearched
#define TT TechTypes::
#define LA TT Lurker_Aspect
#define RP T(rand()%2*X->mapWidth(),rand()%2*128)
#define SM h.empty()?um(S(RP)):um(S(h[0]))
#define NL u->getLoadedUnits().size()
#define dm "\n"
#define R1 Races::Protoss
#define R2 Races::Terran
#define R3 Races::Zerg
#define ER XE->getRace()
#define NW WeaponTypes::None
#define NP Positions::None
#define NT TilePositions::None
#define XF X->getFrameCount()
#define me4or5PoolEarly me4or5Pool && CL(123) < 2 && !CC(140)
Y namespace std; Y namespace BWAPI; Y S = Position; Y T = TilePosition; Y U = Unit; Y V = UnitType; Y W = int; Y H = bool; Y J = double; auto&X = Broodwar;

W CC(W u) { RT C->completedUnitCount(F[u]); } // count my completed units
W CI(W u) { RT C->incompleteUnitCount(F[u]); } // count my incompleted units (being trained, morphed, etc).
W CL(W u) { RT CC(u) + CI(u) + (u == 123 ? CC(124) : 0); } // count my total units
W countMyMorphingUnits(V v) {
	int res = 0;
	for (U u : C->getUnits()) {
		if (u->getType() == Zerg_Egg && u->getBuildType() == v)
			res++;
	}
	return res;
}

A<A<W>> b = {
	{22,4,60},		// ZvP 4 Pool
	{9,9,15},		// ZvP 1baseLurker
	{12,12,12,20},	// ZvP 2baseMuta
	{12,12,12,16},	// ZvP 2baseHydra
	{0},			// ZvP --
	{22,4,60},		// ZvT 4 Pool
	{9,9,15},		// ZvT 1baseLurker
	{12,12,12,20},	// ZvT 2baseMuta
	{12,12,12,16},	// ZvT 2baseHydra
	{0},			// ZvT --
	{22,4,60},		// ZvZ 4 Pool
	{7,16,17},      // ZvZ 7 Pool
	{9,9,16}, 	    // ZvZ 1baseMuta
	{9,11,16,17},   // ZvZ 1baseHydra
	{0}				// ZvZ --
},
c = {
	{123,134,133},		// ZvP 4 Pool
	{134,140,127},		// ZvP 1baseLurker
	{123,134,140,133},	// ZvP 2baseMuta
	{123,134,140,127},	// ZvP 2baseHydra
	{0},				// ZvP --
	{123,134,133},		// ZvT 4 Pool
	{134,140,127},		// ZvT 1baseLurker
	{123,134,140,133},	// ZvT 2baseMuta
	{123,134,140,127},	// ZvT 2baseHydra
	{0},				// ZvT --
	{123,134,133},		// ZvZ 4 Pool
	{134,140,127},		// ZvZ 7 Pool
	{134,140,133},		// ZvZ 1baseMuta
	{123,134,140,127},	// ZvZ 1baseHydra
	{0}					// ZvZ --
};
A<W>bG, cG;// build order timing and what to build ==> pertaining to a specific build order
A<W>si = { 0,20,40,58,76,94,116,140,156,174,190,206,222,238,256 }; // starting index specific to map
A<T>h; // target queue (vector)
map<T, U>i; // target queue (map)
map<U, U>z, q; // map of units that are assigned to attack, map of worker gather assignments
map<U, W>y; // map of latest frame when an enemy unit either attacked or repaired or a friendly unit started an attack
map<J, T>distsAndBases;
H me4or5Pool = false;
H me1BaseLurker = false;
H me2BaseMuta = false;
H me2BaseHydra = false;
H meFastNat = false;
H me1BaseMuta = false;
H me1BaseHydra = false;
H me7Pool = false;
H meCOEP = false;
BWEM::Map & bwemMapInstance = BWEM::Map::Instance();
W myScoutID = -99;
map<T, H> myScoutTargetsAndStatus;
T hisD = NT;
T closestD = NT;
map<W, W> myUnitsCreated;
H hisDKilled = false;
H myAttackCondition = false;
S myCP = NP;
S myknCP = NP;
W nToRetreat = 999;
A<V> myOrders = {};
V m_lastUnitType = UnitTypes::None;
W m_orderRequestedSince = 0;

W n, np;	// current used supply, its prevVal
W w, wp;	// current max supply, its prevVal
W x, xp;	// current loaded overlords count, its prevVal
W R, O;		// iterator for 'b' and 'c' (both), frame count
W cc, co;	// guard frame used in 'BB', guard frame used in morphing overlords
W cs, ck3, ckn;  // guard frame to avoid calling too many scouts, guard frame to avoid calling too many overlords to k3/kn
W G; // build selector
A<W> GS(40, 0); // build stats
T D, kn, k3; // my start location, my nat location
U t; A<U>m; // nullptr unit, queue of available gather targets (mineral patches and vespene geysers)
N o(U u) { m.insert(m.begin(), u); } // Add resources to gather
N p(T u) { for (U z : GR(S(u), 400, BM))o(z), o(z); } // add all mineral patches close to a position to the queue
N s(U u) { if (q[u])o(q[u]), q.erase(u); } // remove a worker from a gather assignment
N I(U u, T z) { O % 48 == 0 ? ua(S(z)) : "a"; } // attack the given target with the given unit unless it is already doing so
N BB(W u, T b0 = D, T z = D) { // b0: build location search center; z: builder search center
	if (O - cc > (b0 == kn ? 600 : 240)) {
		cc = O; U br = X GC(S(z), BW && (B(Idle) || B(GatheringMinerals) || !B(Moving)) && !B(CarryingMinerals) && !B(GatheringGas) && BO, 400);

		//if (L(br)) X << "Building: " << F[u] << ", buildLoc: " << GB(F[u], b0, 18) << endl;

		L(br) ? br->build(F[u], GB(F[u], b0, 18)) : "a";
	}
} // Morph into a building around b0 choosing a worker near z


template<H withAdj>
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
	if (Q == F[35] || Q == F[34] || Q == F[67]) RT - 1; // neglect Zerg_Egg, Zerg_Larva, Protoss_Interceptor
	if (Q.groundWeapon() != NW && !Q.isFlyer()) RT 11; 	// highest priority is something that can attack us or aid in combat
	M(Q.isSpellcaster() && !Q.isFlyer()) RT 10;
	M(Q.airWeapon() != NW && !Q.isFlyer()) RT 9;
	M(Q.isWorker()) RT 8;
	M(Q == F[150] || Q == F[137]) RT 7;	// Static defense: Protoss_Photon_Cannon, Zerg_Sunken_Colony
	M(Q.groundWeapon() != NW && Q.isFlyer()) RT 6;
	M(Q.isRefinery() || Q == F[146]) RT 5; // Protoss_Pylon
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
		Unitset nearbyEnemys = GR(attackerPos, attackerRange, (BE || B(Neutral)) && !B(Invincible)); // Using minimum sight range as search radius; Working as intended

		int hightPriority = -99999;
		int lowHealth = 99999;

		for (auto u : nearbyEnemys)
		{
			if (!u->isDetected() && u->isVisible()) CT;
			if (Q == F[134] && u->isVisible()) CT;
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
	// Special case: Zerg_Lurker
	if (attacker gt == F[97])
		if (attacker->isBurrowed()) {
			if (GR(attacker->getPosition(), 192, BE && !BF).empty())
				attacker->unburrow();
		}

	if (attacker->getLastCommandFrame() >= XF - 10) RT; // if we have issued a command to this unit already this frame, ignore this one
	BWAPI::UnitCommand currentCommand(attacker->getLastCommand()); 	// get the unit's current command
	if ((currentCommand.getType() == BWAPI::UnitCommandTypes::Move) // if we've already told this unit to attack this target, ignore this command
		&& (currentCommand.getTargetPosition() == targetPosition)
		&& (XF - attacker->getLastCommandFrame() < 10)
		&& (attacker->isMoving())) RT;

	attacker->move(targetPosition); // if nothing prevents it, move to the target
} // SmartMove

N SmartAttack(U attacker, U target)
{
	if (attacker == NULL || target == NULL) RT;
	if (attacker->isIrradiated() || attacker->isUnderStorm()) SmartMove(attacker, S(D));

	// Special case: Zerg_Lurker
	if (attacker gt == F[97] && !attacker->isBurrowed()) {
		if (!GR(attacker->getPosition(), 192, BE && !BF).empty()) attacker->burrow();
	}

	// if we have issued a command to this unit already this frame, ignore this one
	if (attacker->getLastCommandFrame() >= XF - 5) RT;

	int attackCD = 10;
	// Data from: https://docs.google.com/spreadsheets/d/1bsvPvFil-kpvEUfSG74U3E5PLSTC02JxSkiR8QdLMuw/edit#gid=0
	BWAPI::UnitType attackerType = attacker gt;
	if (attackerType == F[36]) attackCD = 5;
	M(attackerType == F[37]) attackCD = 3;
	M(attackerType == F[38]) attackCD = 15;
	M(attackerType == F[97]) attackCD = 2;

	BWAPI::UnitCommand currentCommand(attacker->getLastCommand()); // get the unit's current command
	if (currentCommand.getType() == BWAPI::UnitCommandTypes::Attack_Unit &&	currentCommand.getTarget() == target
		&& (XF - attacker->getLastCommandFrame() <= attackCD)) RT;

	if (target->isFlying() && attacker->getAirWeaponCooldown() != 0 || !target->isFlying() && attacker->getGroundWeaponCooldown() != 0) RT;

	attacker->attack(target); // if nothing prevents it, attack the target
} // SmartAttack

N GetMyNaturalAndOtherBases(BWEM::Map & mapBWEM) {
	auto distBest = DBL_MAX;
	auto distBestD = DBL_MAX;
	for (auto &area : mapBWEM.Areas()) {
		if (area.AccessibleNeighbours().empty()) CT;
		for (auto &base : area.Bases()) {
			// Must have gas, be accesible and at least 5 mineral patches
			if (base.Geysers().empty() || base.Minerals().size() < 5) CT;

			const auto dist = BWEB::Map::getGroundDistance(base.Center(), S(D) + S(64, 48));
			distsAndBases[dist] = base.Location();
			if (dist < distBest && !base.Starting()) {
				distBest = dist;
				kn = base.Location();
			}

			if (dist < distBestD && base.Starting() && base.Location() != D) {
				distBestD = dist;
				closestD = base.Location();
			}
		}
	}
	distsAndBases.erase(distsAndBases.find(distBest)); // Exclude my natural
	distsAndBases.erase(distsAndBases.find(0.0)); // Exclude my starting main
	if (X->getStartLocations().size() == 2)
		hisD = closestD;

	k3 = distsAndBases.begin()._Ptr->_Myval.second;
}

N GoScouting(U u) {
	H shouldGoofAround = false;

	for (T sl : X->getStartLocations()) {
		if (sl == D) CT;

		if (me4or5Pool && (Q != F[41] && sl == closestD || Q == F[41] && sl != closestD)) CT; // skip the first target which is to be covered by the overlord.

		S slPos = S(sl);
		if (!GR(slPos, 720, BE && !BW).empty()) {
			if (hisD == NT) hisD = sl;
			myScoutTargetsAndStatus[sl] = true; shouldGoofAround = true; BK;
		}

		if (!myScoutTargetsAndStatus[sl]) { // not visited yet
			if (distSq<true>(slPos, u) < 50176 && GR(slPos, 224, BE && !BW).empty()) { myScoutTargetsAndStatus[sl] = true; CT; }
			M(distSq<true>(slPos, u) >= 50176) { SmartMove(u, slPos); RT; }
		}
	}

	if (shouldGoofAround) {
		if (u->getHitPoints() == u->getInitialHitPoints()) {
			if (Q != F[41]) {
				if (U ZZ = FindTarget(u))
					L(ZZ) ? SmartAttack(u, ZZ) : "a";
			}
			M(hisD != NT) SmartMove(u, S(hisD));
		}
		else {
			if (u->isUnderAttack()) SmartMove(u, S(D));
			M(hisD != NT) SmartMove(u, S(hisD));
		}
	}
}

struct UnitInfo {
	V uType;
	J uVelX;
	J uVelY;
	W uEHP;
};

struct ExampleAIModule :AIModule {
	N onStart() {
		D = C->getStartLocation();
		X->setCommandOptimizationLevel(1);

		ifstream mf("bwapi-data/read/" + XE->getName() + ".txt");
		if (mf) {
			W td = -1; W lc; while (mf >> lc) { ++td; GS[td] = lc; } mf.close();

			A<W> feasibleBOs;
			switch (ER) {
			case R1: feasibleBOs = { 1,2,3,4 }; BK;
			case R2: feasibleBOs = { 6,7,8,9 }; BK;
			case R3: feasibleBOs = { 11,12,13,14 }; BK;
			default: feasibleBOs = { 11,12,13,14 }; BK;
			}
			G = feasibleBOs[0];

			int betaDistributionParamAlpha = 2;
			int betaDistributionParamBeta = betaDistributionParamAlpha;
			J winratePosteriorMeanBest = 0;
			std::unordered_map<int, J> boAndStats;

			for (int iBOstats : feasibleBOs)
			{
				J winratePosteriorMean = (betaDistributionParamAlpha + GS[iBOstats * 2 - 1]) /
					J(betaDistributionParamAlpha + betaDistributionParamBeta + GS[iBOstats * 2 - 2]);

				boAndStats[iBOstats] = winratePosteriorMean;
				if (winratePosteriorMean > winratePosteriorMeanBest) {
					G = iBOstats;
					winratePosteriorMeanBest = winratePosteriorMean;
				}
			}

			int buildOrderMngrIDCopy = G;
			if (rand() % 100 + 1 > int(winratePosteriorMeanBest * 100)) {
				J rewardPlusNoiseBest = 0;
				J expDistParamLambda = 1.0;

				for (int iBO : feasibleBOs)
				{
					J winRateReward = boAndStats[iBO];
					J expNoise = -log(rand() / J(RAND_MAX)) / expDistParamLambda;
					J rewardPlusNoise = winRateReward + expNoise - 0.1 * (iBO == buildOrderMngrIDCopy);
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

		bG = b[G - 1];
		cG = c[G - 1];
		me4or5Pool = G % 5 == 1;
		me1BaseLurker = G == 2 || G == 7;
		me2BaseMuta = G == 3 || G == 8;
		me2BaseHydra = G == 4 || G == 9 || G == 14;
		me7Pool = G == 12;
		me1BaseMuta = G == 13;
		// me1BaseHydra = G == 14;
		meFastNat = me2BaseMuta || me2BaseHydra;
		meCOEP = G % 5 == 0;

		bwemMapInstance.Initialize();
		bwemMapInstance.FindBasesForStartingLocations();
		GetMyNaturalAndOtherBases(bwemMapInstance);
		BWEB::Map::onStart();
		myCP = S(BWEB::Map::getMainChoke()->Center());
		myknCP = S(BWEB::Map::getNaturalChoke()->Center());

		for (T u : X->getStartLocations())u != D ? h.push_back(u) : "a"; p(k3); p(kn); p(D);

		for (T sl : X->getStartLocations())
			myScoutTargetsAndStatus[sl] = false;
	} // N onStart()

	N onFrame() {
		// DEV X << ... << endl;

		// Initializing...
		O = XF;
		H myNatBuilt = !GR(S(kn), 160, BO && BR).empty();
		H my3rdBuilt = !GR(S(k3), 160, BO && BR).empty();

		if (me4or5Pool)
			myAttackCondition = true;
		M(me1BaseLurker)
			myAttackCondition = CC(97) > 2;
		M(me2BaseMuta)
			myAttackCondition = CC(42) > 3;
		M(me2BaseHydra)
			myAttackCondition = CC(37) > 15;
		M(me1BaseMuta)
			myAttackCondition = CC(133) ? CC(42) > 5 : CC(36) > 9;
		M(me1BaseHydra)
			myAttackCondition = CC(127) ? CC(37) > 5 : CC(36) > 15;
		M(me7Pool)
			myAttackCondition = CC(127) ? CC(37) > 5 : CC(36) > 15;
		M(meCOEP) // TODO: what would be a more reasonable myAttackCondition other than some magic number?
			myAttackCondition = CC(123) + CC(124) + CC(125) >= 3;

		// Updating his starting base status...
		if (hisD != NT && !hisDKilled)
			if (!GR(S(hisD), 160, BO).empty() && GR(S(hisD), 160, BE && BR).empty())
				hisDKilled = true;

		// Updating target queue...
		for (auto u = h.begin(); u != h.end();) u = X->isVisible(*u) && GI(*u, B(Building) && BE).empty() ? i[*u] = t, h.erase(u) : u + 1;
		for (U u : XE->getUnits())!i[P] && Q.isBuilding() ? i[P] = u, h.push_back(P) : "a";

		// COEP Starts
		if (meCOEP) {
			if (myOrders.empty()) {
				std::vector<UnitType> myFeasibleActions = myFeasibleActionsGen();
				//runCOEP(..., int popSize, int numGenerations, int championSize, double crossOverRate, double mutationRate)
				myOrders = runCOEP(myFeasibleActions, 12, 12, 18, 0.6, 0.6);
			}
			else {
				if (m_lastUnitType == None) {
					m_lastUnitType = myOrders.front();
					m_orderRequestedSince = O;
				}
				else {
					if (myOrders.front() == m_lastUnitType) {
						if (C->minerals() >= myOrders.front().mineralPrice() && C->gas() >= myOrders.front().gasPrice()) {
							if (O - m_orderRequestedSince > myOrders.front().buildTime()) {
								myOrders.erase(myOrders.begin());
								m_lastUnitType = None;
								m_orderRequestedSince = 0;
							}
						}
						else {
							;
						}
					}
				}

				if (myOrders.front() == Zerg_Extractor)
					if (CL(140) >= CL(123))
						myOrders.erase(myOrders.begin());

				Broodwar->drawTextScreen(10, 20, "onFrame(): %s \n", myOrders.front().c_str());
			}
		}
		// COEP Ends

		// Constructing my buildings...
		if (!meCOEP) {
			for (; R < (W)cG.size(); R++) if (CL(cG[R]) < (cG[R] == 123 ? 2 : 1) && n >= bG[R] * 2) { meFastNat && cG[R] == 123 && CL(cG[R]) == 1 ? BB(cG[R], kn) : BB(cG[R], D); BK; }
			if (n > (me4or5Pool ? 45 : 23) && CL(140) < (W)GR(S(D), 320, B(ResourceContainer) && !BM).size()) BB(140); // Building extractor(s) near my main...
			M(CC(140) == 1 && CL(140) < 2 && myNatBuilt && !GR(S(kn), 320, BO&&BW&&B(Completed)).empty()) BB(140, kn, kn); // Building extractor(s) near my nat...
			M((me2BaseHydra || me2BaseMuta || me1BaseLurker) && CC(140) == 2 && CL(140) < 3 && CC(123) >= 3 && !GR(S(k3), 320, BO&&BW&&B(Completed)).empty() && !GR(S(k3), 320, B(ResourceContainer) && !BM).empty()) BB(140, k3, k3); // Building extractor(s) near my third...
			M((me2BaseHydra || me2BaseMuta) && CC(123) >= 2 && !CI(123) && CC(140) == 2 && !GR(S(k3), 320, BO).empty() && C->minerals() > 300) BB(123, k3, kn); // Expansion at third
			M(!(me2BaseHydra || me2BaseMuta || me1BaseLurker) && CL(cG[cG.size() - 1]) && CL(123) < 5 && C->minerals() > 400) BB(123); // More hatches when tech is ready
			M(me4or5Pool && CL(123) < 2 && C->minerals() > 450) BB(123); // More hatches when possible when me4or5Pool
			M(me1BaseLurker) { // Setting up nat
				//if (GR(S(kn), 160, BO&&BR).empty() && n > 21) BB(123, kn);
				//M(!GR(S(kn), 160, BO&&BW).empty() && CL(135) + CL(137) < 4 && C->minerals() > 50) BB(135, T(myknCP), D);
				if (CC(134) && CL(135) + CL(137) < 6 && C->minerals() > 100) BB(135);
				M(CL(123) == 1 && CL(123) < 2 && C->minerals() > 450) BB(123);
				M (myAttackCondition && !myNatBuilt) BB(123, kn);
				M(myAttackCondition && myNatBuilt && !my3rdBuilt) BB(123, k3);
			}
			M(me2BaseMuta || me2BaseHydra) { // Setting up nat
				if (!GR(S(kn), 160, BO&&BW).empty() && CL(135) + CL(137) < 4 && C->minerals() > 50) BB(135, kn, D);
			}
			M((me1BaseMuta || me1BaseHydra) && CL(123) < 3 && C->minerals() > 450) BB(123); // More hatches when possible
			M((me7Pool) && CL(123) < 3 && C->minerals() > 450) BB(123); // More hatches when possible
			M(O >= 34560 && GR(S(D), 320, BM).empty()) BB(135);
		}
		M(meCOEP) {
			if (GR(S(D), 256, BO&&BW).size() >= 4) {
				if (!myOrders.empty()) {
					if (myOrders.front().isBuilding()) {
						int buildingTypeToBuild = unitTypeToInt(myOrders.front());

						if (buildingTypeToBuild == 123 && CL(123) == 1) { // Expansion at nat
							BB(buildingTypeToBuild, kn);
						}
						M(buildingTypeToBuild == 123 && CL(123) == 2) { // Expansion at third
							BB(buildingTypeToBuild, k3);
						}
						M(buildingTypeToBuild == 135 && CC(123) > 1) { // Sunken at nat
							BB(buildingTypeToBuild, kn);
						}
						M(buildingTypeToBuild == 140 && CC(140) == 1 && !GR(S(D), 8 * 32, BO && Filter::GetType == Zerg_Extractor).empty()) { // Extractor at nat
							BB(buildingTypeToBuild, kn);
						}
						M(buildingTypeToBuild == 140 && CC(140) == 2 && !GR(S(D), 8 * 32, BO && Filter::GetType == Zerg_Extractor).empty() && !GR(S(kn), 8 * 32, BO && Filter::GetType == Zerg_Extractor).empty()) { // Extractor at 3rd (k3)
							BB(buildingTypeToBuild, k3);
						}
						else {
							BB(buildingTypeToBuild);
						}

						if (CI(buildingTypeToBuild))
							myOrders.erase(myOrders.begin());
					}
				}

				if (CL(135) + CL(137) < 4 && CC(123) > 1) { // MORE sunkens at nat
					BB(135, kn);
				}
			}
			else {
				if (!myOrders.empty())
					if (myOrders.front().isBuilding())
						myOrders.erase(myOrders.begin());
			}
		}

		// Resetting...
		wp = w; np = n; xp = x; w = n = x = R = 0; map<U, W>es;
		H myScoutFound = false;

		// Regularizing my units' behaviors...
		for (U u : C->getUnits()) {
			if (!u->exists() || !u->isCompleted() || u->isMaelstrommed() || u->isStasised() || u->isLoaded() || u->isStuck()) CT;

			// Updating supply info...
			w += Q.supplyProvided(); n += Q.supplyRequired();
			U Z = u GC(BE, 200); Z && !Z IM ? y[Z] = O : w; u->isStartingAttack() ? y[u] = O : w;

			// Upgrading/Researching/Morphing...
			if (Q.isBuilding()) {
				if (!me1BaseHydra && !me2BaseHydra && !me7Pool) P == D ? (!CL(124) ? ut(F[124]) : (!CL(125) ? ut(F[125]) : "a")) : "a"; // Main Hatch->Lair->Hive
				if (CC(125) && CC(134)) up(AG); // Crackling
				if ((me1BaseLurker? myAttackCondition:1) && CC(140) && CC(134)) up(UT Metabolic_Boost); // Ling speed
				if ((me1BaseLurker || me2BaseHydra) && CC(124) && CC(127)) ur(LA); // Lurker_Aspect
				//if (me2BaseMuta) { up(VS); up(PC); } // Overlord transportation and speed
				if ((me1BaseHydra || me2BaseHydra || me7Pool) && CC(127)) { up(UT Grooved_Spines); up(UT Muscular_Augments); } // Hydra range and speed upgrades
				if (meCOEP && CC(127)) { ur(LA); up(UT Grooved_Spines); up(UT Muscular_Augments); }
				ut(F[137]); // Creep->Sunken
			}

			// Managing larvae...
			M(Q == F[34]) {
				if (!meCOEP) {
					if (CC(125) && !HU(AG) && !C->isUpgrading(AG) // Crackling upgrade
						|| me1BaseLurker && (CC(124) && CC(127) && !HR(LA) && !C->isResearching(LA) || myUnitsCreated[97] > 5 && !myNatBuilt) // me1BaseLurker
						|| me7Pool && np > 11 && !CL(134) // me7Pool
						) CT; // Cut production in favor of upgrades/researches

					if (wp < 400) { if (CC(cG[2]) && 1.0*np / wp > 0.8)O - co > 200 ? co = O, ut(F[41]) : "a"; M(np > 17 && wp - np < 4)O - co > 601 ? co = O, ut(F[41]) : "a"; } // Overlords

					for (S ip : {S(D), S(kn)}) 
						if (ud(ip) < 128 && (W)GR(ip, 320, BO&&BW).size() < (me4or5PoolEarly ? 4 : 18) && CL(40) < (me4or5PoolEarly ? 4 : (myNatBuilt?36:18))) 
							ut(F[40]); // Drones

					if (me4or5Pool) ut(F[36]); // Zerglings
					M(me1BaseLurker) {
						if (!countMyMorphingUnits(Zerg_Zergling) && CC(36) < 6 * (CC(123) + CC(124))) ut(F[36]); // Zerglings (yielding priority to hydras)
						if (CC(37) < 2 * CC(140)) ut(F[37]); // Hydras
					} // Hydralisks
					M(me2BaseMuta) {
						C->gas() > 100 && CC(133) ? ut(F[42]) : ut(F[36]);
					}
					M(me2BaseHydra) {
						C->gas() > 25 && CC(127) ? ut(F[37]) : ut(F[36]);
					}
					M(me1BaseMuta) {
						C->gas() > 100 && CC(133) ? ut(F[42]) : ut(F[36]);
					}
					M(me1BaseHydra) {
						C->gas() > 25 && CC(127) ? ut(F[37]) : ut(F[36]);
					}
					M(me7Pool) {
						C->gas() > 25 && CC(127) ? ut(F[37]) : ut(F[36]);
					}
				}
				M(meCOEP) {
					if (!myOrders.empty()) {
						if (!myOrders.front().isBuilding()) {
							int unitTypeToMorph = unitTypeToInt(myOrders.front());
							
							if (C->supplyTotal() - C->supplyUsed() >= myOrders.front().supplyRequired()) {
								ut(F[unitTypeToMorph]);

								if (countMyMorphingUnits(myOrders.front()))
									myOrders.erase(myOrders.begin());
							}
							else {
								if (O - co > 630) {
									co = O;
									ut(F[41]);
								}
							}
						}
					}
				}
				CT;
			} // M(Q == F[34])

			// Managing hydras...
			M(Q == F[37]) {
				if (HR(LA))
					if (me1BaseLurker && CC(97) < 2 * CC(37)
						|| me2BaseHydra && 2 * CC(97) < CC(37)
						|| meCOEP && CC(97) < 6) 
						ut(F[97]); // into lurkers!
			}

			// Managing drones...
			M(Q == F[40]) {
				if (hisD == NT && u->getID() == myScoutID) { myScoutFound = true; GoScouting(u); CT; }

				if (hisD == NT && CL(134) && CL(40) > 3 && myUnitsCreated[40] - CC(40) <= 1 && O - cs > 240 && !u->isCarryingMinerals() && !u->isCarryingGas()) { // When it is time to scout...
					if (myScoutID < 0) { cs = O; myScoutID = u->getID(); myScoutFound = true; GoScouting(u); CT; }
				}

				if (Z && (O - y[Z]) < 99 && !Z IF)s(u), K != Z ? ua(Z) : "a";
				M((ud(S(kn)) < 320 || ud(S(k3)) < 320)&& CL(123) > 1) { if (u->isIdle() && !u->isMoving()) if (U cm = u GC(BM)) { ug(cm); CT; } } // Mining at natural/third
				M(m.size() && !q[u])if (ug(m[0]))q[u] = m[0], m.erase(m.begin()); 
				// M(1) (myNatBuilt) ? um(S(kn)) : um(S(D));
				M(K&&K->getResources() && K != q[u])ug(q[u]);
				M(ud(S(D)) < 320) {
					if (u->isIdle() && !u->isMoving() && !u->isGatheringGas() && !u->isCarryingGas()) {
						//if (U ce = u GC(BO && B(Completed) && Filter::GetType == Zerg_Extractor)) { ug(ce); CT; } 
						if (U cm = u GC(BM)) { ug(cm); CT; }
					}
				} // Refining/Mining at main
			}

			// Managing overlords...
			M(Q == F[41]) {
				/*
				// Dropping Zerglings
				if (HU(VS) && HU(PC) && u gh > 159 && myUnitsCreated[42] > 2) {
					if (!GR(S(P), 160, BE && !BF).empty()) { NL ? u->unload(u->getLoadedUnits().begin()._Ptr->_Myval) : um(myNatBuilt ? S(kn) : S(D)); }
					M(U cz = u GC(BO && !BW && !BF && !B(Loaded) && Filter::CanAttack, 256))!GR(cz->getPosition(), 99, BO&&BR).empty() ? (NL < 8 ? u->load(cz) : (!u IM&&xp > 2 ? (SM) : "a")) : "a";
					M(NL > 3)x++, !u IM&&xp > 2 ? SM : "a";
					M(NL < 4)um(myNatBuilt ? S(kn) : S(D));
				}*/

				if (me4or5Pool) {
					if (hisD == NT) { GoScouting(u); CT; }
					if (u->isUnderAttack()) SmartMove(u, S(D));
				}
				else {
					if (!me1BaseLurker) GR(S(kn), 250, BO).empty() && !NL ? um(S(kn)) : "a";

					if (me1BaseLurker) {
						if (myAttackCondition)
							if (GR(S(kn), 250, BO).empty() && !NL && O - ckn > 620) {
								ckn = O;
								um(S(kn));
							}
					}

					if (CC(123) >= 2) {
						if (GR(S(k3), 250, BO).empty() && !NL && O - ck3 > 620) {
							ck3 = O;
							um(S(k3));
						}
					}
				}
			}

			// Prohibitting issuing commands too often
			//M(O - y[u] < 4);

			// Nudging stuck units
			//M(O % 2500 == 0 && u IF)z[u] = u; M(h.empty())O % 500 == 0 ? I(u, RP) : a;

			// Managing my army: 
			//	lings,		mutas,			hydras,		lurkers
			if (Q == F[36] || Q == F[42] || Q == F[37] || Q == F[97]) {
				// Regrouping the army when raiding hisD, aka Kiting
				// SMORC'in until good combat sim
				/*
				if (hisD != NT 
					//&& !me4or5Pool
					//&& CC(123)+ CC(124)+ CC(125) < (meCOEP? 3 : 2)
					) {
					/*
					// Wait group
					int distSqToTarget = distSq<true>(S(hisD), u);
					if (distSqToTarget > 400000) {
						int others = 0;
						int maxDistSqToTargetInRaidersAround = numeric_limits<int>::min();
						for (U u2 : C->getUnits())
							if (u2->canAttack() && !u2->canGather())
								if (u2 != u)
									if (distSq<true>(u->getPosition(), u2) < 1600000)
									{
										int d = distSq<true>(S(hisD), u2);
										maxDistSqToTargetInRaidersAround = max(maxDistSqToTargetInRaidersAround, d);
										++others;
									}


						if (distSqToTarget < maxDistSqToTargetInRaidersAround)
							if (others < 6) // United we stand
								if (!u->isHoldingPosition())
									if (GR(u->getPosition(), 192, BE).empty())
									{
										u->holdPosition();
										CT;
									}
					} //------------------------------------------------------------

					// Can only retreat to starting main when far enough from enemy main and no own hatch nearby
					int distSqToTarget = distSq<true>(S(D), u);
					if (distSqToTarget > 40000 && GR(u->getPosition(), u->getType().sightRange(), BO && BR).empty()) { 
						Unitset nearbyFoes = GR(u->getPosition(), static_cast<int>(1.6 * u->getType().sightRange()), BE && !BW && BWAPI::Filter::IsDetected && BWAPI::Filter::CanAttack && BWAPI::Filter::IsCompleted);
						int totalNearbyFoesEHP = 0;
						if (!nearbyFoes.empty())
							for (const auto &iC : nearbyFoes)
								if (iC && iC->exists())
									totalNearbyFoesEHP += iC->getHitPoints() + iC->getShields();

						Unitset nearbyFriends = GR(u->getPosition(), static_cast<int>(1.6 * u->getType().sightRange()), BO && !BW && BWAPI::Filter::CanAttack);
						int totalNearbyFriendsEHP = 0;
						if (!nearbyFriends.empty())
							for (const auto &iC : nearbyFriends)
								if (iC && iC->exists())
									totalNearbyFriendsEHP += iC->getHitPoints() + iC->getShields();

						if (totalNearbyFriendsEHP < totalNearbyFoesEHP) {
							SmartMove(u, S(D)); CT;
						}
					}
				} */

				if (U ZZ = FindTarget(u))
					if (L(ZZ)) { SmartAttack(u, ZZ); CT; }

				if (!GR(S(D), 320, BE && B(Invincible)).empty()) { SmartMove(u, S(D)); CT; }

				if (hisD == NT && Q == F[36]) { GoScouting(u); CT; }
				M(!hisDKilled) {
					(myAttackCondition || np > nToRetreat) ? SmartMove(u, S(hisD)), (nToRetreat == 999 ? (nToRetreat = CC(40) + max(np - 2 * CC(40), 0) / 6) : 0) : 
						(myNatBuilt ? (GR(S(kn), 9 * 32, BO && Filter::GetType == Zerg_Sunken_Colony).empty() ? SmartMove(u, S(kn)) : SmartMove(u, u GC(BO && Filter::GetType == Zerg_Sunken_Colony)->getPosition())) : SmartMove(u, S(D)), nToRetreat == 999 ? 0 : nToRetreat = 999);
				}
				M(hisDKilled) { // Search and destroy
					if (U Z2 = FindTarget(u)) {
						if (L(Z2)) {
							distSq<true>(Z2->getPosition(), u) > u->getType().groundWeapon().maxRange() * u->getType().groundWeapon().maxRange() ? SmartMove(u, Z2->getPosition()) : SmartAttack(u, Z2);
						}
					}
					M(U Z3 = u GC(BE&&B(Building))) {
						if (L(Z3)) SmartMove(u, Z3->getPosition());
					}
					M(!u->isMoving()) {
						auto it = distsAndBases.begin();
						std::advance(it, rand() % distsAndBases.size());
						J random_key = it->first;
						SmartMove(u, S(distsAndBases[random_key]));
					}
				}
			}

			// Managing scourges (special treatment)
			if (Q == F[46])if (U ZZ = u GC(BE&&BF, 400)) L(ZZ) && es[ZZ] <= (ZZ gh + ZZ gs) / 110 ? es[ZZ]++, ua(ZZ) : "a";
		} // for (U u : C->getUnits())

		if (!myScoutFound) myScoutID = -99;
	}  // N onFrame()

	N onUnitComplete(U u) {
		Q == F[140] ? o(u), o(u) : "a";

		if (Q == F[40]) myUnitsCreated[40]++;
		M(Q == F[42]) myUnitsCreated[42]++;
		M(Q == F[46]) myUnitsCreated[46]++;
		M(Q == F[123]) myUnitsCreated[123]++;
		M(Q == F[97]) myUnitsCreated[97]++;
	}

	N onUnitDiscover(U u) {
		V uType = u->getType();
		if (!uType.isBuilding()) hisUnitIDAndType[u->getID()] = uType;
		M(uType == Protoss_Photon_Cannon ||
			uType == Terran_Bunker || uType == Terran_Missile_Turret ||
			uType == Zerg_Sunken_Colony || uType == Zerg_Spore_Colony) 
			hisBuildingPosAndType[u->getPosition()] = uType;
	}

	N onUnitDestroy(U u) {
		V uType = u->getType();
		if (!uType.isBuilding()) {
			auto it = hisUnitIDAndType.find(u->getID());
			if (it != hisUnitIDAndType.end())
				hisUnitIDAndType.erase(it);
		}
		M(uType == Protoss_Photon_Cannon ||
			uType == Terran_Bunker || uType == Terran_Missile_Turret ||
			uType == Zerg_Sunken_Colony || uType == Zerg_Spore_Colony)
		{
			auto it = hisBuildingPosAndType.find(u->getPosition());
			if (it != hisBuildingPosAndType.end())
				hisBuildingPosAndType.erase(it);
		}
	}

	N onEnd(H u) {
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
