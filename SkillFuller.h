#include<BWAPI.h>
#include<fstream>
#include "BWEM/bwem.h"
#include "BWEB/BWEB.h"
#include "BattleCommander.h"
#include <chrono>
#include "PathSearch.h"

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
#define me4or5PoolEarly me4or5Pool && CL(123) < 2
Y namespace std; Y namespace BWAPI; Y S = Position; Y T = TilePosition; Y U = Unit; Y V = UnitType; Y W = int; Y H = bool; Y J = double; auto&X = Broodwar;

A<string> hisInfo;
const int secondsPerTick = 10;
const int framesPerTick = secondsPerTick * 24;
A<V> hisTypesToCheck;
char num2char(int num_in) {
	int num_33 = num_in + 33;

	if (num_33 >= 127) {
		if (num_33 <= 255) {
			switch (num_33) {
			case 127: num_33++; break;
			case 129: num_33++; break;
			case 141: num_33++; break;
			case 143: num_33 += 2; break;
			case 144: num_33++; break;
			case 157: num_33++; break;
			case 160: num_33++; break;
			default:;
			}
		}
		else num_33 = 255;
	}

	RT static_cast<char>(num_33);
}
string hisRaceName;

W CC(W u) { RT u == 123 ? C->completedUnitCount(F[123]) + C->completedUnitCount(F[124]) + C->completedUnitCount(F[125]) : C->completedUnitCount(F[u]); } // count my completed units
W CI(W u) { RT C->incompleteUnitCount(F[u]); } // count my incompleted units (being trained, morphed, etc).
W countMyMorphingUnits(V v) {
	int res = 0;
	for (U u : C->getUnits()) {
		if (u->getType() == Zerg_Egg && u->getBuildType() == v)
			res++;
		M(u->getType().isBuilding() && u->getBuildType() == v)
			res++;
	}
	return res;
}
W CL(W u) { RT CC(u) + CI(u) + (u == 123 ? CC(124) + countMyMorphingUnits(Zerg_Lair) + countMyMorphingUnits(Zerg_Hive) : 0); } // count my total units

A<A<W>> b = {
	{22,4,60},		// ZvP 4 Pool
	{8,10,15},		// ZvP 1baseLurker
	{12,12,12,20},	// ZvP 2baseMuta
	{12,12,12,16},	// ZvP 2baseHydra
	{0},			// ZvP --
	{22,4,60},		// ZvT 4 Pool
	{8,10,15},		// ZvT 1baseLurker
	{12,12,12,20},	// ZvT 2baseMuta
	{12,12,12,16},	// ZvT 2baseHydra
	{8, 8, 8},		// ZvT -- me987Hydra
	{22,4,60},		// ZvZ 4 Pool
	{8,10,15},		// ZvZ 1baseLurker
	{12,12,12,20},	// ZvZ 2baseMuta
	{12,12,12,16},	// ZvZ 2baseHydra
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
	{134, 140, 127},	// ZvT -- me987Hydra
	{123,134,133},		// ZvZ 4 Pool
	{134,140,127},		// ZvT 1baseLurker
	{123,134,140,133},	// ZvT 2baseMuta
	{123,134,140,127},	// ZvT 2baseHydra
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
H me987Hydra = false;

BWEM::Map & bwemMapInstance = BWEM::Map::Instance();
W myScoutID = -99;
map<T, H> myScoutTargetsAndStatus;
T hisD = NT;
T closestD = NT;
A<T> hisPossibleDs;
map<W, W> myUnitsCreated;
H hisDKilled = false;
H myAttackCondition = false;
S myCP = NP;
S myknCP = NP;
W nToRetreat = 999;
A<V> myOrders = {};
V m_lastUnitType = UnitTypes::None;
W m_orderRequestedSince = 0;
map<Position, int> myWayPointsAndStatus; // His buildings
map<Position, int> myWayPointsAndStatus2; // His units
int wayPointsExpended = 0;

W n, np;	// current used supply, its prevVal
W w, wp;	// current max supply, its prevVal
W x, xp;	// current loaded overlords count, its prevVal
W R, O;		// iterator for 'b' and 'c' (both), frame count
W cc, co;	// guard frame used in 'BB', guard frame used in morphing overlords
W cs, ck3, ckn;  // guard frame to avoid calling too many scouts, guard frame to avoid calling too many overlords to k3/kn
W G; // build selector
A<W> GS(10, 0); // build stats
T D, kn, k3; // my start location, my nat location
U t; A<U>m; // nullptr unit, queue of available gather targets (mineral patches and vespene geysers)
N o(U u) { m.insert(m.begin(), u); } // Add resources to gather
N p(T u) { for (U z : GR(S(u), 400, BM))o(z), o(z); } // add all mineral patches close to a position to the queue
N s(U u) { if (q[u])o(q[u]), q.erase(u); } // remove a worker from a gather assignment
N I(U u, T z) { O % 48 == 0 ? ua(S(z)) : "a"; } // attack the given target with the given unit unless it is already doing so

T FindBuildingPlacementAroundAPoint(V buildingType, T searchCenter) {
	W mapWidth = X->mapWidth();
	W mapHeight = X->mapHeight();
	T mapCenter = T(mapWidth / 2, mapHeight / 2);
	T buildingSize = buildingType.tileSize();

	for (int iRadius = 1; iRadius < 18; ++iRadius) {
		for (int idy = searchCenter.y - iRadius * buildingSize.y; idy <= searchCenter.y + iRadius * buildingSize.y; idy++) {
			for (int idx = searchCenter.x - iRadius * buildingSize.x; idx <= searchCenter.x + iRadius * buildingSize.x; idx++) {
				if (idy == searchCenter.y - iRadius * buildingSize.y
					|| idy == searchCenter.y + iRadius * buildingSize.y
					|| idx == searchCenter.x - iRadius * buildingSize.x
					|| idx == searchCenter.x + iRadius * buildingSize.x
					) // eliminate repeated calculation
					if (idx < mapWidth && idx > 0 && idy < mapHeight && idy > 0) // causes CRASH if coords are outside of the map!!!
					{
						TilePosition buildingTopLeft(idx, idy);
						TilePosition buildingBottomRight = buildingTopLeft + buildingSize;

						bool closerToCenter = (searchCenter.x - idx) * (mapCenter.x - idx) < 0 && (searchCenter.y - idy) * (mapCenter.y - idy) < 0;

						if (X->canBuildHere(buildingTopLeft, buildingType) && closerToCenter && X->hasCreep(buildingTopLeft) && X->hasCreep(buildingBottomRight)
							&& X->getUnitsInRectangle(S(buildingTopLeft), S(buildingBottomRight)).empty() && X->isVisible(buildingTopLeft))
							return buildingTopLeft;
					}
			}
		}
	}

	// Resort to the vanilla finder if everything above fails
	RT GB(buildingType, searchCenter, 18);
} // T FindBuildingPlacementAroundAPoint(V buildingType, T searchCenter)

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

W distSq2(const S &pos1, const S &pos2) {
	RT(pos1.x - pos2.x) * (pos1.x - pos2.x) + (pos1.y - pos2.y) * (pos1.y - pos2.y);
} // distSq2()

int GetAttackPriority(U u) {
	if (Q == F[35] || Q == F[34] || Q == F[67]) RT - 1; // neglect Zerg_Egg, Zerg_Larva, Protoss_Interceptor
	// if (me4or5Pool && Q == F[103]) RT 12; // Wall smashing when 4/5 pooling
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
			if (attackerType.airWeapon() == NW && (Q.isFlyer() || Q.isFlyingBuilding())) CT;
			if (attackerType.groundWeapon() == NW && (!Q.isFlyer() && !Q.isFlyingBuilding())) CT;
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

bool canAttackHim(U mu, U hu) {
	if (!mu || !mu->exists() || !hu || !hu->exists()) return false;

	if (hu->isFlying()) { if (mu->getType().airWeapon() != NW) return true; }
	else if (mu->getType().groundWeapon() != NW) return true;

	return false;
}

bool canHitHim(U mu, U hu) {
	if (!canAttackHim(mu, hu)) return false;

	int muRange = hu->isFlying() ? mu->getType().airWeapon().maxRange() : mu->getType().airWeapon().maxRange();
	if (distSq<true>(mu->getPosition(), hu) <= muRange * muRange) return true;

	return false;
}

N SmartMove(U attacker, S targetPosition)
{
	// Special case: Zerg_Lurker
	if (attacker gt == F[97])
		if (attacker->isBurrowed()) {
			if (GR(attacker->getPosition(), 192, BE && !BF).empty())
				attacker->unburrow();
		}

	if (distSq<true>(targetPosition, attacker) <= 128 * 128)
		if (attacker->isUnderAttack())
			if (U closestEnemy = attacker GC(BE && Filter::CanAttack, 400))
				if (canHitHim(attacker, closestEnemy))
				{
					if (!attacker->isHoldingPosition()) attacker->holdPosition();
					RT;
				}

	if (attacker->getLastCommandFrame() >= XF - 10) RT; // if we have issued a command to this unit already this frame, ignore this one
	UnitCommand currentCommand(attacker->getLastCommand()); 	// get the unit's current command
	if ((currentCommand.getType() == UnitCommandTypes::Move) // if we've already told this unit to attack this target, ignore this command
		&& (currentCommand.getTargetPosition() == targetPosition)
		&& (XF - attacker->getLastCommandFrame() < 10)
		&& (attacker->isMoving())) RT;

	attacker->move(targetPosition); // if nothing prevents it, move to the target
} // SmartMove

N SmartAttack(U attacker, U target)
{
	if (attacker == NULL || target == NULL) RT;
	if (attacker->isIrradiated() || attacker->isUnderStorm()) SmartMove(attacker, S(D));

	V attackerType = attacker gt;

	// Special case: Zerg_Lurker
	if (attackerType == F[97] && !attacker->isBurrowed()) {
		if (!GR(attacker->getPosition(), 192, BE && !BF).empty()) attacker->burrow();
	}

	if (attacker->isUnderAttack())
		if (U closestEnemy = attacker GC(BE && Filter::CanAttack, 400))
			if (canHitHim(attacker, closestEnemy))
			{
				if (!attacker->isHoldingPosition()) attacker->holdPosition();
				RT;
			}

	// if we have issued a command to this unit already this frame, ignore this one
	//if (attacker->getLastCommandFrame() >= XF - 5) RT;

	// Data from: https://docs.google.com/spreadsheets/d/1bsvPvFil-kpvEUfSG74U3E5PLSTC02JxSkiR8QdLMuw/edit#gid=0
	//int attackCD = 10;
	//V attackerType = attacker gt;
	//if (attackerType == F[36]) attackCD = 5; // Zergling
	//M(attackerType == F[37]) attackCD = 3;	// Hydra
	//M(attackerType == F[38]) attackCD = 15; 
	//M(attackerType == F[97]) attackCD = 2; // Lurker

	UnitCommand currentCommand(attacker->getLastCommand()); // get the unit's current command
	//if (currentCommand.getType() == UnitCommandTypes::Attack_Unit &&	currentCommand.getTarget() == target
	//	&& (XF - attacker->getLastCommandFrame() <= attackCD)) RT;

	if (target->isFlying() && attacker->getAirWeaponCooldown() != 0 || !target->isFlying() && attacker->getGroundWeaponCooldown() != 0) RT;

	if (currentCommand.getType() != UnitCommandTypes::Attack_Unit || currentCommand.getTarget() != target)
		attacker->attack(target); // if nothing prevents it, attack the target
} // SmartAttack

N BB(W u, T b0 = D, T z = D) { // b0: build location search center; z: builder search center
	if (T intendedBuildLocation = u == 135 ? FindBuildingPlacementAroundAPoint(F[u], b0) : GB(F[u], b0, 18)) {
		if (!GR(S(intendedBuildLocation), 80, BO && BW).empty()) RT;

		if (O - cc > 120) {
			cc = O; U br = X GC(S(intendedBuildLocation), BW && (B(Idle) || B(GatheringMinerals) || !B(Moving)) && !B(CarryingMinerals) && !B(GatheringGas) && BO, 400);

			if (L(br)) {
				/*if (distSq<true>(S(intendedBuildLocation), br) > 80 * 80)
				{
					SmartMove(br, S(intendedBuildLocation)); RT;
				}*/

				br->build(F[u], intendedBuildLocation);
			}
		}
	}
} // Morph into a building around b0 choosing a worker near z

T FindNatPos(BWEM::Map & mapBWEM, T basePos) {
	int distBest = 99999;
	T cand = NT;

	for (auto &area : mapBWEM.Areas()) {
		if (area.AccessibleNeighbours().empty()) CT;
		for (auto &base : area.Bases()) {
			// Must have gas, be accesible and at least 5 mineral patches
			if (base.Geysers().empty() || base.Minerals().size() < 5) CT;

			int dist = static_cast<int>(BWEB::Map::getGroundDistance(base.Center(), S(basePos) + S(64, 48)));
			if (dist < distBest && !base.Starting() && base.Location() != basePos && base.Location() != kn) {
				distBest = dist;
				cand = base.Location();
			}
		}
	}

	return cand;
}

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

	if (hisD == NT && hisPossibleDs.size() == 1) hisD = hisPossibleDs.front();

	if (hisD != NT) {
		if (!shouldGoofAround) shouldGoofAround = true;
	}
	else {
		for (T sl : X->getStartLocations()) {
			if (sl == D) CT;

			if (me4or5Pool && (Q != F[41] && sl == closestD || Q == F[41] && sl != closestD)) CT; // skip the first target which is to be covered by the overlord.
			
			S slPos = S(sl);

			H hisDFound = false;

			if (!GR(slPos, 12 * 32, BE && Filter::IsBuilding).empty()) hisDFound = true;

			if (T thisNatPos = FindNatPos(bwemMapInstance, sl))
				if (!GR(S(thisNatPos), 9 * 32, BE && Filter::IsBuilding).empty()) hisDFound = true;

			if (hisDFound) {
				if (hisD == NT) hisD = sl;
				myScoutTargetsAndStatus[sl] = true; shouldGoofAround = true; BK;
			}

			if (!myScoutTargetsAndStatus[sl]) { // not visited yet
				// 50176 == (7 * 32) ^ 2
				if (distSq<true>(slPos, u) < 50176 && GR(slPos, 224, BE && !BW).empty()) {
					if (hisPossibleDs.size() >= 2)
						hisPossibleDs.erase(remove(hisPossibleDs.begin(), hisPossibleDs.end(), sl), hisPossibleDs.end());

					myScoutTargetsAndStatus[sl] = true; CT;
				}
				M(distSq<true>(slPos, u) >= 50176) { SmartMove(u, slPos); RT; }
			}
		}
	}

	if (shouldGoofAround) {
		if (u->getHitPoints() == u->getType().maxHitPoints()) {
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

std::map<std::pair<UnitType, UnitType>, double> unitMatchupTable = unitMatchupTableGen;
H meSmashGround() {
	//	lings,		mutas,			hydras,		lurkers
	if (!CC(36) && !CC(42) && !CC(37) && !CC(97)) return false;

	if (hisD == NT) return false;

	if (hisD != NT && hisDKilled) return true;

	U myLeader = NULL;
	S hisDPos = S(hisD);
	int bestDist = 99999;

	// Get my army leader
	for (U u : C->getUnits()) {
		V uType = u->getType();
		if (uType == F[36] || uType == F[42] || uType == F[37] || uType == F[97])
		{
			int distToHisD = static_cast<int>(BWEB::Map::getGroundDistance(u->getPosition(), hisDPos));
			if (distToHisD < bestDist)
			{
				myLeader = u;
				bestDist = distToHisD;
			}
		}
	}

	if (myLeader != NULL) {
		S myLeaderPos = myLeader->getPosition();
		Unitset enemiesNearby = GR(myLeaderPos, 400, BE && !BW && Filter::CanAttack && !BF);
		Unitset friendsNearby = GR(myLeaderPos, 400, BO && !BW && Filter::CanAttack);

		double hisScore = 0;
		double myScore = 0;

		for (auto u : enemiesNearby) 
			hisScore += unitMatchupTable[make_pair(F[36], u->getType())] * (u->getHitPoints() + u->getShields()) / double(u->getType().maxHitPoints() + u->getType().maxShields());
		for (auto u : friendsNearby)
			myScore += unitMatchupTable[make_pair(F[36], u->getType())] * (u->getHitPoints() + u->getShields()) / double(u->getType().maxHitPoints() + u->getType().maxShields());

		if (myScore > hisScore) return true;
	}

	return false;
}

template<typename Repr = float, typename Period = std::milli>
struct ScopeTimer {
	ScopeTimer(Repr &output) : output{ output } { }
	~ScopeTimer() { output = std::chrono::duration<Repr, Period>(std::chrono::steady_clock::now() - start).count(); }
private:
	std::chrono::steady_clock::time_point start = std::chrono::steady_clock::now();
	Repr &output;
	ScopeTimer& operator=(const ScopeTimer&);
};

S vecToPos(A<int> vecIn) {
	return S(T(vecIn.back(), vecIn.front()));
}

A<int> posToVec(T posIn) {
	A<int> res;
	res.push_back(posIn.y);
	res.push_back(posIn.x);
	return res;
}

A<int> getNearestGoodTile(A<int> posIn, int gridInfo[][128]) {
	// Return if this is already a good tile
	int posInX = posIn.front();
	int posInY = posIn.back();
	if (gridInfo[posInX][posInY] == 0) return posIn;

	// Spiral search around `posIn`
	for (int r = 1; r <= 128; ++r) {
		map<int, A<int>> scoreAndPos;
		for (int i = posInX - r; i <= posInX + r; ++i)
			for (int j = posInY - r; j <= posInY + r; ++j)
				if (i >= 0 && i < X->mapWidth() && j >= 0 && j < X->mapHeight() && gridInfo[i][j] == 0) // Within the map and is good
					if (i == posInX - r || i == posInX + r || j == posInY - r || j == posInY + r) { // Only points from the surrounding square frame
						A<int> posIJ = { i, j };
						scoreAndPos[distSq2(S(i, j), S(posInX, posInY))] = posIJ;
					}
					
		if (!scoreAndPos.empty())
			return scoreAndPos.begin()->second;
	}

	A<int> myStartingTopLeft = { D.x, D.y };
	return myStartingTopLeft;
}

struct ExampleAIModule :AIModule {
	N onStart() {
		D = C->getStartLocation();
		X->setCommandOptimizationLevel(1);
		X->enableFlag(Flag::UserInput);

		ER == R1 ? hisRaceName = "_P" : (ER == R2 ? hisRaceName = "_T" : hisRaceName = "_Z");

		ifstream mf("bwapi-data/read/" + XE->getName() + hisRaceName + ".txt");
		if (mf) {
			W td = -1; W lc; while (mf >> lc) { ++td; GS[td] = lc; } mf.close();

			A<W> feasibleBOs;
			switch (ER) {
			case R1: feasibleBOs = { 2 }; BK;
			case R2: feasibleBOs = { 7 }; BK;
			case R3: feasibleBOs = { 12 }; BK;
			default: feasibleBOs = { 12 }; BK;
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

		G = 10; // 987 Hydra rush
		me987Hydra = true;

		bG = b[G - 1];
		cG = c[G - 1];
		me4or5Pool = G % 5 == 1;
		me1BaseLurker = G % 5 == 2; // G == 2 || G == 7;
		me2BaseMuta = G % 5 == 3; // G == 3 || G == 8;
		me2BaseHydra = G % 5 == 4; // G == 9 || G == 14;
		meFastNat = me2BaseMuta || me2BaseHydra;
		meCOEP = me987Hydra ? false : G % 5 == 0;

		bwemMapInstance.Initialize();
		bwemMapInstance.FindBasesForStartingLocations();
		GetMyNaturalAndOtherBases(bwemMapInstance);
		BWEB::Map::onStart();
		myCP = S(BWEB::Map::getMainChoke()->Center());
		myknCP = S(BWEB::Map::getNaturalChoke()->Center());
		
		for (T u : X->getStartLocations())u != D ? h.push_back(u), hisPossibleDs.push_back(u) : "a"; p(k3); p(kn); p(D);

		for (T sl : X->getStartLocations())
			myScoutTargetsAndStatus[sl] = false;

		hisInfo.reserve(3600 / secondsPerTick + 1); // Assuming BASIL env, where games last for 60 minutes

		if (ER == R1) hisTypesToCheck = {
			Protoss_Probe, Protoss_Zealot, Protoss_Dragoon,
			Protoss_High_Templar, Protoss_Dark_Templar, Protoss_Archon,
			Protoss_Dark_Archon, Protoss_Reaver, Protoss_Observer,
			Protoss_Shuttle, Protoss_Scout, Protoss_Carrier,
			Protoss_Arbiter, Protoss_Corsair, Protoss_Photon_Cannon };
		else if (ER == R2) hisTypesToCheck = {
			Terran_SCV, Terran_Marine, Terran_Firebat,
			Terran_Medic, Terran_Ghost, Terran_Vulture,
			Terran_Siege_Tank_Tank_Mode, Terran_Goliath,
			Terran_Wraith, Terran_Valkyrie, Terran_Battlecruiser,
			Terran_Science_Vessel, Terran_Dropship, Terran_Bunker };
		else hisTypesToCheck = {
			Zerg_Drone, Zerg_Zergling, Zerg_Hydralisk,
			Zerg_Lurker, Zerg_Ultralisk, Zerg_Defiler,
			Zerg_Overlord, Zerg_Mutalisk, Zerg_Scourge,
			Zerg_Queen, Zerg_Guardian, Zerg_Devourer
		};
	} // N onStart()

	N onFrame() {
		//float time; { // Timer starts
			//ScopeTimer<float> timer{ time };

		// Initializing...
		O = XF;
		H myNatBuilt = !GR(S(kn), 160, BO && BR).empty();
		H my3rdBuilt = !GR(S(k3), 160, BO && BR).empty();
		
		// Find the index of an element in `F` (vector of all unit types)
		/*auto it = find(F.begin(), F.end(), Terran_Dropship);
		if (it != F.end() && O % 24 == 0) X << distance(F.begin(), it) << endl;*/

		//if (me4or5Pool)
		//	myAttackCondition = true; // CC(36) > 10; // 
		//M(me1BaseLurker)
		//	myAttackCondition = CC(97) > 2;
		//M(me2BaseMuta)
		//	myAttackCondition = CC(42) > 3;
		//M(me2BaseHydra)
		//	myAttackCondition = CC(37) > 15;
		//M(me1BaseMuta)
		//	myAttackCondition = CC(133) ? CC(42) > 5 : CC(36) > 9;
		//M(me1BaseHydra)
		//	myAttackCondition = CC(127) ? CC(37) > 5 : CC(36) > 15;
		//M(me7Pool)
		//	myAttackCondition = CC(127) ? CC(37) > 5 : CC(36) > 15;
		//M(meCOEP) // TODO: what would be a more reasonable myAttackCondition other than some magic number?
		//	myAttackCondition = CC(123) + CC(124) + CC(125) >= 3;

		// Updating his starting base status...
		if (hisD != NT && !hisDKilled)
			if (!GR(S(hisD), 160, BO).empty() && GR(S(hisD), 160, BE && BR).empty())
				hisDKilled = true;

		// Updating target queue...
		for (auto u = h.begin(); u != h.end();) u = X->isVisible(*u) && GI(*u, B(Building) && BE).empty() ? i[*u] = t, h.erase(u) : u + 1;
		for (U u : XE->getUnits())!i[P] && Q.isBuilding() ? i[P] = u, h.push_back(P) : "a";

		//if (O && O < 14400 && O % 720 == 0) ER == R1 ? hisRaceName = "_P" : (ER == R2 ? hisRaceName = "_T" : hisRaceName = "_Z");

		// update hisInfo every X seconds, assuming max game time is 60 minutes
		if (O <= 86401) {
			if ((O - 1) % framesPerTick == 0) {
				myAttackCondition = meSmashGround();

				// X << 3600 / secondsPerTick + 1 << endl;
				string thisStr;
				for (auto i : hisTypesToCheck) {
					if (!i.isBuilding()) thisStr.push_back(num2char(countHisUnits(i)));
					else thisStr.push_back(num2char(countHisBuildings(i)));
				}
				hisInfo.push_back(thisStr);
			}

			if ((O - 1) % 4 == 0) {
				for (auto u : XE->getUnits()) {
					if (!u->getType().isBuilding()) {
						if (u->exists()) {
							hisUnitIDAndInfo[u->getID()].unitPos = u->getPosition();
							hisUnitIDAndInfo[u->getID()].unitSpd = std::make_pair(u->getVelocityX(), u->getVelocityY());
							hisUnitIDAndInfo[u->getID()].lastFrameVisible = O;
						} M(!u->isVisible() && O - hisUnitIDAndInfo[u->getID()].lastFrameVisible > 120) { // reset the speed data
							hisUnitIDAndInfo[u->getID()].unitSpd.first = hisUnitIDAndInfo[u->getID()].unitSpd.second = 0.0;
						}
					}
				}
			}
		}

		// Grid info
		int gridInfo[128][128] = { 0 };
		for (int i = 0; i < 128; ++i) // X direction
			for (int j = 0; j < 128; ++j) // Y direction
			{
				Position posIJ((i + 1) * 32 - 16, (j + 1) * 32 - 16);
				Color colorIJ = Colors::Black;

				if (X->getGroundHeight(TilePosition(posIJ))) colorIJ = Colors::Grey;

				// Mark enemy workers
				if (any_of(hisUnitIDAndInfo.begin(), hisUnitIDAndInfo.end(), [&posIJ](const auto & u)
				{ return u.second.unitType == Protoss_Probe && distSq2(u.second.unitPos, posIJ) <= 25 * 1024; })) colorIJ = Colors::Green;

				// Mark enemy ground-attack-only ground units
				if (any_of(hisUnitIDAndInfo.begin(), hisUnitIDAndInfo.end(), [&posIJ](const auto & u)
				{ return u.second.unitType == Protoss_Zealot && distSq2(u.second.unitPos, posIJ) <= 4 * 1024; })) colorIJ = Colors::Yellow;

				// Mark enemy dual-attack ground units
				if (any_of(hisUnitIDAndInfo.begin(), hisUnitIDAndInfo.end(), [&posIJ](const auto & u)
				{ return u.second.unitType == Protoss_Dragoon 
					&& distSq2(Position(u.second.unitPos.x + int(u.second.unitSpd.first * 48), 
						u.second.unitPos.y + int(u.second.unitSpd.second * 48)), posIJ) <= 64 * 1024; })) colorIJ = Colors::Teal;

				if (any_of(hisUnitIDAndInfo.begin(), hisUnitIDAndInfo.end(), [&posIJ](const auto & u)
				{ return u.second.unitType == Protoss_Dragoon && distSq2(u.second.unitPos, posIJ) <= 64 * 1024; })) colorIJ = Colors::Blue;

				// Mark enemy buildings
				if (any_of(hisBuildingPosAndType.begin(), hisBuildingPosAndType.end(), [&posIJ](const auto & u)
				{ return u.second == Protoss_Photon_Cannon && distSq2(u.first, posIJ) <= 100 * 1024; })) colorIJ = Colors::Red;

				if (colorIJ != Colors::Black) {
					if (colorIJ == Colors::Blue || colorIJ == Colors::Teal || colorIJ == Colors::Red) gridInfo[j][i] = 1;

					X->drawBoxMap(posIJ - Position(16, 16), posIJ + Position(16, 16), colorIJ, false);
					X->drawLineMap(posIJ - Position(16, 16), posIJ + Position(16, 16), colorIJ);
				}
			}

		/// Mark Waypoints
		// His Buildings
		for (auto u : hisBuildingPosAndType) {
			if (u.second == Protoss_Photon_Cannon) {
				const int deltaDist = int(224 * sqrt(2));
				for (auto i1 : { -1, 1 })
					for (auto i2 : { -1, 1 })
					{
						Position p12 = u.first + Position(i1 * deltaDist, i2 * deltaDist);

						if (myWayPointsAndStatus[p12] == 0) myWayPointsAndStatus[p12] = 1;
						if (myWayPointsAndStatus[p12] == 1 && !GR(p12, 32, Filter::IsOwned && Filter::GetType == Terran_Wraith).empty()) {
							myWayPointsAndStatus[p12] = 2;
							wayPointsExpended++;
						}

						X->drawCircleMap(p12, 8, Colors::White, true);
						X->drawTextMap(p12 + Position(0, 12), "%cwaypointStatus: %d", Text::White, myWayPointsAndStatus[p12]);
					}
			}
		}

		// His Units
		myWayPointsAndStatus2.clear(); // reset to account for unit movement
		for (auto u : hisUnitIDAndInfo) {
			if (u.second.unitType == Protoss_Dragoon) {
				const int deltaDist = int(224 * sqrt(2));
				for (auto i1 : { -1, 1 })
					for (auto i2 : { -1, 1 })
					{
						Position p12 = u.second.unitPos + Position(i1 * deltaDist, i2 * deltaDist);

						if (myWayPointsAndStatus2[p12] == 0) myWayPointsAndStatus2[p12] = 1;
						if (myWayPointsAndStatus2[p12] == 1 && !GR(p12, 32, Filter::IsOwned && Filter::GetType == Terran_Wraith).empty()) {
							myWayPointsAndStatus2[p12] = 2;
							wayPointsExpended++;
						}

						X->drawCircleMap(p12, 8, Colors::White, true);
						X->drawTextMap(p12 + Position(0, 12), "%cwaypointStatus: %d", Text::White, myWayPointsAndStatus2[p12]);
					}
			}
		}

		// Display intel on his units
		for (auto u : hisUnitIDAndInfo)
			if (u.second.unitType == Protoss_Dragoon) { // Show movement speed
				X->drawTextMap(u.second.unitPos - Position(0, 16), "%cspdX: %.2f", Text::White, u.second.unitSpd.first);
				X->drawTextMap(u.second.unitPos, "%cspdY: %.2f", Text::White, u.second.unitSpd.second);
			}

		// COEP Starts
		if (meCOEP) {
			if (myOrders.empty()) {
				A<V> myFeasibleActions = myFeasibleActionsGen();
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

				X->drawTextScreen(10, 20, "onFrame(): %s \n", myOrders.front().c_str());
			}
		} // COEP Ends

		// Constructing my buildings...
		if (!meCOEP) {
			if ((me2BaseMuta || me2BaseHydra) && !GR(S(kn), 160, BO&&BW).empty() && CL(135) + CL(137) < 6 && C->minerals() >= 50) BB(135, T(myknCP));

			for (; R < (W)cG.size(); R++) {
				if (cG[R] == 134 && CL(134) == 0 && C->minerals() < 182) BK;

				if (CL(cG[R]) < (cG[R] == 123 ? 2 : 1) && n >= bG[R] * 2) { meFastNat && cG[R] == 123 && CL(cG[R]) == 1 ? BB(cG[R], kn) : BB(cG[R], D); BK; }
			}

			if (n > (me4or5Pool ? 45 : 23) && CL(140) < (W)GR(S(D), 320, B(ResourceContainer) && !BM).size()) BB(140); // Building extractor(s) near my main...
			M(CC(140) == 1 && CL(140) < 2 && myNatBuilt && !GR(S(kn), 320, BO&&BW&&B(Completed)).empty()) BB(140, kn, kn); // Building extractor(s) near my nat...
			M((me2BaseHydra || me2BaseMuta || me1BaseLurker) && CC(140) == 2 && CL(140) < 3 && CC(123) >= 3 && !GR(S(k3), 320, BO&&BW&&B(Completed)).empty() && !GR(S(k3), 320, B(ResourceContainer) && !BM).empty()) BB(140, k3, k3); // Building extractor(s) near my third...
			M(me4or5Pool) {
				if (CC(124) && !CL(133)) BB(133);
				M(CL(123) < 4 && C->minerals() > 300) BB(123); // More hatches when possible when me4or5Pool
			}
			M(!me4or5Pool && my3rdBuilt && CC(123) < 6 && !countMyMorphingUnits(Zerg_Hatchery) && C->minerals() > 300) {
				if (GR(S(D), 320, BO && BW).size() > 4 && GR(S(D), 320, BO && BR).size() < 3)  BB(123); // More hatches when possible
				if (GR(S(kn), 320, BO && BW).size() > 4 && GR(S(kn), 320, BO && BR).size() < 3)  BB(123, kn, kn); // More hatches when possible
				if (GR(S(k3), 320, BO && BW).size() > 4 && GR(S(k3), 320, BO && BR).size() < 3) BB(123, k3, k3); // More hatches when possible
			}
			M(me1BaseLurker) {
				if (CC(134) && CL(135) + CL(137) < 6 && C->minerals() > 100) BB(135);
				M(CL(123) == 1 && CL(123) < 2 && C->minerals() > 450) BB(123);
				M(myAttackCondition && !myNatBuilt) BB(123, kn);
				M(myAttackCondition && myNatBuilt && !my3rdBuilt) BB(123, k3);
			}
			M(me2BaseMuta || me2BaseHydra) {
				if (CC(123) >= 2 && !CI(123) && CC(140) == 2 && !GR(S(k3), 320, BO).empty() && C->minerals() > 300) BB(123, k3, kn); // Expansion at third
			}
			M(!(me2BaseHydra || me2BaseMuta || me1BaseLurker || me987Hydra) && CL(cG[cG.size() - 1]) && CL(123) < 5 && C->minerals() > 400) BB(123); // More hatches when tech is ready
			M((me1BaseMuta || me1BaseHydra) && CL(123) < 3 && C->minerals() > 450) BB(123); // More hatches when possible
			M((me7Pool) && CL(123) < 3 && C->minerals() > 450) BB(123); // More hatches when possible
			M(O >= 34560 && GR(S(D), 320, BM).empty() && !GR(S(D), 320, BO&&BW).empty()) BB(135); // Spend the extra money for base defense after 24m
			M(me987Hydra && CC(40) >= 12 && CL(123) < 3 && C->minerals() > 450) BB(123); // More hatcheries when see fit
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
					BB(135, T(myknCP));
				}
			}
			else {
				if (!myOrders.empty())
					if (myOrders.front().isBuilding())
						myOrders.erase(myOrders.begin());
			}
		} // Constructing my buildings ends

		// Resetting...
		wp = w; np = n; xp = x; w = n = x = R = 0; map<U, W>es;
		H myScoutFound = false;

		// Managing my units' behaviors...
		for (U u : C->getUnits()) {
			if (!u->exists() || !u->isCompleted() || u->isMaelstrommed() || u->isStasised() || u->isLoaded() || u->isStuck()) CT;

			if (u->getType() == Terran_Barracks || u->getBuildType() == Terran_Barracks) X << u->getTilePosition() << endl;

			if (Q == F[0]) { // Terran_Wraith
				if (hisD != NT && !hisDKilled) {
					S uPos = u->getPosition();
					S hisPos = S(hisD) + S(64, 48);
					S uNextPos = hisPos;
					int pathfindMethod = 2;

					if (pathfindMethod == 2) { // Based on waypoints
						S uBestPos = Positions::None;
						int uBestScore = 99999999; // the smaller, the better
						for (auto v : myWayPointsAndStatus)
							if (v.second && v.second != 2) // if valid + NOT visited recently..
							{
								int thisScore = distSq2(uPos, v.first);
								if (thisScore < uBestScore)
								{
									uBestScore = thisScore;
									uBestPos = v.first;
								}
							}

						if (uBestPos != Positions::None && wayPointsExpended < 2) {
							X->drawCircleMap(uBestPos, 16, Colors::Blue, false);
							uNextPos = uBestPos;
						}

						// if there are enemy units nearby..
						if (any_of(hisUnitIDAndInfo.begin(), hisUnitIDAndInfo.end(), [&uPos](const auto & u)
						{ return u.second.unitType == Protoss_Dragoon
							&& distSq2(Position(u.second.unitPos.x + int(u.second.unitSpd.first * 48),
								u.second.unitPos.y + int(u.second.unitSpd.second * 48)), uPos) <= 64 * 1024; })) {
							
							int uBestScore = 99999999; // the smaller, the better
							for (auto v : myWayPointsAndStatus2)
								if (v.second && v.second != 2) // if valid + NOT visited recently..
								{
									int thisScore = distSq2(uPos, v.first);
									if (thisScore < uBestScore)
									{
										uBestScore = thisScore;
										uBestPos = v.first;
									}
								}

							if (uBestPos != Positions::None)
								uNextPos = uBestPos;
						}
					} else { // Using more conventional approaches such as A*
						A<int> adjUPos = getNearestGoodTile(posToVec(T(uPos.x / 32, uPos.y / 32)), gridInfo);

						uNextPos = vecToPos(runPathSearch(gridInfo, adjUPos, posToVec(hisD), false, false, false));
						X->drawCircleMap(vecToPos(adjUPos), 4, Colors::Purple, true);
						X->drawLineMap(uPos, vecToPos(adjUPos), Colors::Purple);
					}
					
					X->drawCircleMap(uNextPos, 8, Colors::Blue, true);
					X->drawLineMap(uPos, uNextPos, Colors::Blue);
				
					//SmartMove(u, uNextPos);
					if (u->getLastCommand().getType() != UnitCommandTypes::Move || u->getLastCommand().getTargetPosition() != uNextPos) 
						u->move(uNextPos);
				}
				continue;
			} M(Q == F[14]) { // Terran_Dropship
				SmartMove(u, S(D));
			}

			// Updating supply info...
			w += Q.supplyProvided(); n += Q.supplyRequired();
			U Z = u GC(BE, 200); Z && !Z IM ? y[Z] = O : w; u->isStartingAttack() ? y[u] = O : w;

			// Upgrading/Researching/Morphing...
			if (Q.isBuilding()) {
				if (!me1BaseHydra && !me2BaseHydra && !me7Pool && !me987Hydra) P == D ? (!CL(124) ? ut(F[124]) : (!CL(125) ? ut(F[125]) : "a")) : "a"; // Main Hatch->Lair->Hive
				if (CC(125) && CC(134)) up(AG); // Crackling
				if (CC(140) && CC(134) && !me987Hydra) up(UT Metabolic_Boost); // Ling speed
				if ((me1BaseLurker || me2BaseHydra) && CC(124) && CC(127)) ur(LA); // Lurker_Aspect
				//if (me2BaseMuta) { up(VS); up(PC); } // Overlord transportation and speed
				if ((me1BaseHydra || me2BaseHydra || me7Pool) && CC(127)) { up(UT Grooved_Spines); up(UT Muscular_Augments); } // Hydra range and speed upgrades
				if (meCOEP && CC(127)) { ur(LA); up(UT Grooved_Spines); up(UT Muscular_Augments); }
				if (me987Hydra && CC(127)) { up(UT Grooved_Spines); } // Hydra upgrades
				ut(F[137]); // Creep->Sunken
			}

			// Managing larvae...
			M(Q == F[34]) {
				if (!meCOEP) {
					if (CC(125) && !HU(AG) && !C->isUpgrading(AG) // Prioritize crackling upgrade when we have Hive
						|| me1BaseLurker && (CC(124) && CC(127) && !HR(LA) && !C->isResearching(LA) || myUnitsCreated[97] > 5 && !myNatBuilt) // me1BaseLurker
						|| me7Pool && np > 11 && !CL(134) // me7Pool
						) CT; // Cut production in favor of upgrades/researches
					
					// Overlords
					if (wp < 400 && !(me987Hydra && CL(134) == 0)) { if (CC(cG[2]) && 1.0*np / wp > 0.8)O - co > 200 ? co = O, ut(F[41]) : "a"; M(np > 17 && wp - np < 4)O - co > 601 ? co = O, ut(F[41]) : "a"; } 
					if (me987Hydra && C->supplyUsed() >= 16 && myUnitsCreated[41] < 2 && CL(134) && CL(140)) ut(F[41]); // Overlord at 8

					// loop through all my bases and morph larvae nearby to drones...
					for (S ip : {S(D), S(kn)}) 
						if (ud(ip) < 128 && (W)GR(ip, 320, BO&&BW).size() < (me4or5PoolEarly ? bG[1] : 18) && CL(40) < (me4or5PoolEarly ? bG[1] : (myNatBuilt ? 36 : (me987Hydra ? 12 : 18))))
							if (!(me987Hydra && C->supplyUsed() >= 16 && myUnitsCreated[41] < 2 && CL(134)))
								if (!(me987Hydra && myUnitsCreated[41] >= 2 && CL(140) == 0 && C->minerals() < 100))
									ut(F[40]); // Drones

					if (me4or5Pool) {
						ut(F[42]);
						if (CC(133) && (CC(36) < 4 * CC(42) || C->gas() < 100) || !CC(133)) ut(F[36]); // Zerglings
					}
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
					M(me987Hydra) {
						if (C->gas() > 25 && CC(127)) ut(F[37]); // Mass hydras
						if (C->gas() < 10 && CC(134) && myUnitsCreated[37] >= 5) ut(F[36]); // Mix in lings when we don't have resources
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
			} // M(Q == F[34]) // larvae...

			// Managing hydras...
			M(Q == F[37]) {
				if (HR(LA))
					if (me1BaseLurker && CC(97) < 2 * CC(37)
						|| me2BaseHydra && 2 * CC(97) < CC(37)
						|| meCOEP && CC(97) < 6)
						ut(F[97]); // into lurkers!
			}

			// Managing drones...
			M(Q == F[40] || Q == F[13]) { // Drone or SCV
				if (hisD == NT && u->getID() == myScoutID) { myScoutFound = true; GoScouting(u); CT; }

				if (hisD == NT && CL(134) && CL(40) > 3 && myUnitsCreated[40] - CC(40) <= 1 && O - cs > 240 && !u->isCarryingMinerals() && !u->isCarryingGas()) { // When it is time to scout...
					if (myScoutID < 0) { cs = O; myScoutID = u->getID(); myScoutFound = true; GoScouting(u); CT; }
				}

				if (Z && (O - y[Z]) < 99 && !Z IF)s(u), K != Z ? ua(Z) : "a";
				M((ud(S(kn)) < 320 || ud(S(k3)) < 320) && CL(123) > 1) { if (u->isIdle() && !u->isMoving()) if (U cm = u GC(BM)) { ug(cm); CT; } } // Mining at natural/third
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

			// Managing my army: 
			//	lings,		mutas,			hydras,		lurkers
			if (Q == F[36] || Q == F[42] || Q == F[37] || Q == F[97]) {
				if (hisD == NT) { GoScouting(u); CT; }
				M(!hisDKilled) {
					if (U ZZ = FindTarget(u)) {
						if (L(ZZ)) {
							//if (myCombatSim(Horizon::getSimValue(u, 5.0), ZZ, u) > 0.9)
							myAttackCondition ? SmartAttack(u, ZZ) : SmartMove(u, S(D));
							CT;
						}
					}

					if (!GR(S(D), 320, BE && B(Invincible)).empty()) { SmartMove(u, S(D)); CT; }

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

			/// Display the time spent per frame
			if (O % 24 == 0) { 
				//X << "Elapsed seconds: " << O / 24 << ", ms taken: " << std::to_string(time) << endl;
				//X->sendText(std::to_string(time).c_str(), "\n"); 
			}

		//} // Timer ends
	}  // N onFrame()

	N onUnitMorph(U u) {
		if (u->getPlayer() == X->self()) {
			if (u->getBuildType() == F[40]) myUnitsCreated[40]++; // Drone
			M(u->getBuildType() == F[41]) myUnitsCreated[41]++;	  // Overlord
			M(u->getBuildType() == F[42]) myUnitsCreated[42]++;   // Muta 
			M(u->getBuildType() == F[46]) myUnitsCreated[46]++;	  // Scourge	
			M(u->getBuildType() == F[123]) myUnitsCreated[123]++; // Hatchery
			M(u->getBuildType() == F[97]) myUnitsCreated[97]++;   // Lurker
		}
	}

	N onUnitComplete(U u) {
		if (u->getPlayer() == X->self()) {
			Q == F[140] ? o(u), o(u) : "a";

			if (O < 24) {
				if (Q == F[40]) myUnitsCreated[40]++; // Drone
				M(Q == F[41]) myUnitsCreated[41]++;	  // Overlord
			}
		}
	}

	N onUnitDiscover(U u) {
		if (u->getPlayer() == XE) {
			V uType = u->getType();
			if (uType == Terran_Siege_Tank_Siege_Mode) uType = Terran_Siege_Tank_Tank_Mode;

			if (!uType.isBuilding()) { 
				hisUnitIDAndInfo[u->getID()].unitType = uType;
				hisUnitIDAndInfo[u->getID()].unitPos = u->getPosition();
				hisUnitIDAndInfo[u->getID()].unitSpd = std::make_pair(u->getVelocityX(), u->getVelocityY());
				hisUnitIDAndInfo[u->getID()].lastFrameVisible = O;
			}
			M(uType == Protoss_Photon_Cannon ||
				uType == Terran_Bunker || uType == Terran_Missile_Turret ||
				uType == Zerg_Sunken_Colony || uType == Zerg_Spore_Colony)
				hisBuildingPosAndType[u->getPosition()] = uType;
		}
	}

	N onUnitDestroy(U u) {
		if (u->getPlayer() == XE) {
			V uType = u->getType();
			if (uType == Terran_Siege_Tank_Siege_Mode) uType = Terran_Siege_Tank_Tank_Mode;

			if (!uType.isBuilding()) {
				auto it = hisUnitIDAndInfo.find(u->getID());
				if (it != hisUnitIDAndInfo.end())
					hisUnitIDAndInfo.erase(it);
			}
			M(uType == Protoss_Photon_Cannon ||
				uType == Terran_Bunker || uType == Terran_Missile_Turret ||
				uType == Zerg_Sunken_Colony || uType == Zerg_Spore_Colony)
			{
				auto it = hisBuildingPosAndType.find(u->getPosition());
				if (it != hisBuildingPosAndType.end())
					hisBuildingPosAndType.erase(it);
			}
		} M(u->getPlayer() == C) {
			// ...
		}
	}

	/*N onUnitHide(U u) {
		if (u->getPlayer() == XE) {
			V uType = u->getType();
			if (uType == Terran_Siege_Tank_Siege_Mode) uType = Terran_Siege_Tank_Tank_Mode;

			if (!uType.isBuilding()) {
				hisUnitIDAndInfo[u->getID()].unitType = uType;
				hisUnitIDAndInfo[u->getID()].unitPos = u->getPosition();
				hisUnitIDAndInfo[u->getID()].unitSpd = std::make_pair(u->getVelocityX(), u->getVelocityY());
			}
		}
	}*/

	N onEnd(H u) {
		// 1st File to Write
		std::ostringstream mfo;
		for (int ki = 0; ki < (W)GS.size(); ++ki) {
			if (ki % 2 == 0 && G == ki / 2 + 1 || ki % 2 && u && G == (ki + 1) / 2) { mfo << GS[ki] + 1 << dm; CT; }
			mfo << GS[ki] << dm;
		}

		ofstream mf("bwapi-data/write/" + XE->getName() + hisRaceName + ".txt", std::ofstream::trunc);
		if (mf)
		{
			mf << mfo.str();
			mf.flush();
		}
		mf.close();

		// 2nd File to Write
		std::ostringstream mfo2;
		for (auto i : hisInfo)
			mfo2 << i << dm;

		ofstream mf2("bwapi-data/write/" + XE->getName() + hisRaceName + "_INFO" + ".txt", std::ofstream::trunc);
		if (mf2)
		{
			mf2 << mfo2.str();
			mf2.flush();
		}
		mf2.close();
	}
};

