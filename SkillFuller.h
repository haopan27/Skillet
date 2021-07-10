#include<BWAPI.h>
#include<fstream>
#include "BWEM/bwem.h"
#include "BWEB/BWEB.h"
#include "BattleCommander.h"
#include <chrono>
#include "PathSearch.h"
#include "Horizon/Horizon.h"

#define C X->self()
#define E UnitTypes::allUnitTypes()
#define F vector<V>(E.begin(),E.end())
#define XE X->enemy()
#define GB X->getBuildLocation
#define GC ->getClosestUnit
#define GI X->getUnitsOnTile
#define uGP u->getPosition()
#define GR X->getUnitsInRadius
#define K u->getOrderTarget()
#define mh X->mapHash()
#define M else if
#define P u->getTilePosition()
#define gt ->getType()
#define Q u gt
#define L(z)(z&&z->exists()&&z->isDetected())
#define Y using
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
#define HR C->hasResearched
#define TT TechTypes::
#define RP T(rand()%2*X->mapWidth(),rand()%2*128)
#define SM h.empty()?um(S(RP)):um(S(h[0]))
#define NL u->getLoadedUnits().size()
#define R1 Races::Protoss
#define R2 Races::Terran
#define R3 Races::Zerg
#define ER XE->getRace()
#define NW WeaponTypes::None
#define NP Positions::None
#define NT TilePositions::None
#define XF X->getFrameCount()
#define me4or5PoolEarly me4or5Pool && CL(123) < 2 && !meGetMuta
#define DO_ONCE for (static int numTimesDone = 0 ; !numTimesDone++ ; )
#define PI 3.141592653589793238462643383279502884L

Y namespace std; Y namespace BWAPI; Y S = Position; Y T = TilePosition; Y U = Unit; Y V = UnitType; auto&X = Broodwar;

vector<string> hisInfo;
std::string hisInfoLastGame[360] = { "-" };
const int secondsPerTick = 10;
const int framesPerTick = secondsPerTick * 24;
float averageFrameTime = 0.0;
int lurkerSafeBurrowRange = 192;
int myRecentStats[10] = { 0 };

vector<V> hisTypesToCheck;
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

	return static_cast<char>(num_33);
}

int CC(int u) { return u == 123 ? C->completedUnitCount(F[123]) + C->completedUnitCount(F[124]) + C->completedUnitCount(F[125]) : C->completedUnitCount(F[u]); } // count my completed units
int CI(int u) { return C->incompleteUnitCount(F[u]); } // count my incompleted units (being trained, morphed, etc).
int myMorphingOverlords = 0;

int countMyMorphingUnits(V v) {
	int res = 0;
	for (U u : C->getUnits()) {
		if (u gt == Zerg_Egg && u->getBuildType() == v) {
			res++;
			if (v == Zerg_Overlord) myMorphingOverlords++;
		}
		M(u gt.isBuilding() && u->getBuildType() == v)
			res++;
	}
	return res;
}
int CL(int u) { return CC(u) + CI(u) + (u == 123 ? CC(124) + countMyMorphingUnits(Zerg_Lair) + countMyMorphingUnits(Zerg_Hive) : 0); } // count my total units

vector<vector<int>> b = {
	{22,4,60},		// ZvP 4 Pool
	{8,10,15},		// ZvP 1baseLurker
	{12,12,12,20},	// ZvP 2baseMuta
	{12,12,12,16},	// ZvP 2baseHydra
	{0},			// ZvP --
	{22,4,60},		// ZvT 4 Pool
	{8,10},			// ZvT 1baseLurker
	{12,12,12,20},	// ZvT 2baseMuta
	{12,12,12,16},	// ZvT 2baseHydra
	{8, 7, 8},		// ZvT -- me987Hydra
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
	{134,140},			// ZvT 1baseLurker
	{123,134,140,133},	// ZvT 2baseMuta
	{123,134,140,127},	// ZvT 2baseHydra
	{134, 140, 127},	// ZvT -- me987Hydra
	{123,134,133},		// ZvZ 4 Pool
	{134,140,127},		// ZvT 1baseLurker
	{123,134,140,133},	// ZvT 2baseMuta
	{123,134,140,127},	// ZvT 2baseHydra
	{0}					// ZvZ --
};

vector<int>bG, cG;// build order timing and what to build ==> pertaining to a specific build order
vector<int>si = { 0,20,40,58,76,94,116,140,156,174,190,206,222,238,256 }; // starting index specific to map
vector<T>h; // target queue (vector)
map<T, U>i; // target queue (map)
map<U, U>z, q; // map of units that are assigned to attack, map of worker gather assignments
map<U, int>y; // map of latest frame when an enemy unit either attacked or repaired or a friendly unit started an attack
map<double, T>distsAndBases;
bool me4or5Pool = false;
bool me1BaseLurker = false;
bool me2BaseMuta = false;
bool me2BaseHydra = false;
bool meFastNat = false;
bool me1BaseMuta = false;
bool me7Pool = false;
bool meCOEP = false;
bool me987Hydra = false;
bool meGetMuta = false;

std::string enemyRaceName = "Unknown";
std::string enemyName;
bool updateGridInfo = false;

BWEM::Map & bwemMapInstance = BWEM::Map::Instance();
int myScoutID = -99;
map<T, bool> myScoutTargetsAndStatus;
T hisD = NT;
T closestD = NT;
vector<T> hisPossibleDs;
map<int, int> myUnitsCreated;
bool hisDKilled = false;
bool myAttackCondition = false;
S myCP = NP;
S myknCP = NP;
int nToRetreat = 999;
vector<V> myOrders = {};
V m_lastUnitType = UnitTypes::None;
int m_orderRequestedSince = 0;
map<Position, int> myWayPointsAndStatus; // His buildings
map<Position, int> myWayPointsAndStatus2; // His units
int wayPointsExpended = 0;
int numStartingLocs = 0;
int myMaxSunks = 6;

int n, np;	// current used supply, its prevVal (doubles the actual value, due to zerglings occupying 1/2 supply)
int w, wp;	// current max supply, its prevVal
int x, xp;	// current loaded overlords count, its prevVal
int R, O;		// iterator for 'b' and 'c' (both), frame count
int cc, co = 0;	// guard frame used in 'BB', guard frame used in morphing overlords
int cs, ck3, ckn;  // guard frame to avoid calling too many scouts, guard frame to avoid calling too many overlords to k3/kn
int G; // build selector
int GS[10] = { 0 }; // build order stats
T D, kn, k3; // my start location, my nat location, my 3rd location
U t; vector<U>m; // nullptr unit, queue of available gather targets (mineral patches and vespene geysers)
void o(U u) { m.insert(m.begin(), u); } // Add resources to gather
void p(T u) { for (U z : GR(S(u), 400, BM))o(z), o(z); } // add all mineral patches close to a position to the queue
void s(U u) { if (q[u])o(q[u]), q.erase(u); } // remove a worker from a gather assignment
void I(U u, T z) { O % 48 == 0 ? ua(S(z)) : "a"; } // attack the given target with the given unit unless it is already doing so

T FindBuildingPlacementAroundAPoint(V buildingType, T searchCenter) {
	int mapWidth = X->mapWidth();
	int mapHeight = X->mapHeight();
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
	return GB(buildingType, searchCenter, 18);
} // T FindBuildingPlacementAroundAPoint(V buildingType, T searchCenter)

template<bool withAdj>
int distSq(const S &pos1, const U &unit2) {
	if (withAdj) {
		int xDist = unit2->getLeft() - pos1.x;
		if (xDist < 0)
		{
			xDist = pos1.x - (unit2->getRight() + 1);
			if (xDist < 0) xDist = 0;
		}

		int yDist = unit2->getTop() - pos1.y;
		if (yDist < 0)
		{
			yDist = pos1.y - (unit2->getBottom() + 1);
			if (yDist < 0) yDist = 0;
		}
		return xDist * xDist + yDist * yDist;
	}
	else {
		S pos2 = unit2->getPosition();
		return(pos1.x - pos2.x) * (pos1.x - pos2.x) + (pos1.y - pos2.y) * (pos1.y - pos2.y);
	}
} // distSq()

int distSq2(const S &pos1, const S &pos2) {
	return(pos1.x - pos2.x) * (pos1.x - pos2.x) + (pos1.y - pos2.y) * (pos1.y - pos2.y);
} // distSq2()

// Return unit attack range in pixels
int GetAttackRange(V v, V targetType = UnitTypes::None) {
	if (v == Zerg_Devourer) return 6 * 32;
	else if (v == Zerg_Guardian) return 8 * 32;
	else if (v == F[37]) return HU(UT Grooved_Spines) ? 160 : 128;
	else if (v == F[97]) return 192; // Lurker
	else if (v == F[42]) return 96; // Muta
	else if (v == F[38]) return targetType.isBuilding() ? 96 : 64; // Ultra (longer than the default)

	return 32;
}

int GetAttackPriority(U u) {
	if (Q == F[35] || Q == F[34] || Q == F[67]) return - 1; // neglect Zerg_Egg, Zerg_Larva, Protoss_Interceptor
	// if (me4or5Pool && Q == F[103]) return 12; // Wall smashing when 4/5 pooling
	if (Q.groundWeapon() != NW && !Q.isFlyer()) return 11; 	// highest priority is something that can attack us or aid in combat
	M(Q.isSpellcaster() && !Q.isFlyer()) return 10;
	M(Q.airWeapon() != NW && !Q.isFlyer()) return 9;
	M(Q.isWorker()) return 8;
	M(Q == F[150] || Q == F[137]) return 7;	// Static defense: Protoss_Photon_Cannon, Zerg_Sunken_Colony
	M(Q.groundWeapon() != NW && Q.isFlyer()) return 6;
	M(Q.isRefinery() || Q == F[146]) return 5; // Protoss_Pylon
	M(Q.isResourceDepot()) return 4;
	M(Q.gasPrice() > 0) return 3; // Buildings that cost gas
	M(Q.mineralPrice() > 0) return 2;
	M(Q.isNeutral()) return 1;
	else return 1;
} // GetAttackPriority

U FindTarget(U attacker)
{
	S attackerPos = attacker->getPosition();
	V attackerType = attacker gt;
	int attackerRange = attackerType.sightRange();
	U target = NULL;

	Unitset nearbyEnemys = GR(attackerPos, 3 * attackerRange / 2, (BE || B(Neutral)) && !B(Invincible)); // Using minimum sight range as search radius; Working as intended

	int hightPriority = -99999;
	int lowHealth = 99999;

	for (auto u : nearbyEnemys)
	{
		if (!u->isDetected() && u->isVisible()) CT;
		if (Q == F[134] && u->isVisible()) CT;
		if (attackerType.airWeapon() == NW && (Q.isFlyer() || Q.isFlyingBuilding())) CT;
		if (attackerType.groundWeapon() == NW && (!Q.isFlyer() && !Q.isFlyingBuilding())) CT;
		if (target != NULL && !Q.canAttack()) CT; //if has target, do not attack the building first

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

	return target;
} // FindTarget()

bool canAttackHim(U mu, U hu) {
	if (!mu || !mu->exists() || !hu || !hu->exists()) return false;

	if (hu->isFlying()) { if (mu gt.airWeapon() != NW) return true; }
	else if (mu gt.groundWeapon() != NW) return true;

	return false;
}

bool canHitHim(U mu, U hu) {
	if (!canAttackHim(mu, hu)) return false;

	int muRange = hu->isFlying() ? mu gt.airWeapon().maxRange() : mu gt.airWeapon().maxRange();
	if (distSq<true>(mu->getPosition(), hu) <= muRange * muRange) return true;

	return false;
}

void SmartMove(U attacker, S targetPosition)
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
					return;
				}

	if (attacker->getLastCommandFrame() >= XF - 10) return; // if we have issued a command to this unit already this frame, ignore this one
	UnitCommand currentCommand(attacker->getLastCommand()); 	// get the unit's current command
	if ((currentCommand.getType() == UnitCommandTypes::Move) // if we've already told this unit to attack this target, ignore this command
		&& (currentCommand.getTargetPosition() == targetPosition)
		&& (XF - attacker->getLastCommandFrame() < 10)
		&& (attacker->isMoving())) return;

	attacker->move(targetPosition); // if nothing prevents it, move to the target
} // SmartMove

void SmartAttack(U attacker, U target)
{
	if (attacker == NULL || target == NULL) return;
	if (attacker->isIrradiated() || attacker->isUnderStorm()) SmartMove(attacker, S(D));

	V attackerType = attacker gt;

	// Special case: Zerg_Lurker
	if (attackerType == F[97] && !attacker->isBurrowed()) {
		if (attacker->getLastCommand().getType() != UnitCommandTypes::Burrow) attacker->burrow();
		return;
	}

	// if we have issued a command to this unit already this frame, ignore this one
	//if (attacker->getLastCommandFrame() >= XF - 5) return;

	// Data from: https://docs.google.com/spreadsheets/d/1bsvPvFil-kpvEUfSG74U3E5PLSTC02JxSkiR8QdLMuw/edit#gid=0
	//int attackCD = 10;
	//V attackerType = attacker gt;
	//if (attackerType == F[36]) attackCD = 5; // Zergling
	//M(attackerType == F[37]) attackCD = 3;	// Hydra
	//M(attackerType == F[38]) attackCD = 15; 
	//M(attackerType == F[97]) attackCD = 2; // Lurker

	UnitCommand currentCommand(attacker->getLastCommand()); // get the unit's current command
	//if (currentCommand.getType() == UnitCommandTypes::Attack_Unit &&	currentCommand.getTarget() == target
	//	&& (XF - attacker->getLastCommandFrame() <= attackCD)) return;

	if (target->isFlying() && attacker->getAirWeaponCooldown() != 0 || !target->isFlying() && attacker->getGroundWeaponCooldown() != 0) return;

	if (currentCommand.getType() != UnitCommandTypes::Attack_Unit || currentCommand.getTarget() != target)
		attacker->attack(target); // if nothing prevents it, attack the target
} // SmartAttack

void BB(int u, T b0 = D, T z = D) { // b0: build location search center; z: builder search center
	T intendedBuildLocation = NT;
	intendedBuildLocation = u == 135 ? FindBuildingPlacementAroundAPoint(F[u], b0) : GB(F[u], b0, 18);

	if (intendedBuildLocation != NT) {
		if (!GR(S(intendedBuildLocation), 80, BO && BW).empty()) return;

		if (O - cc > 120) {
			cc = O; U br = X GC(S(intendedBuildLocation), BW && (B(Idle) || B(GatheringMinerals) || !B(Moving)) && !B(CarryingMinerals) && !B(GatheringGas) && BO, 24 * 32);

			if (!L(br)) br = X GC(S(z), BW && (B(Idle) || B(GatheringMinerals) || !B(Moving)) && !B(CarryingMinerals) && !B(GatheringGas) && BO, 24 * 32);

			if (L(br)) br->build(F[u], intendedBuildLocation);
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

void GetMyNaturalAndOtherBases(BWEM::Map & mapBWEM) {
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

bool LurkerCautiousBurrow(U u) {
	if (GR(uGP, u->getType().sightRange(), BE && B(Sieged)).empty())
		if (U hisClosestAttacker = u GC(BE && Filter::CanAttack && !BF && !BW, lurkerSafeBurrowRange))
			if (!u->isBurrowed()) {
				if (u->getLastCommand().getType() != UnitCommandTypes::Burrow) u->burrow();
				return true;
			}

	return false;
}

void GoScouting(U u) {
	if (Q == F[97]) // Lurkers cautious burrow
		if (LurkerCautiousBurrow(u)) 
			return;

	if (Q == F[40]) { // Drone returning resource first..
		if (u->isCarryingGas() || u->isCarryingMinerals()) {
			if (u->getLastCommand().getType() != UnitCommandTypes::Return_Cargo) u->returnCargo();
			return;
		}
	}

	if (hisD != NT) { 
		if (Q == F[41]) {
			distSq2(uGP, S(D)) > 16 * 1024 ? SmartMove(u, S(D)) : "a"; 
			return; 
		}
		if (Q == F[40])
			if (U cm = X GC(S(D), BM, 7 * 32))
				ug(cm);
	}

	bool shouldGoofAround = false;

	if (hisD == NT && hisPossibleDs.size() == 1) hisD = hisPossibleDs.front();

	if (hisD != NT) {
		if (!shouldGoofAround) shouldGoofAround = true;
	}
	else {
		for (T sl : X->getStartLocations()) {
			if (sl == D) CT;

			if (me4or5Pool || me987Hydra && numStartingLocs == 4)
				if (Q != F[41] && sl == closestD || Q == F[41] && sl != closestD) 
					CT; // skip the first target which is to be covered by the overlord.
			
			S slPos = S(sl);

			bool hisDFound = false;

			if (!GR(slPos, 12 * 32, BE && Filter::IsBuilding).empty()) hisDFound = true;

			if (me987Hydra && Q == F[41] && !GR(uGP, 7 * 32, BE && Filter::GetType == Terran_Marine).empty()
				&& distSq2(uGP, slPos) < 1296 * 1024) hisDFound = true;

			if (T thisNatPos = FindNatPos(bwemMapInstance, sl)) {
				if (!GR(S(thisNatPos), 9 * 32, BE && Filter::IsBuilding).empty()) hisDFound = true;

				if (me1BaseLurker)
					if (GR(S(thisNatPos), 9 * 32, BE && Filter::CanAttack && !BW).size() >= 2) hisDFound = true;
			}

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
				M(distSq<true>(slPos, u) >= 50176) { SmartMove(u, slPos); return; }
			}
		}
	}

	if (shouldGoofAround) {
		if (u->getHitPoints() == u gt.maxHitPoints()) {
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

std::map<std::pair<V, V>, double> unitMatchupTable = unitMatchupTableGen;
bool meSmashGround() {
	//	lings,	   mutas,	  hydras	 lurkers    ultralisks
	if (!CC(36) && !CC(42) && !CC(37) && !CC(97) && !CC(38)) return false;

	if (hisD == NT) return false;

	if (hisD != NT && hisDKilled) return true;

	if (me987Hydra) return true;

	//if (me4or5Pool) return true;

	if (me1BaseLurker) return myUnitsCreated[97] >= 3;
	

	U myLeader = NULL;
	S hisDPos = S(hisD);
	int bestDist = 99999;

	// Get my army leader
	for (U u : C->getUnits()) {
		V uType = u gt;
		if (uType == F[36] || uType == F[42] || uType == F[37] || uType == F[97] || uType == F[38])
		{
			int distToHisD = static_cast<int>(BWEB::Map::getGroundDistance(uGP, hisDPos));
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
			hisScore += unitMatchupTable[make_pair(F[36], u gt)] * (u->getHitPoints() + u->getShields()) / double(u gt.maxHitPoints() + u gt.maxShields());
		for (auto u : friendsNearby)
			myScore += unitMatchupTable[make_pair(F[36], u gt)] * (u->getHitPoints() + u->getShields()) / double(u gt.maxHitPoints() + u gt.maxShields());

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

S vecToPos(vector<int> vecIn) {
	return S(T(vecIn.back(), vecIn.front()));
}

vector<int> posToVec(T posIn) {
	vector<int> res;
	res.push_back(posIn.y);
	res.push_back(posIn.x);
	return res;
}

vector<int> getNearestGoodTile(vector<int> posIn, int gridInfo[][128]) {
	// Return if this is already a good tile
	int posInX = posIn.front();
	int posInY = posIn.back();
	if (gridInfo[posInX][posInY] == 0) return posIn;

	// Spiral search around `posIn`
	for (int r = 1; r <= 128; ++r) {
		map<int, vector<int>> scoreAndPos;
		for (int i = posInX - r; i <= posInX + r; ++i)
			for (int j = posInY - r; j <= posInY + r; ++j)
				if (i >= 0 && i < X->mapWidth() && j >= 0 && j < X->mapHeight() && gridInfo[i][j] == 0) // Within the map and is good
					if (i == posInX - r || i == posInX + r || j == posInY - r || j == posInY + r) { // Only points from the surrounding square frame
						vector<int> posIJ = { i, j };
						scoreAndPos[distSq2(S(i, j), S(posInX, posInY))] = posIJ;
					}
					
		if (!scoreAndPos.empty())
			return scoreAndPos.begin()->second;
	}

	vector<int> myStartingTopLeft = { D.x, D.y };
	return myStartingTopLeft;
}

struct ExampleAIModule :AIModule {
	void onStart() {
		D = C->getStartLocation();
		X->setCommandOptimizationLevel(1);
		X->enableFlag(Flag::UserInput);

		if (ER == R1) enemyRaceName = "_P";
		else if (ER == R2) enemyRaceName = "_T";
		else if (ER == R3) enemyRaceName = "_Z";

		enemyName = XE->getName();

		ifstream mf("bwapi-data/read/" + enemyName + enemyRaceName + ".txt");
		if (mf) {
			int td = -1; int lc; while (mf >> lc) { ++td; GS[td] = lc; } mf.close();

			vector<int> feasibleBOs;
			switch (ER) {
			case R1: feasibleBOs = { 2 }; BK;
			case R2: feasibleBOs = { 7 }; BK;
			case R3: feasibleBOs = { 12 }; BK;
			default: feasibleBOs = { 12 }; BK;
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
		
		numStartingLocs = X->getStartLocations().size();

		if (enemyName.compare("Aurelien Lermant") == 0
			|| enemyName.compare("Marine Hell") == 0
			|| enemyName.compare("Hannes Bredberg") == 0
			|| enemyName.compare("Matej Istenik") == 0
			|| enemyName.compare("NiteKatP") == 0 // untested
			|| enemyName.compare("Sungguk Cha") == 0
			|| enemyName.compare("KaonBot") == 0
			|| enemyName.compare("EggBot") == 0
			|| enemyName.compare("Yuanheng Zhu") == 0
			|| enemyName.compare("Tomas Cere") == 0
			|| enemyName.compare("GuiBot") == 0 // untested
			//|| enemyName.compare("") == 0
			)
			G = 6; // ZvT 4 Pool
		else if (enemyName.compare("Stone") == 0
			|| enemyName.compare("Chris Coxe") == 0
			|| enemyName.compare("Dave Churchill") == 0 && ER == R3
			|| enemyName.compare("PurpleSwarm") == 0 || enemyName.compare("PurpleDestiny") == 0 && ER == R3 // untested
			|| enemyName.compare("legacy") == 0 && ER == R3
			) {
			G = 6;
			me7Pool = true;
		}
		else if (enemyName.compare("Dave Churchill") == 0 && ER != R3
			|| enemyName.compare("WuliBot") == 0
			|| enemyName.compare("legacy") == 0 && ER != R3 // Smorc through vs carriers
			|| enemyName.compare("KasoBot") == 0
			) {
			G = 7;
			if (enemyName.compare("legacy") == 0 && ER == R2) lurkerSafeBurrowRange = 256; // Extra range to be safe against stimmed marines
			if (enemyName.compare("KasoBot") == 0) myMaxSunks = 1;
		}
		else if (enemyName.compare("adias") == 0) {
			G = 10;
			me987Hydra = true;
		}
		else {
			if (ER == R1) G = 5;
			else if (ER == R2) G = 10;
			else if (ER == R3) G = 15;
		}
		
		/// vvv Fix things here vvv
		//myMaxSunks = 2;
		G = 6;
		//me987Hydra = true;
		//me7Pool = true;

		//////////////////////////////////////////////////
		bG = b[G - 1];
		cG = c[G - 1];
		me4or5Pool = G % 5 == 1;
		me1BaseLurker = G % 5 == 2; 
		me2BaseMuta = G % 5 == 3;
		me2BaseHydra = G % 5 == 4;
		meFastNat = me2BaseMuta || me2BaseHydra;
		meCOEP = me987Hydra ? false : G % 5 == 0;

		if (me4or5Pool)
			if (me7Pool)
				me4or5Pool = false;

		bwemMapInstance.Initialize();
		bwemMapInstance.FindBasesForStartingLocations();
		GetMyNaturalAndOtherBases(bwemMapInstance);
		BWEB::Map::onStart();
		myCP = S(BWEB::Map::getMainChoke()->Center());
		myknCP = S(BWEB::Map::getNaturalChoke()->Center());
		
		for (T u : X->getStartLocations())u != D ? h.push_back(u), hisPossibleDs.push_back(u) : "a"; p(k3); p(kn); p(D);

		for (T sl : X->getStartLocations()) myScoutTargetsAndStatus[sl] = false;

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

		// Only do this when his race is known!
		
		if (enemyRaceName.compare("Unknown") != 0) { 
			// Read his unit comp history from the last game..
			ifstream mf2("bwapi-data/read/" + enemyName + enemyRaceName + "_INFO.txt");
				if (mf2) {
					int td = -1; std::string lc; while (mf2 >> lc) { ++td; hisInfoLastGame[td] = lc; } mf2.close();
				}

			// Read my recent stats..
			ifstream mf3("bwapi-data/read/" + enemyName + enemyRaceName + "_RECENT.txt");
			if (mf3) {
				int td = -1; int lc; while (mf3 >> lc) { ++td; myRecentStats[td] = lc; } mf3.close();
			}
		}
	} // onStart()

	void onFrame() {
		//float time; { // Timer starts
			//ScopeTimer<float> timer{ time };

		// Initializing...
		O = XF;
		bool myNatBuilt = !GR(S(kn), 160, BO && BR).empty();
		bool my3rdBuilt = !GR(S(k3), 160, BO && BR).empty();
		
		// Find the index of an element in `F` (vector of all unit types)
		/*auto it = find(F.begin(), F.end(), Terran_Dropship);
		if (it != F.end() && O % 24 == 0) X << distance(F.begin(), it) << endl;*/

		// Updating his starting base status...
		if (hisD != NT && !hisDKilled)
			if (!GR(S(hisD), 160, BO).empty() && GR(S(hisD), 160, BE && BR).empty())
				hisDKilled = true;

		// Updating target queue...
		for (auto u = h.begin(); u != h.end();) u = X->isVisible(*u) && GI(*u, B(Building) && BE).empty() ? i[*u] = t, h.erase(u) : u + 1;
		for (U u : XE->getUnits())!i[P] && Q.isBuilding() ? i[P] = u, h.push_back(P) : "a";

		if (enemyRaceName.compare("Unknown") == 0
			&& O && O < 14400 && O % 720 == 0)
		{
			if (ER == R1) enemyRaceName = "_P";
			else if (ER == R2) enemyRaceName = "_T";
			else if (ER == R3) enemyRaceName = "_Z";
		}

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

				hisUnitCompPrev = hisInfoLastGame[(O - 1) / framesPerTick];
				hisUnitCompNext = hisInfoLastGame[(O - 1) / framesPerTick + 1];
			}

			if ((O - 1) % 4 == 0) {
				for (auto u : XE->getUnits()) {
					if (!u gt.isBuilding()) {
						if (u->exists()) {
							hisUnitIDAndInfo[u->getID()].unitPos = uGP;
							hisUnitIDAndInfo[u->getID()].unitSpd = std::make_pair(u->getVelocityX(), u->getVelocityY());
							hisUnitIDAndInfo[u->getID()].lastFrameVisible = O;
						} M(!u->isVisible() && O - hisUnitIDAndInfo[u->getID()].lastFrameVisible > 120) { // reset the speed data
							hisUnitIDAndInfo[u->getID()].unitSpd.first = hisUnitIDAndInfo[u->getID()].unitSpd.second = 0.0;
						}
					}
				}
			}
		}


		/// Grid info
		int gridInfo[128][128] = { 0 };
		if (updateGridInfo) {
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
			const int radialDist = 448;
			const double startAngle = PI / 4;
			const double deltaAngle = PI / 2;

			for (auto u : hisBuildingPosAndType) {
				if (u.second == Protoss_Photon_Cannon) {
					for (double iAngle = startAngle; iAngle < 2 * PI; iAngle += deltaAngle)
					{
						Position p12 = u.first + Position(static_cast<int>(radialDist * cos(iAngle)), static_cast<int>(radialDist * sin(iAngle)));

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
					for (double iAngle = startAngle; iAngle < 2 * PI; iAngle += deltaAngle)
					{
						Position p12 = u.second.unitPos + Position(static_cast<int>(radialDist * cos(iAngle)), static_cast<int>(radialDist * sin(iAngle)));

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
		} // if (updateGridInfo)

		// COEP Starts
		if (meCOEP) {
			if (myOrders.empty()) {
				vector<V> myFeasibleActions = myFeasibleActionsGen();
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
					}
				}

				if (myOrders.front() == Zerg_Extractor)
					if (CL(140) >= CL(123)
						|| CL(140) && GR(S(kn) + S(64, 48), 96, BO && BR).empty()
						|| CL(140) >= 2 && GR(S(k3) + S(64, 48), 96, BO && BR).empty())
						myOrders.erase(myOrders.begin());

				if (myOrders.front() == Zerg_Overlord)
					if (n < 14)
						myOrders.erase(myOrders.begin());
				X->drawTextScreen(10, 20, "Next COEP item: %s \n", myOrders.front().c_str());
			}
		} // COEP Ends

		// Tech switch
		if (!meGetMuta) {
			if (me7Pool || me4or5Pool)
				if (hisDKilled || O > 14400) // 10:00
					meGetMuta = true;

			if (me1BaseLurker) {
				//if (heMayCarrier != 2)
					if (hisDKilled || O > 17280) // 12:00
						meGetMuta = true;
			}
		}

		// Constructing my buildings...
		if (me7Pool) {
			if (meGetMuta) {
				if (CL(40) >= 7 && CL(140) == 0 && !GR(S(D), 320, B(ResourceContainer) && !BM).empty()) BB(140); // Extractor for tech switch
				if (CL(133) == 0 && CC(124)) BB(133); // Spire
			}

			if (CL(134) == 0 && C->minerals() >= 180 && n >= 12) BB(134, D); // Pool
			if (CL(134) && CL(40) >= 6 && CL(135) + CL(137) < 2) BB(135, D); // Sunk up
			if ((O > 4800 && CL(123) == 1 || O > 5520 && CL(123) == 2) && CL(134) && C->minerals() > 350) BB(123, D); // Additional hatcheries after 3:20 | 3:50
		}
		M(meCOEP) {
			if (GR(S(D), 256, BO&&BW).size() >= 4) {
				if (!myOrders.empty()) {
					bool hasHatNat = !GR(S(kn) + S(64, 48), 96, BO && BR).empty();
					bool hasHat3rd = !GR(S(k3) + S(64, 48), 96, BO && BR).empty();

					if (myOrders.front() != Zerg_Hatchery)
						if (CL(123) < 4 && CL(134) && C->minerals() > 350) { // Additional hatcheries
							if (hasHatNat) { 
								if (hasHat3rd) BB(123, D); 
								else BB(123, k3, D);
							}
							else BB(123, kn, D);
						}

					if (myOrders.front().isBuilding()) {
						int buildingTypeToBuild = unitTypeToInt(myOrders.front());

						if (buildingTypeToBuild == 123 && CL(123) == 1 && !hasHatNat) { // Expansion at nat
							BB(buildingTypeToBuild, kn);
						}
						M(buildingTypeToBuild == 123 && CL(123) == 2 && !hasHat3rd) { // Expansion at third
							BB(buildingTypeToBuild, k3);
						}
						M(buildingTypeToBuild == 135 && CC(123) > 1 && hasHatNat) { // Sunken at nat
							BB(buildingTypeToBuild, kn);
						}
						M(buildingTypeToBuild == 140 && CC(140) == 1 && hasHatNat 
							&& !GR(S(D), 8 * 32, BO && Filter::GetType == Zerg_Extractor).empty()) { // Extractor at nat
							BB(buildingTypeToBuild, kn);
						}
						M(buildingTypeToBuild == 140 && CC(140) == 2 && hasHatNat && hasHat3rd
							&& !GR(S(D), 8 * 32, BO && Filter::GetType == Zerg_Extractor).empty() 
							&& !GR(S(kn), 8 * 32, BO && Filter::GetType == Zerg_Extractor).empty()) { // Extractor at 3rd (k3)
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
			else { // no buildings when we very few workers
				if (!myOrders.empty())
					if (myOrders.front().isBuilding())
						myOrders.erase(myOrders.begin());
			}
		} // Constructing my buildings ends
		else { // Building construction when not on COEP...
			if ((me2BaseMuta || me2BaseHydra) && !GR(S(kn), 160, BO&&BW).empty() && CL(135) + CL(137) < 6 && C->minerals() >= 50) BB(135, T(myknCP));

			for (; R < (int)cG.size(); R++) {
				if (cG[R] == 134 && CL(134) == 0 && C->minerals() < 182) BK;

				if (cG[R] == 127 && CL(127) == 0)
					if (C->minerals() < 86 || C->gas() < 42) BK;

				if (CL(cG[R]) < (cG[R] == 123 ? 2 : 1) && n >= bG[R] * 2) { meFastNat && cG[R] == 123 && CL(cG[R]) == 1 ? BB(cG[R], kn) : BB(cG[R], D); BK; }
			}

			if (n > (me4or5Pool ? 45 : 23) && CL(140) < (int)GR(S(D), 320, B(ResourceContainer) && !BM).size()) BB(140); // Building extractor(s) near my main...
			M(CC(140) == 1 && CL(140) < 2 && myNatBuilt && !GR(S(kn), 320, BO&&BW&&B(Completed)).empty()) BB(140, kn, kn); // Building extractor(s) near my nat...
			M((me2BaseHydra || me2BaseMuta) && CC(140) == 2 && CL(140) < 3 && CC(123) >= 3 && !GR(S(k3), 320, BO&&BW&&B(Completed)).empty() && !GR(S(k3), 320, BO && BR).empty()) BB(140, k3, k3); // Building extractor(s) near my third...
			M(me4or5Pool) {
				if (CC(124) && !CL(133)) BB(133); // Get spire
				M(CL(123) < 4 && C->minerals() > 300) BB(123); // More hatches when possible when me4or5Pool
			}
			M(!me4or5Pool && my3rdBuilt && CC(123) < 6 && !countMyMorphingUnits(Zerg_Hatchery) && C->minerals() > 300) { // Macro hatcheries
				if (GR(S(D), 320, BO && BW).size() > 4 && GR(S(D), 320, BO && BR).size() < 3)  BB(123); // More hatches when possible
				if (GR(S(kn), 320, BO && BW).size() > 4 && GR(S(kn), 320, BO && BR).size() < 3)  BB(123, kn, kn); // More hatches when possible
				if (GR(S(k3), 320, BO && BW).size() > 4 && GR(S(k3), 320, BO && BR).size() < 3) BB(123, k3, k3); // More hatches when possible
			}
			M(me1BaseLurker) {
				if (CL(134) && CL(135) + CL(137) < myMaxSunks && C->minerals() > 100) BB(135); // Sunk up
				if (CL(124) && CL(127) == 0 && C->minerals() >= 100 && C->gas() >= 50) BB(127); // Get den after lair
				if (CL(135) + CL(137) >= myMaxSunks && CL(123) < 3 && C->minerals() > 350 && myAttackCondition) BB(123); // 2nd H after den
				if (meGetMuta && CL(133) == 0) BB(133);
			}
			M(me2BaseMuta || me2BaseHydra) {
				if (CC(123) >= 2 && !CI(123) && CC(140) == 2 && !GR(S(k3), 320, BO).empty() && C->minerals() > 300) BB(123, k3, kn); // Expansion at third
			}
			M(!(me2BaseHydra || me2BaseMuta || me987Hydra) && CL(cG[cG.size() - 1]) && CL(123) < 5 && C->minerals() > 400) BB(123); // More hatches when tech is ready
			M(me1BaseMuta && CL(123) < 3 && C->minerals() > 450) BB(123); // More hatches when possible
			M((me7Pool) && CL(123) < 3 && C->minerals() > 450) BB(123); // More hatches when possible
			M(O >= 34560 && GR(S(D), 320, BM).empty() && !GR(S(D), 320, BO&&BW).empty()) BB(135); // Spend the extra money for base defense after 24m
			M (me987Hydra) {
				if (CC(40) >= 8 && CL(123) < 2 && CC(140) && C->minerals() > 375) BB(123); // More hatcheries when see fit
				if (CL(134) && CL(140) == 0) BB(140);
			}
		}

		// Resetting...
		wp = w; np = n; xp = x; w = n = x = R = 0; map<U, int>es;
		bool myScoutFound = false;

		// Managing my units' behaviors...
		for (U u : C->getUnits()) {
			if (!u->exists() || !u->isCompleted() || u->isMaelstrommed() || u->isStasised() || u->isLoaded() || u->isStuck()) CT;

			if (u gt == Terran_Barracks || u->getBuildType() == Terran_Barracks) X << "RaxTL: " << u->getTilePosition() << endl;
			if (u gt == Terran_Bunker || u->getBuildType() == Terran_Bunker) X << "BunkerTL: " << u->getTilePosition() << endl;

			if (Q == F[0]) { // Terran_Wraith
				if (hisD != NT && !hisDKilled) {
					S uPos = uGP;
					S hisPos = S(hisD) + S(64, 48);
					S uNextPos = hisPos;
					const int pathfindMethod = 1;

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
					}
					else { // Using more conventional approaches such as vector*
						vector<int> adjUPos = getNearestGoodTile(posToVec(T(uPos.x / 32, uPos.y / 32)), gridInfo);

						uNextPos = vecToPos(runPathSearch(gridInfo, adjUPos, posToVec(hisD), false, false, false));
						X->drawCircleMap(vecToPos(adjUPos), 4, Colors::Purple, true);
						X->drawLineMap(uPos, vecToPos(adjUPos), Colors::Purple);
					}

					X->drawCircleMap(uNextPos, 8, Colors::Blue, true);
					X->drawLineMap(uPos, uNextPos, Colors::Blue);

					//SmartMove(u, uNextPos);
					if (u->getLastCommand().getType() != UnitCommandTypes::Move || u->getLastCommand().getTargetPosition() != uNextPos)
						u->move(uNextPos);

					if (distSq2(uPos, hisPos) < 49 * 1024)
						DO_ONCE{
						X << ">>> My HP: " << u->getHitPoints() << ", elapsed seconds: " << O / 24 << endl;
						X->sendText(std::to_string(averageFrameTime * 24 / float(O)).c_str(), "%cavg frame time (ms): %.1f\n");
					}
				}
				continue;
			}

			// Updating supply info...
			w += Q.supplyProvided(); n += Q.supplyRequired();

			// Closest enemy to this unit within ~ 6 * 32 range
			U Z = u GC(BE, 200); Z && !Z IM ? y[Z] = O : w; u->isStartingAttack() ? y[u] = O : w;

			// Upgrading/Researching/Morphing...
			if (Q.isBuilding()) {
				if (!me2BaseHydra && !me987Hydra) 
					P == D ? (!CL(124) ? ut(F[124]) : (!CL(125) ? ut(F[125]) : "a")) : "a"; // Main Hatch->Lair->Hive

				if (CC(125) && CC(134)) up(UT Adrenal_Glands); // Crackling
				if (CC(140) && CC(134) && !me987Hydra && !meGetMuta && !me1BaseLurker) up(UT Metabolic_Boost); // Ling speed
				if ((me1BaseLurker || me2BaseHydra) && CC(124) && CC(127)) ur(TT Lurker_Aspect); // Lurker_Aspect
				//if (me2BaseMuta) { up(VS); up(PC); } // Overlord transportation and speed
				if ((me2BaseHydra || me7Pool) && CC(127)) { up(UT Grooved_Spines); up(UT Muscular_Augments); } // Hydra range and speed upgrades
				if (meCOEP && CC(127)) { ur(TT Lurker_Aspect); up(UT Grooved_Spines); up(UT Muscular_Augments); }
				if (me987Hydra && CC(127) && C->minerals() > 225 && C->gas() > 175) { 
					up(UT Grooved_Spines);
					if (HU(UT Grooved_Spines)) up(UT Muscular_Augments);
				} // Hydra upgrades
				ut(F[137]); // Creep->Sunken
			}

			// Managing larvae (unit production)
			M(Q == F[34]) {
				if (meCOEP) {
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
				else { // Managing larvae when NOT on COEP..
					if (CC(125) && !HU(UT Adrenal_Glands) && !C->isUpgrading(UT Adrenal_Glands) // Prioritize crackling upgrade when we have Hive
						|| me1BaseLurker && (CC(124) && CC(127) && C->minerals() < 450
							&& !HR(TT Lurker_Aspect) && !C->isResearching(TT Lurker_Aspect)) // Prioritize Lurker_Aspect
						|| me7Pool && n >= 12 && !CL(134) // Prioritize pool
						) CT; // Cut production in favor of upgrades/researches

					if (me987Hydra) {
						if (C->supplyUsed() >= 14 && CL(134) && CL(140) == 0) CT; // Yield prio to extractor
						if (C->supplyUsed() < 18 && CL(40) < 9 && CL(134) && CL(140) && CL(127) == 0) ut(F[40]); // Drone up to 9 before hydra den
						if (C->supplyUsed() >= 16 && CL(134) == 0) CT; // Yield prio to pool
					}

					// Spawn MORE overlords
					if (wp < 400 && C->minerals() >= 100 && !(me987Hydra && CL(127) == 0)) { 
						if ((me1BaseLurker ? CC(127) : CC(cG[2])) && 1.0*np / wp > 0.8) 
							O - co > 660 ? co = O, ut(F[41]) : "a"; 
						M(np > 17 && wp - np < 4)O - co > 660 ? co = O, ut(F[41]) : "a"; 
					}
					

					// loop through all my bases and morph the larvae in the vicinity to drones...
					for (S ip : {S(D), S(kn)})
						if (ud(ip) < 128 && (int)GR(ip, 320, BO&&BW).size() < (me4or5PoolEarly ? bG[1] : 18) && CL(40) < (me4or5PoolEarly ? bG[1] : (myNatBuilt ? 36 : (me987Hydra ? 12 : 18)))) {
							if (!(me987Hydra && C->supplyUsed() >= 18 && myUnitsCreated[41] < 2 && CL(134)))
								if (!(me987Hydra && CL(127) && CL(40) >= 8))
									if (!(me7Pool && CL(40) >= 6))
										if (!(me1BaseLurker && CL(40) >= 12))
											ut(F[40]); // Drones

							if (meGetMuta && CL(40) < 12) ut(F[40]); // Drones
						}

					if (me4or5Pool) {
						ut(F[42]); // Mutas
						if (CC(133) && (CC(36) < 4 * CC(42) || C->gas() < 100) 
							|| !CC(133)) // Not having a spire..
							ut(F[36]); // Zerglings
					}
					M(me1BaseLurker) {
						if (CC(133)) ut(F[42]); // Mutas
						if ((HR(TT Lurker_Aspect) || C->isResearching(TT Lurker_Aspect)) && C->gas() > 25 && CL(37) < 3 * CC(140)) ut(F[37]); // Hydras
						if (CL(127) && countMyMorphingUnits(Zerg_Hydralisk) && !countMyMorphingUnits(Zerg_Zergling)
							&& CL(36) < 6 && C->gas() < 50 && C->minerals() > 125) ut(F[36]); // Zerglings (yielding priority to hydras)
					} // me1BaseLurker
					M(me2BaseMuta) {
						C->gas() > 100 && CC(133) ? ut(F[42]) : ut(F[36]);
					}
					M(me2BaseHydra) {
						C->gas() > 25 && CC(127) ? ut(F[37]) : ut(F[36]);
					}
					M(me1BaseMuta) {
						C->gas() > 100 && CC(133) ? ut(F[42]) : ut(F[36]);
					}
					M(me7Pool) {
						if (meGetMuta) CC(133) && C->gas() >= 100 ? ut(F[42]) : ut(F[36]);
						else ut(F[36]);
					}
					M(me987Hydra) {
						if (C->gas() > 25 && CC(127)) ut(F[37]); // Mass hydras
						if (C->gas() < 10 && CC(134) && myUnitsCreated[37] >= 9) ut(F[36]); // Mix in lings when we don't have resources
					}
				}
				CT;
			} // M(Q == F[34]) // larvae...

			// Managing hydras...
			M(Q == F[37]) {
				if (HR(TT Lurker_Aspect)) // when there are avilable hydras to morph into lurkers..
					if (me1BaseLurker && CC(97) < 2 * CC(37)
						|| me2BaseHydra && 2 * CC(97) < CC(37)
						|| meCOEP && CC(97) < 6)
						ut(F[97]); // into lurkers!
			}

			// Managing drones...
			M(Q == F[40] || Q == F[13]) { // Drone or SCV
				// Go scouting when my candidate is found
				if (hisD == NT && u->getID() == myScoutID) { myScoutFound = true; GoScouting(u); CT; }

				// Set up scouting params when we have pool..
				if (hisD == NT && CL(134) && CL(40) > 3 && O - cs > 240 && !u->isCarryingMinerals() && !u->isCarryingGas()) {
					bool timeToScout = false;
					if (me987Hydra && numStartingLocs == 4 && CL(127)) timeToScout = true;
					if (me1BaseLurker && O < 4320) timeToScout = true; // 3:00

					if (myUnitsCreated[40] - CC(40) <= 1 && CC(41) >= 2
						|| (me987Hydra || me1BaseLurker) && timeToScout) { // When it is time to scout...
						if (myScoutID < 0) { cs = O; myScoutID = u->getID(); myScoutFound = true; GoScouting(u); CT; }
					}
				}

				if (Z && (O - y[Z]) < 99 && !Z IF) s(u), K != Z ? ua(Z) : "a"; // Self defense
				M ((me987Hydra || me1BaseLurker) && CC(140) == 1) { // Force drones to mine gas ASAP on 1st extractor completion
					int myGasGatherers = count_if(q.begin(), q.end(), [](std::pair<U, U> u){ return L(u.second) && u.second->getType() == F[140]; });
					
					if (myGasGatherers < 3 && CL(127) == 0)
						if (!u->isGatheringGas() && !u->isCarryingMinerals())
							if (U myClosestExtractor = u GC(BO && Filter::GetType == Zerg_Extractor))
								if (u->getLastCommand().getType() != UnitCommandTypes::Right_Click_Unit
									|| u->getLastCommand().getTarget() != myClosestExtractor) {
									if (!q.empty() && q[u] != myClosestExtractor) q[u] = myClosestExtractor;
									u->rightClick(myClosestExtractor);
									CT;
								}

					if (CL(127) && me987Hydra || me1BaseLurker && CL(134)) { // when we have the tech building..
						if (O % 240 == 0) // Do this every X seconds
							if (u->isIdle() && !u->isMoving())
								if (U cm = X GC(S(D), BM, 7 * 32))
									ug(cm);

						if (me1BaseLurker) {
							if (CL(40) > 7 && myGasGatherers < 3)
								if (!u->isGatheringGas())
									if (U myClosestExtractor = u GC(BO && Filter::GetType == Zerg_Extractor)) {
										if (!q.empty() && q[u] != myClosestExtractor) q[u] = myClosestExtractor;
										ug(myClosestExtractor);
										CT;
									}
						}
						if (me987Hydra)
							if (myGasGatherers > 1)
								if (u->isGatheringGas() || u->isCarryingGas())
									if (U myClosestMineral = u GC(BM))
										if (u->getLastCommand().getType() != UnitCommandTypes::Gather
											|| u->getLastCommand().getTarget() != myClosestMineral) {
											if (!q.empty() && q[u] != myClosestMineral) q[u] = myClosestMineral;
											ug(myClosestMineral);
										}
					}
				}
				M((ud(S(kn)) < 256 || ud(S(k3)) < 256) && CL(123) > 1) { if (u->isIdle() && !u->isMoving()) if (U cm = u GC(BM)) { ug(cm); CT; } } // Mining at natural/third
				M(m.size() && !q[u]) {
					if (ug(m[0]))q[u] = m[0], m.erase(m.begin());
				}
				M(K&&K->getResources() && K != q[u])ug(q[u]); // Pressing workers to mine mineral or gas, whichever is mapped to
				M(ud(S(D)) < 320) {
					if (u->isIdle() && !u->isMoving() && !u->isGatheringGas() && !u->isCarryingGas()) {
						//if (U ce = u GC(BO && B(Completed) && Filter::GetType == Zerg_Extractor)) { ug(ce); CT; } 
						if (U cm = u GC(BM)) { ug(cm); CT; }
					}
				} // Refining/Mining at main
			}

			// Managing overlords...
			M(Q == F[41]) {
				if (me4or5Pool || me987Hydra && numStartingLocs > 2) {
					if (hisD == NT) { GoScouting(u); CT; }
					if (u->isUnderAttack() || hisD != NT) SmartMove(u, S(D));
				}
				else {
					if (u->isUnderAttack()) SmartMove(u, S(D));

					if (!me1BaseLurker) {
						GR(S(kn), 250, BO).empty() && !NL ? um(S(kn)) : "a";

						if (CC(123) >= 2) {
							if (GR(S(k3), 250, BO).empty() && !NL && O - ck3 > 620) {
								ck3 = O;
								um(S(k3));
							}
						}
					}
				}
			}

			// Managing my army: 
			//	lings		  mutas 		hydras		  lurkers		Ultralisks
			if (Q == F[36] || Q == F[42] || Q == F[37] || Q == F[97] || Q == F[38]) {
				if (me1BaseLurker) {
					if (Q == F[97]) { // Lurkers cautious burrow
						if (LurkerCautiousBurrow(u)) CT;
					} M(Q == F[37]) {
						if (myUnitsCreated[97] < 10
							&& (hisD == NT || distSq2(uGP, S(hisD)) > 1024 * 1024))  // Hydras stay with lurkers early
						{
							if (U hisClosestAttacker = u GC(BE, 128)) {
								if (!u->isHoldingPosition()) u->holdPosition();
								CT;
							}

							if (CC(97)) { // if we have lurkers
								if (U myClosestLurker = u GC(BO && Filter::GetType == F[97])) {
									if (distSq2(uGP, myClosestLurker->getPosition()) > 4 * 1024) {
										SmartMove(u, myClosestLurker->getPosition());
										CT;
									}
									else {
										if (!u->isHoldingPosition()) u->holdPosition();
										CT;
									}
								}
							}
							else { // fight with sunks
								if (U myClosestSunk = u GC(BO && Filter::GetType == F[137])) {
									if (distSq2(uGP, myClosestSunk->getPosition()) > 4 * 1024) {
										SmartMove(u, myClosestSunk->getPosition());
										CT;
									}
									else {
										if (!u->isHoldingPosition()) u->holdPosition();
										CT;
									}
								}
							}
						}
					} // hydra
				} // me1BaseLurker

				if (hisD == NT) { GoScouting(u); CT; }
				if (hisDKilled) { // Search and destroy
					if (U Z2 = FindTarget(u)) {
						if (L(Z2)) {
							int thisRange = GetAttackRange(u gt, Z2 gt);
							distSq<true>(Z2->getPosition(), u) > thisRange * thisRange ? SmartMove(u, Z2->getPosition()) : SmartAttack(u, Z2);
						}
					}
					M(U Z3 = u GC(BE&&B(Building))) {
						if (L(Z3)) SmartMove(u, Z3->getPosition());
					}
					M(!u->isMoving()) {
						if (u gt.isFlyer()) {
							SmartMove(u, S(rand() % X->mapWidth() * 32, rand() % X->mapHeight() * 32));
							CT;
						}
						else {
							auto it = distsAndBases.begin();
							std::advance(it, rand() % distsAndBases.size());
							double random_key = it->first;
							SmartMove(u, S(distsAndBases[random_key]));
							CT;
						}
					}
				}
				else { // when his starting main is not destroyed..
					if (U ZZ = FindTarget(u)) {
						if (L(ZZ)) {
							if (ZZ gt.isBuilding() && !ZZ gt.canAttack())
							{
								if (u gt.groundWeapon() != NW && u gt.airWeapon() == NW) {
									if (U nearestThreat = u GC(BE && Filter::CanAttack && !B(Flying), 3 * u gt.sightRange() / 2))
										ZZ = nearestThreat;
								} M (u gt.groundWeapon() == NW && u gt.airWeapon() != NW) {
									if (U nearestThreat = u GC(BE && Filter::CanAttack && B(Flying), 3 * u gt.sightRange() / 2))
										ZZ = nearestThreat;
								} M (u gt.groundWeapon() != NW && u gt.airWeapon() != NW) {
									if (U nearestThreat = u GC(BE && Filter::CanAttack, 3 * u gt.sightRange() / 2))
										ZZ = nearestThreat;
								}
							}

							if (me987Hydra) // Ignore his scouting worker
								if (ZZ gt.isWorker() && distSq2(ZZ->getPosition(), S(D)) < 400 * 1024)
									if (GR(S(D), 20 * 32, BE && Filter::CanAttack).size() <= 2)
										if (hisD != NT && !hisDKilled)
										{
											SmartMove(u, S(hisD));
											CT;
										}

							if (Q == F[37] && ZZ gt.groundWeapon() != NW && ZZ gt.groundWeapon().maxRange() < 128) { // Hydra kiting
								double thisRange = GetAttackRange(F[37]);
								double thisSpeed = HU(UT Muscular_Augments) ? 5.49 : 3.66;
								double timeToEnter = std::max(0.0, (sqrt(distSq2(uGP, ZZ->getPosition())) - thisRange) / thisSpeed); // Marine speed is 4.0 pixels / frame
								if (timeToEnter < u->getGroundWeaponCooldown())
								{
									SmartMove(u, S(D));
									CT;
								}
							}

							//Horizon::updateUnit(u, ZZ);
							//X->drawTextMap(uGP, "%cSmSc: %.2f", Text::Orange, Horizon::getSimValue(u, 240).attackGroundasGround);
							if (myAttackCondition) { 
								int thisRange = GetAttackRange(u gt, ZZ gt);
								distSq2(uGP, ZZ->getPosition()) > thisRange * thisRange ? SmartMove(u, ZZ->getPosition()) : SmartAttack(u, ZZ);
							}
							else SmartMove(u, S(D));
							CT;
						}
					}

					if (hisD != NT && !u->isUnderAttack()) {
						// Wait group
						if (Q == F[37] && CC(37) > 1 && me987Hydra) {
							if (U myClosestUnitToHim = X->getClosestUnit(Position(hisD), BO && Filter::GetType == Zerg_Hydralisk, 64 * 32))
								if (static_cast<int>(GR(myClosestUnitToHim->getPosition(), 7 * 32, BO && Filter::GetType == Zerg_Hydralisk).size()) < CC(37) * 2 / 3)
								{
									if (u == myClosestUnitToHim) {
										if (!u->isHoldingPosition()) u->holdPosition();
									}
									else SmartMove(u, myClosestUnitToHim->getPosition());
									CT;
								}
						}

						if (distSq2(uGP, S(hisD) + S(64, 48)) <= u gt.sightRange() * u gt.sightRange() * 1024)
						{
							U hisBldg = NULL;
							if (u gt.groundWeapon() != NW && u gt.airWeapon() == NW) {
								if (U nearestBldg = u GC(BE && B(Building) && !B(Flying), u gt.sightRange()))
									hisBldg = nearestBldg;
							} M(u gt.groundWeapon() == NW && u gt.airWeapon() != NW) {
								if (U nearestBldg = u GC(BE && B(Building) && B(Flying), u gt.sightRange()))
									hisBldg = nearestBldg;
							} M(u gt.groundWeapon() != NW && u gt.airWeapon() != NW) {
								if (U nearestBldg = u GC(BE && B(Building), u gt.sightRange()))
									hisBldg = nearestBldg;
							}

							if (hisBldg && hisBldg != NULL) { SmartAttack(u, hisBldg); CT; }
						}
					}

					// Save starting main in danger..
					if (U hisClosestAttacker = X GC(S(D), BE && Filter::CanAttack && !Filter::IsWorker && !B(Invincible), 320)) {
						if (hisClosestAttacker->isFlying() && u->getType().airWeapon() != NW || !hisClosestAttacker->isFlying() && u->getType().groundWeapon() != NW)
						{
							SmartMove(u, S(D)); 
							CT;
						}
					}

					(myAttackCondition || np > nToRetreat) ? SmartMove(u, S(hisD)), (nToRetreat == 999 ? (nToRetreat = CC(40) + max(np - 2 * CC(40), 0) / 6) : 0) :
						(myNatBuilt ? (GR(S(kn), 9 * 32, BO && Filter::GetType == Zerg_Sunken_Colony).empty() ? SmartMove(u, S(kn)) : SmartMove(u, u GC(BO && Filter::GetType == Zerg_Sunken_Colony)->getPosition())) : SmartMove(u, S(D)), nToRetreat == 999 ? 0 : nToRetreat = 999);
				}
			}

			// Managing scourges (avoid overkill)
			if (Q == F[46])if (U ZZ = u GC(BE&&BF, 400)) L(ZZ) && es[ZZ] <= (ZZ->getHitPoints() + ZZ->getShields()) / 110 ? es[ZZ]++, ua(ZZ) : "a";
		} // for (U u : C->getUnits())

		if (!myScoutFound) myScoutID = -99;
			/// Display the time spent per frame
			//if (O % 24 == 0) { 
			//	averageFrameTime += time;
			//	//X << "Elapsed seconds: " << O / 24 << ", ms taken: " << std::to_string(time) << endl;
			//	//X->sendText(std::to_string(time).c_str(), "\n"); 
			//}

		//} // Timer ends
	}  // onFrame()

	void onUnitMorph(U u) {
		if (u->getPlayer() == X->self()) {
			if (u->getBuildType() == F[40]) myUnitsCreated[40]++; // Drone
			M(u->getBuildType() == F[41]) myUnitsCreated[41]++;	  // Overlord
			M(u->getBuildType() == F[42]) myUnitsCreated[42]++;   // Muta 
			M(u->getBuildType() == F[46]) myUnitsCreated[46]++;	  // Scourge	
			M(u->getBuildType() == F[123]) myUnitsCreated[123]++; // Hatchery
			M(u->getBuildType() == F[97]) myUnitsCreated[97]++;   // Lurker
		}
	}

	void onUnitComplete(U u) {
		if (u->getPlayer() == X->self()) {
			if (me987Hydra && Q == F[140]) {
				;
			} else Q == F[140] ? o(u), o(u) : "a";

			if (O < 24) { // Count the initial 4 drones and 1 overlord
				if (Q == F[40]) myUnitsCreated[40]++; // Drone
				M(Q == F[41]) myUnitsCreated[41]++;	  // Overlord
			}
		}
	}

	void onUnitDiscover(U u) {
		if (u->getPlayer() == XE) {
			V uType = u gt;
			if (uType == Terran_Siege_Tank_Siege_Mode) uType = Terran_Siege_Tank_Tank_Mode;

			if (!uType.isBuilding()) { 
				hisUnitIDAndInfo[u->getID()].unitType = uType;
				hisUnitIDAndInfo[u->getID()].unitPos = uGP;
				hisUnitIDAndInfo[u->getID()].unitSpd = std::make_pair(u->getVelocityX(), u->getVelocityY());
				hisUnitIDAndInfo[u->getID()].lastFrameVisible = O;
			}
			M(uType == Protoss_Photon_Cannon ||
				uType == Terran_Bunker || uType == Terran_Missile_Turret ||
				uType == Zerg_Sunken_Colony || uType == Zerg_Spore_Colony)
				hisBuildingPosAndType[uGP] = uType;
		}
	}

	void onUnitDestroy(U u) {
		if (u->getPlayer() == XE) {
			V uType = u gt;
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
				auto it = hisBuildingPosAndType.find(uGP);
				if (it != hisBuildingPosAndType.end())
					hisBuildingPosAndType.erase(it);
			}
		} M(u->getPlayer() == C) {
			// ...
		}
	}

	/*void onUnitHide(U u) {
		if (u->getPlayer() == XE) {
			V uType = u gt;
			if (uType == Terran_Siege_Tank_Siege_Mode) uType = Terran_Siege_Tank_Tank_Mode;

			if (!uType.isBuilding()) {
				hisUnitIDAndInfo[u->getID()].unitType = uType;
				hisUnitIDAndInfo[u->getID()].unitPos = uGP;
				hisUnitIDAndInfo[u->getID()].unitSpd = std::make_pair(u->getVelocityX(), u->getVelocityY());
			}
		}
	}*/

	void onEnd(bool u) {
		// 1st File to Write
		std::ostringstream mfo;
		int numStats = sizeof(GS) / sizeof(GS[0]);
		int GMod5 = G % 5;

		for (int ki = 0; ki < numStats; ++ki) {
			if (ki % 2 == 0 && GMod5 == ki / 2 + 1 || ki % 2 && u && GMod5 == (ki + 1) / 2) GS[ki]++;
			mfo << GS[ki] << "\n";
		}

		ofstream mf("bwapi-data/write/" + enemyName + enemyRaceName + ".txt", std::ofstream::trunc);
		if (mf)
		{
			mf << mfo.str();
			mf.flush();
		}
		mf.close();

		// 2nd File to Write
		std::ostringstream mfo2;
		for (auto i : hisInfo)
			mfo2 << i << "\n";

		ofstream mf2("bwapi-data/write/" + enemyName + enemyRaceName + "_INFO" + ".txt", std::ofstream::trunc);
		if (mf2)
		{
			mf2 << mfo2.str();
			mf2.flush();
		}
		mf2.close();

		// 3rd File to Write
		std::ostringstream mfo3;
		numStats = sizeof(myRecentStats) / sizeof(myRecentStats[0]);
		for (int i = 1; i < numStats; ++i)
			mfo3 << myRecentStats[i] << "\n";

		mfo3 << GMod5 * 10 + u << "\n";

		ofstream mf3("bwapi-data/write/" + enemyName + enemyRaceName + "_RECENT" + ".txt", std::ofstream::trunc);
		if (mf3)
		{
			mf3 << mfo3.str();
			mf3.flush();
		}
		mf3.close();
	}
};

