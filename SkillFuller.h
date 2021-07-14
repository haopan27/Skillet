#include<BWAPI.h>
#include<fstream>
#include "BWEM/bwem.h"
#include "BWEB/BWEB.h"
#include "BattleCommander.h"
#include <chrono>
#include "PathSearch.h"
#include "Horizon/Horizon.h"

#define C X->self()
#define F vector<V>(UnitTypes::allUnitTypes().begin(),UnitTypes::allUnitTypes().end())
#define XE X->enemy()
#define XR XE->getRace()
#define GB X->getBuildLocation
#define GC ->getClosestUnit
#define GP ->getPosition()
#define GR X->getUnitsInRadius
#define K u->getOrderTarget()
#define P u->getTilePosition()
#define gt ->getType()
#define Q u gt
#define L(z)(z&&z->exists()&&z->isDetected())
#define B(z)Filter::Is##z
#define BE B(Enemy)
#define BF B(Flying)
#define BM B(MineralField)
#define BO B(Owned)
#define BR B(ResourceDepot)
#define BW B(Worker)
#define FGT Filter::GetType
#define ua u->attack
#define ud u->getDistance
#define ug u->gather
#define ut u->morph
#define um u->move
#define up u->upgrade
#define ur u->research
#define HU C->getUpgradeLevel
#define UT UpgradeTypes::
#define HR C->hasResearched
#define TT TechTypes::
#define R1 Races::Protoss
#define R2 Races::Terran
#define R3 Races::Zerg
#define NW WeaponTypes::None
#define NP Positions::None
#define NT TilePositions::None
#define me4or5PoolEarly me4or5Pool && CL(123) < 2 && !meGetMuta
#define DO_ONCE for (static int numTimesDone = 0 ; !numTimesDone++ ; )
#define PI 3.141592653589793238462643383279502884L

using namespace std; 
using namespace BWAPI; 
using S = Position; 
using T = TilePosition; 
using U = Unit; 
using V = UnitType; 
auto&X = Broodwar;

vector<string> hisInfo;
std::string hisInfoLastGame[360] = { "-" };
const int secondsPerTick = 10;
const int framesPerTick = secondsPerTick * 24;
float averageFrameTime = 0.0;
int lurkerSafeBurrowRange = 192;
const int numMyRecentStats = 10;
int myRecentStats[numMyRecentStats] = { 0 };

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
		else if(u gt.isBuilding() && u->getBuildType() == v)
			res++;
	}
	return res;
}
int CL(int u) { return CC(u) + CI(u) + (u == 123 ? CC(124) + countMyMorphingUnits(Zerg_Lair) + countMyMorphingUnits(Zerg_Hive) : 0); } // count my total units

vector<vector<int>> b = {
	{22,4,60},		// BO#1 4 Pool
	{8,10},			// BO#2 1baseLurker
	{12,12,12,13},	// BO#3 12H3HLing
	{12,12,12,16},	// BO#4 9Pool2HLing
	{8, 7, 8}		// BO#5 COEP
},
c = {
	{123,134,133},		// BO#1 4 Pool
	{134,140},			// BO#2 1baseLurker
	{123,134,123,140},	// BO#3 12H3HLing
	{123,134,140,127},	// BO#4 9Pool2HLing
	{134, 140, 127}		// BO#5 COEP
};

vector<int>bG, cG;// build order timing and what to build ==> pertaining to a specific build order
vector<T>h; // target queue (vector)
map<T, U>enemyBldgTLAndUnit; // target queue (map)
map<U, U>q; // map of worker gather assignments
map<U, int> enemyUnitAndFrameAttackStarted; // map of latest frame when an enemy unit either attacked or repaired or a friendly unit started an attack
map<double, T>distsAndBases;
bool me4or5Pool = false;
bool me1BaseLurker = false;
bool me7Pool = false;
bool meCOEP = false;
bool me987Hydra = false;
bool meGetMuta = false;
bool me3HLing = false;

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
int myMaxSunks = 1;
int myMaxSpores = 0;

int n, np;	// current used supply, its prevVal (doubles the actual value, due to zerglings occupying 1/2 supply)
int w, wp;	// current max supply, its prevVal
int R, O;		// iterator for 'b' and 'c' (both), frame count
int cc, co = 0;	// guard frame used in 'BB', guard frame used in morphing overlords
int cs, ck3, ckn;  // guard frame to avoid calling too many scouts, guard frame to avoid calling too many overlords to k3/kn
int G = 0; // build selector
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
		S pos2 = unit2 GP;
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
	else if(Q.isSpellcaster() && !Q.isFlyer()) return 10;
	else if(Q.airWeapon() != NW && !Q.isFlyer()) return 9;
	else if(Q.isWorker()) return 8;
	else if(Q == F[150] || Q == F[137]) return 7;	// Static defense: Protoss_Photon_Cannon, Zerg_Sunken_Colony
	else if(Q.groundWeapon() != NW && Q.isFlyer()) return 6;
	else if(Q.isRefinery() || Q == F[146]) return 5; // Protoss_Pylon
	else if(Q.isResourceDepot()) return 4;
	else if(Q.gasPrice() > 0) return 3; // Buildings that cost gas
	else if(Q.mineralPrice() > 0) return 2;
	else if(Q.isNeutral()) return 1;
	else return 1;
} // GetAttackPriority

U FindTarget(U attacker)
{
	S attackerPos = attacker GP;
	V attackerType = attacker gt;
	int attackerRange = attackerType.sightRange();
	U target = NULL;

	Unitset nearbyEnemys = GR(attackerPos, 3 * attackerRange / 2, (BE || B(Neutral)) && !B(Invincible)); // Using minimum sight range as search radius; Working as intended

	int hightPriority = -99999;
	int lowHealth = 99999;

	for (auto u : nearbyEnemys)
	{
		if (!u->isDetected() && u->isVisible()) continue;
		if (Q == F[134] && u->isVisible()) continue;
		if (attackerType.airWeapon() == NW && (Q.isFlyer() || Q.isFlyingBuilding())) continue;
		if (attackerType.groundWeapon() == NW && (!Q.isFlyer() && !Q.isFlyingBuilding())) continue;
		if (target != NULL && !Q.canAttack()) continue; //if has target, do not attack the building first

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
	if (distSq<true>(mu GP, hu) <= muRange * muRange) return true;

	return false;
}

void SmartMove(U attacker, S targetPosition)
{
	// Special case: Zerg_Lurker
	if (attacker gt == F[97])
		if (attacker->isBurrowed()) {
			if (GR(attacker GP, 192, BE && !BF).empty())
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

	if (attacker->getLastCommandFrame() >= O - 10) return; // if we have issued a command to this unit already this frame, ignore this one
	UnitCommand currentCommand(attacker->getLastCommand()); 	// get the unit's current command
	if ((currentCommand.getType() == UnitCommandTypes::Move) // if we've already told this unit to attack this target, ignore this command
		&& (currentCommand.getTargetPosition() == targetPosition)
		&& (O - attacker->getLastCommandFrame() < 10)
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
	//if (attacker->getLastCommandFrame() >= O - 5) return;

	// Data from: https://docs.google.com/spreadsheets/d/1bsvPvFil-kpvEUfSG74U3E5PLSTC02JxSkiR8QdLMuw/edit#gid=0
	//int attackCD = 10;

	UnitCommand currentCommand(attacker->getLastCommand()); // get the unit's current command
	//if (currentCommand.getType() == UnitCommandTypes::Attack_Unit &&	currentCommand.getTarget() == target
	//	&& (O - attacker->getLastCommandFrame() <= attackCD)) return;

	if (target->isFlying() && attacker->getAirWeaponCooldown() != 0 || !target->isFlying() && attacker->getGroundWeaponCooldown() != 0) return;

	if (currentCommand.getType() != UnitCommandTypes::Attack_Unit || currentCommand.getTarget() != target)
		attacker->attack(target); // if nothing prevents it, attack the target
} // SmartAttack

void BB(int u, T b0 = D, T z = D) { // b0: build location search center; z: builder search center
	T intendedBuildLocation = NT;
	intendedBuildLocation = u == 135 ? FindBuildingPlacementAroundAPoint(F[u], b0) : GB(F[u], b0, 18);

	if (intendedBuildLocation != NT) {
		if (!GR(S(intendedBuildLocation), 64, BO && BW).empty()) return;

		if (O - cc > 240) {
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
		if (area.AccessibleNeighbours().empty()) continue;
		for (auto &base : area.Bases()) {
			// Must have gas, be accesible and at least 5 mineral patches
			if (base.Geysers().empty() || base.Minerals().size() < 5) continue;

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
		if (area.AccessibleNeighbours().empty()) continue;
		for (auto &base : area.Bases()) {
			// Must have gas, be accesible and at least 5 mineral patches
			if (base.Geysers().empty() || base.Minerals().size() < 5) continue;

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
	if (GR(u GP, u->getType().sightRange(), BE && B(Sieged)).empty())
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

	// Lings deal with cannon rush
	if (Q == F[36] && XR == R1) {
		if (U hisBldg = u GC(BE && B(Building), 160))
			if (distSq2(hisBldg GP, S(D)) < 2025 * 1024)
			{
				SmartAttack(u, hisBldg);
				return;
			}
	}

	if (hisD == NT && hisPossibleDs.size() == 1) hisD = hisPossibleDs.front();

	if (hisD != NT) {
		if (Q == F[41]) {
			distSq2(u GP, S(D)) > 16 * 1024 ? SmartMove(u, S(D)) : "a";
			return;
		}
		if (Q == F[40])
			if (U cm = X GC(S(D), BM, 7 * 32))
				ug(cm);
	}
	else {
		for (T sl : X->getStartLocations()) {
			if (sl == D) continue;

			if (me4or5Pool || me987Hydra && numStartingLocs == 4)
				if (Q != F[41] && sl == closestD || Q == F[41] && sl != closestD) 
					continue; // skip the first target which is to be covered by the overlord.
			
			S slPos = S(sl);

			bool hisDFound = false;

			if (!GR(slPos, 12 * 32, BE && Filter::IsBuilding).empty()) hisDFound = true;

			if (me987Hydra && Q == F[41] && !GR(u GP, 7 * 32, BE && FGT == F[2]).empty()
				&& distSq2(u GP, slPos) < 1296 * 1024) hisDFound = true;

			if (T thisNatPos = FindNatPos(bwemMapInstance, sl)) {
				if (!GR(S(thisNatPos), 9 * 32, BE && Filter::IsBuilding).empty()) hisDFound = true;

				if (me1BaseLurker)
					if (GR(S(thisNatPos), 9 * 32, BE && Filter::CanAttack && !BW).size() >= 2) hisDFound = true;
			}

			if (hisDFound) {
				if (hisD == NT) hisD = sl;
				myScoutTargetsAndStatus[sl] = true; 

				if (Q == F[41]) {
					distSq2(u GP, S(D)) > 16 * 1024 ? SmartMove(u, S(D)) : "a";
					return;
				}
				if (Q == F[40])
					if (U cm = X GC(S(D), BM, 7 * 32))
						ug(cm);

				break;
			}

			if (!myScoutTargetsAndStatus[sl]) { // not visited yet
				// 50176 == (7 * 32) ^ 2
				if (distSq<true>(slPos, u) < 50176 && GR(slPos, 224, BE && !BW).empty()) {
					if (hisPossibleDs.size() >= 2)
						hisPossibleDs.erase(remove(hisPossibleDs.begin(), hisPossibleDs.end(), sl), hisPossibleDs.end());

					myScoutTargetsAndStatus[sl] = true; continue;
				}
				else if(distSq<true>(slPos, u) >= 50176) { SmartMove(u, slPos); return; }
			}
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

	if (me1BaseLurker) return myUnitsCreated[97] >= 3;

	if (me3HLing) return myUnitsCreated[36] >= 20;

	U myLeader = NULL;
	S hisDPos = S(hisD);
	int bestDist = 99999;

	// Get my army leader
	for (U u : C->getUnits()) {
		V uType = u gt;
		if (uType == F[36] || uType == F[42] || uType == F[37] || uType == F[97] || uType == F[38])
		{
			int distToHisD = static_cast<int>(BWEB::Map::getGroundDistance(u GP, hisDPos));
			if (distToHisD < bestDist)
			{
				myLeader = u;
				bestDist = distToHisD;
			}
		}
	}

	if (myLeader != NULL) {
		S myLeaderPos = myLeader GP;
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

		if (XR == R1) enemyRaceName = "_P";
		else if (XR == R2) enemyRaceName = "_T";
		else if (XR == R3) enemyRaceName = "_Z";

		enemyName = XE->getName();

		ifstream mf("bwapi-data/read/" + enemyName + enemyRaceName + ".txt");
		if (mf) {
			int td = -1; int lc; while (mf >> lc) { ++td; GS[td] = lc; } mf.close();

			vector<int> feasibleBOs = { 1, 2, 3, 4, 5 };
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
		else G = 5;
		
		// Only do this when enemy race is known!
		if (enemyRaceName.compare("Unknown") != 0) {
			// Read enemy unit comp history from the last game..
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

		numStartingLocs = X->getStartLocations().size();

		
		
		/// vvv fix things here vvv
		//myMaxSunks = 4;
		//myMaxSpores = 1;
		G = 3;
		//me987Hydra = true;
		//me7Pool = true;

		/*[1] "Final time: 303"
		[1] "Final order"
		[1] 1 1 1 1 2 1 1 3 1 1 7 1 7 6 6 4 6 1 6 2 5 6 1 1 1 6 6 6 6 6 2 6 6 6 6 6 6 6 6 2 6 6 6 6 6 6 6
		[48] 6 6 2*/

		//////////////////////////////////////////////////
		bG = b[G - 1];
		cG = c[G - 1];
		if (G == 1) me4or5Pool = true;
		else if (G == 2) me1BaseLurker = true; 
		else if (G == 3) me3HLing = true;
		else if (G == 5) {
			if (!me987Hydra) meCOEP = true;
		}
		if (me4or5Pool && me7Pool) me4or5Pool = false;

		bwemMapInstance.Initialize();
		bwemMapInstance.FindBasesForStartingLocations();
		GetMyNaturalAndOtherBases(bwemMapInstance);
		BWEB::Map::onStart();
		myCP = S(BWEB::Map::getMainChoke()->Center());
		myknCP = S(BWEB::Map::getNaturalChoke()->Center());
		
		for (T u : X->getStartLocations())u != D ? h.push_back(u), hisPossibleDs.push_back(u) : "a"; p(k3); p(kn); p(D);

		for (T sl : X->getStartLocations()) myScoutTargetsAndStatus[sl] = false;

		hisInfo.reserve(3600 / secondsPerTick + 1); // Assuming BASIL env, where games last for 60 minutes

		if (XR == R1) hisTypesToCheck = {
			Protoss_Probe, Protoss_Zealot, Protoss_Dragoon,
			Protoss_High_Templar, Protoss_Dark_Templar, Protoss_Archon,
			Protoss_Dark_Archon, Protoss_Reaver, Protoss_Observer,
			Protoss_Shuttle, Protoss_Scout, Protoss_Carrier,
			Protoss_Arbiter, Protoss_Corsair, Protoss_Photon_Cannon };
		else if (XR == R2) hisTypesToCheck = {
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
	} // onStart()

	void onFrame() {
		//float time; { // Timer starts
			//ScopeTimer<float> timer{ time };

		// Initializing...
		O = X->getFrameCount();
		bool myNatBuilt = !GR(S(kn), 160, BO && BR).empty();
		bool my3rdBuilt = !GR(S(k3), 160, BO && BR).empty();
		
		//if (O % 120 == 0) X << "my lings created: " << myUnitsCreated[36] << " @ " << O / 24 << "(s)" << endl;

		// Find the index of an element in `F` (vector of all unit types)
		/*auto it = find(F.begin(), F.end(), Terran_Dropship);
		if (it != F.end() && O % 24 == 0) X << distance(F.begin(), it) << endl;*/

		// Updating enemy starting base status...
		if (hisD != NT && !hisDKilled)
			if (!GR(S(hisD), 160, BO).empty() && GR(S(hisD), 160, BE && BR).empty())
				hisDKilled = true;
		
		// Updating target queue...
		for (auto u = h.begin(); u != h.end();) u = X->isVisible(*u) && X->getUnitsOnTile(*u, B(Building) && BE).empty() ? enemyBldgTLAndUnit[*u] = t, h.erase(u) : u + 1;
		for (U u : XE->getUnits())!enemyBldgTLAndUnit[P] && Q.isBuilding() ? enemyBldgTLAndUnit[P] = u, h.push_back(P) : "a";

		if (enemyRaceName.compare("Unknown") == 0
			&& O && O < 14400 && O % 720 == 0)
		{
			if (XR == R1) enemyRaceName = "_P";
			else if (XR == R2) enemyRaceName = "_T";
			else if (XR == R3) enemyRaceName = "_Z";
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
							hisUnitIDAndInfo[u->getID()].unitPos = u GP;
							hisUnitIDAndInfo[u->getID()].unitSpd = std::make_pair(u->getVelocityX(), u->getVelocityY());
							hisUnitIDAndInfo[u->getID()].lastFrameVisible = O;
						} else if(!u->isVisible() && O - hisUnitIDAndInfo[u->getID()].lastFrameVisible > 120) { // reset the speed data
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
						if (myWayPointsAndStatus[p12] == 1 && !GR(p12, 32, Filter::IsOwned && FGT == F[0]).empty()) {
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
						if (myWayPointsAndStatus2[p12] == 1 && !GR(p12, 32, Filter::IsOwned && FGT == F[0]).empty()) {
							myWayPointsAndStatus2[p12] = 2;
							wayPointsExpended++;
						}

						X->drawCircleMap(p12, 8, Colors::White, true);
						X->drawTextMap(p12 + Position(0, 12), "%cwaypointStatus: %d", Text::White, myWayPointsAndStatus2[p12]);
					}
				}
			}

			// Display intel on enemy units
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
			int earliestTimeToStart = 99999;
			if (me3HLing) earliestTimeToStart = 11520; // 8:00
			else if (me7Pool || me4or5Pool) earliestTimeToStart = 14400; // 10:00
			else if (me1BaseLurker) earliestTimeToStart = 17280; // 12:00

			if (hisDKilled || O > earliestTimeToStart)
				meGetMuta = true;
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
		else if(meCOEP) {
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
						else if(buildingTypeToBuild == 123 && CL(123) == 2 && !hasHat3rd) { // Expansion at third
							BB(buildingTypeToBuild, k3);
						}
						else if(buildingTypeToBuild == 135 && CC(123) > 1 && hasHatNat) { // Sunken at nat
							BB(buildingTypeToBuild, kn);
						}
						else if(buildingTypeToBuild == 140 && CC(140) == 1 && hasHatNat 
							&& !GR(S(D), 8 * 32, BO && FGT == F[140]).empty()) { // Extractor at nat
							BB(buildingTypeToBuild, kn);
						}
						else if(buildingTypeToBuild == 140 && CC(140) == 2 && hasHatNat && hasHat3rd
							&& !GR(S(D), 8 * 32, BO && FGT == F[140]).empty()
							&& !GR(S(kn), 8 * 32, BO && FGT == F[140]).empty()) { // Extractor at 3rd (k3)
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
			if (me3HLing) {
				if (C->supplyUsed() >= 24 && CL(123) == 1 && C->minerals() > 168) BB(123); // 2nd H
				if (C->supplyUsed() >= 22 && CL(123) == 2 && C->minerals() > 80 && CL(134) == 0) BB(134); // Pool
				if (CL(134) && CL(123) == 2 && C->minerals() > 180) BB(123); // 3rd H
				if (CL(123) >= 3 && CL(140) == 0 && C->minerals() > 50) BB(140);
				if (CC(124) && CL(133) == 0 && C->minerals() > 200 && C->gas() > 150) BB(133); // Spire
			}

			if (!me3HLing) {
				for (; R < (int)cG.size(); R++) {
					if (cG[R] == 134 && CL(134) == 0 && C->minerals() < 182) break;

					if (cG[R] == 127 && CL(127) == 0)
						if (C->minerals() < 86 || C->gas() < 42) break;

					if (CL(cG[R]) < (cG[R] == 123 ? 2 : 1) && n >= bG[R] * 2) { BB(cG[R], D); break; }
				}
			}

			if (!me3HLing && n > (me4or5Pool ? 45 : 23) && CL(140) < (int)GR(S(D), 320, B(ResourceContainer) && !BM).size()) BB(140); // Building extractor(s) near my main...
			else if(CC(140) == 1 && CL(140) < 2 && myNatBuilt && !GR(S(kn), 320, BO&&BW&&B(Completed)).empty()) BB(140, kn, kn); // Building extractor(s) near my nat...
			else if(me4or5Pool) {
				if (CC(124) && CL(133) == 0) BB(133); // Get spire
				else if(CL(123) < 4 && C->minerals() > 300) BB(123); // More hatches when possible when me4or5Pool
			}
			else if(!me4or5Pool && my3rdBuilt && CC(123) < 6 && !countMyMorphingUnits(Zerg_Hatchery) && C->minerals() > 300) { // Macro hatcheries
				if (GR(S(D), 320, BO && BW).size() > 4 && GR(S(D), 320, BO && BR).size() < 3)  BB(123); // More hatches when possible
				if (GR(S(kn), 320, BO && BW).size() > 4 && GR(S(kn), 320, BO && BR).size() < 3)  BB(123, kn, kn); // More hatches when possible
				if (GR(S(k3), 320, BO && BW).size() > 4 && GR(S(k3), 320, BO && BR).size() < 3) BB(123, k3, k3); // More hatches when possible
			}
			else if(me1BaseLurker) { // Constructions
				if (CL(134) && CL(135) + CL(137) < myMaxSunks && C->minerals() > 100) BB(135); // Sunk up
				if (CL(124) && CL(127) == 0 && C->minerals() >= 100 && C->gas() >= 50) BB(127); // Get den after lair
				if (CL(135) + CL(137) >= myMaxSunks && CL(123) < 3 && C->minerals() > 350 && myAttackCondition) BB(123); // 2nd H after den
				if (meGetMuta && CL(133) == 0) BB(133);

				if (myMaxSpores) {
					if (O > 6000 // 4:10
						&& CL(131) == 0 && C->minerals() > 100) BB(131); // Evo chamber

					if (CC(131) && CL(135) + CL(137) + CL(136) < myMaxSunks + myMaxSpores) BB(135); // Creep Colony
				}
			}
			
			else if(!me987Hydra && !me3HLing && CL(cG[cG.size() - 1]) && CL(123) < 5 && C->minerals() > 400) BB(123); // More hatches when tech is ready
			else if(O >= 34560 && GR(S(D), 320, BM).empty() && !GR(S(D), 320, BO&&BW).empty()) BB(135); // Spend the extra money for base defense after 24m
			else if (me987Hydra) {
				if (CC(40) >= 8 && CL(123) < 2 && CC(140) && C->minerals() > 375) BB(123); // More hatcheries when see fit
				if (CL(134) && CL(140) == 0) BB(140);
			}
		}

		// Resetting...
		wp = w; np = n; w = n = R = 0; map<U, int>es;
		bool myScoutFound = false;

		// Managing my units' behaviors...
		for (U u : C->getUnits()) {
			if (!u->exists() || !u->isCompleted() || u->isMaelstrommed() || u->isStasised() || u->isLoaded() || u->isStuck()) continue;

			if (u gt == Terran_Barracks || u->getBuildType() == Terran_Barracks) X << "RaxTL: " << u->getTilePosition() << endl;
			if (u gt == Terran_Bunker || u->getBuildType() == Terran_Bunker) X << "BunkerTL: " << u->getTilePosition() << endl;

			if (Q == F[0]) { // Terran_Wraith
				if (hisD != NT && !hisDKilled) {
					S uPos = u GP;
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
			U Z = u GC(BE, 200); Z && !Z->isMoving() ? enemyUnitAndFrameAttackStarted[Z] = O : w; u->isStartingAttack() ? enemyUnitAndFrameAttackStarted[u] = O : w;

			// Upgrading/Researching/Morphing...
			if (Q.isBuilding()) {
				if (!me987Hydra && !me3HLing) 
					P == D ? (!CL(124) ? ut(F[124]) : (!CL(125) ? ut(F[125]) : "a")) : "a"; // Main Hatch->Lair->Hive

				if (me3HLing && meGetMuta && P == D && CL(124) == 0) ut(F[124]);

				if (CC(125) && CC(134)) up(UT Adrenal_Glands); // Crackling
				if (CC(140) && CC(134) && !me987Hydra && !me1BaseLurker) up(UT Metabolic_Boost); // Ling speed
				if (me1BaseLurker && CC(124) && CC(127)) ur(TT Lurker_Aspect); // Lurker_Aspect
				if (me7Pool && CC(127)) { up(UT Grooved_Spines); up(UT Muscular_Augments); } // Hydra range and speed upgrades
				if (meCOEP && CC(127)) { ur(TT Lurker_Aspect); up(UT Grooved_Spines); up(UT Muscular_Augments); }
				if (me987Hydra && CC(127) && C->minerals() > 225 && C->gas() > 175) { 
					up(UT Grooved_Spines);
					if (HU(UT Grooved_Spines)) up(UT Muscular_Augments);
				} // Hydra upgrades
				if (myMaxSpores) {
					if (CC(131) && CL(136) < myMaxSpores) ut(F[136]); // Creep->Spore
					if (CL(136) < myMaxSpores && CL(137) >= myMaxSunks) continue; // Prevent excessive sunkens
				}
				ut(F[137]); // Creep->Sunken
			}

			// Managing larvae (unit production)
			else if(Q == F[34]) {
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
						) continue; // Cut production in favor of upgrades/researches

					if (me987Hydra) {
						if (C->supplyUsed() >= 14 && CL(134) && CL(140) == 0) continue; // Yield prio to extractor
						if (C->supplyUsed() < 18 && CL(40) < 9 && CL(134) && CL(140) && CL(127) == 0) ut(F[40]); // Drone up to 9 before hydra den
						if (C->supplyUsed() >= 16 && CL(134) == 0) continue; // Yield prio to pool
					}

					// Spawn MORE overlords
					if (wp < 400 && C->minerals() >= 100 && !(me987Hydra && CL(127) == 0)) {
						if (me3HLing && C->supplyTotal() > 18 && C->supplyTotal() < 86) {
							if (C->supplyUsed() >= 28 && myUnitsCreated[41] < 3
								|| C->supplyUsed() >= 42 && myUnitsCreated[41] < 4
								|| C->supplyUsed() >= 58 && myUnitsCreated[41] < 5
								|| C->supplyUsed() >= 72 && myUnitsCreated[41] < 6) {
								if (CL(123) >= 3 && O - co > 660) {
									ut(F[41]);
										co = O;
								}
							}
						}
						else {
							if ((me1BaseLurker ? CC(127) : CC(cG[2])) && 1.0*np / wp > 0.8)
								O - co > 660 ? co = O, ut(F[41]) : "a";
							else if (np > 17 && wp - np < 4)O - co > 660 ? co = O, ut(F[41]) : "a";
						}
					}
					

					// loop through all my bases and morph the larvae in the vicinity to drones...
					for (S ip : {S(D), S(kn)})
						if (ud(ip) < 128 && (int)GR(ip, 320, BO&&BW).size() < (me4or5PoolEarly ? bG[1] : 18) && CL(40) < (me4or5PoolEarly ? bG[1] : (myNatBuilt ? 36 : (me987Hydra ? 12 : 18)))) {
							if (!(me987Hydra && C->supplyUsed() >= 18 && myUnitsCreated[41] < 2 && CL(134)))
								if (!(me987Hydra && CL(127) && CL(40) >= 8))
									if (!(me7Pool && CL(40) >= 6))
										if (!(me1BaseLurker && CL(40) >= 12))
											if (!(me3HLing && CL(40) >= 12))
												ut(F[40]); // Drones

							if (meGetMuta && CL(40) < 12) ut(F[40]); // Drones
						}

					if (me4or5Pool) {
						ut(F[42]); // Mutas
						if (CC(133) && (CC(36) < 4 * CC(42) || C->gas() < 100) 
							|| !CC(133)) // Not having a spire..
							ut(F[36]); // Zerglings
					}
					else if(me1BaseLurker) { // Unit production..
						if (CC(133)) ut(F[42]); // Mutas
						if ((HR(TT Lurker_Aspect) || C->isResearching(TT Lurker_Aspect)) && C->gas() > 25 && CL(37) < 3 * CC(140)) ut(F[37]); // Hydras
						if (CL(127) && countMyMorphingUnits(Zerg_Hydralisk) && !countMyMorphingUnits(Zerg_Zergling)
							&& CL(36) < 6 && C->gas() < 50 && C->minerals() > 125) ut(F[36]); // Zerglings (yielding priority to hydras)
					} // me1BaseLurker
					else if (me3HLing) {
						if (C->gas() >= 100) ut(F[42]); // muta
						if (CL(123) >= 3) ut(F[36]);
					}
					else if(me7Pool) {
						if (meGetMuta) CC(133) && C->gas() >= 100 ? ut(F[42]) : ut(F[36]);
						else ut(F[36]);
					}
					else if(me987Hydra) {
						if (C->gas() > 25 && CC(127)) ut(F[37]); // Mass hydras
						if (C->gas() < 10 && CC(134) && myUnitsCreated[37] >= 9) ut(F[36]); // Mix in lings when we don't have resources
					}
				}
				continue;
			} // else if(Q == F[34]) // larvae...

			// Managing hydras...
			else if(Q == F[37]) {
				if (HR(TT Lurker_Aspect)) // when there are avilable hydras to morph into lurkers..
					if (me1BaseLurker && CC(97) < 2 * CC(37)
						|| meCOEP && CC(97) < 6)
						ut(F[97]); // into lurkers!
			}

			// Managing drones...
			else if(Q == F[40] || Q == F[13]) { // Drone or SCV
				// Go scouting when my candidate is found
				if (hisD == NT && u->getID() == myScoutID) { myScoutFound = true; GoScouting(u); continue; }

				// Set up scouting params when we have pool..
				if (hisD == NT && CL(134) && CL(40) > 3 && O - cs > 240 && !u->isCarryingMinerals() && !u->isCarryingGas()) {
					bool timeToScout = false;
					if (O < 4320) { // 3:00
						if (me987Hydra && numStartingLocs == 4 && CL(127)) timeToScout = true;
						if (me1BaseLurker) timeToScout = true; 
						if (me4or5Pool && CL(40) >= 4) timeToScout = true;
						if (me3HLing && CL(123) + CL(134) >= 2 && numStartingLocs == 4) timeToScout = true;
					}

					if (myUnitsCreated[40] - CC(40) <= 1 && CC(41) >= 2
						|| timeToScout) { // When it is time to scout...
						if (myScoutID < 0) { cs = O; myScoutID = u->getID(); myScoutFound = true; GoScouting(u); continue; }
					}
				}

				if (Z && (O - enemyUnitAndFrameAttackStarted[Z]) < 99 && !Z->isFlying()) s(u), K != Z && distSq2(Z GP, S(D)) < 225 * 1024 ? ua(Z) : "a"; // worker defense
				else if ((me987Hydra || me1BaseLurker || me3HLing) && CC(140) == 1) { // Force drones to mine gas ASAP on 1st extractor completion
					int myGasGatherers = count_if(q.begin(), q.end(), [](std::pair<U, U> u){ return L(u.second) && u.second->getType() == F[140]; });
					
					if (myGasGatherers < 3)
						if (CL(127) == 0 && !me3HLing || me3HLing && !HU(UT Metabolic_Boost) && !C->isUpgrading(UT Metabolic_Boost))
							if (!u->isGatheringGas() && !u->isCarryingMinerals())
								if (U myClosestExtractor = u GC(BO && FGT == F[140]))
									if (u->getLastCommand().getType() != UnitCommandTypes::Right_Click_Unit
										|| u->getLastCommand().getTarget() != myClosestExtractor) {
										if (!q.empty() && q[u] != myClosestExtractor) q[u] = myClosestExtractor;
										u->rightClick(myClosestExtractor);
										continue;
									}

					if (CL(127) && me987Hydra || me1BaseLurker && CL(134)
						|| me3HLing && (HU(UT Metabolic_Boost) || C->isUpgrading(UT Metabolic_Boost))) { // dispatch drones between mining and extracting
						if (O % 240 == 0) // Do this every X seconds
							if (u->isIdle() && !u->isMoving())
								if (U cm = X GC(S(D), BM, 7 * 32))
									ug(cm);

						if (me1BaseLurker) {
							if (CL(40) > 7 && myGasGatherers < 3)
								if (!u->isGatheringGas())
									if (U myClosestExtractor = u GC(BO && FGT == F[140])) {
										if (!q.empty() && q[u] != myClosestExtractor) q[u] = myClosestExtractor;
										ug(myClosestExtractor);
										continue;
									}
						}

						if (me987Hydra && myGasGatherers > 2 || me3HLing && myGasGatherers)
							if (u->isGatheringGas() || u->isCarryingGas())
								if (U myClosestMineral = u GC(BM))
									if (u->getLastCommand().getType() != UnitCommandTypes::Gather
										|| u->getLastCommand().getTarget() != myClosestMineral) {
										if (!q.empty() && q[u] != myClosestMineral) q[u] = myClosestMineral;
										ug(myClosestMineral);
										continue;
									}
					}
				}
				else if((ud(S(kn)) < 256 || ud(S(k3)) < 256) && CL(123) > 1) { if (u->isIdle() && !u->isMoving()) if (U cm = u GC(BM)) { ug(cm); continue; } } // Mining at natural/third
				else if(m.size() && !q[u]) {
					if (ug(m[0]))q[u] = m[0], m.erase(m.begin());
				}
				else if(K&&K->getResources() && K != q[u])ug(q[u]); // Pressing workers to mine mineral or gas, whichever is mapped to
				else if(ud(S(D)) < 320) {
					if (u->isIdle() && !u->isMoving() && !u->isGatheringGas() && !u->isCarryingGas()) {
						if (U cm = u GC(BM)) { ug(cm); continue; }
					}
				} // Refining/Mining at main
			}

			// Managing overlords...
			else if(Q == F[41]) {
				if (me4or5Pool || me7Pool || me987Hydra || me3HLing) {
					if (hisD == NT) { GoScouting(u); continue; }
					if (u->isUnderAttack() || hisD != NT) SmartMove(u, S(D));
				}
				else {
					if (u->isUnderAttack()) SmartMove(u, S(D));

					if (!me1BaseLurker) {
						GR(S(kn), 250, BO).empty() && u->getLoadedUnits().empty() ? um(S(kn)) : "a";

						if (CC(123) >= 2) {
							if (GR(S(k3), 250, BO).empty() && u->getLoadedUnits().empty() && O - ck3 > 620) {
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
						if (LurkerCautiousBurrow(u)) continue;
					} else if(Q == F[37]) {
						if (myUnitsCreated[97] < 10
							&& (hisD == NT || distSq2(u GP, S(hisD)) > 1024 * 1024))  // Hydras stay with lurkers early
						{
							if (U hisClosestAttacker = u GC(BE, 128)) {
								if (!u->isHoldingPosition()) u->holdPosition();
								continue;
							}

							if (CC(97)) { // if we have lurkers
								if (U myClosestLurker = u GC(BO && FGT == F[97])) {
									if (distSq2(u GP, myClosestLurker GP) > 4 * 1024) {
										SmartMove(u, myClosestLurker GP);
										continue;
									}
									else {
										if (!u->isHoldingPosition()) u->holdPosition();
										continue;
									}
								}
							}
							else { // fight with sunks
								if (U myClosestSunk = u GC(BO && FGT == F[137])) {
									if (distSq2(u GP, myClosestSunk GP) > 4 * 1024) {
										SmartMove(u, myClosestSunk GP);
										continue;
									}
									else {
										if (!u->isHoldingPosition()) u->holdPosition();
										continue;
									}
								}
							}
						}
					} // hydra
				} // me1BaseLurker

				if (Q == F[36]) { // Zergling counter attack
					if (U hisClosestAttacker = u GC(BE && Filter::CanAttack && !BF, 32)) {
						if (u->getLastCommand().getType() != UnitCommandTypes::Attack_Unit || u->getLastCommand().getTarget() != hisClosestAttacker)
							ua(hisClosestAttacker);
						continue;
					}

					if (me3HLing) {
						if (!myAttackCondition) {
							if (U hisScout = X GC(S(D), BE && BW, 15 * 32))
								if (U myLing = hisScout GC(BO && FGT == F[36]))
									if (u == myLing) {
										if (u->getLastCommand().getType() != UnitCommandTypes::Attack_Unit || u->getLastCommand().getTarget() != hisScout)
											ua(hisScout);
										continue;
									}

							if (hisD != NT && !hisDKilled)
								if (S hisNatTL = S(FindNatPos(bwemMapInstance, hisD)))
									if (!u->isUnderAttack())
									{
										S hisNatCenter = hisNatTL + S(64, 48);
										if (distSq2(u GP, hisNatCenter) > 900 * 1024) {
											SmartMove(u, hisNatCenter);
											continue;
										}
										else {
											if (!u->isHoldingPosition()) u->holdPosition();
											continue;
										}
									}
						}
					}
				}

				if (Q == F[37]) { // Hydra micro to tackle sieged up tanks
					if (U hisClosestTank = u GC(BE && B(Sieged), 15 * 32)) {
						int distToTank2 = distSq2(u GP, hisClosestTank GP);
						if (distToTank2 > 4 * 1024 && u->getGroundWeaponCooldown()) {
							SmartMove(u, hisClosestTank GP);
							continue;
						}

						int thisRange = GetAttackRange(F[37]);
						if (distToTank2 <= thisRange * thisRange && u->getGroundWeaponCooldown() == 0) {
							if (u->getLastCommand().getType() != UnitCommandTypes::Attack_Unit || u->getLastCommand().getTarget() != hisClosestTank)
								ua(hisClosestTank);
							continue;
						}
					}
				}

				if (hisD == NT) { GoScouting(u); continue; }
				if (hisDKilled) { // Search and destroy
					if (U Z2 = FindTarget(u)) {
						if (L(Z2)) {
							int thisRange = GetAttackRange(u gt, Z2 gt);
							distSq<true>(Z2 GP, u) > thisRange * thisRange ? SmartMove(u, Z2 GP) : SmartAttack(u, Z2);
						}
					}
					else if(U Z3 = u GC(BE&&B(Building))) {
						if (L(Z3)) SmartMove(u, Z3 GP);
					}
					else if(!u->isMoving()) {
						if (u gt.isFlyer()) {
							SmartMove(u, S(rand() % X->mapWidth() * 32, rand() % X->mapHeight() * 32));
							continue;
						}
						else {
							auto it = distsAndBases.begin();
							std::advance(it, rand() % distsAndBases.size());
							double random_key = it->first;
							SmartMove(u, S(distsAndBases[random_key]));
							continue;
						}
					}
				}
				else { // when enemy starting main is not destroyed..
					if (U ZZ = FindTarget(u)) {
						if (L(ZZ)) {
							if (ZZ gt.isBuilding() && !ZZ gt.canAttack())
							{
								if (u gt.groundWeapon() != NW && u gt.airWeapon() == NW) {
									if (U nearestThreat = u GC(BE && Filter::CanAttack && !B(Flying), 3 * u gt.sightRange() / 2))
										ZZ = nearestThreat;
								} else if (u gt.groundWeapon() == NW && u gt.airWeapon() != NW) {
									if (U nearestThreat = u GC(BE && Filter::CanAttack && B(Flying), 3 * u gt.sightRange() / 2))
										ZZ = nearestThreat;
								} else if (u gt.groundWeapon() != NW && u gt.airWeapon() != NW) {
									if (U nearestThreat = u GC(BE && Filter::CanAttack, 3 * u gt.sightRange() / 2))
										ZZ = nearestThreat;
								}
							}

							if (me987Hydra) // Ignore enemy scouting worker
								if (ZZ gt.isWorker() && distSq2(ZZ GP, S(D)) < 400 * 1024)
									if (GR(S(D), 20 * 32, BE && Filter::CanAttack).size() <= 2)
										if (hisD != NT && !hisDKilled)
										{
											SmartMove(u, S(hisD));
											continue;
										}

							if (Q == F[37] && ZZ gt.groundWeapon() != NW && ZZ gt.groundWeapon().maxRange() < 128) { // Hydra kiting
								double thisRange = GetAttackRange(F[37]);
								double thisSpeed = HU(UT Muscular_Augments) ? 5.49 : 3.66;
								double timeToEnter = std::max(0.0, (sqrt(distSq2(u GP, ZZ GP)) - thisRange) / thisSpeed); // Marine speed is 4.0 pixels / frame
								if (timeToEnter < u->getGroundWeaponCooldown())
								{
									SmartMove(u, S(D));
									continue;
								}
							}

							//Horizon::updateUnit(u, ZZ);
							//X->drawTextMap(u GP, "%cSmSc: %.2f", Text::Orange, Horizon::getSimValue(u, 240).attackGroundasGround);
							if (myAttackCondition) {
								if (Q == F[97])
								{
									int thisRange = GetAttackRange(u gt, ZZ gt);
									distSq2(u GP, ZZ GP) > thisRange * thisRange ? SmartMove(u, ZZ GP) : SmartAttack(u, ZZ);
								} else SmartAttack(u, ZZ);
							}
							else SmartMove(u, S(D));
							continue;
						}
					}

					if (hisD != NT && !u->isUnderAttack()) {
						// Wait group
						if (Q == F[37] && CC(37) > 1 && me987Hydra) {
							if (U myClosestUnitToHim = X GC(Position(hisD), BO && FGT == F[37], 64 * 32))
								if (static_cast<int>(GR(myClosestUnitToHim GP, 7 * 32, BO && FGT == F[37]).size()) < CC(37) * 2 / 3)
								{
									if (u == myClosestUnitToHim) {
										if (!u->isHoldingPosition()) u->holdPosition();
									}
									else SmartMove(u, myClosestUnitToHim GP);
									continue;
								}
						}

						if (Q == F[36] && CC(36) > 1 && me3HLing) {
							if (U myClosestUnitToHim = X GC(Position(hisD), BO && FGT == F[36], 64 * 32))
								if (static_cast<int>(GR(myClosestUnitToHim GP, 7 * 32, BO && FGT == F[36]).size()) < CC(36) / 2)
								{
									if (u == myClosestUnitToHim) {
										if (!u->isHoldingPosition()) u->holdPosition();
									}
									else SmartMove(u, myClosestUnitToHim GP);
									continue;
								}
						}

						if (distSq2(u GP, S(hisD) + S(64, 48)) <= u gt.sightRange() * u gt.sightRange() * 1024)
						{
							U hisBldg = NULL;
							if (u gt.groundWeapon() != NW && u gt.airWeapon() == NW) {
								if (U nearestBldg = u GC(BE && B(Building) && !B(Flying), u gt.sightRange()))
									hisBldg = nearestBldg;
							} else if(u gt.groundWeapon() == NW && u gt.airWeapon() != NW) {
								if (U nearestBldg = u GC(BE && B(Building) && B(Flying), u gt.sightRange()))
									hisBldg = nearestBldg;
							} else if(u gt.groundWeapon() != NW && u gt.airWeapon() != NW) {
								if (U nearestBldg = u GC(BE && B(Building), u gt.sightRange()))
									hisBldg = nearestBldg;
							}

							if (hisBldg && hisBldg != NULL) { SmartAttack(u, hisBldg); continue; }
						}
					}

					// Save starting main in danger..
					if (U hisClosestAttacker = X GC(S(D), BE && Filter::CanAttack && !Filter::IsWorker && !B(Invincible), 320)) {
						if (hisClosestAttacker->isFlying() && u->getType().airWeapon() != NW || !hisClosestAttacker->isFlying() && u->getType().groundWeapon() != NW)
						{
							SmartMove(u, S(D)); 
							continue;
						}
					}

					(myAttackCondition || np > nToRetreat) ? SmartMove(u, S(hisD)), (nToRetreat == 999 ? (nToRetreat = CC(40) + max(np - 2 * CC(40), 0) / 6) : 0) :
						(myNatBuilt ? (GR(S(kn), 9 * 32, BO && FGT == F[137]).empty() ? SmartMove(u, S(kn)) : SmartMove(u, u GC(BO && FGT == F[137]) GP)) : SmartMove(u, S(D)), nToRetreat == 999 ? 0 : nToRetreat = 999);
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
			if (u->getBuildType() == F[40]) myUnitsCreated[40]++;		// Drone
			else if (u->getBuildType() == F[36]) myUnitsCreated[36]++;	// Ling
			else if(u->getBuildType() == F[41]) myUnitsCreated[41]++;	// Overlord
			else if(u->getBuildType() == F[42]) myUnitsCreated[42]++;   // Muta 
			else if(u->getBuildType() == F[46]) myUnitsCreated[46]++;	// Scourge	
			else if(u->getBuildType() == F[123]) myUnitsCreated[123]++; // Hatchery
			else if(u->getBuildType() == F[97]) myUnitsCreated[97]++;   // Lurker
		}
	}

	void onUnitComplete(U u) {
		if (u->getPlayer() == X->self()) {
			Q == F[140] ? o(u), o(u) : "a";

			if (O < 24) { // Count the initial 4 drones and 1 overlord
				if (Q == F[40]) myUnitsCreated[40]++; // Drone
				else if(Q == F[41]) myUnitsCreated[41]++;	  // Overlord
			}
		}
	}

	void onUnitDiscover(U u) {
		if (u->getPlayer() == XE) {
			V uType = u gt;
			if (uType == Terran_Siege_Tank_Siege_Mode) uType = Terran_Siege_Tank_Tank_Mode;

			if (!uType.isBuilding()) { 
				hisUnitIDAndInfo[u->getID()].unitType = uType;
				hisUnitIDAndInfo[u->getID()].unitPos = u GP;
				hisUnitIDAndInfo[u->getID()].unitSpd = std::make_pair(u->getVelocityX(), u->getVelocityY());
				hisUnitIDAndInfo[u->getID()].lastFrameVisible = O;
			}
			else if(uType == Protoss_Photon_Cannon ||
				uType == Terran_Bunker || uType == Terran_Missile_Turret ||
				uType == Zerg_Sunken_Colony || uType == Zerg_Spore_Colony)
				hisBuildingPosAndType[u GP] = uType;
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
			else if(uType == Protoss_Photon_Cannon ||
				uType == Terran_Bunker || uType == Terran_Missile_Turret ||
				uType == Zerg_Sunken_Colony || uType == Zerg_Spore_Colony)
			{
				auto it = hisBuildingPosAndType.find(u GP);
				if (it != hisBuildingPosAndType.end())
					hisBuildingPosAndType.erase(it);
			}
		} else if(u->getPlayer() == C) {
			// ...
		}
	}

	/*void onUnitHide(U u) {
		if (u->getPlayer() == XE) {
			V uType = u gt;
			if (uType == Terran_Siege_Tank_Siege_Mode) uType = Terran_Siege_Tank_Tank_Mode;

			if (!uType.isBuilding()) {
				hisUnitIDAndInfo[u->getID()].unitType = uType;
				hisUnitIDAndInfo[u->getID()].unitPos = u GP;
				hisUnitIDAndInfo[u->getID()].unitSpd = std::make_pair(u->getVelocityX(), u->getVelocityY());
			}
		}
	}*/

	void onEnd(bool u) {
		// 1st File to Write
		std::ostringstream mfo;
		int numStats = sizeof(GS) / sizeof(GS[0]);

		for (int ki = 0; ki < numStats; ++ki) {
			if (ki % 2 == 0 &&  ki / 2 + 1 == G || ki % 2 && (ki + 1) / 2 == G && u) GS[ki]++;
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
		for (int i = 1; i < numMyRecentStats; ++i)
			mfo3 << myRecentStats[i] << "\n";

		mfo3 << G * 10 + u << "\n";

		ofstream mf3("bwapi-data/write/" + enemyName + enemyRaceName + "_RECENT" + ".txt", std::ofstream::trunc);
		if (mf3)
		{
			mf3 << mfo3.str();
			mf3.flush();
		}
		mf3.close();
	}
};

