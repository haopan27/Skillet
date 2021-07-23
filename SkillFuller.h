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
#define ER XE->getRace()
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
#define FCA Filter::CanAttack
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
int countMyMorphingUnits(V v) {
	int res = 0;
	for (U u : C->getUnits()) {
		if (Q == Zerg_Egg && u->getBuildType() == v) {
			res++;
		}
		else if (Q == Zerg_Lurker_Egg && v == Zerg_Lurker) {
			res++;
		}
		else if(Q.isBuilding() && !u->isCompleted() && u->getBuildType() == v)
			res++;
	}
	return res;
} 
int CL(int u) { return u == 123 ? CC(123) + countMyMorphingUnits(F[123]) + countMyMorphingUnits(F[124]) + countMyMorphingUnits(F[125]) : CC(u) + countMyMorphingUnits(F[u]); } // count my total units (completed + being morphed)

vector<T>h; // target queue (vector)
map<T, U>enemyBldgTLAndUnit; // target queue (map)
map<U, U>q; // map of worker gather assignments
map<U, int> enemyUnitAndFrameAttackStarted; // map of latest frame when an enemy unit either attacked or repaired or a friendly unit started an attack
map<double, T>distsAndBases;
bool me4or5Pool = false;
bool me1BaseLurkerMuta = false;
bool me7Pool = false;
bool meCOEP = false;
bool me987Hydra = false;
bool meGetMuta = false;
int me3HLing = 0;
bool me9PoolLing = false;
bool meLurkerRush = false;
bool meUltraLing = false;

std::string enemyRaceName = "Unknown";
std::string enemyName;

BWEM::Map & bwemMapInstance = BWEM::Map::Instance();
int myScoutID = -99;
map<T, bool> myScoutTargetsAndStatus;
T hisD = NT;
T closestD = NT;
vector<T> hisPossibleDs;
map<int, int> myUnitsCreated;
bool hisDKilled = false;
bool myAttackCondition = false;
int myAttackStartedSince = 0;
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
int	thisMapIndex = 999;
bool doPathfindingAnalysis = false;

int n, np;	// current used supply, its prevVal (doubles the actual value, due to zerglings occupying 1/2 supply)
int w, wp;	// current max supply, its prevVal
int O;		// frame count
int co = 0;	// guard frame used for morphing overlords
int cs, ck3, ckn;  // guard frame to avoid calling too many scouts, guard frame to avoid calling too many overlords to k3/kn
int G = 0; // build selector
int GS[10] = { 0 }; // build order stats
T D, kn, k3; // my start location, my nat location, my 3rd location
U t; vector<U>m; // nullptr unit, queue of available gather targets (mineral patches and vespene geysers)
void o(U u) { m.insert(m.begin(), u); } // Add resources to gather
void p(T u) { for (U z : GR(S(u), 400, BM))o(z), o(z); } // add all mineral patches close to a position to the queue
void s(U u) { if (q[u])o(q[u]), q.erase(u); } // remove a worker from a gather assignment
void I(U u, T z) { O % 48 == 0 ? ua(S(z)) : "a"; } // attack the given target with the given unit unless it is already doing so
int myBuilderID = 0;
T myBuildLoc = NT;
int myStartingInd = 0;
int hisStartingInd = 0;

int distSq2(const S &pos1, const S &pos2) {
	return(pos1.x - pos2.x) * (pos1.x - pos2.x) + (pos1.y - pos2.y) * (pos1.y - pos2.y);
} // distSq2()

T FindBuildingPlacementAroundAPoint(V buildingType, T searchCenter) {
	int mapWidth = X->mapWidth();
	int mapHeight = X->mapHeight();
	T buildingSize = buildingType.tileSize();
	T mapCenter = T(mapWidth / 2, mapHeight / 2);
	//map<int, T> distAndTL;

	if (buildingType = F[135]) {
		if (!GR(S(kn), 120, BO && BR && B(Completed)).empty()) mapCenter = T(myknCP);
		//else mapCenter = T(myCP);
		else if (myStartingInd != 82 && myStartingInd != 83
			&& myStartingInd != 102
			&& myStartingInd != 112
			&& myStartingInd != 122 && myStartingInd != 123
			&& myStartingInd != 142
			&& myStartingInd != 154) mapCenter = T(myCP); // Exclude a few maps
	}

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

						bool closerToCalibCenter = (searchCenter.x - idx) * (mapCenter.x - idx) < 0 && (searchCenter.y - idy) * (mapCenter.y - idy) < 0;

						if (X->canBuildHere(buildingTopLeft, buildingType) && closerToCalibCenter && X->hasCreep(buildingTopLeft) && X->hasCreep(buildingBottomRight)
							&& X->getUnitsInRectangle(S(buildingTopLeft), S(buildingBottomRight)).empty()
							&& X->isVisible(buildingTopLeft)) {
							/*if (buildingType != F[135]) return buildingTopLeft;
							else distAndTL[distSq2(S(buildingTopLeft) + S(32, 32), S(mapCenter))] = buildingTopLeft;*/
							return buildingTopLeft;
						}
					}
			}
		}
	}

	//if (buildingType == F[135] && !distAndTL.empty()) return distAndTL.begin()->second;

	// Resort to the vanilla finder if everything above fails
	return GB(buildingType, searchCenter, 18);
} // T FindBuildingPlacementAroundAPoint(V buildingType, T searchCenter)




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


void SmartMove(U attacker, S targetPosition)
{
	// Special case: Zerg_Lurker
	if (attacker gt == F[97]) {
		if (attacker->isBurrowed())
			if (GR(attacker GP, 192, BE && !BF).empty()) {
				if (attacker->getLastCommand().getType() != UnitCommandTypes::Unburrow)
					attacker->unburrow();
					
				return;
			}

		if (!attacker->isBurrowed())
			if (!GR(attacker GP, 96, BE && !BF && B(Building)).empty()) {
				if (attacker->getLastCommand().getType() != UnitCommandTypes::Burrow)
					attacker->burrow();
				
				return;
			}
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
	if (myBuildLoc == NT 
		|| !X->getUnitsInRectangle(S(myBuildLoc), S(myBuildLoc + T(F[u].tileWidth(), F[u].tileHeight())), BO && (B(Building) || FGT == F[34])).empty()
		)
		myBuildLoc = (u == 135 || u == 130 || u == 132) ? FindBuildingPlacementAroundAPoint(F[u], b0) : GB(F[u], b0, 18);

	if (myBuildLoc != NT) {
		//X << "myBuildLoc found!" << endl;
		// Find builder
		U br = NULL;

		if (myBuilderID == 0
			|| none_of(C->getUnits().begin(), C->getUnits().end(), [](U u1) { return u1->isCompleted() && u1 gt == F[40] && u1->getID() == myBuilderID; })
			)
		{
			U br = X GC(S(myBuildLoc), BO && BW && (B(Idle) || B(GatheringMinerals)) && !B(CarryingGas) && !B(CarryingMinerals) && !B(GatheringGas), 24 * 32);

			if (!L(br)) br = X GC(S(z), BO && BW && (B(Idle) || B(GatheringMinerals)) && !B(CarryingGas) && !B(CarryingMinerals) && !B(GatheringGas), 24 * 32);

			if (L(br)) myBuilderID = br->getID();
		}
		else {
			for (auto u2 : C->getUnits())
				if (u2->isCompleted() && u2 gt == F[40] && u2->getID() == myBuilderID)
				{
					br = u2; break;
				}
		}

		// Build
		if (L(br)) {
			S myBuildCenter = (S(myBuildLoc) + S(myBuildLoc + T(F[u].tileWidth(), F[u].tileHeight()))) / 2;

			if (distSq2(br GP, myBuildCenter) > 9000) {
				SmartMove(br, myBuildCenter);
			}
			else {
				if (C->minerals() < F[u].mineralPrice() || C->gas() < F[u].gasPrice())
				{
					if (!br->isHoldingPosition()) br->holdPosition();
				}
				else if (br->getLastCommand().getType() != UnitCommandTypes::Build || O - br->getLastCommandFrame() > 120) {
					br->build(F[u], myBuildLoc);
				}
			}

			/*X << "wanted: " << u << ", (x,y): " << myBuildLoc << endl;
 			X->drawBoxMap(S(myBuildLoc), S(myBuildLoc + T(F[u].tileWidth(), F[u].tileHeight())), Colors::Cyan);
			X->drawTextScreen(br GP, "%s\n", F[u].c_str());
			X->drawCircleMap(myCP, 24, Colors::Yellow, true);
			X->drawCircleMap(myknCP, 24, Colors::White, true);*/
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
		if (U hisClosestAttacker = u GC(BE && FCA && !BF && !BW, lurkerSafeBurrowRange))
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
	if (Q == F[36] && ER == R1) {
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
			{
				ug(cm); return;
			}
	}
	else {
		for (T sl : X->getStartLocations()) {
			if (sl == D) continue;

			if (me4or5Pool || me987Hydra && numStartingLocs == 4 || me3HLing && numStartingLocs == 4)
				if (Q != F[41] && sl == closestD || Q == F[41] && sl != closestD) 
					continue; // skip the first target which is to be covered by the overlord.
			
			S slPos = S(sl);

			bool hisDFound = false;

			if (!GR(slPos, 12 * 32, BE && Filter::IsBuilding).empty()) hisDFound = true;

			if (me987Hydra && Q == F[41] && !GR(u GP, 7 * 32, BE && FGT == F[2]).empty()
				&& distSq2(u GP, slPos) < 1296 * 1024) hisDFound = true;

			if (T thisNatPos = FindNatPos(bwemMapInstance, sl)) {
				if (!GR(S(thisNatPos), (numStartingLocs == 4 ? 14 : 9) * 32, BE && Filter::IsBuilding).empty()) hisDFound = true;

				if (me1BaseLurkerMuta)
					if (GR(S(thisNatPos), 9 * 32, BE && FCA && !BW).size() >= 2) hisDFound = true;
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
					{
						ug(cm); return;
					}

				break;
			}

			if (!myScoutTargetsAndStatus[sl]) { // not visited yet
				// 50176 == (7 * 32) ^ 2
				if (distSq2(slPos, u GP) < 50176 && GR(slPos, 224, BE && !BW).empty()) {
					if (hisPossibleDs.size() >= 2)
						hisPossibleDs.erase(remove(hisPossibleDs.begin(), hisPossibleDs.end(), sl), hisPossibleDs.end());

					myScoutTargetsAndStatus[sl] = true; continue;
				}
				else if(distSq2(slPos, u GP) >= 50176) { SmartMove(u, slPos); return; }
			}
		}
	}
}

std::map<std::pair<V, V>, double> unitMatchupTable = unitMatchupTableGen;
bool meSmash() {
	//	lings,	   mutas,	  hydras	 lurkers    ultralisks
	if (!CC(36) && !CC(42) && !CC(37) && !CC(97) && !CC(38)) return false;

	if (hisD == NT) return false;

	if (hisD != NT && hisDKilled) {
		if (me4or5Pool) {
			if (ER == R1) {
				double hisCannons = countHisBuildings(F[150]);
				
				if (hisCannons) {
					double myScore = 0.0;
					for (auto u : C->getUnits()) {
						if (u->isCompleted() && !u gt.isWorker() && !u gt.isBuilding() && u gt.canAttack()) {
							S uPos = u GP;
							bool shouldAdd = false;
							if (any_of(hisBuildingPosAndType.begin(), hisBuildingPosAndType.end(), [&uPos](const auto & u)
							{ return u.second == Protoss_Photon_Cannon && distSq2(u.first, uPos) <= 225 * 1024; })) shouldAdd = true;

							if (shouldAdd) {
								switch (unitTypeToInt(u gt)) {
								case 36: myScore += 0.25; break; // ling
								case 42: myScore += 0.33; break; // muta
								default:;
								}
							}
						}
					}

					if (myScore < 2.0 * hisCannons) return false;
				}
			}
		} // me4or5Pool

		return true;
	} // hisD != NT && hisDKilled

	if (me1BaseLurkerMuta) {
		if (meLurkerRush) return myUnitsCreated[97] >= 2;

		if (meGetMuta && myUnitsCreated[42] >= 3) return true;
		return myUnitsCreated[97] >= 3;
	}

	if (me3HLing) {
		if (me9PoolLing) return myUnitsCreated[36] >= 3;

		// Attack early
		if (me3HLing == 21) if (myUnitsCreated[36] >= 15) return true;

		if (me3HLing == 3) if (myUnitsCreated[36] >= 3) return true;

		return myUnitsCreated[36] >= 20;
	}

	if (meUltraLing) //return myUnitsCreated[38] >= 4; 
		return HU(UT Anabolic_Synthesis);

	if (me987Hydra) return true;

	U myLeader = NULL;
	S hisDPos = S(hisD);
	int bestDist = 99999;

	// Get my army leader
	for (U u : C->getUnits()) {
		if (Q == F[36] || Q == F[42] || Q == F[37] || Q == F[97] || Q == F[38])
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
		Unitset enemiesNearby = GR(myLeaderPos, 400, BE && !BW && FCA && !BF);
		Unitset friendsNearby = GR(myLeaderPos, 400, BO && !BW && FCA);

		double hisScore = 0;
		double myScore = 0;

		for (auto u : enemiesNearby) 
			hisScore += unitMatchupTable[make_pair(F[36], Q)] * (u->getHitPoints() + u->getShields()) / double(Q.maxHitPoints() + Q.maxShields());
		for (auto u : friendsNearby)
			myScore += unitMatchupTable[make_pair(F[36], Q)] * (u->getHitPoints() + u->getShields()) / double(Q.maxHitPoints() + Q.maxShields());

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


int GetStartingInd(TilePosition startingLoc) {
	switch (thisMapIndex) {
	case 0: if (startingLoc == TilePosition(117, 13)) return thisMapIndex * 10 + 1;
			else return thisMapIndex * 10 + 2;
		break; // Benzene
	case 1: if (startingLoc == TilePosition(64, 118)) return thisMapIndex * 10 + 1;
			else return thisMapIndex * 10 + 2;
		break; // Destination
	case 2: if (startingLoc == TilePosition(117, 56)) return thisMapIndex * 10 + 1;
			else return thisMapIndex * 10 + 2;
		break; // HBR
	case 4: if (startingLoc == TilePosition(117, 100)) return thisMapIndex * 10 + 1;
			else if (startingLoc == TilePosition(7, 82)) return thisMapIndex * 10 + 2;
			else return thisMapIndex * 10 + 3;
		break; // Aztec == Neo Aztec (torchup)
	case 5: if (startingLoc == TilePosition(117, 96)) return thisMapIndex * 10 + 1;
			else if (startingLoc == TilePosition(7, 90)) return thisMapIndex * 10 + 2;
			else return thisMapIndex * 10 + 3;
		break; // NMG
	case 6: if (startingLoc == TilePosition(117, 9)) return thisMapIndex * 10 + 1;
			else if (startingLoc == TilePosition(93, 118)) return thisMapIndex * 10 + 2;
			else return thisMapIndex * 10 + 3;
		break; // TC
	case 7: if (startingLoc == TilePosition(117, 7)) return thisMapIndex * 10 + 1;
			else if (startingLoc == TilePosition(117, 119)) return thisMapIndex * 10 + 2;
			else if (startingLoc == TilePosition(7, 118)) return thisMapIndex * 10 + 3;
			else return thisMapIndex * 10 + 4;
		break; // Andromeda
	case 8: if (startingLoc == TilePosition(117, 9)) return thisMapIndex * 10 + 1;
			else if (startingLoc == TilePosition(117, 118)) return thisMapIndex * 10 + 2;
			else if (startingLoc == TilePosition(7, 118)) return thisMapIndex * 10 + 3;
			else return thisMapIndex * 10 + 4;
		break; // Circuit Breaker
	case 9: if (startingLoc == TilePosition(117, 13)) return thisMapIndex * 10 + 1;
			else if (startingLoc == TilePosition(117, 111)) return thisMapIndex * 10 + 2;
			else if (startingLoc == TilePosition(7, 112)) return thisMapIndex * 10 + 3;
			else return thisMapIndex * 10 + 4;
		break; // Eddy
	case 10: if (startingLoc == TilePosition(117, 6)) return thisMapIndex * 10 + 1;
			 else if (startingLoc == TilePosition(117, 119)) return thisMapIndex * 10 + 2;
			 else if (startingLoc == TilePosition(7, 119)) return thisMapIndex * 10 + 3;
			 else return thisMapIndex * 10 + 4;
		break; // Empire of the Sun
	case 11: if (startingLoc == TilePosition(117, 7)) return thisMapIndex * 10 + 1;
			 else if (startingLoc == TilePosition(117, 117)) return thisMapIndex * 10 + 2;
			 else if (startingLoc == TilePosition(7, 116)) return thisMapIndex * 10 + 3;
			 else return thisMapIndex * 10 + 4;
		break; // Fighting Spirit
	case 12: if (startingLoc == TilePosition(116, 47)) return thisMapIndex * 10 + 1;
			 else if (startingLoc == TilePosition(81, 118)) return thisMapIndex * 10 + 2;
			 else if (startingLoc == TilePosition(8, 77)) return thisMapIndex * 10 + 3;
			 else return thisMapIndex * 10 + 4;
		break; // Icarus
	case 13: if (startingLoc == TilePosition(117, 7)) return thisMapIndex * 10 + 1;
			 else if (startingLoc == TilePosition(117, 117)) return thisMapIndex * 10 + 2;
			 else if (startingLoc == TilePosition(8, 117)) return thisMapIndex * 10 + 3;
			 else return thisMapIndex * 10 + 4;
		break; // Jade
	case 14: if (startingLoc == TilePosition(116, 6)) return thisMapIndex * 10 + 1;
			 else if (startingLoc == TilePosition(116, 117)) return thisMapIndex * 10 + 2;
			 else if (startingLoc == TilePosition(8, 117)) return thisMapIndex * 10 + 3;
			 else return thisMapIndex * 10 + 4;
		break; // La Mancha
	case 15: if (startingLoc == TilePosition(83, 6)) return thisMapIndex * 10 + 1;
			 else if (startingLoc == TilePosition(117, 40)) return thisMapIndex * 10 + 2;
			 else if (startingLoc == TilePosition(42, 119)) return thisMapIndex * 10 + 3;
			 else return thisMapIndex * 10 + 4;
		break; // Python
	case 16: if (startingLoc == TilePosition(117, 68)) return thisMapIndex * 10 + 1;
			 else if (startingLoc == TilePosition(57, 118)) return thisMapIndex * 10 + 2;
			 else if (startingLoc == TilePosition(7, 57)) return thisMapIndex * 10 + 3;
			 else if (startingLoc == TilePosition(67, 8)) return thisMapIndex * 10 + 4;
		break; // Roadkill
	case 17: if (startingLoc == TilePosition(117, 35)) return thisMapIndex * 10 + 1;
			 else if (startingLoc == TilePosition(98, 119)) return thisMapIndex * 10 + 2;
			 else if (startingLoc == TilePosition(7, 90)) return thisMapIndex * 10 + 3;
			 else return thisMapIndex * 10 + 4;
		break; // Roadrunner
	case 18: if (startingLoc == TilePosition(116, 8)) return thisMapIndex * 10 + 1;
			 else return thisMapIndex * 10 + 2;
		break; // Blue Storm
	case 19: if (startingLoc == TilePosition(117, 110)) return thisMapIndex * 10 + 1;
			 else if (startingLoc == TilePosition(7, 70)) return thisMapIndex * 10 + 2;
			 else return thisMapIndex * 10 + 3;
		break; // Gold Rush
	case 20: if (startingLoc == TilePosition(117, 29)) return thisMapIndex * 10 + 1;
			 else if (startingLoc == TilePosition(69, 118)) return thisMapIndex * 10 + 2;
			 else return thisMapIndex * 10 + 3;
		break; // Power Bond
	case 21: if (startingLoc == TilePosition(117, 20)) return thisMapIndex * 10 + 1;
			 else if (startingLoc == TilePosition(110, 118)) return thisMapIndex * 10 + 2;
			 else if (startingLoc == TilePosition(7, 106)) return thisMapIndex * 10 + 3;
			 else if (startingLoc == TilePosition(14, 7)) return thisMapIndex * 10 + 4;
		break; // Gladiator
	case 22: if (startingLoc == TilePosition(117, 89)) return thisMapIndex * 10 + 1;
			 else if (startingLoc == TilePosition(34, 118)) return thisMapIndex * 10 + 2;
			 else if (startingLoc == TilePosition(7, 34)) return thisMapIndex * 10 + 3;
			 else if (startingLoc == TilePosition(90, 6)) return thisMapIndex * 10 + 4;
		break; // Sparkle
	default:;
	}
	return 0;
} // int GetStartingInd(...)

struct ExampleAIModule :AIModule {
	void onStart() {
		D = C->getStartLocation();
		X->setCommandOptimizationLevel(1);
		X->enableFlag(Flag::UserInput);

		if (ER == R1) enemyRaceName = "_P";
		else if (ER == R2) enemyRaceName = "_T";
		else if (ER == R3) enemyRaceName = "_Z";

		enemyName = XE->getName();

		const std::vector<std::string> mapHashes = {
					"af618ea3ed8a8926ca7b17619eebcb9126f0d8b1", // Benzene ==> 0
					"4e24f217d2fe4dbfa6799bc57f74d8dc939d425b", // Destination
					"6f8da3c3cc8d08d9cf882700efa049280aedca8c", // Heartbreak Ridge
					"a697fc93c38b098d94ae0ccddaf2eb7aac137b8a", // New Heartbreak Ridge
					"e6d0144e14315118d916905ff5e7045f68db541e", // Aztec == Neo Aztec (torchup)
					"c8386b87051f6773f6b2681b0e8318244aa086a6", // Neo Moon Glaive ==> 5
					"9bfc271360fa5bab3707a29e1326b84d0ff58911", // Tau Cross
					"1e983eb6bcfa02ef7d75bd572cb59ad3aab49285", // Andromeda
					"450a792de0e544b51af5de578061cb8a2f020f32", // Circuit Breaker
					"3078ee93e4a0c3c2ad22c73ab62ef806d9436c3d", // Eddy
					"a220d93efdf05a439b83546a579953c63c863ca7", // Empire of the Sun ==> 10
					"d2f5633cc4bb0fca13cd1250729d5530c82c7451", // Fighting Spirit
					"0409ca0d7fe0c7f4083a70996a8f28f664d2fe37", // Icarus
					"df21ac8f19f805e1e0d4e9aa9484969528195d9f", // Jade
					"e47775e171fe3f67cc2946825f00a6993b5a415e", // La Mancha
					"de2ada75fbc741cfa261ee467bf6416b10f9e301", // Python ==> 15
					"b997dbc7792e7067668ff56a33793a43b35ffdd2", // Roadkill 1.08
					"9a4498a896b28d115129624f1c05322f48188fe0", // Roadrunner
					"aab66dbf9c85f85c47c219277e1e36181fe5f9fc", // Blue Storm
					"666dd28cd3c85223ebc749a481fc281e58221e4a", // Gold Rush
					"731138b5b844a4a0b4a4bb4e495969fd6659414c", // Power Bond ==> 20
					"798bea3acce68788ff1b32d9d777ba7a12c883a1", // Gladiator
					"099930076c0a873b8dc82e8b9646590fd4c887a3"	// Sparkle
		};

		auto thisMapIt = std::find(mapHashes.begin(), mapHashes.end(), X->mapHash());
		if (thisMapIt != mapHashes.end()) // map found!
			thisMapIndex = static_cast<int>(std::distance(mapHashes.begin(), thisMapIt));

		myStartingInd = GetStartingInd(D);

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

		if (enemyName.compare("Aurelien Lermant") == 0
			|| enemyName.compare("Marine Hell") == 0
			|| enemyName.compare("KaonBot") == 0
			|| enemyName.compare("EggBot") == 0
			|| enemyName.compare("Yuanheng Zhu") == 0
			|| enemyName.compare("KangarooBot") == 0 // untested
			|| enemyName.compare("tscmooz") == 0 || enemyName.compare("tscmoor") == 0 && ER == R1
			|| enemyName.compare("Andrew Smith") == 0
			) {
			G = 1; // 4 Pool
		}
		else if (enemyName.compare("Stone") == 0
			|| enemyName.compare("Dave Churchill") == 0 && ER == R3
			|| enemyName.compare("PurpleSwarm") == 0 || enemyName.compare("PurpleDestiny") == 0 && ER == R3
			
			) {
			G = 1;
			me7Pool = true;
		}
		else if (enemyName.compare("WuliBot") == 0
			|| enemyName.compare("Black Crow") == 0
			|| enemyName.compare("KasoBot") == 0
			|| enemyName.compare("Matej Istenik") == 0
			|| enemyName.compare("Ecgberht") == 0
			|| enemyName.compare("Hannes Bredberg") == 0
			|| enemyName.compare("Roman Danielis") == 0
			|| enemyName.compare("PurpleSpirit") == 0 || enemyName.compare("PurpleDestiny") == 0 && ER == R2
			|| enemyName.compare("WillBot") == 0
			|| enemyName.compare("Prism Cactus") == 0 || enemyName.compare("Toothpick Cactus") == 0
			|| enemyName.compare("NiteKatP") == 0 // untested
			|| enemyName.compare("Simon Prins") == 0
			|| enemyName.compare("Junkbot") == 0
			|| enemyName.compare("Microwave") == 0 // untested
			|| enemyName.compare("StyxZ") == 0
			|| enemyName.compare("Dave Churchill") == 0 && ER != R3
			|| enemyName.compare("legacy") == 0 // Smorc through vs carriers
			|| enemyName.compare("MadMixT") == 0 || enemyName.compare("MadMixR") == 0 && ER == R2
			|| enemyName.compare("Slater") == 0
			|| enemyName.compare("NLPRbot") == 0
			|| enemyName.compare("Florian Richoux") == 0
			|| enemyName.compare("Lukas Moravec") == 0
			|| enemyName.compare("Bryan Weber") == 0
			|| enemyName.compare("Pineapple Cactus") == 0
			|| enemyName.compare("MegaBot2017") == 0
			|| enemyName.compare("CUBOT") == 0
			|| enemyName.compare("Marian Devecka") == 0
			|| enemyName.compare("Randomhammer") == 0 && ER == R2
			|| enemyName.compare("Arrakhammer") == 0
			) {
			G = 2;

			if (enemyName.compare("legacy") == 0) {
				myMaxSunks = 6;
				if (ER == R2) lurkerSafeBurrowRange = 256; // Extra range to be safe against stimmed marines
				if (ER == R3) meGetMuta = true;
			}
			else if (enemyName.compare("Marian Devecka") == 0
				|| enemyName.compare("Arrakhammer") == 0
				|| enemyName.compare("CUBOT") == 0) {
				myMaxSunks = 3;
				meGetMuta = true;
			}
			else if (enemyName.compare("Slater") == 0
				|| enemyName.compare("Randomhammer") == 0 && ER == R2
				|| enemyName.compare("MegaBot2017") == 0) {
				myMaxSunks = 4;
			}
			else if (enemyName.compare("Lukas Moravec") == 0) {
				myMaxSunks = 2;
				myMaxSpores = 1;
			}
			else if (enemyName.compare("NLPRbot") == 0) {
				myMaxSunks = 3;
				myMaxSpores = 2;
			}
			else if (enemyName.compare("Dave Churchill") == 0
				|| enemyName.compare("WuliBot") == 0
				|| enemyName.compare("Black Crow") == 0
				|| enemyName.compare("StyxZ") == 0
				|| enemyName.compare("Microwave") == 0
				|| enemyName.compare("Bryan Weber") == 0
				) myMaxSunks = 6;
			else if (enemyName.compare("Junkbot") == 0
				|| enemyName.compare("Roman Danielis") == 0)
				myMaxSpores = 1;
			else if (enemyName.compare("Pineapple Cactus") == 0) {
				myMaxSunks = 2;
				myMaxSpores = 1;
				meGetMuta = true;
			}
		}
		else if (enemyName.compare("Sungguk Cha") == 0) { // Reactive
			G = 1;

			if (myRecentStats[numMyRecentStats - 1] == 21 || myRecentStats[numMyRecentStats - 1] == 10)
			{
				G = 2; myMaxSunks = 2;
			}
		}
		else if (enemyName.compare("Sijia Xu") == 0) { // Reactive
			G = 2; myMaxSunks = 3; myMaxSpores = 1;

			if (myRecentStats[numMyRecentStats - 1] == 20 && myRecentStats[numMyRecentStats - 2] == 20
				|| myRecentStats[numMyRecentStats - 1] == 21) G = 1;
		}
		else if (enemyName.compare("Soeren Klett") == 0
			|| enemyName.compare("ICELab") == 0
			|| enemyName.compare("tscmoop2") == 0
			|| enemyName.compare("krasi0P") == 0
			|| enemyName.compare("Tomas Vajda") == 0
			|| enemyName.compare("Simplicity") == 0
			|| enemyName.compare("GuiBot") == 0
			|| enemyName.compare("ZurZurZur") == 0
			|| enemyName.compare("Bereaver") == 0
			|| enemyName.compare("Zia bot") == 0
			|| enemyName.compare("Tomas Cere") == 0
			|| enemyName.compare("XIAOYICOG2019") == 0 
			|| enemyName.compare("tscmoo") == 0) {
			G = 3;

			if (enemyName.compare("tscmoop2") == 0
				//|| enemyName.compare("Tomas Vajda") == 0
				) me3HLing = 2; // Lings stay at home before moving out
			else if (enemyName.compare("krasi0P") == 0) me3HLing = 21;
			else if (enemyName.compare("GuiBot") == 0) me3HLing = 11;
			else if (enemyName.compare("ZurZurZur") == 0) me3HLing = 11;
			else if (enemyName.compare("XIAOYICOG2019") == 0 
				|| enemyName.compare("tscmoo") == 0
				|| enemyName.compare("Tomas Cere") == 0
				|| enemyName.compare("Bereaver") == 0) me9PoolLing = true;
		}
		else if (enemyName.compare("Dragon") == 0) { // Reactive
			me3HLing = 2; G = 3;

			if (myRecentStats[numMyRecentStats - 1] == 30
				|| myRecentStats[numMyRecentStats - 1] == 11) {
				G = 1; me3HLing = 0;
			}
		}
		else if (enemyName.compare("MadMixZ") == 0 || enemyName.compare("MadMixR") == 0 && ER == R3 || enemyName.compare("Chris Coxe") == 0) { // Reactive
			G = 1; me7Pool = true;
			if (myRecentStats[numMyRecentStats - 1] == 10 || myRecentStats[numMyRecentStats - 1] == 21)
			{
				G = 2;
				meGetMuta = true;
				me7Pool = false;
				myMaxSunks = 2;

				if (enemyName.compare("Chris Coxe") == 0) myMaxSunks = 3;
			}
		}
		else if (enemyName.compare("NiteKatT") == 0 || enemyName.compare("Martin Rooijackers") == 0) {
			G = 2;
			meLurkerRush = true;
			myMaxSunks = 0;
		}
		else if (enemyName.compare("AILien") == 0) { // Reactive
			if (myRecentStats[numMyRecentStats - 1] == 20) {
				G = 3;
				me3HLing = 11;
			}
			else {
				G = 2;
				myMaxSunks = 2;
				myMaxSpores = 1;
				meGetMuta = true;
			}
		}
		else if (enemyName.compare("MadMixP") == 0 || enemyName.compare("MadMixR") == 0 && ER == R1) { // Reactive
			if (myRecentStats[numMyRecentStats - 1] == 10) {
				G = 3;
				me3HLing = 20;
			}
			else {
				G = 1;
			}
		}
		else if (enemyName.compare("TyrProtoss") == 0) {
			G = 4; myMaxSunks = 6;
			if (myRecentStats[numMyRecentStats - 1] == 41) { G = 3; myMaxSunks = 0; }
		}
		else if (enemyName.compare("Proxy") == 0) {
			G = 2; myMaxSunks = 2; meGetMuta = true; myMaxSpores = 1;
			//if (myRecentStats[numMyRecentStats - 1] == 20) { G = 1; me7Pool = true; }
		}
		else G = 5; // enemy name is unknown..
		
		/// vvv fix things here vvv
		//G = 3;
		//myMaxSunks = 6;
		//myMaxSpores = 1;
		//me3HLing = 2;
		//me9PoolLing = true;
		//meGetMuta = true;
		//me987Hydra = true;
		//me7Pool = true;


		//////////////////////////////////////////////////
		if (G == 1) me4or5Pool = true;
		else if (G == 2) me1BaseLurkerMuta = true; 
		else if (G == 3 && me3HLing != 2 && me3HLing != 3) {
			if (me3HLing > 10) {
				myMaxSunks = max(me3HLing / 10 - 1, 0);
				myMaxSpores = me3HLing % 10;
			}
			else {
				me3HLing = 1;
				myMaxSunks = myMaxSpores = 0;
			}
		}
		else if (G == 4) meUltraLing = true;
		else if (G == 5 && C->getRace() == R3) {
			if (!me987Hydra) { meCOEP = true; myMaxSunks = 6; }
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
	} // onStart()

	void onFrame() {
		//float time; { // Timer starts
			//ScopeTimer<float> timer{ time };

		// Initializing...
		O = X->getFrameCount();
		bool myNatBuilt = !GR(S(kn), 160, BO && BR).empty();
		bool my3rdBuilt = !GR(S(k3), 160, BO && BR).empty();
		
		if (hisStartingInd == 0 && hisD != NT) hisStartingInd = GetStartingInd(hisD);

		// >>>>>>>>>> Display START >>>>>>>>>
		//if (O % 48 == 0) X << "me3HLing: " << me3HLing << ", meSmash? " << myAttackCondition << endl;
		// <<<<<<<<<<< Display END <<<<<<<<<<

		// Find the index of an element in `F` (vector of all unit types)
		/*auto it = find(F.begin(), F.end(), Zerg_Lurker_Egg);
		if (it != F.end() && O % 24 == 0) X << distance(F.begin(), it) << endl;*/
		//if (O % 120 == 0) X << F[91] << endl;

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
			if (ER == R1) enemyRaceName = "_P";
			else if (ER == R2) enemyRaceName = "_T";
			else if (ER == R3) enemyRaceName = "_Z";
		}

		// update hisInfo every X seconds, assuming max game time is 60 minutes
		if (O <= 86401) {
			if ((O - 1) % framesPerTick == 0) {
				myAttackCondition = meSmash();

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
					if (!Q.isBuilding()) {
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
		if (hisDKilled && me4or5Pool) {
			for (int i = 0; i < 128; ++i) { // X direction
				for (int j = 0; j < 128; ++j) { // Y direction
					Position posIJ((i + 1) * 32 - 16, (j + 1) * 32 - 16);
					Color colorIJ = Colors::Black;

					if (X->getGroundHeight(TilePosition(posIJ))) colorIJ = Colors::Grey;

					if (ER == R1) { // Protoss units
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
					}

					if (colorIJ != Colors::Black) {
						//  Goon					|| Goon seconds later	   || Cannon
						if (colorIJ == Colors::Blue || colorIJ == Colors::Teal || colorIJ == Colors::Red) gridInfo[j][i] = 1;

						// Visualization
						//X->drawBoxMap(posIJ - Position(16, 16), posIJ + Position(16, 16), colorIJ, false);
						//X->drawLineMap(posIJ - Position(16, 16), posIJ + Position(16, 16), colorIJ);
					}
				} // Y direction
			} // X direction

			if (myAttackStartedSince && O - myAttackStartedSince > 720) myAttackStartedSince = 0;
			if (myAttackStartedSince == 0 && meSmash()) myAttackStartedSince = O;
		} // if (updateGridInfo)

		
		if (doPathfindingAnalysis) {
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
		} // if (doPathfindingAnalysis)

		// COEP Starts
		if (meCOEP) {
			if (myOrders.empty()) {
				vector<V> myFeasibleActions = myFeasibleActionsGen();
				//runCOEP(..., int popSize, int numGenerations, int championSize, double crossOverRate, double mutationRate)
				myOrders = runCOEP(myFeasibleActions, 12, 12, 18, 0.6, 0.6);
			}
			else {
				// Build max 1! ----------- den, mnd, qsn, evc, cav, spr, pool
				// https://www.techiedelight.com/remove-elements-vector-inside-loop-cpp/

				for (int bldgInd : { 127, 128, 130, 131, 132, 133, 134 })
					for (auto it = myOrders.begin(); it != myOrders.end(); it++)
					{
						if (*it._Ptr == F[bldgInd] && CL(bldgInd))
						{
							//X << "erasing bldg: " << bldgInd << endl;
							myOrders.erase(it--);
						}
					}
			}

			if (!myOrders.empty()) { // Extractor
				if (myOrders.front() == F[140])
					if (CL(140) >= CL(123)
						|| CL(140) && GR(S(kn) + S(64, 48), 96, BO && BR).empty()
						|| CL(140) >= 2 && GR(S(k3) + S(64, 48), 96, BO && BR).empty()
						|| CL(140) >= 3)
						myOrders.erase(myOrders.begin());
			}

			if (!myOrders.empty()) {
				if (myOrders.front() == F[41])
					if (n < 14)
						myOrders.erase(myOrders.begin());
				
			}

			if (!myOrders.empty()) { // Avoid getting too many hatcheries without an army
				if (myOrders.front() == F[123])
					if (CL(123) >= 6)
						myOrders.erase(myOrders.begin());
			}

			if (!myOrders.empty()) { // Avoid getting too many sunks
				if (myOrders.front() == F[135])
					if (CL(135) + CL(137) >= myMaxSunks)
						myOrders.erase(myOrders.begin());
			}

			if (!myOrders.empty()) {
				X->drawTextScreen(10, 20, "Next COEP item: %s \n", myOrders.front().c_str());

				if (m_lastUnitType == None) {
					m_lastUnitType = myOrders.front();
					m_orderRequestedSince = O;
				}
			}
		} // COEP Ends

		// Tech switch
		if (!meGetMuta) {
			int earliestTimeToStart = 99999;
			if (me3HLing) earliestTimeToStart = 12960; // 9:00
			else if (me7Pool || me4or5Pool) earliestTimeToStart = 14400; // 10:00
			else if (me1BaseLurkerMuta) earliestTimeToStart = 17280; // 12:00

			if (hisDKilled || O > earliestTimeToStart)
				meGetMuta = true;
		}

		
		bool hasHatNat = !GR(S(kn) + S(64, 48), 96, BO && BR).empty();
		bool hasHat3rd = !GR(S(k3) + S(64, 48), 96, BO && BR).empty();

		if(meCOEP) {
			if (GR(S(D), 256, BO&&BW).size() >= 4) {
				if (!myOrders.empty()) {
					if (myOrders.front() != F[123] && CL(40) >= 8 && CL(123) < 6 && C->minerals() > 350) // Additional Hatcheries
					{
						if (!GR(S(k3), 256, BO && FGT == F[41]).empty() && hasHatNat && !hasHat3rd) BB(123, k3); // 3rd
						else if (CL(123) < 4) {
							if (!GR(S(kn), 256, BO && FGT == F[41]).empty() && !hasHatNat) BB(123, kn); // Nat
							else BB(123); // Near starting main
						}

						goto endOfCOEPOrder;
					}

					if (myOrders.front().isBuilding()) {
						int buildingTypeToBuild = unitTypeToInt(myOrders.front());

						if (buildingTypeToBuild == 123 && CL(123) == 1 && !hasHatNat && C->minerals() > 300) { // Expansion at nat
							if (!GR(S(kn), 256, BO && FGT == F[41]).empty()) BB(buildingTypeToBuild, kn);
							else BB(buildingTypeToBuild);
						}
						else if (buildingTypeToBuild == 123 && CL(123) >= 2 && !hasHat3rd && hasHatNat && C->minerals() > 300) { // Expansion at third
							if (!GR(S(k3), 256, BO && FGT == F[41]).empty()) BB(buildingTypeToBuild, k3);
							else BB(buildingTypeToBuild);
						}
						else if (buildingTypeToBuild == 135 && CC(123) > 1 && hasHatNat) { // Sunken at nat and 3rd
							/*if (hasHat3rd && !GR(S(k3), 256, BO && BR && B(Completed)).empty()
								&& GR(S(k3), 256, BO && BW).size() >= 4 
								&& GR(S(k3), 256, BO && (FGT == F[135] || FGT == F[137] || Filter::BuildType == F[135] || Filter::BuildType == F[137])).size() < 2)
								BB(buildingTypeToBuild, k3, k3);*/
							if (!GR(S(kn), 256, BO && BR && B(Completed)).empty()) BB(buildingTypeToBuild, kn);
						}
						else if (buildingTypeToBuild == 140 && CC(140) == 1 && hasHatNat && C->minerals() > 50
							&& !GR(S(D), 8 * 32, BO && FGT == F[140]).empty()) { // Extractor at nat
							BB(buildingTypeToBuild, kn);
						}
						else if (buildingTypeToBuild == 140 && CC(140) == 2 && hasHatNat && hasHat3rd && C->minerals() > 50
							&& !GR(S(D), 8 * 32, BO && FGT == F[140]).empty()
							&& !GR(S(kn), 8 * 32, BO && FGT == F[140]).empty()) { // Extractor at 3rd (k3)
							BB(buildingTypeToBuild, k3);
						}
						else if (C->minerals() >= F[buildingTypeToBuild].mineralPrice() && C->gas() >= F[buildingTypeToBuild].gasPrice())
							BB(buildingTypeToBuild);

						if (countMyMorphingUnits(buildingTypeToBuild) || myOrders.front() == F[132] // Avoid getting stuck while morphing ultralisk cavern
							|| buildingTypeToBuild == 123 && CL(123) >= 4 && GR(S(k3), 256, BO && FGT == F[41]).empty())
							myOrders.erase(myOrders.begin());
					}

					endOfCOEPOrder:;
				}

				if (!myOrders.empty()) {
					if (myOrders.front() != F[140] && CL(123) >= 2 && CL(123) && CL(140) == 0 && C->minerals() > 100) { // Extractor at main
						BB(140);
					}

					if (myOrders.front() != F[135] && CL(135) + CL(137) < myMaxSunks / 2 && CC(123) > 1 && hasHatNat) { // MORE sunkens at nat
						BB(135, kn);
					}
				}
			}
			else { // no buildings when we very few workers
				if (!myOrders.empty())
					if (myOrders.front().isBuilding())
						myOrders.erase(myOrders.begin());
			}
		} // when on COEP
		else { // Construct my buildings...
			if (CC(140) == 1 && hasHatNat && C->minerals() > 100
				&& GR(S(kn), 8 * 32, BO && FGT == F[140]).empty()
				&& GR(S(kn), 8 * 32, BO && BW).size() >= 5) // Extractor at nat
				BB(140, kn);

			if (me4or5Pool) {
				if (CC(134) == 0 && CL(40) >= 4 && C->minerals() >= 200) BB(134); // Get pool
				if (meGetMuta && CL(140) == 0 && CL(40) > 9 && C->minerals() > 50) BB(140); // Extractor
				if (CC(124) && CL(133) == 0 && C->minerals() > 250 && C->gas() > 150) BB(133); // Get spire
				if (CL(123) < 5 && C->minerals() > 400) {
					if (!GR(S(kn), 256, BO && FGT == F[41]).empty() && !hasHatNat && CL(123) >= 2) BB(123, kn, D);
					else BB(123); // More hatches when possible when me4or5Pool
				}
			}

			if (me7Pool) {
				if (meGetMuta) {
					if (CL(40) >= 7 && CL(140) == 0 && !GR(S(D), 320, B(ResourceContainer) && !BM).empty()) BB(140); // Extractor for tech switch
					if (CL(133) == 0 && CC(124) && C->minerals() > 200 && C->gas() > 150) BB(133); // Spire
				}

				if (CL(134) == 0 && C->minerals() >= 188 && n >= 12) BB(134); // Pool
				if (CL(134) && CL(40) >= 6 && CL(135) + CL(137) < 2 && C->minerals() >= 60) BB(135); // Sunk up
				if ((O > 4800 && CL(123) == 1 || O > 5520 && CL(123) == 2) && CL(134) && C->minerals() > 400) { // Additional hatcheries after 3:20 | 3:50
					if (!GR(S(kn), 256, BO && FGT == F[41]).empty() && !hasHatNat && CL(123) == 2) BB(123, kn, D); // Lay 3rd H at nat
					else BB(123, D); // Macro H
				}
			}

			if (me1BaseLurkerMuta) { // Constructions
				if (meLurkerRush) {
					if (CL(134) == 0 && C->minerals() >= 188 && C->supplyUsed() >= 18) BB(134); // Pool
					if (C->supplyUsed() >= 16 && CL(134) && C->minerals() >= 42 && CL(140) == 0) BB(140); // Extractor
				}
				else {
					if (CL(134) == 0 && C->minerals() >= 188 && C->supplyUsed() >= 24) BB(134); // Pool
					if (CL(134) && C->minerals() >= 42 && CL(140) == 0) BB(140); // Extractor
				}

				if (CL(134) && CL(140) && CL(135) + CL(137) < myMaxSunks && C->minerals() >= 68) BB(135); // Sunk up
				if (!meGetMuta && CL(124) && CL(127) == 0 && C->minerals() >= 100 && C->gas() >= 50) BB(127); // Get den after lair
				if (CL(135) + CL(137) >= myMaxSunks && myAttackCondition
					&& (CL(123) < 2 && C->minerals() > 400 || CL(123) == 2 && C->minerals() > 500)) BB(123); // 2nd/3rd H after den

				if (meGetMuta) {
					if (CL(133) == 0 && CC(124)) BB(133); // Get spire after we completed lair
					if (CL(133) && CL(135) + CL(136) + CL(137) >= myMaxSunks + myMaxSpores && CL(123) < 3 && C->minerals() > 380) BB(123); // Additional H
				}

				if (myMaxSpores) {
					if (O > 6000 // 4:10
						&& CL(131) == 0 && C->minerals() > 100) BB(131); // Evo chamber

					if (CC(131) && CL(135) + CL(137) + CL(136) < myMaxSunks + myMaxSpores) BB(135); // Creep Colony
				}
			}

			if (me3HLing) {
				if (me9PoolLing) {
					if (C->supplyUsed() >= 18 && C->minerals() > 180 && CL(134) == 0) BB(134); // Pool
					if (C->supplyUsed() >= 24 && CL(123) == 1 && C->minerals() > 284) BB(123); // 2nd H
					if (CL(123) >= 2 && CL(140) == 0 && C->minerals() > 32) BB(140); // Extractor
				}
				else {
					if (C->supplyUsed() >= 24 && CL(123) == 1 && C->minerals() > 284) BB(123); // 2nd H
					if (C->supplyUsed() >= 22 && CL(123) == 2 && C->minerals() > 170 && CL(134) == 0) BB(134); // Pool
					if (CL(134) && CL(123) == 2 && C->minerals() > 264) BB(123); // 3rd H
					if (CL(123) >= 3 && CL(140) == 0 && C->minerals() > 30) BB(140); // Extractor
				}

				if (CC(124) && CL(133) == 0 && C->minerals() > 200 && C->gas() > 150) BB(133); // Spire

				if (me3HLing > 10) {
					if (HU(UT Metabolic_Boost) || C->isUpgrading(UT Metabolic_Boost)) {
						if (CL(134) && (CL(135) + CL(137) < myMaxSunks || CL(135) + CL(136) < myMaxSpores) && C->minerals() >= 75) BB(135); // Sunk up
						if (CL(137) >= myMaxSunks && myMaxSpores && CL(131) == 0 && C->minerals() > 75) BB(131); // Evo chamber
					}
				}
			}

			if (meUltraLing) {
				if (CL(123) == 1 && C->minerals() >= 240 && C->supplyUsed() >= 24) BB(123, kn); // 2nd H
				if (CL(123) == 2 && C->minerals() >= 70) BB(123, k3); // 3rd H
				if (CL(123) == 3 && CL(134) == 0 && C->minerals() >= 180) BB(134); // Pool
				if (CL(134) && CL(140) == 0 && C->minerals() > 40) BB(140); // Extractor at main

				if (CL(124) && CL(127) == 0 && C->minerals() > 100 && C->gas() > 50) BB(127); // Den
				if (C->minerals() > 75 && !GR(S(kn) + S(64, 48), 160, BO && BR && B(Completed)).empty())
					if (CL(135) + CL(137) < myMaxSunks / 2 && (HR(TT Lurker_Aspect) || C->isResearching(TT Lurker_Aspect))
						|| CL(135) + CL(137) < myMaxSunks && CL(130))
						BB(135, kn); // Sunks
				if (CL(124) && CL(130) == 0 && C->minerals() > 150 && C->gas() > 100) BB(130); // Queen's nest
				if (CL(140) == 1 && CL(130) && C->minerals() > 50 && hasHatNat) BB(140, kn); // Extractor at nat
				if (CL(140) == 2 && CL(125) && C->minerals() > 50 && hasHatNat && hasHat3rd) BB(140, k3); // Extractor at 3rd
				if (CC(125) && CL(132) == 0 && C->minerals() > 150 && C->gas() > 200) BB(132); // Ultralisk Cavern
				if (CL(132) && CL(123) < 9 && C->minerals() > 300 + 100 * CC(123)) BB(123); // Macro H
			}

			if (me987Hydra) {
				if (CL(134) == 0 && C->minerals() >= 200 && C->supplyUsed() >= 16) BB(134); // Pool
				if (CL(127) == 0 && C->minerals() >= 100 && C->gas() >= 50) BB(127); // Den
				if (CC(40) >= 8 && CL(123) < 2 && CC(140) && C->minerals() > 375) BB(123); // More hatcheries when see fit
				if (CL(134) && CL(140) == 0) BB(140);
			}

			if (O >= 28800 && GR(S(D), 320, BM).empty() && !GR(S(D), 320, BO&&BW).empty() && C->minerals() > 500 && CL(135) + CL(137) < 4) BB(135); // Spend the extra money for base defense after 20m
		}

		// Resetting...
		wp = w; np = n; w = n = 0; map<U, int>es;
		bool myScoutFound = false;
		bool pauseDroneProduction = false;
		if (me4or5Pool && (CL(134) == 0 || CL(40) >= 4 && !meGetMuta || meGetMuta && CL(40) >= 18)
			|| me987Hydra && CL(127) && CL(40) >= 8
			|| me7Pool && CL(40) >= 6
			|| me1BaseLurkerMuta 
			&& (CL(40) >= (meLurkerRush && !meGetMuta ? 10 : 12) || !meLurkerRush && C->supplyUsed() >= 16 && CL(41) == 1 // OV at supply 8
				|| meLurkerRush && (CL(134) && CL(140) == 0 || CL(140) && C->supplyUsed() >= 16 && CL(41) == 1 
					|| CC(140) && C->minerals() < 200 && CL(124) == 0 || CL(124) && CL(127) == 0 && C->minerals() < 150))
			|| me3HLing && !meGetMuta
			&& (CL(40) >= 12 || CL(123) == 2 && CL(134) == 0 
				|| CL(134) && CC(40) >= 10 && CL(40) >= 12
				|| CL(140) && CC(40) >= 11
				|| me9PoolLing && (C->supplyUsed() >= 18 || CL(123) >= 2 || CL(140)))
			|| meUltraLing
			&& ((CL(40) >=  12 && CL(123) == 1 || CL(123) == 2) && C->minerals() < 350
				|| CL(123) == 3 && CL(134) == 0 && C->minerals() < 250
				|| CL(134) && CL(140) == 0 && C->minerals() < 100)
			)
			pauseDroneProduction = true;
		
		bool pauseLingProduction = false;
		if (me3HLing) {
			if (CC(123) >= 3 && C->supplyTotal() - C->supplyUsed() <= 2 && C->minerals() < 150 && countMyMorphingUnits(F[41]) == 0
				|| meGetMuta && (CC(124) && CL(133) == 0 && C->minerals() < 250 && C->gas() > 150
					|| CL(124) == 0 && C->gas() > 60 && C->minerals() < 200)
				|| me9PoolLing && (CL(36) >= 3 && CL(123) < 2 && C->minerals() < 350
					|| CL(123) >= 2 && CL(140) == 0 && C->minerals() < 100
					|| C->gas() > 60 && C->minerals() < 150 && !HU(UT Metabolic_Boost) && !C->isUpgrading(UT Metabolic_Boost)))
				pauseLingProduction = true;
		}
		if (me3HLing > 10) {
			if (HU(UT Metabolic_Boost) || C->isUpgrading(UT Metabolic_Boost))
				if (C->minerals() < 150) // Extra minerals to acoomodate the delay in construction
					if (CL(134) && CL(135) + CL(137) < myMaxSunks
						|| CL(131) && CL(135) + CL(136) < myMaxSpores
						|| CL(137) >= myMaxSunks && myMaxSpores && CL(131) == 0)
						pauseLingProduction = true;
		}
		if (me7Pool)
		{
			if (meGetMuta && (CC(124) && CL(133) == 0 && C->minerals() < 250 || CL(140) == 0 && C->minerals() < 100)
				|| CC(133) && C->gas() >= 100 && C->minerals() < 150) pauseLingProduction = true;
		}

		bool pauseOVProduction = false;
		if (me9PoolLing) {
			if (CL(134) == 0 || C->supplyUsed() < 18) pauseOVProduction = true;
		}
		if (me987Hydra && CL(127) == 0) pauseOVProduction = true;

		// Manage my units' behaviors...
		for (U u : C->getUnits()) {
			if (!u->exists() || !u->isCompleted() || u->isMaelstrommed() || u->isStasised() || u->isLoaded() || u->isStuck()) continue;

			if (Q == Terran_Barracks || u->getBuildType() == Terran_Barracks) X << "RaxTL: " << u->getTilePosition() << endl;
			if (Q == Terran_Bunker || u->getBuildType() == Terran_Bunker) X << "BunkerTL: " << u->getTilePosition() << endl;

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
			} // if this is Terran_Wraith

			// Updating supply info...
			w += Q.supplyProvided(); n += Q.supplyRequired();

			// Closest enemy to this unit within ~ 6 * 32 range
			U Z = u GC(BE, 200); Z && !Z->isMoving() ? enemyUnitAndFrameAttackStarted[Z] = O : w; u->isStartingAttack() ? enemyUnitAndFrameAttackStarted[u] = O : w;

			// Upgrade/Research/Morph...
			if (Q.isBuilding()) {
				if (!me987Hydra && !me3HLing) 
					P == D ? (!CL(124) ? ut(F[124]) : (!CL(125) ? ut(F[125]) : "a")) : "a"; // Main Hatch->Lair->Hive

				if (CC(125) && CC(134)) up(UT Adrenal_Glands); // Crackling
				if (CC(140) && CC(134) && !me987Hydra && !me1BaseLurkerMuta && !meUltraLing) up(UT Metabolic_Boost); // Ling speed
				
				if (me7Pool && CC(127)) { up(UT Grooved_Spines); up(UT Muscular_Augments); } // Hydra range and speed upgrades
				if (me1BaseLurkerMuta && CC(124) && CC(127) && !meGetMuta) ur(TT Lurker_Aspect); // Lurker_Aspect
				if (me3HLing && meGetMuta && P == D && CL(124) == 0) ut(F[124]); // Lair
				if (meUltraLing) {
					ur(TT Lurker_Aspect);
					if (CL(125)) up(UT Metabolic_Boost); // Ling speed
					up(UT Chitinous_Plating);
					if (HU(UT Chitinous_Plating)) up(UT Anabolic_Synthesis);
				}
				if (me987Hydra && CC(127) && C->minerals() > 225 && C->gas() > 175) { 
					up(UT Grooved_Spines);
					if (HU(UT Grooved_Spines)) up(UT Muscular_Augments);
				} // Hydra upgrades
				if (meCOEP && CC(127)) { ur(TT Lurker_Aspect); up(UT Grooved_Spines); up(UT Muscular_Augments); }

				if (myMaxSpores) {
					if (CC(131) && CL(136) < myMaxSpores) ut(F[136]); // Creep->Spore
					if (CL(136) < myMaxSpores && CL(137) >= myMaxSunks) continue; // Prevent excessive sunkens
				}
				ut(F[137]); // Creep->Sunken
			}

			// Manage larvae (unit production)
			else if(Q == F[34]) {
				if (meCOEP) {
					if (!myOrders.empty()) {
						if (C->supplyTotal() - C->supplyUsed()) {
							if (myOrders.front() != F[40] && CC(123) >= 2 && CL(40) < 24 && countMyMorphingUnits(F[40]) < 2)
								ut(F[40]); // Drone up

							if (myOrders.front() == F[42] && CC(123) >= 3 && CC(34) >= 3
								&& C->gas() < 50 && C->minerals() > 100 + 50 * CC(123) && HU(UT Metabolic_Boost))
								ut(F[36]); // Mix in lings
						}

						if (!myOrders.front().isBuilding()) {
							int unitTypeToMorph = unitTypeToInt(myOrders.front());

							if (C->supplyTotal() - C->supplyUsed() >= myOrders.front().supplyRequired()) {
								ut(F[unitTypeToMorph]);

								if (countMyMorphingUnits(myOrders.front()))
									myOrders.erase(myOrders.begin());
							}
							else {
								if (O - co > 630) { // Get overlords
									co = O;
									ut(F[41]);
								}
							}
						}
					}
				}
				else { // Manage larvae when NOT on COEP..
					if (CC(125) && !HU(UT Adrenal_Glands) && !C->isUpgrading(UT Adrenal_Glands) // Prioritize crackling upgrade when we have Hive
						|| me1BaseLurkerMuta && !meGetMuta && (CC(124) && CC(127) && C->minerals() < 450
							&& !HR(TT Lurker_Aspect) && !C->isResearching(TT Lurker_Aspect)) // Prioritize Lurker_Aspect
						|| me7Pool && n >= 12 && !CL(134) // Prioritize pool
						) continue; // Cut production in favor of upgrades/researches

					if (me987Hydra) {
						if (C->supplyUsed() >= 14 && CL(134) && CL(140) == 0) continue; // Yield prio to extractor
						if (C->supplyUsed() < 18 && CL(40) < 9 && CL(134) && CL(140) && CL(127) == 0) ut(F[40]); // Drone up to 9 before hydra den
						if (C->supplyUsed() >= 16 && CL(134) == 0) continue; // Yield prio to pool
					}

					// Spawn more overlords
					if (wp < 400 && C->minerals() >= 100) {
						if (me1BaseLurkerMuta) {
							if (C->supplyTotal() <= 18) {
								if (C->supplyUsed() >= 16 && O - co > 660) {
									if (!meLurkerRush || CL(124)) {
										ut(F[41]);
										co = O;
									}
								}
							}

							if (meGetMuta) {
								if (C->supplyTotal() > 18 && C->supplyTotal() < 70) {
									if (C->supplyUsed() >= 24 && myUnitsCreated[41] < 3
										|| C->supplyUsed() >= 42 && myUnitsCreated[41] < 4
										|| C->supplyUsed() >= 58 && myUnitsCreated[41] < 5
										) {
										if (CL(123) >= 3 && O - co > 660) {
											ut(F[41]);
											co = O;
										}
									}
								}
							}
						}

						if (meUltraLing) {
							if (CL(132)) { // When we have ultralisk cavern..
								if (C->supplyTotal() - C->supplyUsed() < 12 && O - co > 330) {
									ut(F[41]);
									co = O;
								}
							}
						}

						if (me3HLing && C->supplyTotal() > 18 && C->supplyTotal() < 86 && !me9PoolLing) {
							if (C->supplyUsed() >= 28 && myUnitsCreated[41] < 3
								|| C->supplyUsed() >= 42 && myUnitsCreated[41] < 4
								|| C->supplyUsed() >= 58 && myUnitsCreated[41] < 5
								|| C->supplyUsed() >= 72 && myUnitsCreated[41] < 6
								|| C->supplyUsed() >= C->supplyTotal()) {
								if (CL(123) >= 3 && O - co > 660) {
									ut(F[41]);
										co = O;
								}
							}
						}
						else {
							int techBldgInd = 127;
							if (me4or5Pool) techBldgInd = 133;
							else if (me3HLing) techBldgInd = me9PoolLing? 134 : 123;

							if (!pauseOVProduction) {
								if (CC(techBldgInd) && 1.0*np / wp > 0.8)
									O - co > 660 ? co = O, ut(F[41]) : "a";
								else if (np > 17 && wp - np < 4)O - co > 660 ? co = O, ut(F[41]) : "a";
							}
						}

						// when we're supply blocked..
						if (C->supplyUsed() > C->supplyTotal() && O - co > 660 && C->minerals() > 100) {
							co = O; ut(F[41]);
						}
					}

					// loop through all my bases and morph the larvae in the vicinity to drones...
					for (S ip : {S(D), S(kn)})
						if (ud(ip) < 128 && (int)GR(ip, 320, BO&&BW).size() < 18 
							&& CL(40) < (myNatBuilt ? 36 : (me987Hydra ? 12 : 18))) {
							if (!pauseDroneProduction)
								ut(F[40]); // Drones

							if (meGetMuta && CL(40) < 12) ut(F[40]); // Drones
						}

					if (me4or5Pool) {
						ut(F[42]); // Mutas
						if (CC(133) && (CC(36) < 4 * CC(42) || C->gas() < 100) 
							|| !CC(133)) // Not having a spire..
							ut(F[36]); // Zerglings
					}
					else if (me7Pool) {
						if (meGetMuta) {
							ut(F[42]);
							if (!pauseLingProduction) ut(F[36]);
						}
						else ut(F[36]);
					}
					else if(me1BaseLurkerMuta) { // Unit production..
						if (CC(133)) ut(F[42]); // Mutas
						if ((HR(TT Lurker_Aspect) || C->isResearching(TT Lurker_Aspect)) && C->gas() > 25 && CL(37) < 3 * CC(140)) ut(F[37]); // Hydras
						if (CL(127) + CL(133) 
							&& countMyMorphingUnits(Zerg_Hydralisk) + countMyMorphingUnits(Zerg_Mutalisk) + countMyMorphingUnits(Zerg_Lurker)
							&& (meGetMuta || CL(34) >= 3)
							&& CL(36) < 3 * CC(123) && C->gas() < 50 && C->minerals() > 150) ut(F[36]); // Zerglings (yielding priority to hydras)
					}
					else if (me3HLing) {
						if (C->gas() >= 100 && CC(133)) ut(F[42]); // muta
						if (CL(123) >= 3 && !pauseLingProduction) {
							if (meGetMuta) {
								if (CL(124) == 0 && C->gas() > 50 && C->minerals() > 200
									|| CL(124) && C->minerals() > 200
									|| CC(124) && CL(133) == 0 && C->gas() > 100 && C->minerals() > 250
									|| CL(133) && C->minerals() > C->gas()
									|| CC(133) && C->gas() > 50 && C->minerals() > 150)
									ut(F[36]);
							}
							else if (C->minerals() > 150 && HU(UT Metabolic_Boost) == 0 && !C->isUpgrading(UT Metabolic_Boost)
								|| HU(UT Metabolic_Boost) || C->isUpgrading(UT Metabolic_Boost))
								ut(F[36]);
						}
						if (me9PoolLing) {
							if (!pauseLingProduction) ut(F[36]);
						}
					}
					else if (meUltraLing) {
						if (CC(132)) {
							ut(F[38]);
							if (C->gas() < 76) ut(F[36]);
						}
						else {
							if (CL(37) + CL(91) + CL(97) < 6) ut(F[37]);
						}
					}
					else if(me987Hydra) {
						if (C->gas() > 25 && CC(127)) ut(F[37]); // Mass hydras
						if (C->gas() < 10 && CC(134) && myUnitsCreated[37] >= 9) ut(F[36]); // Mix in lings when we don't have resources
					}
				}
				continue;
			} // else if(Q == F[34]) // larvae...

			// Manage hydras...
			else if(Q == F[37]) {
				if (HR(TT Lurker_Aspect)) // when there are avilable hydras to morph into lurkers..
					if (me1BaseLurkerMuta && CC(97) < 2 * CC(37)
						|| meCOEP && CC(97) < 6
						|| meUltraLing)
						ut(F[97]); // into lurkers!
			}

			// Manage drones...
			else if((Q == F[40] || Q == F[13]) && (myBuilderID == 0 || u->getID() != myBuilderID)) { // Drone or SCV
				// Go scouting when my candidate is found
				if (hisD == NT && u->getID() == myScoutID) { myScoutFound = true; GoScouting(u); continue; }

				// Set up scouting params when we have pool..
				if (hisD == NT && CL(134) && CL(40) >= 4 && O - cs > 240 && !u->isCarryingMinerals() && !u->isCarryingGas()) {
					bool timeToScout = false;
					if (O < 4320) { // 3:00
						if (me987Hydra && numStartingLocs == 4 && CL(127)) timeToScout = true;
						if (me1BaseLurkerMuta && O < 3600) timeToScout = true; // 2:30
						if (me4or5Pool) timeToScout = true;
						if (me3HLing && CL(123) + CL(134) >= 2 && numStartingLocs == 4) timeToScout = true;
					}

					if (timeToScout) { // When it is time to scout...
						if (myScoutID < 0) { cs = O; myScoutID = u->getID(); myScoutFound = true; GoScouting(u); continue; }
					}
				}

				if (Z && (O - enemyUnitAndFrameAttackStarted[Z]) < 99 && !Z->isFlying()) {
					bool shouldChase = true;
					int hisNearbyWorkers = GR(S(D), 15 * 32, BE && BW).size();
					int hisNearbyAttackers = GR(S(D), 15 * 32, BE && !BW && FCA).size();
					int hisNearbyLings = GR(S(D), 15 * 32, BE && FGT == F[36]).size();
					if (hisNearbyWorkers <= 1 && hisNearbyAttackers == 0
						|| distSq2(Z GP, S(D)) > 225 * 1024
						|| (numStartingLocs < 4 || thisMapIndex == 15) && hisNearbyWorkers >= 2 && hisNearbyAttackers == 0
						|| hisNearbyWorkers == 0 && hisNearbyLings <= 1 && hisNearbyAttackers <= 1
						)
						shouldChase = false;

					// Avoid over-reacting to scouting workers
					if (!shouldChase) {
						if (!Z->isAttacking() && Z gt.isWorker())
							if (u->getLastCommand().getType() == UnitCommandTypes::Attack_Unit && u->getLastCommand().getTarget() == Z)
								if (U cm = X GC(S(D), BM, 7 * 32))
								{
									ug(cm);
									continue;
								}

						if (distSq2(u GP, S(D)) < 225 * 1024)
							if (Z gt.isWorker() && distSq2(Z GP, u GP) <= 1024)
								if (u->getLastCommand().getType() != UnitCommandTypes::Attack_Unit || u->getLastCommand().getTarget() != Z)
								{
									ua(Z);
									continue;
								}
					}

					if (shouldChase) {
						s(u), K != Z ? ua(Z) : "a"; // worker defense
						continue;
					}
				}
				else if ((me987Hydra || me1BaseLurkerMuta || me3HLing) && CC(140) == 1) { // Force drones to mine gas ASAP on 1st extractor completion
					int myGasGatherers = count_if(q.begin(), q.end(), [](std::pair<U, U> u){ return L(u.second) && u.second->getType() == F[140]; });
					
					if (myGasGatherers < 3)
						if (CL(127) == 0 && !me3HLing || me3HLing && (!HU(UT Metabolic_Boost) && !C->isUpgrading(UT Metabolic_Boost) || meGetMuta))
							if (!u->isGatheringGas() && !u->isCarryingMinerals())
								if (U myClosestExtractor = u GC(BO && FGT == F[140]))
									if (u->getLastCommand().getType() != UnitCommandTypes::Right_Click_Unit
										|| u->getLastCommand().getTarget() != myClosestExtractor) {
										if (!q.empty() && q[u] != myClosestExtractor) q[u] = myClosestExtractor;
										u->rightClick(myClosestExtractor);
										continue;
									}

					if (me987Hydra && CL(127) // den
						|| me1BaseLurkerMuta && CL(134) // pool
						|| me3HLing && (HU(UT Metabolic_Boost) || C->isUpgrading(UT Metabolic_Boost))) { // dispatch drones between mining and extracting
						if (O % 168 == 0) // Do this every X seconds
							if (u->isIdle() && !u->isMoving())
								if (U cm = X GC(S(D), BM, 7 * 32))
								{
									ug(cm);
									continue;
								}

						if (me1BaseLurkerMuta) {
							if (CL(40) > 7 && myGasGatherers < 3)
								if (!u->isGatheringGas())
									if (U myClosestExtractor = u GC(BO && FGT == F[140])) {
										if (!q.empty() && q[u] != myClosestExtractor) q[u] = myClosestExtractor;
										ug(myClosestExtractor);
										continue;
									}
						}

						if (me987Hydra && myGasGatherers > 2 || 
							me3HLing && !meGetMuta)
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

			// Manage overlords...
			else if(Q == F[41]) {
				if (U hisFlyingAttacker = u GC(BE && BF && FCA && Filter::AirWeapon != NW, 9 * 32))
					if (CL(136)) // Spore
						if (U myClosestSpore = u GC(BO && FGT == F[136]))
						{
							SmartMove(u, myClosestSpore GP); 
							continue;
						}

				if (me1BaseLurkerMuta || meCOEP || meUltraLing) {
					if (u->isUnderAttack()) { SmartMove(u, S(D)); continue; }

					if (meUltraLing) {
						if (GR(S(kn), 100, BO && FGT == F[41]).empty()) {
							if (U myClosestOV = X GC(S(kn), BO && FGT == F[41]))
								if (u == myClosestOV)
								{
									um(S(kn)); continue;
								}
						}
						else if (distSq2(u GP, S(kn)) > 10000) {
							if (distSq2(u GP, S(D)) > 3600) { um(S(D)); continue; }
							else { if (!u->isHoldingPosition()) u->holdPosition(); continue; }
						}
					}

					if (meCOEP) {
						if (GR(S(kn), 250, BO && FGT == F[41]).empty() && u->getLoadedUnits().empty())
							if (U myClosestOV = X GC(S(kn), BO && FGT == F[41]))
								if (u == myClosestOV)
								{
									um(S(kn)); continue;
								}

						if (CL(123) >= 2) {
							if (GR(S(k3), 250, BO && FGT == F[41]).empty() && u->getLoadedUnits().empty() && O - ck3 > 620)
								if (U myClosestOV = X GC(S(k3), BO && FGT == F[41]))
									if (u == myClosestOV)
									{
										ck3 = O;
										um(S(k3));
										continue;
									}
						}
					}
				} else  {
					if (hisD == NT && myUnitsCreated[41] < 2) { GoScouting(u); continue; }
					if (u->isUnderAttack() || hisD != NT && !hisDKilled) { SmartMove(u, S(D)); continue; }
					if (hisD != NT && hisDKilled && G == 1) {
						if (GR(S(kn), 250, BO && BR).empty() && u->getLoadedUnits().empty())
							if (U myClosestOV = X GC(S(kn), BO && FGT == F[41]))
								if (myClosestOV == u)
									um(S(kn));
					}
				}
			} // Manage overlords

			// Manage my army: 
			//	lings		  mutas 		hydras		  lurkers		Ultralisks
			if (Q == F[36] || Q == F[42] || Q == F[37] || Q == F[97] || Q == F[38]) {
				if (me1BaseLurkerMuta) {
					if (Q == F[97]) { // Lurkers cautious burrow
						if (LurkerCautiousBurrow(u)) continue;
					} else if(Q == F[37]) { // hydras
						// Close in on his nat when doing `meLurkerRush`
						if (meLurkerRush && !myAttackCondition && hisD != NT && !hisDKilled) {
							if (S hisNatTL = S(FindNatPos(bwemMapInstance, hisD)))
								if (!u->isUnderAttack())
								{
									S hisNatCenter = hisNatTL + S(64, 48);
									if (distSq2(u GP, hisNatCenter) > 625 * 1024) {
										SmartMove(u, hisNatCenter);
										continue;
									}
									else {
										if (!u->isHoldingPosition()) u->holdPosition();
										continue;
									}
								}
						}

						if (myUnitsCreated[97] < 10 // Hydras stay with lurkers early
							&& (hisD == NT || !hisDKilled && distSq2(u GP, S(hisD)) > 1024 * 1024))  
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
				} // me1BaseLurkerMuta

				if (meUltraLing) {
					if (Q == F[97]) { // Lurkers go defensive
						if (u GC(BO && FGT == F[137], 64)) {
							if (!u->isBurrowed() && u->getLastCommand().getType() != UnitCommandTypes::Burrow) u->burrow();
							continue;
						}
					}

					if (Q == F[36] || Q == F[38]) // Ling or Ultralisk defensive actions
					{
						if (!myAttackCondition) {
							if (U myClosestSunk = u GC(BO && FGT == F[137])) {
								if (U hisAttacker = myClosestSunk GC(BE && FCA && !BF, 96)) {
									SmartAttack(u, hisAttacker);
									continue;
								}
							}
						}
					}
				}

				if (Q == F[36]) { // Zergling counter attack
					if (U hisClosestAttacker = u GC(BE && FCA && !BF, 32)) {
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

							//if (GR(S(D), 15 * 32, BE && FGT == F[36]).size() <= 1)
								if (U hisLing = X GC(S(D), BE && FGT == F[36], 15 * 32))
									if (U myLing = hisLing GC(BO && FGT == F[36]))
										if (u == myLing) {
											if (u->getLastCommand().getType() != UnitCommandTypes::Attack_Unit || u->getLastCommand().getTarget() != hisLing)
												ua(hisLing);
											continue;
										}

							// Get to the staging area (close to his nat)
							if (hisD != NT && !hisDKilled && me3HLing != 2 && me3HLing < 10)
								if (S hisNatTL = S(FindNatPos(bwemMapInstance, hisD)))
									if (!u->isUnderAttack())
									{
										S hisNatCenter = hisNatTL + S(64, 48);
										if (distSq2(u GP, hisNatCenter) > 625 * 1024) {
											SmartMove(u, hisNatCenter);
											continue;
										}
										else {
											if (!u->isHoldingPosition()) u->holdPosition();
											continue;
										}
									}
						}
						else {
							if (ER == R2)
								if (U hisBunker = u GC(BE && FGT == F[117], 5 * 32))
								{
									if (u->getLastCommand().getType() != UnitCommandTypes::Attack_Unit || u->getLastCommand().getTarget() != hisBunker)
										ua(hisBunker);
									continue;
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
					// Smart destroy defensive buildings
					if (me4or5Pool && ER == R1 && (!myAttackCondition || O - myAttackStartedSince > 720)) {
						S hisPos = NP;
						int minDist2 = 99999999;

						for (auto u : hisBuildingPosAndType)
							if (u.second == F[150]) {
								int iDist2 = distSq2(u.first, S(D));
								if (iDist2 < minDist2) {
									hisPos = u.first;
									minDist2 = iDist2;
								}
							}

						if (hisPos != NP) {
							S uPos = u GP;
							int u2HisPosDist = distSq2(uPos, hisPos);

							if (u2HisPosDist > 196 * 1024) {
								SmartMove(u, hisPos);
								continue;
							}
							else if (u2HisPosDist < 121 * 1024) {
								S retreatPos = S(D);
								double deltaX = uPos.x - hisPos.x;
								double deltaY = uPos.y - hisPos.y;
								double uAngle = atan2(deltaY, deltaX);

								int desiredPosX = uPos.x + static_cast<int>(96.0 * cos(uAngle));
								int desiredPosY = uPos.y + static_cast<int>(96.0 * sin(uAngle));

								S desiredPos = NP;
								if (desiredPosX && desiredPosX < X->mapWidth() * 32 && desiredPosY && desiredPosY < X->mapHeight() * 32) {
									desiredPos = S(desiredPosX, desiredPosY);
									WalkPosition desiredWalkPos = WalkPosition(desiredPos);
									if (X->isWalkable(desiredWalkPos))
										retreatPos = desiredPos;
								}

								//X->drawLineMap(uPos, retreatPos, Colors::Red);
								//X->drawCircleMap(retreatPos, 8, Colors::Orange, true);

								SmartMove(u, retreatPos);
								continue;
							}
							else {
								if (!u->isHoldingPosition()) u->holdPosition();
								continue;
							}
						}
					}

					if (U Z2 = FindTarget(u)) {
						if (L(Z2)) {
							if (Q == F[97])
							{
								int thisRange = GetAttackRange(Q, Z2 gt);
								distSq2(u GP, Z2 GP) > thisRange * thisRange ? SmartMove(u, Z2 GP) : SmartAttack(u, Z2);
							}
							else SmartAttack(u, Z2);
							continue;
						}
					}
					else if(U Z3 = u GC(BE&&B(Building))) {
						if (L(Z3)) {
							int thisRange = GetAttackRange(Q, Z3 gt);
							distSq2(u GP, Z3 GP) > thisRange * thisRange ? SmartMove(u, Z3 GP) : SmartAttack(u, Z3);
							continue; 
						}
					}
					else if(!u->isMoving()) {
						if (Q.isFlyer()) {
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
								if (Q.groundWeapon() != NW && Q.airWeapon() == NW) {
									if (U nearestThreat = u GC(BE && FCA && !B(Flying), 3 * Q.sightRange() / 2))
										ZZ = nearestThreat;
								} else if (Q.groundWeapon() == NW && Q.airWeapon() != NW) {
									if (U nearestThreat = u GC(BE && FCA && B(Flying), 3 * Q.sightRange() / 2))
										ZZ = nearestThreat;
								} else if (Q.groundWeapon() != NW && Q.airWeapon() != NW) {
									if (U nearestThreat = u GC(BE && FCA, 3 * Q.sightRange() / 2))
										ZZ = nearestThreat;
								}
							}

							if (me987Hydra) // Ignore enemy scouting worker
								if (ZZ gt.isWorker() && distSq2(ZZ GP, S(D)) < 400 * 1024)
									if (GR(S(D), 20 * 32, BE && FCA).size() <= 2)
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
									int thisRange = GetAttackRange(Q, ZZ gt);
									distSq2(u GP, ZZ GP) > thisRange * thisRange ? SmartMove(u, ZZ GP) : SmartAttack(u, ZZ);
								} else SmartAttack(u, ZZ);
							}
							else SmartMove(u, S(D));
							continue;
						}
					}

					if (hisD != NT && !u->isUnderAttack()) {
						// Wait group (hydra)
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

						// Wait group (ling)
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

						if (distSq2(u GP, S(hisD) + S(64, 48)) <= Q.sightRange() * Q.sightRange() * 1024)
						{
							U hisBldg = NULL;
							if (Q.groundWeapon() != NW && Q.airWeapon() == NW) {
								if (U nearestBldg = u GC(BE && B(Building) && !B(Flying), Q.sightRange()))
									hisBldg = nearestBldg;
							} else if(Q.groundWeapon() == NW && Q.airWeapon() != NW) {
								if (U nearestBldg = u GC(BE && B(Building) && B(Flying), Q.sightRange()))
									hisBldg = nearestBldg;
							} else if(Q.groundWeapon() != NW && Q.airWeapon() != NW) {
								if (U nearestBldg = u GC(BE && B(Building), Q.sightRange()))
									hisBldg = nearestBldg;
							}

							if (hisBldg && hisBldg != NULL) { SmartAttack(u, hisBldg); continue; }
						}
					}

					// Save starting main in danger..
					if (U hisClosestAttacker = X GC(S(D), BE && FCA && !Filter::IsWorker && !B(Invincible), 320)) {
						if (hisClosestAttacker->isFlying() && u->getType().airWeapon() != NW || !hisClosestAttacker->isFlying() && u->getType().groundWeapon() != NW)
						{
							SmartMove(u, S(D)); 
							continue;
						}
					}

					(myAttackCondition || np > nToRetreat) ? SmartMove(u, S(hisD)), (nToRetreat == 999 ? (nToRetreat = CC(40) + max(np - 2 * CC(40), 0) / 6) : 0) :
						(myNatBuilt ? (GR(S(kn), 9 * 32, BO && FGT == F[137]).empty() ? SmartMove(u, S(kn)) : SmartMove(u, u GC(BO && FGT == F[137]) GP)) : SmartMove(u, S(D)), nToRetreat == 999 ? 0 : nToRetreat = 999);
				}
			} // Manage my army units

			// Manage scourges (avoid overkill)
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
			else if (u->getBuildType() == F[41]) myUnitsCreated[41]++;	// Overlord
			else if (u->getBuildType() == F[42]) myUnitsCreated[42]++;   // Muta 
			else if (u->getBuildType() == F[46]) myUnitsCreated[46]++;	// Scourge	
			else if (u->getBuildType() == F[123]) myUnitsCreated[123]++; // Hatchery
			else if (u->getBuildType() == F[97]) myUnitsCreated[97]++;   // Lurker
			else if (u->getBuildType() == F[38]) myUnitsCreated[38]++;   // Ultra
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
			V uType = Q;
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
			V uType = Q;
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
			V uType = Q;
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

