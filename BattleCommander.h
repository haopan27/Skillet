// COEP stuff
#include<BWAPI.h>
#include <random>

using namespace std;
using namespace BWAPI;
using namespace BWAPI::UnitTypes;

inline map<Position, UnitType> hisBuildingPosAndType = {};
struct UnitInfo {
	UnitType unitType;
	Position unitPos;
	std::pair<double, double> unitSpd;
	int lastFrameVisible;
};
inline map<int, UnitInfo> hisUnitIDAndInfo = {};
inline std::string hisUnitCompPrev = "-";
inline std::string hisUnitCompNext = "-";

extern const std::map<std::pair<UnitType, UnitType>, double> unitMatchupTableGen;
decltype(unitMatchupTableGen) unitMatchupTableGen{ [] {
std::map<std::pair<UnitType, UnitType>, double> ret;
ret[std::make_pair(Zerg_Zergling, Protoss_Zealot)] = 4;
ret[std::make_pair(Zerg_Zergling, Protoss_Dragoon)] = 3;
ret[std::make_pair(Zerg_Zergling, Protoss_Dark_Templar)] = 5;
ret[std::make_pair(Zerg_Zergling, Protoss_Archon)] = 6;
ret[std::make_pair(Zerg_Zergling, Protoss_Reaver)] = 4;
ret[std::make_pair(Zerg_Zergling, Protoss_Scout)] = 99;
ret[std::make_pair(Zerg_Zergling, Protoss_Carrier)] = 99;
ret[std::make_pair(Zerg_Zergling, Protoss_Corsair)] = 99;
ret[std::make_pair(Zerg_Zergling, Protoss_Photon_Cannon)] = 4;
ret[std::make_pair(Zerg_Zergling, Terran_Marine)] = 0.75;
ret[std::make_pair(Zerg_Zergling, Terran_Firebat)] = 2;
ret[std::make_pair(Zerg_Zergling, Terran_Medic)] = 2.6;
ret[std::make_pair(Zerg_Zergling, Terran_Ghost)] = 0.75;
ret[std::make_pair(Zerg_Zergling, Terran_Vulture)] = 8;
ret[std::make_pair(Zerg_Zergling, Terran_Siege_Tank_Tank_Mode)] = 5;
ret[std::make_pair(Zerg_Zergling, Terran_Siege_Tank_Siege_Mode)] = 5;
ret[std::make_pair(Zerg_Zergling, Terran_Goliath)] = 3;
ret[std::make_pair(Zerg_Zergling, Terran_Wraith)] = 99;
ret[std::make_pair(Zerg_Zergling, Terran_Valkyrie)] = 99;
ret[std::make_pair(Zerg_Zergling, Terran_Battlecruiser)] = 99;
ret[std::make_pair(Zerg_Zergling, Terran_Bunker)] = 16;
ret[std::make_pair(Zerg_Zergling, Zerg_Zergling)] = 1;
ret[std::make_pair(Zerg_Zergling, Zerg_Hydralisk)] = 2;
ret[std::make_pair(Zerg_Zergling, Zerg_Lurker)] = 6;
ret[std::make_pair(Zerg_Zergling, Zerg_Ultralisk)] = 11;
ret[std::make_pair(Zerg_Zergling, Zerg_Mutalisk)] = 99;
ret[std::make_pair(Zerg_Zergling, Zerg_Scourge)] = 99;
ret[std::make_pair(Zerg_Zergling, Zerg_Guardian)] = 99;
ret[std::make_pair(Zerg_Zergling, Zerg_Devourer)] = 99;
ret[std::make_pair(Zerg_Zergling, Zerg_Sunken_Colony)] = 6;
ret[std::make_pair(Zerg_Zergling, Zerg_Spore_Colony)] = 1;

ret[std::make_pair(Zerg_Hydralisk, Protoss_Zealot)] = 3;
ret[std::make_pair(Zerg_Hydralisk, Protoss_Dragoon)] = 2;
ret[std::make_pair(Zerg_Hydralisk, Protoss_Dark_Templar)] = 4;
ret[std::make_pair(Zerg_Hydralisk, Protoss_Archon)] = 4;
ret[std::make_pair(Zerg_Hydralisk, Protoss_Reaver)] = 4;
ret[std::make_pair(Zerg_Hydralisk, Protoss_Scout)] = 2;
ret[std::make_pair(Zerg_Hydralisk, Protoss_Carrier)] = 6;
ret[std::make_pair(Zerg_Hydralisk, Protoss_Corsair)] = 0.01;
ret[std::make_pair(Zerg_Hydralisk, Protoss_Photon_Cannon)] = 3;
ret[std::make_pair(Zerg_Hydralisk, Terran_Marine)] = 0.5;
ret[std::make_pair(Zerg_Hydralisk, Terran_Firebat)] = 0.5;
ret[std::make_pair(Zerg_Hydralisk, Terran_Medic)] = 1.2;
ret[std::make_pair(Zerg_Hydralisk, Terran_Ghost)] = 0.5;
ret[std::make_pair(Zerg_Hydralisk, Terran_Vulture)] = 0.75;
ret[std::make_pair(Zerg_Hydralisk, Terran_Siege_Tank_Tank_Mode)] = 3;
ret[std::make_pair(Zerg_Hydralisk, Terran_Siege_Tank_Siege_Mode)] = 3;
ret[std::make_pair(Zerg_Hydralisk, Terran_Goliath)] = 2;
ret[std::make_pair(Zerg_Hydralisk, Terran_Wraith)] = 1;
ret[std::make_pair(Zerg_Hydralisk, Terran_Valkyrie)] = 0.01;
ret[std::make_pair(Zerg_Hydralisk, Terran_Battlecruiser)] = 4;
ret[std::make_pair(Zerg_Hydralisk, Terran_Bunker)] = 5;
ret[std::make_pair(Zerg_Hydralisk, Zerg_Zergling)] = 0.5;
ret[std::make_pair(Zerg_Hydralisk, Zerg_Hydralisk)] = 1;
ret[std::make_pair(Zerg_Hydralisk, Zerg_Lurker)] = 2;
ret[std::make_pair(Zerg_Hydralisk, Zerg_Ultralisk)] = 5;
ret[std::make_pair(Zerg_Hydralisk, Zerg_Mutalisk)] = 2;
ret[std::make_pair(Zerg_Hydralisk, Zerg_Scourge)] = 0.01;
ret[std::make_pair(Zerg_Hydralisk, Zerg_Guardian)] = 2;
ret[std::make_pair(Zerg_Hydralisk, Zerg_Devourer)] = 0.01;
ret[std::make_pair(Zerg_Hydralisk, Zerg_Sunken_Colony)] = 4;
ret[std::make_pair(Zerg_Hydralisk, Zerg_Spore_Colony)] = 0.01;

ret[std::make_pair(Zerg_Lurker, Protoss_Zealot)] = 1;
ret[std::make_pair(Zerg_Lurker, Protoss_Dragoon)] = 1;
ret[std::make_pair(Zerg_Lurker, Protoss_Dark_Templar)] = 1;
ret[std::make_pair(Zerg_Lurker, Protoss_Archon)] = 2;
ret[std::make_pair(Zerg_Lurker, Protoss_Reaver)] = 1.5;
ret[std::make_pair(Zerg_Lurker, Protoss_Scout)] = 99;
ret[std::make_pair(Zerg_Lurker, Protoss_Carrier)] = 99;
ret[std::make_pair(Zerg_Lurker, Protoss_Corsair)] = 99;
ret[std::make_pair(Zerg_Lurker, Protoss_Photon_Cannon)] = 2;
ret[std::make_pair(Zerg_Lurker, Terran_Marine)] = 0.25;
ret[std::make_pair(Zerg_Lurker, Terran_Firebat)] = 0.333;
ret[std::make_pair(Zerg_Lurker, Terran_Medic)] = 1;
ret[std::make_pair(Zerg_Lurker, Terran_Ghost)] = 0.25;
ret[std::make_pair(Zerg_Lurker, Terran_Vulture)] = 0.333;
ret[std::make_pair(Zerg_Lurker, Terran_Siege_Tank_Tank_Mode)] = 2;
ret[std::make_pair(Zerg_Lurker, Terran_Siege_Tank_Siege_Mode)] = 2;
ret[std::make_pair(Zerg_Lurker, Terran_Goliath)] = 1;
ret[std::make_pair(Zerg_Lurker, Terran_Wraith)] = 99;
ret[std::make_pair(Zerg_Lurker, Terran_Valkyrie)] = 99;
ret[std::make_pair(Zerg_Lurker, Terran_Battlecruiser)] = 99;
ret[std::make_pair(Zerg_Lurker, Terran_Bunker)] = 4;
ret[std::make_pair(Zerg_Lurker, Zerg_Zergling)] = 0.333;
ret[std::make_pair(Zerg_Lurker, Zerg_Hydralisk)] = 0.5;
ret[std::make_pair(Zerg_Lurker, Zerg_Lurker)] = 1;
ret[std::make_pair(Zerg_Lurker, Zerg_Ultralisk)] = 4;
ret[std::make_pair(Zerg_Lurker, Zerg_Mutalisk)] = 99;
ret[std::make_pair(Zerg_Lurker, Zerg_Scourge)] = 99;
ret[std::make_pair(Zerg_Lurker, Zerg_Guardian)] = 99;
ret[std::make_pair(Zerg_Lurker, Zerg_Devourer)] = 99;
ret[std::make_pair(Zerg_Lurker, Zerg_Sunken_Colony)] = 3;
ret[std::make_pair(Zerg_Lurker, Zerg_Spore_Colony)] = 0.01;

ret[std::make_pair(Zerg_Mutalisk, Protoss_Zealot)] = 0.01;
ret[std::make_pair(Zerg_Mutalisk, Protoss_Dragoon)] = 2;
ret[std::make_pair(Zerg_Mutalisk, Protoss_Dark_Templar)] = 0.01;
ret[std::make_pair(Zerg_Mutalisk, Protoss_Archon)] = 5;
ret[std::make_pair(Zerg_Mutalisk, Protoss_Reaver)] = 0.01;
ret[std::make_pair(Zerg_Mutalisk, Protoss_Scout)] = 3;
ret[std::make_pair(Zerg_Mutalisk, Protoss_Carrier)] = 7;
ret[std::make_pair(Zerg_Mutalisk, Protoss_Corsair)] = 2;
ret[std::make_pair(Zerg_Mutalisk, Protoss_Photon_Cannon)] = 3;
ret[std::make_pair(Zerg_Mutalisk, Terran_Marine)] = 0.5;
ret[std::make_pair(Zerg_Mutalisk, Terran_Firebat)] = 0.01;
ret[std::make_pair(Zerg_Mutalisk, Terran_Medic)] = 2.5;
ret[std::make_pair(Zerg_Mutalisk, Terran_Ghost)] = 0.5;
ret[std::make_pair(Zerg_Mutalisk, Terran_Vulture)] = 0.01;
ret[std::make_pair(Zerg_Mutalisk, Terran_Siege_Tank_Tank_Mode)] = 0.01;
ret[std::make_pair(Zerg_Mutalisk, Terran_Siege_Tank_Siege_Mode)] = 0.01;
ret[std::make_pair(Zerg_Mutalisk, Terran_Goliath)] = 2;
ret[std::make_pair(Zerg_Mutalisk, Terran_Wraith)] = 1.25;
ret[std::make_pair(Zerg_Mutalisk, Terran_Valkyrie)] = 2;
ret[std::make_pair(Zerg_Mutalisk, Terran_Battlecruiser)] = 6;
ret[std::make_pair(Zerg_Mutalisk, Terran_Bunker)] = 6;
ret[std::make_pair(Zerg_Mutalisk, Zerg_Zergling)] = 0.01;
ret[std::make_pair(Zerg_Mutalisk, Zerg_Hydralisk)] = 0.5;
ret[std::make_pair(Zerg_Mutalisk, Zerg_Lurker)] = 0.01;
ret[std::make_pair(Zerg_Mutalisk, Zerg_Ultralisk)] = 0.01;
ret[std::make_pair(Zerg_Mutalisk, Zerg_Mutalisk)] = 1;
ret[std::make_pair(Zerg_Mutalisk, Zerg_Scourge)] = 0.5;
ret[std::make_pair(Zerg_Mutalisk, Zerg_Guardian)] = 0.01;
ret[std::make_pair(Zerg_Mutalisk, Zerg_Devourer)] = 2;
ret[std::make_pair(Zerg_Mutalisk, Zerg_Sunken_Colony)] = 0.01;
ret[std::make_pair(Zerg_Mutalisk, Zerg_Spore_Colony)] = 5;

ret[std::make_pair(Zerg_Sunken_Colony, Protoss_Zealot)] = 0.5;
ret[std::make_pair(Zerg_Sunken_Colony, Protoss_Dragoon)] = 0.333;
ret[std::make_pair(Zerg_Sunken_Colony, Protoss_Dark_Templar)] = 0.5;
ret[std::make_pair(Zerg_Sunken_Colony, Protoss_Archon)] = 2;
ret[std::make_pair(Zerg_Sunken_Colony, Protoss_Reaver)] = 2;
ret[std::make_pair(Zerg_Sunken_Colony, Protoss_Scout)] = 99;
ret[std::make_pair(Zerg_Sunken_Colony, Protoss_Carrier)] = 99;
ret[std::make_pair(Zerg_Sunken_Colony, Protoss_Corsair)] = 99;
ret[std::make_pair(Zerg_Sunken_Colony, Protoss_Photon_Cannon)] = 0.5;
ret[std::make_pair(Zerg_Sunken_Colony, Terran_Marine)] = 0.167;
ret[std::make_pair(Zerg_Sunken_Colony, Terran_Firebat)] = 0.143;
ret[std::make_pair(Zerg_Sunken_Colony, Terran_Medic)] = 0.6;
ret[std::make_pair(Zerg_Sunken_Colony, Terran_Ghost)] = 0.125;
ret[std::make_pair(Zerg_Sunken_Colony, Terran_Vulture)] = 0.143;
ret[std::make_pair(Zerg_Sunken_Colony, Terran_Siege_Tank_Tank_Mode)] = 0.333;
ret[std::make_pair(Zerg_Sunken_Colony, Terran_Siege_Tank_Siege_Mode)] = 2;
ret[std::make_pair(Zerg_Sunken_Colony, Terran_Goliath)] = 0.333;
ret[std::make_pair(Zerg_Sunken_Colony, Terran_Wraith)] = 99;
ret[std::make_pair(Zerg_Sunken_Colony, Terran_Valkyrie)] = 99;
ret[std::make_pair(Zerg_Sunken_Colony, Terran_Battlecruiser)] = 99;
ret[std::make_pair(Zerg_Sunken_Colony, Terran_Bunker)] = 1;
ret[std::make_pair(Zerg_Sunken_Colony, Zerg_Zergling)] = 0.2;
ret[std::make_pair(Zerg_Sunken_Colony, Zerg_Hydralisk)] = 0.25;
ret[std::make_pair(Zerg_Sunken_Colony, Zerg_Lurker)] = 0.33;
ret[std::make_pair(Zerg_Sunken_Colony, Zerg_Ultralisk)] = 2;
ret[std::make_pair(Zerg_Sunken_Colony, Zerg_Mutalisk)] = 99;
ret[std::make_pair(Zerg_Sunken_Colony, Zerg_Scourge)] = 99;
ret[std::make_pair(Zerg_Sunken_Colony, Zerg_Guardian)] = 99;
ret[std::make_pair(Zerg_Sunken_Colony, Zerg_Devourer)] = 99;
ret[std::make_pair(Zerg_Sunken_Colony, Zerg_Sunken_Colony)] = 1;
ret[std::make_pair(Zerg_Sunken_Colony, Zerg_Spore_Colony)] = 0.01;
// ...
return ret;
}() };

int countHisBuildings(UnitType v) {
	int res = 0;
	for (auto u : hisBuildingPosAndType) {
		if (u.second == v)
			res++;
	}
	return res;
}

int countHisUnits(UnitType v) {
	int res = 0;
	for (auto u : hisUnitIDAndInfo) {
		if (u.second.unitType == v)
			res++;
	}
	return res;
}

std::map<UnitType, int> hisArmyCompositionGen(int currentFrame, std::vector<UnitType>& hisArmyUnits) {
	std::map<UnitType, int> compositionMap;
	bool isPrevAndNextInfoValid = hisUnitCompPrev.compare("-") && hisUnitCompNext.compare("-");

	if (Broodwar->enemy()->getRace() == Races::Protoss) {
		for (UnitType iType : hisArmyUnits) {
			if (isPrevAndNextInfoValid) {
				switch (iType) {
				case Protoss_Probe: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[0] + (int)hisUnitCompNext[0] - 66) / 2);
				case Protoss_Zealot: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[1] + (int)hisUnitCompNext[1] - 66) / 2);
				case Protoss_Dragoon: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[2] + (int)hisUnitCompNext[2] - 66) / 2);
				case Protoss_Dark_Templar: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[4] + (int)hisUnitCompNext[4] - 66) / 2);
				case Protoss_Archon: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[5] + (int)hisUnitCompNext[5] - 66) / 2);
				case Protoss_Reaver: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[7] + (int)hisUnitCompNext[7] - 66) / 2);
				case Protoss_Scout: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[10] + (int)hisUnitCompNext[10] - 66) / 2);
				case Protoss_Carrier: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[11] + (int)hisUnitCompNext[11] - 66) / 2);
				case Protoss_Arbiter: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[12] + (int)hisUnitCompNext[12] - 66) / 2);
				case Protoss_Corsair: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[13] + (int)hisUnitCompNext[13] - 66) / 2);
				case Protoss_Photon_Cannon: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[14] + (int)hisUnitCompNext[14] - 66) / 2);
				default: compositionMap[iType] = countHisUnits(iType);
				}
			}
			else {
				switch (iType) {
				case Protoss_Zealot:
					if (currentFrame < 4000)
						compositionMap[iType] = max(countHisUnits(iType), static_cast<int>(0.0005 * currentFrame));
					else
						compositionMap[iType] = max(countHisUnits(iType), static_cast<int>(-42.0177 - (-0.01602966 / 0.0002503888)*(1 - exp(-0.0002503888*currentFrame))));
				case Protoss_Dragoon:
					compositionMap[iType] = max(countHisUnits(iType), static_cast<int>(-0.1122988 - (-7.133408e-7 / -0.0007119366)*(1 - exp(+0.0007119366*currentFrame))));
				default:
					compositionMap[iType] = countHisUnits(iType);
				}
			}
		}
	}

	if (Broodwar->enemy()->getRace() == Races::Terran) {
		for (UnitType iType : hisArmyUnits) {
			if (isPrevAndNextInfoValid) {
				switch (iType) {
				case Terran_SCV: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[0] + (int)hisUnitCompNext[0] - 66) / 2);
				case Terran_Marine: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[1] + (int)hisUnitCompNext[1] - 66) / 2);
				case Terran_Firebat: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[2] + (int)hisUnitCompNext[2] - 66) / 2);
				case Terran_Ghost: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[4] + (int)hisUnitCompNext[4] - 66) / 2);
				case Terran_Vulture: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[5] + (int)hisUnitCompNext[5] - 66) / 2);
				case Terran_Siege_Tank_Tank_Mode: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[6] + (int)hisUnitCompNext[6] - 66) / 2);
				case Terran_Goliath: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[7] + (int)hisUnitCompNext[7] - 66) / 2);
				case Terran_Wraith: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[8] + (int)hisUnitCompNext[8] - 66) / 2);
				case Terran_Valkyrie: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[9] + (int)hisUnitCompNext[9] - 66) / 2);
				case Terran_Battlecruiser: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[10] + (int)hisUnitCompNext[10] - 66) / 2);
				case Terran_Bunker: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[13] + (int)hisUnitCompNext[13] - 66) / 2);
				default: compositionMap[iType] = countHisUnits(iType);
				}
			}
			else {
				switch (iType) {
				case Terran_Marine:
					if (currentFrame < 3000)
						compositionMap[iType] = max(countHisUnits(iType), 1);
					else if (currentFrame < 5000)
						compositionMap[iType] = max(countHisUnits(iType), 1 + (currentFrame - 3000) / 500);
					else
						compositionMap[iType] = max(countHisUnits(iType), static_cast<int>(16.46371 + (7.96268 - 16.46371) / (1 + pow(currentFrame / 9263.375, 25.82466))));
				case Terran_Medic:
					if (currentFrame < 6000)
						compositionMap[iType] = max(countHisUnits(iType), static_cast<int>(0.00016129 * currentFrame));
					else
						compositionMap[iType] = max(countHisUnits(iType), static_cast<int>(-49.64762 - (-0.01936287 / 0.0003440688)*(1 - exp(-0.0003440688*currentFrame))));
				case Terran_Firebat:
					if (currentFrame < 5000)
						compositionMap[iType] = max(countHisUnits(iType), static_cast<int>(0.0006 * currentFrame));
					else
						compositionMap[iType] = max(countHisUnits(iType), 3);
				case Terran_Siege_Tank_Tank_Mode:
					if (currentFrame < 12000)
						compositionMap[iType] = countHisUnits(iType);
					else if (currentFrame < 28000)
						compositionMap[iType] = max(countHisUnits(iType), 4 * (currentFrame - 12000) / 2000);
					else
						compositionMap[iType] = max(countHisUnits(iType), 32);
				default:
					compositionMap[iType] = countHisUnits(iType);
				}
			}
		}
	}

	if (Broodwar->enemy()->getRace() == Races::Zerg) {
		for (UnitType iType : hisArmyUnits) {
			if (isPrevAndNextInfoValid) {
				switch (iType) {
				case Zerg_Drone: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[0] + (int)hisUnitCompNext[0] - 66) / 2);
				case Zerg_Zergling: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[1] + (int)hisUnitCompNext[1] - 66) / 2);
				case Zerg_Hydralisk: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[2] + (int)hisUnitCompNext[2] - 66) / 2);
				case Zerg_Lurker: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[3] + (int)hisUnitCompNext[3] - 66) / 2);
				case Zerg_Ultralisk: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[4] + (int)hisUnitCompNext[4] - 66) / 2);
				case Zerg_Mutalisk: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[7] + (int)hisUnitCompNext[7] - 66) / 2);
				case Zerg_Scourge: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[8] + (int)hisUnitCompNext[8] - 66) / 2);
				case Zerg_Guardian: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[10] + (int)hisUnitCompNext[10] - 66) / 2);
				case Zerg_Devourer: compositionMap[iType] = max(countHisUnits(iType), ((int)hisUnitCompPrev[11] + (int)hisUnitCompNext[11] - 66) / 2);
				default: compositionMap[iType] = countHisUnits(iType);
				}
			}
			else {
				switch (iType) {
				case Zerg_Zergling:
					if (currentFrame < 5000)
						compositionMap[iType] = max(countHisUnits(iType), static_cast<int>(0.0016 * currentFrame));
					else if (currentFrame < 55000)
						compositionMap[iType] = max(countHisUnits(iType), static_cast<int>(14 + (8 - 14) / (1 + pow(currentFrame / 12752.4, 480.0013))));
					else
						compositionMap[iType] = max(countHisUnits(iType), 14);
				case Zerg_Hydralisk:
					if (currentFrame < 5000)
						compositionMap[iType] = max(countHisUnits(iType), static_cast<int>(0.0008 * currentFrame));
					else if (currentFrame < 20000)
						compositionMap[iType] = max(countHisUnits(iType), static_cast<int>(3.069396 - (-0.0001316532 / -0.0002135979)*(1 - exp(0.0002135979*currentFrame))));
					else
						compositionMap[iType] = max(countHisUnits(iType), 47);
				case Zerg_Lurker:
					if (currentFrame < 10000)
						compositionMap[iType] = countHisUnits(iType);
					else if (currentFrame < 12800)
						compositionMap[iType] = max(countHisUnits(iType), static_cast<int>(0.00285714 * (currentFrame - 10000)));
					else
						compositionMap[iType] = max(countHisUnits(iType), 8);
				default:
					compositionMap[iType] = countHisUnits(iType);
				}
			}
		}
	}

	return compositionMap;
} // std::map<UnitType, int> BattleCommander::hisArmyCompositionGen(frame_t currentFrame)

std::vector<UnitType> myFeasibleActionsGen() { // Assuming we are Zerg
	std::vector<UnitType> actionsContainer;

	if (Broodwar->self()->supplyTotal() - Broodwar->self()->supplyUsed() < 14)
		actionsContainer.push_back(Zerg_Overlord);

	if (Broodwar->self()->completedUnitCount(Zerg_Drone) < 20 * Broodwar->self()->completedUnitCount(Zerg_Hatchery))
		if (Broodwar->self()->completedUnitCount(Zerg_Drone) < 60)
			actionsContainer.push_back(Zerg_Drone);

	if (Broodwar->self()->completedUnitCount(Zerg_Spawning_Pool))
		actionsContainer.push_back(Zerg_Zergling);

	if (Broodwar->self()->completedUnitCount(Zerg_Hydralisk_Den) && Broodwar->self()->completedUnitCount(Zerg_Extractor))
		actionsContainer.push_back(Zerg_Hydralisk);

	if (Broodwar->self()->completedUnitCount(Zerg_Spire) && Broodwar->self()->completedUnitCount(Zerg_Extractor))
		actionsContainer.push_back(Zerg_Mutalisk);

	if (Broodwar->self()->completedUnitCount(Zerg_Ultralisk_Cavern) && Broodwar->self()->completedUnitCount(Zerg_Extractor))
		actionsContainer.push_back(Zerg_Ultralisk);

	if (Broodwar->self()->supplyUsed() >= 18)
		actionsContainer.push_back(Zerg_Hatchery);

	if (Broodwar->self()->completedUnitCount(Zerg_Spawning_Pool))
		if (Broodwar->self()->completedUnitCount(Zerg_Extractor) < Broodwar->self()->completedUnitCount(Zerg_Hatchery))
			if (Broodwar->self()->completedUnitCount(Zerg_Extractor) < 3)
				actionsContainer.push_back(Zerg_Extractor);

	if (Broodwar->self()->completedUnitCount(Zerg_Hatchery)) {
		if (Broodwar->self()->completedUnitCount(Zerg_Spawning_Pool))
			if (Broodwar->self()->completedUnitCount(Zerg_Creep_Colony) < 6)
				actionsContainer.push_back(Zerg_Creep_Colony);

		if (!Broodwar->self()->allUnitCount(Zerg_Spawning_Pool))
			actionsContainer.push_back(Zerg_Spawning_Pool);
	}

	if (Broodwar->self()->completedUnitCount(Zerg_Spawning_Pool) && Broodwar->self()->completedUnitCount(Zerg_Extractor)) {
		if (Broodwar->self()->allUnitCount(Zerg_Hydralisk_Den) == 0)
			actionsContainer.push_back(Zerg_Hydralisk_Den);
	}

	if (Broodwar->self()->completedUnitCount(Zerg_Lair) && Broodwar->self()->completedUnitCount(Zerg_Extractor)) {
		if (!Broodwar->self()->allUnitCount(Zerg_Spire))
			actionsContainer.push_back(Zerg_Spire);

		if (!Broodwar->self()->allUnitCount(Zerg_Queens_Nest))
			actionsContainer.push_back(Zerg_Queens_Nest);
	}

	if (Broodwar->self()->completedUnitCount(Zerg_Hive))
		if (!Broodwar->self()->allUnitCount(Zerg_Ultralisk_Cavern))
			actionsContainer.push_back(Zerg_Ultralisk_Cavern);

	return actionsContainer;
} // std::vector<UnitType> BattleCommander::myFeasibleActionsGen()

const int randomIntGen(int uniformDistUpperBnd) {
	std::random_device random_device;
	std::mt19937 engine{ random_device() };
	std::uniform_int_distribution<int> dist(0, uniformDistUpperBnd - 1);
	return dist(engine);
}

std::vector<UnitType> myChampionGen(std::vector<UnitType>& actions, int championSize) {
	std::vector<UnitType> champion;
	std::vector<UnitType> actionsCopy = actions;
	int lurkersPlanned = 0;
	int overlordsPlanned = 0;

	for (int i = 0; i < championSize; ++i) {
		int randomIndex = randomIntGen(actionsCopy.size());
		UnitType randomAction = actionsCopy[randomIndex]; // Causes a crash on Jade (map)
		champion.push_back(randomAction);
		if (randomAction.isBuilding()) // As Zerg, train no more than 1 building at a time
			actionsCopy.erase(actionsCopy.begin() + randomIndex);

		if (randomAction == Zerg_Lurker) {
			lurkersPlanned++;
			if (Broodwar->self()->completedUnitCount(Zerg_Hydralisk) - Broodwar->self()->completedUnitCount(Zerg_Lurker) - lurkersPlanned <= 0)
				actionsCopy.erase(actionsCopy.begin() + randomIndex);
		}

		if (randomAction == Zerg_Overlord) {
			overlordsPlanned++;
			if (overlordsPlanned >= 1)
				actionsCopy.erase(actionsCopy.begin() + randomIndex);
		}
	}

	return champion;
} // std::vector<UnitType> BattleCommander::myChampionGen(std::vector<UnitType>& actions)

std::vector<UnitType> myOffspringGen(std::vector<UnitType>& myChampion, std::vector<UnitType>& actions, int mutationType) {
	std::vector<UnitType> myOffspring = myChampion;
	int randomIndex = randomIntGen(myOffspring.size());
	int randomIndexAnother = randomIntGen(myOffspring.size());
	std::vector<UnitType> actionsCopy = actions;
	UnitType oldAction = myOffspring[randomIndex];

	switch (mutationType) {
	case 1: // cross-over
		for (int i = 0; i < randomIndex; ++i) {
			myOffspring.push_back(myOffspring.front());
			myOffspring.erase(myOffspring.begin());
		}
		return myOffspring;
		break;
	case 2: // swap-mutation
		swap(myOffspring[randomIndex], myOffspring[randomIndexAnother]);
		return myOffspring;
		break;
	case 3: // add-mutation
		myOffspring.erase(myOffspring.begin() + randomIndex);
		for (vector<UnitType>::iterator it = actionsCopy.begin(); it != actionsCopy.end();) {
			UnitType iut = *it;
			if (any_of(myOffspring.begin(), myOffspring.end(), [&iut](UnitType i) {return i.isBuilding() && i == iut; }))
				actionsCopy.erase(it);
			else if (iut == Zerg_Lurker && Broodwar->self()->completedUnitCount(Zerg_Hydralisk) - Broodwar->self()->completedUnitCount(Zerg_Lurker) - count_if(myOffspring.begin(), myOffspring.end(), [](UnitType i) {return i == Zerg_Lurker; }) < 1)
				actionsCopy.erase(it);
			else if (iut == Zerg_Overlord && count_if(myOffspring.begin(), myOffspring.end(), [](UnitType i) {return i == Zerg_Lurker; }) >= 1)
				actionsCopy.erase(it);
			else
				++it;
		}
		myOffspring.emplace(myOffspring.begin() + randomIndex, actionsCopy[randomIntGen(actionsCopy.size())]);
		return myOffspring;
		break;
	case 4: // remove-mutation
		myOffspring.erase(myOffspring.begin() + randomIndex);
		myOffspring.push_back(oldAction);
		return myOffspring;
		break;
	default:
		return myOffspring;
	}
} // std::vector<UnitType> BattleCommander::myOffspringGen(std::vector<UnitType>& myChampion, int mutationType)

std::vector<UnitType> armyUnitsGen(Race iRace) {
	if (iRace == BWAPI::Races::Protoss)
		return { Protoss_Zealot, Protoss_Dragoon, Protoss_Dark_Templar, Protoss_Archon, Protoss_Reaver, Protoss_Scout, Protoss_Carrier, Protoss_Photon_Cannon };
	else if (iRace == BWAPI::Races::Terran)
		return { Terran_Marine, Terran_Firebat, Terran_Medic, Terran_Ghost, Terran_Vulture, Terran_Siege_Tank_Tank_Mode, Terran_Siege_Tank_Siege_Mode, Terran_Goliath, Terran_Wraith, Terran_Valkyrie, Terran_Battlecruiser, Terran_Bunker };
	else if (iRace == BWAPI::Races::Zerg)
		return { Zerg_Zergling, Zerg_Hydralisk, Zerg_Lurker, Zerg_Ultralisk, Zerg_Mutalisk, Zerg_Scourge, Zerg_Guardian, Zerg_Sunken_Colony };
	else
		return { None };
}

double fitnessEval(std::vector<UnitType>& myChampion, int startingFrame, int currentAvailableSupply) {
	const int deltaFrame = 24;
	const int endingFrameTemp = startingFrame + deltaFrame * 60 * 10; // the deeper into the future, the more inaccuracy
	const int endingFrameReal = deltaFrame * 60 * 90;
	const int endingFrame = endingFrameTemp < endingFrameReal ? endingFrameTemp : endingFrameReal;
	const double badFitnessVal = -999;
	if (startingFrame >= endingFrame - deltaFrame)
		return badFitnessVal;

	double currentMinerals = Broodwar->self()->minerals();
	double currentGas = Broodwar->self()->gas();
	const int builderReachingTime = deltaFrame * 1;
	int timeToFinish = 0;
	const double mineralsPerSecond = 1.08;
	const double gasPerSecond = 1.68;

	// Find max mapped value:
	//std::map<UnitType, int> myArmyComposition;
	//auto maxn = max_element(myArmyComposition.begin(), myArmyComposition.end(), myArmyComposition.value_comp())->second;
	
	const int currentDrones = Broodwar->self()->completedUnitCount(Zerg_Drone);
	int currentExtractors = 3 * Broodwar->self()->completedUnitCount(Zerg_Extractor);
	int currentMiners = currentDrones - currentExtractors;
	const int maxMinersPlanned = count_if(myChampion.begin(), myChampion.end(), [](UnitType i) {return i == Zerg_Drone; });
	std::map<int, int> minersState;
	for (int i = 1; i <= maxMinersPlanned; ++i)
		minersState[i] = 0;

	int maxLarvae = 3 * Broodwar->self()->completedUnitCount(Zerg_Hatchery);

	UnitType zergOverlordUnitType = Zerg_Overlord;
	int timeOverlordStartedMorphing = 0;
	std::map<int, int> larvaeState;
	for (int i = 1; i <= maxLarvae; ++i)
		larvaeState[i] = 0;

	for (const auto &iChampion : myChampion) {
		for (int iFrame = startingFrame + timeToFinish; iFrame <= endingFrame; iFrame += deltaFrame) {
			// update larvaeState
			int currentLarvae = 0;
			for (int i = 1; i <= maxLarvae; ++i) {
				if (larvaeState[i] > 0)
					larvaeState[i]--;
				else
					currentLarvae++;
			}

			// update minersState
			int minersToAdd = 0;
			for (int i = 1; i <= maxMinersPlanned; ++i) {
				if (minersState[i] == 1)
					minersToAdd++;
				if (minersState[i] > 0)
					minersState[i]--;
			}
			currentMiners += minersToAdd;

			// update currentAvailableSupply
			if (!timeOverlordStartedMorphing)
				if (startingFrame + timeToFinish - timeOverlordStartedMorphing >= zergOverlordUnitType.buildTime()) {
					currentAvailableSupply += zergOverlordUnitType.supplyProvided() / 2;
					timeOverlordStartedMorphing = 0;
				}

			if (iChampion.isBuilding()) {
				if (iChampion == Zerg_Lair || iChampion == Zerg_Hive) {
					if (currentMinerals >= iChampion.mineralPrice() && currentGas >= iChampion.gasPrice()) {
						currentMinerals -= iChampion.mineralPrice();
						currentGas -= iChampion.gasPrice();
						break;
					}
					else { // waiting for money:
						currentMinerals += mineralsPerSecond * currentMiners;
						currentGas += gasPerSecond * currentExtractors;
						timeToFinish += deltaFrame;
					}
				}
				else {
					if (currentMinerals >= iChampion.mineralPrice() && currentGas >= iChampion.gasPrice()) {
						if (currentMiners > 0) {
							currentMiners--;
							currentAvailableSupply++;
							currentMinerals -= iChampion.mineralPrice();
							currentGas -= iChampion.gasPrice();
							break;
						}
						else
							return badFitnessVal;
					}
					else { // waiting for money:
						currentMinerals += mineralsPerSecond * currentMiners;
						currentGas += gasPerSecond * currentExtractors;
						timeToFinish += deltaFrame;
					}
				}
			}
			else { // non-building:
				if (currentMinerals >= iChampion.mineralPrice() && currentGas >= iChampion.gasPrice()) {
					if (iChampion == Zerg_Overlord) {
						if (currentLarvae) {
							currentMinerals -= iChampion.mineralPrice();
							if (!timeOverlordStartedMorphing)
								timeOverlordStartedMorphing = startingFrame + timeToFinish;
							for (int i = 1; i <= maxLarvae; ++i)
								if (larvaeState[i] == 0) {
									larvaeState[i] = 14;
									break; // only concerns 1 larva
								}
							break;
						}
						else { // waiting for larvae:
							currentMinerals += mineralsPerSecond * currentMiners;
							currentGas += gasPerSecond * currentExtractors;
							timeToFinish += deltaFrame;
						}
					}
					else if (iChampion == Zerg_Lurker) {
						if (currentAvailableSupply >= iChampion.supplyRequired() / 4) {
							currentAvailableSupply++;
							currentMinerals -= iChampion.mineralPrice();
							currentGas -= iChampion.gasPrice();
							break;
						}
						else {
							currentMinerals += mineralsPerSecond * currentMiners;
							currentGas += gasPerSecond * currentExtractors;
							timeToFinish += deltaFrame;
						}
					}
					else if (iChampion == Zerg_Zergling) { // Zergling has a special supply count of 0.5:
						if (currentAvailableSupply >= iChampion.supplyRequired()) {
							if (currentLarvae) {
								currentAvailableSupply -= iChampion.supplyRequired();
								currentMinerals -= iChampion.mineralPrice();
								currentGas -= iChampion.gasPrice();
								for (int i = 1; i <= maxLarvae; ++i)
									if (larvaeState[i] == 0) {
										larvaeState[i] = 14;
										break; // only concerns 1 larva
									}
								break;
							}
							else {
								currentMinerals += mineralsPerSecond * currentMiners;
								currentGas += gasPerSecond * currentExtractors;
								timeToFinish += deltaFrame;
							}
						}
						else {
							currentMinerals += mineralsPerSecond * currentMiners;
							currentGas += gasPerSecond * currentExtractors;
							timeToFinish += deltaFrame;
						}
					}
					else if (iChampion == Zerg_Drone) { // adding miner:
						if (currentAvailableSupply >= iChampion.supplyRequired() / 2) {
							if (currentLarvae) {
								currentAvailableSupply -= iChampion.supplyRequired() / 2;
								currentMinerals -= iChampion.mineralPrice();
								currentGas -= iChampion.gasPrice();
								for (int i = 1; i <= maxLarvae; ++i)
									if (larvaeState[i] == 0) {
										larvaeState[i] = 14;
										break; // only concerns 1 larva
									}
								for (int i = 1; i <= maxMinersPlanned; ++i)
									if (minersState[i] == 0) {
										minersState[i] = 13;
										break; // only concerns 1 drone at a time
									}
								break;
							}
							else {
								currentMinerals += mineralsPerSecond * currentMiners;
								currentGas += gasPerSecond * currentExtractors;
								timeToFinish += deltaFrame;
							}
						}
						else {
							currentMinerals += mineralsPerSecond * currentMiners;
							currentGas += gasPerSecond * currentExtractors;
							timeToFinish += deltaFrame;
						}
					}
					else { // other units from Larvae:
						if (currentAvailableSupply >= iChampion.supplyRequired() / 2) {
							if (currentLarvae) {
								currentAvailableSupply -= iChampion.supplyRequired() / 2;
								currentMinerals -= iChampion.mineralPrice();
								currentGas -= iChampion.gasPrice();
								for (int i = 1; i <= maxLarvae; ++i)
									if (larvaeState[i] == 0) {
										larvaeState[i] = 14;
										break; // only concerns 1 larva
									}
								break;
							}
							else {
								currentMinerals += mineralsPerSecond * currentMiners;
								currentGas += gasPerSecond * currentExtractors;
								timeToFinish += deltaFrame;
							}
						}
						else {
							currentMinerals += mineralsPerSecond * currentMiners;
							currentGas += gasPerSecond * currentExtractors;
							timeToFinish += deltaFrame;
						}
					}
				}
				else { // waiting for money:
					currentMinerals += mineralsPerSecond * currentMiners;
					currentGas += gasPerSecond * currentExtractors;
					timeToFinish += deltaFrame;
				}
			}
		} // time elapses
	} // for (const auto &iChampion : myChampion)	

	// ==========================================================================================================================================================================
	vector<UnitType> hisArmyUnits = armyUnitsGen(Broodwar->enemy()->getRace());
	vector<UnitType> myArmyUnits = armyUnitsGen(Broodwar->self()->getRace());
	double myArmyValueTotal = 0.0;
	int armySizeProductTotal = 0;
	std::map<std::pair<UnitType, UnitType>, double> unitMatchupTable = unitMatchupTableGen;
	std::map<UnitType, int> hisArmyComposition = hisArmyCompositionGen(Broodwar->getFrameCount(), hisArmyUnits);

	for (UnitType iMyType : myArmyUnits) {
		for (UnitType iHisType : hisArmyUnits) {
			int hisNumber = hisArmyComposition[iHisType];
			int myNumber = Broodwar->self()->completedUnitCount(iMyType)
				+ count_if(myChampion.begin(), myChampion.end(), [&iMyType](UnitType i) {return i == iMyType; });

			int iArmySizeProduct = hisNumber * myNumber;
			if (iArmySizeProduct) {
				armySizeProductTotal += iArmySizeProduct;
				myArmyValueTotal += iMyType == Zerg_Creep_Colony ? iArmySizeProduct / unitMatchupTable.find(std::make_pair(Zerg_Sunken_Colony, iHisType))->second :
					iArmySizeProduct / unitMatchupTable.find(std::make_pair(iMyType, iHisType))->second;
			}
		}
	}

	timeToFinish += myChampion.back().buildTime();
	if (armySizeProductTotal) // army quality is more important here
		return (1.00 - timeToFinish / static_cast<double>(endingFrame - startingFrame)) * 0.2 + myArmyValueTotal / armySizeProductTotal * 0.8;
	else
		return (1.00 - timeToFinish / static_cast<double>(endingFrame - startingFrame));
	//return exp(-timeToFinish / static_cast<double>(endingFrame - startingFrame)) * 100;
} // double BattleCommander::fitnessEval(std::vector<UnitType>& myChampion, frame_t startingFrame, int currentAvailableSupply)

std::vector<UnitType> runCOEP(std::vector<UnitType>& actions, int popSize, int numGenerations, int championSize, double crossOverRate, double mutationRate) {
	std::map<double, vector<UnitType>> genomeFitnessMap;
	int currentFrame = Broodwar->getFrameCount();
	int currentAvailableSupply = Broodwar->self()->supplyUsed() / 2;
	std::vector<double> genomeFitnessKeys;

	// Generate initial population
	for (int i = 0; i < popSize; ++i) {
		std::vector<UnitType> iChampion = myChampionGen(actions, championSize);
		double iFitness = fitnessEval(iChampion, currentFrame, currentAvailableSupply);

		genomeFitnessKeys.push_back(iFitness);
		genomeFitnessMap[iFitness] = iChampion;
	}

	const int maxCrossovers = static_cast<int>(crossOverRate * championSize);
	const int maxMutations = static_cast<int>(mutationRate * championSize);

	// Propagate through all generations
	for (int iGeneration = 0; iGeneration < numGenerations; ++iGeneration) {
		int currentPopulationSize = genomeFitnessKeys.size();
		double currentBestFitness = genomeFitnessMap.rbegin()->first;
		//bw << "currentBestFitness: " << currentBestFitness << endl;

		// Do cross-over
		for (int iC = 0; iC < maxCrossovers; ++iC) {
			double key1 = genomeFitnessKeys[randomIntGen(currentPopulationSize)];
			double key2 = genomeFitnessKeys[randomIntGen(currentPopulationSize)];
			std::vector<UnitType> parent1 = genomeFitnessMap[key1];
			std::vector<UnitType> parent2 = genomeFitnessMap[key2];
			std::vector<UnitType> offSpring1;
			std::vector<UnitType> offSpring2;
			const int crossoverCutoff = randomIntGen(championSize);

			for (int iL = 0; iL < crossoverCutoff; ++iL) {
				offSpring1.push_back(parent2[iL]);
				offSpring2.push_back(parent1[iL]);
			}
			for (int iR = crossoverCutoff; iR < championSize; ++iR) {
				offSpring1.push_back(parent1[iR]);
				offSpring2.push_back(parent2[iR]);
			}

			double fitness1 = fitnessEval(offSpring1, currentFrame, currentAvailableSupply);
			if (fitness1 > currentBestFitness) {
				genomeFitnessKeys.push_back(fitness1);
				genomeFitnessMap[fitness1] = offSpring1;
			}
			double fitness2 = fitnessEval(offSpring2, currentFrame, currentAvailableSupply);
			if (fitness2 > currentBestFitness) {
				genomeFitnessKeys.push_back(fitness2);
				genomeFitnessMap[fitness2] = offSpring2;
			}
		}

		// Do mutation
		for (int iM = 0; iM < maxMutations; ++iM) {
			double iKeyM = genomeFitnessKeys[randomIntGen(currentPopulationSize)];
			std::vector<UnitType> iOffSpringM = myOffspringGen(genomeFitnessMap[iKeyM], actions, randomIntGen(4));
			double iFitnessM = fitnessEval(iOffSpringM, currentFrame, currentAvailableSupply);
			if (iFitnessM > currentBestFitness) {
				genomeFitnessKeys.push_back(iFitnessM);
				genomeFitnessMap[iFitnessM] = iOffSpringM;
			}
		}
	}

	// find the biggest key in a map: m.rbegin();
	// https://stackoverflow.com/questions/1660195/c-how-to-find-the-biggest-key-in-a-stdmap
	return genomeFitnessMap.rbegin()->second;
}

int UnitTypeToInt(UnitType ut) {
	if (ut == Zerg_Larva) return 34;
	else if (ut == Zerg_Egg) return 35;
	else if (ut == Zerg_Zergling) return 36;
	else if (ut == Zerg_Hydralisk) return 37;
	else if (ut == Zerg_Ultralisk) return 38;
	else if (ut == Zerg_Drone) return 40;
	else if (ut == Zerg_Overlord) return 41;
	else if (ut == Zerg_Mutalisk) return 42;
	else if (ut == Zerg_Scourge) return 46;
	else if (ut == Zerg_Lurker) return 97;
	else if (ut == Zerg_Hatchery) return 123;
	else if (ut == Zerg_Lair) return 124;
	else if (ut == Zerg_Hive) return 125;
	else if (ut == Zerg_Hydralisk_Den) return 127;
	else if (ut == Zerg_Queens_Nest) return 130;
	else if (ut == Zerg_Evolution_Chamber) return 131;
	else if (ut == Zerg_Spire) return 133;
	else if (ut == Zerg_Spawning_Pool) return 134;
	else if (ut == Zerg_Creep_Colony) return 135;
	else if (ut == Zerg_Sunken_Colony) return 137;
	else if (ut == Zerg_Spore_Colony) return 136;
	else if (ut == Zerg_Extractor) return 140;
	else if (ut == Terran_Marine) return 2;
	else if (ut == Protoss_Zealot) return 4;
	else return 0;
}