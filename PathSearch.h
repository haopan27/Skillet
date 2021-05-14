#include<stdlib.h>
#include<math.h>
#include<utility>
#include<stack>
#include<set>
#include<vector>
#include<map>

//#define COLMAX 1024
#define ROW 128
#define COL 128
#define MINROWCOL 128

using namespace std;
int numSteps = 0;

// Creating a shortcut for int, int pair type 
typedef pair<int, int> Pair; 

// Creating a shortcut for pair<int, pair<int, int>> type 
typedef pair<double, pair<int, int>> pPair; 

// A structure to hold the neccesary parameters 
struct cell 
{ 
  // Row and Column index of its parent 
  // Note that 0 <= i <= ROW-1 & 0 <= j <= COL-1 
  int parent_i, parent_j; 
  // f = g + h 
  double f, g, h; 
}; 

// A Utility Function to check whether given cell (row, col) 
// is a valid cell or not. 
bool isValid(int row, int col) 
{ 
  // Returns true if row number and column number is in range 
  //cout << "isValid? " << row << ", " << col << ", " << ROW << ", " << COL << endl;
  
  return (row >= 0) && (row < ROW) && 
    (col >= 0) && (col < COL); 
} 

// A Utility Function to check whether the given cell is 
// blocked or not 
bool isUnBlocked(int grid[][COL], int row, int col) // [COL]
{ 
  // Returns true if the cell is not blocked else false 
  if (grid[row][col] == 0) 
    return (true); 
  else
    return (false); 
} 

// A Utility Function to check whether destination cell has 
// been reached or not 
bool isDestination(int row, int col, Pair dest) 
{ 
  if (row == dest.first && col == dest.second) 
    return (true); 
  else
    return (false); 
} 

// A Utility Function to calculate the 'h' heuristics. 
double calculateHValue(int row, int col, Pair dest) 
{ 
  // Return using the distance formula 
  return ((double)sqrt ((row-dest.first)*(row-dest.first) 
                          + (col-dest.second)*(col-dest.second))); 
} 

// A Utility Function to trace the path from the source 
// to destination 
double tracePath(cell cellDetails[][COL], Pair dest, vector<int> * nextPos) // [COL]
{ 
  int row = dest.first; 
  int col = dest.second; 
  
  stack<Pair> Path; 
  
  while (!(cellDetails[row][col].parent_i == row 
             && cellDetails[row][col].parent_j == col )) 
  { 
    Path.push (make_pair (row, col)); 
    int temp_row = cellDetails[row][col].parent_i; 
    int temp_col = cellDetails[row][col].parent_j; 
    row = temp_row; 
    col = temp_col; 
  } 
  
  Path.push (make_pair (row, col));
  double pathLength = 0.0;
  stack<Pair> Path0 = Path;
  stack<Pair> Path1 = Path;
  Path1.pop();

  nextPos->push_back(Path1.top().first);
  nextPos->push_back(Path1.top().second);

  int pathSize = Path1.size();
  for (int i = 0; i < pathSize; i++)
  {
	  pathLength += calculateHValue(Path0.top().first, Path0.top().second, Path1.top());
	  Path0.pop();
	  Path1.pop();
  }

  while (!Path.empty()) 
  { 
    pair<int,int> p = Path.top(); 
    Path.pop(); 
  } 
  
  return pathLength; 
} 

bool isCovered(Pair fromNode, Pair toNode, Pair thisNode) {
	int rowStart = fromNode.first;
	int rowEnd = toNode.first;
	if (fromNode.first > toNode.first) {
		rowStart = toNode.first;
		rowEnd = fromNode.first;
	}

	int colStart = fromNode.second;
	int colEnd = toNode.second;
	if (fromNode.second > toNode.second) {
		colStart = toNode.second;
		colEnd = fromNode.second;
	}

	if (fromNode.first == toNode.first && fromNode.first == thisNode.first)
		//if (colStart <= thisNode.second && thisNode.second <= colEnd)
		if ((colEnd - colStart) * (thisNode.second - colEnd) < 0)
			return true;

	if (fromNode.second == toNode.second && fromNode.second == thisNode.second)
		//if (rowStart <= thisNode.first && thisNode.first <= rowEnd)
		if ((rowEnd - rowStart) * (thisNode.first - rowEnd) < 0)
			return true;

	if (abs(fromNode.first - toNode.first) == abs(fromNode.second - toNode.second) 
		&& abs(fromNode.first - thisNode.first) == abs(fromNode.second - thisNode.second))
		// if (colStart <= thisNode.second && thisNode.second <= colEnd
		// 	&& rowStart <= thisNode.first && thisNode.first <= rowEnd)
		if ((colEnd - colStart) * (thisNode.second - colEnd) < 0
        && (rowEnd - rowStart) * (thisNode.first - rowEnd) < 0)
			return true;

	return false;
}

bool canTraverse(Pair fromNode, Pair toNode, int grid[][COL]) { // [COL]
	bool passesObstacles = false;
	bool directionIsGood = false;

	int rowStart = fromNode.first;
	int rowEnd = toNode.first;
	int rowDir = 1;
	if (fromNode.first > toNode.first) {
		rowStart = toNode.first;
		rowEnd = fromNode.first;
		rowDir = -1;
	}

	int colStart = fromNode.second;
	int colEnd = toNode.second;
	int colDir = 1;
	if (fromNode.second > toNode.second) {
		colStart = toNode.second;
		colEnd = fromNode.second;
		colDir = -1;
	}

	if (fromNode.first == toNode.first) {
		directionIsGood = true;
		for (int iCol = colStart + 1; iCol < colEnd; iCol++)
			if (!isUnBlocked(grid, rowStart, iCol)) {
				passesObstacles = true;
				break;
			}
	}
	else if (fromNode.second == toNode.second) {
		directionIsGood = true;
		for (int iRow = rowStart + 1; iRow < rowEnd; iRow++)
			if (!isUnBlocked(grid, iRow, colStart)) {
				passesObstacles = true;
				break;
			}
	} 
	else if (abs(fromNode.first - toNode.first) == abs(fromNode.second - toNode.second)) 
	{
		directionIsGood = true;
		int numSteps = abs(fromNode.first - toNode.first) - 1;
		if (numSteps)
			for (int nS = 1; nS <= numSteps; nS++)
				if (!isUnBlocked(grid, rowStart + rowDir * nS, colStart + colDir * nS)) {
					passesObstacles = true;
					break;
				}
	}

	return !passesObstacles && directionIsGood;
}

bool isJP(int i, int j, int grid[][COL], int dir, Pair dest) { // [COL]
	if(!isValid(i, j)) return false;
	
	if (isDestination(i, j, dest)) return true;

	if (dir == 1) { // dir == 1: left -> right
		if (isValid(i - 1, j) && isValid(i - 1, j + 1) && !isUnBlocked(grid, i - 1, j) && isUnBlocked(grid, i - 1, j + 1)) return true;
		if (isValid(i + 1, j) && isValid(i + 1, j + 1) && !isUnBlocked(grid, i + 1, j) && isUnBlocked(grid, i + 1, j + 1)) return true;
	}
	else if (dir == 2) { // dir == 2: right -> left
		if (isValid(i - 1, j) && isValid(i - 1, j - 1) && !isUnBlocked(grid, i - 1, j) && isUnBlocked(grid, i - 1, j - 1)) return true;
		if (isValid(i + 1, j) && isValid(i + 1, j - 1) && !isUnBlocked(grid, i + 1, j) && isUnBlocked(grid, i + 1, j - 1)) return true;
	}
	else if (dir == 3) { // dir == 3: top -> bottom
		if (isValid(i, j - 1) && isValid(i + 1, j - 1) && !isUnBlocked(grid, i, j - 1) && isUnBlocked(grid, i + 1, j - 1)) return true;
		if (isValid(i, j + 1) && isValid(i + 1, j + 1) && !isUnBlocked(grid, i, j + 1) && isUnBlocked(grid, i + 1, j + 1)) return true;
	} else if (dir == 4) { // dir == 4: bottom -> top
		if (isValid(i, j - 1) && isValid(i - 1, j - 1) && !isUnBlocked(grid, i, j - 1) && isUnBlocked(grid, i - 1, j - 1)) return true;
		if (isValid(i, j + 1) && isValid(i - 1, j + 1) && !isUnBlocked(grid, i, j + 1) && isUnBlocked(grid, i - 1, j + 1)) return true;
	}

	return false;
} // isJP(...)

Pair FindNextJP(int i, int j, int grid[][COL], int dir0, Pair dest, int countIJ) { // [COL]
	if (isDestination(i, j, dest)) return make_pair(i, j);

	if (dir0 == 4) { // NE
		for (int hI = j + (countIJ == 0 ? 1 : 0); hI < COL; hI++) {
			if (!isUnBlocked(grid, i, hI)) break;

			if (isJP(i, hI, grid, 1, dest))
				return make_pair(i, hI);
		}

		for (int vI = i - (countIJ == 0 ? 1 : 0); vI >= 0; vI--) {
			if (!isUnBlocked(grid, vI, j)) break;

			if (isJP(vI, j, grid, 4, dest))
				return make_pair(vI, j);
		}
	} 
	else if (dir0 == 5) { // NW
		for (int hI = j - (countIJ == 0 ? 1 : 0); hI >= 0; hI--) {
			if (!isUnBlocked(grid, i, hI)) break;

			if (isJP(i, hI, grid, 2, dest))
				return make_pair(i, hI);
		}

		for (int vI = i - (countIJ == 0 ? 1 : 0); vI >= 0; vI--) {
			if (!isUnBlocked(grid, vI, j)) break;

			if (isJP(vI, j, grid, 4, dest))
				return make_pair(vI, j);
		}
	}
	else if (dir0 == 6) { // SE
		for (int hI = j + (countIJ == 0 ? 1 : 0); hI < COL; hI++) {
			if (!isUnBlocked(grid, i, hI)) break;

			if (isJP(i, hI, grid, 1, dest))
				return make_pair(i, hI);
		}

		for (int vI = i + (countIJ == 0 ? 1 : 0); vI < ROW; vI++) {
			if (!isUnBlocked(grid, vI, j)) break;

			if (isJP(vI, j, grid, 3, dest))
				return make_pair(vI, j);
		}
	}
	else if (dir0 == 7) { // SW
		for (int hI = j - (countIJ == 0 ? 1 : 0); hI >= 0; hI--) {
			if (!isUnBlocked(grid, i, hI)) break;

			if (isJP(i, hI, grid, 2, dest))
				return make_pair(i, hI);
		}

		for (int vI = i + (countIJ == 0 ? 1 : 0); vI < ROW; vI++) {
			if (!isUnBlocked(grid, vI, j)) break;

			if (isJP(vI, j, grid, 3, dest))
				return make_pair(vI, j);
		}
	}

	return make_pair(-1, -1);
} // Pair FindNextJP(...)

// A Function to find the shortest path between 
// a given source cell to a destination cell according 
// to the Custom Search Algorithm 
double pathSearch(int grid[][COL], Pair src, Pair dest, bool useMCTS, bool usePF, bool useJPS, vector<int> * nextPos) // [COL]
{ 
  // If the source is out of range 
  if (isValid (src.first, src.second) == false) 
  { 
    return 10.0 * ROW * COL; 
  } 
  
  // If the destination is out of range 
  if (isValid (dest.first, dest.second) == false) 
  { 
    return 10.0 * ROW * COL;
  } 
  
  // Either the source or the destination is blocked 
  if (isUnBlocked(grid, src.first, src.second) == false || 
      isUnBlocked(grid, dest.first, dest.second) == false) 
  { 
    return 10.0 * ROW * COL;
  } 
  
  // If the destination cell is the same as source cell 
  if (isDestination(src.first, src.second, dest) == true) 
  { 
    return 0.0; 
  } 
  
  // Create a closed list and initialise it to false which means 
  // that no cell has been included yet 
  // This closed list is implemented as a boolean 2D array 
  bool closedList[ROW][COL]; 
  memset(closedList, false, sizeof (closedList)); 
  
  // Declare a 2D array of structure to hold the details of that cell 
  //vector<vector<cell>> cellDetails(ROW, vector<cell>(COL)); // [COL]
  cell cellDetails[ROW][COL];

  int i, j; 
  
  for (i=0; i<ROW; i++) 
  { 
    for (j=0; j<COL; j++) 
    { 
      cellDetails[i][j].f = FLT_MAX; 
      cellDetails[i][j].g = FLT_MAX; 
      cellDetails[i][j].h = FLT_MAX; 
      cellDetails[i][j].parent_i = -1; 
      cellDetails[i][j].parent_j = -1; 
    } 
  } 
  
  // Initialising the parameters of the starting node 
  i = src.first, j = src.second; 
  cellDetails[i][j].f = 0.0; 
  cellDetails[i][j].g = 0.0; 
  cellDetails[i][j].h = 0.0; 
  cellDetails[i][j].parent_i = i; 
  cellDetails[i][j].parent_j = j; 
  
  /* 
  Create an open list having information as- 
  <f, <i, j>> 
  where f = g + h, 
  and i, j are the row and column index of that cell 
  Note that 0 <= i <= ROW-1 & 0 <= j <= COL-1 
  This open list is implenented as a set of pair of pair.*/
  set<pPair> openList; 
  
  // Put the starting cell on the open list and set its 
  // 'f' as 0 
  openList.insert(make_pair (0.0, make_pair (i, j))); 
  
  // We set this boolean value as false as initially 
  // the destination is not reached. 
  bool foundDest = false; 
  //vector<vector<int> > numVisits(ROW, vector<int>(COL, 0)); // [COL]
  int numVisits[ROW][COL] = {};
  
  while (!openList.empty()) 
  { 
    pPair p = *openList.begin(); 
    
    // Remove this vertex from the open list 
    openList.erase(openList.begin()); 
    
    // Add this vertex to the closed list 
    i = p.second.first; 
    j = p.second.second; 
    closedList[i][j] = true; 
    
    /* 
    Generating all the 8 successor of this cell 
    
    N.W   N     N.E 
	  \   |    / 
		\ |  / 
    W----Cell----E 
		/ |  \ 
	  /   |   \ 
    S.W   S    S.E 
    
    Cell-->Popped Cell (i, j) 
    N -->  North       (i-1, j) 
    S -->  South       (i+1, j) 
    E -->  East        (i, j+1) 
    W -->  West        (i, j-1) 
    N.E--> North-East  (i-1, j+1) 
    N.W--> North-West  (i-1, j-1) 
    S.E--> South-East  (i+1, j+1) 
    S.W--> South-West  (i+1, j-1)*/
    
    // To store the 'g', 'h' and 'f' of the 8 successors 
    double gNew, hNew, fNew; 
    
    vector<vector<int>> allDirections = {{i - 1, j}, {i + 1, j}, {i, j + 1}, {i, j - 1}, 
                                         {i - 1, j + 1}, {i - 1, j - 1}, {i + 1, j + 1}, {i + 1, j - 1}};
    
	if (useJPS) {
	  //cout << "JPS >>> Current node is: (" << i << ", " << j << ")" << endl;
	  
		int ind1 = -1;
		int ind2 = -1;
		map<double, Pair> scoreAndNode;
		Pair nodeIJ = make_pair(i, j);
		vector<Pair> allCandidates;
		map<int, bool> dirAndBlocked;

		// Find the next JP
		for (int diagI = 0; diagI < MINROWCOL; diagI++)
			for (int dir0 = 4; dir0 <= 7; dir0++) { // 4: NE; 5: NW; 6: SE; 7: SW
				if (diagI == 0) dirAndBlocked[dir0] = false;

				//cout << "diagI: " << diagI << ", dir0: " << dir0 << endl;
				int sI = i + diagI * (allDirections.at(dir0).front() - i);
				int sJ = j + diagI * (allDirections.at(dir0).back() - j);
				Pair nodeSISJ = make_pair(sI, sJ);

				if (!dirAndBlocked[dir0] && !isUnBlocked(grid, sI, sJ)) dirAndBlocked[dir0] = true;

				if (!dirAndBlocked[dir0] && isValid(sI, sJ) && isUnBlocked(grid, sI, sJ)) {
  					Pair nextJP = FindNextJP(sI, sJ, grid, dir0, dest, diagI);
  			  
  					if (isValid(nextJP.first, nextJP.second) == true && isUnBlocked(grid, nextJP.first, nextJP.second) && !closedList[nextJP.first][nextJP.second]
						&& canTraverse(nodeSISJ, nextJP, grid))
  					{
						Pair parentNode = make_pair(cellDetails[i][j].parent_i, cellDetails[i][j].parent_j);
						if (diagI == 0) {
							if (!isCovered(parentNode, nodeIJ, nextJP)) {
								//scoreAndNode[cellDetails[i][j].g + calculateHValue(i, j, nextJP) + calculateHValue(nextJP.first, nextJP.second, dest)] = nextJP;
								//scoreAndNode[calculateHValue(nextJP.first, nextJP.second, dest)] = nextJP;
								scoreAndNode[pathSearch(grid, nextJP, dest, false, false, false, nextPos)] = nextJP;
								allCandidates.push_back(nextJP);
							}
						}
						else if (canTraverse(nodeIJ, nodeSISJ, grid)) { 
							if (!isCovered(parentNode, nodeIJ, nodeSISJ)) {
								//scoreAndNode[cellDetails[i][j].g + calculateHValue(i, j, nodeSISJ) + calculateHValue(sI, sJ, dest)] = nodeSISJ; 
								//scoreAndNode[calculateHValue(sI, sJ, dest)] = nodeSISJ;
								scoreAndNode[pathSearch(grid, nodeSISJ, dest, false, false, false, nextPos)] = nodeSISJ;
								allCandidates.push_back(nodeSISJ);
							}
						}
  						continue;
  					}
				}
				
				if (diagI == MINROWCOL && dir0 == 7)
				  if (scoreAndNode.empty())
				    return 10.0 * ROW * COL;
			}

		if (!scoreAndNode.empty()) {
		  
			ind1 = scoreAndNode.begin()->second.first;
			ind2 = scoreAndNode.begin()->second.second;
		}

		if (isValid(ind1, ind2) == true)
		{
			// If the destination cell is the same as the 
			// current successor 
			if (isDestination(ind1, ind2, dest) == true)
			{
				// Set the Parent of the destination cell 
				cellDetails[ind1][ind2].parent_i = i;
				cellDetails[ind1][ind2].parent_j = j;
				foundDest = true;
				return tracePath(cellDetails, dest, nextPos);
			}
			// If the successor is already on the closed 
			// list or if it is blocked, then ignore it. 
			// Else do the following 
			else if (closedList[ind1][ind2] == false && isUnBlocked(grid, ind1, ind2) == true)
			{
			  //gNew = cellDetails[i][j].g + max(double(abs(ind1 - i)), double(abs(ind2 - j)));
				gNew = cellDetails[i][j].g + calculateHValue(ind1, ind2, nodeIJ);
				//hNew = calculateHValue(ind1, ind2, dest);
				hNew = pathSearch(grid, make_pair(ind1, ind2), dest, false, false, false, nextPos);
				fNew = gNew + hNew;

				// If it isn't on the open list, add it to 
				// the open list. Make the current square 
				// the parent of this square. Record the 
				// f, g, and h costs of the square cell 
				//                OR 
				// If it is on the open list already, check 
				// to see if this path to that square is better, 
				// using 'f' cost as the measure. 
				
				if (cellDetails[ind1][ind2].f == FLT_MAX ||
					cellDetails[ind1][ind2].f > fNew)
				{
					openList.insert(make_pair(fNew,
						make_pair(ind1, ind2)));

					// Update the details of this cell 
					cellDetails[ind1][ind2].f = fNew;
					cellDetails[ind1][ind2].g = gNew;
					cellDetails[ind1][ind2].h = hNew;
					cellDetails[ind1][ind2].parent_i = i;
					cellDetails[ind1][ind2].parent_j = j;

					numVisits[ind1][ind2]++;
					numSteps++;
				}
			}
		}
	} // if using JPS
    else if (useMCTS || usePF) {
      //-------- MCTS Picking the Child Node ---------
      map<double, vector<int>> scoreAndDirection;
      map<vector<int>, double> directionAndScore;
      int ind1 = -1;
      int ind2 = -1;
      
      for (auto u : allDirections) {
        int u1 = u.front();
        int u2 = u.back();
        if (isValid(u1, u2) == true) {
          if (isDestination(u1, u2, dest) == true) {
            ind1 = u1;
            ind2 = u2;
            break;
          } else if (closedList[u1][u2] == false && isUnBlocked(grid, u1, u2) == true) {
    			  double scoreAll = 0.0;
    			  if (useMCTS) {
    				  double score1 = calculateHValue(u1, u2, dest) / (double)(ROW * COL);
    				  double score2 = numVisits[u1][u2] == 0 ? (double)(ROW * COL) : sqrt(2 * log(numVisits[i][j]) / double(numVisits[u1][u2]));
    				  scoreAll = score1 + score2;
    			  }
    			  else if (usePF) {
    				  double v1x = i - u1;
    				  double v1y = j - u2;
    				  double v2x = i - dest.first;
    				  double v2y = j - dest.second;
    				  double v1l = sqrt(v1x * v1x + v1y * v1y);
    				  double v2l = sqrt(v2x * v2x + v2y * v2y);
    				  scoreAll = v1l * v2l == 0.0 ? 0.0 : 1.0 + (v1x * v2x + v1y * v2y) / v1l / v2l;
    			  }
            scoreAndDirection[scoreAll] = {u1, u2};
            directionAndScore[{u1, u2}] = scoreAll;
            continue;
          } // if NOT in `closestList` and NOT an obstacle
          
          // Everything else is bad ==> assign negative score
          scoreAndDirection[-1.0] = {u1, u2};
        }
      } 
      
      if (ind1 < 0 && ind2 < 0) {
        double scoreOpt = scoreAndDirection.rbegin()->first;
        vector<vector<int>> candidates;
        for (auto u : directionAndScore)
          if (u.second - scoreOpt > -0.001 && u.second - scoreOpt < 0.001)
            candidates.push_back(u.first);
        
        if (candidates.size() == 1) {  
          ind1 = scoreAndDirection.rbegin()->second.front();
          ind2 = scoreAndDirection.rbegin()->second.back();
        } else {
          int indChosen = rand() % candidates.size();
          ind1 = candidates.at(indChosen).front();
          ind2 = candidates.at(indChosen).back();
        }
      }
      
      if (isValid(ind1, ind2) == true) 
      { 
        // If the destination cell is the same as the 
        // current successor 
        if (isDestination(ind1, ind2, dest) == true) 
        { 
          // Set the Parent of the destination cell 
          cellDetails[ind1][ind2].parent_i = i; 
          cellDetails[ind1][ind2].parent_j = j; 
          foundDest = true; 
          return tracePath(cellDetails, dest, nextPos);
        } 
        // If the successor is already on the closed 
        // list or if it is blocked, then ignore it. 
        // Else do the following 
        else if (closedList[ind1][ind2] == false && isUnBlocked(grid, ind1, ind2) == true) 
        { 
          gNew = cellDetails[i][j].g + 1.0; 
          hNew = calculateHValue (ind1, ind2, dest); 
          fNew = gNew + hNew; 
          
          // If it isn't on the open list, add it to 
          // the open list. Make the current square 
          // the parent of this square. Record the 
          // f, g, and h costs of the square cell 
          //                OR 
          // If it is on the open list already, check 
          // to see if this path to that square is better, 
          // using 'f' cost as the measure. 
          if (cellDetails[ind1][ind2].f == FLT_MAX || 
              cellDetails[ind1][ind2].f > fNew) 
          { 
            openList.insert(make_pair(fNew, 
                                       make_pair(ind1, ind2))); 
            
            // Update the details of this cell 
            cellDetails[ind1][ind2].f = fNew; 
            cellDetails[ind1][ind2].g = gNew; 
            cellDetails[ind1][ind2].h = hNew; 
            cellDetails[ind1][ind2].parent_i = i; 
            cellDetails[ind1][ind2].parent_j = j; 
            
            numVisits[ind1][ind2]++;
            numSteps++;
          } 
        } 
      }
    } // if using MCTS or PF 
	else { // if using the standard A*
      for (auto u : allDirections)
      {
        int ind1 = u.front();
        int ind2 = u.back();
        // Only process this cell if this is a valid one 
        if (isValid(ind1, ind2) == true) 
        { 
          // If the destination cell is the same as the 
          // current successor 
          if (isDestination(ind1, ind2, dest) == true) 
          { 
            // Set the Parent of the destination cell 
            cellDetails[ind1][ind2].parent_i = i; 
            cellDetails[ind1][ind2].parent_j = j; 
            foundDest = true; 
			return tracePath(cellDetails, dest, nextPos);
          } 
          // If the successor is already on the closed 
          // list or if it is blocked, then ignore it. 
          // Else do the following 
          else if (closedList[ind1][ind2] == false && 
                   isUnBlocked(grid, ind1, ind2) == true) 
          { 
            gNew = cellDetails[i][j].g + 1.0; 
            hNew = calculateHValue (ind1, ind2, dest); 
            fNew = gNew + hNew; 
            
            // If it isn't on the open list, add it to 
            // the open list. Make the current square 
            // the parent of this square. Record the 
            // f, g, and h costs of the square cell 
            //                OR 
            // If it is on the open list already, check 
            // to see if this path to that square is better, 
            // using 'f' cost as the measure. 
            if (cellDetails[ind1][ind2].f == FLT_MAX || 
                cellDetails[ind1][ind2].f > fNew) 
            { 
              openList.insert( make_pair(fNew, 
                                         make_pair(ind1, ind2))); 
              
              // Update the details of this cell 
              cellDetails[ind1][ind2].f = fNew; 
              cellDetails[ind1][ind2].g = gNew; 
              cellDetails[ind1][ind2].h = hNew; 
              cellDetails[ind1][ind2].parent_i = i; 
              cellDetails[ind1][ind2].parent_j = j; 
              
              numSteps++;
            } 
          } 
        } 
      } // Loop through all directions
    } // if NOT using MCTS
  } // while (!openList.empty()) 
  
  // When the destination cell is not found and the open 
  // list is empty, then we conclude that we failed to 
  // reach the destiantion cell. This may happen when the 
  // there is no way to destination cell (due to blockages) 
  
  return 10.0 * ROW * COL; 
} // int pathSearch(...) 


// Driver program to test above function 
// [[Rcpp::export]]
vector<int> runPathSearch(int grid0[][COL], vector<int> src0, vector<int> dest0, bool useMCTS, bool usePF, bool useJPS)
{ 
	// Create a 2D vector with user-defined sizes: https://www.geeksforgeeks.org/2d-vector-in-cpp-with-user-defined-size/
	//vector<vector<int>> grid(ROW, vector<int>(COL)); // [COL]
	int grid[ROW][COL];

	for (int i = 0; i < ROW; i++) 
		for (int j = 0; j < COL; j++) 
			grid[i][j] = grid0[i][j];

  /* Description of the Grid- 
   0--> The cell is not blocked 
   1--> The cell is blocked    */
  Pair src = make_pair(src0.front(), src0.back());
  Pair dest = make_pair(dest0.front(), dest0.back());
  
  vector<int> nextPos;
  pathSearch(grid, src, dest, useMCTS, usePF, useJPS, &nextPos);
  
  if (nextPos.empty()) {
	  vector<int> vec00 = { 0, 0 };
	  return vec00;
  }
  return nextPos;
}
