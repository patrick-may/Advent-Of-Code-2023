#include <cmath>
#include <deque>
#include <iostream>
#include <set>
#include <string>
#include <vector>
using namespace std;

int readNum(const vector<string> &board, pair<int, int> start) {
  string num = "";
  while (start.second < board[0].size() &&
         isdigit(board[start.first][start.second])) {
    num += board[start.first][start.second];
    start.second += 1;
  }

  if (num.length() == 0) {
    return 0;
  }
  return stoi(num);
}

bool isEngine(const vector<string> &board, set<pair<int, int>> &visitedDigits,
              pair<int, int> start) {
  deque<pair<int, int>> to_visit = {start};
  while (!to_visit.empty()) {
    auto curr = to_visit[0];
    to_visit.pop_front();
    char currval = board[curr.first][curr.second];
    if (!isdigit(currval) && currval != '.') {
      return true;
    }

    if (isdigit(currval)) {
      visitedDigits.insert(curr);
      vector<pair<int, int>> shifts = {{-1, 0}, {1, 0},  {0, -1},  {0, 1},
                                       {1, 1},  {1, -1}, {-1, -1}, {-1, 1}};

      for (pair<int, int> s : shifts) {
        pair<int, int> neighbor = {curr.first + s.first,
                                   curr.second + s.second};
        if (neighbor.first >= 0 && neighbor.first < board.size() &&
            neighbor.second >= 0 && neighbor.second < board[0].size()) {
          if (!visitedDigits.count(neighbor)) {
            to_visit.push_back(neighbor);
          }
        }
      }
    }
  }

  return false;
}
int main() {
  vector<string> board;
  string line;
  while (cin >> line) {
    board.push_back(line);
  }

  set<pair<int, int>> visited;
  long long soln = 0;
  for (int i = 0; i < board.size(); ++i) {
    for (int j = 0; j < board[0].size(); ++j) {
      pair<int, int> next = {i, j};
      cout << "visiting " << i << " " << j << "\n";
      if (!visited.count(next) && isEngine(board, visited, next)) {
        int v = readNum(board, next);
        cout << "FOUND!; adding " << v << "\n";
        soln += v;
      }
    }
  }

  cout << soln << endl;
}
