#include <iostream>
#include <cstdlib>
#include <ctime>
#include <vector>
#include <map>
#include <cmath>
#include <limits>
#include <set>

//*************************************************************
// Function declarations **************************************
//*************************************************************
void set_random_seed();
int randn(int n);

//*************************************************************
// Structure declarations *************************************
//*************************************************************

struct mm_code_maker{
    //DO NOT CHANGE
    void init(int i_length, int i_num){
        length = i_length;
        num = i_num;
    }
    // DO NOT CHANGE
    void generate_sequence(){
        for(int i = 0; i < length; i++){
            sequence.push_back(randn(num));
        }
    }
    // EDIT BUT DO NOT CHANGE INTERFACE (NAME, PARAMETERS, VOID RETURN)
    void give_feedback(const std::vector<int>& attempt, int& black_hits, int& white_hits){
        std::vector<int> tmp_attempt = attempt;
        std::vector<int> tmp_sequence = sequence;
        black_hits = 0;
        white_hits = 0;
        // Update black hits
        for (int i = 0; i < length; i++) {
            if (tmp_sequence[i] == tmp_attempt[i]) {
              black_hits++;
              tmp_attempt[i] = -1;
              tmp_sequence[i] = -1;
            }
        }
        // Update white hits for remaining
        for (int i = 0; i < length; i++) { // For each element in tmp_attempt
            bool found = false;
            for (int j = 0; (j < length) && !found && (tmp_attempt[i] > -1); j++) { // For each element in sequence
                if((tmp_attempt[i] == tmp_sequence[j]) && (i != j) && (tmp_sequence[j] > -1)) {
                  white_hits++;
                  tmp_sequence[j] = -1;
                  found = true;
                }
            }
        }
    }
    // DO NOT CHANGE OR ADD ANY MEMBER DATA
    std::vector<int> sequence;

    int length;
    int num;
};

struct mm_solver{
    // EDITABLE BUT DO NOT CHANGE FUNCTION INTERFACE
    void init(int i_length, int i_num){
        length = i_length; // These are member variables within the structure
        num = i_num;
        unsigned long long int set_size;
        if (length < 16 || num < 16) {
          set_size = pow(num, length);
        } else {
          set_size = elimination_limit+1; // Manually bypass the calculation
        }
        if ((set_size < minimax_limit || (length == 8 && num == 3)) && !(length == 3 && num > 14)) {
          knuth_minimax_mode = true;
          std::vector<int> temp(length, 0); // Create vector with length number of zeroes
          generate_possibilities(temp, 0, length);
          remaining = possibilities;
        } else if (set_size < elimination_limit) {
          elimination_mode = true;
          std::vector<int> temp(length, 0); // Create vector with length number of zeroes
          generate_possibilities(temp, 0, length);
          remaining = possibilities;
        } else {
          brute_force_mode = true;
          desired_length = log(elimination_limit)/log(num);
        }
    }

    void generate_possibilities(std::vector<int>& current, int position, int len) { // Useful up to 8x8
        // Base case
        if (position >= len) { // Recursive call has generated possibilities for the length of the code
            possibilities.push_back(current); // Each combination gets added to the master list of possibilies
        }
        else {
          for (int i = 0; i < num; i++) { // For loop iterates through 0 - () for each position
              current[position] = i; // Sets the elements in the current position to the current value
              // Generate possibilities for subsequent positions
              generate_possibilities(current, position+1, len);
              // Recursive call increments the position to be changed by the for loop from position 0 - length
              // Value of i is held in current position while the recursive call cycles through all combinations for subsequent positions
          }
        }
    }

    // EDITABLE BUT DO NOT CHANGE FUNCTION INTERFACE
    void create_attempt(std::vector<int>& attempt){
      // First attempt is half 1 half 2 e.g. 1122, if bruteforce or num = 1 then all 0s
      if (first_attempt) {
        if ((knuth_minimax_mode || elimination_mode) && num > 1) {
          int mid = length/2;
          for (int i = 0; i < mid; i++) {
            attempt.push_back(0);
          }
          for (int i = 0; i < length-mid; i++) {
            attempt.push_back(1);
          }
        } else {
          for (int i = 0; i < length; i++) {
            attempt.push_back(0);
          }
          return;
        }
        first_attempt = false;
      } else if (knuth_minimax_mode) {  // For non-first attempts
        generate_next_attempts();
        attempt = next_attempt;
      } else if (elimination_mode) {
        attempt = remaining[0];
      } else if (brute_force_mode) {
        attempt = next_attempt;
      } else if (hybrid_mode) {
        for (int i = 0; i < back_partition.size(); i++) {
          next_attempt.pop_back();
        }
        for (int i = 0; i < back_partition.size(); i++) {
          next_attempt.push_back(back_partition[i]);
        }
        attempt = next_attempt;
      }
      searchAndDelete(possibilities, attempt);
      searchAndDelete(remaining, attempt);
    }

    // EDITABLE BUT DO NOT CHANGE FUNCTION INTERFACE
    void learn(std::vector<int>& attempt, int black_hits, int white_hits){
      if (knuth_minimax_mode || elimination_mode) {
        // Step 1. Based on the feedback, eliminate other combinations that do not give the same hit score as the current attempt
        int new_w, new_b;
        std::vector<std::vector<int>> tmp;
        for (int i = 0; i < remaining.size(); i++) {
          givefb_solver(attempt, remaining[i], new_b, new_w, length);
          if ((black_hits == new_b) && (white_hits == new_w)) {
            tmp.push_back(remaining[i]);
          }
        }
        remaining = tmp;
      } else if (brute_force_mode && (black_hits < length)) {
          if (current_position < (length-desired_length)) {
            brute_force(attempt, black_hits, white_hits);
            next_attempt = attempt;
          } else {
              // Switch to elimination mode
              hybrid_mode = true;
              brute_force_mode = false;
              first_black = length-desired_length;
              next_attempt = attempt;
              // Elimination initialization
              std::vector<int> temp(desired_length, 0);
              generate_possibilities(temp, 0, desired_length);
              remaining = possibilities;
              for (int i = 0; i < desired_length; i++) {
                back_partition.push_back(0);
              }
            }
        }
      if (hybrid_mode) {
        /* Brute-elim hybrid method
        1. For the given num, calculate the desired length (D) such that the num^D is below the threshold for elimination
        2. Use brute force algorithm to reduce the set size to num^D, ie D unsolved positions
        3. Separate the first length-D elements and the last D Elements into separate containers
        4. Use elimination algorithm on the last D Elements
        5. To create next attempt, join first length-D elements to the new attempt
        */
        int diff_black = black_hits - first_black;
        int new_w, new_b;
        std::vector<std::vector<int>> tmp;
        for (int i = 0; i < remaining.size(); i++) {
          givefb_solver(back_partition, remaining[i], new_b, new_w, desired_length);
          if ((diff_black == new_b) && (white_hits == new_w)) {
            tmp.push_back(remaining[i]);
          }
        }
        remaining = tmp;
        back_partition = remaining[0];
      }
    }

    /* Knuth's minimax algorithm
    1. For each guess in possibilities not just those in S, calculate how many possibilities in S
    would be eliminated for each possible colored/white peg score.
    2. Create map<hit_scores:hit_count> for each possiblity
    3. Choose highest number of elimination_modes and map<hitcount:combination>
    4. Choose minimum from map<combination:hitcount> (cannot have duplicate keys)
    5. Add those combinations to a vector<next_attempts>
    6. Choose the lowest combination
    */
    void generate_next_attempts() {
      // Step 1. For each guess in possibilities not just those in S, calculate how many possibilities in S
      //     would be eliminated for each possible colored/white peg score.
      int new_w, new_b;
      for (int combination = 0; combination < possibilities.size(); combination++) {
        for (int rem_combination = 0; rem_combination < remaining.size(); rem_combination++) {
          givefb_solver(possibilities[combination], remaining[rem_combination], new_b, new_w, length);
          std::vector<int> hit_score{new_b, new_w};
          // Step 2. Create map<hit_scores:hit_count> for each possiblity
          if (score_to_hitcount.count(hit_score) > 0) { // If hit_score exists in hitcount_to_combinations
            score_to_hitcount.at(hit_score)++;
          } else {
            score_to_hitcount.emplace(hit_score, 1); // Create new key value pair
          }
        }
        // Step 3. Choose highest number of elimination_modes and map<combination:hit_count>
        combinations_to_hitcount.emplace(possibilities[combination], getMaximum(score_to_hitcount));
        score_to_hitcount.clear();
      }
      // Step 4. Choose minimum from map<combination:hitcount> (cannot have duplicate keys)
      int min = getMinimum(combinations_to_hitcount);
      // Step 5. Add those combinations to a vector<next_attempts>
      for (auto i : combinations_to_hitcount) {
        if (min == i.second) {
          next_attempts.push_back(i.first);
        }
      }
      // Step 6. Choose the lowest combination
      bool ready = false;
      // Trying to find a next attempt in remaining
      for (int i = 0; i < next_attempts.size() && !ready; i++) {
        if (vv_search(next_attempts[i], remaining)) {
          next_attempt = next_attempts[i];
          ready = true;
        }
      }
      // Trying to find a next attempt in all possibilities
      for (int i = 0; i < next_attempts.size() && !ready; i++) {
        if (vv_search(next_attempts[i], possibilities)) {
          next_attempt = next_attempts[i];
          ready = true;
        }
      }
      if (!ready) {
        next_attempt = next_attempts[0];
      }
      next_attempts.clear();
      combinations_to_hitcount.clear();
    }

    // Checks if a vector resides in a vector of vectors
    bool vv_search(const std::vector<int>& v, const std::vector<std::vector<int>>& vector_of_vectors) {
      int i = 0;
      for (int i = 0; i < vector_of_vectors.size(); i++) {
        if (vector_of_vectors[i] == v) {
          return true;
        }
      }
      return false;
    }

    // Modified verison of give_feedback: generates [B W] pair by reference as output
    void givefb_solver(const std::vector<int>& attempt, const std::vector<int>& base_attempt, int& black_hits, int& white_hits, const int& len){
      std::vector<int> tmp_attempt = base_attempt;
      std::vector<int> tmp_sequence = attempt;
      black_hits=0;
      white_hits=0;
      // Update black hits
      for (int i = 0; i < len; i++) {
          if (tmp_sequence[i] == tmp_attempt[i]) {
            black_hits++;
            tmp_attempt[i] = -1;
            tmp_sequence[i] = -1;
          }
      }
      // Update white hits for remaining
      for (int i = 0; i < len; i++) { // For each element in tmp_attempt
          bool found = false;
          for (int j = 0; (j < len) && !found && (tmp_attempt[i] > -1); j++) { // For each element in sequence
            if((tmp_attempt[i] == tmp_sequence[j]) && (i != j) && (tmp_sequence[j] > -1)) {
              white_hits++;
              tmp_sequence[j] = -1;
              found = true;
            }
          }
      }
    }

    // Returns the maximum value in a map
    int getMaximum(const std::map<std::vector<int>, int>& map) {
      int max = 0;
      for (auto i : map) {
        if (i.second > max) { // Found new max
          max = i.second;
        }
      }
      return max;
    }

    // Returns the minimum value in a map
    int getMinimum(const std::map<std::vector<int>, int>& map) {
      int min = std::numeric_limits<int>::max();
      for (auto i : map) {
        if (i.second < min) { // Found new min
          min = i.second;
        }
      }
      return min;
    }

    // Removes a vector from a vector of vectors (may not need)
    void searchAndDelete(std::vector<std::vector<int>>& set, std::vector<int>& to_remove) {
      bool found = false;
      for (int i = 0; i < set.size() && !found; i++) {
        if (set[i] == to_remove) {
          set.erase(set.begin()+i);
          found = true;
        }
      }
    }

    /* Brute force method

    1. Compare last_black with black hits
    1.1 If white found do nothing
    2. (CASE 1) If no change increment same position
    3. (CASE 2) If current<last then revert to last attempt and increment position
    4. (CASE 3) If current>last then keep current attempt and incrememnt position

    */
    void brute_force(std::vector<int>& attempt, const int& black_hits, const int& white_hits) {
      if (first_attempt) {
        last_black = black_hits;
        first_attempt = false;
      }
      int change = black_hits - last_black;
        // CASE 1
      if (!change) { // If no change to black and white hits
        last_attempt = attempt; // Update last attempt
        if (!white_hits) {
          not_in_sequence.emplace(attempt[current_position]);  // Add to list of values not in sequence
        }
        while (not_in_sequence.count(attempt[current_position]+1)) { // If next value not in sequence
          attempt[current_position]++; // Increment at current position
        }
        attempt[current_position]++; // Increment at current position
      }
      // CASE 2
      else if (change < 0) { // If black hits decrease
        attempt = last_attempt; // Revert attempt
        current_position++; // Increment position
        if (current_position == length-desired_length) {
          return;
        }
        attempt[current_position]++; // Increment at new position
      }
      // CASE 3
      else { // black_hits > last_black
        last_black = black_hits; // Update last black
        last_attempt = attempt; // Update last attempt
        current_position++; // Increment position
        if (current_position == length-desired_length) {
          return;
        }
        attempt[current_position]++; // Increment at new position
      }
    }

    // YOU MAY ADD OTHER MEMBER FUNCTIONS AND MEMBER DATA AS NEEDED
    // (KEEP IN MIND THE DISTINCTION BETWEEN MEMBER FUNCTION VARIABLE AND MEMBER DATA OF THE STRUCT)
    int length; // Length of sequence
    int num; // Number of 'colours'
    int minimax_limit = 6000;
    int elimination_limit = 4000000;
    int desired_length; // For brute-elim hybrid
    int last_black = 0; // For brute_force
    int first_black; // For brute-elim hybrid
    int current_position = 0; // For brute_force
    bool first_attempt = true;
    std::vector<int> next_attempt;
    std::vector<int> last_attempt; // For brute_force
    std::vector<int> back_partition; // For brute-elim hybrid
    std::vector<std::vector<int>> next_attempts; // Contains the next attempts based on learn
    std::vector<std::vector<int>> possibilities; // Master set containing all possibilities
    std::vector<std::vector<int>> remaining; // Set containing all remaining possibilities after elimination_mode
    std::map<std::vector<int>, int> combinations_to_hitcount; // Maps each combination to the maximum value
    std::map<std::vector<int>, int> score_to_hitcount; // Maps the BWscore to potential elimination_modes
    std::set<int> not_in_sequence;
    // Modes
    bool knuth_minimax_mode = false;
    bool elimination_mode = false;
    bool brute_force_mode = false;
    bool hybrid_mode = false;
};

//*************************************************************
// Main Function **********************************************
//*************************************************************
int main(){
    // THIS IS THE DEFAULT MAIN FUNCTION
    set_random_seed();

    int length, num;
    std::cout << "enter length of sequence and number of possible values:" << std::endl;
    std::cin >> length >> num;

    mm_solver solver; // Create a solver
    solver.init(length, num); // Initialize the solver

    mm_code_maker maker; // Create code maker
    maker.init(length, num); // Initialize code maker
    maker.generate_sequence(); // Generate a sequence

    int black_hits=0, white_hits=0; // Initialize default values
    int attempts_limit = 5000;
    int attempts = 0;
    while((black_hits < length) && (attempts < attempts_limit)){ // Winning condition is if #black_hits = length (of sequence) or limit reached
        std::vector<int> attempt;
        solver.create_attempt(attempt); // Solver creates an attempt
        maker.give_feedback(attempt, black_hits, white_hits); // Maker returns feedback
        // Display
        std::cout << "attempt: " << std::endl;
        for(int i = 0; i < attempt.size(); i++){
            std::cout << attempt[i] << " ";
        }
        std::cout << std::endl;
        std::cout << "black pegs: " << black_hits << " " << " white pegs: " << white_hits << std::endl;

        solver.learn(attempt, black_hits, white_hits); // Solver learns from previous attempt
        attempts++;
    }

    if(black_hits == length) { // Winning condition
        std::cout << "the solver has found the sequence in " << attempts << " attempts" << std::endl;
    }
    else { // Game over condition
        std::cout << "after " << attempts << " attempts still no solution" << std::endl;
    }
    std::cout << "the sequence generated by the code maker was:" << std::endl;
    for(int i = 0; i < maker.sequence.size(); i++){
        std::cout << maker.sequence[i] << " ";
    }
    std::cout << std::endl;
    return 0;
}

//*************************************************************
// Function definitions ***************************************
//*************************************************************
void set_random_seed(){
  // Can be edited for better results
    srand(time(0));
}

int randn(int n){
    return std::rand() % n;
}
