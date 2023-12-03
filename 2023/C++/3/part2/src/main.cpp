#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <algorithm>
#include <optional>

std::stringstream readFile(const std::string& path) {
    std::ifstream fileStream(path);

    if (!fileStream) {
        throw std::runtime_error("Could not open file: " + path);
    }

    std::stringstream buffer;
    buffer << fileStream.rdbuf();
    return buffer;
}

// Can only be called on the side of the string so isok
std::vector<int> neightbour(int idx, int length_line) {
    std::vector<int> voisins;
    voisins.push_back(idx - length_line - 1); // up left
    voisins.push_back(idx - length_line);     // up
    voisins.push_back(idx - length_line + 1); // up right
    voisins.push_back(idx - 1);               // left
    voisins.push_back(idx + 1);               // right
    voisins.push_back(idx + length_line - 1); // down left
    voisins.push_back(idx + length_line);     // down
    voisins.push_back(idx + length_line + 1); // down right
    return voisins;
}

// returns the number from the idx of one of its digits
int get_number(const std::string& input, int idx) {
    std::string number_str;
    int i = idx;
    while (isdigit(input[i])) {
        number_str.insert(number_str.begin(), input[i]);
        i--;
    }
    i = idx + 1;
    while (isdigit(input[i])) {
        number_str.push_back(input[i]);
        i++;
    }
    return std::stoi(number_str);
}

// returns the numbers around the idx (can't be called on the "side" of the string)
std::vector<int> get_numbers_around(const std::string& input, int idx, int length) {
    std::vector<int> voisins = neightbour(idx, length);
    std::vector<int> numbers;

    for (int idx : voisins) {
        if (isdigit(input[idx])) {
            int number = get_number(input, idx);
            if (std::find(numbers.begin(), numbers.end(), number) == numbers.end()) {
                numbers.push_back(number);
            }
        }
    }

    return numbers;
}

const std::vector<char> SYMBOLS = {'*'};

int main(int /*argc*/, char** argv) {
    std::stringstream input = readFile(argv[1]);
    std::string input_str = input.str();

    int length_line = input_str.find('\n')+1;
    int sum = 0;

    for (size_t i = 0; i < input_str.length(); i++) {
        char c = input_str[i];
        if (std::find(SYMBOLS.begin(), SYMBOLS.end(), c) != SYMBOLS.end()) {
            std::vector<int> numbers = get_numbers_around(input_str, i, length_line);
            if (numbers.size() > 1) {
                int gear_ration = 1;
                for (int number : numbers) {
                    gear_ration *= number;
                }
                sum += gear_ration;
            }
        }
    }

    std::cout << sum << std::endl;

    return 0;
}