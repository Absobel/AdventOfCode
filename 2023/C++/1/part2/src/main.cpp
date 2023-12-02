#include <iostream>
#include <fstream>
#include <sstream>
#include <optional>
#include <unordered_map>

std::stringstream readFile(const std::string& path) {
    std::ifstream fileStream(path);

    if (!fileStream) {
        throw std::runtime_error("Could not open file: " + path);
    }

    std::stringstream buffer;
    buffer << fileStream.rdbuf();
    return buffer;
}

std::optional<int> parse(std::string& line) {
    char c = line.front();
    line.erase(0, 1);

    if (c >= '0' && c <= '9') {
        return c - '0';
    } else if (c == 'o' && line.substr(0, 2) == "ne") {
        return 1;
    } else if (c == 't' && line.substr(0, 2) == "wo") {
        return 2;
    } else if (c == 't' && line.substr(0, 4) == "hree") {
        return 3;
    } else if (c == 'f' && line.substr(0, 3) == "our") {
        return 4;
    } else if (c == 'f' && line.substr(0, 3) == "ive") {
        return 5;
    } else if (c == 's' && line.substr(0, 2) == "ix") {
        return 6;
    } else if (c == 's' && line.substr(0, 4) == "even") {
        return 7;
    } else if (c == 'e' && line.substr(0, 4) == "ight") {
        return 8;
    } else if (c == 'n' && line.substr(0, 3) == "ine") {
        return 9;
    } else {
        return std::nullopt;
    }
}

int main(int /*argc*/, char** argv) {
    int result = 0;

    std::stringstream input = readFile(argv[1]);
    std::string line;
    while (std::getline(input, line)) {
        int first_digit = -1, last_digit = -1;
        while (line.size() > 0) {
            std::optional<int> digit = parse(line);
            if (digit != std::nullopt) {
                if (first_digit == -1) {
                    first_digit = digit.value();
                }
                last_digit = digit.value();
            }
        }
        result += first_digit*10 + (last_digit == -1 ? first_digit : last_digit);
    }

    std::cout << "Result: " << result << std::endl;

    return 0;
}