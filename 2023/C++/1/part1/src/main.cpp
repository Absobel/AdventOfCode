#include <iostream>
#include <fstream>
#include <sstream>
#include <cctype>

std::stringstream readFile(const std::string& path) {
    std::ifstream fileStream(path);

    if (!fileStream) {
        throw std::runtime_error("Could not open file: " + path);
    }

    std::stringstream buffer;
    buffer << fileStream.rdbuf();
    return buffer;
}

int main(int /*argc*/, char** argv) {
    int result = 0;

    std::stringstream input = readFile(argv[1]);
    std::string line;
    while (std::getline(input, line)) {
        int first_digit = -1, last_digit = -1;
        for (char c : line) {
            if (std::isdigit(c)) {
                if (first_digit == -1) {
                    first_digit = c - '0';
                } else {
                    last_digit = c - '0';
                }
            }
        }

        result += first_digit*10 + (last_digit == -1 ? first_digit : last_digit);
    }

    std::cout << "Result: " << result << std::endl;

    return 0;
}