#include <iostream>
#include <fstream>
#include <sstream>

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
    std::stringstream input = readFile(argv[1]);
    std::string line;
    while (std::getline(input, line)) {
        
    }

    return 0;
}