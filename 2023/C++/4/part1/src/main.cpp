#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <algorithm>

std::stringstream readFile(const std::string& path) {
    std::ifstream fileStream(path);

    if (!fileStream) {
        throw std::runtime_error("Could not open file: " + path);
    }

    std::stringstream buffer;
    buffer << fileStream.rdbuf();
    return buffer;
}

std::vector<int> parseLineNumber(std::stringstream& ss) {
    std::vector<int> numbers;
    std::string token;
    while (std::getline(ss, token, ' ')) {
        try {
            numbers.push_back(std::stoi(token));
        } catch (const std::invalid_argument& e) {/* Ignore */}
    }
    return numbers;
} 

class Card {
    private:
        int id;
        std::vector<int> winning_nb;
        std::vector<int> had_nb;

    public:
        void parse(const std::string& line) {
            std::stringstream ss(line);
            std::string token;
            
            // Card id
            std::getline(ss, token, ':');
            token.erase(0, 4);
            this->id = std::stoi(token);

            // Winning numbers
            std::getline(ss, token, '|');
            std::stringstream ss2(token);
            this->winning_nb = parseLineNumber(ss2);

            // Had numbers
            std::getline(ss, token, '|');
            std::stringstream ss3(token);
            this->had_nb = parseLineNumber(ss3);
        }

        int value() {
            int value = 0;
            for (int i : this->had_nb) {
                if (std::find(this->winning_nb.begin(), this->winning_nb.end(), i) != this->winning_nb.end()) {
                    value = value == 0 ? 1 : value * 2;
                }
            }
            return value;
        }
};

int main(int /*argc*/, char** argv) {
    std::stringstream input = readFile(argv[1]);
    std::string line;

    std::vector<Card> cards;

    while (std::getline(input, line)) {
        Card card;
        card.parse(line);
        cards.push_back(card);
    }

    int total = 0;
    for (Card card : cards) {
        total += card.value();
    }

    std::cout << total << std::endl;

    return 0;
}