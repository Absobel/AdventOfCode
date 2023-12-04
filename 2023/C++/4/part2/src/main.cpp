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
        std::vector<int> winning_nb;
        std::vector<int> had_nb;

    public:
        void parse(const std::string& line) {
            std::stringstream ss(line);
            std::string token;
            
            // Useless things
            std::getline(ss, token, ':');

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
                    value++;
                }
            }
            return value;
        }
};

int main(int /*argc*/, char** argv) {
    std::stringstream input = readFile(argv[1]);
    std::string line;

    std::vector<std::tuple<Card, int>> cards;

    // Parse cards
    while (std::getline(input, line)) {
        Card card;
        card.parse(line);
        cards.push_back(std::make_tuple(card, 1));
    }

    // Compute the number of cards for each card
    for (int i = 0; i < (int)cards.size(); i++) {
        int value = std::get<0>(cards[i]).value();
        int nb_copies = std::get<1>(cards[i]);
        for (int j = i + 1; j < std::min(i + value+1, (int)cards.size()); j++) {
            std::get<1>(cards[j]) += nb_copies;
        }
    }

    int total = 0;
    for (auto card : cards) {
        total += std::get<1>(card);
    }

    std::cout << total << std::endl;

    return 0;
}