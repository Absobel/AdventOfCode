#include <iostream>
#include <fstream>
#include <sstream>
#include <optional>
#include <unordered_map>
#include <vector>

std::stringstream readFile(const std::string& path) {
    std::ifstream fileStream(path);

    if (!fileStream) {
        throw std::runtime_error("Could not open file: " + path);
    }

    std::stringstream buffer;
    buffer << fileStream.rdbuf();
    return buffer;
}

void trim_left(std::string& str) {
    while (std::isspace(str.front())) {
        str.erase(0, 1);
    }
}

std::vector<std::string> split(const std::string& str, char delim) {
    std::vector<std::string> tokens;
    std::string token;
    std::istringstream tokenStream(str);
    while (std::getline(tokenStream, token, delim)) {
        tokens.push_back(token);
    }

    return tokens;
}

class Tirage {
    private:
        int nb_red = 0;
        int nb_green = 0;
        int nb_blue = 0;

    public:
        // Part du principe que la ligne match ^\s*[0-9]+.*;$
        void parseTirage(std::string& line) {
            trim_left(line);

            while (!line.empty()) {
                int nb = 0;
                while (std::isdigit(line.front())) {
                    nb = nb * 10 + (line.front() - '0');
                    line.erase(0, 1);
                }

                line.erase(0, 1);

                switch (line.front()) {
                case 'r':
                    this->nb_red = nb;
                    line.erase(0, 4);
                    break;
                case 'g':
                    this->nb_green = nb;
                    line.erase(0, 6);
                    break;
                case 'b':
                    this->nb_blue = nb;
                    line.erase(0, 5);
                    break;
                default:
                    throw std::runtime_error("Invalid tirage");
                }

                trim_left(line);
            }
        }

        bool is_valid(int max_red, int max_green, int max_blue) {
            return this->nb_red <= max_red && this->nb_green <= max_green && this->nb_blue <= max_blue;
        }
};

class Game {
    private:
        int id_game = 0;
        std::vector<Tirage> tirages;

    public:
        void parseGame(std::string& line) {
            line.erase(0, 5);
            while (std::isdigit(line.front())) {
                this->id_game = this->id_game * 10 + (line.front() - '0');
                line.erase(0, 1);
            }
            line.erase(0, 2);

            std::vector<std::string> tirages_str = split(line, ';');
            for (std::string& tirage_str : tirages_str) {
                Tirage tirage;
                tirage.parseTirage(tirage_str);
                this->tirages.push_back(tirage);
            }
        }

        bool is_valid(int max_red, int max_green, int max_blue) {
            for (Tirage& tirage : this->tirages) {
                if (!tirage.is_valid(max_red, max_green, max_blue)) {
                    return false;
                }
            }
            return true;
        }

        int get_id_game() {
            return this->id_game;
        }
};

class Games {
    private:
        std::vector<Game> games;
        int max_red;
        int max_green;
        int max_blue;

    public:
        Games(int max_red, int max_green, int max_blue) : max_red(max_red), max_green(max_green), max_blue(max_blue) {}

        void push_game_str(std::string& line) {
            Game game;
            game.parseGame(line);
            this->games.push_back(game);
        }

        std::vector<int> valid_games_id() {
            std::vector<int> valid_games;
            for (Game game : this->games) {
                if (game.is_valid(this->max_red, this->max_green, this->max_blue)) {
                    valid_games.push_back(game.get_id_game());
                }                
            }
            return valid_games;
        }        
};

const int MAX_RED = 12;
const int MAX_GREEN = 13;
const int MAX_BLUE = 14;

int main(int /*argc*/, char** argv) {
    std::stringstream input = readFile(argv[1]);
    std::string line;
    
    Games games(MAX_RED, MAX_GREEN, MAX_BLUE);

    while (std::getline(input, line)) {
        games.push_game_str(line);
    }

    std::vector<int> valid_games = games.valid_games_id();
    int sum = 0;
    for (int id : valid_games) {
        sum += id;
    }

    std::cout << sum << std::endl;

    return 0;
}