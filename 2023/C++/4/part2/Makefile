# Compiler settings - Can be customized.
CXX = g++
CXXFLAGS = -std=c++20 -Wall -Wextra

# Project files
SRCS = $(wildcard src/*.cpp)
OBJS = $(SRCS:src/%.cpp=target/%.o)
EXEC = part1

# Specify the target
all: $(EXEC)

# Linking
$(EXEC): $(OBJS)
	$(CXX) $(CXXFLAGS) -o target/$(EXEC) $(OBJS)

# Compiling
target/%.o: src/%.cpp
	mkdir -p target
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Run with full input
full: all
	./target/$(EXEC) $(realpath input/full.txt)

# Run with test input
test: all
	./target/$(EXEC) $(realpath input/test.txt)

# Clean up
clean:
	rm -f target/$(EXEC) $(OBJS)

.PHONY: all clean full test
