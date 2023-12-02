# Compiler settings - Can be customized.
CXX = g++
CXXFLAGS = -std=c++20 -Wall -Wextra

# Project files
SRCS = $(wildcard src/*.cpp)
OBJS = $(SRCS:src/%.cpp=target/%.o)
EXEC = part2

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
run_full: all
	./target/$(EXEC) $(realpath input/full.txt)

# Run with test input
run_test: all
	./target/$(EXEC) $(realpath input/test.txt)

# Clean up
clean:
	rm -f target/$(EXEC) $(OBJS)

.PHONY: all clean run_full run_test
