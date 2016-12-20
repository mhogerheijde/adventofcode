#!/usr/bin/env python3
#For parsing the data
def parse_input(line):
    line=line.rstrip().split(" ")
    if line[0] == "bot":
        line = ("give", int(line[1]), (line[5], int(line[6])), (line[10], int(line[11])))
    if line[0] == "value":
        line = ("value", int(line[5]), int(line[1]))
    return line

#Reads in the data
with open("../resources/net/hogerheijde/aoc2016/days/day10.input", "r") as input_file:
    data = map(parse_input, input_file.readlines())

print("digraph robots {")
for line in data:
    if line[0] == "give":
        print("bot{} -> {}{} [label = low];".format(line[1],line[2][0],line[2][1]))
        print("bot{} -> {}{} [label = high];".format(line[1],line[3][0],line[3][1]))
    elif line[0] == "value":
        print("input{} -> bot{};".format(line[2],line[1]))
print("}")
