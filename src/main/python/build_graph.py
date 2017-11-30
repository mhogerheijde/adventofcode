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
def buildGraph():
    with open("../resources/net/hogerheijde/aoc2016/days/day10.input", "r") as input_file:
        data = map(parse_input, input_file.readlines())

    graph = "digraph robots {\n"
    for line in data:
        if line[0] == "give":
            graph = graph  + "  \"Bot({})\" -> \"{}({})\" [label = low];\n".format(line[1],line[2][0].title(),line[2][1])
            graph = graph  + "  \"Bot({})\" -> \"{}({})\" [label = high];\n".format(line[1],line[3][0].title(),line[3][1])
        elif line[0] == "value":
            graph = graph + "  \"Microchip({})\" [shape = box, color = blue];\n".format(line[2])
            graph = graph  + "  \"Microchip({})\" -> \"Bot({})\";\n".format(line[2],line[1])
    graph = graph  + "}\n"

    return graph

with open("./graph.dot", "w") as output_file:
    output_file.write(buildGraph())

