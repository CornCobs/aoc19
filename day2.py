with open(r"C:\Users\joash\Desktop\CS\haskell\AOC19\aoc\day2.txt", "r") as f:
  program = [ int(i) for i in f.read().split(",") ]
  
pos = 0
while program[pos] != 99:
  print("currently on pos:", pos, "opcode is: ", program[pos])
  if program[pos] == 1:
    program[program[pos+3]] = program[program[pos+1]] + program[program[pos+2]]
  else:
    program[program[pos+3]] = program[program[pos+1]] * program[program[pos+2]]
  pos += 4
  
print(program[0])