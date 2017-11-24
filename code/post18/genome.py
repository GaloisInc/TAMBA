import argparse
import random

counter = 0

class Person:
    def __init__(self, name, dad=None, mom=None):
        self.name = name + str(counter)
        self.dad  = dad
        self.mom  = mom
        global counter
        counter += 1

    def __str__(self):
        return str(self.name)

def belief_stmt(person):
  l1 = "  int "+str(person)+"_0 = uniform 0 1;\n"
  l2 = "  int "+str(person)+"_1 = uniform 0 1;\n"
  return l1 + l2

def secret_stmt(person):
  l1 = "  int "+str(person)+"_0 = " + str(random.randint(0,1)) + ";\n"
  l2 = "  int "+str(person)+"_1 = " + str(random.randint(0,1)) + ";\n"
  return l1 + l2

parser = argparse.ArgumentParser(description='Generate a genome prob file.')
parser.add_argument('num_roots', type=int,
                   help='Number of roots (ancestors without modeled parents) to include.')
parser.add_argument('num_derived', type=int,
                   help='Number of descendants in the tree.')

args = parser.parse_args()
print(args.num_roots)
print(args.num_derived)

random.seed()

no_kids = list()

for x in range(0,args.num_roots):
    no_kids += [Person('root')]

roots = list(no_kids)
all_people = list(no_kids)

for x in range(0,args.num_derived):
    random.shuffle(no_kids)
    if len(no_kids) == 0:
        break;
    parent1 = no_kids.pop()
    if len(no_kids) == 0:
        break;
    parent2 = no_kids.pop()
    child = Person('kid',parent1,parent2)
    no_kids.append(child)
    all_people.append(child)

print("Number still without kids: " + str(len(no_kids)))
print("Number of nodes: " + str(len(all_people)))

tree_people = [x for x in all_people if x not in no_kids]

filename = 'genome_'+str(len(tree_people))+'.prob'
print("Writing "+filename)

f = open(filename,'w')

f.write("secret:\n")

for p in roots:
    f.write(secret_stmt(p))

f.write("\nbelief:\n")

for p in roots:
    f.write(belief_stmt(p))

f.write("\nquerydef exact_sum -> result :\n")
f.write("  int result = 0;\n")

for p in tree_people:
    if p.mom == None:
        f.write("  result = result + " + p.name + "_0 + " + p.name + "_1;\n")
    else:
        template = """
  int {child};
  pif 1:1 then
    {child} = {parent1};
  else
    {child} = {parent2};
  endpif;\n"""
        f.write(template.format(child=str(p)+"_0", parent1=str(p.mom)+"_0", parent2=str(p.dad)+"_0"))
        f.write(template.format(child=str(p)+"_1", parent1=str(p.mom)+"_1", parent2=str(p.dad)+"_1"))
        f.write("  result = result + "+p.name+"_0 + "+p.name+"_1;\n")

f.write("\nquery exact_sum:\n")
f.write("  skip;\n")
f.close()
