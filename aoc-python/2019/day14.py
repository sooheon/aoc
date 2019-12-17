from math import ceil
import re

reactions = {}
with open('day14.in') as file:
    for line in file:
        *inputs, (n, output) = re.findall(r'(\d+) (\w+)', line)
        stoichiometry = {input_chem: int(n) for n, input_chem in inputs}
        stoichiometry[output] = -int(n)
        reactions[output] = stoichiometry

output_chemicals = reactions.keys()
all_chemicals = output_chemicals | {'ORE'}


def minimum_ore(target_fuel=1):
    needed = dict.fromkeys(all_chemicals, 0)
    needed['FUEL'] = target_fuel

    while any(needed[(output := chem)] > 0 for chem in output_chemicals):
        stoichiometry = reactions[output]
        multiplier = ceil(needed[output] / -stoichiometry[output])
        for chemical, coefficient in stoichiometry.items():
            needed[chemical] += multiplier * coefficient

    return needed['ORE']


def maximum_fuel(target_ore):
    fuel = 1
    while (ore := minimum_ore(fuel + 1)) <= target_ore:
        guess = (fuel + 1) * target_ore // ore
        fuel = max(guess, fuel + 1)
    return fuel


print('Part 1:', minimum_ore(target_fuel=1))
print('Part 2:', maximum_fuel(target_ore=1_000_000_000_000))
