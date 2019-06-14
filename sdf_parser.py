import sys
import pandas as pd

inchikeys = []
name = []
plants = []
p_diseases = []
prevline = ""

with open(sys.argv[1]) as f1:
	inchikeys.append(f1.readline().strip('\n'))
	for line in f1:
		line = line.strip('\n')
		if '$' in prevline:
			inchikeys.append(line)
		elif 'COMPOUND_NAME' in prevline:
			name.append(line)
		elif '> <Plants>' in prevline:
			plants.append(line)
		elif '> <Plants_diseases>' in prevline:
			p_diseases.append(line)
		prevline = line

results_inchi = {}
results_plants = {}

for i in range(len(plants)-1):
	elem_p = plants[i].split('|')
	elem_pd = p_diseases[i].split('|')
	results_inchi[inchikeys[i]] = elem_p
	for j in range(len(elem_p)):
		results_plants[elem_p[j]] = elem_pd[j]

print(inchikeys)


with open(sys.argv[3], 'w') as f2:
	for n in inchikeys:
		f2.write('%s\n' % n)


#df = pd.DataFrame({'InChIKey': inchikeys, 'Compound': name, 'Plants': plants, 'Plants diseases': p_diseases})

#print(df)
#df.to_csv(sys.argv[2], sep='\t')
