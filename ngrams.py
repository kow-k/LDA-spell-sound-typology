## generator fuctions for 2-gram, 3-gram
## developed by Kow Kuroda (kow.kuroda@gmail.com)
## 2024/02/20: added alises

def gen_unigrams (L: list, sep = r"", check = False):
	"""
	returns the 1-gram of the items in list L with separator regex
	"""
	import re
	#U = [ list(filter(lambda x: len(x) > 0, y)) for y in [ re.split(sep, z) for z in L ] ]
	#U = [ y for y in [ re.split(sep, x) for x in L ] if len(y) > 0 ]
	U = [ ]
	for x in L:
		seg = [ y for y in re.split(sep, x) if len(y) > 0 ]
		if check:
			print(seg)
		U.append(seg)
	return (U)
## alises
gen_1grams = gen_unigrams

## bigram
def gen_bigrams (L: list, sep = r"", joint = "", check = False):
	import re
	n = 2
	B = [ ]
	for x in L:
		if check:
			print(x)
		seg = [ s for s in re.split(sep, x) if len(s) > 0 ]
		size = len(seg)
		if size < n:
			B.append(joint.join(seg))
		else:
			C = [ ]
			for i in range(size - n + 1):
				y = seg[ i : i + n ]
				if check:
					print(y)
				if len(y) == n:
					C.append(joint.join(y))
			B.append(C)
	return (B)

## alises
gen_2grams = gen_bigrams

### trigram
def gen_trigrams (L: list, sep = r"", joint = "", check = False):
	import re
	n = 3
	T = [ ]
	for x in L:
		if check:
			print(x)
		seg = [ s for s in re.split(sep, x) if len(s) > 0 ]
		size = len(seg)
		if size < n:
			T.append(joint.join(seg))
		else:
			C = [ ]
			for i in range(size - n + 1):
				y = seg[ i : i + n ]
				if check:
					print(y)
				if len(y) == n:
					C.append(joint.join(y))
			T.append(C)
	return (T)

## alises
gen_3grams = gen_trigrams

### end of script