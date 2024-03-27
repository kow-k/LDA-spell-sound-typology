## generator fuctions for 2-gram, 3-gram
## developed by Kow Kuroda (kow.kuroda@gmail.com)
## 2024/02/20: added alises

def str_gen_unigrams (text: str, sep = r"", check = False):
	"""
	returns a list of unigrams from elements from a given string by splitting it by <sep>
	"""
	import re
	seg = [ x for x in re.split(sep, text) if len(x) > 0 ]
	return (seg)

## alises
str_gen1grams = str_gen_unigrams

def str_gen_bigrams (text: str, sep = r"", joint = "", check = False):
	"""
	returns a list of bigrams from elements from a given string by splitting it by <sep>
	"""
	import re
	n = 2
	if check:
		print(text)
	B = [ ]
	segs = [ seg for seg in re.split(sep, text) if len(seg) > 0 ]
	size = len(segs)
	if size < n:
		B.append(joint.join(segs))
	else:
		C = [ ]
		for i in range(size - n + 1):
			y = segs[ i : i + n ]
			if check:
				print(y)
			if len(y) == n:
				C.append(joint.join(y))
		B.append(C)
	#
	return (B[0]) # X[0] is in need. why?

## aliases
str_gen2grams = str_gen_bigrams

## trigrams
def str_gen_trigrams (text: str, sep = r"", joint = "", check = False):
	import re
	n = 3
	T = [ ]
	if check:
		print(text)
	segs = [ seg for seg in re.split(sep, text) if len(seg) > 0 ]
	size = len(segs)
	if size < n:
		T.append(joint.join(segs))
	else:
		C = [ ]
		for i in range(size - n + 1):
			y = segs[ i : i + n ]
			if check:
				print(y)
			if len(y) == n:
				C.append(joint.join(y))
		T.append(C)
	return (T[0]) # X[0] is in need. why?

## aliases
str_gen3grams = str_gen_trigrams

def list_gen_unigrams (L: list, sep = r"", check = False):
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
list_gen1grams = list_gen_unigrams

## bigram
def list_gen_bigrams (L: list, sep = r"", joint = "", check = False):
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
list_gen2grams = list_gen_bigrams

### trigram
def list_gen_trigrams (L: list, sep = r"", joint = "", check = False):
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
list_gen3grams = list_gen_trigrams

### end of script
