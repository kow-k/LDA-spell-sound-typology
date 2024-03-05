## Generator of skippy n-grams
## created by Kow Kuroda
## on 2023/10/29
## modified on 2023/10/30, 31; 11/07, 08, 27
## 2024/02/05: added minimize option to control over the redundant elements
## modified on 2024/03/01: added implmentation of genearate_skippy_quarigrams

import re

##
def generate_skippy_bigrams (L, sep = "", ignore_adjacency = False, max_distance = None, missing_mark = "_", chars_to_strip = "", minimize = True, check = False):
	"""
	generate skippy trigram from a given list of items

	Options
	- sep [string] sets separator/glue between units [default to ""]
	- ignore_adjacency [boolean] turns off or on removing missing mark
	between adjacent units or not
	- max_distance [integer] sets maximum for the longest gap between
	two units
	- missing_mark [string] replaces the symbol for missing_mark
	- chars_to_strip [string] removes extra characters in [string]
	- inclusively [boolean] generates n-grams inclusively (e.g., 2-grams include 1-grams)
	- minimize [boolean] turns on or off inclusion of redundant elements [defaults to True]
	- check [boolean] sets on and off printing internal objects
	"""

	##
	if check:
		print(f"# raw input: {L}")

	## modify forms
	if sep == "" and chars_to_strip:
		L = [ x.strip(chars_to_strip) for x in L ]

	## set max_distance
	if max_distance == None:
		max_distance = len(L)

	# create double of indices for segment selection
	P = [ ]; B = [ ]
	size = len(L)
	if size < 2:
		B.append(sep.join(L))
	else:
		for i in range(size):
			for j in range(size):
				if i < j and (j - i) <= max_distance:
					p = [i,j]
					if p not in P:
						P.append(p)
						## generate bigrams
						b = ""
						i = p[0]; j = p[1]
						## handles adjacency
						if ignore_adjacency:
							b = L[i] + missing_mark + L[j]
						else:
							# case of adjacent double
							if i + 1 == j:
								b = L[i] + sep + L[j]
							# all other discontinuos cases
							else:
								b = L[i] + missing_mark + L[j]
						## adding the generated unit to B
						if check:
							print(f"# b: {b}")
						## filter elements to add
						if minimize:
							if b not in B:
								B.append(b)
						else:
							B.append(b)
	return B

## aliases
gen_skippy_bigrams = generate_skippy_bigrams 
gen_skippy2grams   = generate_skippy_bigrams
gen_skippy_2grams  = generate_skippy_bigrams

##
def generate_skippy_trigrams (L, sep = "", ignore_adjacency = False, max_distance = None, missing_mark = "_", chars_to_strip = "", minimize = True, check = False):
	"""
	generate skippy trigrams from a given list of items

	Options
	- sep [string] sets separator/glue between units [default to ""]
	- ignore_adjacency [boolean] turns off or on removing missing mark
	between adjacent units or not
	- max_distance [integer] sets maximum for the longest gap between
	two units
	- missing_mark [string] replaces the symbol for missing_mark
	- chars_to_strip [string] removes extra characters in [string]
	- inclusively [boolean] generates n-grams inclusively (e.g., 3-grams include 1- and 2-grams)
	- check [boolean] sets on and off printing internal objects
	"""

	##
	if check:
		print(f"# raw input: {L}")

	## modify forms
	if sep == "" and chars_to_strip:
		L = [ x.strip(chars_to_strip) for x in L ]

	## set max_distance
	if max_distance == None:
		max_distance = len(L)

	# create triple of indices for segment selection
	P = [ ]; T = [ ]
	size = len(L)
	if size < 3:
		T.append(sep.join(L))
	else:
		for i in range(size):
			for j in range(size):
					for k in range(size):
						if i < j < k and (k - i) < max_distance :
							## bottom of loop
							p = [i,j,k]
							if p not in P:
								P.append(p)
								## generate trigrams
								q = ""
								i = p[0]; j = p[1]; k = p[2]
								## adjancency handling
								if ignore_adjacency:
									t = L[i] + missing_mark + L[j] + missing_mark + L[k]
								else: # case of adjacent triple
									if (i + 1 == j) and (j + 1 == k):
										t = L[i] + sep + L[j] + sep + L[k]
									# case 1 of adjacent double
									elif i + 1 == j:
										t = L[i] + sep + L[j] + missing_mark + L[k]
									# case 2 of adjacent double
									elif j + 1 == k:
										t = L[i] + missing_mark + L[j] + sep + L[k]
									# all other discontinuos cases
									else:
										t = L[i] + missing_mark + L[j] + missing_mark + L[k]
									## appending the generated unit
								if check:
									print(f"# t: {t}")
								## filter elements to add
								if minimize:
									if t not in T:
										T.append(t)
								else:
									T.append(t)
	#
	return T

## aliases
gen_skippy_trigrams = generate_skippy_trigrams
gen_skippy3grams    = generate_skippy_trigrams
gen_skippy_3grams   = generate_skippy_trigrams

##
def generate_skippy_quadrigrams (L, sep = "", ignore_adjacency = False, max_distance = None, missing_mark = "_", chars_to_strip = "", minimize = True, check = False):
	"""
	generate skippy quadrigrams from a given list of items

	Options
	- sep [string] sets separator/glue between units [default to ""]
	- ignore_adjacency [boolean] turns off or on removing missing mark
	between adjacent units or not
	- max_distance [integer] sets maximum for the longest gap between
	two units
	- missing_mark [string] replaces the symbol for missing_mark
	- chars_to_strip [string] removes extra characters in [string]
	- inclusively [boolean] generates n-grams inclusively (e.g., 3-grams include 2- and 1-grams)
	- check [boolean] sets on and off printing internal objects
	"""

	##
	if check:
		print(f"# raw input: {L}")

	## modify forms
	if sep == "" and chars_to_strip:
		L = [ x.strip(chars_to_strip) for x in L ]

	## set max_distance
	if max_distance == None:
		max_distance = len(L)

	# create triple of indices for segment selection
	P = [ ]; Q = [ ]
	size = len(L)
	if size < 4:
		Q.append(sep.join(L))
	else:
		for h in range(size):
			for i in range(size):
				for j in range(size):
						for k in range(size):
							if h < i < j < k and (k - h) < max_distance :
								## bottom of loop
								p = [h,i,j,k]
								if p not in P:
									P.append(p)
									## generate quadrigrams
									q = ""
									h = p[0]; i = p[1]; j = p[2]; k = p[3]
									## adjancency handling
									if ignore_adjacency:
										q = L[h]  + missing_mark + L[i] + missing_mark + L[j] + missing_mark + L[k]
									else:
										# case 1: no gap
										if (h + 1 == i) and (i + 1 == j) and (j + 1 == k):
											q = L[h] + sep + L[i] + sep + L[j] + sep + L[k]
										# case 2: 1-gap case 1
										elif (h + 1 == i) and (i + 1 == j):
											q = L[h] + sep + L[i] + sep + L[j] + missing_mark + L[k]
										# case 3: 1-gap case 2
										elif (h + 1 == i) and (j + 1 == k):
											q = L[h]  + sep + L[i] + missing_mark + sep + L[j] + L[k]
										# case 4: 1-gap case 3
										elif (j + 1 == i) and (j + 1 == k):
											q = L[h] + missing_mark + sep + L[i] + sep + L[j] + L[k]
										# case 5: 2-gaps case 1
										elif (j + 1 == k):
											q = L[h] + missing_mark + L[i] + missing_mark + L[j] + sep + L[k]
										# case 6: 2-gapss case 2
										elif (i + 1 == j):
											q = L[h] + missing_mark + L[i] + sep + L[j] + missing_mark + L[k]
										# case 7: 2-gaps case 3
										elif (h + 1 == i):
											q = L[h] + sep + L[i] + missing_mark + L[j] + missing_mark + L[k]
										# all other discontinuos cases
										else:
											q = L[h] + missing_mark + L[i] + missing_mark + L[j] + missing_mark + L[k]
										## appending the generated unit
									if check:
										print(f"# q: {q}")
									## filter elements to add
									if minimize:
										if q not in Q:
											Q.append(q)
									else:
										Q.append(q)
	#
	return Q

## aliases
gen_skippy_quardigrams = generate_skippy_quadrigrams
gen_skippy4grams       = generate_skippy_quadrigrams
gen_skippy_4grams      = generate_skippy_quadrigrams

### end of module
