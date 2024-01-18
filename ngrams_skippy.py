## Generator of skippy n-grams
## created by Kow Kuroda
## on 2023/10/29
## modified on 2023/10/30, 31; 11/07, 08, 27

import re

##
def generate_skippy_bigrams (L, sep = "", ignore_adjacency = False, max_distance = None, missing_mark = "_", chars_to_strip = "", check = False):
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
	- check [boolean] sets on and off printing internal objects
	"""

	##
	if check:
		print(f"raw input: {L}")

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
						if check: print(f"b: {b}")
						if b not in B:
							B.append(b)
	return B

##
def generate_skippy_trigrams (L, sep = "", ignore_adjacency = False, max_distance = None, missing_mark = "_", chars_to_strip = "", check = False):
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
		print(f"raw input: {L}")

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
								t = ""
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
									print(f"t: {t}")
								if t not in T:
									T.append(t)
	#
	return T

### end of module
