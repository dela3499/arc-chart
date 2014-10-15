def find_repeated_substrings(s):
	all_substrings = {} # store all repeated substrings here in a dictionary of lists of tuples
	for index in range(0,len(s)):
		for substring_length in range(1, len(s)-index+1):
			start = index
			end = index+substring_length
			substring = s[start:end]
			if substring not in all_substrings:
				all_substrings[substring] = [(start,end)]
			else:
				all_substrings[substring].append((start,end))
	return all_substrings

x = find_repeated_substrings("This is a substring. This is a substring.")

min_length = 25

for key in x: 
	if len(x[key]) > 0 and len(key) > min_length:
		print [key, x[key]]


