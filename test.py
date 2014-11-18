from repeated_substring import *


s = "ababab"
a = ArcChart(s)

x = a.matching_pairs

for xi in x:
	print xi.value

