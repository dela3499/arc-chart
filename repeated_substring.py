class ArcChart(object):
    def __init__(self,s):
        self.s = s

    def get_substrings(self):
        s = self.s
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
    
    def get_repeated_substring(self):
        s = self.s
    	return dict((k,v) for k,v in get_substrings(s).iteritems() if len(v) > 1)
    
    @staticmethod
    def is_overlapping(x, y):
        """take tuples of form (start,end) that denote start and end points for
        each instance of a repeated substring
        return True if instances overlap (either the start or endpoint of the
        second instance lie between the start and endpoint of the first instance)"""
        
        def is_between(x,y1,y2):
            """return true if x is between y1 and y2"""
            if x > y1 and x < y2:
                return True
            else:
                return False        
        
        if is_between(y[0],x[0],x[1]) or is_between(y[1],x[0],x[1]):
            return True
        else:
            return False

            
