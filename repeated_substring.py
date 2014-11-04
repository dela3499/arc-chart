import json

class ArcChart(object):

    def __init__(self, original_string):
        self.org = original_string
        self.substrings = self.get_repeated_substring() # identical pairs
        ArcChart.remove_overlapping_substrings(self.substrings) # non-overlaiing identical pairs

    def get_json_format(self, file_name):
        list_of_pairs = []
        for each_key in self.substrings.keys():
            sublist = self.substrings[each_key]
            for i in range(len(sublist)-1):
                for j in range(i+1, len(sublist)):
                    pair = {'a': sublist[i][0], 'b': sublist[j][0],\
                            'n': sublist[i][1] - sublist[i][0]}
                    list_of_pairs.append( pair )
        with open(file_name, 'w') as f:
            json.dump( list_of_pairs, f) 
    @staticmethod
    def _conver_format(file_name):
        content = ''
        with open(file_name) as f:
            content = f.read()
            content = content.replace('"', '')
            content = content.replace(':', '=')
        with open(file_name, 'w') as f:
            f.write(content)            


    def get_substrings(self):
        """ returns all identical substrings of the original string """
        s = self.org
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
        s = self.org
    	return dict((k,v) for k,v in self.get_substrings().iteritems() if len(v) > 1)

    @staticmethod
    def remove_overlapping_substrings(dic):
        for each_key in dic.keys():
            dic[each_key].sort()
            sublist = dic[each_key]
            for i in range(len(sublist)-1):
                to_be_removed = []
                for j in range(i+1, len(sublist)):
                    if ArcChart.is_overlapping(sublist[i], sublist[j]):
                        to_be_removed.append(sublist[j])
                for each_element in to_be_removed:
                    try:
                        sublist.remove(each_element) 
                    except ValueError:
                        pass

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
